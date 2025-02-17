#include <stdio.h>
#include <stdlib.h>

#define MSIZE 32768
static char map[MSIZE];
static size_t ncols;
static size_t nrows;

static void die(const char *msg) {
  perror(msg);
  exit(1);
}

static void load(size_t size) {
  size_t i;
  for (i = 0; i < size; i++)
    if (map[i] == '\n') {
      ncols = i;
      map[i] = '\0';
      break;
    }
  nrows = size / (ncols + 1);
  for (i = 1; i <= nrows; i++)
    map[(i * (ncols + 1)) - 1] = '\0';
}

static void input(const char *path) {
  FILE *file;
  char *read;
  size_t size;
  file = fopen(path, "r");
  if (file == NULL) die("input file");
  size = fread(map, 1, MSIZE, file);
  if (ferror(file) != 0) die("map read");
  load(size);
}

static void dump(void) {
  size_t x, y;
  for (y = 0; y < nrows; y++)
    printf("%s\n", &map[y * (ncols + 1)]);
}

int main(void) {
  input("data/example-20.txt");
  dump();
  return 0;
}
