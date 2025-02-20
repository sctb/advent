#include <stdio.h>
#include <stdlib.h>

#define BSIZE 128
static char buffer[BSIZE];
static char *codes[5];

static void die(const char *msg) {
  perror(msg);
  exit(1);
}

static void load(size_t size) {
  int i, j;
  codes[0] = &buffer[0];
  for (i = 0, j = 1; i < size; i++)
    if (buffer[i] == '\n') {
      buffer[i] = '\0';
      if (i + 1 < size)
	codes[j++] = &buffer[i + 1];
    }
}

static void input(const char *path) {
  FILE *file;
  char *read;
  size_t size;
  file = fopen(path, "r");
  if (file == NULL) die("file");
  size = fread(buffer, 1, BSIZE, file);
  if (ferror(file) != 0) die("read");
  load(size);
}

static void puzzle1(void) {
  int i;
  for (i = 0; i < 5; i++)
    printf("CODE: %s\n", codes[i]);
}

int main(void) {
  input("data/example-21.txt");
  puzzle1();
  return 0;
}
