#include <stdio.h>
#include <stdlib.h>

#define MSIZE 32768
#define DOMAP(pos) \
  for (pos.y = 0; pos.y < nrows; pos.y++) \
    for (pos.x = 0; pos.x < ncols; pos.x++) \

static char map[MSIZE];
static size_t ncols;
static size_t nrows;

struct pos {
  size_t x;
  size_t y;
};

static struct pos steps[MSIZE];
static int nsteps;

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

static char mapget(struct pos p) {
  return map[(p.y * (ncols + 1)) + p.x];
}

static void mapset(struct pos p, char c) {
  map[(p.y * (ncols + 1)) + p.x] = c;
}

static struct pos findchar(char c) {
  struct pos p;
  DOMAP(p) if (mapget(p) == c) return p;
  return p;
}

static int there(struct pos a, struct pos b) {
  return a.x == b.x && a.y == b.y;
}

static int ontrack(struct pos p) {
  if (nsteps >= 0 && there(p, steps[nsteps])) return 0;
  if (mapget(p) == '#') return 0;
  return 1;
}

static struct pos next(struct pos p) {
  struct pos q;
  q.x = p.x; q.y = p.y + 1; if (ontrack(q)) return q;
  q.x = p.x; q.y = p.y - 1; if (ontrack(q)) return q;
  q.y = p.y; q.x = p.x + 1; if (ontrack(q)) return q;
  q.y = p.y; q.x = p.x - 1; return q;
}

static void trace(struct pos here, struct pos end) {
  struct pos prev;
  nsteps = -1;
  while (!there(here, end)) {
    prev.x = here.x; prev.y = here.y;
    here = next(here);
    steps[++nsteps] = prev;
  }
  steps[++nsteps] = here;
}

static void puzzle1(void) {
  struct pos start, end;
  start = findchar('S');
  end = findchar('E');
  trace(start, end);
  printf("took %d steps\n", nsteps);
}

int main(void) {
  input("data/example-20.txt");
  puzzle1();
  return 0;
}
