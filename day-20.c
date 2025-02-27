#include <stdio.h>
#include <stdlib.h>

#define MSIZE 32768
#define DOMAP(pos) \
  for (pos.y = 0; pos.y < nrows; pos.y++) \
    for (pos.x = 0; pos.x < ncols; pos.x++) \

static char map[MSIZE];
static int ncols;
static int nrows;

struct pos {
  int x;
  int y;
};

static struct pos steps[MSIZE];
static int nsteps;

static void die(const char *msg) {
  perror(msg);
  exit(1);
}

static void load(int size) {
  int i;
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
  int size;
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
  if (nsteps > 0 && there(p, steps[nsteps - 1])) return 0;
  if (mapget(p) == '#') return 0;
  return 1;
}

static struct pos next(struct pos p) {
  struct pos q;
  q.x = p.x; q.y = p.y + 1; if (ontrack(q)) return q;
  q.x = p.x; q.y = p.y - 1; if (ontrack(q)) return q;
  q.y = p.y; q.x = p.x + 1; if (ontrack(q)) return q;
  q.y = p.y; q.x = p.x - 1; if (ontrack(q)) return q;
  die("next"); return p; /* not reached */
}

static void trace(struct pos here, struct pos end) {
  struct pos prev;
  while (!there(here, end)) {
    prev.x = here.x; prev.y = here.y;
    here = next(here);
    steps[nsteps++] = prev;
  }
  steps[nsteps++] = here;
}

static int cheatable(struct pos a, struct pos b, int ps) {
  int n;
  n = abs((int)a.x - (int)b.x) + abs((int)a.y - (int)b.y);
  if (n >= 2 && n <= ps) return n;
  return 0;
}

static void cheats(int ps) {
  int n, i, j, saved;
  struct pos a, b;
  for (n = 0, i = 0; i < (nsteps - 2); i++)
    for (j = i + 2; j < nsteps; j++) {
      a = steps[i];
      b = steps[j];
      if ((saved = cheatable(a, b, ps)) && (j - i - saved) >= 100)
	n++;
    }
  printf("%d cheats\n", n);
}

static void puzzle1(void) {
  printf("Part 1: ");
  cheats(2);
}

static void puzzle2(void) {
  printf("Part 2: ");
  cheats(20);
}

int main(void) {
  struct pos start, end;
  input("data/input-20.txt");
  start = findchar('S');
  end = findchar('E');
  trace(start, end);
  puzzle1();
  puzzle2();
  return 0;
}
