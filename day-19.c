#include <stdio.h>
#include <stdlib.h>

#define TSIZE 4096
#define DSIZE 32768
static char tbuf[TSIZE];
static char dbuf[DSIZE];

#define ISIZE 1024
static const char *towel[ISIZE];
static const char *design[ISIZE];
static int ntowels;
static int ndesigns;

static const char *good[DSIZE];
static const char *bad[DSIZE];
static long goodways[DSIZE];
static int ngood;
static int nbad;

static void parse_towels(void) {
  char *s;
  int n, m;
  for(n = 0, s = tbuf; *s != '\0'; n++) {
    towel[n] = s;
    while ((*s != ',') && (*s != '\n')) s++;
    *s = '\0'; s++;
    if (*s == ' ') s++;
  }
  ntowels = n;
}

static void parse_designs(size_t size) {
  int n, i;
  char *s;
  for(i = 0, n = 0, s = dbuf; i < size; n++) {
    design[n] = s;
    while (*s != '\n') { i++, s++; }
    *s = '\0';
    i++; s++;
  }
  ndesigns = n;
}

static void die(const char *msg) {
  perror(msg);
  exit(1);
}

static void input(const char *path) {
  FILE *file;
  char *read;
  size_t size;
  file = fopen(path, "r");
  if (file == NULL) die("input file");
  read = fgets(tbuf, TSIZE, file);
  if (read == NULL) die("towel read");
  /* skip blank line */
  if (fseek(file, 1, SEEK_CUR) != 0) die("seek");
  size = fread(dbuf, 1, DSIZE, file);
  if (ferror(file) != 0) die("design read");
  parse_towels();
  parse_designs(size);
}

int match(const char *prefix, const char *string) {
  int i = 0;
  while (*prefix == *string) {
    prefix++; string++; i++;
    if (*prefix == '\0') return i;
    if (*string == '\0') return 0;
  }
  return 0;
}

long known_good(const char *design) {
  int i;
  for (i = 0; i < ngood; i++) {
    if (good[i] == design) return goodways[i];
  }
  return 0;
}

int known_bad(const char *design) {
  int i;
  for (i = 0; i < nbad; i++) {
    if (bad[i] == design) return 1;
  }
  return 0;
}

int possible(const char *design) {
  long r, k;
  int i, n;
  if (*design == '\0') return 1;
  if ((r = known_good(design))) return r;
  if (known_bad(design)) return 0;
  for (r = 0, i = 0, n = 0; n < ntowels; n++) {
    i = match(towel[n], design);
    if (i > 0 && (k = possible(design + i))) {
      r += k;
    }
  }
  if (r) {
    good[ngood] = design;
    goodways[ngood] = r;
    ngood++;
  } else bad[nbad++] = design;
  return r;
}

void puzzle1(void) {
  int i, n;
  for (i = 0, n = 0; i < ndesigns; i++) {
    if (possible(design[i])) n++;
  }
  printf("%d possible designs\n", n);
}

void puzzle2(void) {
  long k;
  int i;
  for (i = 0, k = 0; i < ndesigns; i++) {
    k += possible(design[i]);
  }
  printf("%ld different ways\n", k);
}

int main(void) {
  input("data/input-19.txt");
  puzzle1();
  puzzle2();
  return 0;
}

/*
  1555825069 too low (int)
  5850792365 too low (long)
*/
