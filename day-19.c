#include <stdio.h>
#include <stdlib.h>

#define TSIZE 4096
#define DSIZE 32768
static char tbuf[TSIZE];
static char dbuf[DSIZE];

#define ISIZE 1024
static char *towel[ISIZE];
static char *design[ISIZE];

static int ntowels;
static int ndesigns;

static void parse_towels(void) {
  int n;
  char *s;

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

int possible(char *design) {
  /*
   * FIXME: insufficient
   *
   * The design needs to be recursive, following through each initial
   * matching towel, so that all combinations of towels are considered
   * (not just longest-match-first)
   */
  int i, j, n;
  while (*design != '\0') {
    for (i = 0, n = 0; n < ntowels; n++) {
      j = match(towel[n], design);
      i = j > i ? j : i;
    }
    if (i == 0) return 0;
    design += i;
  }
  return i;
}

void puzzle(void) {
  int i, n;
  for (i = 0, n = 0; i < ndesigns; i++) {
    if (possible(design[i])) n++;
  }
  printf("%d possible designs\n", n);
}

int main(void) {
  input("data/input-19.txt");
  puzzle();

  return 0;
}
