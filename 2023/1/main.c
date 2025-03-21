#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct entry {
  char *s;
  char  c;
};

#define BSIZE 32768
static char buffer[BSIZE];
static size_t size;
static char pair[3];

static struct entry table[] = {
  {"one",   '1'},
  {"two",   '2'},
  {"three", '3'},
  {"four",  '4'},
  {"five",  '5'},
  {"six",   '6'},
  {"seven", '7'},
  {"eight", '8'},
  {"nine",  '9'}
};

static void die(const char *msg) {
  perror(msg);
  exit(1);
}

static void input(const char *path) {
  FILE *file;
  char *read;
  file = fopen(path, "r");
  if (file == NULL) die("file");
  size = fread(buffer, 1, BSIZE, file);
  if (ferror(file)) die("read");
}

static char spelled(const char *s, int left) {
  int i, n;
  for (i = 0; i < 9; i++) {
    n = strlen(table[i].s);
    if (n <= left && !memcmp(s, table[i].s, n)) {
      return table[i].c;
    }
  }
  return '\0';
}

static void puzzle(void) {
  int i, sum;
  char c;
  for (i = 0, sum = 0; i < size; i++) {
    c = buffer[i];
    if (isdigit(c) || (c = spelled(&buffer[i], size - i))) {
      if (!pair[0]) {
        pair[0] = c;
        pair[1] = c;
      } else pair[1] = c;
    } else if (buffer[i] == '\n') {
      sum += atoi(pair);
      pair[0] = '\0';
      pair[1] = '\0';
    }
  }
  printf("Sum of calibration values: %d\n", sum);
}

int main(void) {
  input("input.txt");
  puzzle();
  return 0;
}
