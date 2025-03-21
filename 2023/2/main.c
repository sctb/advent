#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BSIZE 1024
static char buffer[BSIZE];
static const int red = 12;
static const int green = 13;
static const int blue = 14;

static void die(const char *msg) {
  perror(msg);
  exit(1);
}

static int game(char *line) {
  int id = 0;
  char *field;
  line += strlen("Game ");
  field = strsep(&line, ":");
  id = atoi(field);
  return id;
}

static void puzzle(const char *path) {
  int sum = 0;
  FILE *file;
  char *line;
  file = fopen(path, "r");
  if (file == NULL) die("file");
  while ((line = fgets(buffer, BSIZE, file))) {
    sum += game(line);
  }
  printf("Sum of the game IDs: %d\n", sum);
}

int main(void) {
  puzzle("example.txt");
}
