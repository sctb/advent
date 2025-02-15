#include <stdio.h>
#include <stdlib.h>

#define TSIZE 4096
#define DSIZE 32768
static char tbuf[TSIZE];
static char dbuf[DSIZE];

#define MAXN 1024
static char *towel[MAXN];
static char *design[MAXN];

static int ntowels;
static int ndesigns;

/*
 * Task: arrange the onsen towels
 * Towels have colored strips: 
 *   (w)hite, bl(u)e, (b)black, (r)ed, (g)reen
 * Designs are combinations of towels that create a pattern
 *   e.g. rgrgr could be two rg towels and an r, or rgr then a gr
 * Input: list of available towel patterns, list of desired designs
 *   Line 1: list of towel patterns
 *   Line 2: blank
 *   Line 3-N: design
 * NB: not all designs are possible given the towel patterns
 * Goal: how many designs are possible?
 */

static void parse_towels(void) {
	int n;
	char *s;

	for(n = 0, s = tbuf; *s != '\0'; n++) {
		towel[n] = s;
		while ((*s != ',') && (*s != '\n')) s++;
		*s = '\0'; s++;
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

static void dump(void) {
	int i;
	for (i = 0; i < ntowels; i++) {
		printf("%s,", towel[i]);
		fflush(stdout);
	}
	printf("\n\n");
	for (i = 0; i < ndesigns; i++) {
		printf("%s\n", design[i]);
	}
}

int main(void) {
	input("data/example-19.txt");
	dump();

	return 0;
}