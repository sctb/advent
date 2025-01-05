#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CACHE 128*1024*1024

static long tray1[1024*1024];
static long tray2[1024*1024];
static long tray3[1024*1024];
static long cache[CACHE];
static char string[64];

static size_t blink(size_t len, long tray[]) {
	long n;
	size_t i, j, k;
	size_t new = 0;
	for (i = 0; i < len; i++) {
		n = tray[i];
		if (n == 0) tray[i] = 1;
		else {
			j = sprintf(string, "%ld", n);
			if (j % 2 == 0) {
				k = j / 2;
				tray[i] = strtol(string+k, NULL, 10);
				string[k] = 0;
				tray[len+new++] = strtol(string, NULL, 10);
			} else tray[i] = n*2024;
		}
	}
	return len+new;
}

static size_t blinkn(size_t times, size_t len, long tray[]) {
	size_t i;
	for (i = 0; i < times; i++)
		len = blink(len, tray);
	return len;
}

static size_t blink1(long n, size_t times, long tray[]) {
	size_t count;
	tray[0] = n;
	count = blinkn(times, 1, tray);
	if (n < CACHE) cache[n] = count;
	return count;
}

static size_t blink2(
	size_t times,
	size_t len,
	long ta[],
	long tb[],
	long tc[])
{
	long n;
	size_t i, j;
	size_t count = 0;
	for (i = 0; i < len; i++) {
		n = ta[i];
		/* prime the cache on the first run, use it second */
		if (tc == NULL && n < CACHE && cache[n] > 0) j = cache[n];
		else j = blink1(n, times, tb);
		if (tc != NULL)	count += blink2(times, j, tb, tc, NULL);
		else count += j;
	}
	return count;
}

int main(int argc, char *argv[]) {
	size_t len = 8;
	tray1[0] = 773;
	tray1[1] = 79858;
	tray1[2] = 0;
	tray1[3] = 71;
	tray1[4] = 213357;
	tray1[5] = 2937;
	tray1[6] = 1;
	tray1[7] = 3998391;

	/* start with the original question: 25 blinks */
	len = blinkn(25, len, tray1);
	
	/* blink each stone in the tray a further 50 times, 25 times
	 * each in two additional trays */
	len = blink2(25, len, tray1, tray2, tray3);

	printf("%lu\n", len);
	return 0;
}
