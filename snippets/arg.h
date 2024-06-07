#ifndef ARG_H
#define ARG_H

/* int main(int argc, char *argv[]) */
/* opts (case 'a': break; default: printf("Unknown: %c\n", opt);); */

#define opt argv[optidx][opt_i]
#define optarg (!argv[optidx + optargidx] ? NULL : (argv[optidx + optargidx++]))
#define opts(cases)                                                    \
	do {                                                           \
		unsigned optargidx, opt_i, optidx;                     \
		for (optargidx = 1, opt_i = 1, optidx = 1;             \
		     argv[optidx] && argv[optidx][0] == '-' &&         \
		     argv[optidx][1] && argv[optidx][1] != '-';        \
		     optidx += optargidx) {                            \
			for (opt_i = 1, optargidx = 1; opt; opt_i++) { \
				switch (opt) {                         \
					cases                          \
				}                                      \
			}                                              \
		}                                                      \
	} while (0)

#endif
