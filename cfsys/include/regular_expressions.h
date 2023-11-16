#ifndef REGULAR_EXPRESSIONS_H__
#define REGULAR_EXPRESSIONS_H__

#ifdef __cplusplus
extern "C" {
#endif

void c_regex_match(const char *src, const char *pattern, int numbuff, 
                  int buffsizes, char **buffer, int *itemsizes, int *count);

void c_regex_search(const char *src, const char *pattern, int numbuff,
                    int buffsizes, char **buffer, int *itemsizes, int *count);

void c_regex_replace(const char *src, const char *pattern, const char *rplc, 
                     int buffsize, char *buffer, int *nbuff);

#ifdef __cplusplus
}
#endif
#endif
