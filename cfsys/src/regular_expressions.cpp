#include <regex>
#include <string>
#include <cstring>
#include <cstdlib>
#include "regular_expressions.h"
#include "helper_macros.h"

using namespace std;

// References:
// - http://www.cplusplus.com/reference/regex/regex_match/

void c_regex_match(const char *src, const char *pattern, int numbuff, 
                  int buffsizes, char **buffer, int *itemsizes, int *count)
{
    // Perform the match process
    cmatch cm;
    try {
        regex ex(pattern);
        regex_match(src, cm, ex);
    }
    catch (...) {
        *count = 0;
        return;
    }

    // Process each match
    *count = (int)cm.size();
    for (int i = 0; i < MIN(*count, numbuff); ++i)
    {
        string rst = cm[i];
        itemsizes[i] = (int)rst.size();
        strncpy(
            buffer[i],
            rst.c_str(),
            MIN((size_t)buffsizes, rst.size())
        );
    }
}

void c_regex_search(const char *src, const char *pattern, int numbuff,
                    int buffsizes, char **buffer, int *itemsizes, int *count)
{
    // Perform the search process
    cmatch cm;
    try {
        regex ex(pattern);
        regex_search(src, cm, ex);
    }
    catch (...) {
        *count = 0;
        return;
    }

    // Process each match
    *count = (int)cm.size();
    for (int i = 0; i < MIN(*count, numbuff); ++i) 
    {
        string rst = cm[i];
        itemsizes[i] = (int)rst.size();
        strncpy(
            buffer[i], 
            rst.c_str(), 
            MIN((size_t)buffsizes, rst.size()) );
    }
}

void c_regex_replace(const char *src, const char *pattern, const char *rplc, 
                     int buffsize, char *buffer, int *nbuff)
{
    string rst;
    try {
        // Replace all instances
        regex ex(pattern);
        rst = regex_replace(src, ex, rplc, regex_constants::match_any);
    }
    catch (...) {
        // On error, simply return the original string
        *nbuff = MIN(buffsize, strlen(src));
        strncpy(buffer, src, (size_t)(*nbuff));
        return;
    }

    // Copy the new string to the output
    *nbuff = MIN(buffsize, (int)rst.size());
    strncpy(buffer, rst.c_str(), (size_t)(*nbuff));
}
