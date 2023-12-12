
#include "c_fsys_test.h"

int main() {
    // Local Variables
    int flag;

    // Initialization
    flag = 0;

    // Process
    if (!test_string_equality()) flag = 1;
    if (!test_split_string()) flag = 2;
    if (!test_to_string()) flag = 3;
    if (!test_to_c_string()) flag = 4;
    if (!test_string_to_int()) flag = 5;
    if (!test_string_to_real()) flag = 6;
    if (!test_string_builder()) flag = 7;
    if (!test_remove()) flag = 8;
    if (!test_insert()) flag = 9;
    if (!test_regex_match()) flag = 10;
    if (!test_regex_search()) flag = 11;
    if (!test_replace()) flag = 12;
    if (!test_to_upper()) flag = 13;
    if (!test_to_lower()) flag = 14;
    if (!test_split_path()) flag = 15;
    if (!test_get_directory_contents()) flag = 16;
    if (!test_find_all_files()) flag = 17;

    // End
    return flag;
}