#ifndef C_FSYS_TEST_H_
#define C_FSYS_TEST_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

bool test_string_equality();
bool test_split_string();
bool test_to_string();
bool test_to_c_string();
bool test_string_to_int();
bool test_string_to_real();
bool test_string_builder();
bool test_remove();
bool test_insert();
bool test_regex_match();
bool test_regex_search();
bool test_replace();
bool test_to_upper();
bool test_to_lower();
bool test_split_path();
bool test_get_directory_contents();
bool test_find_all_files();

#ifdef __cplusplus
}
#endif
#endif