program main
    use string_tests
    use fsys_test_routines
    implicit none

    logical :: rst
    integer :: flag

    flag = 0

    ! Tests
    rst = test_string_equality()
    if (.not.rst) flag = 1

    rst = test_split_string()
    if (.not.rst) flag = 2

    rst = test_to_string()
    if (.not.rst) flag = 3

    rst = test_to_c_string()
    if (.not.rst) flag = 4

    rst = test_string_to_int()
    if (.not.rst) flag = 5

    rst = test_string_to_real()
    if (.not.rst) flag = 6

    rst = test_string_builder()
    if (.not.rst) flag = 7

    rst = test_remove()
    if (.not.rst) flag = 8

    rst = test_insert()
    if (.not.rst) flag = 9

    rst = test_regex_match()
    if (.not.rst) flag = 10

    rst = test_regex_search()
    if (.not.rst) flag = 11

    rst = test_replace()
    if (.not.rst) flag = 12

    rst = test_split_path()
    if (.not.rst) flag = 13

    rst = test_get_directory_contents()
    if (.not.rst) flag = 14

    rst = test_find_all_files()
    if (.not.rst) flag = 15

    ! End
    if (flag /= 0) stop flag
end program