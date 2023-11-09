program main
    use string_tests
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

    ! End
    if (flag /= 0) stop flag
end program