module string_tests
    use strings
    use iso_fortran_env
    use iso_c_binding
    implicit none
contains
! ------------------------------------------------------------------------------
    function test_string_equality() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: string1 = "String 1"
        character(len = *), parameter :: string2 = "string 1"
        character(len = *), parameter :: string3 = "String 1"

        ! Local Variables
        type(string) :: s1, s2, s3

        ! Initialization
        rst = .true.
        s1 = string1
        s2 = string2
        s3 = string3

        if (s1 == s2) then
            rst = .false.
            print "(A)", "TEST FAILED: test_string_equality -1"
        end if
        if (s1 /= s3) then
            rst = .false.
            print "(A)", "TEST FAILED: test_string_equality -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_split_string() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: s1 = "Test string 1"
        character(len = *), parameter :: s2 = "Test string 2"
        character(len = *), parameter :: s3 = "Test string 3"
        character, parameter :: delimiter = ","

        ! Local Variables
        integer(int32) :: n
        type(string) :: str1, str2
        type(string), allocatable, dimension(:) :: items1, items2, items3

        ! Initialization
        rst = .true.
        str1 = s1 // delimiter // s2 // delimiter // s3
        str2 = str1 // delimiter

        ! Test 1
        items1 = split_string(str1, delimiter)
        if (size(items1) /= 3) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -1"
            return
        end if
        if (items1(1) /= s1) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -2"
        end if
        if (items1(2) /= s2) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -3"
        end if
        if (items1(3) /= s3) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -4"
        end if

        ! Test 2
        items2 = split_string(str2, delimiter)
        if (size(items2) /= 3) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -5"
        end if
        if (items2(1) /= s1) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -6"
        end if
        if (items2(2) /= s2) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -7"
        end if
        if (items2(3) /= s3) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -8"
        end if

        ! Test 3
        items3 = split_string(s1, delimiter)
        if (size(items3) /= 1) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -9"
        end if
        if (items3(1) /= s1) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_string -10"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_to_string() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        integer(int32), parameter :: i1 = 4321
        character(len = *), parameter :: s1 = "4321"
        real(real64), parameter :: r1 = 1.234d0
        character(len = *), parameter :: s2 = "1.234"
        character(len = *), parameter :: fmt = "(F5.3)"

        ! Local Variables
        type(string) :: str1, str2

        ! Initialization
        rst = .true.
        
        ! Test 1
        str1 = to_string(i1)
        if (str1 /= s1) then
            rst = .false.
            print "(A)", "TEST FAILED: test_to_string -1"
        end if

        ! Test 2
        str2 = to_string(r1, fmt)
        if (str2 /= s2) then
            rst = .false.
            print "(A)", "TEST FAILED: test_to_string -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_to_c_string() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: s1 = "Test C String."

        ! Local Variables
        character(kind = c_char), allocatable, dimension(:) :: c_str
        integer(int32) :: i, n

        ! Initialization
        rst = .true.
        n = len(s1)

        ! Test
        c_str = to_c_string(s1)
        if (size(c_str) /= n + 1) then
            rst = .false.
            print "(A)", "TEST FAILED: test_to_c_string -1"
        end if
        do i = 1, n
            if (c_str(i) /= s1(i:i)) then
                rst = .false.
                print "(A)", "TEST FAILED: test_to_c_string -2"
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    function test_string_to_int() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: x1 = "123456789"
        integer(int32), parameter :: ans = 123456789

        ! Local Variables
        integer(int32) :: x

        ! Initialization
        rst = .true.

        ! Test
        x = string_to_int(x1)
        if (x /= ans) then
            rst = .false.
            print "(A)", "TEST FAILED: test_string_to_int -1"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_string_to_real() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: x1 = "1.2345"
        real(real64), parameter :: ans = 1.2345d0

        ! Local Variables
        real(real64) :: x, tol

        ! Initialization
        rst = .true.
        tol = sqrt(epsilon(tol))

        ! Test
        x = string_to_real(x1)
        if (abs(x - ans) > tol) then
            rst = .false.
            print "(A)", "TEST FAILED: test_string_to_real -1"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_string_builder() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: s1 = "Test string, part 1"
        character(len = *), parameter :: s2 = ", and this is part 2."

        ! Local Variables
        type(string_builder) :: str
        type(string) :: x, ans

        ! Initialization
        rst = .true.
        ans = to_string(s1 // s2)

        ! Test
        call str%append(s1)
        call str%append(s2)
        x = str%to_string()
        if (x /= ans) then
            rst = .false.
            print "(A)", "TEST FAILED: test_string_builder -1"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_remove() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        integer(int32), parameter :: start = 9  ! j in just
        integer(int32), parameter :: last = 13
        character(len = *), parameter :: x = "This is just a test string."
        character(len = *), parameter :: ans = "This is a test string."
        character(len = *), parameter :: ans2 = "This is just a test."

        ! Local Variables
        integer(int32) :: si
        type(string) :: y, y2

        ! Initialization
        rst = .true.

        ! Find the starting index
        si = index(x, "just")
        if (si /= start) then
            rst = .false.
            print "(A)", "TEST FAILED: test_remove -1"
        end if

        ! Remove "just "
        y = remove(x, start = si, finish = last)
        if (y /= ans) then
            rst = .false.
            print "(A)", "TEST FAILED: test_remove -2"
        end if

        ! Try by just using the substring itself
        y2 = remove(x, " string")
        if (y2 /= ans2) then
            rst = .false.
            print "(A)", "TEST FAILED: test_remove -3"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_insert() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: str = "This is a test string."
        character(len = *), parameter :: ans = "This is just a test string."
        character(len = *), parameter :: substr = " just"
        integer(int32), parameter :: start = 8

        ! Local Variables
        type(string) :: x

        ! Initialization
        rst = .true.

        ! Test
        x = insert(str, start, substr)
        if (x /= ans) then
            rst = .false.
            print "(A)", "TEST FAILED: test_insert -1"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_regex_match() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: str = "string."
        character(len = *), parameter :: pattern = "(str)(.*)"

        ! Local Variables
        type(string), allocatable, dimension(:) :: x

        ! Initialization
        rst = .true.

        ! Test 1
        x = regex_match(str, pattern)
        if (size(x) /= 3) then
            rst = .false.
            print "(A)", "TEST FAILED: test_regex_match -1"
            return
        end if
        if (x(1) /= "string.") then
            rst = .false.
            print "(A)", "TEST FAILED: test_regex_match -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_regex_search() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: str = "Test with numbers 123456."
        character(len = *), parameter :: pattern = "(\d)(.*)"

        ! Local Variables
        type(string), allocatable, dimension(:) :: x

        ! Initialization
        rst = .true.

        ! Test 1
        x = regex_search(str, pattern)
        if (size(x) /= 3) then
            rst = .false.
            print "(A)", "TEST FAILED: test_regex_search -1"
            return
        end if
        if (x(1) /= "123456.") then
            rst = .false.
            print "(A)", "TEST FAILED: test_regex_search -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_replace() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: str = "This is just a test string."
        character(len = *), parameter :: pattern = "(t)"
        character(len = *), parameter :: substr = "T"
        character(len = *), parameter :: ans = "This is jusT a TesT sTring."

        ! Local Variables
        type(string) :: x

        ! Initialization
        rst = .true.

        ! Test 1
        x = replace(str, pattern, substr)
        if (x /= ans) then
            rst = .false.
            print "(A)", "TEST FAILED: test_replace -1"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_to_upper() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: str = "This is a test string 123&."
        character(len = *), parameter :: ans = "THIS IS A TEST STRING 123&."

        ! Local Variables
        character(len = :), allocatable :: x

        ! Initialization
        rst = .true.

        ! Test 1
        x = to_upper(str)
        print *, x
        print *, len(x)
        if (x /= ans) then
            rst = .false.
            print "(A)", "TEST FAILED: test_to_upper -1"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_to_lower() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: str = "This Is A Test String 123&."
        character(len = *), parameter :: ans = "this is a test string 123&."

        ! Local Variables
        character(len = :), allocatable :: x

        ! Initialization
        rst = .true.

        ! Test 1
        x = to_lower(str)
        print *, x
        print *, len(x)
        if (x /= ans) then
            rst = .false.
            print "(A)", "TEST FAILED: test_to_lower -1"
        end if
    end function

! ------------------------------------------------------------------------------
end module