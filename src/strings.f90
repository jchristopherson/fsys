module strings
    use iso_fortran_env
    use iso_c_binding
    implicit none
    private
    public :: string
    public :: string_builder
    public :: assignment(=)
    public :: operator(==)
    public :: operator(/=)
    public :: operator(//)
    public :: len
    public :: len_trim
    public :: split_string
    public :: trim
    public :: to_string
    public :: char
    public :: to_c_string
    public :: string_to_int
    public :: string_to_real
    public :: adjustl
    public :: adjustr
    public :: index
    public :: scan
    public :: remove
    public :: insert
    public :: regex_match
    public :: regex_search
    public :: replace
    public :: to_upper
    public :: to_lower

! ------------------------------------------------------------------------------
    type string
        !! Defines a string type.
        character(len = :), private, allocatable :: m_str
    contains
        procedure, public :: get => str_get
        procedure, public :: set => str_set
    end type

    ! ----------
    type string_builder
        !! Provides a mechanism for building strings without reallocation on
        !! each addition.
    private
        integer(int32) :: m_length = 0
            ! Actual length of the string
        character(len = :), allocatable :: m_buffer
            ! The string buffer
    contains
        procedure, public :: initialize => sb_init
        generic, public :: append => sb_append_char, sb_append_string
        procedure, public :: to_string => sb_to_string
        procedure, public :: length => sb_get_length

        procedure, private :: sb_append_char
        procedure, private :: sb_append_string
    end type

! ------------------------------------------------------------------------------
    interface assignment(=)
        !! Copies a string to a traditional Fortran character(len = *) string
        !! or vice-versa.
        module procedure :: string_to_char
        module procedure :: char_to_string
    end interface

    interface operator(==)
        !! Element-wise comparison of two strings.
        module procedure :: string_eq_string
        module procedure :: char_eq_string
        module procedure :: string_eq_char
    end interface

    interface operator(/=)
        !! Element-wise comparison of two strings.
        module procedure :: string_neq_string
        module procedure :: char_neq_string
        module procedure :: string_neq_char
    end interface

    interface operator(//)
        !! Concatenation of two strings.
        module procedure :: concat_strings
        module procedure :: concat_char_string
        module procedure :: concat_string_char
    end interface

    interface len
        !! Returns the length of a string.
        module procedure :: string_len
    end interface

    interface len_trim
        !! Returns the length of a string.
        module procedure :: string_len_trim
    end interface

    interface split_string
        !! Splits a string by the specified delimiter character.
        module procedure :: split_string_char
        module procedure :: split_string_str
    end interface

    interface trim
        !! Trims any trailing blanks from the end of the string.
        module procedure :: string_trim
    end interface

    interface to_string
        !! Converts an item to a string.
        module procedure :: char_convert_to_string
        module procedure :: int8_to_string
        module procedure :: int16_to_string
        module procedure :: int32_to_string
        module procedure :: int64_to_string
        module procedure :: real32_to_string
        module procedure :: real64_to_string
        module procedure :: real128_to_string
        module procedure :: c_string_to_string
    end interface

    interface to_c_string
        !! Converts a Fortran string to a C-compatible, null-terminated string.
        module procedure :: char_to_c_string
        module procedure :: string_to_c_string
    end interface

    interface char
        !! Converts a string to a Fortran character array.
        module procedure :: string_convert_to_char
    end interface

    interface string_to_int
        !! Converts a string to a 32-bit integer.
        module procedure :: string_to_int_char
        module procedure :: string_to_int_string
    end interface

    interface string_to_real
        !! Converts a string to a 64-bit real.
        module procedure :: string_to_real_char
        module procedure :: string_to_real_string
    end interface

    interface adjustl
        !! Adjusts the string to the left by removing any leading blanks and
        !! pushing any removed blanks onto the end of the string.
        module procedure :: string_adjustl
    end interface

    interface adjustr
        !! Adjusts the string to the right by removing any trailing blanks and
        !! pushing any removed blanks onto the front of the string.
        module procedure :: string_adjustr
    end interface

    interface index
        !! Returns the starting position of a substring within a string.
        module procedure :: string_index_string
        module procedure :: string_index_char
        module procedure :: char_index_string
    end interface

    interface scan
        !! Scans the string for any of the characters in the specified substring.
        module procedure :: string_scan_string
        module procedure :: string_scan_char
        module procedure :: char_scan_string
    end interface

    interface remove
        !! Removes a range of characters from the string.
        module procedure :: remove_char
        module procedure :: remove_string
        module procedure :: char_remove_char
        module procedure :: char_remove_string
        module procedure :: string_remove_char
        module procedure :: string_remove_string
    end interface

    interface insert
        !! Inserts a substring into another string at the specified index.
        module procedure :: char_insert_char
        module procedure :: char_insert_string
        module procedure :: string_insert_char
        module procedure :: string_insert_string
    end interface

    interface regex_match
        !! Looks for sequences that match the requested pattern.  The entire
        !! target sequence must match the regular expression for this function 
        !! to succeed.
        module procedure :: char_regex_match_char
        module procedure :: char_regex_match_string
        module procedure :: string_regex_match_char
        module procedure :: string_regex_match_string
    end interface

    interface regex_search
        !! Looks for sequences that match the requested pattern.
        module procedure :: char_regex_search_char
        module procedure :: char_regex_search_string
        module procedure :: string_regex_search_char
        module procedure :: string_regex_search_string
    end interface

    interface replace
        !! Replaces the substring identified by the supplied regular 
        !! expression pattern with a new substring.
        module procedure :: char_replace_char_char
        module procedure :: char_replace_char_string
        module procedure :: char_replace_string_char
        module procedure :: char_replace_string_string
        module procedure :: string_replace_char_char
        module procedure :: string_replace_char_string
        module procedure :: string_replace_string_char
        module procedure :: string_replace_string_string
    end interface

    interface to_upper
        !! Converts a string to all uppercase characters.  Any characters that 
        !! are already uppercase, a special symbol, or a numeric value are not
        !! modified.
        module procedure :: to_upper_char
        module procedure :: to_upper_string
    end interface

    interface to_lower
        !! Converts a string to all lowercase characters.  Any characters that 
        !! are already lowercase, a special symbol, or a numeric value are not
        !! modified.
        module procedure :: to_lower_char
        module procedure :: to_lower_string
    end interface

! ------------------------------------------------------------------------------
    interface ! from stdlib.h
        pure function atof(str) result(rst) bind(C, name = "atof")
            use iso_c_binding, only : c_char, c_double
            character(kind = c_char), intent(in) :: str(*)
            real(c_double) :: rst
        end function

        pure function atoi(str) result(rst) bind(C, name = "atoi")
            use iso_c_binding, only : c_char, c_int
            character(kind = c_char), intent(in) :: str(*)
            integer(c_int) :: rst
        end function
    end interface

    interface ! from regular_expressions.h
        pure subroutine c_regex_match(src, pattern, numbuff, buffsizes, &
            buffer, itemsizes, count) bind(C, name = "c_regex_match")
            use iso_c_binding
            character(kind = c_char), intent(in) :: src(*), pattern(*)
            integer(c_int), intent(in), value :: numbuff, buffsizes
            type(c_ptr), intent(out) :: buffer(numbuff)
            integer(c_int), intent(out) :: itemsizes(numbuff), count
        end subroutine

        pure subroutine c_regex_search(src, pattern, numbuff, buffsizes, &
            buffer, itemsizes, count) bind(C, name = "c_regex_search")
            use iso_c_binding
            character(kind = c_char), intent(in) :: src(*), pattern(*)
            integer(c_int), intent(in), value :: numbuff, buffsizes
            type(c_ptr), intent(out) :: buffer(numbuff)
            integer(c_int), intent(out) :: itemsizes(numbuff), count
        end subroutine

        pure subroutine c_regex_replace(src, pattern, rplc, buffsize, buffer, &
            nbuff) bind(C, name = "c_regex_replace")
            use iso_c_binding
            character(kind = c_char), intent(in) :: src(*), pattern(*), rplc(*)
            integer(c_int), intent(in), value :: buffsize
            character(kind = c_char), intent(out) :: buffer(*)
            integer(c_int), intent(out) :: nbuff
        end subroutine
    end interface

contains
! ------------------------------------------------------------------------------
    pure function str_get(this, i) result(rst)
        !! Gets the requested character from the string.
        class(string), intent(in) :: this
            !! The string object.
        integer(int32), intent(in) :: i
            !! The index of the character to retrieve.
        character :: rst
            !! The requested character.

        if (i < 1 .or. i > len(this)) then
            rst = c_null_char
        else
            rst = this%m_str(i:i)
        end if
    end function

! ------------------------------------------------------------------------------
    subroutine str_set(this, i, x)
        !! Replaces a character in the string.
        class(string), intent(inout) :: this
            !! The string object.
        integer(int32), intent(in) :: i
            !! The index of the character to replace.
        character, intent(in) :: x
            !! The character.

        if (i > 0 .and. i <= len(this)) then
            this%m_str(i:i) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    elemental subroutine char_to_string(lhs, rhs)
        !! Assigns a character array to a string type.
        type(string), intent(out) :: lhs
            !! The string object to which rhs is assigned.
        character(len = *), intent(in) :: rhs
            !! The string to copy.
        allocate(lhs%m_str, source = rhs)
    end subroutine

    ! -----------
    elemental subroutine string_to_char(lhs, rhs)
        !! Assigns a string to a character array.
        character(len = *), intent(out) :: lhs
            !! The string to which lhs is assigned.
        type(string), intent(in) :: rhs
            !! The string to copy.
        if (allocated(rhs%m_str)) then
            lhs = rhs%m_str
        else
            lhs = ""
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure elemental function string_eq_string(lhs, rhs) result(rst)
        !! Tests two strings for equality.
        type(string), intent(in) :: lhs
            !! The left-hand-side argument.
        type(string), intent(in) :: rhs
            !! The right-hand-side argument.
        logical :: rst
            !! True if the strings are equal; else, false.

        if (allocated(lhs%m_str)) then
            if (allocated(rhs%m_str)) then
                rst = lhs%m_str == rhs%m_str
            else
                rst = lhs%m_str == ""
            end if
        else
            if (allocated(rhs%m_str)) then
                rst = "" == rhs%m_str
            else
                rst = .true.
            end if
        end if
    end function

    ! ----------
    pure elemental function char_eq_string(lhs, rhs) result(rst)
        !! Tests two strings for equality.
        character(len = *), intent(in) :: lhs
            !! The left-hand-side argument.
        type(string), intent(in) :: rhs
            !! The right-hand-side argument.
        logical :: rst
            !! True if the strings are equal; else, false.

        if (allocated(rhs%m_str)) then
            rst = lhs == rhs%m_str
        else
            rst = lhs == ""
        end if
    end function

    ! ----------
    pure elemental function string_eq_char(lhs, rhs) result(rst)
        !! Tests two strings for equality.
        type(string), intent(in) :: lhs
            !! The left-hand-side argument.
        character(len = *), intent(in) :: rhs
            !! The right-hand-side argument.
        logical :: rst
            !! True if the strings are equal; else, false.

        if (allocated(lhs%m_str)) then
            rst = lhs%m_str == rhs
        else
            rst = "" == rhs
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental function string_neq_string(lhs, rhs) result(rst)
        !! Tests two strings for inequality.
        type(string), intent(in) :: lhs
            !! The left-hand-side argument.
        type(string), intent(in) :: rhs
            !! The right-hand-side argument.
        logical :: rst
            !! True if the strings are not equal; else, false.

        if (allocated(lhs%m_str)) then
            if (allocated(rhs%m_str)) then
                rst = lhs%m_str /= rhs%m_str
            else
                rst = lhs%m_str /= ""
            end if
        else
            if (allocated(rhs%m_str)) then
                rst = "" /= rhs%m_str
            else
                rst = .false.
            end if
        end if
    end function

    ! ----------
    pure elemental function char_neq_string(lhs, rhs) result(rst)
        !! Tests two strings for inequality.
        character(len = *), intent(in) :: lhs
            !! The left-hand-side argument.
        type(string), intent(in) :: rhs
            !! The right-hand-side argument.
        logical :: rst
            !! True if the strings are not equal; else, false.

        if (allocated(rhs%m_str)) then
            rst = lhs /= rhs%m_str
        else
            rst = lhs /= ""
        end if
    end function

    ! ----------
    pure elemental function string_neq_char(lhs, rhs) result(rst)
        !! Tests two strings for inequality.
        type(string), intent(in) :: lhs
            !! The left-hand-side argument.
        character(len = *), intent(in) :: rhs
            !! The right-hand-side argument.
        logical :: rst
            !! True if the strings are not equal; else, false.

        if (allocated(lhs%m_str)) then
            rst = lhs%m_str /= rhs
        else
            rst = "" /= rhs
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental function concat_strings(lhs, rhs) result(rst)
        !! Concatenates two strings.
        type(string), intent(in) :: lhs
            !! The left-hand-side string.
        type(string), intent(in) :: rhs
            !! The right-hand-side string.
        type(string) :: rst
            !! The resulting string.

        if (allocated(lhs%m_str)) then
            if (allocated(rhs%m_str)) then
                allocate(rst%m_str, source = lhs%m_str // rhs%m_str)
            else
                allocate(rst%m_str, source = lhs%m_str)
            end if
        else
            if (allocated(rhs%m_str)) then
                allocate(rst%m_str, source = rhs%m_str)
            else
                allocate(character(len = 0) :: rst%m_str)
            end if
        end if
    end function

    ! ----------
    pure elemental function concat_char_string(lhs, rhs) result(rst)
        !! Concatenates two strings.
        character(len = *), intent(in) :: lhs
            !! The left-hand-side string.
        type(string), intent(in) :: rhs
            !! The right-hand-side string.
        type(string) :: rst
            !! The resulting string.

        if (allocated(rhs%m_str)) then
            allocate(rst%m_str, source = lhs // rhs%m_str)
        else
            allocate(rst%m_str, source = lhs)
        end if
    end function

    ! ----------
    pure elemental function concat_string_char(lhs, rhs) result(rst)
        !! Concatenates two strings.
        type(string), intent(in) :: lhs
            !! The left-hand-side string.
        character(len = *), intent(in) :: rhs
            !! The right-hand-side string.
        type(string) :: rst
            !! The resulting string.

        if (allocated(lhs%m_str)) then
            allocate(rst%m_str, source = lhs%m_str // rhs)
        else
            allocate(rst%m_str, source = rhs)
        end if
    end function

! ------------------------------------------------------------------------------
    pure function string_len(x) result(rst)
        !! Returns the length of a string.
        type(string), intent(in) :: x
            !! The string to interrogate.
        integer(int32) :: rst
            !! The length of the string.
        
        if (allocated(x%m_str)) then
            rst = len(x%m_str)
        else
            rst = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure function string_len_trim(x) result(rst)
        !! Returns the length of a string.
        type(string), intent(in) :: x
            !! The string to interrogate.
        integer(int32) :: rst
            !! The length of the string.

        if (allocated(x%m_str)) then
            rst = len_trim(x%m_str)
        else
            rst = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure function split_string_char(txt, delim) result(rst)
        !! Splits a string by the supplied delimiter character.
        character(len = *), intent(in) :: txt
            !! The string on which to operate.
        character, intent(in) :: delim
            !! The delimiter character.
        type(string), allocatable, dimension(:) :: rst
            !! An array containing the resulting sub-strings.

        ! Local Variables
        integer(int32) :: i, j, nb, n, i1, i2
        integer(int32), allocatable, dimension(:) :: buffer, copy

        ! Initialization
        n = len_trim(txt)
        nb = 1024
        allocate(buffer(nb))

        ! Find all occurrences of delim in str
        j = 0
        do i = 1, n
            if (txt(i:i) == delim) then
                j = j + 1
                if (j > nb) then
                    allocate(copy(nb), source = buffer)
                    deallocate(buffer)
                    allocate(buffer(2 * nb))
                    buffer(1:nb) = copy
                    deallocate(copy)
                    nb = size(buffer)
                end if
                buffer(j) = i
            end if
        end do

        ! If j == 0, no delimiter was found - just return the original string
        if (j == 0) then
            allocate(rst(1))
            rst(1) = txt
            return
        end if

        ! If necessary, account for 1 additional spot in the event anything is 
        ! beyond the last delimiter
        if (buffer(j) == n) then
            ! The last character is the delimiter
            allocate(rst(j))
        else
            ! There is stuff beyond the last delimiter
            allocate(rst(j + 1))
        end if
        
        i1 = 1
        do i = 1, size(rst)
            if (i > j) then
                i2 = n
            else
                i2 = buffer(i) - 1 ! get the character before the delimiter
            end if
            rst(i) = txt(i1:i2)
            i1 = i2 + 2 ! index to the next character after the delimiter
        end do
    end function

    ! ----------
    pure function split_string_str(txt, delim) result(rst)
        !! Splits a string by the supplied delimiter character.
        type(string), intent(in) :: txt
            !! The string on which to operate.
        character, intent(in) :: delim
            !! The delimiter character.
        type(string), allocatable, dimension(:) :: rst
            !! An array containing the resulting sub-strings.

        ! Process
        rst = split_string_char(txt%m_str, delim)
    end function

! ------------------------------------------------------------------------------
    pure elemental function string_trim(x) result(rst)
        !! Trims any trailing blanks from the end of the string.
        type(string), intent(in) :: x
            !! The string on which to operate.
        type(string) :: rst
            !! The trimmed string.

        if (allocated(x%m_str)) then
            allocate(rst%m_str, source = trim(x%m_str))
        else
            allocate(character(len = 0) :: rst%m_str)
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental function char_convert_to_string(x) result(rst)
        !! Converts a Fortran character array to a string.
        character(len = *), intent(in) :: x
            !! The string to convert.
        type(string) :: rst
            !! The resulting string.

        allocate(rst%m_str, source = x)
    end function

    ! ----------
    pure elemental function int8_to_string(x) result(rst)
        !! Converts an 8-bit integer to a string.
        integer(int8), intent(in) :: x
            !! The value to convert.
        type(string) :: rst
            !! The resulting string.

        character(len = 4) :: buffer
        write(buffer, "(I0)") x
        allocate(rst%m_str, source = trim(adjustl(buffer)))
    end function

    ! ----------
    pure elemental function int16_to_string(x) result(rst)
        !! Converts a 16-bit integer to a string.
        integer(int16), intent(in) :: x
            !! The value to convert.
        type(string) :: rst
            !! The resulting string.

        character(len = 8) :: buffer
        write(buffer, "(I0)") x
        allocate(rst%m_str, source = trim(adjustl(buffer)))
    end function

    ! ----------
    pure elemental function int32_to_string(x) result(rst)
        !! Converts a 32-bit integer to a string.
        integer(int32), intent(in) :: x
            !! The value to convert.
        type(string) :: rst
            !! The resulting string.

        character(len = 16) :: buffer
        write(buffer, "(I0)") x
        allocate(rst%m_str, source = trim(adjustl(buffer)))
    end function

    ! ----------
    pure elemental function int64_to_string(x) result(rst)
        !! Converts a 64-bit integer to a string.
        integer(int64), intent(in) :: x
            !! The value to convert.
        type(string) :: rst
            !! The resulting string.

        character(len = 32) :: buffer
        write(buffer, "(I0)") x
        allocate(rst%m_str, source = trim(adjustl(buffer)))
    end function

    ! ----------
    pure elemental function real32_to_string(x, fmt) result(rst)
        !! Converts a 32-bit real to a string.
        real(real32), intent(in) :: x
            !! The value to convert.
        character(len = *), intent(in), optional :: fmt
            !! Optional: The format to employ.  If nothing is specified, G13.6
            !! is used.  If used, specify as "(G13.6)", or whatever appropriate
            !! format string is desired.  Regardless, be sure to include the
            !! parenthesis.
        type(string) :: rst
            !! The resulting string.

        character(len = 64) :: buffer
        if (present(fmt)) then
            write(buffer, fmt) x
        else
            write(buffer, "(G13.6)") x
        end if
        allocate(rst%m_str, source = trim(adjustl(buffer)))
    end function

    ! ----------
    pure elemental function real64_to_string(x, fmt) result(rst)
        !! Converts a 64-bit real to a string.
        real(real64), intent(in) :: x
            !! The value to convert.
        character(len = *), intent(in), optional :: fmt
            !! Optional: The format to employ.  If nothing is specified, G13.6
            !! is used.  If used, specify as "(G13.6)", or whatever appropriate
            !! format string is desired.  Regardless, be sure to include the
            !! parenthesis.
        type(string) :: rst
            !! The resulting string.

        character(len = 64) :: buffer
        if (present(fmt)) then
            write(buffer, fmt) x
        else
            write(buffer, "(G13.6)") x
        end if
        allocate(rst%m_str, source = trim(adjustl(buffer)))
    end function

    ! ----------
    pure elemental function real128_to_string(x, fmt) result(rst)
        !! Converts a 128-bit real to a string.
        real(real128), intent(in) :: x
            !! The value to convert.
        character(len = *), intent(in), optional :: fmt
            !! Optional: The format to employ.  If nothing is specified, G13.6
            !! is used.  If used, specify as "(G13.6)", or whatever appropriate
            !! format string is desired.  Regardless, be sure to include the
            !! parenthesis.
        type(string) :: rst
            !! The resulting string.

        character(len = 64) :: buffer
        if (present(fmt)) then
            write(buffer, fmt) x
        else
            write(buffer, "(G13.6)") x
        end if
        allocate(rst%m_str, source = trim(adjustl(buffer)))
    end function

    ! ----------
    pure function c_string_to_string(str, nchar) result(rst)
        !! Converts an array of C characters (C string) to a string.  The C
        !! string must be null-terminated.  The null-terimator character will
        !! not be included in the result.
        character(kind = c_char), intent(in) :: str(*)
            !! The C-string to convert.
        integer(int32), intent(in), optional :: nchar
            !! OPTIONAL: The maximum number of characters to consider in the
            !! conversion.  The default is 2048.
        type(string) :: rst

        integer(int32), parameter :: default_n = 2048
        integer(int32) :: i, j, n
        character(len = :), allocatable :: buffer
        if (present(nchar)) then
            if (nchar <= 0) then
                n = default_n
            else
                n = nchar
            end if
        else
            n = default_n
        end if
        allocate(character(len = n) :: buffer)
        j = 0
        do i = 1, n
            if (str(i) == c_null_char) exit
            j = j + 1
            buffer(j:j) = str(i)
        end do
        if (j /= 0) then
            allocate(rst%m_str, source = buffer(1:j))
        else
            allocate(character(len = 0) :: rst%m_str)
        end if
    end function

! ------------------------------------------------------------------------------
    pure function string_convert_to_char(x) result(rst)
        !! Converts a string to a Fortran character array.
        type(string), intent(in) :: x
            !! The string to convert.
        character(len = :), allocatable :: rst
            !! The resulting string.

        if (allocated(x%m_str)) then
            allocate(rst, source = x%m_str)
        else
            allocate(character(len = 0) :: rst)
        end if
    end function

! ------------------------------------------------------------------------------
    pure function char_to_c_string(x) result(rst)
        !! Converts a Fortran string to a C-compatible, null-terminated string.
        character(len = *), intent(in) :: x
            !! The string to convert.
        character(kind = c_char), allocatable, dimension(:) :: rst
            !! The resulting C string.

        integer(int32) :: i, n
        n = len_trim(x)
        allocate(rst(n + 1)) ! +1 for C null character
        do i = 1, n
            rst(i) = x(i:i)
        end do
        rst(n+1) = c_null_char
    end function

    ! ----------
    pure function string_to_c_string(x) result(rst)
        !! Converts a Fortran string to a C-compatible, null-terminated string.
        type(string), intent(in) :: x
            !! The string to convert.
        character(kind = c_char), allocatable, dimension(:) :: rst
            !! The resulting C string.

        rst = char_to_c_string(x%m_str)
    end function

! ------------------------------------------------------------------------------
    pure function string_to_int_char(x) result(rst)
        !! Converts a string to a 32-bit integer.
        character(len = *), intent(in) :: x
            !! The string to convert.
        integer(int32) :: rst
            !! The resulting integer.

        rst = atoi(to_c_string(x))
    end function

    ! ----------
    pure function string_to_int_string(x) result(rst)
        !! Converts a string to a 32-bit integer.
        type(string), intent(in) :: x
            !! The string to convert.
        integer(int32) :: rst
            !! The resulting integer.

        rst = atoi(to_c_string(x))
    end function

! ------------------------------------------------------------------------------
    pure function string_to_real_char(x) result(rst)
        !! Converts a string to a 64-bit real.
        character(len = *), intent(in) :: x
            !! The string to convert.
        real(real64) :: rst
            !! The resulting value.

        rst = atof(to_c_string(x))
    end function

    ! ----------
    pure function string_to_real_string(x) result(rst)
        !! Converts a string to a 64-bit real.
        type(string), intent(in) :: x
            !! The string to convert.
        real(real64) :: rst
            !! The resulting value.

        rst = atof(to_c_string(x))
    end function

! ------------------------------------------------------------------------------
    pure elemental function string_adjustl(x) result(rst)
        !! Adjusts the string to the left by removing any leading blanks and
        !! pushing any removed blanks onto the end of the string.
        type(string), intent(in) :: x
            !! The string to adjust.
        type(string) :: rst
            !! The adjusted string.

        if (allocated(x%m_str)) then
            allocate(rst%m_str, source = adjustl(x%m_str))
        else
            allocate(character(len = 0) :: rst%m_str)
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental function string_adjustr(x) result(rst)
        !! Adjusts the string to the right by removing any trailing blanks and
        !! pushing any removed blanks onto the front of the string.
        type(string), intent(in) :: x
            !! The string to adjust.
        type(string) :: rst
            !! The adjusted string.

        if (allocated(x%m_str)) then
            allocate(rst%m_str, source = adjustr(x%m_str))
        else
            allocate(character(len = 0) :: rst%m_str)
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental function string_index_string(str, substr, back) result(rst)
        !! Returns the starting position of a substring within a string.
        type(string), intent(in) :: str
            !! The string to search.
        type(string), intent(in) :: substr
            !! The substring for which to search.
        logical, intent(in), optional :: back
            !! OPTIONAL: If false, the return value is the start of the last 
            !! occurrence rather than the first; else, if true, the index is 
            !! the start of the first occurrence of the substring.
        integer(int32) :: rst
            !! The starting index of the substring.

        if (allocated(str%m_str)) then
            if (allocated(substr%m_str)) then
                rst = index(str%m_str, substr%m_str, back)
            else
                rst = index(str%m_str, "", back)
            end if
        else
            if (allocated(substr%m_str)) then
                rst = index("", substr%m_str, back)
            else
                rst = index("", "", back)
            end if
        end if
    end function

    ! ----------
    pure elemental function string_index_char(str, substr, back) result(rst)
        !! Returns the starting position of a substring within a string.
        type(string), intent(in) :: str
            !! The string to search.
        character(len = *), intent(in) :: substr
            !! The substring for which to search.
        logical, intent(in), optional :: back
            !! OPTIONAL: If false, the return value is the start of the last 
            !! occurrence rather than the first; else, if true, the index is 
            !! the start of the first occurrence of the substring.
        integer(int32) :: rst
            !! The starting index of the substring.

        if (allocated(str%m_str)) then
            rst = index(str%m_str, substr, back)
        else
            rst = index("", substr, back)
        end if
    end function

    ! ----------
    pure elemental function char_index_string(str, substr, back) result(rst)
        !! Returns the starting position of a substring within a string.
        character(len = *), intent(in) :: str
            !! The string to search.
        type(string), intent(in) :: substr
            !! The substring for which to search.
        logical, intent(in), optional :: back
            !! OPTIONAL: If false, the return value is the start of the last 
            !! occurrence rather than the first; else, if true, the index is 
            !! the start of the first occurrence of the substring.
        integer(int32) :: rst
            !! The starting index of the substring.

        if (allocated(substr%m_str)) then
            rst = index(str, substr%m_str, back)
        else
            rst = index(str, "", back)
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental function string_scan_string(str, substr, back) result(rst)
        !! Scans the string for any of the characters in the specified substring.
        type(string), intent(in) :: str
            !! The string to search.
        type(string), intent(in) :: substr
            !! The string for which to search.
        logical, intent(in), optional :: back
            !! OPTIONAL: If false, the return value is the position of the 
            !! leftmost character; else, if true, the rightmost position is
            !! returned.  
        integer(int32) :: rst
            !! The index of either the leftmost or rightmost character.

        if (allocated(str%m_str)) then
            if (allocated(substr%m_str)) then
                rst = scan(str%m_str, substr%m_str, back)
            else
                rst = scan(str%m_str, "", back)
            end if
        else
            if (allocated(substr%m_str)) then
                rst = scan("", substr%m_str, back)
            else
                rst = scan("", "", back)
            end if
        end if
    end function

    ! ----------
    pure elemental function string_scan_char(str, substr, back) result(rst)
        !! Scans the string for any of the characters in the specified substring.
        type(string), intent(in) :: str
            !! The string to search.
        character(len = *), intent(in) :: substr
            !! The string for which to search.
        logical, intent(in), optional :: back
            !! OPTIONAL: If false, the return value is the position of the 
            !! leftmost character; else, if true, the rightmost position is
            !! returned.  
        integer(int32) :: rst
            !! The index of either the leftmost or rightmost character.

        if (allocated(str%m_str)) then
            rst = scan(str%m_str, substr, back)
        else
            rst = scan("", substr)
        end if
    end function

    ! ----------
    pure elemental function char_scan_string(str, substr, back) result(rst)
        !! Scans the string for any of the characters in the specified substring.
        character(len = *), intent(in) :: str
            !! The string to search.
        type(string), intent(in) :: substr
            !! The string for which to search.
        logical, intent(in), optional :: back
            !! OPTIONAL: If false, the return value is the position of the 
            !! leftmost character; else, if true, the rightmost position is
            !! returned.  
        integer(int32) :: rst
            !! The index of either the leftmost or rightmost character.

        if (allocated(substr%m_str)) then
            rst = scan(str, substr%m_str, back)
        else
            rst = scan(str, "", back)
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental function remove_char(str, start, finish) result(rst)
        !! Removes a range of characters from the string.
        character(len = *), intent(in) :: str
            !! The string on which to operate.
        integer(int32), intent(in), optional :: start
            !! OPTIONAL: The starting index.  The default is 1.
        integer(int32), intent(in), optional :: finish
            !! OPTIONAL: The finishing index.  The default is len(str)
        type(string) :: rst
            !! The resulting string.

        integer(int32) :: s, f

        if (present(start)) then
            s = start
        else
            s = 1
        end if
        if (present(finish)) then
            f = finish
        else
            f = len(str)
        end if

        if (s > f) then
            allocate(rst%m_str, source = str)
        else
            allocate(rst%m_str, source = str(1:s-1) // str(f+1:len(str)))
        end if
    end function

    ! ----------
    pure elemental function remove_string(str, start, finish) result(rst)
        !! Removes a range of characters from the string.
        type(string), intent(in) :: str
            !! The string on which to operate.
        integer(int32), intent(in), optional :: start
            !! OPTIONAL: The starting index.  The default is 1.
        integer(int32), intent(in), optional :: finish
            !! OPTIONAL: The finishing index.  The default is len(str)
        type(string) :: rst
            !! The resulting string.

        if (allocated(str%m_str)) then
            rst = remove_char(str%m_str, start, finish)
        else
            rst = remove_char("", start, finish)
        end if
    end function

    ! ----------
    pure elemental function char_remove_char(str, substr) result(rst)
        !! Removes the first occurrence of the specified substring from the 
        !! string.
        character(len = *), intent(in) :: str
            !! The string from which to remove the substring.
        character(len = *), intent(in) :: substr
            !! The substring to remove.
        type(string) :: rst
            !! The resulting string.

        integer(int32) :: start, finish, n

        start = index(str, substr)
        n = len(substr)
        finish = min(start + n - 1, len(str))
        if (start < 1) then
            allocate(rst%m_str, source = str)
        else
            rst = remove(str, start = start, finish = finish)
        end if
    end function

    ! ----------
    pure elemental function char_remove_string(str, substr) result(rst)
        !! Removes the first occurrence of the specified substring from the 
        !! string.
        character(len = *), intent(in) :: str
            !! The string from which to remove the substring.
        type(string), intent(in) :: substr
            !! The substring to remove.
        type(string) :: rst
            !! The resulting string.

        if (allocated(substr%m_str)) then
            rst = char_remove_char(str, substr%m_str)
        else
            allocate(rst%m_str, source = str)
        end if
    end function

    ! ----------
    pure elemental function string_remove_char(str, substr) result(rst)
        !! Removes the first occurrence of the specified substring from the 
        !! string.
        type(string), intent(in) :: str
            !! The string from which to remove the substring.
        character(len = *), intent(in) :: substr
            !! The substring to remove.
        type(string) :: rst
            !! The resulting string.

        if (allocated(str%m_str)) then
            rst = char_remove_char(str%m_str, substr)
        else
            rst = char_remove_char("", substr)
        end if
    end function

    ! ----------
    pure elemental function string_remove_string(str, substr) result(rst)
        !! Removes the first occurrence of the specified substring from the 
        !! string.
        type(string), intent(in) :: str
            !! The string from which to remove the substring.
        type(string), intent(in) :: substr
            !! The substring to remove.
        type(string) :: rst
            !! The resulting string.

        if (allocated(str%m_str)) then
            if (allocated(substr%m_str)) then
                rst = char_remove_char(str%m_str, substr%m_str)
            else
                allocate(rst%m_str, source = str%m_str)
            end if
        else
            allocate(character(len = 0) :: rst%m_str)
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental function char_insert_char(str, start, substr) result(rst)
        !! Inserts a substring into another string at the specified index.
        character(len = *), intent(in) :: str
            !! The string into which the substring will be inserted.
        integer(int32), intent(in) :: start
            !! The starting index at which to insert the substring.
        character(len = *), intent(in) :: substr
            !! The substring to insert.
        type(string) :: rst
            !! The resulting string.

        if (start < 1) then
            allocate(rst%m_str, source = substr // str)
        else if (start > len(str)) then
            allocate(rst%m_str, source = str // substr)
        else
            allocate(rst%m_str, source = str(1:start-1) // substr // str(start:len(str)))
        end if
    end function

    ! ----------
    pure elemental function char_insert_string(str, start, substr) result(rst)
        !! Inserts a substring into another string at the specified index.
        character(len = *), intent(in) :: str
            !! The string into which the substring will be inserted.
        integer(int32), intent(in) :: start
            !! The starting index at which to insert the substring.
        type(string), intent(in) :: substr
            !! The substring to insert.
        type(string) :: rst
            !! The resulting string.

        if (allocated(substr%m_str)) then
            rst = char_insert_char(str, start, substr%m_str)
        else
            allocate(rst%m_str, source = str)
        end if
    end function

    ! ----------
    pure elemental function string_insert_char(str, start, substr) result(rst)
        !! Inserts a substring into another string at the specified index.
        type(string), intent(in) :: str
            !! The string into which the substring will be inserted.
        integer(int32), intent(in) :: start
            !! The starting index at which to insert the substring.
        character(len = *), intent(in) :: substr
            !! The substring to insert.
        type(string) :: rst
            !! The resulting string.

        if (allocated(str%m_str)) then
            rst = char_insert_char(str%m_str, start, substr)
        else
            allocate(rst%m_str, source = substr)
        end if
    end function

    ! ----------
    pure elemental function string_insert_string(str, start, substr) result(rst)
        !! Inserts a substring into another string at the specified index.
        type(string), intent(in) :: str
            !! The string into which the substring will be inserted.
        integer(int32), intent(in) :: start
            !! The starting index at which to insert the substring.
        type(string), intent(in) :: substr
            !! The substring to insert.
        type(string) :: rst
            !! The resulting string.

        if (allocated(str%m_str)) then
            if (allocated(substr%m_str)) then
                rst = char_insert_char(str%m_str, start, substr%m_str)
            else
                allocate(rst%m_str, source = str%m_str)
            end if
        else
            if (allocated(substr%m_str)) then
                allocate(rst%m_str, source = substr%m_str)
            else
                allocate(character(len = 0) :: rst%m_str)
            end if
        end if
    end function

! ------------------------------------------------------------------------------
    pure function to_upper_char(x) result(rst)
        !! Converts a string to all uppercase characters.  Any characters that 
        !! are already uppercase, a special symbol, or a numeric value are not
        !! modified.
        character(len = *), intent(in) :: x
            !! The string on which to operate.
        type(string) :: rst
            !! The resulting string.

        integer(int32) :: i, n, c
        n = len(x)
        allocate(character(len = n) :: rst%m_str)
        do i = 1, n
            c = iachar(x(i:i))
            if (c >= iachar("a") .and. c <= iachar("z")) then
                rst%m_str(i:i) = achar(c - 32)
            else
                rst%m_str(i:i) = x(i:i)
            end if
        end do
    end function

    ! ----------
    pure elemental function to_upper_string(x) result(rst)
        !! Converts a string to all uppercase characters.  Any characters that 
        !! are already uppercase, a special symbol, or a numeric value are not
        !! modified.
        type(string), intent(in) :: x
            !! The string on which to operate.
        type(string) :: rst
            !! The resulting string.

        if (allocated(x%m_str)) then
            ! allocate(rst%m_str, source = to_upper_char(x%m_str))
            rst = to_upper_char(x%m_str)
        else
            allocate(character(len = 0) :: rst%m_str)
        end if
    end function

! ------------------------------------------------------------------------------
    pure function to_lower_char(x) result(rst)
        !! Converts a string to all lowercase characters.  Any characters that 
        !! are already lowercase, a special symbol, or a numeric value are not
        !! modified.
        character(len = *), intent(in) :: x
            !! The string on which to operate.
        type(string) :: rst
            !! The resulting string.

        integer(int32) :: i, n, c
        n = len(x)
        allocate(character(len = n) :: rst%m_str)
        do i = 1, n
            c = iachar(x(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) then
                rst%m_str(i:i) = achar(c + 32)
            else
                rst%m_str(i:i) = x(i:i)
            end if
        end do
    end function

    ! ----------
    pure elemental function to_lower_string(x) result(rst)
        !! Converts a string to all lowercase characters.  Any characters that 
        !! are already lowercase, a special symbol, or a numeric value are not
        !! modified.
        type(string), intent(in) :: x
            !! The string on which to operate.
        type(string) :: rst
            !! The resulting string.

        if (allocated(x%m_str)) then
            ! allocate(rst%m_str, source = to_lower_char(x%m_str))
            rst = to_lower_char(x%m_str)
        else
            allocate(character(len = 0) :: rst%m_str)
        end if
    end function

! ******************************************************************************
! STRING_BUILDER ROUTINES
! ******************************************************************************
    subroutine sb_init(this)
        !! Initializes the string_builder object.
        class(string_builder), intent(inout) :: this
            !! The string_builder object.

        ! Process
        integer(int32), parameter :: bufferSize = 2048
        this%m_length = 0
        if (.not.allocated(this%m_buffer)) then
            allocate(character(len = bufferSize) :: this%m_buffer)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    subroutine sb_append_char(this, x)
        !! Appends a new string to the end of the existing string.
        class(string_builder), intent(inout) :: this
            !! The string_builder object.
        character(len = *), intent(in) :: x
            !! The string to append.

        ! Parameters Size
        integer(int32), parameter :: bufferSize = 2048

        ! Local Variables
        integer(int32) :: space, n, start, finish, nb
        character(len = :), allocatable :: temp

        ! Process
        if (.not.allocated(this%m_buffer)) call this%initialize()
        space = len(this%m_buffer) - this%m_length
        n = len(x)
        if (space < n) then
            nb = len(this%m_buffer) + max(n, bufferSize)
            allocate(temp, source = this%m_buffer(1:this%m_length))
            deallocate(this%m_buffer)
            allocate(character(len = nb) :: this%m_buffer)
            this%m_buffer(1:this%m_length) = temp
        end if
        start = this%m_length + 1
        finish = start + n - 1
        this%m_buffer(start:finish) = x
        this%m_length = this%m_length + n
    end subroutine

    ! ----------
    subroutine sb_append_string(this, x)
        !! Appends a new string to the end of the existing string.
        class(string_builder), intent(inout) :: this
            !! The string_builder object.
        type(string), intent(in) :: x
            !! The string to append.

        call sb_append_char(this, x%m_str)
    end subroutine

! ------------------------------------------------------------------------------
    pure function sb_to_string(this) result(rst)
        !! Returns the buffer contents as a string.
        class(string_builder), intent(in) :: this
            !! The string_builder object.
        type(string) :: rst
            !! The resulting string.

        if (allocated(this%m_buffer)) then
            if (this%m_length > 0) then
                allocate(rst%m_str, source = this%m_buffer(1:this%m_length))
            else
                allocate(character(len = 0) :: rst%m_str)
            end if
        else
            allocate(character(len = 0) :: rst%m_str)
        end if
    end function

! ------------------------------------------------------------------------------
    pure function sb_get_length(this) result(rst)
        !! Returns the length of the stored string.
        class(string_builder), intent(in) :: this
            !! The string_builder object.
        integer(int32) :: rst
            !! The lenght of the stored string.

        rst = this%m_length
    end function

! ******************************************************************************
! REGULAR EXPRESSION ROUTINES
! ******************************************************************************
    pure function char_regex_match_char(src, pattern) result(rst)
        !! Looks for sequences that match the requested pattern.  The entire
        !! target sequence must match the regular expression for this function 
        !! to succeed.
        character(len = *), intent(in) :: src
            !! The target string.
        character(len = *), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), allocatable, dimension(:) :: rst
            !! A list of all matching sequences.

        ! Parameters
        integer(c_int), parameter :: buffer_size = 2048
        integer(c_int), parameter :: buffer_count = 1024

        ! Local Variables
        integer(c_int) :: i, count
        type(c_ptr), allocatable, dimension(:) :: buffer
        integer(c_int), allocatable, dimension(:) :: sizeList
        character(kind = c_char, len = :), allocatable, target, &
            dimension(:) :: bufferStrings
        character(kind = c_char), allocatable, dimension(:) :: csrc, cpattern

        ! Initialization
        count = 0
        allocate(buffer(buffer_count))
        allocate(sizeList(buffer_count), source = 0)
        allocate(character(kind = c_char, len = buffer_size) :: &
            bufferStrings(buffer_count))
        do i = 1, buffer_count
            buffer(i) = c_loc(bufferStrings(i))
        end do

        ! Process
        allocate(csrc, source = to_c_string(src))
        allocate(cpattern, source = to_c_string(pattern))
        call c_regex_match(csrc, cpattern, buffer_count, buffer_size, buffer, &
            sizeList, count)

        allocate(rst(count))
        if (count > 0) then
            do i = 1, count
                rst(i) = c_string_to_string(bufferStrings(i), sizeList(i))
            end do
        end if
    end function

    ! -----------
    pure function char_regex_match_string(src, pattern) result(rst)
        !! Looks for sequences that match the requested pattern.  The entire
        !! target sequence must match the regular expression for this function 
        !! to succeed.
        character(len = *), intent(in) :: src
            !! The target string.
        type(string), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), allocatable, dimension(:) :: rst
            !! A list of all matching sequences.

        if (allocated(pattern%m_str)) then
            rst = char_regex_match_char(src, pattern%m_str)
        else
            rst = char_regex_match_char(src, "")
        end if
    end function

    ! -----------
    pure function string_regex_match_char(src, pattern) result(rst)
        !! Looks for sequences that match the requested pattern.  The entire
        !! target sequence must match the regular expression for this function 
        !! to succeed.
        type(string), intent(in) :: src
            !! The target string.
        character(len = *), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), allocatable, dimension(:) :: rst
            !! A list of all matching sequences.

        if (allocated(src%m_str)) then
            rst = char_regex_match_char(src%m_str, pattern)
        else
            rst = char_regex_match_char("", pattern)
        end if
    end function

    ! -----------
    pure function string_regex_match_string(src, pattern) result(rst)
        !! Looks for sequences that match the requested pattern.  The entire
        !! target sequence must match the regular expression for this function 
        !! to succeed.
        type(string), intent(in) :: src
            !! The target string.
        type(string), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), allocatable, dimension(:) :: rst
            !! A list of all matching sequences.

        if (allocated(src%m_str)) then
            if (allocated(pattern%m_str)) then
                rst = char_regex_match_char(src%m_str, pattern%m_str)
            else
                rst = char_regex_match_char(src%m_str, "")
            end if
        else
            if (allocated(pattern%m_str)) then
                rst = char_regex_match_char("", pattern%m_str)
            else
                rst = char_regex_match_char("", "")
            end if
        end if
    end function

! ------------------------------------------------------------------------------
    pure function char_regex_search_char(src, pattern) result(rst)
        !! Looks for sequences that match the requested pattern.
        character(len = *), intent(in) :: src
            !! The target string.
        character(len = *), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), allocatable, dimension(:) :: rst
            !! A list of all matching sequences.

        ! Parameters
        integer(c_int), parameter :: buffer_size = 2048
        integer(c_int), parameter :: buffer_count = 1024

        ! Local Variables
        integer(c_int) :: i, count
        type(c_ptr), allocatable, dimension(:) :: buffer
        integer(c_int), allocatable, dimension(:) :: sizeList
        character(kind = c_char, len = :), allocatable, target, &
            dimension(:) :: bufferStrings
        character(kind = c_char), allocatable, dimension(:) :: csrc, cpattern

        ! Initialization
        count = 0
        allocate(buffer(buffer_count))
        allocate(sizeList(buffer_count), source = 0)
        allocate(character(kind = c_char, len = buffer_size) :: &
            bufferStrings(buffer_count))
        do i = 1, buffer_count
            buffer(i) = c_loc(bufferStrings(i))
        end do

        ! Process
        allocate(csrc, source = to_c_string(src))
        allocate(cpattern, source = to_c_string(pattern))
        call c_regex_search(csrc, cpattern, buffer_count, buffer_size, buffer, &
            sizeList, count)

        allocate(rst(count))
        if (count > 0) then
            do i = 1, count
                rst(i) = c_string_to_string(bufferStrings(i), sizeList(i))
            end do
        end if
    end function

    ! ----------
    pure function char_regex_search_string(src, pattern) result(rst)
        !! Looks for sequences that match the requested pattern.
        character(len = *), intent(in) :: src
            !! The target string.
        type(string), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), allocatable, dimension(:) :: rst
            !! A list of all matching sequences.

        if (allocated(pattern%m_str)) then
            rst = char_regex_search_char(src, pattern%m_str)
        else
            rst = char_regex_search_char(src, "")
        end if
    end function

    ! ----------
    pure function string_regex_search_char(src, pattern) result(rst)
        !! Looks for sequences that match the requested pattern.
        type(string), intent(in) :: src
            !! The target string.
        character(len = *), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), allocatable, dimension(:) :: rst
            !! A list of all matching sequences.

        if (allocated(src%m_str)) then
            rst = char_regex_search_char(src%m_str, pattern)
        else
            rst = char_regex_search_char("", pattern)
        end if
    end function

    ! ----------
    pure function string_regex_search_string(src, pattern) result(rst)
        !! Looks for sequences that match the requested pattern.
        type(string), intent(in) :: src
            !! The target string.
        type(string), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), allocatable, dimension(:) :: rst
            !! A list of all matching sequences.

        if (allocated(src%m_str)) then
            if (allocated(pattern%m_str)) then
                rst = char_regex_search_char(src%m_str, pattern%m_str)
            else
                rst = char_regex_search_char(src%m_str, "")
            end if
        else
            if (allocated(pattern%m_str)) then
                rst = char_regex_search_char("", pattern%m_str)
            else
                rst = char_regex_search_char("", "")
            end if
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental function char_replace_char_char(src, pattern, substr) &
        result(rst)
        !! Replaces the substring identified by the supplied regular 
        !! expression pattern with a new substring.
        character(len = *), intent(in) :: src
            !! The string on which to operate.
        character(len = *), intent(in) :: pattern
            !! The regular expression pattern.
        character(len = *), intent(in) :: substr
            !! The new substring.
        type(string) :: rst
            !! The resulting string.

        ! Local Variables
        integer(c_int) :: count, bufferSize
        character(kind = c_char), allocatable, dimension(:) :: buffer, csrc, &
            cpattern, csubstr

        ! Initialization
        count = 0
        bufferSize = len(src) * len(substr)
        allocate(buffer(bufferSize), source = c_null_char)

        ! Process
        allocate(csrc, source = to_c_string(src))
        allocate(cpattern, source = to_c_string(pattern))
        allocate(csubstr, source = to_c_string(substr))
        call c_regex_replace(csrc, cpattern, csubstr, bufferSize, buffer, count)
        if (count > 0) then
            rst = to_string(buffer, count)
        else
            allocate(character(len = 0) :: rst%m_str)
        end if
    end function

    ! ----------
    pure elemental function char_replace_string_char(src, pattern, substr) &
        result(rst)
        !! Replaces the substring identified by the supplied regular 
        !! expression pattern with a new substring.
        character(len = *), intent(in) :: src
            !! The string on which to operate.
        type(string), intent(in) :: pattern
            !! The regular expression pattern.
        character(len = *), intent(in) :: substr
            !! The new substring.
        type(string) :: rst
            !! The resulting string.

        if (allocated(pattern%m_str)) then
            rst = replace(src, pattern%m_str, substr)
        else
            rst = replace(src, "", substr)
        end if
    end function

    ! ----------
    pure elemental function char_replace_char_string(src, pattern, substr) &
        result(rst)
        !! Replaces the substring identified by the supplied regular 
        !! expression pattern with a new substring.
        character(len = *), intent(in) :: src
            !! The string on which to operate.
        character(len = *), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), intent(in) :: substr
            !! The new substring.
        type(string) :: rst
            !! The resulting string.

        if (allocated(substr%m_str)) then
            rst = replace(src, pattern, substr%m_str)
        else
            rst = replace(src, pattern, "")
        end if
    end function

    ! ----------
    pure elemental function char_replace_string_string(src, pattern, substr) &
        result(rst)
        !! Replaces the substring identified by the supplied regular 
        !! expression pattern with a new substring.
        character(len = *), intent(in) :: src
            !! The string on which to operate.
        type(string), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), intent(in) :: substr
            !! The new substring.
        type(string) :: rst
            !! The resulting string.

        if (allocated(pattern%m_str)) then
            if (allocated(substr%m_str)) then
                rst = replace(src, pattern%m_str, substr%m_str)
            else
                rst = replace(src, pattern%m_str, "")
            end if
        else
            if (allocated(substr%m_str)) then
                rst = replace(src, "", substr%m_str)
            else
                rst = replace(src, "", "")
            end if
        end if
    end function

    ! ----------
    pure elemental function string_replace_char_char(src, pattern, substr) &
        result(rst)
        !! Replaces the substring identified by the supplied regular 
        !! expression pattern with a new substring.
        type(string), intent(in) :: src
            !! The string on which to operate.
        character(len = *), intent(in) :: pattern
            !! The regular expression pattern.
        character(len = *), intent(in) :: substr
            !! The new substring.
        type(string) :: rst
            !! The resulting string.

        if (allocated(src%m_str)) then
            rst = replace(src%m_str, pattern, substr)
        else
            rst = replace("", pattern, substr)
        end if
    end function

    ! ----------
    pure elemental function string_replace_string_char(src, pattern, substr) &
        result(rst)
        !! Replaces the substring identified by the supplied regular 
        !! expression pattern with a new substring.
        type(string), intent(in) :: src
            !! The string on which to operate.
        type(string), intent(in) :: pattern
            !! The regular expression pattern.
        character(len = *), intent(in) :: substr
            !! The new substring.
        type(string) :: rst
            !! The resulting string.

        if (allocated(src%m_str)) then
            if (allocated(pattern%m_str)) then
                rst = replace(src%m_str, pattern%m_str, substr)
            else
                rst = replace(src%m_str, "", substr)
            end if
        else
            if (allocated(pattern%m_str)) then
                rst = replace("", pattern%m_str, substr)
            else
                rst = replace("", "", substr)
            end if
        end if
    end function

    ! ----------
    pure elemental function string_replace_char_string(src, pattern, substr) &
        result(rst)
        !! Replaces the substring identified by the supplied regular 
        !! expression pattern with a new substring.
        type(string), intent(in) :: src
            !! The string on which to operate.
        character(len = *), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), intent(in) :: substr
            !! The new substring.
        type(string) :: rst
            !! The resulting string.

        if (allocated(src%m_str)) then
            if (allocated(substr%m_str)) then
                rst = replace(src%m_str, pattern, substr%m_str)
            else
                rst = replace(src%m_str, pattern, "")
            end if
        else
            if (allocated(substr%m_str)) then
                rst = replace("", pattern, substr%m_str)
            else
                rst = replace("", pattern, "")
            end if
        end if
    end function

    ! ----------
    pure elemental function string_replace_string_string(src, pattern, substr) &
        result(rst)
        !! Replaces the substring identified by the supplied regular 
        !! expression pattern with a new substring.
        type(string), intent(in) :: src
            !! The string on which to operate.
        type(string), intent(in) :: pattern
            !! The regular expression pattern.
        type(string), intent(in) :: substr
            !! The new substring.
        type(string) :: rst
            !! The resulting string.

        if (allocated(src%m_str)) then
            if (allocated(pattern%m_str)) then
                if (allocated(substr%m_str)) then
                    rst = replace(src%m_str, pattern%m_str, substr%m_str)
                else
                    rst = replace(src%m_str, pattern%m_str, "")
                end if
            else
                if (allocated(substr%m_str)) then
                    rst = replace(src%m_str, "", substr%m_str)
                else
                    rst = replace(src%m_str, "", "")
                end if
            end if
        else
            if (allocated(pattern%m_str)) then
                if (allocated(substr%m_str)) then
                    rst = replace("", pattern%m_str, substr%m_str)
                else
                    rst = replace("", pattern%m_str, "")
                end if
            else
                if (allocated(substr%m_str)) then
                    rst = replace("", "", substr%m_str)
                else
                    rst = replace("", "", "")
                end if
            end if
        end if
    end function

! ------------------------------------------------------------------------------
end module