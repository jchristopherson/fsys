module fsys_test_routines
    use fsys
    use strings
    use iso_fortran_env
    implicit none
contains
! ------------------------------------------------------------------------------
    function test_split_path() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: drive = "C:"
        character(len = *), parameter :: directory = "/Program Files/CMake/bin/"
        character(len = *), parameter :: filename = "cmake"
        character(len = *), parameter :: extension = ".exe"
        character(len = *), parameter :: str = drive // directory // filename // extension

        ! Local Variables
        type(file_path) :: x

        ! Initialization
        rst = .true.

        ! Test 1
        x = split_path(str)
        if (x%drive /= drive .and. x%drive /= "") then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_path -1"
        end if
        if (x%directory /= directory .and. x%directory /= drive // directory) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_path -2"
        end if
        if (x%filename /= filename) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_path -3"
        end if
        if (x%extension /= extension) then
            rst = .false.
            print "(A)", "TEST FAILED: test_split_path -4"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_get_directory_contents() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        type(string) :: wd
        type(directory_contents) :: dc
        type(file_path) :: path
        integer(int32) :: i
        logical :: check

        ! Use the working directory as our test
        wd = get_current_work_directory()
        
        ! Get the directory contents - look for the executable as a check
        dc = get_directory_contents(wd)
        rst = .false.
        do i = 1, size(dc%files)
            path = split_path(dc%files(i))
            if (path%filename == "fsys_test" .or. path%filename == "CMakeLists") then
                rst = .true.
                exit
            end if
        end do
        if (.not.rst) then
            print "(A)", "TEST FAILED: test_get_directory_contents -1"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_find_all_files() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        type(string) :: wd, ext
        type(directory_contents) :: dc
        type(file_path) :: path
        type(string), allocatable, dimension(:) :: list
        integer(int32) :: i

        ! Initialization
        rst = .true.

        ! Get the current working directory
        wd = get_current_work_directory()

        ! Get the directory contents
        dc = get_directory_contents(wd)
        if (size(dc%files) < 1) return

        ! Use the file extension from the first file in the available list
        path = split_path(dc%files(1))
        ext = path%extension

        ! Now, look for a list of files with the specified extension
        list = find_all_files(wd, ext, .true.) ! look in any subfolders

        ! Ensure at least 1 file
        if (size(list) < 1) then
            rst = .false.
            print "(A)", "TEST FAILED: test_find_all_files -1"
        end if
    end function

! ------------------------------------------------------------------------------
end module