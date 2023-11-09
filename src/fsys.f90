module fsys
    !! Provides a collection of types and routines for system-related 
    !! operations.
    use strings
    use iso_c_binding
    use iso_fortran_env
    implicit none
    private
    public :: file_path
    public :: directory_contents
    public :: split_path
    public :: get_directory_contents
    public :: get_current_work_directory

    type file_path
        !! Defines a container for parts of a file path.
        type(string) :: drive
            !! The drive.
        type(string) :: directory
            !! The directory.
        type(string) :: filename
            !! The filename.
        type(string) :: extension
            !! The extension.  Notice, the '.' is included (e.g. ".txt").
    end type

    type directory_contents
        !! Defines a container describing the contents of a directory.
        type(string), allocatable, dimension(:) :: files
            !! A list of files in the directory.
        type(string), allocatable, dimension(:) :: subdirectories
            !! A list of sub-directories.
        type(string) :: directory
            !! The directory containing the aforementioned contents.
    end type

! ------------------------------------------------------------------------------
    interface split_path
        !! Splits the supplied path into components.
        module procedure :: split_path_char
        module procedure :: split_path_string
    end interface

    interface get_directory_contents
        !! Returns the contents of the specified directory.
        module procedure :: get_directory_contents_char
        module procedure :: get_directory_contents_string
    end interface

! ------------------------------------------------------------------------------
    interface ! fsys.h
        pure subroutine c_split_file_path(path, drive, dir, fname, ext) &
            bind(C, name = "c_split_file_path")
            use iso_c_binding, only : c_char
            character(kind = c_char), intent(in) :: path(*)
            character(kind = c_char), intent(out) :: drive(*), dir(*), &
                fname(*), ext(*)
        end subroutine

        pure subroutine c_get_directory_contents(dir, nbuffers, bufferSize, &
            fnames, nnames, nameLengths, dirNames, ndir, dirLengths, success) &
            bind(C, name = "c_get_directory_contents")
            use iso_c_binding
            character(kind = c_char), intent(in) :: dir(*)
            integer(c_int), intent(in), value :: nbuffers, bufferSize
            integer(c_int), intent(out) :: ndir, nnames
            type(c_ptr), intent(out) :: fnames(*), dirNames(*)
            integer(c_int), intent(out) :: nameLengths(*), dirLengths(*)
            logical(c_bool), intent(out) :: success
        end subroutine

        pure subroutine c_get_current_work_directory(buffer, buffsize, &
            success) bind(C, name = "c_get_current_work_directory")
            use iso_c_binding
            character(kind = c_char), intent(out) :: buffer(*)
            integer(c_int), intent(in), value :: buffsize
            logical(c_bool), intent(out) :: success
        end subroutine
    end interface

contains
! ------------------------------------------------------------------------------
    pure elemental function split_path_char(path) result(rst)
        !! Splits the supplied path into components.
        character(len = *), intent(in) :: path
            !! The path to split.
        type(file_path) :: rst
            !! The resulting file path components.

        ! Local Variables
        character(kind = c_char), dimension(2048) :: cdrive, cdir, cfname, cext

        ! Process
        call c_split_file_path(to_c_string(path), cdrive, cdir, cfname, cext)
        rst%drive = to_string(cdrive)
        rst%directory = to_string(cdir)
        rst%filename = to_string(cfname)
        rst%extension = to_string(cext)
    end function

    ! ----------
    pure elemental function split_path_string(path) result(rst)
        !! Splits the supplied path into components.
        type(string), intent(in) :: path
            !! The path to split.
        type(file_path) :: rst
            !! The resulting file path components.

        rst = split_path_char(char(path))
    end function

! ------------------------------------------------------------------------------
    pure function get_directory_contents_char(dir) result(rst)
        !! Returns the contents of the specified directory.
        character(len = *), intent(in) :: dir
            !! The directory to search.
        type(directory_contents) :: rst
            !! The directory contents.

        ! Parameters
        integer(c_int), parameter :: bufferCount = 2048
        integer(c_int), parameter :: bufferSize = 2048

        ! Local Variables
        logical(c_bool) :: check
        integer(c_int) :: i, n, nnames, ndir, nameLengths(bufferCount), &
            dirLengths(bufferCount)
        character(kind = c_char, len = :), allocatable, dimension(:), &
            target :: nameBuffer, dirBuffer
        type(c_ptr), dimension(bufferCount) :: namePtr, dirPtr

        ! Initialization
        allocate(character(kind = c_char, len = bufferSize) :: nameBuffer(bufferCount))
        allocate(character(kind = c_char, len = bufferSize) :: dirBuffer(bufferCount))
        do i = 1, bufferCount
            namePtr(i) = c_loc(nameBuffer(i))
            dirPtr(i) = c_loc(dirBuffer(i))
        end do

        ! Process
        call c_get_directory_contents(to_c_string(dir), bufferCount, &
            bufferSize, namePtr, nnames, nameLengths, dirPtr, ndir, &
            dirLengths, check)

        rst%directory = to_string(dir)
        if (check) then
            allocate(rst%files(nnames))
            do i = 1, nnames
                n = nameLengths(i)
                rst%files(i) = to_string(nameBuffer(i)(1:n))
            end do

            allocate(rst%subdirectories(ndir))
            do i = 1, ndir
                n = dirLengths(i)
                rst%subdirectories(i) = to_string(dirBuffer(i)(1:n))
            end do
        else
            allocate(rst%files(0))
            allocate(rst%subdirectories(0))
        end if
    end function

    ! ----------
    pure function get_directory_contents_string(dir) result(rst)
        !! Returns the contents of the specified directory.
        type(string), intent(in) :: dir
            !! The directory to search.
        type(directory_contents) :: rst
            !! The directory contents.

        rst = get_directory_contents_char(char(dir))
    end function

! ------------------------------------------------------------------------------
    pure function get_current_work_directory() result(rst)
        !! Gets the current working directory.
        type(string) :: rst
            !! The working directory.

        ! Local Variables
        integer(c_int), parameter :: bufferSize = 2048
        character(kind = c_char) :: buffer(bufferSize)
        logical(c_bool) :: success

        ! Process
        call c_get_current_work_directory(buffer, bufferSize, success)
        if (success) then
            rst = to_string(buffer);
        else
            rst = to_string("")
        end if
    end function

! ------------------------------------------------------------------------------
    pure recursive function find_all_files_char_char(dir, ext, sub) &
        result(rst)
        !! Finds all files with the specified extension within a directory.
        character(len = *), intent(in) :: dir
            !! The parent directory to search.
        character(len = *), intent(in) :: ext
            !! The extension to match.  The extension must include a '.'
            !! character (e.g. ".txt").
        logical, intent(in), optional :: sub
            !! OPTIONAL: Set to true to search any subdirectories as well.
            !! If false, only the parent directory will be searched.  The 
            !! default is false such that only the parent directory will be 
            !! searched.
        type(string), allocatable, dimension(:) :: rst

        ! Local Variables
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module