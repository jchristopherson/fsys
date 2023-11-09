module fsys
    !! Provides a collection of types and routines for system-related 
    !! operations.
    implicit none
    private
    public :: file_path

    type file_path
        !! Defines a container for parts of a file path.
        character(len = :), allocatable :: drive
        !! The drive.
        character(len = :), allocatable :: directory
        !! The directory.
        character(len = :), allocatable :: filename
        !! The filename.
        character(len = :), allocatable :: extension
        !! The extension.  Notice, the '.' is included (e.g. ".txt").
    end type

contains
! ------------------------------------------------------------------------------
end module