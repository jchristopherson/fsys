program main
    use fsys_test_routines
    implicit none

    if (.not.test_split_path()) stop 1
    if (.not.test_get_directory_contents()) stop 2
    if (.not.test_find_all_files()) stop 3
end program