#include "fsys.h"
#include <cstdlib>
#include <cwchar>
#include <vector>
#include <cstring>
#include "helper_macros.h"

void strip_extension(char *fname, char *ext, char sep) ;

#ifdef WIN32

#include <windows.h>
#include <windowsx.h>
#include <shobjidl.h>
#include <direct.h>

bool get_dir_contents_win32(const char *dir, int nbuffers, int bufferSize, 
    char **fnames, int *nnames, int *nameLengths, char **dirnames, int *ndir, 
    int *dirLengths);

#define GETCWD _getcwd
#define GETDIRCONTENTS get_dir_contents_win32

#else

#include <unistd.h>
#include <dirent.h>
#include <libgen.h>

bool get_dir_contents_posix(const char *dir, int nbuffers, int bufferSize,
    char **fnames, int *nnames, int *nameLengths, char **dirnames, int *ndir,
    int *dirLengths);

#define GETCWD getcwd
#define GETDIRCONTENTS get_dir_contents_posix

#endif

using namespace std;

////////////////////////////////////////////////////////////////////////////////

void c_split_file_path(const char *path, char *drive, char *dir, char *fname, 
                       char *ext)
{
#ifdef WIN32
    _splitpath(path, drive, dir, fname, ext);
#else
    *drive = '\0';
    *dir = '\0';
    *fname = '\0';
    *ext = '\0';

    char *cpath1, *cpath2;
    cpath1 = (char*)malloc((size_t)((strlen(path) + 1) * sizeof(char)));
    if (cpath1 == NULL) return;
    strcpy(cpath1, path);

    cpath2 = (char*)malloc((size_t)((strlen(path) + 1) * sizeof(char)));
    if (cpath2 == NULL) return;
    strcpy(cpath2, path);

    strcpy(dir, dirname(cpath1));
    strcat(dir, "/");
    strcpy(fname, basename(cpath2));
    strip_extension(fname, ext, '.');

    free(cpath1);
    free(cpath2);
#endif
}

void c_get_directory_contents(const char *dir, int nbuffers, int bufferSize, 
                              char **fnames, int *nnames, int *nameLengths, 
                              char **dirnames, int *ndir, int *dirLengths, 
                              bool *success)
{
    *success = GETDIRCONTENTS(dir, nbuffers, bufferSize, fnames, 
        nnames, nameLengths, dirnames, ndir, dirLengths);
}

//https://stackoverflow.com/questions/30279228/is-there-an-alternative-to-getcwd-in-fortran-2003-2008
void c_get_current_work_directory(char *buffer, int buffsize, bool *success)
{
    *success = GETCWD(buffer, buffsize) == buffer;
}

////////////////////////////////////////////////////////////////////////////////
#ifdef WIN32

bool get_dir_contents_win32(const char *dir, int nbuffers, int bufferSize, 
    char **fnames, int *nnames, int *nameLengths, char **dirnames, int *ndir, 
    int *dirLengths)
{
    // Local Variables
    const size_t SPATH_SIZE = 2048;
    WIN32_FIND_DATA fdFile;
    HANDLE hFind = NULL;
    wchar_t spath[SPATH_SIZE];
    size_t n;

    // Initialize the output variables
    *nnames = 0;
    *ndir = 0;

    // Get everything in the directory
    n = strlen(dir);
    if (dir[n-1] == 92) swprintf(spath, SPATH_SIZE, L"%s*.*", dir);
    else swprintf(spath, SPATH_SIZE, L"%s\\*.*", dir);
    if ((hFind = FindFirstFile(spath, &fdFile)) == INVALID_HANDLE_VALUE) {
        // ERROR: Path not found
        return false;
    }

    do {
        // FindFirstFile always returns '.' and '..'
        size_t fdFileSize = wcslen(fdFile.cFileName);
        if (wcsncmp(fdFile.cFileName, L".", fdFileSize) != 0 && 
            wcsncmp(fdFile.cFileName, L"..", fdFileSize) != 0) 
        {
            // Build up the file path using the passed-in info
            n = strlen(dir);
            if (dir[n-1] == 92) 
                swprintf(spath, SPATH_SIZE, L"%s%ls", dir, fdFile.cFileName);
            else 
                swprintf(spath, SPATH_SIZE, L"%s\\%ls", dir, fdFile.cFileName);

            // Is the entry a file, or a folder
            if (fdFile.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
                // We've got a directory
                if (*ndir >= nbuffers) continue;
                size_t lstr = MIN(wcslen(spath), bufferSize);
                dirLengths[*ndir] = (int)wcstombs(dirnames[*ndir], spath, lstr);
                *ndir += 1;
            }
            else {
                // We've got a file
                if (*nnames >= nbuffers) continue;
                size_t lstr = MIN(wcslen(spath), bufferSize);
                nameLengths[*nnames] = 
                    (int)wcstombs(fnames[*nnames], spath, lstr);
                *nnames += 1;
            }
        }
    } while (FindNextFile(hFind, &fdFile));

    // Clean up after ourselves
    FindClose(hFind);

    // End
    return true;
}

#else

// https://stackoverflow.com/questions/4204666/how-to-list-files-in-a-directory-in-a-c-program
// https://man7.org/linux/man-pages/man3/readdir.3.html
bool get_dir_contents_posix(const char *dir, int nbuffers, int bufferSize,
    char **fnames, int *nnames, int *nameLengths, char **dirnames, int *ndir,
    int *dirLengths)
{
    size_t mn, n;
    DIR *d;
    struct dirent *ddir;
    d = opendir(dir);
    *nnames = 0;
    *ndir = 0;
    if (d) {
        while ((ddir = readdir(d)) != NULL) {
            if (ddir->d_type == DT_DIR) {
                // Directory
                if (*ndir >= nbuffers) continue;
                n = strlen(ddir->d_name);
                mn = MIN((size_t)bufferSize, n);
                strncpy(
                    dirnames[*ndir],
                    ddir->d_name,
                    mn
                );
                dirLengths[*ndir] = mn;
                *ndir += 1;
            }
            else if (ddir->d_type == DT_REG) {
                // File
                if (*nnames >= nbuffers) continue;
                n = strlen(ddir->d_name);
                mn = MIN((size_t)bufferSize, n);
                strncpy(
                    fnames[*nnames],
                    ddir->d_name,
                    mn
                );
                nameLengths[*nnames] = mn;
                *nnames += 1;
            }
        }
    }

    // End
    return true;
}

#endif

void strip_extension(char *fname, char *ext, char sep) 
{
    // Find the last 'sep' character
    char *lastSep = strrchr(fname, sep);
    *ext = '\0';
    if (lastSep != NULL) {
        strcpy(ext, lastSep);
        *lastSep = '\0'; // modifies fname as lastSep is a pointer to the appropriate location in fname
    }
}