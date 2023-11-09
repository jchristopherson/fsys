#ifndef FSYS_H__
#define FSYS_H__
#ifdef __cplusplus
extern "C" {
#endif

void c_split_file_path(const char *path, char *drive, char *dir, char *fname, 
                       char *ext);

void c_get_directory_contents(const char *dir, int nbuffers, int bufferSize, 
                              char **fnames, int *nnames, int *nameLengths, 
                              char **dirnames, int *ndir, int *dirLengths,
                              bool *success);

void c_get_current_work_directory(char *buffer, int buffsize, bool *success);

#ifdef __cplusplus
}
#endif
#endif