#ifndef __COMPARE_STRING_H__
#define __COMPARE_STRING_H__

static inline int
compare_string(const char* str1,
               const char* str2) {
    int i = 0;
    while (str1[i] == str2[i] && str1[i] && str2[i])
        i++;

    return str1[i] == str2[i];
}

#endif //__COMPARE_STRING_H__