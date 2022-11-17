#ifndef _WCHAR_H_
#define _WCHAR_H_

typedef union {
       char            __mbstate8[128];
       long long       _mbstateL;                      /* for alignment */
} __mbstate_t;

typedef __mbstate_t mbstate_t;
wchar_t        *wcschr(const wchar_t *, wchar_t);
wchar_t        *wcspbrk(const wchar_t *, const wchar_t *);
wchar_t        *wcsrchr(const wchar_t *, wchar_t);
wchar_t        *wcsstr(const wchar_t * __restrict, const wchar_t * __restrict);
wchar_t        *wmemchr(const wchar_t *, wchar_t, size_t);

#endif /* !_WCHAR_H_ */
