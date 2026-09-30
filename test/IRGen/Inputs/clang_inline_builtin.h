// A stand-in for the _FORTIFY_SOURCE definitions that a C standard library
// header provides. Each of these functions is a library builtin that the header
// redefines with GNU inline semantics, routing the call to a bounds checking
// _chk variant.
//
// Clang gives such a function ("inline builtin", see
// FunctionDecl::isInlineBuiltinDeclaration) special treatment: the unsuffixed
// symbol keeps referring to the library function, and the inline body is
// emitted into a private `<name>.inline` clone that direct calls are redirected
// to.

#include <stdarg.h>
#include <stddef.h>

#define FORTIFY_INLINE                                                         \
  __attribute__((__gnu_inline__, __always_inline__)) extern __inline__

#define BOS(p) __builtin_object_size(p, 0)

void *__memccpy_chk(void *, const void *, int, size_t, size_t);
void *__memcpy_chk(void *, const void *, size_t, size_t);
void *__memmove_chk(void *, const void *, size_t, size_t);
void *__memset_chk(void *, int, size_t, size_t);
char *__stpcpy_chk(char *, const char *, size_t);
char *__stpncpy_chk(char *, const char *, size_t, size_t);
char *__strcat_chk(char *, const char *, size_t);
char *__strcpy_chk(char *, const char *, size_t);
size_t __strlcat_chk(char *, const char *, size_t, size_t);
size_t __strlcpy_chk(char *, const char *, size_t, size_t);
char *__strncat_chk(char *, const char *, size_t, size_t);
char *__strncpy_chk(char *, const char *, size_t, size_t);
int __vsnprintf_chk(char *restrict, size_t, int, size_t, const char *restrict,
                    va_list);
int __vsprintf_chk(char *restrict, int, size_t, const char *restrict, va_list);

void *memccpy(void *, const void *, int, size_t);
void *memcpy(void *, const void *, size_t);
void *memmove(void *, const void *, size_t);
void *memset(void *, int, size_t);
char *stpcpy(char *, const char *);
char *stpncpy(char *, const char *, size_t);
char *strcat(char *, const char *);
char *strcpy(char *, const char *);
size_t strlcat(char *, const char *, size_t);
size_t strlcpy(char *, const char *, size_t);
char *strncat(char *, const char *, size_t);
char *strncpy(char *, const char *, size_t);
int snprintf(char *restrict, size_t, const char *restrict, ...);
int sprintf(char *restrict, const char *restrict, ...);
int vsnprintf(char *restrict, size_t, const char *restrict, va_list);
int vsprintf(char *restrict, const char *restrict, va_list);

FORTIFY_INLINE void *memccpy(void *dst, const void *src, int c, size_t n) {
  return __memccpy_chk(dst, src, c, n, BOS(dst));
}

FORTIFY_INLINE void *memcpy(void *dst, const void *src, size_t n) {
  return __memcpy_chk(dst, src, n, BOS(dst));
}

FORTIFY_INLINE void *memmove(void *dst, const void *src, size_t n) {
  return __memmove_chk(dst, src, n, BOS(dst));
}

FORTIFY_INLINE void *memset(void *dst, int c, size_t n) {
  return __memset_chk(dst, c, n, BOS(dst));
}

FORTIFY_INLINE char *stpcpy(char *dst, const char *src) {
  return __stpcpy_chk(dst, src, BOS(dst));
}

FORTIFY_INLINE char *stpncpy(char *dst, const char *src, size_t n) {
  return __stpncpy_chk(dst, src, n, BOS(dst));
}

FORTIFY_INLINE char *strcat(char *dst, const char *src) {
  return __strcat_chk(dst, src, BOS(dst));
}

FORTIFY_INLINE char *strcpy(char *dst, const char *src) {
  return __strcpy_chk(dst, src, BOS(dst));
}

FORTIFY_INLINE size_t strlcat(char *dst, const char *src, size_t n) {
  return __strlcat_chk(dst, src, n, BOS(dst));
}

FORTIFY_INLINE size_t strlcpy(char *dst, const char *src, size_t n) {
  return __strlcpy_chk(dst, src, n, BOS(dst));
}

FORTIFY_INLINE char *strncat(char *dst, const char *src, size_t n) {
  return __strncat_chk(dst, src, n, BOS(dst));
}

FORTIFY_INLINE char *strncpy(char *dst, const char *src, size_t n) {
  return __strncpy_chk(dst, src, n, BOS(dst));
}

// Real C standard libraries fortify the variadic printf functions with macros,
// because neither Clang nor Swift can forward a `...` parameter. They are
// spelled out here anyway to cover the inline builtin handling for a variadic
// function; Swift cannot call them, so there is no call site to check.
FORTIFY_INLINE int snprintf(char *restrict dst, size_t n,
                            const char *restrict fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int result = __vsnprintf_chk(dst, n, 0, BOS(dst), fmt, ap);
  va_end(ap);
  return result;
}

FORTIFY_INLINE int sprintf(char *restrict dst, const char *restrict fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int result = __vsprintf_chk(dst, 0, BOS(dst), fmt, ap);
  va_end(ap);
  return result;
}

FORTIFY_INLINE int vsnprintf(char *restrict dst, size_t n,
                             const char *restrict fmt, va_list ap) {
  return __vsnprintf_chk(dst, n, 0, BOS(dst), fmt, ap);
}

FORTIFY_INLINE int vsprintf(char *restrict dst, const char *restrict fmt,
                            va_list ap) {
  return __vsprintf_chk(dst, 0, BOS(dst), fmt, ap);
}
