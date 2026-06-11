void plain_func(int x);

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))
void buffered_func(const int *__counted_by(len) __noescape p, int len);
void buffered_func_no_anno(const int *p, int len);
