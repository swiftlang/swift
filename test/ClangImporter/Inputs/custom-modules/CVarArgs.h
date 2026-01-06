#include <stdarg.h>
extern void takeVaList(va_list args);

extern void takeVaList2(__builtin_va_list args);

typedef __builtin_va_list my_builtin_va_list_alias;
extern void takeVaList3(my_builtin_va_list_alias args);
