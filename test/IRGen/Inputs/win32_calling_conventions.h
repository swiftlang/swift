#ifndef STDCALL_FUNCTION_H_
#define STDCALL_FUNCTION_H_

int __stdcall stdcall_fn(int a, int b, int c);

typedef int (__stdcall *stdcall_fn_ptr_t)(int a, int b, int c);

extern stdcall_fn_ptr_t stdcall_fn_ptr;

int __fastcall fastcall_fn(int a, int b, int c);

typedef int (__fastcall *fastcall_fn_ptr_t)(int a, int b, int c);

extern fastcall_fn_ptr_t fastcall_fn_ptr;

int __vectorcall vectorcall_fn(int a, int b, int c);

typedef int (__vectorcall *vectorcall_fn_ptr_t)(int a, int b, int c);

extern vectorcall_fn_ptr_t vectorcall_fn_ptr;

#endif /* STDCALL_FUNCTION_H_ */
