#ifndef CALLING_CONVENTIONS_H_
#define CALLING_CONVENTIONS_H_

int cdecl_fn(int a, int b, int c);

typedef int (*cdecl_fn_ptr_t)(int a, int b, int c);

extern cdecl_fn_ptr_t cdecl_fn_ptr;

int __attribute__((swiftcall)) swiftcc_fn(int a, int b, int c);

typedef int (__attribute__((swiftcall)) *swiftcc_fn_ptr_t)(int a, int b, int c);

extern swiftcc_fn_ptr_t swiftcc_fn_ptr;

#endif /* CALLING_CONVENTIONS_H_ */
