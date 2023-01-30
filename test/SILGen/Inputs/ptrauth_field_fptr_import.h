#ifndef TEST_C_FUNCTION
#define TEST_C_FUNCTION

struct SecureStruct {
  int (*__ptrauth(2, 0, 88)(secure_func_ptr1))();
  int (*__ptrauth(3, 0, 66)(secure_func_ptr2))();
};

struct SecureStruct *ptr_to_secure_struct;
#endif
