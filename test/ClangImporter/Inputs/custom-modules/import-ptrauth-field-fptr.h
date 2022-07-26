#ifndef TEST_C_FUNCTION
#define TEST_C_FUNCTION

struct SecureStruct {
  int (*__ptrauth(1, 0, 50)(secure_func_ptr))();
};

extern struct SecureStruct *ptr_to_secure_struct;
#endif
