#ifndef TEST_C_FUNCTION
#define TEST_C_FUNCTION

int returnInt() { return 111; }
struct SecureStruct {
  int (*__ptrauth(2, 0, 88)(secure_func_ptr))();
};

struct SecureStruct *ptr_to_secure_struct;
#endif
