#ifndef TEST_C_FUNCTION
#define TEST_C_FUNCTION

int returnInt() { return 111; }

struct SecureStruct {
  int (*__ptrauth(1, 0, 88)(secure_func_ptr))();
};

struct AddressDiscriminatedSecureStruct {
  int (*__ptrauth(1, 1, 88)(secure_func_ptr))();
};

struct SecureStruct *ptr_to_secure_struct;
struct AddressDiscriminatedSecureStruct
    *ptr_to_addr_discriminated_secure_struct;
#endif
