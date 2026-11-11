#ifndef TEST_PTRAUTH_FIELD_DATA_PTR_IMPORT
#define TEST_PTRAUTH_FIELD_DATA_PTR_IMPORT

struct DataPointerStruct {
  void *__ptrauth(2, 0, 77) data_ptr;
};

struct AddressDiscriminatedDataPointerStruct {
  void *__ptrauth(2, 1, 77) data_ptr;
};

struct NonnullPointerStruct {
  void *_Nonnull __ptrauth(2, 1, 77) data_ptr;
  int (*_Nonnull __ptrauth(1, 1, 88) func_ptr)(void);
};

struct DataPointerStruct *ptr_to_data_pointer_struct;
struct AddressDiscriminatedDataPointerStruct
    *ptr_to_addr_discriminated_data_pointer_struct;
struct NonnullPointerStruct *ptr_to_nonnull_pointer_struct;

#endif
