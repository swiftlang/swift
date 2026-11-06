#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_PTRAUTH_FIELD_FRT_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_PTRAUTH_FIELD_FRT_H

#include <ptrauth.h>

// Signed with the DA key, address-diversified, with a constant discriminator
#define SIGNED_PTR \
  __ptrauth(ptrauth_key_process_independent_data, 1, \
            ptrauth_string_discriminator("RefBox.p"))

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:PtrAuthFRTRetain")))
__attribute__((swift_attr("release:PtrAuthFRTRelease")))
RefBox {
  int * _Nullable SIGNED_PTR p;
  int (* _Nullable __ptrauth(ptrauth_key_function_pointer, 1, 77) fn)(void);
  int * _Nonnull SIGNED_PTR nn;
  int plain;

  virtual ~RefBox() {}
};

inline void PtrAuthFRTRetain(RefBox * _Nonnull) {}
inline void PtrAuthFRTRelease(RefBox * _Nonnull) {}

struct CBox {
  int * _Nullable SIGNED_PTR p;
};

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_PTRAUTH_FIELD_FRT_H
