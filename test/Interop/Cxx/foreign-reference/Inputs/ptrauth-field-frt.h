#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_PTRAUTH_FIELD_FRT_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_PTRAUTH_FIELD_FRT_H

#include <ptrauth.h>

// A pointer field signed with the DA key, address-diversified, with a constant
// discriminator -- what xnu's OS_PTRAUTH_SIGNED_PTR("...") expands to on arm64e.
#define SIGNED_PTR \
  __ptrauth(ptrauth_key_process_independent_data, 1, \
            ptrauth_string_discriminator("RefBox.p"))

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:PtrAuthFRTRetain")))
__attribute__((swift_attr("release:PtrAuthFRTRelease")))
RefBox {
  // A ptrauth-qualified data pointer field -- the subject of the bug.
  int * _Nullable SIGNED_PTR p;

  // A ptrauth-qualified C function pointer field.
  int (* _Nullable __ptrauth(ptrauth_key_function_pointer, 1, 77) fn)(void);

  // A non-nullable ptrauth-qualified data pointer field (imported as a
  // non-optional type).
  int * _Nonnull SIGNED_PTR nn;

  // A plain (unqualified) int field, for control comparison.
  int plain;

  virtual ~RefBox() {}
};

inline void PtrAuthFRTRetain(RefBox * _Nonnull) {}
inline void PtrAuthFRTRelease(RefBox * _Nonnull) {}

// A plain C struct with the same signed field, for comparison.
struct CBox {
  int * _Nullable SIGNED_PTR p;
};

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_PTRAUTH_FIELD_FRT_H
