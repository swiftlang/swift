#ifndef SWIFT_ERROR_CXX_FUNCTIONS_H
#define SWIFT_ERROR_CXX_FUNCTIONS_H

@import Foundation;

// The throwing variant covers C functions only, so each of these keeps its
// error parameter.

bool sec_cxx_global(int x, NSError **err);

namespace SECNS {
bool sec_in_namespace(int x, NSError **err);
}

template <class T>
bool sec_template(T *out, NSError **err);

struct SECStruct {
  bool sec_method(int x, NSError **err);
};

// A declaration with C language linkage is a C function even in a C++ header.
extern "C" bool sec_extern_c(int x, NSError **err);

#endif
