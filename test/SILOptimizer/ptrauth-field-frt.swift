// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift -O -cxx-interoperability-mode=default \
// RUN:   -import-objc-header %t/header.h %t/test.swift \
// RUN:   -Xfrontend -sil-verify-all -emit-sil -o /dev/null

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

// Regression test for rdar://190639894:
// Verify that ptrauth-qualified fields of C++ foreign reference types
// correctly emit begin_access [signed] through the full optimization pipeline,
// and that passes such as DeadAccessScopeElimination or key path folding do
// not incorrectly remove or bypass the signed access scope (which would crash
// the SILVerifier).

//--- header.h

#include <ptrauth.h>

#define SIGNED_PTR \
  __ptrauth(ptrauth_key_process_independent_data, 1, \
            ptrauth_string_discriminator("Box.fn"))

#define SIGNED_DATA_PTR \
  __ptrauth(ptrauth_key_process_independent_data, 1, \
            ptrauth_string_discriminator("Box.p"))

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:BoxRetain")))
__attribute__((swift_attr("release:BoxRelease")))
Box {
  int (* _Nullable SIGNED_PTR fn)(void);
  int * _Nullable SIGNED_DATA_PTR p;
  virtual ~Box() {}
};

inline void BoxRetain(Box * _Nonnull) {}
inline void BoxRelease(Box * _Nonnull) {}

//--- test.swift

public func readField(_ b: Box) -> Int32 {
  if let f = b.fn { return f() }
  return -1
}

public func writeField(_ b: Box, _ f: @convention(c) () -> Int32) {
  b.fn = f
}

public func readDataField(_ b: Box) -> Int32 {
  return unsafe b.p?.pointee ?? -1
}

public func writeDataField(_ b: Box, _ q: UnsafeMutablePointer<Int32>) {
  unsafe b.p = q
}

public func readKeyPath(_ b: Box) -> UnsafeMutablePointer<Int32>? {
  return unsafe b[keyPath: \Box.p]
}

public func writeKeyPath(_ b: Box, _ q: UnsafeMutablePointer<Int32>?) {
  unsafe b[keyPath: \Box.p] = q
}

public func readFnKeyPath(_ b: Box) -> (@convention(c) () -> Int32)? {
  return b[keyPath: \Box.fn]
}
