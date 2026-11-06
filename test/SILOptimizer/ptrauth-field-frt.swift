// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift -O -cxx-interoperability-mode=default \
// RUN:   -import-objc-header %t/header.h %t/test.swift \
// RUN:   -Xfrontend -sil-verify-all -emit-sil -o /dev/null
// RUN: %target-build-swift -Onone -cxx-interoperability-mode=default \
// RUN:   -import-objc-header %t/header.h %t/test.swift \
// RUN:   -Xfrontend -sil-verify-all -emit-sil -o /dev/null

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

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

struct CStruct {
  int (* _Nullable SIGNED_PTR fn)(void);
  int * _Nullable SIGNED_DATA_PTR p;
};

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

// @inline(never) prevents the early inliner from consuming the key path
// getter/setter before SILCombine
@inline(never)
public func readKeyPathNeverInline(_ b: Box) -> UnsafeMutablePointer<Int32>? {
  return unsafe b[keyPath: \Box.p]
}
@inline(never)
public func writeKeyPathNeverInline(_ b: Box, _ q: UnsafeMutablePointer<Int32>?) {
  unsafe b[keyPath: \Box.p] = q
}

// C struct ptrauth fields
public func readCStructKeyPath(_ s: CStruct) -> UnsafeMutablePointer<Int32>? {
  return unsafe s[keyPath: \CStruct.p]
}
public func writeCStructKeyPath(_ s: inout CStruct, _ q: UnsafeMutablePointer<Int32>?) {
  unsafe s[keyPath: \CStruct.p] = q
}
@inline(never)
public func readCStructKeyPathNeverInline(_ s: CStruct) -> UnsafeMutablePointer<Int32>? {
  return unsafe s[keyPath: \CStruct.p]
}
@inline(never)
public func writeCStructKeyPathNeverInline(_ s: inout CStruct, _ q: UnsafeMutablePointer<Int32>?) {
  unsafe s[keyPath: \CStruct.p] = q
}

// Closure capture
public func makeFRTReader(_ b: Box) -> () -> Int32 {
  return { unsafe b.p?.pointee ?? -1 }
}

// Generic key paths
public func readGenericKeyPath<T>(_ t: T, _ kp: KeyPath<T, UnsafeMutablePointer<Int32>?>) -> UnsafeMutablePointer<Int32>? {
  return t[keyPath: kp]
}
