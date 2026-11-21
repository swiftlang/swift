// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift -O -import-objc-header %t/header.h %t/test.swift \
// RUN:   -Xfrontend -sil-verify-all -emit-sil -o /dev/null
// RUN: %target-build-swift -Onone -import-objc-header %t/header.h %t/test.swift \
// RUN:   -Xfrontend -sil-verify-all -emit-sil -o /dev/null

// REQUIRES: CPU=arm64e

//--- header.h

#include <ptrauth.h>

struct Box {
  void *_Nullable __ptrauth(ptrauth_key_process_independent_data, 1, 1234) p;
  void (*_Nullable __ptrauth(ptrauth_key_function_pointer, 1, 5678) f)(void);
};

//--- test.swift

public func readKeyPath(_ b: Box) -> UnsafeMutableRawPointer? {
  return b[keyPath: \Box.p]
}
public func writeKeyPath(_ b: inout Box, _ p: UnsafeMutableRawPointer?) {
  b[keyPath: \Box.p] = p
}
public func readFunctionKeyPath(_ b: Box) -> (@convention(c) () -> Void)? {
  return b[keyPath: \Box.f]
}

// Keep the key path application for SILCombine.
@inline(never)
public func readKeyPathNeverInline(_ b: Box) -> UnsafeMutableRawPointer? {
  return b[keyPath: \Box.p]
}
@inline(never)
public func writeKeyPathNeverInline(_ b: inout Box, _ p: UnsafeMutableRawPointer?) {
  b[keyPath: \Box.p] = p
}

public func readGenericKeyPath<T>(_ t: T, _ kp: KeyPath<T, UnsafeMutableRawPointer?>) -> UnsafeMutableRawPointer? {
  return t[keyPath: kp]
}
public func callGeneric(_ b: Box) -> UnsafeMutableRawPointer? {
  return readGenericKeyPath(b, \Box.p)
}
