// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// A library built with the flag off serializes a stored_property component,
// which the projector must not turn into a raw field access.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Lib \
// RUN:   -I %t -disable-import-ptrauth-field-function-pointers -o %t/Lib.swiftmodule
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all %t/client.swift \
// RUN:   -module-name client -I %t | %FileCheck %t/client.swift

// REQUIRES: CPU=arm64e

//--- module.modulemap
module CBox {
  header "header.h"
  export *
}

//--- header.h
#include <ptrauth.h>

struct Box {
  // Not address-diversified, so importable with the flag off.
  void *_Nullable __ptrauth(ptrauth_key_process_independent_data, 0, 1234) p;
};

//--- Lib.swift
import CBox

@inlinable
public func makeKeyPath() -> WritableKeyPath<Box, UnsafeMutableRawPointer?> {
  return \Box.p
}

//--- client.swift
import CBox
import Lib

// Applied through the runtime, not projected.
// CHECK-LABEL: sil @$s6client4readySvSgSo3BoxVF :
// CHECK:   keypath
// CHECK-NOT: struct_element_addr {{.*}} #Box.p
// CHECK-LABEL: } // end sil function
public func read(_ b: Box) -> UnsafeMutableRawPointer? {
  return b[keyPath: makeKeyPath()]
}
