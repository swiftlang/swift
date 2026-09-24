// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-sil %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/use.sil
// RUN: %target-swift-frontend -emit-ir -O %t/use.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/use.ll

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module UnnamedThrowingFunctions {
  header "unnamed.h"
  requires cplusplus
}

//--- unnamed.h
#define SWIFT_THROWS __attribute__((swift_attr("import_throws")))
inline int unnamed(int, int value) SWIFT_THROWS { return value; }
struct Functions {
  static int unnamed(int, int value) SWIFT_THROWS { return value; }
};

//--- use.swift
import UnnamedThrowingFunctions

public func callUnnamed(_ value: CInt) throws -> CInt {
  try unnamed(0, value) + Functions.unnamed(0, value)
}

public func capturedUnnamed(_ value: CInt) throws -> CInt {
  let free = unnamed
  let member = Functions.unnamed
  return try free(0, value) + member(0, value)
}
