// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/library.swift -module-name ThrowingLibrary -o %t/ThrowingLibrary.swiftmodule -I %t/Inputs -O -enable-default-cmo -enable-library-evolution -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -disable-cxx-interop-requirement-at-import
// RUN: not %target-swift-frontend -typecheck %t/client.swift -I %t -enable-experimental-feature CxxExceptionBridging 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -emit-ir %t/client.swift -o %t/disabled.ll -I %t -I %t/Inputs -O -enable-default-cmo -enable-experimental-feature CxxExceptionBridging 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck %t/client.swift -I %t -I %t/Inputs -enable-experimental-feature CxxExceptionBridging -disable-cxx-interop-requirement-at-import -experimental-allow-module-with-compiler-errors 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %t/client.swift -o %t/enabled.ll -I %t -I %t/Inputs -O -enable-default-cmo -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

// Diagnose the C++ requirement before loading dependencies, even when the
// producer or consumer disables the ordinary interoperability import check.
// Without this gate, the optimized Swift-only client can crash while loading
// the serialized exception adapter or reconstruct it with the wrong type.
// CHECK: error: module 'ThrowingLibrary' was built with C++ interoperability enabled, but current compilation does not enable C++ interoperability
// CHECK: note: visit https://www.swift.org/documentation/cxx-interop/project-build-setup to learn how to enable C++ interoperability

//--- Inputs/module.modulemap
module ThrowingFunctions {
  header "functions.h"
}

//--- Inputs/functions.h
// This header deliberately works in C, so its language requirement cannot
// prevent the consumer from reaching serialized Swift exception bridges.
#ifdef __cplusplus
inline int checkedValue(int value)
    __attribute__((swift_attr("import_throws"))) {
  if (value < 0)
    throw value;
  return value + 1;
}
#else
int checkedValue(int value);
#endif

//--- library.swift
import ThrowingFunctions

@inlinable public func directWrapper(_ value: CInt) throws -> CInt {
  try checkedValue(value)
}

@inlinable public func capturedWrapper(_ value: CInt) throws -> CInt {
  let operation: (CInt) throws -> CInt = checkedValue
  return try operation(value)
}

//--- client.swift
import ThrowingLibrary

public func direct(_ value: CInt) throws -> CInt {
  try directWrapper(value)
}

public func captured(_ value: CInt) throws -> CInt {
  try capturedWrapper(value)
}
