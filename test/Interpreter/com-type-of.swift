// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/COMIdentity/COMIdentity.c -c -o %t/COMIdentity.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/COMIdentity -module-name M %S/Inputs/COMIdentity.swift %t/main.swift %t/COMIdentity.o -L %t -lCOM %target-rpath(%t) -o %t/test
// RUN: %target-codesign %t/test %t/%target-library-name(COM)
// RUN: %target-run %t/test %t/%target-library-name(COM) | %FileCheck %s
// RUN: %target-build-swift -O -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/COMIdentity -module-name M %S/Inputs/COMIdentity.swift %t/main.swift %t/COMIdentity.o -L %t -lCOM %target-rpath(%t) -o %t/test-opt
// RUN: %target-codesign %t/test-opt %t/%target-library-name(COM)
// RUN: %target-run %t/test-opt %t/%target-library-name(COM) | %FileCheck %s
// REQUIRES: executable_test

import COMIdentity

@inline(never)
func dynamicType(_ source: borrowing any ISource) -> Any.Type {
  type(of: source)
}

@inline(never)
func discardedDynamicType(_ source: borrowing any ISource) {
  _ = type(of: source)
}

@inline(never)
func repeatedDynamicType(_ source: borrowing any ISource) -> (Any.Type, Any.Type) {
  (type(of: source), type(of: source))
}

@inline(never)
func checkNative() {
  let source = makeIdentity(NativeObject())
  precondition(dynamicType(source) == NativeObject.self)
  precondition(COMIdentityObject_GetQueries() == 1)
  discardedDynamicType(source)
  precondition(COMIdentityObject_GetQueries() == 2)
  let pair = repeatedDynamicType(source)
  precondition(pair.0 == NativeObject.self && pair.1 == NativeObject.self)
  precondition(COMIdentityObject_GetQueries() == 4)
}
checkNative()
checkIdentityDestruction(1)

@inline(never)
func checkForeign() {
  let source = makeIdentity(NativeObject(), supportsIdentity: false)
  precondition(dynamicType(source) == (any ISource).self)
  precondition(COMIdentityObject_GetQueries() == 1)
  discardedDynamicType(source)
  precondition(COMIdentityObject_GetQueries() == 2)
  let pair = repeatedDynamicType(source)
  precondition(pair.0 == (any ISource).self && pair.1 == (any ISource).self)
  precondition(COMIdentityObject_GetQueries() == 4)
}
checkForeign()
checkIdentityDestruction(2)
print("COM dynamic types passed")
// CHECK: COM dynamic types passed
