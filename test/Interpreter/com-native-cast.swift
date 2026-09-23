// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/COMIdentity/COMIdentity.c -c -o %t/COMIdentity.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -I %S/Inputs/COMIdentity -module-name M -emit-module-path %t/M.swiftmodule %S/Inputs/COMIdentity.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name Casts -sil-verify-all -emit-object %S/../Inputs/COMNativeCasts.sil -o %t/casts.o
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/COMIdentity -module-name M %S/Inputs/COMIdentity.swift %t/main.swift %t/COMIdentity.o %t/casts.o -L %t -lCOM %target-rpath(%t) -o %t/test
// RUN: %target-codesign %t/test %t/%target-library-name(COM)
// RUN: %target-run %t/test %t/%target-library-name(COM) | %FileCheck %s
// RUN: %target-not-crash %target-run %t/test fail %t/%target-library-name(COM) 2>&1 | %FileCheck %s --check-prefix=FAILURE
// RUN: %target-build-swift -O -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/COMIdentity -module-name M %S/Inputs/COMIdentity.swift %t/main.swift %t/COMIdentity.o %t/casts.o -L %t -lCOM %target-rpath(%t) -o %t/test-opt
// RUN: %target-codesign %t/test-opt %t/%target-library-name(COM)
// RUN: %target-run %t/test-opt %t/%target-library-name(COM) | %FileCheck %s
// RUN: %target-not-crash %target-run %t/test-opt fail %t/%target-library-name(COM) 2>&1 | %FileCheck %s --check-prefix=FAILURE
// REQUIRES: executable_test

import COMIdentity

@_silgen_name("com_native_conditional")
func scalarConditional(_ source: consuming any ISource) -> NativeBase?
@_silgen_name("com_native_forced")
func scalarForced(_ source: consuming any ISource) -> NativeBase
@_silgen_name("com_native_optional")
func scalarOptional(_ source: consuming (any ISource)?) -> NativeBase?
@_silgen_name("com_native_protocol")
func scalarProtocol(_ source: consuming any ISource) -> (any NativeMarker)?

if CommandLine.arguments.contains("fail") {
  let source = makeIdentity(NativeObject(), supportsIdentity: false)
  _ = scalarForced(source)
  fatalError("unexpected cast success")
}
// FAILURE: Could not cast value of type
// FAILURE-SAME: NativeBase
// FAILURE-NOT: unexpected cast success

@inline(never)
func checkNative() {
  let object = NativeObject()
  let source = makeIdentity(object)
  precondition((source as? NativeObject) === object)
  precondition((source as! NativeBase) === object)
  precondition((source as? any NativeMarker) === object)
  precondition((source as? Unrelated) == nil)
  precondition(scalarConditional(source) === object)
  precondition(scalarForced(source) === object)
  precondition(scalarOptional(source) === object)
  precondition(scalarProtocol(source) === object)
  precondition(COMIdentityObject_GetQueries() >= 8)
  precondition(COMIdentityObject_GetReferences() >= 1)
}
checkNative()
checkIdentityDestruction(1)

@inline(never)
func checkForeign() {
  let source = makeIdentity(NativeObject(), supportsIdentity: false)
  precondition((source as? NativeObject) == nil)
  precondition(scalarConditional(source) == nil)
  precondition(scalarOptional(source) == nil)
  precondition(scalarProtocol(source) == nil)
}
checkForeign()
checkIdentityDestruction(2)
precondition(scalarOptional(nil) == nil)

@inline(never)
func recoverOwned() -> NativeBase {
  let source = makeIdentity(NativeObject())
  return scalarForced(source)
}
@inline(never)
func checkOwned() {
  let result = recoverOwned()
  // The interface adapter is gone; the recovered result owns the Swift object.
  precondition(COMIdentityObject_GetReferences() == 0)
  precondition(COMIdentityObject_GetDestructions() == 1)
  precondition(NativeObject.destructions == 2)
  withExtendedLifetime(result) {}
}
checkOwned()
checkIdentityDestruction(3)
print("COM native casts passed")
// CHECK: COM native casts passed
