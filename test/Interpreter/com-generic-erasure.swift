// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/ForeignCOM/ForeignCOM.c -c -o %t/ForeignCOM.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) -Xfrontend -enable-experimental-com-interop -enable-library-evolution -I %t -module-name Library -emit-module-path %t/Library.swiftmodule %S/../Inputs/COMGenericArguments.swift %S/../Inputs/COMGenericErasure.swift -L %t -lCOM %target-rpath(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %s %t/ForeignCOM.o -L %t -lLibrary -lCOM %target-rpath(%t) -o %t/test
// RUN: %target-codesign %t/test %t/%target-library-name(COM) %t/%target-library-name(Library)
// RUN: %target-run %t/test %t/%target-library-name(COM) %t/%target-library-name(Library) | %FileCheck %s
// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) -O -Xfrontend -enable-experimental-com-interop -enable-library-evolution -I %t -module-name Library -emit-module-path %t/Library.swiftmodule %S/../Inputs/COMGenericArguments.swift %S/../Inputs/COMGenericErasure.swift -L %t -lCOM %target-rpath(%t)
// RUN: %target-build-swift -O -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %s %t/ForeignCOM.o -L %t -lLibrary -lCOM %target-rpath(%t) -o %t/test-opt
// RUN: %target-codesign %t/test-opt %t/%target-library-name(Library)
// RUN: %target-run %t/test-opt %t/%target-library-name(COM) %t/%target-library-name(Library) | %FileCheck %s
// REQUIRES: executable_test

import Library
import ForeignCOM

@inline(never)
func packed<T: IItem>(_ value: T) -> [any IItem] {
  erasePack(value, value)
}

@inline(never)
func makeErased(_ initial: Int32) -> any IItem {
  let object = ForeignCOMObject_Create(initial)!
  let value = ForeignCOMObject_GetValueStorage(object)!.load(as: (any IExtended).self)
  ForeignCOMObject_Release(object)
  // Every path returns an independently owned result while borrowing value.
  precondition(erase(value).value(1) == initial + 1)
  precondition(eraseBorrowed(value).value(0) == initial)
  precondition(copyBorrowed(value).value(0) == initial)
  precondition(inherited(value).value(2) == initial + 2)
  precondition(inlineErase(value).value(0) == initial)
  precondition(refine(value).value(0) == initial)
  precondition(value.multiply(3) == initial * 3)
  let elements = packed(value)
  precondition(elements.count == 2)
  precondition(elements[1].value(4) == initial + 4)
  precondition(GetForeignCOMQueryInterfaceCalls() == 0)
  return erase(value)
}

@inline(never)
func checkBorrowed() {
  let result = makeErased(7)
  // The factory and all its source temporaries have gone away.
  precondition(result.value(0) == 7)
  precondition(GetForeignCOMReferenceCount() == 1)
  withExtendedLifetime(result) {}
}

@inline(never)
func checkConsumed() {
  let result = eraseConsumed(makeErased(9))
  precondition(result.value(0) == 9)
  precondition(GetForeignCOMReferenceCount() == 1)
  withExtendedLifetime(result) {}
}

@inline(never)
func makeClosure() -> () -> any IItem {
  captureErasure(makeErased(11))
}

@inline(never)
func checkClosure() {
  let closure = makeClosure()
  let result = closure()
  precondition(result.value(0) == 11)
  precondition(closure().value(1) == 12)
  withExtendedLifetime((closure, result)) {}
}

@inline(never)
func makeErasedClass() -> any IClassItem {
  let object = ForeignCOMObject_Create(13)!
  let value = ForeignCOMObject_GetPropertyStorage(object)!.load(as: (any IClassItem).self)
  ForeignCOMObject_Release(object)
  precondition(eraseClass(value).value == 13)
  value.value = 17
  return eraseClass(value)
}

@inline(never)
func checkClass() {
  let result = makeErasedClass()
  precondition(result.value == 17)
  precondition(GetForeignCOMReferenceCount() == 1)
  withExtendedLifetime(result) {}
}

func checkDestruction() {
  precondition(GetForeignCOMReferenceCount() == 0)
  precondition(GetForeignCOMDestructionCount() == 1)
  precondition(GetForeignCOMReleaseCalls() == GetForeignCOMAddRefCalls() + 1)
  precondition(GetForeignCOMQueryInterfaceCalls() == 0)
}

checkBorrowed()
checkDestruction()
checkConsumed()
checkDestruction()
checkClosure()
checkDestruction()
checkClass()
checkDestruction()
print("generic erasure balanced")
// CHECK: generic erasure balanced
