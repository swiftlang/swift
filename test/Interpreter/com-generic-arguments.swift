// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/ForeignCOM/ForeignCOM.c -c -o %t/ForeignCOM.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) -Xfrontend -enable-experimental-com-interop -enable-library-evolution -I %t -module-name Library -emit-module-path %t/Library.swiftmodule %S/../Inputs/COMGenericArguments.swift -L %t -lCOM %target-rpath(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %s %t/ForeignCOM.o -L %t -lLibrary -lCOM %target-rpath(%t) -o %t/test
// RUN: %target-codesign %t/test %t/%target-library-name(COM) %t/%target-library-name(Library)
// RUN: %target-run %t/test %t/%target-library-name(COM) %t/%target-library-name(Library) | %FileCheck %s
// RUN: %target-build-swift -O -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %s %t/ForeignCOM.o -L %t -lLibrary -lCOM %target-rpath(%t) -o %t/test-opt
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt %t/%target-library-name(COM) %t/%target-library-name(Library) | %FileCheck %s
// REQUIRES: executable_test

import Library
import ForeignCOM

@inline(never)
func exercise<T: IExtended>(_ value: T) {
  let width = MemoryLayout<UnsafeRawPointer>.size
  precondition(size(value) == width)
  precondition(forward(value) == width)
  precondition(Holder(value).size() == width)
  precondition(Owner(value).size() == width)
  precondition(capture(value)() == width)
  precondition(forwardPack(value, value) == 2 * width)
}

@inline(never)
func exerciseClass(_ object: OpaquePointer) {
  let property = ForeignCOMObject_GetPropertyStorage(object)!.load(as: (any IClassItem).self)
  precondition(captureClass(property)() == MemoryLayout<UnsafeRawPointer>.size)
  withExtendedLifetime(property) {}
}

@inline(never)
func run() {
  let object = ForeignCOMObject_Create(42)!
  let value = ForeignCOMObject_GetValueStorage(object)!.load(as: (any IExtended).self)
  ForeignCOMObject_Release(object)
  exercise(value)
  exerciseClass(object)
  precondition(GetForeignCOMReferenceCount() == 1)
  withExtendedLifetime(value) {}
}
run()
precondition(GetForeignCOMReferenceCount() == 0)
precondition(GetForeignCOMDestructionCount() == 1)
precondition(GetForeignCOMReleaseCalls() == GetForeignCOMAddRefCalls() + 1)
print("generic arguments balanced")
// CHECK: generic arguments balanced
