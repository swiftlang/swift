// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/ForeignCOM/ForeignCOM.c -c -o %t/ForeignCOM.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %S/Inputs/ForeignCOM.swift %t/main.swift %t/ForeignCOM.o -L %t -lCOM %target-rpath(%t) -o %t/test
// RUN: %target-codesign %t/test %t/%target-library-name(COM)
// RUN: %target-run %t/test %t/%target-library-name(COM) | %FileCheck %s
// RUN: %target-build-swift -O -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %S/Inputs/ForeignCOM.swift %t/main.swift %t/ForeignCOM.o -L %t -lCOM %target-rpath(%t) -o %t/test-opt
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt %t/%target-library-name(COM) | %FileCheck %s
// REQUIRES: executable_test

import ForeignCOM

@inline(never)
func read(_ interface: borrowing any IValue) -> Int32 {
  interface.value(0)
}

@inline(never)
func makeCapture() -> () -> Int32 {
  let interface = makeValue(11)
  return { read(interface) }
}

func exerciseCapture() {
  let closure = makeCapture()
  precondition(closure() == 11)
  withExtendedLifetime(closure) {
    precondition(GetForeignCOMReferenceCount() > 0)
    precondition(GetForeignCOMDestructionCount() == 0)
  }
}

exerciseCapture()
checkDestruction()
print("capture balanced")
// CHECK: capture balanced

@inline(never)
func makeArray() -> [any IValue] {
  let interface = makeValue(22)
  return [interface, interface]
}

func exerciseArray() {
  let original = makeArray()
  var copy = original
  copy.removeLast()
  precondition(copy.count == 1)
  precondition(read(copy[0]) == 22)
  precondition(original.count == 2)
  precondition(original.reduce(0) { $0 + read($1) } == 44)
  // Keep the original buffer alive across mutation so this requires a
  // distinct buffer and exercises the COM element value witnesses.
  withExtendedLifetime(original) {
    withExtendedLifetime(copy) {
      precondition(GetForeignCOMDestructionCount() == 0)
    }
  }
}

exerciseArray()
checkDestruction()
print("array copy balanced")
// CHECK-NEXT: array copy balanced

@inline(never)
func makeDictionary() -> [Int: any IValue] {
  [1: makeValue(33)]
}

func exerciseDictionary() {
  let original = makeDictionary()
  var copy = original
  let removed = copy.removeValue(forKey: 1)!
  precondition(copy.isEmpty)
  precondition(read(removed) == 33)
  precondition(read(original[1]!) == 33)
  withExtendedLifetime(original) {
    withExtendedLifetime(removed) {
      precondition(GetForeignCOMDestructionCount() == 0)
    }
  }
}

exerciseDictionary()
checkDestruction()
print("dictionary copy balanced")
// CHECK-NEXT: dictionary copy balanced

@inline(never)
func makeOptional() -> (any IValue)? {
  makeValue(44)
}

func exerciseOptional() {
  var original = makeOptional()
  let copy = original
  original = nil
  precondition(original == nil)
  precondition(read(copy!) == 44)
  withExtendedLifetime(copy) {
    precondition(GetForeignCOMDestructionCount() == 0)
  }
}

exerciseOptional()
checkDestruction()
print("optional copy balanced")
// CHECK-NEXT: optional copy balanced
