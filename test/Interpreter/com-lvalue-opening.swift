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
func exchange<T: IValue>(_ value: inout T) {
  var copy = value
  swap(&value, &copy)
  precondition(MemoryLayout<T>.size == MemoryLayout<UnsafeRawPointer>.size)
}

struct Storage {
  var value: any IValue
  var writes = 0
  var computed: any IValue {
    get { value }
    set {
      value = newValue
      writes += 1
    }
  }
}

@inline(never)
func run() {
  var storage = Storage(value: makeValue(42))
  exchange(&storage.value)
  precondition(storage.value.value(0) == 42)
  exchange(&storage.computed)
  precondition(storage.writes == 1)
  precondition(storage.value.value(0) == 42)
  var optional: (any IValue)? = storage.value
  exchange(&optional!)
  precondition(optional!.value(0) == 42)
  var array = [storage.value]
  exchange(&array[0])
  precondition(array[0].value(0) == 42)
  withExtendedLifetime((storage, optional, array)) {
    precondition(GetForeignCOMDestructionCount() == 0)
  }
}
run()
checkDestruction()
print("existential lvalues balanced")
// CHECK: existential lvalues balanced

@inline(never)
func exchangeClass<T: IProperty>(_ value: inout T) {
  var copy = value
  swap(&value, &copy)
}

@inline(never)
func runClass() {
  var value = makeProperty(9)
  exchangeClass(&value)
  precondition(value.value == 9)
}
runClass()
checkDestruction()
print("class-bound lvalues balanced")
// CHECK-NEXT: class-bound lvalues balanced
