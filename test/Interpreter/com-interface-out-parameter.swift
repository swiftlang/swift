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

@com(interface: "10000000-0000-0000-0000-000000000004")
protocol IProvider: IExtended {
  func GetValue(_ result: UnsafeMutablePointer<(any IValue)?>?) -> Int32
  func Matches(_ other: (any IValue)?) -> Int32
}

@inline(never)
func makeProvider(_ value: Int32) -> any IProvider {
  let object = ForeignCOMObject_Create(value)!
  let storage = ForeignCOMObject_GetValueStorage(object)!
  let interface = storage.load(as: (any IProvider).self)
  ForeignCOMObject_Release(object)
  return interface
}

@inline(never)
func getValue() -> any IValue {
  let provider = makeProvider(40)
  var value: (any IValue)? = nil
  precondition(provider.GetValue(&value) == 0)
  precondition(value!.value(2) == 42)
  precondition(provider.Matches(value) == 1)
  precondition(provider.Matches(nil) == 0)
  withExtendedLifetime(provider) {
    precondition(GetForeignCOMDestructionCount() == 0)
  }
  precondition(GetForeignCOMMethodCalls() == 4)
  // GetValue supplies a reference owned by the result. It must remain valid
  // after the provider has been released.
  return value!
}

func exerciseResult() {
  let value = getValue()
  precondition(value.value(3) == 43)
  withExtendedLifetime(value) {
    precondition(GetForeignCOMDestructionCount() == 0)
  }
}

exerciseResult()
checkDestruction()
print("out parameter balanced")
// CHECK: out parameter balanced

func exerciseNullResult() {
  let provider = makeProvider(10)
  withExtendedLifetime(provider) {
    let references = GetForeignCOMReferenceCount()
    precondition(provider.GetValue(nil) == Int32(bitPattern: 0x80004003))
    precondition(GetForeignCOMReferenceCount() == references)
    precondition(GetForeignCOMDestructionCount() == 0)
  }
  precondition(GetForeignCOMMethodCalls() == 1)
}

exerciseNullResult()
checkDestruction()
print("null out parameter balanced")
// CHECK-NEXT: null out parameter balanced
