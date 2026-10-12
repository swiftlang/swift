// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/ForeignCOM/ForeignCOM.c -c -o %t/ForeignCOM.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) -Xfrontend -enable-experimental-com-interop -enable-library-evolution -I %t -module-name Library -emit-module-path %t/Library.swiftmodule %S/../Inputs/COMGenericArguments.swift %S/../Inputs/COMGenericDispatch.swift -L %t -lCOM %target-rpath(%t)
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
func packed<T: IItem>(_ value: T) -> Int32 {
  dispatchPack(value, value)
}

enum Stop: Error { case stop }

@inline(never)
func run() {
  let object = ForeignCOMObject_Create(7)!
  let value = ForeignCOMObject_GetValueStorage(object)!.load(as: (any IExtended).self)
  let other = ForeignCOMObject_GetPropertyStorage(object)!.load(as: (any IClassItem).self)
  ForeignCOMObject_Release(object)
  precondition(dispatch(value) == 22)
  precondition(value.adjusted(5) == 12)
  precondition(packed(value) == 14)
  precondition(property(other) == 8)
  precondition(value.adjusted(0) == 8)
  do {
    try edit(other) { value in
      value = 42
      throw Stop.stop
    }
    preconditionFailure("expected an error")
  } catch Stop.stop {
    precondition(value.adjusted(0) == 42)
  } catch {
    preconditionFailure("unexpected error")
  }
  precondition(try! value.nonnegative == 42)
  precondition(try! readNonnegative(value) == 42)
  other.value = -1
  do {
    _ = try value.nonnegative
    preconditionFailure("expected the extension getter to throw")
  } catch ValueError.negative {
    precondition(value.value(1) == 0)
  } catch {
    preconditionFailure("unexpected error")
  }
  do {
    _ = try readNonnegative(value)
    preconditionFailure("expected the generic call to throw")
  } catch ValueError.negative {
    precondition(value.value(2) == 1)
  } catch {
    preconditionFailure("unexpected error")
  }
  precondition(GetForeignCOMQueryInterfaceCalls() == 0)
  withExtendedLifetime((value, other)) {}
}
run()
precondition(GetForeignCOMReferenceCount() == 0)
precondition(GetForeignCOMDestructionCount() == 1)
precondition(GetForeignCOMReleaseCalls() == GetForeignCOMAddRefCalls() + 1)
print("generic dispatch balanced")
// CHECK: generic dispatch balanced
