// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file.swiftmodule -primary-file %s %S/Inputs/multi-file-2.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-2.swiftmodule %s -primary-file %S/Inputs/multi-file-2.swift

// RUN: llvm-bcanalyzer %t/multi-file.swiftmodule | %FileCheck %s -check-prefix=THIS-FILE
// RUN: llvm-bcanalyzer %t/multi-file.swiftmodule | %FileCheck %s -check-prefix=THIS-FILE-NEG
// RUN: llvm-bcanalyzer %t/multi-file-2.swiftmodule | %FileCheck %s -check-prefix=OTHER-FILE
// RUN: llvm-bcanalyzer %t/multi-file-2.swiftmodule | %FileCheck %s -check-prefix=OTHER-FILE-NEG

// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file.swiftmodule %t/multi-file-2.swiftmodule -o %t
// RUN: llvm-bcanalyzer %t/Multi.swiftmodule | %FileCheck %s -check-prefix=THIS-FILE
// RUN: llvm-bcanalyzer %t/Multi.swiftmodule | %FileCheck %s -check-prefix=OTHER-FILE

// Do not put any enums in this file. It's part of the test that no enums
// get serialized here.

class MyClass {
  var value: TheEnum = .A
}

func foo<T: Equatable>(_ x: T) {}
func bar() {
  foo(EquatableEnum.A)
}

// THIS-FILE-DAG: PROTOCOL_DECL
// OTHER-FILE-NEG-NOT: PROTOCOL_DECL
// OTHER-FILE-DAG: ENUM_DECL
// THIS-FILE-NEG-NOT: ENUM_DECL


// <rdar://problem/17251682>
struct StructWithInheritedConformances: Sequence {
  struct EmptyIterator : IteratorProtocol {
    mutating func next() -> Int? {
      return nil
    }
  }

  func makeIterator() -> EmptyIterator {
    return EmptyIterator()
  }
}

// https://bugs.swift.org/browse/SR-2576
// An associated type inside a private protocol would cause crashes during
// module merging.
private protocol SomeProto {
  // THIS-FILE-DAG: ASSOCIATED_TYPE_DECL
  associatedtype Item
}

private struct Generic<T> {
  // THIS-FILE-DAG: GENERIC_TYPE_PARAM_DECL
}

class Sub: Base {
  override class var conflict: Int { return 100 }
  override var conflict: Int { return 200 }
}
