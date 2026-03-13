// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -module-name a -parse-as-library -emit-ir -g %t/Seq.swift %t/A.swift | %FileCheck %s

// This code used to trigger the verifier.
// https://github.com/apple/swift/issues/73338

// BEGIN Seq.swift
struct A<Element: Equatable>: ExpressibleByArrayLiteral, Equatable {
  var base: Element?
  init(arrayLiteral elements: Element...) {}
}
struct B<Element: Equatable>: ExpressibleByArrayLiteral, Equatable {
  var first: Element?
  init(arrayLiteral elements: Element...) {}
}

// BEGIN A.swift
enum E<T: P> {
  case a(A<T.ID>)
  case b(B<T.ID>)
  case c

  static func ==(lhs: Self, rhs: Self) -> Bool {
    switch (lhs, rhs) {
    case (.a([]), .c), (.c, .a([])),
         (.b([]), .c), (.c, .b([])):
      return true
    default:
      return false
    }
  }
}
public protocol P {
  associatedtype ID: Equatable
}

// The [] expressions should be available in the debugger

// CHECK: !DILocalVariable(name: "$_0", {{.+}} line: 9
// CHECK: !DILocalVariable(name: "$_1", {{.+}} line: 9
// CHECK: !DILocalVariable(name: "$_2", {{.+}} line: 8
// CHECK: !DILocalVariable(name: "$_3", {{.+}} line: 8

