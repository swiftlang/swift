// RUN: %empty-directory(%t)

// %t.input: "A ---> B" ==> "A"
// RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/decl-reconstr-names.txt > %t.input

// %t.check: "A ---> B" ==> "B"
// RUN: sed -ne '/--->/s/^.*---> *//p' < %S/Inputs/decl-reconstr-names.txt > %t.check

// RUN: %target-build-swift -emit-executable %s -g -o %t/DeclReconstr -emit-module

// Input validation tests.
// RUN: not %lldb-moduleimport-test patatino 2>&1 | %FileCheck %s \
// RUN:   --check-prefix=INVALID-INPUT
// INVALID-INPUT: patatino No such file or directory

// RUN: not %lldb-moduleimport-test %t/DeclReconstr \
// RUN:   --decl-from-mangled=patatino 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=INVALID-DECL
// INVALID-DECL: patatino does not exists, exiting.

// RUN: not %lldb-moduleimport-test %t/DeclReconstr \
// RUN:   --type-from-mangled=patatino 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=INVALID-TYPE
// INVALID-TYPE: patatino does not exists, exiting.

// RUN: %lldb-moduleimport-test %t/DeclReconstr \
// RUN:   -decl-from-mangled=%t.input > %t.output 2>&1
// RUN: diff %t.check %t.output

// REQUIRES: executable_test
struct S {
  init() {
  }
}

func patatino() -> Int {
  let s = S()
  return 0
}

patatino()

class Foo<T> {
  var x : T
  init(_ x : T) {
    self.x = x
  }
}

typealias Patatino<T> = Foo<T>

public struct Outer<T> {
  public struct Inner { }
  public struct GenericInner<U> { }

  public typealias Foo<U> = Outer<U>.Inner

  public func blah() {
    let foo: Foo<Int> = Outer<Int>.Inner()
  }
}

extension Outer.GenericInner {
  public typealias Bar = Int

  public func useBar() {
    let bar: Bar = 7
  }
}

// Mangling for generic typealiases.
protocol P {
  associatedtype A
}

protocol Q {
  associatedtype B: P
  typealias ProtocolTypeAliasThing = B.A
}

struct ConformsToP: P {
  typealias A = Int
}

struct ConformsToQ: Q {
  typealias B = ConformsToP
}

struct Blah {
  typealias SomeQ = ConformsToQ

  func foo() {
    let bar: SomeQ.ProtocolTypeAliasThing? = nil
  }
}

func main() -> Int {
  var p : Patatino<Int> = Patatino(23);
  return 0
}

let _ = main()
