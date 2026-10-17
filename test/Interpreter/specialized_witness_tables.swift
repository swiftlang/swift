// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -O -wmo -parse-as-library -module-name Lib -emit-module-path %t/Lib.swiftmodule %S/Inputs/specialized_witness_tables_lib.swift -o %t/Lib.o
// RUN: %target-build-swift -O -wmo -module-name main -I %t %s %t/Lib.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// Calls through existentials of specialized generic types, which use statically
// emitted specialized witness tables. Also checks that such tables are interchangeable
// with the tables the runtime instantiates, e.g. for dynamic casts.

import Lib

protocol P {
  func foo() -> String
}

protocol Q: P {
  associatedtype A: P
  func bar() -> A
}

struct Box<T>: P {
  var t: T
  @inline(never)
  func foo() -> String { "Box<\(T.self)>(\(t))" }
}

struct Wrapper<T>: Q {
  var t: T
  func foo() -> String { "Wrapper<\(T.self)>" }
  func bar() -> Box<T> { Box(t: t) }
}

struct Cond<T> {
  var t: T
}

extension Cond: P where T: P {
  func foo() -> String { "Cond(\(t.foo()))" }
}

@inline(never)
func makeBox() -> any P { Box(t: 27) }

@inline(never)
func makeWrapper() -> any Q { Wrapper(t: true) }

@inline(never)
func makeCond() -> any P { Cond(t: Box(t: 1.5)) }

@inline(never)
func makeAny() -> Any { Wrapper(t: 3) }

@inline(never)
func makeBoxAsAny() -> Any { Box(t: 5) }

@inline(never)
func callFoo(_ p: any P) -> String { p.foo() }

@inline(never)
func callBar(_ q: any Q) -> String { q.bar().foo() }

@inline(never)
func callGeneric<T: P>(_ p: T) -> String { "\(type(of: p)): \(p.foo())" }

struct Holder<T: P> {
  var t: T
}

@inline(never)
func makeHolder(_ p: any P) -> Any {
  func open<T: P>(_ t: T) -> Any { Holder(t: t) }
  return open(p)
}

protocol Copies {
  func copy() -> Self
}

protocol Named {
  func name() -> String
}

extension Named {
  func name() -> String { "\(type(of: self))" }
}

class Base<T>: Copies, Named {
  required init() {}
  func copy() -> Self { Self() }
}

final class Sub: Base<Int> {}

// The static type is Base<Int>, but the dynamic Self is Sub.
@inline(never)
func makeBaseCopies() -> any Copies { let b: Base<Int> = Sub(); return b }

@inline(never)
func makeBaseNamed() -> any Named { let b: Base<Int> = Sub(); return b }

// The getter returns an associated type indirectly. A direct call of the witness
// thunk is specialized with a direct result, which must not be confused with the
// specialized witness in the witness table.
protocol ValueHolder<T> {
  associatedtype T
  var value: T { get }
}

struct GenericValueHolder<T>: ValueHolder {
  var value: T
}

@inline(never)
func makeValueHolder() -> any ValueHolder<Int> { GenericValueHolder(value: 5) }

// CHECK: 5 5
print(GenericValueHolder(value: 5).value, makeValueHolder().value)

// CHECK: Box<Int>(27)
print(callFoo(makeBox()))
// CHECK: Sub
print(type(of: makeBaseCopies().copy()))
// CHECK: Sub
print(makeBaseNamed().name())
// CHECK: Wrapper<Bool>
print(callFoo(makeWrapper()))
// CHECK: Box<Bool>(true)
print(callBar(makeWrapper()))
// CHECK: Cond(Box<Double>(1.5))
print(callFoo(makeCond()))
// CHECK: Box<Int>: Box<Int>(27)
print(callGeneric(makeBox()))
// CHECK: Box<Int>
print(type(of: makeBox()))

// Casts get the witness table from the runtime.
// CHECK: Wrapper<Int>
print((makeAny() as! any P).foo())
// CHECK: Box<Int>(3)
print((makeAny() as! any Q).bar().foo())
// CHECK: true
print((makeBox() as Any) is any P)
// CHECK: false
print((makeBox() as Any) is any Q)

// Generic metadata keyed by a statically emitted and a runtime-instantiated table.
// CHECK: true
print(type(of: makeHolder(makeBox())) == type(of: makeHolder(makeBoxAsAny() as! any P)))
// CHECK: true
print(type(of: makeHolder(makeBoxAsAny() as! any P)) == Holder<Box<Int>>.self)
// CHECK: Holder<Box<Int>>
print(type(of: makeHolder(makeBox())))

// Both modules may emit the same specialized table.
// CHECK: LibBox<Int>(42)
print(libMakeBox().describe())
// CHECK: LibBox<Int>(7)
print((LibBox(t: 7) as any LibP).describe())
