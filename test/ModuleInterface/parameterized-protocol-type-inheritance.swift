// RUN: %target-swift-emit-module-interface(%t/Fancy.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Fancy.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Fancy.swiftinterface

public protocol Fancy<Stuff> {
    associatedtype Stuff
}

// CHECK: public struct T : Library::Fancy {
// CHECK: public typealias Stuff = Swift::Float64
public struct T: Fancy<Float64> {
}

public protocol Q {
}

// CHECK: public struct S : Library::Fancy & Library::Q {
// CHECK: public typealias Stuff = Swift::Int
public struct S: Fancy<Int> & Q {
}

public protocol P {
}

// CHECK: public struct V : Library::Fancy & Library::P & Library::Q {
// CHECK: public typealias Stuff = Swift::CChar32
public struct V: ((Fancy<CChar32> & P) & Q) {
}

public protocol Bar<T> {
  associatedtype T
}

// CHECK: public struct X : Library::Bar & Library::Fancy
// CHECK: public typealias Stuff = Swift::CChar32
// CHECK: public typealias T = Swift::Int
public struct X: Fancy<CChar32> & Bar<Int> {
}

public class Base {
}

public protocol B<A>: ~Copyable {
  associatedtype A
}

// CHECK: public class Derived : Library::Base & Library::B {
// CHECK: public typealias A = Swift::Int
public class Derived: Base & B<Int> {
}

public protocol R<E>: ~Copyable & ~Escapable {
  associatedtype E
}

// CHECK: public struct N : Library::B & Library::R & ~Copyable {
// CHECK: public typealias A = Swift::Float64
// CHECK: public typealias E = Swift::Int
public struct N: R<Int> & B<Float64> & ~Copyable  {
}

public protocol P1<AT> {
  associatedtype AT
}

public protocol P2<AT> {
   associatedtype AT
}

// CHECK-COUNT-1: public typealias AT = Swift::Int
public struct S1: P1<Int>, P2<Int> {}
