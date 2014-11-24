// RUN: %swift -emit-ir -verify %s
// XFAIL: linux

// compiler_crashers/029-class-with-anyobject-type-constraint.swift
// Test case submitted to project by https://github.com/jansabbe (Jan Sabbe)

protocol Fooable { func foo() }

class A<B: Fooable where B: AnyObject> {
}
