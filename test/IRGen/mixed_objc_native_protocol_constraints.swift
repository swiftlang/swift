// RUN: %target-swift-frontend -emit-ir -verify %s

// compiler_crashers/029-class-with-anyobject-type-constraint.swift
// Test case submitted to project by https://github.com/jansabbe (Jan Sabbe)

protocol Fooable { func foo() }

class A<B: Fooable where B: AnyObject> {
}
