// RUN: %target-swift-frontend -primary-file %s %S/Inputs/implicit_some_b.swift -enable-experimental-feature ImplicitSome -emit-ir

protocol P {}
struct S: P {}

func foo() -> P { return S() }
