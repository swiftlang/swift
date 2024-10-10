// RUN: %target-swift-frontend -emit-ir -disable-availability-checking -primary-file %s %S/Inputs/implicit_some_b.swift -enable-experimental-feature ImplicitSome

// REQUIRES: swift_feature_ImplicitSome

protocol P {}
struct S: P {}

func foo() -> P { return S() }
