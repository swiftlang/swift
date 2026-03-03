// RUN: %target-swift-frontend -emit-ir -target %target-swift-5.1-abi-triple -primary-file %s %S/Inputs/implicit_some_b.swift -enable-experimental-feature ImplicitSome

// REQUIRES: swift_feature_ImplicitSome

protocol P {}
struct S: P {}

func foo() -> P { return S() }
