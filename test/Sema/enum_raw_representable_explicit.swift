// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros

// REQUIRES: swift_feature_DeriveConformancesViaMacros

enum Foo: Int, RawRepresentable { case A }

enum Bar: Int { case A }

extension Bar: RawRepresentable {}

enum Bas: Int { case A }

// expected-note@+1{{'Bas' declares conformance to protocol 'RawRepresentable' here}}
extension Bas: RawRepresentable {}

// expected-error@+1{{redundant conformance of 'Bas' to protocol 'RawRepresentable'}}
extension Bas: RawRepresentable {}
