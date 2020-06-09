// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: not %target-swift-frontend -typecheck -primary-file %t/main.swift -emit-reference-dependencies-path - -experimental-private-intransitive-dependencies > %t.swiftdeps

associatedtype Baz
case bar
deinit {}
extension Foo {}
init() {}
