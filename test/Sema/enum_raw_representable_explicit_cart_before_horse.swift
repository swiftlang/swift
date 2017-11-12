// RUN: %target-swift-frontend -typecheck -verify %s

enum Foo: Int { case A }
extension Foo: RawRepresentable {}
