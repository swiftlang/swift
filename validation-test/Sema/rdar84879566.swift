// RUN: %target-swift-frontend -typecheck -target %target-cpu-apple-macosx10.15 -swift-version 5 %S/Inputs/rdar84879566_invalid_decls.swift -primary-file %s -verify
// REQUIRES: OS=macosx

protocol Tupled {
  associatedtype TupleType

  @TupleBuilder var tuple: TupleType { get }
}

@resultBuilder
struct TupleBuilder {
  static func buildBlock() -> () {
    return ()
  }

  static func buildBlock<T1>(_ t1: T1) -> (T1) {
    return (t1)
  }
}

struct MyApp: Tupled {
  var tuple: some Any {
    MyView() // expected-error {{ambiguous use of 'init()'}}
  }
}
