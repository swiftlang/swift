// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -verify -emit-ir %s

public protocol P {
    associatedtype Element
}

public class C<O: P>: P {
    public typealias Element = O.Element
}

// CHECK: Generic signature: <T, O, E where T : C<E>, O : P, E : P, O.Element == E.Element>
public func toe1<T, O, E>(_: T, _: O, _: E, _: T.Element)
    where T : P, // expected-warning {{redundant conformance constraint 'T' : 'P'}}
          O : P,
          O.Element == T.Element,
          T : C<E> {} // expected-note {{conformance constraint 'T' : 'P' implied here}}

// CHECK: Generic signature: <T, O, E where T : C<E>, O : P, E : P, O.Element == E.Element>
public func toe2<T, O, E>(_: T, _: O, _: E, _: T.Element)
    where O : P,
          O.Element == T.Element,
          T : C<E> {}

