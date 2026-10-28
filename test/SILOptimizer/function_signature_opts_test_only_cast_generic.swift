// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -module-name Lib -target %target-future-triple -enable-experimental-feature NoncopyableCasting %s | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting

// Regression test: Ensure that we notice the generic type on the
// target type of an `is`.
// Without this, generic optimization could drop the generic
// signature when the only generic type reference was in
// the target type of `checked_cast_addr_br test_only`.

public protocol P: ~Copyable {}
public struct Box<T>: P, ~Copyable { public var t: T }

public func run(_ x: borrowing any P & ~Copyable,
                _ matches: (borrowing any P & ~Copyable) -> Bool) -> Bool {
  matches(x)
}

// CHECK-LABEL: sil private @{{.*}}check{{.*}}
// CHECK-NOT:     @error_type
// CHECK:         checked_cast_addr_br test_only any P & ~Copyable in {{%.*}} to Box<E>,
// CHECK-NOT:     @error_type
public func check<E>(_ x: borrowing any P & ~Copyable, _ t: E.Type) -> Bool {
  run(x, { $0 is Box<E> })
}

// CHECK-LABEL: sil private @{{.*}}checkArray{{.*}}
// CHECK-NOT:     @error_type
// CHECK:         checked_cast_addr_br test_only any P & ~Copyable in {{%.*}} to Array<E>,
// CHECK-NOT:     @error_type
public func checkArray<E>(_ x: borrowing any P & ~Copyable, _ t: E.Type) -> Bool {
  run(x, { $0 is [E] })
}
