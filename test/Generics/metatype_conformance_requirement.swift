// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P { }
struct S<T> { }

// CHECK-LABEL: .f@
// CHECK-NEXT:  Generic signature: <T where T.Type : P>
func f<T>(_: T.Type) where T.Type: P { }

// CHECK-LABEL: .g@
// CHECK-NEXT:  Generic signature: <T where T.Type : P>
func g<T>(_: T) where T.Type: P { }

// CHECK-LABEL: .h@
// CHECK-NEXT:  Generic signature: <T>
func h<T>(_: S<T.Type>) { }
