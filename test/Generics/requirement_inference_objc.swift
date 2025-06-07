// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

// rdar://problem/30610428
@objc protocol P14 { }

class X12<S: AnyObject> {
  // CHECK-LABEL: .X12.bar(v:)@
  // CHECK-NEXT: Generic signature: <S, V where S == any P14>
  func bar<V>(v: V) where S == any P14 {
  }
}

@objc protocol P15: P14 { }

class X13<S: P14> {
  // CHECK-LABEL: .X13.bar(v:)@
  // CHECK-NEXT: Generic signature: <S, V where S == any P15>
  func bar<V>(v: V) where S == any P15 {
  }
}

