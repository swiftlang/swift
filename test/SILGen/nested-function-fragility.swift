// RUN: %target-swift-emit-silgen -enable-sil-ownership -module-name main %s | %FileCheck %s
internal func internalFunc() {}

// CHECK-LABEL: sil @$s4main3fooyyF
public func foo() {
  // CHECK-LABEL: sil private [always_inline] @$s4main3foo{{[_0-9a-zA-Z]*}}zim
  @inline(__always)
  func zim() {
    // CHECK-LABEL: sil private @$s4main3fooyyF3zimL_yyF4zangL_yyF
    func zang() { internalFunc() }
    internalFunc()
  }

  // CHECK-LABEL: sil private @$s4main3foo{{[_0-9a-zA-Z]*}}U_
  let zung = {
    // CHECK-LABEL: sil private [always_inline] @$s4main3fooyyFyycfU_7zippityL_yyF
    @inline(__always)
    func zippity() { internalFunc() }
    internalFunc()
  }
}

// CHECK-LABEL: sil hidden [always_inline] @$s4main3baryyF
@inline(__always)
internal func bar() {
  // CHECK-LABEL: sil private [always_inline] @$s4main3baryyF3zimL_yyF
  @inline(__always)
  func zim() {
    // CHECK-LABEL: sil private @$s4main3baryyF3zimL_yyF4zangL_yyF
    func zang() { internalFunc() }
    internalFunc()
  }

  // CHECK-LABEL: sil private @$s4main3bar{{[_0-9a-zA-Z]*}}U_
  let zung = {
    // CHECK-LABEL: sil private [always_inline] @$s4main3baryyFyycfU_7zippityL_yyF
    @inline(__always)
    func zippity() { internalFunc() }
    internalFunc()
  }
}

public func publicFunc() {}

// CHECK-LABEL: sil [serialized] [always_inline] @$s4main3basyyF
@inlinable @inline(__always)
public func bas() {
  // CHECK-LABEL: sil shared [serialized] [always_inline] @$s4main3basyyF3zimL_yyF
  @inline(__always)
  func zim() {
    // CHECK-LABEL: sil shared [serialized] @$s4main3basyyF3zimL_yyF4zangL_yyF
    func zang() { publicFunc() }
    publicFunc()
  }

  // CHECK-LABEL: sil shared [serialized] @$s4main3bas{{[_0-9a-zA-Z]*}}U_
  let zung = {
    // CHECK-LABEL: sil shared [serialized] [always_inline] @$s4main3basyyFyycfU_7zippityL_yyF
    @inline(__always)
    func zippity() { publicFunc() }
    publicFunc()
  }
}
