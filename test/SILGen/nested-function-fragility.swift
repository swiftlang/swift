// RUN: %target-swift-frontend -emit-silgen -module-name main %s | %FileCheck %s
internal func internalFunc() {}

// CHECK-LABEL: sil @_T04main3fooyyF
public func foo() {
  // CHECK-LABEL: sil private [always_inline] @_T04main3foo{{[_0-9a-zA-Z]*}}zim
  @inline(__always)
  func zim() {
    // CHECK-LABEL: sil private @_T04main3fooyyF3zimL_yyF4zangL_yyF
    func zang() { internalFunc() }
    internalFunc()
  }

  // CHECK-LABEL: sil private @_T04main3foo{{[_0-9a-zA-Z]*}}U_
  let zung = {
    // CHECK-LABEL: sil private [always_inline] @_T04main3fooyyFyycfU_7zippityL_yyF
    @inline(__always)
    func zippity() { internalFunc() }
    internalFunc()
  }
}

// CHECK-LABEL: sil hidden [always_inline] @_T04main3baryyF
@inline(__always)
internal func bar() {
  // CHECK-LABEL: sil private [always_inline] @_T04main3baryyF3zimL_yyF
  @inline(__always)
  func zim() {
    // CHECK-LABEL: sil private @_T04main3baryyF3zimL_yyF4zangL_yyF
    func zang() { internalFunc() }
    internalFunc()
  }

  // CHECK-LABEL: sil private @_T04main3bar{{[_0-9a-zA-Z]*}}U_
  let zung = {
    // CHECK-LABEL: sil private [always_inline] @_T04main3baryyFyycfU_7zippityL_yyF
    @inline(__always)
    func zippity() { internalFunc() }
    internalFunc()
  }
}

public func publicFunc() {}

// CHECK-LABEL: sil [serialized] [always_inline] @_T04main3basyyF
@inline(__always)
public func bas() {
  // CHECK-LABEL: sil shared [serialized] [always_inline] @_T04main3basyyF3zimL_yyF
  @inline(__always)
  func zim() {
    // CHECK-LABEL: sil shared [serialized] @_T04main3basyyF3zimL_yyF4zangL_yyF
    func zang() { publicFunc() }
    publicFunc()
  }

  // CHECK-LABEL: sil shared [serialized] @_T04main3bas{{[_0-9a-zA-Z]*}}U_
  let zung = {
    // CHECK-LABEL: sil shared [serialized] [always_inline] @_T04main3basyyFyycfU_7zippityL_yyF
    @inline(__always)
    func zippity() { publicFunc() }
    publicFunc()
  }
}
