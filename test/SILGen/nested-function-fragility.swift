// RUN: %target-swift-frontend -emit-silgen -module-name main %s | %FileCheck %s
internal func internalFunc() {}

// CHECK-LABEL: sil @_TF4main3foo
public func foo() {
  // CHECK-LABEL: sil shared [always_inline] @_TFF4main3foo{{.*}}zim
  @inline(__always)
  func zim() {
    // CHECK-LABEL: sil shared @_TFFF4main3foo{{.*}}zim{{.*}}zang
    func zang() { internalFunc() }
    internalFunc()
  }

  // CHECK-LABEL: sil shared @_TFF4main3foo{{.*}}U_
  let zung = {
    // CHECK-LABEL: sil shared [always_inline] @_TFFF4main3foo{{.*}}U_{{.*}}zippity
    @inline(__always)
    func zippity() { internalFunc() }
    internalFunc()
  }
}

// CHECK-LABEL: sil hidden [always_inline] @_TF4main3bar
@inline(__always)
internal func bar() {
  // CHECK-LABEL: sil shared [always_inline] @_TFF4main3bar{{.*}}zim
  @inline(__always)
  func zim() {
    // CHECK-LABEL: sil shared @_TFFF4main3bar{{.*}}zim{{.*}}zang
    func zang() { internalFunc() }
    internalFunc()
  }

  // CHECK-LABEL: sil shared @_TFF4main3bar{{.*}}U_
  let zung = {
    // CHECK-LABEL: sil shared [always_inline] @_TFFF4main3bar{{.*}}U_{{.*}}zippity
    @inline(__always)
    func zippity() { internalFunc() }
    internalFunc()
  }
}

public func publicFunc() {}

// CHECK-LABEL: sil [fragile] [always_inline] @_TF4main3bas
@inline(__always)
public func bas() {
  // CHECK-LABEL: sil shared [fragile] [always_inline] @_TFF4main3bas{{.*}}zim
  @inline(__always)
  func zim() {
    // CHECK-LABEL: sil shared [fragile] @_TFFF4main3bas{{.*}}zim{{.*}}zang
    func zang() { publicFunc() }
    publicFunc()
  }

  // CHECK-LABEL: sil shared [fragile] @_TFF4main3bas{{.*}}U_
  let zung = {
    // CHECK-LABEL: sil shared [fragile] [always_inline] @_TFFF4main3bas{{.*}}U_{{.*}}zippity
    @inline(__always)
    func zippity() { publicFunc() }
    publicFunc()
  }
}
