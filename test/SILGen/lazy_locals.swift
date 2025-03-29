// RUN: %target-swift-emit-silgen -primary-file %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -primary-file %s

// CHECK-LABEL: sil hidden [ossa] @$s11lazy_locals6simpleSiyF : $@convention(thin) () -> Int {
func simple() -> Int {
  lazy var x = 123
  return x
}

// CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11lazy_locals6simpleSiyF1xL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }) -> Int {

// CHECK-LABEL: sil hidden [ossa] @$s11lazy_locals8captures1xS2i_tF : $@convention(thin) (Int) -> Int {
func captures(x: Int) -> Int {
  let y = x * x
  lazy var z = x + y
  let fn = { z }
  return fn()
}

// CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11lazy_locals8captures1xS2i_tF1zL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }, Int, Int) -> Int {
// CHECK-LABEL: sil private [ossa] @$s11lazy_locals8captures1xS2i_tFSiycfU_ : $@convention(thin) (@guaranteed { var Optional<Int> }, Int, Int) -> Int {

// CHECK-LABEL: sil hidden [ossa] @$s11lazy_locals6assignSiyF : $@convention(thin) () -> Int {
func assign() -> Int {
  lazy var z = 123
  z = 321
  return z
}

// CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11lazy_locals6assignSiyF1zL_Sivg : $@convention(thin) (@guaranteed { var Optional<Int> }) -> Int {
// CHECK-LABEL: sil private [ossa] @$s11lazy_locals6assignSiyF1zL_Sivs : $@convention(thin) (Int, @guaranteed { var Optional<Int> }) -> () {

// CHECK-LABEL: sil hidden [ossa] @$s11lazy_locals7generic1xxx_tlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
func generic<T>(x: T) -> T {
  lazy var z = x
  return z
}

// CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s11lazy_locals7generic1xxx_tlF1zL_xvg : $@convention(thin) <T> (@guaranteed <τ_0_0> { var Optional<τ_0_0> } <T>, @in_guaranteed T) -> @out T {

func lazyLocalWithNestedClosure() {
  lazy var x = {
    return 3
  }()
}

func lazyLocalInClosure() {
  // Make sure we can type-check.
  _ = {
    lazy var x = 0
    return x
  }
}
