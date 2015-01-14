// RUN: %swift -emit-silgen -enable-guaranteed-self %s | FileCheck %s

protocol Fooable {
  init()
  func foo(x: Int)
  mutating func bar()
  mutating func bas()

  prefix func +(x: Self)

  var prop1: Int { get set }
  var prop2: Int { get set }
  var prop3: Int { get nonmutating set }
}

protocol Barrable: class {
  init()
  func foo(x: Int)
  func bar()
  func bas()

  prefix func +(x: Self)

  var prop1: Int { get set }
  var prop2: Int { get set }
  var prop3: Int { get set }
}

struct S: Fooable {
  var x: C? // Make the type nontrivial, so +0/+1 is observable.

  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1SCfMS0_FT_S0_ : $@thin (@thin S.Type) -> @owned S
  init() {}
  // TODO: Way too many redundant r/r pairs here. Should use +0 rvalues.
  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1S3foofS0_FSiT_ : $@cc(method) @thin (Int, @guaranteed S) -> () {
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : $S):
  // CHECK:         retain_value [[SELF]]
  // CHECK:         release_value [[SELF]]
  // CHECK-NOT:     release_value [[SELF]]
  func foo(x: Int) {
    self.foo(x)
  }

  func foooo(x: (Int, Bool)) {
    self.foooo(x)
  }

  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1S3barfRS0_FT_T_ : $@cc(method) @thin (@inout S) -> ()
  // CHECK:       bb0([[SELF:%.*]] : $*S):
  // CHECK-NOT:     destroy_addr [[SELF]]
  mutating func bar() {
    self.bar()
  }
  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1S3basfS0_FT_T_ : $@cc(method) @thin (@guaranteed S) -> ()
  // CHECK:       bb0([[SELF:%.*]] : $S):
  // CHECK:         retain_value [[SELF]]
  // CHECK:         release_value [[SELF]]
  // CHECK-NOT:     release_value [[SELF]]
  func bas() {
    self.bas()
  }

  var prop1: Int = 0

  var prop2: Int {
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1Sg5prop2Si : $@cc(method) @thin (@guaranteed S) -> Int
    // CHECK:       bb0([[SELF:%.*]] : $S):
    // CHECK-NOT:     release_value [[SELF]]
    get { return 0 }
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1Ss5prop2Si : $@cc(method) @thin (Int, @inout S) -> ()
    // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Sm5prop2Si : $@cc(method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>)
    set { }
  }

  var prop3: Int {
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1Sg5prop3Si : $@cc(method) @thin (@guaranteed S) -> Int
    // CHECK:       bb0([[SELF:%.*]] : $S):
    // CHECK-NOT:     release_value [[SELF]]
    get { return 0 }
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1Ss5prop3Si : $@cc(method) @thin (Int, @guaranteed S) -> ()
    // CHECK:       bb0({{.*}} [[SELF:%.*]] : $S):
    // CHECK-NOT:     release_value [[SELF]]
    // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Sm5prop3Si : $@cc(method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed S) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>)
    // CHECK:       bb0({{.*}} [[SELF:%.*]] : $S):
    // CHECK-NOT:     release_value [[SELF]]
    nonmutating set { }
  }

  // Getter for prop1
  // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Sg5prop1Si : $@cc(method) @thin (@guaranteed S) -> Int
  // CHECK:       bb0([[SELF:%.*]] : $S):
  // CHECK-NOT:     release_value [[SELF]]

  // Setter for prop1
  // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Ss5prop1Si : $@cc(method) @thin (Int, @inout S) -> ()
  // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
  // CHECK-NOT:     load [[SELF_ADDR]]
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]

  // materializeForSet for prop1
  // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Sm5prop1Si : $@cc(method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>)
  // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
  // CHECK-NOT:     load [[SELF_ADDR]]
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
}

prefix func +(x: S) {}

// Witness thunk for nonmutating 'foo'
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_3fooUS1___fQPS1_FSiT_ : $@cc(witness_method) @thin (Int, @in_guaranteed S) -> () {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for mutating 'bar'
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_3barUS1___fRQPS1_FT_T_ : $@cc(witness_method) @thin (@inout S) -> () {
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     load [[SELF_ADDR]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for 'bas', which is mutating in the protocol, but nonmutating
// in the implementation
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_3basUS1___fRQPS1_FT_T_ : $@cc(witness_method) @thin (@inout S) -> ()
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF:%.*]] = load [[SELF_ADDR]]
// CHECK:         retain_value [[SELF]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]

// Witness thunk for prop1 getter
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_g5prop1Si : $@cc(witness_method) @thin (@in_guaranteed S) -> Int
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop1 setter
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_s5prop1Si : $@cc(witness_method) @thin (Int, @inout S) -> () {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop1 materializeForSet
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_m5prop1Si : $@cc(witness_method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>) {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop2 getter
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_g5prop2Si : $@cc(witness_method) @thin (@in_guaranteed S) -> Int
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop2 setter
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_s5prop2Si : $@cc(witness_method) @thin (Int, @inout S) -> () {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop2 materializeForSet
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_m5prop2Si : $@cc(witness_method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>) {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop3 getter
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_g5prop3Si : $@cc(witness_method) @thin (@in_guaranteed S) -> Int
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop3 nonmutating setter
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_s5prop3Si : $@cc(witness_method) @thin (Int, @in_guaranteed S) -> ()
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop3 nonmutating materializeForSet
// CHECK-LABEL: sil hidden @_TTWV15guaranteed_self1SS_7FooableS_FS1_m5prop3Si : $@cc(witness_method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>)
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF:%.*]] = load [[SELF_ADDR]]
// CHECK:         retain_value [[SELF]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK:       }

//
// TODO: Expected output for the other cases
//

struct AO<T>: Fooable {
  var x: T?

  init() {}
  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AO3fooU__fGS0_Q__FSiT_ : $@cc(method) @thin <T> (Int, @in_guaranteed AO<T>) -> ()
  // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*AO<T>):
  // TODO: We could avoid the copy/destroy here.
  // CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY:%.*]]#1
  // CHECK:         apply {{.*}} [[SELF_COPY]]
  // CHECK:         destroy_addr [[SELF_COPY]]
  // CHECK:         dealloc_stack [[SELF_COPY]]
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
  // CHECK:       }
  func foo(x: Int) {
    self.foo(x)
  }
  mutating func bar() {
    self.bar()
  }
  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AO3basU__fGS0_Q__FT_T_ : $@cc(method) @thin <T> (@in_guaranteed AO<T>) -> () 
  // CHECK:       bb0([[SELF_ADDR:%.*]] : $*AO<T>):
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
  func bas() {
    self.bas()
  }

  
  var prop1: Int = 0
  var prop2: Int {
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AOg5prop2Si : $@cc(method) @thin <T> (@in_guaranteed AO<T>) -> Int {
    // CHECK:       bb0([[SELF_ADDR:%.*]] : $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    get { return 0 }
    set { }
  }
  var prop3: Int {
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AOg5prop3Si : $@cc(method) @thin <T> (@in_guaranteed AO<T>) -> Int
    // CHECK:       bb0([[SELF_ADDR:%.*]] : $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    get { return 0 }
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AOs5prop3Si : $@cc(method) @thin <T> (Int, @in_guaranteed AO<T>) -> ()
    // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self2AOm5prop3Si : $@cc(method) @thin <T> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @in_guaranteed AO<T>) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout AO<T>, @thick AO<T>.Type) -> ()>)
    // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    // CHECK:       }
    nonmutating set { }
  }
}

prefix func +<T>(x: AO<T>) {}

enum E: Fooable {
  case A, B(C)
  init() { self = .A }
  init(c: C) { self = .B(c) }

  func foo(x: Int) {
    self.foo(x)
  }
  mutating func bar() {
    self.bar()
  }
  func bas() {
    self.bas()
  }

  var prop1: Int {
    get { return 0 }
    set { }
  }
  var prop2: Int {
    get { return 0 }
    set { }
  }
  var prop3: Int {
    get { return 0 }
    nonmutating set { }
  }
}

prefix func +(x: E) {}

class C: Fooable, Barrable {
  required init() {}
  @objc func foo(x: Int) {
    self.foo(x)
  }
  @objc func bar() {
    self.bar()
  }
  @objc func bas() {
    self.bas()
  }

  @objc var prop1: Int = 0
  @objc var prop2: Int {
    get { return 0 }
    set {}
  }
  @objc var prop3: Int {
    get { return 0 }
    set {}
  }
}

prefix func +(x: C) {}
