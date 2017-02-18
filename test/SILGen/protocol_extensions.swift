// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -disable-objc-attr-requires-foundation-module -emit-silgen %s | %FileCheck %s

public protocol P1 {
  func reqP1a()
  subscript(i: Int) -> Int { get set }
}

struct Box {
  var number: Int
}

extension P1 {
  // CHECK-LABEL: sil hidden @_T019protocol_extensions2P1PAAE6extP1a{{[_0-9a-zA-Z]*}}F : $@convention(method) <Self where Self : P1> (@in_guaranteed Self) -> () {
  // CHECK: bb0([[SELF:%[0-9]+]] : $*Self):
  final func extP1a() {
    // CHECK: [[WITNESS:%[0-9]+]] = witness_method $Self, #P1.reqP1a!1 : {{.*}} : $@convention(witness_method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    // CHECK-NEXT: apply [[WITNESS]]<Self>([[SELF]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    reqP1a()
    // CHECK: return
  }

  // CHECK-LABEL: sil @_T019protocol_extensions2P1PAAE6extP1b{{[_0-9a-zA-Z]*}}F : $@convention(method) <Self where Self : P1> (@in_guaranteed Self) -> () {
  // CHECK: bb0([[SELF:%[0-9]+]] : $*Self):
  public final func extP1b() {
    // CHECK: [[FN:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE6extP1a{{[_0-9a-zA-Z]*}}F : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    // CHECK-NEXT: apply [[FN]]<Self>([[SELF]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    extP1a()
    // CHECK: return
  }

  subscript(i: Int) -> Int {
    // materializeForSet can do static dispatch to peer accessors (tested later, in the emission of the concrete conformance)
    get {
      return 0
    }
    set {}
  }

  final func callSubscript() -> Int {
    // But here we have to do a witness method call:

    // CHECK-LABEL: sil hidden @_T019protocol_extensions2P1PAAE13callSubscript{{[_0-9a-zA-Z]*}}F
    // CHECK: bb0(%0 : $*Self):
    // CHECK: witness_method $Self, #P1.subscript!getter.1
    // CHECK: return
    return self[0]
  }

  static var staticReadOnlyProperty: Int {
    return 0
  }

  static var staticReadWrite1: Int {
    get { return 0 }
    set { }
  }

  static var staticReadWrite2: Box {
    get { return Box(number: 0) }
    set { }
  }
}

// ----------------------------------------------------------------------------
// Using protocol extension members with concrete types
// ----------------------------------------------------------------------------
class C : P1 {
  func reqP1a() { }
}

//   (materializeForSet test from above)
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T019protocol_extensions1CCAA2P1AaaDP9subscriptSiSicfmTW
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $Int, %3 : $*C):
// CHECK: function_ref @_T019protocol_extensions2P1PAAE9subscriptSiSicfg
// CHECK: return

class D : C { }

struct S : P1 {
  func reqP1a() { }
}

struct G<T> : P1 {
  func reqP1a() { }
}

struct MetaHolder {
  var d: D.Type = D.self
  var s: S.Type = S.self
}

struct GenericMetaHolder<T> {
  var g: G<T>.Type = G<T>.self
}

func inout_func(_ n: inout Int) {}

// CHECK-LABEL: sil hidden @_T019protocol_extensions5testDyAA10MetaHolderV_AA1DCm2ddAF1dtF : $@convention(thin) (MetaHolder, @thick D.Type, @owned D) -> ()
// CHECK: bb0([[M:%[0-9]+]] : $MetaHolder, [[DD:%[0-9]+]] : $@thick D.Type, [[D:%[0-9]+]] : $D):
func testD(_ m: MetaHolder, dd: D.Type, d: D) {
  // CHECK: [[D2:%[0-9]+]] = alloc_box ${ var D }
  // CHECK: [[RESULT:%.*]] = project_box [[D2]]
  // CHECK: [[FN:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE11returnsSelf{{[_0-9a-zA-Z]*}}F
  // CHECK: [[BORROWED_D:%.*]] = begin_borrow [[D]]
  // CHECK: [[MATERIALIZED_BORROWED_D:%[0-9]+]] = alloc_stack $D
  // CHECK: store_borrow [[BORROWED_D]] to [[MATERIALIZED_BORROWED_D]]
  // CHECK: apply [[FN]]<D>([[RESULT]], [[MATERIALIZED_BORROWED_D]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK-NEXT: dealloc_stack [[MATERIALIZED_BORROWED_D]]
  // CHECK-NEXT: end_borrow [[BORROWED_D]] from [[D]]
  var d2: D = d.returnsSelf()

  // CHECK: metatype $@thick D.Type
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE22staticReadOnlyPropertySifgZ
  let _ = D.staticReadOnlyProperty

  // CHECK: metatype $@thick D.Type
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  D.staticReadWrite1 = 1

  // CHECK: metatype $@thick D.Type
  // CHECK: alloc_stack $Int
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifgZ
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: dealloc_stack
  D.staticReadWrite1 += 1

  // CHECK: metatype $@thick D.Type
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  D.staticReadWrite2 = Box(number: 2)

  // CHECK: metatype $@thick D.Type
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  D.staticReadWrite2.number += 5

  // CHECK: function_ref @_T019protocol_extensions10inout_funcySizF
  // CHECK: metatype $@thick D.Type
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  inout_func(&D.staticReadWrite2.number)

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE22staticReadOnlyPropertySifgZ
  let _ = dd.staticReadOnlyProperty

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  dd.staticReadWrite1 = 1

  // CHECK: alloc_stack $Int
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifgZ
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: dealloc_stack
  dd.staticReadWrite1 += 1

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  dd.staticReadWrite2 = Box(number: 2)

  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  dd.staticReadWrite2.number += 5

  // CHECK: function_ref @_T019protocol_extensions10inout_funcySizF
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  inout_func(&dd.staticReadWrite2.number)

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE22staticReadOnlyPropertySifgZ
  let _ = m.d.staticReadOnlyProperty

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  m.d.staticReadWrite1 = 1

  // CHECK: alloc_stack $Int
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifgZ
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: dealloc_stack
  m.d.staticReadWrite1 += 1

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  m.d.staticReadWrite2 = Box(number: 2)

  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  m.d.staticReadWrite2.number += 5

  // CHECK: function_ref @_T019protocol_extensions10inout_funcySizF
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  inout_func(&m.d.staticReadWrite2.number)

  // CHECK: return
}

// CHECK-LABEL: sil hidden @_T019protocol_extensions5testSyAA10MetaHolderV_AA1SVm2sstF
func testS(_ m: MetaHolder, ss: S.Type) {
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE22staticReadOnlyPropertySifgZ
  // CHECK: metatype $@thick S.Type
  let _ = S.staticReadOnlyProperty

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: metatype $@thick S.Type
  S.staticReadWrite1 = 1

  // CHECK: alloc_stack $Int
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifgZ
  // CHECK: metatype $@thick S.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: dealloc_stack
  S.staticReadWrite1 += 1

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: metatype $@thick S.Type
  S.staticReadWrite2 = Box(number: 2)

  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick S.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  S.staticReadWrite2.number += 5

  // CHECK: function_ref @_T019protocol_extensions10inout_funcySizF
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick S.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  inout_func(&S.staticReadWrite2.number)

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE22staticReadOnlyPropertySifgZ
  // CHECK: metatype $@thick S.Type
  let _ = ss.staticReadOnlyProperty

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: metatype $@thick S.Type
  ss.staticReadWrite1 = 1

  // CHECK: alloc_stack $Int
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifgZ
  // CHECK: metatype $@thick S.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: dealloc_stack
  ss.staticReadWrite1 += 1

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: metatype $@thick S.Type
  ss.staticReadWrite2 = Box(number: 2)

  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick S.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  ss.staticReadWrite2.number += 5

  // CHECK: function_ref @_T019protocol_extensions10inout_funcySizF
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick S.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  inout_func(&ss.staticReadWrite2.number)

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE22staticReadOnlyPropertySifgZ
  // CHECK: metatype $@thick S.Type
  let _ = m.s.staticReadOnlyProperty

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: metatype $@thick S.Type
  m.s.staticReadWrite1 = 1

  // CHECK: alloc_stack $Int
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifgZ
  // CHECK: metatype $@thick S.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: dealloc_stack
  m.s.staticReadWrite1 += 1

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: metatype $@thick S.Type
  m.s.staticReadWrite2 = Box(number: 2)

  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick S.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  m.s.staticReadWrite2.number += 5

  // CHECK: function_ref @_T019protocol_extensions10inout_funcySizF
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick S.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  inout_func(&m.s.staticReadWrite2.number)

  // CHECK: return
}

// CHECK-LABEL: sil hidden @_T019protocol_extensions5testG{{[_0-9a-zA-Z]*}}F
func testG<T>(_ m: GenericMetaHolder<T>, gg: G<T>.Type) {
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE22staticReadOnlyPropertySifgZ
  // CHECK: metatype $@thick G<T>.Type
  let _ = G<T>.staticReadOnlyProperty

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: metatype $@thick G<T>.Type
  G<T>.staticReadWrite1 = 1

  // CHECK: alloc_stack $Int
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifgZ
  // CHECK: metatype $@thick G<T>.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: dealloc_stack
  G<T>.staticReadWrite1 += 1

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: metatype $@thick G<T>.Type
  G<T>.staticReadWrite2 = Box(number: 2)

  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick G<T>.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  G<T>.staticReadWrite2.number += 5

  // CHECK: function_ref @_T019protocol_extensions10inout_funcySizF
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick G<T>.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  inout_func(&G<T>.staticReadWrite2.number)

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE22staticReadOnlyPropertySifgZ
  // CHECK: metatype $@thick G<T>.Type
  let _ = gg.staticReadOnlyProperty

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: metatype $@thick G<T>.Type
  gg.staticReadWrite1 = 1

  // CHECK: alloc_stack $Int
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifgZ
  // CHECK: metatype $@thick G<T>.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: dealloc_stack
  gg.staticReadWrite1 += 1

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: metatype $@thick G<T>.Type
  gg.staticReadWrite2 = Box(number: 2)

  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick G<T>.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  gg.staticReadWrite2.number += 5

  // CHECK: function_ref @_T019protocol_extensions10inout_funcySizF
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick G<T>.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  inout_func(&gg.staticReadWrite2.number)

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE22staticReadOnlyPropertySifgZ
  // CHECK: metatype $@thick G<T>.Type
  let _ = m.g.staticReadOnlyProperty

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: metatype $@thick G<T>.Type
  m.g.staticReadWrite1 = 1

  // CHECK: alloc_stack $Int
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifgZ
  // CHECK: metatype $@thick G<T>.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite1SifsZ
  // CHECK: dealloc_stack
  m.g.staticReadWrite1 += 1

  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: metatype $@thick G<T>.Type
  m.g.staticReadWrite2 = Box(number: 2)

  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick G<T>.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  m.g.staticReadWrite2.number += 5

  // CHECK: function_ref @_T019protocol_extensions10inout_funcySizF
  // CHECK: alloc_stack $Box
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfgZ
  // CHECK: metatype $@thick G<T>.Type
  // CHECK: store
  // CHECK: load
  // CHECK: function_ref @_T019protocol_extensions2P1PAAE16staticReadWrite2AA3BoxVfsZ
  // CHECK: dealloc_stack
  inout_func(&m.g.staticReadWrite2.number)

  // CHECK: return
}

// ----------------------------------------------------------------------------
// Using protocol extension members with existentials
// ----------------------------------------------------------------------------
extension P1 {
  final func f1() { }

  final subscript (i: Int64) -> Bool {
    get { return true }
  }

  final var prop: Bool {
    get { return true }
  }

  final func returnsSelf() -> Self { return self }

  final var prop2: Bool {
    get { return true }
    set { }
  }

  final subscript (b: Bool) -> Bool {
    get { return b }
    set { }
  }
}

// CHECK-LABEL: sil hidden @_T019protocol_extensions17testExistentials1{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%[0-9]+]] : $*P1, [[B:%[0-9]+]] : $Bool, [[I:%[0-9]+]] : $Int64):
func testExistentials1(_ p1: P1, b: Bool, i: Int64) {
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr immutable_access [[P]] : $*P1 to $*@opened([[UUID:".*"]])
  // CHECK: [[F1:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE2f1{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[F1]]<@opened([[UUID]]) P1>([[POPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
  p1.f1()

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr immutable_access [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]] :
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE9subscriptSbs5Int64Vcfg
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[I]], [[POPENED_COPY]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Int64, @in_guaranteed τ_0_0) -> Bool
  // CHECK: destroy_addr [[POPENED_COPY]]
  // CHECK: store{{.*}} : $*Bool
  // CHECK: dealloc_stack [[POPENED_COPY]]
  var b2 = p1[i]

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr immutable_access [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]] :
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE4propSbfg
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[POPENED_COPY]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> Bool
  // CHECK: store{{.*}} : $*Bool
  // CHECK: dealloc_stack [[POPENED_COPY]]
  var b3 = p1.prop
}

// CHECK-LABEL: sil hidden @_T019protocol_extensions17testExistentials2{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%[0-9]+]] : $*P1):
func testExistentials2(_ p1: P1) {
  // CHECK: [[P1A:%[0-9]+]] = alloc_box ${ var P1 }
  // CHECK: [[PB:%.*]] = project_box [[P1A]]
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr immutable_access [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[P1AINIT:%[0-9]+]] = init_existential_addr [[PB]] : $*P1, $@opened([[UUID2:".*"]]) P1
  // CHECK: [[FN:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE11returnsSelf{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[FN]]<@opened([[UUID]]) P1>([[P1AINIT]], [[POPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> @out τ_0_0
  var p1a: P1 = p1.returnsSelf()
}

// CHECK-LABEL: sil hidden @_T019protocol_extensions23testExistentialsGetters{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%[0-9]+]] : $*P1):
func testExistentialsGetters(_ p1: P1) {
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr immutable_access [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]] :
  // CHECK: [[FN:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE5prop2Sbfg
  // CHECK: [[B:%[0-9]+]] = apply [[FN]]<@opened([[UUID]]) P1>([[POPENED_COPY]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> Bool
  let b: Bool = p1.prop2

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr immutable_access [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]] :
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE9subscriptSbSbcfg
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[B]], [[POPENED_COPY]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, @in_guaranteed τ_0_0) -> Bool
  let b2: Bool = p1[b]
}

// CHECK-LABEL: sil hidden @_T019protocol_extensions22testExistentialSetters{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[P:%[0-9]+]] : $*P1, [[B:%[0-9]+]] : $Bool):
func testExistentialSetters(_ p1: P1, b: Bool) {
  var p1 = p1
  // CHECK: [[PBOX:%[0-9]+]] = alloc_box ${ var P1 }
  // CHECK: [[PBP:%[0-9]+]] = project_box [[PBOX]]
  // CHECK-NEXT: copy_addr [[P]] to [initialization] [[PBP]] : $*P1
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr mutable_access [[PBP]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE5prop2Sbfs
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[B]], [[POPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, @inout τ_0_0) -> ()
  // CHECK-NOT: deinit_existential_addr
  p1.prop2 = b

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr mutable_access [[PBP]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[SUBSETTER:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE9subscriptSbSbcfs
  // CHECK: apply [[SUBSETTER]]<@opened([[UUID]]) P1>([[B]], [[B]], [[POPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, Bool, @inout τ_0_0) -> ()
  // CHECK-NOT: deinit_existential_addr [[PB]] : $*P1
  p1[b] = b

  // CHECK: return
}

struct HasAP1 {
  var p1: P1

  var someP1: P1 {
    get { return p1 }
    set { p1 = newValue }
  }
}

// CHECK-LABEL: sil hidden @_T019protocol_extensions29testLogicalExistentialSetters{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[HASP1:%[0-9]+]] : $*HasAP1, [[B:%[0-9]+]] : $Bool)
func testLogicalExistentialSetters(_ hasAP1: HasAP1, _ b: Bool) {
  var hasAP1 = hasAP1
  // CHECK: [[HASP1_BOX:%[0-9]+]] = alloc_box ${ var HasAP1 }
  // CHECK: [[PBHASP1:%[0-9]+]] = project_box [[HASP1_BOX]]
  // CHECK-NEXT: copy_addr [[HASP1]] to [initialization] [[PBHASP1]] : $*HasAP1
  // CHECK: [[P1_COPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: [[HASP1_COPY:%[0-9]+]] = alloc_stack $HasAP1
  // CHECK-NEXT: copy_addr [[PBHASP1]] to [initialization] [[HASP1_COPY]] : $*HasAP1
  // CHECK: [[SOMEP1_GETTER:%[0-9]+]] = function_ref @_T019protocol_extensions6HasAP1V6someP1AA0F0_pfg : $@convention(method) (@in_guaranteed HasAP1) -> @out P1
  // CHECK: [[RESULT:%[0-9]+]] = apply [[SOMEP1_GETTER]]([[P1_COPY]], %8) : $@convention(method) (@in_guaranteed HasAP1) -> @out P1
  // CHECK: [[P1_OPENED:%[0-9]+]] = open_existential_addr mutable_access [[P1_COPY]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[PROP2_SETTER:%[0-9]+]] = function_ref @_T019protocol_extensions2P1PAAE5prop2Sbfs : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, @inout τ_0_0) -> ()
  // CHECK: apply [[PROP2_SETTER]]<@opened([[UUID]]) P1>([[B]], [[P1_OPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, @inout τ_0_0) -> ()
  // CHECK: [[SOMEP1_SETTER:%[0-9]+]] = function_ref @_T019protocol_extensions6HasAP1V6someP1AA0F0_pfs : $@convention(method) (@in P1, @inout HasAP1) -> ()
  // CHECK: apply [[SOMEP1_SETTER]]([[P1_COPY]], [[PBHASP1]]) : $@convention(method) (@in P1, @inout HasAP1) -> ()
  // CHECK-NOT: deinit_existential_addr
  hasAP1.someP1.prop2 = b
  // CHECK: return
}

func plusOneP1() -> P1 {}

// CHECK-LABEL: sil hidden @_T019protocol_extensions38test_open_existential_semantics_opaque{{[_0-9a-zA-Z]*}}F
func test_open_existential_semantics_opaque(_ guaranteed: P1,
                                            immediate: P1) {
  var immediate = immediate
  // CHECK: [[IMMEDIATE_BOX:%.*]] = alloc_box ${ var P1 }
  // CHECK: [[PB:%.*]] = project_box [[IMMEDIATE_BOX]]
  // CHECK: [[VALUE:%.*]] = open_existential_addr immutable_access %0
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])

  guaranteed.f1()
  
  // -- Need a guaranteed copy because it's immutable
  // CHECK: copy_addr [[PB]] to [initialization] [[IMMEDIATE:%.*]] :
  // CHECK: [[VALUE:%.*]] = open_existential_addr immutable_access [[IMMEDIATE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // -- Can consume the value from our own copy
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: deinit_existential_addr [[IMMEDIATE]]
  // CHECK: dealloc_stack [[IMMEDIATE]]
  immediate.f1()

  // CHECK: [[PLUS_ONE:%.*]] = alloc_stack $P1
  // CHECK: [[VALUE:%.*]] = open_existential_addr immutable_access [[PLUS_ONE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // -- Can consume the value from our own copy
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: deinit_existential_addr [[PLUS_ONE]]
  // CHECK: dealloc_stack [[PLUS_ONE]]
  plusOneP1().f1()
}

protocol CP1: class {}

extension CP1 {
  final func f1() { }
}

func plusOneCP1() -> CP1 {}

// CHECK-LABEL: sil hidden @_T019protocol_extensions37test_open_existential_semantics_class{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG0:%.*]] : $CP1, [[ARG1:%.*]] : $CP1):
func test_open_existential_semantics_class(_ guaranteed: CP1,
                                           immediate: CP1) {
  var immediate = immediate
  // CHECK: [[IMMEDIATE_BOX:%.*]] = alloc_box ${ var CP1 }
  // CHECK: [[PB:%.*]] = project_box [[IMMEDIATE_BOX]]

  // CHECK-NOT: copy_value [[ARG0]]
  // CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK: [[VALUE:%.*]] = open_existential_ref [[BORROWED_ARG0]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK-NEXT: end_borrow [[BORROWED_ARG0]] from [[ARG0]]
  // CHECK-NOT: destroy_value [[VALUE]]
  // CHECK-NOT: destroy_value [[ARG0]]
  guaranteed.f1()

  // CHECK: [[IMMEDIATE:%.*]] = load [copy] [[PB]]
  // CHECK: [[VALUE:%.*]] = open_existential_ref [[IMMEDIATE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: destroy_value [[VALUE]]
  // CHECK-NOT: destroy_value [[IMMEDIATE]]
  immediate.f1()

  // CHECK: [[F:%.*]] = function_ref {{.*}}plusOneCP1
  // CHECK: [[PLUS_ONE:%.*]] = apply [[F]]()
  // CHECK: [[VALUE:%.*]] = open_existential_ref [[PLUS_ONE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: destroy_value [[VALUE]]
  // CHECK-NOT: destroy_value [[PLUS_ONE]]
  plusOneCP1().f1()
}

protocol InitRequirement {
  init(c: C)
}

extension InitRequirement {
  // CHECK-LABEL: sil hidden @_T019protocol_extensions15InitRequirementPAAE{{[_0-9a-zA-Z]*}}fC : $@convention(method) <Self where Self : InitRequirement> (@owned D, @thick Self.Type) -> @out Self
  // CHECK:       bb0([[OUT:%.*]] : $*Self, [[ARG:%.*]] : $D, [[SELF_TYPE:%.*]] : $@thick Self.Type):
  init(d: D) {
    // CHECK:         [[DELEGATEE:%.*]] = witness_method $Self, #InitRequirement.init!allocator.1 : {{.*}} : $@convention(witness_method) <τ_0_0 where τ_0_0 : InitRequirement> (@owned C, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK:           [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:           [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK:           [[ARG_COPY_CAST:%.*]] = upcast [[ARG_COPY]]
  // CHECK:           apply [[DELEGATEE]]<Self>({{%.*}}, [[ARG_COPY_CAST]], [[SELF_TYPE]])
  // CHECK:           end_borrow [[BORROWED_ARG]] from [[ARG]]
    self.init(c: d)
  }
  // CHECK: } // end sil function '_T019protocol_extensions15InitRequirementPAAE{{[_0-9a-zA-Z]*}}fC'

  // CHECK-LABEL: sil hidden @_T019protocol_extensions15InitRequirementPAAE{{[_0-9a-zA-Z]*}}fC : $@convention(method)
  // CHECK:         function_ref @_T019protocol_extensions15InitRequirementPAAE{{[_0-9a-zA-Z]*}}fC
  // CHECK: } // end sil function '_T019protocol_extensions15InitRequirementPAAE{{[_0-9a-zA-Z]*}}fC'
  init(d2: D) {
    self.init(d: d2)
  }
}

protocol ClassInitRequirement: class {
  init(c: C)
}

extension ClassInitRequirement {
  // CHECK-LABEL: sil hidden @_T019protocol_extensions20ClassInitRequirementPAAE{{[_0-9a-zA-Z]*}}fC : $@convention(method) <Self where Self : ClassInitRequirement> (@owned D, @thick Self.Type) -> @owned Self
  // CHECK:       bb0([[ARG:%.*]] : $D, [[SELF_TYPE:%.*]] : $@thick Self.Type):
  // CHECK:         [[DELEGATEE:%.*]] = witness_method $Self, #ClassInitRequirement.init!allocator.1 : {{.*}} : $@convention(witness_method) <τ_0_0 where τ_0_0 : ClassInitRequirement> (@owned C, @thick τ_0_0.Type) -> @owned τ_0_0
  // CHECK:         [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:         [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK:         [[ARG_COPY_CAST:%.*]] = upcast [[ARG_COPY]]
  // CHECK:         apply [[DELEGATEE]]<Self>([[ARG_COPY_CAST]], [[SELF_TYPE]])
  // CHECK:         end_borrow [[BORROWED_ARG]] from [[ARG]]
  
  // CHECK: } // end sil function '_T019protocol_extensions20ClassInitRequirementPAAE{{[_0-9a-zA-Z]*}}fC'
  init(d: D) {
    self.init(c: d)
  }
}

@objc class OC {}
@objc class OD: OC {}

@objc protocol ObjCInitRequirement {
  init(c: OC, d: OC)
}

func foo(_ t: ObjCInitRequirement.Type, c: OC) -> ObjCInitRequirement {
  return t.init(c: OC(), d: OC())
}

extension ObjCInitRequirement {
  // CHECK-LABEL: sil hidden @_T019protocol_extensions19ObjCInitRequirementPAAE{{[_0-9a-zA-Z]*}}fC : $@convention(method) <Self where Self : ObjCInitRequirement> (@owned OD, @thick Self.Type) -> @owned Self
  // CHECK:       bb0([[ARG:%.*]] : $OD, [[SELF_TYPE:%.*]] : $@thick Self.Type):
  // CHECK:         [[OBJC_SELF_TYPE:%.*]] = thick_to_objc_metatype [[SELF_TYPE]]
  // CHECK:         [[SELF:%.*]] = alloc_ref_dynamic [objc] [[OBJC_SELF_TYPE]] : $@objc_metatype Self.Type, $Self
  // CHECK:         [[WITNESS:%.*]] = witness_method [volatile] $Self, #ObjCInitRequirement.init!initializer.1.foreign : {{.*}} : $@convention(objc_method) <τ_0_0 where τ_0_0 : ObjCInitRequirement> (OC, OC, @owned τ_0_0) -> @owned τ_0_0
  // CHECK:         [[BORROWED_ARG_1:%.*]] = begin_borrow [[ARG]]
  // CHECK:         [[ARG_COPY_1:%.*]] = copy_value [[BORROWED_ARG_1]]
  // CHECK:         [[ARG_COPY_1_UPCAST:%.*]] = upcast [[ARG_COPY_1]]
  // CHECK:         [[BORROWED_ARG_2:%.*]] = begin_borrow [[ARG]]
  // CHECK:         [[ARG_COPY_2:%.*]] = copy_value [[BORROWED_ARG_2]]
  // CHECK:         [[ARG_COPY_2_UPCAST:%.*]] = upcast [[ARG_COPY_2]]
  // CHECK:         apply [[WITNESS]]<Self>([[ARG_COPY_1_UPCAST]], [[ARG_COPY_2_UPCAST]], [[SELF]])
  // CHECK:         end_borrow [[BORROWED_ARG_2]] from [[ARG]]
  // CHECK:         end_borrow [[BORROWED_ARG_1]] from [[ARG]]
  // CHECK: } // end sil function '_T019protocol_extensions19ObjCInitRequirementPAAE{{[_0-9a-zA-Z]*}}fC'
  init(d: OD) {
    self.init(c: d, d: d)
  }
}

// rdar://problem/21370992 - delegation from an initializer in a
// protocol extension to an @objc initializer in a class.
class ObjCInitClass {
  @objc init() { }
}

protocol ProtoDelegatesToObjC { }

extension ProtoDelegatesToObjC where Self : ObjCInitClass {
  // CHECK-LABEL: sil hidden @_T019protocol_extensions20ProtoDelegatesToObjCPAaA0F10CInitClassC{{[_0-9a-zA-Z]*}}fC
  // CHECK: bb0([[STR:%[0-9]+]] : $String, [[SELF_META:%[0-9]+]] : $@thick Self.Type):
  init(string: String) {
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0 where τ_0_0 : ObjCInitClass, τ_0_0 : ProtoDelegatesToObjC> { var τ_0_0 } <Self>
    // CHECK:   [[PB:%.*]] = project_box [[SELF_BOX]]
    // CHECK:   [[SELF:%[0-9]+]] = mark_uninitialized [delegatingself] [[PB]] : $*Self
    // CHECK:   [[SELF_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[SELF_META]] : $@thick Self.Type to $@objc_metatype Self.Type
    // CHECK:   [[SELF_ALLOC:%[0-9]+]] = alloc_ref_dynamic [objc] [[SELF_META_OBJC]] : $@objc_metatype Self.Type, $Self
    // CHECK:   [[SELF_ALLOC_C:%[0-9]+]] = upcast [[SELF_ALLOC]] : $Self to $ObjCInitClass
    // CHECK:   [[OBJC_INIT:%[0-9]+]] = class_method [[SELF_ALLOC_C]] : $ObjCInitClass, #ObjCInitClass.init!initializer.1 : (ObjCInitClass.Type) -> () -> ObjCInitClass, $@convention(method) (@owned ObjCInitClass) -> @owned ObjCInitClass
    // CHECK:   [[SELF_RESULT:%[0-9]+]] = apply [[OBJC_INIT]]([[SELF_ALLOC_C]]) : $@convention(method) (@owned ObjCInitClass) -> @owned ObjCInitClass
    // CHECK:   [[SELF_RESULT_AS_SELF:%[0-9]+]] = unchecked_ref_cast [[SELF_RESULT]] : $ObjCInitClass to $Self
    // CHECK:   assign [[SELF_RESULT_AS_SELF]] to [[SELF]] : $*Self
    self.init()
  }
}

// Delegating from an initializer in a protocol extension where Self
// has a superclass to a required initializer of that class.
class RequiredInitClass {
  required init() { }
}

protocol ProtoDelegatesToRequired { }

extension ProtoDelegatesToRequired where Self : RequiredInitClass {
  // CHECK-LABEL: sil hidden @_T019protocol_extensions24ProtoDelegatesToRequiredPAaA0F9InitClassC{{[_0-9a-zA-Z]*}}fC 
  // CHECK: bb0([[STR:%[0-9]+]] : $String, [[SELF_META:%[0-9]+]] : $@thick Self.Type):
  init(string: String) {
  // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0 where τ_0_0 : RequiredInitClass, τ_0_0 : ProtoDelegatesToRequired> { var τ_0_0 } <Self>
  // CHECK:   [[PB:%.*]] = project_box [[SELF_BOX]]
  // CHECK:   [[SELF:%[0-9]+]] = mark_uninitialized [delegatingself] [[PB]] : $*Self
  // CHECK:   [[SELF_META_AS_CLASS_META:%[0-9]+]] = upcast [[SELF_META]] : $@thick Self.Type to $@thick RequiredInitClass.Type
  // CHECK:   [[INIT:%[0-9]+]] = class_method [[SELF_META_AS_CLASS_META]] : $@thick RequiredInitClass.Type, #RequiredInitClass.init!allocator.1 : (RequiredInitClass.Type) -> () -> RequiredInitClass, $@convention(method) (@thick RequiredInitClass.Type) -> @owned RequiredInitClass
  // CHECK:   [[SELF_RESULT:%[0-9]+]] = apply [[INIT]]([[SELF_META_AS_CLASS_META]]) : $@convention(method) (@thick RequiredInitClass.Type) -> @owned RequiredInitClass
  // CHECK:   [[SELF_RESULT_AS_SELF:%[0-9]+]] = unchecked_ref_cast [[SELF_RESULT]] : $RequiredInitClass to $Self
  // CHECK:   assign [[SELF_RESULT_AS_SELF]] to [[SELF]] : $*Self
    self.init()
  }
}

// ----------------------------------------------------------------------------
// Default implementations via protocol extensions
// ----------------------------------------------------------------------------

protocol P2 {
  associatedtype A
  func f1(_ a: A)
  func f2(_ a: A)
  var x: A { get }
}

extension P2 {
  // CHECK-LABEL: sil hidden @_T019protocol_extensions2P2PAAE2f1{{[_0-9a-zA-Z]*}}F
  // CHECK: witness_method $Self, #P2.f2!1
  // CHECK: function_ref @_T019protocol_extensions2P2PAAE2f3{{[_0-9a-zA-Z]*}}F
  // CHECK: return
  func f1(_ a: A) {
    f2(a)
    f3(a)
  }

  // CHECK-LABEL: sil hidden @_T019protocol_extensions2P2PAAE2f2{{[_0-9a-zA-Z]*}}F
  // CHECK: witness_method $Self, #P2.f1!1
  // CHECK: function_ref @_T019protocol_extensions2P2PAAE2f3{{[_0-9a-zA-Z]*}}F
  // CHECK: return
  func f2(_ a: A) {
    f1(a)
    f3(a)
  }

  func f3(_ a: A) {}

  // CHECK-LABEL: sil hidden @_T019protocol_extensions2P2PAAE2f4{{[_0-9a-zA-Z]*}}F
  // CHECK: witness_method $Self, #P2.f1!1
  // CHECK: witness_method $Self, #P2.f2!1
  // CHECK: return
  func f4() {
    f1(x)
    f2(x)
  }
}
