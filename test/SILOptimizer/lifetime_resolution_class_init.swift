// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -module-name test -verify %s \
// RUN:   -Xllvm -sil-print-after=raw-sil-inst-lowering -o /dev/null 2>&1 | %FileCheck %s

// For extra coverage, compile without opaque values, but skip FileCheck.
// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -disable-sil-opaque-values -verify %s -o /dev/null

// The stored properties of `self` in a root class initializer are initialized by their first
// assignment, and reassigned afterwards. When the initializer fails before all of them are
// initialized, those that are get destroyed and `self` is deallocated without running its deinit.

class Link {}

func use(_ x: AnyObject) {}
func mayThrow() throws {}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test6FieldsC4linkAcA4LinkC_tcfc :
// CHECK:         [[NEXT:%.*]] = ref_element_addr {{.*}} #Fields.next
// CHECK:         store {{.*}} to [init] [[NEXT]]
// CHECK:         [[COUNT:%.*]] = ref_element_addr {{.*}} #Fields.count
// CHECK-NEXT:    [[COUNT_ACCESS:%.*]] = begin_access [modify] [dynamic] [[COUNT]]
// CHECK-NEXT:    store {{.*}} to [trivial] [[COUNT_ACCESS]]
// CHECK:         [[PI:%.*]] = ref_element_addr {{.*}} #Fields.pi
// CHECK-NEXT:    store {{.*}} to [trivial] [[PI]]
// CHECK:         [[SELF:%.*]] = end_init_let_ref
// CHECK:         [[LINK1:%.*]] = ref_element_addr {{.*}} #Fields.link
// CHECK-NEXT:    [[LINK1_ACCESS:%.*]] = begin_access [modify] [dynamic] [[LINK1]]
// CHECK-NEXT:    [[ARG:%.*]] = copy_value %0
// CHECK-NEXT:    store [[ARG]] to [init] [[LINK1_ACCESS]]
// CHECK:         [[LINK2:%.*]] = ref_element_addr {{.*}} #Fields.link
// CHECK-NEXT:    [[LINK2_ACCESS:%.*]] = begin_access [modify] [dynamic] [[LINK2]]
// CHECK-NEXT:    store {{.*}} to [assign] [[LINK2_ACCESS]]
// CHECK:         [[COPY:%.*]] = copy_value [[SELF]]
// CHECK-NEXT:    init_existential_ref [[COPY]]
// CHECK:         return [[SELF]]
// CHECK:       } // end sil function '$s4test6FieldsC4linkAcA4LinkC_tcfc'
class Fields {
  var count: Int
  let pi: Float
  var next: Link? = nil
  var link: Link

  init(link: Link) {
    count = 0
    pi = 3
    self.link = link
    self.link = Link()
    use(self)
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test7GenericC1tACyxGx_tcfc :
// CHECK:         [[T1:%.*]] = ref_element_addr {{.*}} #Generic.t
// CHECK-NEXT:    [[T1_ACCESS:%.*]] = begin_access [modify] [dynamic] [[T1]]
// CHECK-NEXT:    [[ARG1:%.*]] = copy_value %0
// CHECK-NEXT:    store [[ARG1]] to [init] [[T1_ACCESS]]
// CHECK:         [[U:%.*]] = ref_element_addr {{.*}} #Generic.u
// CHECK-NEXT:    [[ARG2:%.*]] = copy_value %0
// CHECK-NEXT:    store [[ARG2]] to [init] [[U]]
// CHECK:         end_init_let_ref
// CHECK:         [[T2:%.*]] = ref_element_addr {{.*}} #Generic.t
// CHECK-NEXT:    [[T2_ACCESS:%.*]] = begin_access [modify] [dynamic] [[T2]]
// CHECK-NEXT:    [[ARG3:%.*]] = copy_value %0
// CHECK-NEXT:    store [[ARG3]] to [assign] [[T2_ACCESS]]
// CHECK:       } // end sil function '$s4test7GenericC1tACyxGx_tcfc'
class Generic<T> {
  var t: T
  let u: T

  init(t: T) {
    self.t = t
    self.u = t
    self.t = t
  }
}

// A fully initialized `self` is released as usual when the initializer throws.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test8ThrowingCACyKcfc :
// CHECK:         [[SELF:%.*]] = end_init_let_ref %0
// CHECK:         [[LINK:%.*]] = ref_element_addr {{.*}} #Throwing.link
// CHECK-NEXT:    [[LINK_ACCESS:%.*]] = begin_access [modify] [dynamic] [[LINK]]
// CHECK-NEXT:    store {{.*}} to [init] [[LINK_ACCESS]]
// CHECK:       bb2([[ERROR:%.*]] : @owned $any Error):
// CHECK-NEXT:    destroy_value [[SELF]]
// CHECK-NEXT:    throw [[ERROR]]
// CHECK:       } // end sil function '$s4test8ThrowingCACyKcfc'
class Throwing {
  var link: Link

  init() throws {
    link = Link()
    try mayThrow()
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test7PartialC1nACSgSi_tcfc :
// CHECK:         [[SELF:%.*]] = end_init_let_ref %1
// CHECK:         cond_br {{%.*}}, [[NONE_INIT:bb[0-9]+]], {{bb[0-9]+}}
// CHECK:       [[NONE_INIT]]:
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thick Partial.Type
// CHECK-NEXT:    dealloc_partial_ref [[SELF]], [[METATYPE]]
// CHECK:         cond_br {{%.*}}, [[X_INIT:bb[0-9]+]], {{bb[0-9]+}}
// CHECK:       [[X_INIT]]:
// CHECK-NEXT:    [[BORROW:%.*]] = begin_borrow [[SELF]]
// CHECK-NEXT:    [[X:%.*]] = ref_element_addr [[BORROW]], #Partial.x
// CHECK-NEXT:    destroy_addr [[X]]
// CHECK-NEXT:    end_borrow [[BORROW]]
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thick Partial.Type
// CHECK-NEXT:    dealloc_partial_ref [[SELF]], [[METATYPE]]
// CHECK-NOT:     destroy_value [[SELF]]
// CHECK:       } // end sil function '$s4test7PartialC1nACSgSi_tcfc'
class Partial {
  var x: Link
  var y: Link
  var n: Int

  init?(n: Int) {
    if n == 0 { return nil }
    x = Link()
    self.n = n
    if n == 1 { return nil }
    y = Link()
  }
}

// Whether `self` is fully initialized when the initializer fails is only known at runtime.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test11ConditionalC5initX4failACSgSb_Sbtcfc :
// CHECK:         [[BITS:%.*]] = alloc_stack $Builtin.Int2
// CHECK:         [[SELF:%.*]] = end_init_let_ref %2
// CHECK:         cond_br {{%.*}}, {{bb[0-9]+}}, {{bb[0-9]+}}
// CHECK:         cond_br {{%.*}}, [[FAIL:bb[0-9]+]], {{bb[0-9]+}}
// CHECK:       [[FAIL]]:
// CHECK-NEXT:    [[LOADED:%.*]] = load [trivial] [[BITS]]
// CHECK-NEXT:    [[MASK:%.*]] = integer_literal $Builtin.Int2, 1
// CHECK-NEXT:    [[X_BIT:%.*]] = builtin "and_Int2"([[LOADED]], [[MASK]])
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Int2, 0
// CHECK-NEXT:    [[IS_INIT:%.*]] = builtin "cmp_ne_Int2"([[X_BIT]], [[ZERO]])
// CHECK-NEXT:    cond_br [[IS_INIT]], [[FULLY_INIT:bb[0-9]+]], [[NOT_FULLY_INIT:bb[0-9]+]]
// CHECK:       [[FULLY_INIT]]:
// CHECK-NEXT:    destroy_value [[SELF]]
// CHECK:       [[NOT_FULLY_INIT]]:
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thick Conditional.Type
// CHECK-NEXT:    dealloc_partial_ref [[SELF]], [[METATYPE]]
// CHECK:       } // end sil function '$s4test11ConditionalC5initX4failACSgSb_Sbtcfc'
class Conditional {
  var x: Link
  var n: Int

  init?(initX: Bool, fail: Bool) {
    n = 0
    if initX { x = Link() }
    if fail { return nil }
    x = Link()
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test15ThrowsPartiallyCACyKcfc :
// CHECK:         [[SELF:%.*]] = end_init_let_ref %0
// CHECK:       bb2([[ERROR:%.*]] : @owned $any Error):
// CHECK-NEXT:    [[BORROW:%.*]] = begin_borrow [[SELF]]
// CHECK-NEXT:    [[X:%.*]] = ref_element_addr [[BORROW]], #ThrowsPartially.x
// CHECK-NEXT:    destroy_addr [[X]]
// CHECK-NEXT:    end_borrow [[BORROW]]
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thick ThrowsPartially.Type
// CHECK-NEXT:    dealloc_partial_ref [[SELF]], [[METATYPE]]
// CHECK-NEXT:    throw [[ERROR]]
// CHECK:       } // end sil function '$s4test15ThrowsPartiallyCACyKcfc'
class ThrowsPartially {
  var x: Link
  var y: Link

  init() throws {
    x = Link()
    try mayThrow()
    y = Link()
  }
}
