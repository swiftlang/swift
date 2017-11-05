// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -I %t -emit-silgen -enable-sil-ownership -enable-resilience %s | %FileCheck %s

import resilient_class

// Accessing final property of resilient class from different resilience domain
// through accessor

// CHECK-LABEL: sil @_T016class_resilience20finalPropertyOfOthery010resilient_A022ResilientOutsideParentCF
// CHECK: function_ref @_T015resilient_class22ResilientOutsideParentC13finalPropertySSvg

public func finalPropertyOfOther(_ other: ResilientOutsideParent) {
  _ = other.finalProperty
}

public class MyResilientClass {
  public final var finalProperty: String = "MyResilientClass.finalProperty"

  // CHECK-LABEL: sil [thunk] @_T016class_resilience16MyResilientClassC17publicMethodFirstSiSgyFTj : $@convention(method) (@guaranteed MyResilientClass) -> Optional<Int>
  // CHECK:   class_method %0 : $MyResilientClass, #MyResilientClass.publicMethodFirst!1 : (MyResilientClass) -> () -> Int?, $@convention(method) (@guaranteed MyResilientClass) -> Optional<Int>
  // CHECK:   return
  public func publicMethodFirst() -> Int? { return nil }

  // CHECK-LABEL: sil [thunk] @_T016class_resilience16MyResilientClassC18publicMethodSecondyyXlFTj : $@convention(method) (@owned AnyObject, @guaranteed MyResilientClass) -> ()
  // CHECK:   class_method %1 : $MyResilientClass, #MyResilientClass.publicMethodSecond!1 : (MyResilientClass) -> (AnyObject) -> (), $@convention(method) (@owned AnyObject, @guaranteed MyResilientClass) -> ()
  // CHECK:   return
  public func publicMethodSecond(_ a: AnyObject) {}
}

public class MyResilientSubclass : MyResilientClass {
  // This override changes ABI so it gets its own dispatch thunk
  // CHECK-LABEL: sil [thunk] @_T016class_resilience19MyResilientSubclassC17publicMethodFirstSiyFTj : $@convention(method) (@guaranteed MyResilientSubclass) -> Int
  // CHECK:   class_method %0 : $MyResilientSubclass, #MyResilientSubclass.publicMethodFirst!1 : (MyResilientSubclass) -> () -> Int, $@convention(method) (@guaranteed MyResilientSubclass) -> Int
  // CHECK: return
  public override func publicMethodFirst() -> Int { return 0 }

  public override func publicMethodSecond(_ a: AnyObject?) {}
}

// Accessing final property of resilient class from my resilience domain
// directly

// CHECK-LABEL: sil @_T016class_resilience19finalPropertyOfMineyAA16MyResilientClassCF
// CHECK: bb0([[ARG:%.*]] : @owned $MyResilientClass):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   ref_element_addr [[BORROWED_ARG]] : $MyResilientClass, #MyResilientClass.finalProperty
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]

public func finalPropertyOfMine(_ other: MyResilientClass) {
  _ = other.finalProperty
}

class SubclassOfOutsideChild : ResilientOutsideChild {
  override func method() {}

  func newMethod() {}
}

// CHECK-LABEL: sil @_T016class_resilience19callResilientMethodyAA02MyD8SubclassCF : $@convention(thin) (@owned MyResilientSubclass) -> ()
public func callResilientMethod(_ s: MyResilientSubclass) {
// CHECK:   class_method {{.*}} : $MyResilientSubclass, #MyResilientSubclass.publicMethodFirst!1
  _ = s.publicMethodFirst()
// CHECK:   class_method {{.*}} : $MyResilientSubclass, #MyResilientSubclass.publicMethodSecond!1
  s.publicMethodSecond(nil)
// CHECK:   return
}

// CHECK-LABEL: sil [serialized] @_T016class_resilience29callResilientMethodInlineableyAA02MyD8SubclassCF : $@convention(thin) (@owned MyResilientSubclass) -> ()
@_inlineable public func callResilientMethodInlineable(_ s: MyResilientSubclass) {
// CHECK:   [[FN:%.*]] = function_ref @_T016class_resilience19MyResilientSubclassC17publicMethodFirstSiyFTj : $@convention(method) (@guaranteed MyResilientSubclass) -> Int
  _ = s.publicMethodFirst()
  // CHECK:   [[FN:%.*]] = function_ref @_T016class_resilience16MyResilientClassC18publicMethodSecondyyXlFTj : $@convention(method) (@owned AnyObject, @guaranteed MyResilientClass) -> ()
  // CHECK:   [[CONVERTED:%.*]] = convert_function [[FN]] : $@convention(method) (@owned AnyObject, @guaranteed MyResilientClass) -> () to $@convention(method) (@owned Optional<AnyObject>, @guaranteed MyResilientSubclass) -> ()
  s.publicMethodSecond(nil)

  // CHECK:   function_ref @_T016class_resilience19MyResilientSubclassC17publicMethodFirstSiyFTc
  _ = s.publicMethodFirst

  // CHECK:   function_ref @_T016class_resilience19MyResilientSubclassC18publicMethodSecondyyXlSgFTc
  _ = s.publicMethodSecond
// CHECK:   return
}

// CHECK-LABEL: sil shared [serializable] [thunk] @_T016class_resilience19MyResilientSubclassC17publicMethodFirstSiyFTc : $@convention(thin) (@owned MyResilientSubclass) -> @owned @callee_guaranteed () -> Int
// CHECK:     function_ref @_T016class_resilience19MyResilientSubclassC17publicMethodFirstSiyFTj
// CHECK:     return

// CHECK-LABEL: sil shared [serializable] [thunk] @_T016class_resilience19MyResilientSubclassC18publicMethodSecondyyXlSgFTc : $@convention(thin) (@owned MyResilientSubclass) -> @owned @callee_guaranteed (@owned Optional<AnyObject>) -> ()
// CHECK:     function_ref @_T016class_resilience16MyResilientClassC18publicMethodSecondyyXlFTj : $@convention(method) (@owned AnyObject, @guaranteed MyResilientClass) -> ()
// CHECK:     return

// Note: no entries for [inherited] methods

// CHECK-LABEL: sil_vtable SubclassOfOutsideChild {
// CHECK-NEXT:  #ResilientOutsideParent.init!initializer.1: (ResilientOutsideParent.Type) -> () -> ResilientOutsideParent : _T016class_resilience22SubclassOfOutsideChildCACycfc [override]
// CHECK-NEXT:  #ResilientOutsideParent.method!1: (ResilientOutsideParent) -> () -> () : _T016class_resilience22SubclassOfOutsideChildC6methodyyF [override]
// CHECK-NEXT:  #SubclassOfOutsideChild.newMethod!1: (SubclassOfOutsideChild) -> () -> () : _T016class_resilience22SubclassOfOutsideChildC9newMethodyyF
// CHECK-NEXT:  #SubclassOfOutsideChild.deinit!deallocator: _T016class_resilience22SubclassOfOutsideChildCfD
// CHECK-NEXT: }
