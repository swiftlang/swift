// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

// CHECK-LABEL: sil hidden  @_T026objc_ownership_conventions5test3So8NSObjectCyF
func test3() -> NSObject {
  // initializer returns at +1
  return Gizmo()
  // CHECK: [[CTOR:%[0-9]+]] = function_ref @_T0So5GizmoC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick Gizmo.Type) -> @owned Optional<Gizmo>
  // CHECK-NEXT: [[GIZMO_META:%[0-9]+]] = metatype $@thick Gizmo.Type
  // CHECK-NEXT: [[GIZMO:%[0-9]+]] = apply [[CTOR]]([[GIZMO_META]]) : $@convention(method) (@thick Gizmo.Type) -> @owned Optional<Gizmo>
  // CHECK: [[GIZMO_NS:%[0-9]+]] = upcast [[GIZMO:%[0-9]+]] : $Gizmo to $NSObject
  // CHECK: return [[GIZMO_NS]] : $NSObject

  // CHECK-LABEL: sil shared @_T0So5GizmoC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick Gizmo.Type) -> @owned Optional<Gizmo>
  // alloc is implicitly ns_returns_retained
  // init is implicitly ns_consumes_self and ns_returns_retained
  // CHECK: bb0([[GIZMO_META:%[0-9]+]] : $@thick Gizmo.Type):
  // CHECK-NEXT: [[GIZMO_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[GIZMO_META]] : $@thick Gizmo.Type to $@objc_metatype Gizmo.Type
  // CHECK-NEXT: [[GIZMO:%[0-9]+]] = alloc_ref_dynamic [objc] [[GIZMO_META_OBJC]] : $@objc_metatype Gizmo.Type, $Gizmo
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[INIT_CTOR:%[0-9]+]] = function_ref @_T0So5GizmoC{{[_0-9a-zA-Z]*}}fcTO
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[INIT_CTOR]]([[GIZMO]]) : $@convention(method) (@owned Gizmo) -> @owned Optional<Gizmo>
  // CHECK-NEXT: return [[RESULT]] : $Optional<Gizmo>
}



// Normal message send with argument, no transfers.
// CHECK-LABEL: sil hidden  @_T026objc_ownership_conventions5test5{{[_0-9a-zA-Z]*}}F
func test5(_ g: Gizmo) {
  var g = g
  Gizmo.inspect(g)
  // CHECK: bb0([[ARG:%.*]] : $Gizmo):
  // CHECK:   [[GIZMO_BOX:%.*]] = alloc_box ${ var Gizmo }
  // CHECK:   [[GIZMO_BOX_PB:%.*]] = project_box [[GIZMO_BOX]]
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK:   store [[ARG_COPY]] to [init] [[GIZMO_BOX_PB]]
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:   [[CLASS:%.*]] = metatype $@thick Gizmo.Type
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[CLASS]] : {{.*}}, #Gizmo.inspect!1.foreign
  // CHECK:   [[OBJC_CLASS:%[0-9]+]] = thick_to_objc_metatype [[CLASS]] : $@thick Gizmo.Type to $@objc_metatype Gizmo.Type
  // CHECK:   [[V:%.*]] = load [copy] [[GIZMO_BOX_PB]]
  // CHECK:   [[G:%.*]] = enum $Optional<Gizmo>, #Optional.some!enumelt.1, [[V]]
  // CHECK:   apply [[METHOD]]([[G]], [[OBJC_CLASS]])
  // CHECK:   destroy_value [[G]]
  // CHECK:   destroy_value [[GIZMO_BOX]]
  // CHECK:   destroy_value [[ARG]]
}
// CHECK: } // end sil function '_T026objc_ownership_conventions5test5{{[_0-9a-zA-Z]*}}F'

// The argument to consume is __attribute__((ns_consumed)).
// CHECK-LABEL: sil hidden  @_T026objc_ownership_conventions5test6{{[_0-9a-zA-Z]*}}F
func test6(_ g: Gizmo) {
  var g = g
  Gizmo.consume(g)
  // CHECK: bb0([[ARG:%.*]] : $Gizmo):
  // CHECK:   [[GIZMO_BOX:%.*]] = alloc_box ${ var Gizmo }
  // CHECK:   [[GIZMO_BOX_PB:%.*]] = project_box [[GIZMO_BOX]]
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK:   store [[ARG_COPY]] to [init] [[GIZMO_BOX_PB]]
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:   [[CLASS:%.*]] = metatype $@thick Gizmo.Type
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[CLASS]] : {{.*}}, #Gizmo.consume!1.foreign
  // CHECK:   [[OBJC_CLASS:%.*]] = thick_to_objc_metatype [[CLASS]] : $@thick Gizmo.Type to $@objc_metatype Gizmo.Type
  // CHECK:   [[V:%.*]] = load [copy] [[GIZMO_BOX_PB]]
  // CHECK:   [[G:%.*]] = enum $Optional<Gizmo>, #Optional.some!enumelt.1, [[V]]
  // CHECK:   apply [[METHOD]]([[G]], [[OBJC_CLASS]])
  // CHECK-NOT:  destroy_value [[G]]
  // CHECK:   destroy_value [[GIZMO_BOX]]
  // CHECK-NOT:  destroy_value [[G]]
  // CHECK:   destroy_value [[ARG]]
  // CHECK-NOT:  destroy_value [[G]]
}
// CHECK: } // end sil function '_T026objc_ownership_conventions5test6{{[_0-9a-zA-Z]*}}F'

// fork is __attribute__((ns_consumes_self)).
// CHECK-LABEL: sil hidden  @_T026objc_ownership_conventions5test7{{[_0-9a-zA-Z]*}}F
func test7(_ g: Gizmo) {
  var g = g
  g.fork()
  // CHECK: bb0([[ARG:%.*]] : $Gizmo):
  // CHECK:   [[GIZMO_BOX:%.*]] = alloc_box ${ var Gizmo }
  // CHECK:   [[GIZMO_BOX_PB:%.*]] = project_box [[GIZMO_BOX]]
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK:   store [[ARG_COPY]] to [init] [[GIZMO_BOX_PB]]
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:   [[G:%.*]] = load [copy] [[GIZMO_BOX_PB]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.fork!1.foreign
  // CHECK:   apply [[METHOD]]([[G]])
  // CHECK-NOT:  destroy_value [[G]]
  // CHECK:   destroy_value [[GIZMO_BOX]]
  // CHECK-NOT:  destroy_value [[G]]
  // CHECK:   destroy_value [[ARG]]
  // CHECK-NOT:  destroy_value [[G]]
}
// CHECK: } // end sil function '_T026objc_ownership_conventions5test7{{[_0-9a-zA-Z]*}}F'

// clone is __attribute__((ns_returns_retained)).
// CHECK-LABEL: sil hidden  @_T026objc_ownership_conventions5test8{{[_0-9a-zA-Z]*}}F
func test8(_ g: Gizmo) -> Gizmo {
  return g.clone()
  // CHECK: bb0([[G:%.*]] : $Gizmo):
  // CHECK-NOT:  copy_value
  // CHECK:      [[BORROWED_G:%.*]] = begin_borrow [[G]]
  // CHECK:      [[METHOD:%.*]] = class_method [volatile] [[BORROWED_G]] : {{.*}}, #Gizmo.clone!1.foreign
  // CHECK-NOT:  copy_value [[RESULT]]
  // CHECK:      bb2([[RESULT:%.*]] : $Gizmo):
  // CHECK-NOT:  copy_value [[RESULT]]
  // CHECK:      end_borrow [[BORROWED_G]] from [[G]]
  // CHECK-NEXT: destroy_value [[G]]
  // CHECK-NEXT: return [[RESULT]]
}
// duplicate returns an autoreleased object at +0.
// CHECK-LABEL: sil hidden  @_T026objc_ownership_conventions5test9{{[_0-9a-zA-Z]*}}F
func test9(_ g: Gizmo) -> Gizmo {
  return g.duplicate()
  // CHECK: bb0([[G:%.*]] : $Gizmo):
  // CHECK-NOT:      copy_value [[G:%0]]
  // CHECK: [[BORROWED_G:%.*]] = begin_borrow [[G]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_G]] : {{.*}}, #Gizmo.duplicate!1.foreign
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[BORROWED_G]])
  // CHECK-NOT:  copy_value [[RESULT]]
  // CHECK: bb2([[RESULT:%.*]] : $Gizmo):
  // CHECK-NOT:  copy_value [[RESULT]]
  // CHECK:      end_borrow [[BORROWED_G]] from [[G]]
  // CHECK-NEXT: destroy_value [[G]]
  // CHECK-NEXT: return [[RESULT]]
}

// CHECK-LABEL: sil hidden @_T026objc_ownership_conventions6test10{{[_0-9a-zA-Z]*}}F
func test10(_ g: Gizmo) -> AnyClass {
  // CHECK: bb0([[G:%[0-9]+]] : $Gizmo):
  // CHECK:      [[BORROWED_G:%.*]] = begin_borrow [[G]]
  // CHECK:      [[G_COPY:%.*]] = copy_value [[BORROWED_G]]
  // CHECK-NEXT: [[NS_G_COPY:%[0-9]+]] = upcast [[G_COPY]] : $Gizmo to $NSObject
  // CHECK-NEXT: [[GETTER:%[0-9]+]] = class_method [volatile] [[NS_G_COPY]] : $NSObject, #NSObject.classProp!getter.1.foreign : (NSObject) -> () -> AnyObject.Type!, $@convention(objc_method) (NSObject) -> Optional<@objc_metatype AnyObject.Type>
  // CHECK-NEXT: [[OPT_OBJC:%.*]] = apply [[GETTER]]([[NS_G_COPY]]) : $@convention(objc_method) (NSObject) -> Optional<@objc_metatype AnyObject.Type>
  // CHECK-NEXT: switch_enum [[OPT_OBJC]] : $Optional<{{.*}}>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[OBJC:%.*]] : $@objc_metatype AnyObject.Type):
  // CHECK-NEXT: [[THICK:%.*]] = objc_to_thick_metatype [[OBJC]]
  // CHECK:      [[T0:%.*]] = enum $Optional<@thick AnyObject.Type>, #Optional.some!enumelt.1, [[THICK]]
  // CHECK:   bb5([[RES:%.*]] : $@thick AnyObject.Type):
  // CHECK:      destroy_value [[NS_G_COPY]] : $NSObject
  // CHECK:      destroy_value [[G]] : $Gizmo
  // CHECK-NEXT: return [[RES]] : $@thick AnyObject.Type
  return g.classProp
}

// CHECK-LABEL: sil hidden @_T026objc_ownership_conventions6test11{{[_0-9a-zA-Z]*}}F
func test11(_ g: Gizmo) -> AnyClass {
  // CHECK: bb0([[G:%[0-9]+]] : $Gizmo):
  // CHECK: [[BORROWED_G:%.*]] = begin_borrow [[G]]
  // CHECK: [[G_COPY:%.*]] = copy_value [[BORROWED_G]]
  // CHECK: [[NS_G_COPY:%[0-9]+]] = upcast [[G_COPY:%[0-9]+]] : $Gizmo to $NSObject
  // CHECK: [[GETTER:%[0-9]+]] = class_method [volatile] [[NS_G_COPY]] : $NSObject, #NSObject.qualifiedClassProp!getter.1.foreign : (NSObject) -> () -> NSAnsing.Type!, $@convention(objc_method) (NSObject) -> Optional<@objc_metatype NSAnsing.Type>
  // CHECK-NEXT: [[OPT_OBJC:%.*]] = apply [[GETTER]]([[NS_G_COPY]]) : $@convention(objc_method) (NSObject) -> Optional<@objc_metatype NSAnsing.Type>
  // CHECK-NEXT: switch_enum [[OPT_OBJC]] : $Optional<{{.*}}>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[OBJC:%.*]] : $@objc_metatype NSAnsing.Type):
  // CHECK-NEXT: [[THICK:%.*]] = objc_to_thick_metatype [[OBJC]]
  // CHECK:      [[T0:%.*]] = enum $Optional<@thick NSAnsing.Type>, #Optional.some!enumelt.1, [[THICK]]
  // CHECK:   bb5([[RES:%.*]] : $@thick NSAnsing.Type):
  // CHECK:      [[OPENED:%.*]] = open_existential_metatype [[RES]]
  // CHECK:      [[RES_ANY:%.*]] = init_existential_metatype [[OPENED]]
  // CHECK:      destroy_value [[NS_G_COPY]] : $NSObject
  // CHECK:      destroy_value [[G]] : $Gizmo
  // CHECK-NEXT: return [[RES_ANY]] : $@thick AnyObject.Type
  return g.qualifiedClassProp
}

// ObjC blocks should have cdecl calling convention and follow C/ObjC
// ownership conventions, where the callee, arguments, and return are all +0.
// CHECK-LABEL: sil hidden @_T026objc_ownership_conventions10applyBlock{{[_0-9a-zA-Z]*}}F
func applyBlock(_ f: @convention(block) (Gizmo) -> Gizmo, x: Gizmo) -> Gizmo {
  // CHECK:     bb0([[BLOCK:%.*]] : $@convention(block) (Gizmo) -> @autoreleased Gizmo, [[ARG:%.*]] : $Gizmo):
  // CHECK:       [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:       [[BORROWED_BLOCK_COPY:%.*]] = begin_borrow [[BLOCK_COPY]]
  // CHECK:       [[BLOCK_COPY_COPY:%.*]] = copy_value [[BORROWED_BLOCK_COPY]]
  // CHECK:       [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:       [[RESULT:%.*]] = apply [[BLOCK_COPY_COPY]]([[BORROWED_ARG]])
  // CHECK:       end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:       destroy_value [[BLOCK_COPY_COPY]]
  // CHECK:       end_borrow [[BORROWED_BLOCK_COPY]] from [[BLOCK_COPY]]
  // CHECK:       destroy_value [[ARG]]
  // CHECK:       destroy_value [[BLOCK_COPY]]
  // CHECK:       destroy_value [[BLOCK]]
  // CHECK:       return [[RESULT]]
  return f(x)
}

// CHECK-LABEL: sil hidden @_T026objc_ownership_conventions15maybeApplyBlock{{[_0-9a-zA-Z]*}}F
func maybeApplyBlock(_ f: (@convention(block) (Gizmo) -> Gizmo)?, x: Gizmo) -> Gizmo? {
  // CHECK:     bb0([[BLOCK:%.*]] : $Optional<@convention(block) (Gizmo) -> @autoreleased Gizmo>, [[ARG:%.*]] : $Gizmo):
  // CHECK:       [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  return f?(x)
}

func useInnerPointer(_ p: UnsafeMutableRawPointer) {}

// Handle inner-pointer methods by autoreleasing self after the call.
// CHECK-LABEL: sil hidden @_T026objc_ownership_conventions18innerPointerMethodySo5GizmoCF : $@convention(thin) (@owned Gizmo) -> () {
// CHECK: bb0([[ARG:%.*]] : $Gizmo):
// CHECK:         [[USE:%.*]] = function_ref @_T026objc_ownership_conventions15useInnerPointer{{[_0-9a-zA-Z]*}}F
// CHECK:         [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:         [[METHOD:%.*]] = class_method [volatile] [[BORROWED_ARG]] : $Gizmo, #Gizmo.getBytes!1.foreign : (Gizmo) -> () -> UnsafeMutableRawPointer, $@convention(objc_method) (Gizmo) -> @unowned_inner_pointer UnsafeMutableRawPointer
// CHECK:         [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// => SEMANTIC ARC TODO: The apply below /should/ be on ARG_COPY. It is safe how
// it is today though since we are using a reference type.
// CHECK:         [[PTR:%.*]] = apply [[METHOD]]([[BORROWED_ARG]])
// CHECK:         autorelease_value [[ARG_COPY]]
// CHECK:         apply [[USE]]([[PTR]])
// CHECK:         destroy_value [[ARG]]
func innerPointerMethod(_ g: Gizmo) {
  useInnerPointer(g.getBytes())
}

// CHECK-LABEL: sil hidden @_T026objc_ownership_conventions20innerPointerPropertyySo5GizmoCF : $@convention(thin) (@owned Gizmo) -> () {
// CHECK:       bb0([[ARG:%.*]] : $Gizmo):
// CHECK:         [[USE:%.*]] = function_ref @_T026objc_ownership_conventions15useInnerPointer{{[_0-9a-zA-Z]*}}F
// CHECK:         [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:         [[METHOD:%.*]] = class_method [volatile] [[BORROWED_ARG]] : $Gizmo, #Gizmo.innerProperty!getter.1.foreign : (Gizmo) -> () -> UnsafeMutableRawPointer, $@convention(objc_method) (Gizmo) -> @unowned_inner_pointer UnsafeMutableRawPointer
// CHECK:         [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// => SEMANTIC ARC TODO: The apply below should be on ARG_COPY. It is benign since objc objects are reference types.
// CHECK:         [[PTR:%.*]] = apply [[METHOD]]([[BORROWED_ARG]])
// CHECK:         autorelease_value [[ARG_COPY]]
// CHECK:         apply [[USE]]([[PTR]])
// CHECK:         destroy_value [[ARG]]
// CHECK: } // end sil function '_T026objc_ownership_conventions20innerPointerPropertyySo5GizmoCF'
func innerPointerProperty(_ g: Gizmo) {
  useInnerPointer(g.innerProperty)
}
