// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

// CHECK-LABEL: sil hidden  @_TF26objc_ownership_conventions5test3FT_CSo8NSObject
func test3() -> NSObject {
  // initializer returns at +1
  return Gizmo()
  // CHECK: [[CTOR:%[0-9]+]] = function_ref @_TFCSo5GizmoC{{.*}} : $@convention(method) (@thick Gizmo.Type) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
  // CHECK-NEXT: [[GIZMO_META:%[0-9]+]] = metatype $@thick Gizmo.Type
  // CHECK-NEXT: [[GIZMO:%[0-9]+]] = apply [[CTOR]]([[GIZMO_META]]) : $@convention(method) (@thick Gizmo.Type) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
  // CHECK: [[GIZMO_NS:%[0-9]+]] = upcast [[GIZMO:%[0-9]+]] : $Gizmo to $NSObject
  // CHECK: return [[GIZMO_NS]] : $NSObject

  // CHECK-LABEL: sil shared @_TFCSo5GizmoC{{.*}} : $@convention(method) (@thick Gizmo.Type) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
  // alloc is implicitly ns_returns_retained
  // init is implicitly ns_consumes_self and ns_returns_retained
  // CHECK: bb0([[GIZMO_META:%[0-9]+]] : $@thick Gizmo.Type):
  // CHECK-NEXT: [[GIZMO_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[GIZMO_META]] : $@thick Gizmo.Type to $@objc_metatype Gizmo.Type
  // CHECK-NEXT: [[GIZMO:%[0-9]+]] = alloc_ref_dynamic [objc] [[GIZMO_META_OBJC]] : $@objc_metatype Gizmo.Type, $Gizmo
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[INIT_CTOR:%[0-9]+]] = function_ref @_TTOFCSo5Gizmoc{{.*}}
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[INIT_CTOR]]([[GIZMO]]) : $@convention(method) (@owned Gizmo) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
  // CHECK-NEXT: return [[RESULT]] : $ImplicitlyUnwrappedOptional<Gizmo>
}



// Normal message send with argument, no transfers.
// CHECK-LABEL: sil hidden  @_TF26objc_ownership_conventions5test5
func test5(_ g: Gizmo) {
  var g = g
  Gizmo.inspect(g)
  // CHECK:      [[CLASS:%.*]] = metatype $@thick Gizmo.Type
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[CLASS]] : {{.*}}, #Gizmo.inspect!1.foreign
  // CHECK-NEXT: [[OBJC_CLASS:%[0-9]+]] = thick_to_objc_metatype [[CLASS]] : $@thick Gizmo.Type to $@objc_metatype Gizmo.Type
  // CHECK:      [[V:%.*]] = load
  // CHECK:      strong_retain [[V]]
  // CHECK:      [[G:%.*]] = enum $ImplicitlyUnwrappedOptional<Gizmo>, #ImplicitlyUnwrappedOptional.some!enumelt.1, [[V]]
  // CHECK-NEXT: apply [[METHOD]]([[G]], [[OBJC_CLASS]])
  // CHECK-NEXT: release_value [[G]]
}
// The argument to consume is __attribute__((ns_consumed)).
// CHECK-LABEL: sil hidden  @_TF26objc_ownership_conventions5test6
func test6(_ g: Gizmo) {
  var g = g
  Gizmo.consume(g)
  // CHECK:      [[CLASS:%.*]] = metatype $@thick Gizmo.Type
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[CLASS]] : {{.*}}, #Gizmo.consume!1.foreign
  // CHECK-NEXT: [[OBJC_CLASS:%.*]] = thick_to_objc_metatype [[CLASS]] : $@thick Gizmo.Type to $@objc_metatype Gizmo.Type
  // CHECK:      [[V:%.*]] = load
  // CHECK:      strong_retain [[V]]
  // CHECK:      [[G:%.*]] = enum $ImplicitlyUnwrappedOptional<Gizmo>, #ImplicitlyUnwrappedOptional.some!
  // CHECK-NEXT: apply [[METHOD]]([[G]], [[OBJC_CLASS]])
  // CHECK-NOT:  release_value [[G]]
}
// fork is __attribute__((ns_consumes_self)).
// CHECK-LABEL: sil hidden  @_TF26objc_ownership_conventions5test7
func test7(_ g: Gizmo) {
  var g = g
  g.fork()
  // CHECK:      [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.fork!1.foreign
  // CHECK-NEXT: apply [[METHOD]]([[G]])
  // CHECK-NOT:  release [[G]]
}
// clone is __attribute__((ns_returns_retained)).
// CHECK-LABEL: sil hidden  @_TF26objc_ownership_conventions5test8
func test8(_ g: Gizmo) -> Gizmo {
  return g.clone()
  // CHECK: bb0([[G:%.*]] : $Gizmo):
  // CHECK-NOT:  retain
  // CHECK:      [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.clone!1.foreign
  // CHECK-NOT:  retain [[RESULT]]
  // CHECK:      [[RESULT:%.*]] = unchecked_enum_data
  // CHECK-NOT:  retain [[RESULT]]
  // CHECK:      return [[RESULT]]
}
// duplicate returns an autoreleased object at +0.
// CHECK-LABEL: sil hidden  @_TF26objc_ownership_conventions5test9
func test9(_ g: Gizmo) -> Gizmo {
  return g.duplicate()
  // CHECK: bb0([[G:%.*]] : $Gizmo):
  // CHECK-NOT:      retain [[G:%0]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.duplicate!1.foreign
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[G]])
  // CHECK-NOT:  retain [[RESULT]]
  // CHECK:      [[RESULT:%.*]] = unchecked_enum_data
  // CHECK-NOT:  retain [[RESULT]]
  // CHECK:      return [[RESULT]]
}

// CHECK-LABEL: sil hidden @_TF26objc_ownership_conventions6test10
func test10(_ g: Gizmo) -> AnyClass {
  // CHECK: bb0([[G:%[0-9]+]] : $Gizmo):
  // CHECK:      strong_retain [[G]]
  // CHECK-NEXT: [[NS_G:%[0-9]+]] = upcast [[G:%[0-9]+]] : $Gizmo to $NSObject
  // CHECK-NEXT: [[GETTER:%[0-9]+]] = class_method [volatile] [[NS_G]] : $NSObject, #NSObject.classProp!getter.1.foreign : (NSObject) -> () -> AnyObject.Type! , $@convention(objc_method) (NSObject) -> ImplicitlyUnwrappedOptional<@objc_metatype AnyObject.Type>
  // CHECK-NEXT: [[OPT_OBJC:%.*]] = apply [[GETTER]]([[NS_G]]) : $@convention(objc_method) (NSObject) -> ImplicitlyUnwrappedOptional<@objc_metatype AnyObject.Type>
  // CHECK:      select_enum [[OPT_OBJC]]
  // CHECK:      [[OBJC:%.*]] = unchecked_enum_data [[OPT_OBJC]]
  // CHECK-NEXT: [[THICK:%.*]] = objc_to_thick_metatype [[OBJC]]
  // CHECK:      [[T0:%.*]] = enum $ImplicitlyUnwrappedOptional<AnyObject.Type>, #ImplicitlyUnwrappedOptional.some!enumelt.1, [[THICK]]
  // CHECK:      [[RES:%.*]] = unchecked_enum_data
  // CHECK:      strong_release [[G]] : $Gizmo
  // CHECK:      strong_release [[G]] : $Gizmo
  // CHECK-NEXT: return [[RES]] : $@thick AnyObject.Type
  return g.classProp
}

// CHECK-LABEL: sil hidden @_TF26objc_ownership_conventions6test11
func test11(_ g: Gizmo) -> AnyClass {
  // CHECK: bb0([[G:%[0-9]+]] : $Gizmo):
  // CHECK: strong_retain [[G]]
  // CHECK: [[NS_G:%[0-9]+]] = upcast [[G:%[0-9]+]] : $Gizmo to $NSObject
  // CHECK: [[GETTER:%[0-9]+]] = class_method [volatile] [[NS_G]] : $NSObject, #NSObject.qualifiedClassProp!getter.1.foreign : (NSObject) -> () -> NSAnsing.Type! , $@convention(objc_method) (NSObject) -> ImplicitlyUnwrappedOptional<@objc_metatype NSAnsing.Type>
  // CHECK-NEXT: [[OPT_OBJC:%.*]] = apply [[GETTER]]([[NS_G]]) : $@convention(objc_method) (NSObject) -> ImplicitlyUnwrappedOptional<@objc_metatype NSAnsing.Type>
  // CHECK:      select_enum [[OPT_OBJC]]
  // CHECK:      [[OBJC:%.*]] = unchecked_enum_data [[OPT_OBJC]]
  // CHECK-NEXT: [[THICK:%.*]] = objc_to_thick_metatype [[OBJC]]
  // CHECK:      [[T0:%.*]] = enum $ImplicitlyUnwrappedOptional<NSAnsing.Type>, #ImplicitlyUnwrappedOptional.some!enumelt.1, [[THICK]]
  // CHECK:      [[RES:%.*]] = unchecked_enum_data
  // CHECK:      [[OPENED:%.*]] = open_existential_metatype [[RES]]
  // CHECK:      [[RES_ANY:%.*]] = init_existential_metatype [[OPENED]]
  // CHECK:      strong_release [[G]] : $Gizmo
  // CHECK:      strong_release [[G]] : $Gizmo
  // CHECK-NEXT: return [[RES_ANY]] : $@thick AnyObject.Type
  return g.qualifiedClassProp
}

// ObjC blocks should have cdecl calling convention and follow C/ObjC
// ownership conventions, where the callee, arguments, and return are all +0.
// CHECK-LABEL: sil hidden @_TF26objc_ownership_conventions10applyBlock
func applyBlock(_ f: @convention(block) (Gizmo) -> Gizmo, x: Gizmo) -> Gizmo {
  // CHECK:     bb0([[BLOCK:%.*]] : $@convention(block) (Gizmo) -> @autoreleased Gizmo, [[ARG:%.*]] : $Gizmo):
  // CHECK:       [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:       strong_retain [[BLOCK_COPY]]
  // CHECK:       [[RESULT:%.*]] = apply [[BLOCK_COPY]]([[ARG]])
  // CHECK:       strong_release [[BLOCK_COPY]]
  // CHECK:       strong_release [[ARG]]
  // CHECK:       strong_release [[BLOCK_COPY]]
  // CHECK:       strong_release [[BLOCK]]
  // CHECK:       return [[RESULT]]
  return f(x)
}

// CHECK-LABEL: sil hidden @_TF26objc_ownership_conventions15maybeApplyBlock
func maybeApplyBlock(_ f: (@convention(block) (Gizmo) -> Gizmo)?, x: Gizmo) -> Gizmo? {
  // CHECK:     bb0([[BLOCK:%.*]] : $Optional<@convention(block) (Gizmo) -> Gizmo>, [[ARG:%.*]] : $Gizmo):
  // CHECK:       [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  return f?(x)
}

func useInnerPointer(_ p: UnsafeMutableRawPointer) {}

// Handle inner-pointer methods by autoreleasing self after the call.
// CHECK-LABEL: sil hidden @_TF26objc_ownership_conventions18innerPointerMethod
// CHECK:         [[USE:%.*]] = function_ref @_TF26objc_ownership_conventions15useInnerPointer
// CHECK:         [[METHOD:%.*]] = class_method [volatile] %0 : $Gizmo, #Gizmo.getBytes!1.foreign : (Gizmo) -> () -> UnsafeMutableRawPointer , $@convention(objc_method) (Gizmo) -> @unowned_inner_pointer UnsafeMutableRawPointer
// CHECK:         strong_retain %0
// CHECK:         [[PTR:%.*]] = apply [[METHOD]](%0)
// CHECK:         autorelease_value %0
// CHECK:         apply [[USE]]([[PTR]])
// CHECK:         strong_release %0
func innerPointerMethod(_ g: Gizmo) {
  useInnerPointer(g.getBytes())
}

// CHECK-LABEL: sil hidden @_TF26objc_ownership_conventions20innerPointerProperty
// CHECK:         [[USE:%.*]] = function_ref @_TF26objc_ownership_conventions15useInnerPointer
// CHECK:         [[METHOD:%.*]] = class_method [volatile] %0 : $Gizmo, #Gizmo.innerProperty!getter.1.foreign : (Gizmo) -> () -> UnsafeMutableRawPointer , $@convention(objc_method) (Gizmo) -> @unowned_inner_pointer UnsafeMutableRawPointer
// CHECK:         strong_retain %0
// CHECK:         [[PTR:%.*]] = apply [[METHOD]](%0)
// CHECK:         autorelease_value %0
// CHECK:         apply [[USE]]([[PTR]])
// CHECK:         strong_release %0
func innerPointerProperty(_ g: Gizmo) {
  useInnerPointer(g.innerProperty)
}
