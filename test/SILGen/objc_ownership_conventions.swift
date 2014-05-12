// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | FileCheck %s

import gizmo

// CHECK-LABEL: sil  @_TF26objc_ownership_conventions5test3FT_CSo8NSObject
func test3() -> NSObject {
  // initializer returns at +1
  return Gizmo()
  // CHECK: [[CTOR:%[0-9]+]] = function_ref @_TFCSo5GizmoCfMS_FT_S_ : $@thin (@thick Gizmo.Type) -> @owned Gizmo
  // CHECK-NEXT: [[GIZMO_META:%[0-9]+]] = metatype $@thick Gizmo.Type
  // CHECK-NEXT: [[GIZMO:%[0-9]+]] = apply [[CTOR]]([[GIZMO_META]]) : $@thin (@thick Gizmo.Type) -> @owned Gizmo
  // CHECK-NEXT: [[GIZMO_NS:%[0-9]+]] = upcast [[GIZMO]] : $Gizmo to $NSObject
  // CHECK-NEXT: return [[GIZMO_NS]] : $NSObject

  // CHECK-LABEL: sil shared @_TFCSo5GizmoCfMS_FT_S_ : $@thin (@thick Gizmo.Type) -> @owned Gizmo
  // alloc is implicitly ns_returns_retained
  // init is implicitly ns_consumes_self and ns_returns_retained
  // CHECK-NEXT: bb0([[GIZMO_META:%[0-9]+]] : $@thick Gizmo.Type):
  // CHECK-NEXT: [[GIZMO_META_OBJC:%[0-9]+]] = thick_to_objc_metatype [[GIZMO_META]] : $@thick Gizmo.Type to $@objc_metatype Gizmo.Type
  // CHECK-NEXT: [[GIZMO:%[0-9]+]] = alloc_ref_dynamic [objc] [[GIZMO_META_OBJC]] : $@objc_metatype Gizmo.Type, $Gizmo
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[INIT_CTOR:%[0-9]+]] = function_ref @_TTOFCSo5GizmocfMS_FT_S_
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[INIT_CTOR]]([[GIZMO]]) : $@cc(method) @thin (@owned Gizmo) -> @owned Gizmo
  // CHECK-NEXT: return [[RESULT]] : $Gizmo
}



// Normal message send with argument, no transfers.
// CHECK-LABEL: sil  @_TF26objc_ownership_conventions5test5
func test5(var g: Gizmo) {
  Gizmo.inspect(g)
  // CHECK:      [[CLASS:%.*]] = metatype $@thick Gizmo.Type
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[CLASS]] : {{.*}}, #Gizmo.inspect!1.foreign
  // CHECK-NEXT: [[OBJC_CLASS:%[0-9]+]] = thick_to_objc_metatype [[CLASS]] : $@thick Gizmo.Type to $@objc_metatype Gizmo.Type
  // CHECK:      copy_addr %1#1 to [initialization] {{%.*}}#1 : $*Gizmo
  // CHECK:      injectValueIntoImplicitlyUnwrappedOptional
  // CHECK:      [[G:%.*]] = load {{%.*}}#1 : $*ImplicitlyUnwrappedOptional<Gizmo>
  // CHECK-NEXT: apply [[METHOD]]([[G]], [[OBJC_CLASS]])
  // CHECK-NEXT: release_value [[G]]
}
// The argument to consume is __attribute__((ns_consumed)).
// CHECK-LABEL: sil  @_TF26objc_ownership_conventions5test6
func test6(var g: Gizmo) {
  Gizmo.consume(g)
  // CHECK:      [[CLASS:%.*]] = metatype $@thick Gizmo.Type
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[CLASS]] : {{.*}}, #Gizmo.consume!1.foreign
  // CHECK-NEXT: [[OBJC_CLASS:%.*]] = thick_to_objc_metatype [[CLASS]] : $@thick Gizmo.Type to $@objc_metatype Gizmo.Type
  // CHECK:      copy_addr %1#1 to [initialization] {{%.*}}#1 : $*Gizmo
  // CHECK:      injectValueIntoImplicitlyUnwrappedOptional
  // CHECK:      [[G:%.*]] = load {{%.*}}#1 : $*ImplicitlyUnwrappedOptional<Gizmo>
  // CHECK-NEXT: apply [[METHOD]]([[G]], [[OBJC_CLASS]])
  // CHECK-NOT:  release_value [[G]]
}
// fork is __attribute__((ns_consumes_self)).
// CHECK-LABEL: sil  @_TF26objc_ownership_conventions5test7
func test7(var g: Gizmo) {
  g.fork()
  // CHECK:      [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.fork!1.foreign
  // CHECK-NEXT: apply [[METHOD]]([[G]])
  // CHECK-NOT:  release [[G]]
}
// clone is __attribute__((ns_returns_retained)).
// CHECK-LABEL: sil  @_TF26objc_ownership_conventions5test8
func test8(g: Gizmo) -> Gizmo {
  return g.clone()
  // CHECK:      retain [[G:%0]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.clone!1.foreign
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[G]])
  // CHECK-NEXT: store
  // CHECK-NEXT: function_ref
  // CHECK-NEXT: function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
  // CHECK-NEXT: alloc_stack
  // CHECK-NEXT: apply
  // CHECK-NEXT: [[RESULT:%.*]] = load
  // CHECK-NEXT: dealloc_stack
  // CHECK-NEXT: release [[G]]
  // CHECK-NEXT: dealloc_stack
  // CHECK-NEXT: release [[G]]
  // CHECK-NEXT: return [[RESULT]]
}
// duplicate returns an autoreleased object at +0.
// CHECK-LABEL: sil  @_TF26objc_ownership_conventions5test9
func test9(g: Gizmo) -> Gizmo {
  return g.duplicate()
  // CHECK:      retain [[G:%0]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.duplicate!1.foreign
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[G]])
  // CHECK-NEXT: retain_autoreleased [[RESULT]]
  // CHECK-NEXT: store [[RESULT]]
  // CHECK-NEXT: function_ref
  // CHECK-NEXT: function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
  // CHECK-NEXT: alloc_stack
  // CHECK-NEXT: apply
  // CHECK-NEXT: [[RESULT:%.*]] = load
  // CHECK-NEXT: dealloc_stack
  // CHECK-NEXT: release [[G]]
  // CHECK-NEXT: dealloc_stack
  // CHECK-NEXT: release [[G]]
  // CHECK-NEXT: return [[RESULT]]
}

// CHECK-LABEL: sil @_TF26objc_ownership_conventions6test10
func test10(let g: Gizmo) -> AnyClass {
  // CHECK: bb0([[G:%[0-9]+]] : $Gizmo):
  // CHECK:      strong_retain [[G]]
  // CHECK-NEXT: [[NS_G:%[0-9]+]] = upcast [[G:%[0-9]+]] : $Gizmo to $NSObject
  // CHECK-NEXT: [[GETTER:%[0-9]+]] = class_method [volatile] [[NS_G]] : $NSObject, #NSObject.classProp!getter.1.foreign : NSObject -> () -> AnyClass! , $@cc(objc_method) @thin (NSObject) -> ImplicitlyUnwrappedOptional<@objc_metatype AnyObject.Type>
  // CHECK-NEXT: [[OPT_OBJC:%.*]] = apply [[GETTER]]([[NS_G]]) : $@cc(objc_method) @thin (NSObject) -> ImplicitlyUnwrappedOptional<@objc_metatype AnyObject.Type>
  // CHECK:      store [[OPT_OBJC]] to [[OPT_OBJC_BUF:%.*]]#1
  // CHECK:      [[T0:%.*]] = function_ref @_TFSs41_doesImplicitlyUnwrappedOptionalHaveValueU__FRGSQQ__Bi1_
  // CHECK-NEXT: apply [transparent] [[T0]]<@objc_metatype AnyObject.Type>([[OPT_OBJC_BUF]]#1)
  // CHECK:      [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
  // CHECK:      apply [transparent] [[T0]]<@objc_metatype AnyObject.Type>([[OBJC_BUF:%.*]]#1, [[OPT_OBJC_BUF]]#1)
  // CHECK-NEXT: [[OBJC:%.*]] = load [[OBJC_BUF]]
  // CHECK-NEXT: [[THICK:%.*]] = objc_to_thick_metatype [[OBJC]]
  // CHECK:      store [[THICK]] to [[THICK_BUF:%.*]]#1
  // CHECK:      [[T0:%.*]] = function_ref @_TFSs43_injectValueIntoImplicitlyUnwrappedOptionalU__FQ_GSQQ__
  // CHECK-NEXT: apply [transparent] [[T0]]<AnyObject.Type>([[OPT_THICK_BUF:%.*]]#1, [[THICK_BUF]]#1)
  // CHECK:      [[T0:%.*]] = load [[OPT_THICK_BUF]]#1
  // CHECK-NEXT: store [[T0]] to [[OPT_THICK_BUF:%.*]]#1
  // CHECK:      [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
  // CHECK:      apply [transparent] [[T0]]<AnyObject.Type>([[THICK_BUF:%.*]]#1, [[OPT_THICK_BUF]]#1)
  // CHECK-NEXT: [[RES:%.*]] = load [[THICK_BUF]]#1
  // CHECK:      strong_release [[G]] : $Gizmo
  // CHECK:      strong_release [[G]] : $Gizmo
  // CHECK-NEXT: return [[RES]] : $@thick AnyObject.Type
  return g.classProp
}

// CHECK-LABEL: sil @_TF26objc_ownership_conventions6test11
func test11(let g: Gizmo) -> AnyClass {
  // CHECK: bb0([[G:%[0-9]+]] : $Gizmo):
  // CHECK: strong_retain [[G]]
  // CHECK: [[NS_G:%[0-9]+]] = upcast [[G:%[0-9]+]] : $Gizmo to $NSObject
  // CHECK: [[GETTER:%[0-9]+]] = class_method [volatile] [[NS_G]] : $NSObject, #NSObject.qualifiedClassProp!getter.1.foreign : NSObject -> () -> AnyObject.Type! , $@cc(objc_method) @thin (NSObject) -> ImplicitlyUnwrappedOptional<@objc_metatype AnyObject.Type>
  // CHECK-NEXT: [[OPT_OBJC:%.*]] = apply [[GETTER]]([[NS_G]]) : $@cc(objc_method) @thin (NSObject) -> ImplicitlyUnwrappedOptional<@objc_metatype AnyObject.Type>
  // CHECK:      store [[OPT_OBJC]] to [[OPT_OBJC_BUF:%.*]]#1
  // CHECK:      [[T0:%.*]] = function_ref @_TFSs41_doesImplicitlyUnwrappedOptionalHaveValueU__FRGSQQ__Bi1_
  // CHECK-NEXT: apply [transparent] [[T0]]<@objc_metatype AnyObject.Type>([[OPT_OBJC_BUF]]#1)
  // CHECK:      [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
  // CHECK:      apply [transparent] [[T0]]<@objc_metatype AnyObject.Type>([[OBJC_BUF:%.*]]#1, [[OPT_OBJC_BUF]]#1)
  // CHECK-NEXT: [[OBJC:%.*]] = load [[OBJC_BUF]]
  // CHECK-NEXT: [[THICK:%.*]] = objc_to_thick_metatype [[OBJC]]
  // CHECK:      store [[THICK]] to [[THICK_BUF:%.*]]#1
  // CHECK:      [[T0:%.*]] = function_ref @_TFSs43_injectValueIntoImplicitlyUnwrappedOptionalU__FQ_GSQQ__
  // CHECK-NEXT: apply [transparent] [[T0]]<AnyObject.Type>([[OPT_THICK_BUF:%.*]]#1, [[THICK_BUF]]#1)
  // CHECK:      [[T0:%.*]] = load [[OPT_THICK_BUF]]#1
  // CHECK-NEXT: store [[T0]] to [[OPT_THICK_BUF:%.*]]#1
  // CHECK:      [[T0:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
  // CHECK:      apply [transparent] [[T0]]<AnyObject.Type>([[THICK_BUF:%.*]]#1, [[OPT_THICK_BUF]]#1)
  // CHECK-NEXT: [[RES:%.*]] = load [[THICK_BUF]]#1
  // CHECK:      strong_release [[G]] : $Gizmo
  // CHECK:      strong_release [[G]] : $Gizmo
  // CHECK-NEXT: return [[RES]] : $@thick AnyObject.Type
  return g.qualifiedClassProp
}

// ObjC blocks should have cdecl calling convention and follow C/ObjC
// ownership conventions, where the callee, arguments, and return are all +0.
// CHECK-LABEL: sil @_TF26objc_ownership_conventions10applyBlock
func applyBlock(f: @objc_block Gizmo -> Gizmo, x: Gizmo) -> Gizmo {
  // CHECK:     bb0([[BLOCK:%.*]] : $@cc(cdecl) @objc_block (Gizmo) -> @autoreleased Gizmo, [[ARG:%.*]] : $Gizmo):
  // CHECK:       [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:       strong_retain [[BLOCK_COPY]]
  // CHECK:       strong_retain [[ARG]]
  // CHECK:       [[RESULT:%.*]] = apply [[BLOCK_COPY]]([[ARG]])
  // CHECK:       strong_retain_autoreleased [[RESULT]]
  // CHECK:       strong_release [[ARG]]
  // CHECK:       strong_release [[BLOCK_COPY]]
  // CHECK:       strong_release [[BLOCK]]
  // CHECK:       strong_release [[ARG]]
  // CHECK:       strong_release [[BLOCK_COPY]]
  // CHECK:       return [[RESULT]]
  return f(x)
}

func useInnerPointer(p: COpaquePointer) {}

// Handle inner-pointer methods by autoreleasing self after the call.
// CHECK-LABEL: sil @_TF26objc_ownership_conventions18innerPointerMethod
// CHECK:         [[USE:%.*]] = function_ref @_TF26objc_ownership_conventions15useInnerPointer
// CHECK:         strong_retain %0
// CHECK:         [[METHOD:%.*]] = class_method [volatile] %0 : $Gizmo, #Gizmo.getBytes!1.foreign : Gizmo -> () -> COpaquePointer , $@cc(objc_method) @thin (Gizmo) -> @unowned_inner_pointer COpaquePointer
// CHECK:         [[PTR:%.*]] = apply [[METHOD]](%0)
// CHECK:         autorelease_value %0
// CHECK:         apply [[USE]]([[PTR]])
// CHECK:         strong_release %0
func innerPointerMethod(g: Gizmo) {
  useInnerPointer(g.getBytes())
}

// CHECK-LABEL: sil @_TF26objc_ownership_conventions20innerPointerProperty
// CHECK:         [[USE:%.*]] = function_ref @_TF26objc_ownership_conventions15useInnerPointer
// CHECK:         strong_retain %0
// CHECK:         [[METHOD:%.*]] = class_method [volatile] %0 : $Gizmo, #Gizmo.innerProperty!getter.1.foreign : Gizmo -> () -> COpaquePointer , $@cc(objc_method) @thin (Gizmo) -> @unowned_inner_pointer COpaquePointer
// CHECK:         [[PTR:%.*]] = apply [[METHOD]](%0)
// CHECK:         autorelease_value %0
// CHECK:         apply [[USE]]([[PTR]])
// CHECK:         strong_release %0
func innerPointerProperty(g: Gizmo) {
  useInnerPointer(g.innerProperty)
}
