// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

//===----------------------------------------------------------------------===//
// Calling Existential Subscripts
//===----------------------------------------------------------------------===//

protocol SubscriptableGet {
  subscript(a : Int) -> Int { get }
}

protocol SubscriptableGetSet {
  subscript(a : Int) -> Int { get set }
}

var subscriptableGet : SubscriptableGet
var subscriptableGetSet : SubscriptableGetSet

func use_subscript_rvalue_get(_ i : Int) -> Int {
  return subscriptableGet[i]
}

// CHECK-LABEL: sil hidden @{{.*}}use_subscript_rvalue_get
// CHECK: bb0(%0 : $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr @_Tv9protocols16subscriptableGetPS_16SubscriptableGet_ : $*SubscriptableGet
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr [[GLOB]] : $*SubscriptableGet to $*[[OPENED:@opened(.*) SubscriptableGet]]
// CHECK: [[ALLOCSTACK:%[0-9]+]] = alloc_stack $[[OPENED]]
// CHECK: copy_addr [[PROJ]] to [initialization] [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #SubscriptableGet.subscript!getter.1
// CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[METH]]<[[OPENED]]>(%0, [[ALLOCSTACK]])
// CHECK-NEXT: destroy_addr [[ALLOCSTACK]]
// CHECK-NEXT: dealloc_stack [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: return [[RESULT]]

func use_subscript_lvalue_get(_ i : Int) -> Int {
  return subscriptableGetSet[i]
}

// CHECK-LABEL: sil hidden @{{.*}}use_subscript_lvalue_get
// CHECK: bb0(%0 : $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr @_Tv9protocols19subscriptableGetSetPS_19SubscriptableGetSet_ : $*SubscriptableGetSet
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr [[GLOB]] : $*SubscriptableGetSet to $*[[OPENED:@opened(.*) SubscriptableGetSet]]
// CHECK: [[ALLOCSTACK:%[0-9]+]] = alloc_stack $[[OPENED]]
// CHECK: copy_addr [[PROJ]] to [initialization] [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #SubscriptableGetSet.subscript!getter.1
// CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[METH]]<[[OPENED]]>(%0, [[ALLOCSTACK]])
// CHECK-NEXT: destroy_addr [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: dealloc_stack [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: return [[RESULT]]

func use_subscript_lvalue_set(_ i : Int) {
  subscriptableGetSet[i] = i
}

// CHECK-LABEL: sil hidden @{{.*}}use_subscript_lvalue_set
// CHECK: bb0(%0 : $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr @_Tv9protocols19subscriptableGetSetPS_19SubscriptableGetSet_ : $*SubscriptableGetSet
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr [[GLOB]] : $*SubscriptableGetSet to $*[[OPENED:@opened(.*) SubscriptableGetSet]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #SubscriptableGetSet.subscript!setter.1
// CHECK-NEXT: apply [[METH]]<[[OPENED]]>(%0, %0, [[PROJ]])


//===----------------------------------------------------------------------===//
// Calling Archetype Subscripts
//===----------------------------------------------------------------------===//

func use_subscript_archetype_rvalue_get<T : SubscriptableGet>(_ generic : T, idx : Int) -> Int {
  return generic[idx]
}
// CHECK-LABEL: sil hidden @{{.*}}use_subscript_archetype_rvalue_get
// CHECK: bb0(%0 : $*T, %1 : $Int):
// CHECK: [[STACK:%[0-9]+]] = alloc_stack $T
// CHECK: copy_addr %0 to [initialization] [[STACK]]
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #SubscriptableGet.subscript!getter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, [[STACK]])
// CHECK-NEXT: destroy_addr [[STACK]] : $*T
// CHECK-NEXT: dealloc_stack [[STACK]] : $*T
// CHECK-NEXT: destroy_addr %0


func use_subscript_archetype_lvalue_get<T : SubscriptableGetSet>(_ generic: inout T, idx : Int) -> Int {
  return generic[idx]
}
// CHECK-LABEL: sil hidden @{{.*}}use_subscript_archetype_lvalue_get
// CHECK: bb0(%0 : $*T, %1 : $Int):
// CHECK: [[INOUTBOX:%[0-9]+]] = alloc_box $T, var, name "generic"
// CHECK: [[PB:%.*]] = project_box [[INOUTBOX]]
// CHECK: [[GUARANTEEDSTACK:%[0-9]+]] = alloc_stack $T
// CHECK: copy_addr [[PB]] to [initialization]
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #SubscriptableGetSet.subscript!getter.1
// CHECK-NEXT: [[APPLYRESULT:%[0-9]+]] = apply [[METH]]<T>(%1, [[GUARANTEEDSTACK]])
// CHECK-NEXT: destroy_addr [[GUARANTEEDSTACK]] : $*T
// CHECK-NEXT: dealloc_stack [[GUARANTEEDSTACK]] : $*T
// CHECK-NEXT: copy_addr [[PB]] to %0 : $*T
// CHECK-NEXT: strong_release [[INOUTBOX]] : $@box T
// CHECK: return [[APPLYRESULT]]


func use_subscript_archetype_lvalue_set<T : SubscriptableGetSet>(_ generic: inout T, idx : Int) {
  generic[idx] = idx
}
// CHECK-LABEL: sil hidden @{{.*}}use_subscript_archetype_lvalue_set
// CHECK: bb0(%0 : $*T, %1 : $Int):
// CHECK: [[INOUTBOX:%[0-9]+]] = alloc_box $T
// CHECK: [[PB:%.*]] = project_box [[INOUTBOX]]
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #SubscriptableGetSet.subscript!setter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, %1, [[PB]])
// CHECK: strong_release [[INOUTBOX]]


//===----------------------------------------------------------------------===//
// Calling Existential Properties
//===----------------------------------------------------------------------===//

protocol PropertyWithGetter {
  var a : Int { get }
}

protocol PropertyWithGetterSetter {
  var b : Int { get set }
}


var propertyGet : PropertyWithGetter
var propertyGetSet : PropertyWithGetterSetter

func use_property_rvalue_get() -> Int {
  return propertyGet.a
}
// CHECK-LABEL: sil hidden @{{.*}}use_property_rvalue_get
// CHECK: [[GLOB:%[0-9]+]] = global_addr @_Tv9protocols11propertyGetPS_18PropertyWithGetter_ : $*PropertyWithGetter
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr [[GLOB]] : $*PropertyWithGetter to $*[[OPENED:@opened(.*) PropertyWithGetter]]
// CHECK: [[COPY:%.*]] = alloc_stack $[[OPENED]]
// CHECK-NEXT: copy_addr [[PROJ]] to [initialization] [[COPY]] : $*[[OPENED]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #PropertyWithGetter.a!getter.1
// CHECK-NEXT: apply [[METH]]<[[OPENED]]>([[COPY]])

func use_property_lvalue_get() -> Int {
  return propertyGetSet.b
}
// CHECK-LABEL: sil hidden @{{.*}}use_property_lvalue_get
// CHECK: [[GLOB:%[0-9]+]] = global_addr @_Tv9protocols14propertyGetSetPS_24PropertyWithGetterSetter_ : $*PropertyWithGetterSetter
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr [[GLOB]] : $*PropertyWithGetterSetter to $*[[OPENED:@opened(.*) PropertyWithGetterSetter]]
// CHECK: [[STACK:%[0-9]+]] = alloc_stack $[[OPENED]]
// CHECK: copy_addr [[PROJ]] to [initialization] [[STACK]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #PropertyWithGetterSetter.b!getter.1
// CHECK-NEXT: apply [[METH]]<[[OPENED]]>([[STACK]])

func use_property_lvalue_set(_ x : Int) {
  propertyGetSet.b = x
}

// CHECK-LABEL: sil hidden @{{.*}}use_property_lvalue_set
// CHECK: bb0(%0 : $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr @_Tv9protocols14propertyGetSetPS_24PropertyWithGetterSetter_ : $*PropertyWithGetterSetter
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr [[GLOB]] : $*PropertyWithGetterSetter to $*[[OPENED:@opened(.*) PropertyWithGetterSetter]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #PropertyWithGetterSetter.b!setter.1
// CHECK-NEXT: apply [[METH]]<[[OPENED]]>(%0, [[PROJ]])

//===----------------------------------------------------------------------===//
// Calling Archetype Properties
//===----------------------------------------------------------------------===//

func use_property_archetype_rvalue_get<T : PropertyWithGetter>(_ generic : T) -> Int {
  return generic.a
}

// CHECK-LABEL: sil hidden @{{.*}}use_property_archetype_rvalue_get
// CHECK: bb0(%0 : $*T):
// CHECK: [[STACK:%[0-9]+]] = alloc_stack $T
// CHECK: copy_addr %0 to [initialization] [[STACK]]
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #PropertyWithGetter.a!getter.1
// CHECK-NEXT: apply [[METH]]<T>([[STACK]])
// CHECK-NEXT: destroy_addr [[STACK]]
// CHECK-NEXT: dealloc_stack [[STACK]]
// CHECK-NEXT: destroy_addr %0


func use_property_archetype_lvalue_get<T : PropertyWithGetterSetter>(_ generic : T) -> Int {
  return generic.b
}

// CHECK-LABEL: sil hidden @{{.*}}use_property_archetype_lvalue_get
// CHECK: bb0(%0 : $*T):
// CHECK: [[STACK:%[0-9]+]] = alloc_stack $T
// CHECK: copy_addr %0 to [initialization] [[STACK]] : $*T
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #PropertyWithGetterSetter.b!getter.1
// CHECK-NEXT: apply [[METH]]<T>([[STACK]])
// CHECK-NEXT: destroy_addr [[STACK]] : $*T
// CHECK-NEXT: dealloc_stack [[STACK]] : $*T
// CHECK-NEXT: destroy_addr %0


func use_property_archetype_lvalue_set<T : PropertyWithGetterSetter>(_ generic: inout T, v : Int) {
  generic.b = v
}
// CHECK-LABEL: sil hidden @{{.*}}use_property_archetype_lvalue_set
// CHECK: bb0(%0 : $*T, %1 : $Int):
// CHECK: [[INOUTBOX:%[0-9]+]] = alloc_box $T
// CHECK: [[PB:%.*]] = project_box [[INOUTBOX]]
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #PropertyWithGetterSetter.b!setter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, [[PB]])
// CHECK: strong_release [[INOUTBOX]]

//===----------------------------------------------------------------------===//
// Calling Initializers
//===----------------------------------------------------------------------===//
protocol Initializable {
  init(int: Int)
}

// CHECK-LABEL: sil hidden @_TF9protocols27use_initializable_archetype
func use_initializable_archetype<T: Initializable>(_ t: T, i: Int) {
  // CHECK:   [[T_INIT:%[0-9]+]] = witness_method $T, #Initializable.init!allocator.1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : Initializable> (Int, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK:   [[T_META:%[0-9]+]] = metatype $@thick T.Type
  // CHECK:   [[T_RESULT:%[0-9]+]] = alloc_stack $T
  // CHECK:   [[T_RESULT_ADDR:%[0-9]+]] = apply [[T_INIT]]<T>([[T_RESULT]], %1, [[T_META]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : Initializable> (Int, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK:   destroy_addr [[T_RESULT]] : $*T
  // CHECK:   dealloc_stack [[T_RESULT]] : $*T
  // CHECK:   destroy_addr [[VAR_0:%[0-9]+]] : $*T
  // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
  T(int: i)
}

// CHECK: sil hidden @_TF9protocols29use_initializable_existential
func use_initializable_existential(_ im: Initializable.Type, i: Int) {
// CHECK: bb0([[IM:%[0-9]+]] : $@thick Initializable.Type, [[I:%[0-9]+]] : $Int):
// CHECK:   [[ARCHETYPE_META:%[0-9]+]] = open_existential_metatype [[IM]] : $@thick Initializable.Type to $@thick (@opened([[N:".*"]]) Initializable).Type
// CHECK:   [[TEMP_VALUE:%[0-9]+]] = alloc_stack $Initializable
// CHECK:   [[TEMP_ADDR:%[0-9]+]] = init_existential_addr [[TEMP_VALUE]] : $*Initializable, $@opened([[N]]) Initializable
// CHECK:   [[INIT_WITNESS:%[0-9]+]] = witness_method $@opened([[N]]) Initializable, #Initializable.init!allocator.1, [[ARCHETYPE_META]]{{.*}} : $@convention(witness_method) <τ_0_0 where τ_0_0 : Initializable> (Int, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[INIT_RESULT:%[0-9]+]] = apply [[INIT_WITNESS]]<@opened([[N]]) Initializable>([[TEMP_ADDR]], [[I]], [[ARCHETYPE_META]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : Initializable> (Int, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   destroy_addr [[TEMP_VALUE]] : $*Initializable
// CHECK:   dealloc_stack [[TEMP_VALUE]] : $*Initializable
  im.init(int: i)
// CHECK:   [[RESULT:%[0-9]+]] = tuple ()
// CHECK:   return [[RESULT]] : $()
}

//===----------------------------------------------------------------------===//
// Protocol conformance and witness table generation
//===----------------------------------------------------------------------===//

class ClassWithGetter : PropertyWithGetter {
  var a: Int {
    get {
      return 42
    }
  }
}

// Make sure we are generating a protocol witness that calls the class method on
// ClassWithGetter.
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9protocols15ClassWithGetterS_18PropertyWithGetterS_FS1_g1aSi : $@convention(witness_method) (@in_guaranteed ClassWithGetter) -> Int {
// CHECK: bb0([[C:%.*]] : $*ClassWithGetter):
// CHECK-NEXT: [[CCOPY:%.*]] = alloc_stack $ClassWithGetter
// CHECK-NEXT: copy_addr [[C]] to [initialization] [[CCOPY]]
// CHECK-NEXT: [[CCOPY_LOADED:%.*]] = load [[CCOPY]]
// CHECK-NEXT: [[FUN:%.*]] = class_method [[CCOPY_LOADED]] : $ClassWithGetter, #ClassWithGetter.a!getter.1 : (ClassWithGetter) -> () -> Int , $@convention(method) (@guaranteed ClassWithGetter) -> Int
// CHECK-NEXT: apply [[FUN]]([[CCOPY_LOADED]])
// CHECK-NEXT: strong_release [[CCOPY_LOADED]]
// CHECK-NEXT: dealloc_stack [[CCOPY]]
// CHECK-NEXT: return

class ClassWithGetterSetter : PropertyWithGetterSetter, PropertyWithGetter {
  var a: Int {
    get {
      return 1
    }
    set {}
  }
  var b: Int {
    get {
      return 2
    }
    set {}
  }
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC9protocols21ClassWithGetterSetterS_24PropertyWithGetterSetterS_FS1_g1bSi : $@convention(witness_method) (@in_guaranteed ClassWithGetterSetter) -> Int {
// CHECK: bb0([[C:%.*]] : $*ClassWithGetterSetter):
// CHECK-NEXT: [[CCOPY:%.*]] = alloc_stack $ClassWithGetterSetter
// CHECK-NEXT: copy_addr [[C]] to [initialization] [[CCOPY]]
// CHECK-NEXT: [[CCOPY_LOADED:%.*]] = load [[CCOPY]]
// CHECK-NEXT: [[FUN:%.*]] = class_method [[CCOPY_LOADED]] : $ClassWithGetterSetter, #ClassWithGetterSetter.b!getter.1 : (ClassWithGetterSetter) -> () -> Int , $@convention(method) (@guaranteed ClassWithGetterSetter) -> Int
// CHECK-NEXT: apply [[FUN]]([[CCOPY_LOADED]])
// CHECK-NEXT: strong_release [[CCOPY_LOADED]]
// CHECK-NEXT: dealloc_stack [[CCOPY]]
// CHECK-NEXT: return

// Stored variables fulfilling property requirements
//
class ClassWithStoredProperty : PropertyWithGetter {
  var a : Int = 0

  // Make sure that accesses go through the generated accessors for classes.
  func methodUsingProperty() -> Int {
    return a
  }
  // CHECK-LABEL: sil hidden @{{.*}}ClassWithStoredProperty{{.*}}methodUsingProperty
  // CHECK: bb0([[ARG:%.*]] : $ClassWithStoredProperty):
  // CHECK-NEXT: debug_value [[ARG]]
  // CHECK-NOT: strong_retain
  // CHECK-NEXT: [[FUN:%.*]] = class_method [[ARG]] : $ClassWithStoredProperty, #ClassWithStoredProperty.a!getter.1 : (ClassWithStoredProperty) -> () -> Int , $@convention(method) (@guaranteed ClassWithStoredProperty) -> Int
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[FUN]]([[ARG]])
  // CHECK-NOT: strong_release
  // CHECK-NEXT: return [[RESULT]] : $Int
}

struct StructWithStoredProperty : PropertyWithGetter {
  var a : Int

  // Make sure that accesses aren't going through the generated accessors.
  func methodUsingProperty() -> Int {
    return a
  }
  // CHECK-LABEL: sil hidden @{{.*}}StructWithStoredProperty{{.*}}methodUsingProperty
  // CHECK: bb0(%0 : $StructWithStoredProperty):
  // CHECK-NEXT: debug_value %0
  // CHECK-NEXT: %2 = struct_extract %0 : $StructWithStoredProperty, #StructWithStoredProperty.a
  // CHECK-NEXT: return %2 : $Int
}

// Make sure that we generate direct function calls for out struct protocol
// witness since structs don't do virtual calls for methods.
//
// *NOTE* Even though at first glance the copy_addr looks like a leak
// here, StructWithStoredProperty is a trivial struct implying that no
// leak is occurring. See the test with StructWithStoredClassProperty
// that makes sure in such a case we don't leak. This is due to the
// thunking code being too dumb but it is harmless to program
// correctness.
//
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9protocols24StructWithStoredPropertyS_18PropertyWithGetterS_FS1_g1aSi : $@convention(witness_method) (@in_guaranteed StructWithStoredProperty) -> Int {
// CHECK: bb0([[C:%.*]] : $*StructWithStoredProperty):
// CHECK-NEXT: [[CCOPY:%.*]] = alloc_stack $StructWithStoredProperty
// CHECK-NEXT: copy_addr [[C]] to [initialization] [[CCOPY]]
// CHECK-NEXT: [[CCOPY_LOADED:%.*]] = load [[CCOPY]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[FUN:%.*]] = function_ref @_TFV9protocols24StructWithStoredPropertyg1aSi : $@convention(method) (StructWithStoredProperty) -> Int
// CHECK-NEXT: apply [[FUN]]([[CCOPY_LOADED]])
// CHECK-NEXT: dealloc_stack [[CCOPY]]
// CHECK-NEXT: return

class C {}

// Make sure that if the getter has a class property, we pass it in
// in_guaranteed and don't leak.
struct StructWithStoredClassProperty : PropertyWithGetter {
  var a : Int
  var c: C = C()

  // Make sure that accesses aren't going through the generated accessors.
  func methodUsingProperty() -> Int {
    return a
  }
  // CHECK-LABEL: sil hidden @{{.*}}StructWithStoredClassProperty{{.*}}methodUsingProperty
  // CHECK: bb0(%0 : $StructWithStoredClassProperty):
  // CHECK-NEXT: debug_value %0
  // CHECK-NEXT: %2 = struct_extract %0 : $StructWithStoredClassProperty, #StructWithStoredClassProperty.a
  // CHECK-NEXT: return %2 : $Int
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV9protocols29StructWithStoredClassPropertyS_18PropertyWithGetterS_FS1_g1aSi : $@convention(witness_method) (@in_guaranteed StructWithStoredClassProperty) -> Int {
// CHECK: bb0([[C:%.*]] : $*StructWithStoredClassProperty):
// CHECK-NEXT: [[CCOPY:%.*]] = alloc_stack $StructWithStoredClassProperty
// CHECK-NEXT: copy_addr [[C]] to [initialization] [[CCOPY]]
// CHECK-NEXT: [[CCOPY_LOADED:%.*]] = load [[CCOPY]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[FUN:%.*]] = function_ref @_TFV9protocols29StructWithStoredClassPropertyg1aSi : $@convention(method) (@guaranteed StructWithStoredClassProperty) -> Int
// CHECK-NEXT: apply [[FUN]]([[CCOPY_LOADED]])
// CHECK-NEXT: release_value [[CCOPY_LOADED]]
// CHECK-NEXT: dealloc_stack [[CCOPY]]
// CHECK-NEXT: return

// rdar://22676810

protocol ExistentialProperty {
  var p: PropertyWithGetterSetter { get set }
}

func testExistentialPropertyRead<T: ExistentialProperty>(_ t: inout T) {
    let b = t.p.b
}
// CHECK-LABEL: sil hidden @_TF9protocols27testExistentialPropertyRead
// CHECK:      [[T:%.*]] = alloc_box $T
// CHECK:      [[PB:%.*]] = project_box [[T]]
// CHECK:      copy_addr %0 to [initialization] [[PB]] : $*T
// CHECK:      [[P_TEMP:%.*]] = alloc_stack $PropertyWithGetterSetter
// CHECK:      [[T_TEMP:%.*]] = alloc_stack $T
// CHECK:      copy_addr [[PB]] to [initialization] [[T_TEMP]] : $*T
// CHECK:      [[P_GETTER:%.*]] = witness_method $T, #ExistentialProperty.p!getter.1 :
// CHECK-NEXT: apply [[P_GETTER]]<T>([[P_TEMP]], [[T_TEMP]])
// CHECK-NEXT: destroy_addr [[T_TEMP]]
// CHECK-NEXT: [[OPEN:%.*]] = open_existential_addr [[P_TEMP]] : $*PropertyWithGetterSetter to $*[[P_OPENED:@opened(.*) PropertyWithGetterSetter]]
// CHECK-NEXT: [[T0:%.*]] = alloc_stack $[[P_OPENED]]
// CHECK-NEXT: copy_addr [[OPEN]] to [initialization] [[T0]]
// CHECK-NEXT: [[B_GETTER:%.*]] = witness_method $[[P_OPENED]], #PropertyWithGetterSetter.b!getter.1
// CHECK-NEXT: apply [[B_GETTER]]<[[P_OPENED]]>([[T0]])
// CHECK-NEXT: destroy_addr [[T0]]
// CHECK-NOT:  witness_method
// CHECK:      return

func modify(_ x: inout Int) {}

// Make sure we call the materializeForSet callback with the correct
// generic signature.

func modifyProperty<T : PropertyWithGetterSetter>(_ x: inout T) {
  modify(&x.b)
}
// CHECK-LABEL: sil hidden @_TF9protocols14modifyPropertyuRxS_24PropertyWithGetterSetterrFRxT_
// CHECK:      [[SELF_BOX:%.*]] = alloc_box $T
// CHECK:      [[SELF:%.*]] = project_box %1 : $@box T
// CHECK:      [[MODIFY_FN:%.*]] = function_ref @_TF9protocols6modifyFRSiT_
// CHECK:      [[WITNESS_FN:%.*]] = witness_method $T, #PropertyWithGetterSetter.b!materializeForSet.1
// CHECK:      [[RESULT:%.*]] = apply [[WITNESS_FN]]<T>
// CHECK:      [[TEMPORARY:%.*]] = tuple_extract [[RESULT]]
// CHECK:      [[CALLBACK:%.*]] = tuple_extract [[RESULT]]
// CHECK:      [[TEMPORARY_ADDR_TMP:%.*]] = pointer_to_address [[TEMPORARY]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:      [[TEMPORARY_ADDR:%.*]] = mark_dependence [[TEMPORARY_ADDR_TMP]] : $*Int on [[SELF]] : $*T
// CHECK:      apply [[MODIFY_FN]]([[TEMPORARY_ADDR]])
// CHECK:      switch_enum [[CALLBACK]] : $Optional<Builtin.RawPointer>, case #Optional.some!enumelt.1: bb1, case #Optional.none!enumelt: bb2
// CHECK:    bb1([[CALLBACK_ADDR:%.*]] : $Builtin.RawPointer):
// CHECK:      [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]]
// CHECK:      [[METATYPE:%.*]] = metatype $@thick T.Type
// CHECK:      [[TEMPORARY:%.*]] = address_to_pointer [[TEMPORARY_ADDR]] : $*Int to $Builtin.RawPointer
// CHECK:      apply [[CALLBACK]]<T>

// CHECK-LABEL: sil_witness_table hidden ClassWithGetter: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: @_TTWC9protocols15ClassWithGetterS_18PropertyWithGetterS_FS1_g1aSi
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden ClassWithGetterSetter: PropertyWithGetterSetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!getter.1: @_TTWC9protocols21ClassWithGetterSetterS_24PropertyWithGetterSetterS_FS1_g1bSi
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!setter.1: @_TTWC9protocols21ClassWithGetterSetterS_24PropertyWithGetterSetterS_FS1_s1bSi
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!materializeForSet.1: @_TTWC9protocols21ClassWithGetterSetterS_24PropertyWithGetterSetterS_FS1_m1bSi
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden ClassWithGetterSetter: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: @_TTWC9protocols21ClassWithGetterSetterS_18PropertyWithGetterS_FS1_g1aSi
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden StructWithStoredProperty: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: @_TTWV9protocols24StructWithStoredPropertyS_18PropertyWithGetterS_FS1_g1aSi
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden StructWithStoredClassProperty: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: @_TTWV9protocols29StructWithStoredClassPropertyS_18PropertyWithGetterS_FS1_g1aSi
// CHECK-NEXT: }
