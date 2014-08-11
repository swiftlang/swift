// RUN: %swift -emit-silgen %s | FileCheck %s

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

func use_subscript_rvalue_get(i : Int) -> Int {
  return subscriptableGet[i]
}

// CHECK-LABEL: sil @{{.*}}use_subscript_rvalue_get
// CHECK-NEXT: bb0(%0 : $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr #subscriptableGet : $*SubscriptableGet
// CHECK: [[PROJ:%[0-9]+]] = project_existential [[GLOB]] : $*SubscriptableGet to $*@sil_self SubscriptableGet
// CHECK-NEXT: [[METH:%[0-9]+]] = protocol_method [[GLOB]] : $*SubscriptableGet, #SubscriptableGet.subscript!getter.1
// CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[METH]](%0, [[PROJ]])
// CHECK-NEXT: return [[RESULT]]

func use_subscript_lvalue_get(i : Int) -> Int {
  return subscriptableGetSet[i]
}

// CHECK-LABEL: sil @{{.*}}use_subscript_lvalue_get
// CHECK-NEXT: bb0(%0 : $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr #subscriptableGetSet : $*SubscriptableGet
// CHECK: [[PROJ:%[0-9]+]] = project_existential [[GLOB]] : $*SubscriptableGetSet to $*@sil_self SubscriptableGetSet
// CHECK-NEXT: [[METH:%[0-9]+]] = protocol_method [[GLOB]] : $*SubscriptableGetSet, #SubscriptableGetSet.subscript!getter.1
// CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[METH]](%0, [[PROJ]])
// CHECK-NEXT: return [[RESULT]]

func use_subscript_lvalue_set(i : Int) {
  subscriptableGetSet[i] = i
}

// CHECK-LABEL: sil @{{.*}}use_subscript_lvalue_set
// CHECK-NEXT: bb0(%0 : $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr #subscriptableGetSet : $*SubscriptableGet
// CHECK: [[PROJ:%[0-9]+]] = project_existential [[GLOB]] : $*SubscriptableGetSet to $*@sil_self SubscriptableGetSet
// CHECK-NEXT: [[METH:%[0-9]+]] = protocol_method [[GLOB]] : $*SubscriptableGetSet, #SubscriptableGetSet.subscript!setter.1
// CHECK-NEXT: apply [[METH]](%0, %0, [[PROJ]])


//===----------------------------------------------------------------------===//
// Calling Archetype Subscripts
//===----------------------------------------------------------------------===//

func use_subscript_archetype_rvalue_get<T : SubscriptableGet>(generic : T, idx : Int) -> Int {
  return generic[idx]
}
// CHECK-LABEL: sil @{{.*}}use_subscript_archetype_rvalue_get
// CHECK-NEXT: bb0(%0 : $*T, %1 : $Int):
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #SubscriptableGet.subscript!getter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, %0)
// CHECK-NEXT: destroy_addr %0


func use_subscript_archetype_lvalue_get<T : SubscriptableGetSet>(inout generic : T, idx : Int) -> Int {
  return generic[idx]
}
// CHECK-LABEL: sil @{{.*}}use_subscript_archetype_lvalue_get
// CHECK-NEXT: bb0(%0 : $*T, %1 : $Int):
// CHECK: [[INOUTBOX:%[0-9]+]] = alloc_box $T
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #SubscriptableGetSet.subscript!getter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, [[INOUTBOX]]#1)
// CHECK: strong_release [[INOUTBOX]]#0


func use_subscript_archetype_lvalue_set<T : SubscriptableGetSet>(inout generic : T, idx : Int) {
  generic[idx] = idx
}
// CHECK-LABEL: sil @{{.*}}use_subscript_archetype_lvalue_set
// CHECK-NEXT: bb0(%0 : $*T, %1 : $Int):
// CHECK: [[INOUTBOX:%[0-9]+]] = alloc_box $T
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #SubscriptableGetSet.subscript!setter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, %1, [[INOUTBOX]]#1)
// CHECK: strong_release [[INOUTBOX]]#0


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
// CHECK-LABEL: sil @{{.*}}use_property_rvalue_get
// CHECK: [[GLOB:%[0-9]+]] = global_addr #propertyGet : $*PropertyWithGetter
// CHECK: [[PROJ:%[0-9]+]] = project_existential [[GLOB]] : $*PropertyWithGetter to $*@sil_self PropertyWithGetter
// CHECK-NEXT: [[METH:%[0-9]+]] = protocol_method [[GLOB]] : $*PropertyWithGetter, #PropertyWithGetter.a!getter.1
// CHECK-NEXT: apply [[METH]]([[PROJ]])

func use_property_lvalue_get() -> Int {
  return propertyGetSet.b
}
// CHECK-LABEL: sil @{{.*}}use_property_lvalue_get
// CHECK: [[GLOB:%[0-9]+]] = global_addr #propertyGetSet : $*PropertyWithGetterSetter
// CHECK: [[PROJ:%[0-9]+]] = project_existential [[GLOB]] : $*PropertyWithGetterSetter to $*@sil_self PropertyWithGetterSetter
// CHECK-NEXT: [[METH:%[0-9]+]] = protocol_method [[GLOB]] : $*PropertyWithGetterSetter, #PropertyWithGetterSetter.b!getter.1
// CHECK-NEXT: apply [[METH]]([[PROJ]])

func use_property_lvalue_set(x : Int) {
  propertyGetSet.b = x
}

// CHECK-LABEL: sil @{{.*}}use_property_lvalue_set
// CHECK-NEXT: bb0(%0 : $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr #propertyGetSet : $*PropertyWithGetterSetter
// CHECK: [[PROJ:%[0-9]+]] = project_existential [[GLOB]] : $*PropertyWithGetterSetter to $*@sil_self PropertyWithGetterSetter
// CHECK-NEXT: [[METH:%[0-9]+]] = protocol_method [[GLOB]] : $*PropertyWithGetterSetter, #PropertyWithGetterSetter.b!setter.1
// CHECK-NEXT: apply [[METH]](%0, [[PROJ]])

//===----------------------------------------------------------------------===//
// Calling Archetype Properties
//===----------------------------------------------------------------------===//

func use_property_archetype_rvalue_get<T : PropertyWithGetter>(generic : T) -> Int {
  return generic.a
}

// CHECK-LABEL: sil @{{.*}}use_property_archetype_rvalue_get
// CHECK-NEXT: bb0(%0 : $*T):
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #PropertyWithGetter.a!getter.1
// CHECK-NEXT: apply [[METH]]<T>(%0)
// CHECK-NEXT: destroy_addr %0


func use_property_archetype_lvalue_get<T : PropertyWithGetterSetter>(generic : T) -> Int {
  return generic.b
}

// CHECK-LABEL: sil @{{.*}}use_property_archetype_lvalue_get
// CHECK-NEXT: bb0(%0 : $*T):
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #PropertyWithGetterSetter.b!getter.1
// CHECK-NEXT: apply [[METH]]<T>(%0)
// CHECK-NEXT: destroy_addr %0


func use_property_archetype_lvalue_set<T : PropertyWithGetterSetter>(inout generic : T, v : Int) {
  generic.b = v
}
// CHECK-LABEL: sil @{{.*}}use_property_archetype_lvalue_set
// CHECK-NEXT: bb0(%0 : $*T, %1 : $Int):
// CHECK: [[INOUTBOX:%[0-9]+]] = alloc_box $T
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #PropertyWithGetterSetter.b!setter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, [[INOUTBOX]]#1)
// CHECK: strong_release [[INOUTBOX]]#0

//===----------------------------------------------------------------------===//
// Calling Initializers
//===----------------------------------------------------------------------===//
protocol Initializable {
  init(int: Int)
}

// CHECK-LABEL: sil @_TF9protocols27use_initializable_archetype
func use_initializable_archetype<T: Initializable>(t: T, i: Int) {
  // CHECK:   [[T_META:%[0-9]+]] = metatype $@thick T.Type
  // CHECK:   [[T_INIT:%[0-9]+]] = witness_method $T, #Initializable.init!allocator.1 : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : Initializable> (@out τ_0_0, Int, @thick τ_0_0.Type) -> ()
  // CHECK:   [[T_RESULT:%[0-9]+]] = alloc_stack $T
  // CHECK:   [[T_RESULT_ADDR:%[0-9]+]] = apply [[T_INIT]]<T>([[T_RESULT]]#1, %1, [[T_META]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : Initializable> (@out τ_0_0, Int, @thick τ_0_0.Type) -> ()
  // CHECK:   destroy_addr [[T_RESULT]]#1 : $*T
  // CHECK:   dealloc_stack [[T_RESULT]]#0 : $*@local_storage T
  // CHECK:   destroy_addr [[VAR_0:%[0-9]+]] : $*T
  // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
  T(int: i)
}

// CHECK: sil @_TF9protocols29use_initializable_existential
func use_initializable_existential(im: Initializable.Type, i: Int) {
// CHECK: bb0([[IM:%[0-9]+]] : $@thick Initializable.Type, [[I:%[0-9]+]] : $Int):
// CHECK:   [[ARCHETYPE_META:%[0-9]+]] = open_existential_ref [[IM]] : $@thick Initializable.Type to $@thick @opened(0) Initializable.Type
// CHECK:   [[TEMP_VALUE:%[0-9]+]] = alloc_stack $Initializable
// CHECK:   [[TEMP_ADDR:%[0-9]+]] = init_existential [[TEMP_VALUE]]#1 : $*Initializable, $*@opened(0) Initializable
// CHECK:   [[INIT_WITNESS:%[0-9]+]] = witness_method $@opened(0) Initializable, #Initializable.init!allocator.1 : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : Initializable> (@out τ_0_0, Int, @thick τ_0_0.Type) -> ()
// CHECK:   [[INIT_RESULT:%[0-9]+]] = apply [[INIT_WITNESS]]<@opened(0) Initializable>([[TEMP_ADDR]], [[I]], [[ARCHETYPE_META]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : Initializable> (@out τ_0_0, Int, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr [[TEMP_VALUE]]#1 : $*Initializable
// CHECK:   dealloc_stack [[TEMP_VALUE]]#0 : $*@local_storage Initializable
  im(int: i)
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
// CHECK-LABEL: sil @_TTWC9protocols15ClassWithGetterS_18PropertyWithGetterFS1_g1aSi : $@cc(witness_method) @thin (@inout ClassWithGetter) -> Int {
// CHECK: bb0
// CHECK-NEXT: load
// CHECK-NEXT: strong_retain
// CHECK-NEXT: class_method
// CHECK-NEXT: apply
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

// CHECK-LABEL: sil @_TTWC9protocols21ClassWithGetterSetterS_24PropertyWithGetterSetterFS1_g1bSi : $@cc(witness_method) @thin (@inout ClassWithGetterSetter) -> Int {
// CHECK: bb0
// CHECK-NEXT: load
// CHECK-NEXT: strong_retain
// CHECK-NEXT: class_method
// CHECK-NEXT: apply
// CHECK-NEXT: return

// Stored variables fulfilling property requirements
//
class ClassWithStoredProperty : PropertyWithGetter {
  var a : Int = 0

  // Make sure that accesses go through the generated accessors for classes.
  func methodUsingProperty() -> Int {
    return a
  }
  // CHECK-LABEL: sil @{{.*}}ClassWithStoredProperty{{.*}}methodUsingProperty
  // CHECK-NEXT: bb0(%0 : $ClassWithStoredProperty):
  // CHECK-NEXT: debug_value %0
  // CHECK-NEXT: strong_retain %0 : $ClassWithStoredProperty
  // CHECK-NEXT: %3 = class_method %0 : $ClassWithStoredProperty, #ClassWithStoredProperty.a!getter.1
  // CHECK-NEXT: %4 = apply %3(%0)
  // CHECK-NEXT: strong_release %0 : $ClassWithStoredProperty
  // CHECK-NEXT: return %4 : $Int
}

struct StructWithStoredProperty : PropertyWithGetter {
  var a : Int

  // Make sure that accesses aren't going through the generated accessors.
  func methodUsingProperty() -> Int {
    return a
  }
  // CHECK-LABEL: sil @{{.*}}StructWithStoredProperty{{.*}}methodUsingProperty
  // CHECK-NEXT: bb0(%0 : $StructWithStoredProperty):
  // CHECK-NEXT: debug_value %0
  // CHECK-NEXT: %2 = struct_extract %0 : $StructWithStoredProperty, #StructWithStoredProperty.a
  // CHECK-NEXT: return %2 : $Int
}

// Make sure that we generate direct function calls for out struct protocl
// witness since structs don't do virtual calls for methods.
// CHECK-LABEL: sil @_TTWV9protocols24StructWithStoredPropertyS_18PropertyWithGetterFS1_g1aSi : $@cc(witness_method) @thin (@inout StructWithStoredProperty) -> Int {
// CHECK: bb0
// CHECK-NEXT: load
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @_TFV9protocols24StructWithStoredPropertyg1aSi : $@cc(method) @thin (StructWithStoredProperty) -> Int
// CHECK-NEXT: apply %2(%1) : $@cc(method) @thin (StructWithStoredProperty) -> Int
// CHECK-NEXT: return

// CHECK-LABEL: sil_witness_table ClassWithGetter: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: @_TTWC9protocols15ClassWithGetterS_18PropertyWithGetterFS1_g1aSi
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table ClassWithGetterSetter: PropertyWithGetterSetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!getter.1: @_TTWC9protocols21ClassWithGetterSetterS_24PropertyWithGetterSetterFS1_g1bSi
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!setter.1: @_TTWC9protocols21ClassWithGetterSetterS_24PropertyWithGetterSetterFS1_s1bSi
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table ClassWithGetterSetter: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: @_TTWC9protocols21ClassWithGetterSetterS_18PropertyWithGetterFS1_g1aSi
// CHECK-NEXT: }


