
// RUN: %target-swift-frontend -module-name protocols -emit-silgen -enable-sil-ownership %s | %FileCheck %s

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
// CHECK: bb0(%0 : @trivial $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr @$S9protocols16subscriptableGetAA013SubscriptableC0_pvp : $*SubscriptableGet
// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[GLOB]] : $*SubscriptableGet
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr immutable_access [[READ]] : $*SubscriptableGet to $*[[OPENED:@opened(.*) SubscriptableGet]]
// CHECK: [[ALLOCSTACK:%[0-9]+]] = alloc_stack $[[OPENED]]
// CHECK: copy_addr [[PROJ]] to [initialization] [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #SubscriptableGet.subscript!getter.1
// CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[METH]]<[[OPENED]]>(%0, [[ALLOCSTACK]])
// CHECK-NEXT: destroy_addr [[ALLOCSTACK]]
// CHECK-NEXT: end_access [[READ]] : $*SubscriptableGet
// CHECK-NEXT: dealloc_stack [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: return [[RESULT]]

func use_subscript_lvalue_get(_ i : Int) -> Int {
  return subscriptableGetSet[i]
}

// CHECK-LABEL: sil hidden @{{.*}}use_subscript_lvalue_get
// CHECK: bb0(%0 : @trivial $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr @$S9protocols19subscriptableGetSetAA013SubscriptablecD0_pvp : $*SubscriptableGetSet
// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[GLOB]] : $*SubscriptableGetSet
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr immutable_access [[READ]] : $*SubscriptableGetSet to $*[[OPENED:@opened(.*) SubscriptableGetSet]]
// CHECK: [[ALLOCSTACK:%[0-9]+]] = alloc_stack $[[OPENED]]
// CHECK: copy_addr [[PROJ]] to [initialization] [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #SubscriptableGetSet.subscript!getter.1
// CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[METH]]<[[OPENED]]>(%0, [[ALLOCSTACK]])
// CHECK-NEXT: destroy_addr [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: end_access [[READ]] : $*SubscriptableGetSet
// CHECK-NEXT: dealloc_stack [[ALLOCSTACK]] : $*[[OPENED]]
// CHECK-NEXT: return [[RESULT]]

func use_subscript_lvalue_set(_ i : Int) {
  subscriptableGetSet[i] = i
}

// CHECK-LABEL: sil hidden @{{.*}}use_subscript_lvalue_set
// CHECK: bb0(%0 : @trivial $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr @$S9protocols19subscriptableGetSetAA013SubscriptablecD0_pvp : $*SubscriptableGetSet
// CHECK: [[READ:%.*]] = begin_access [modify] [dynamic] [[GLOB]] : $*SubscriptableGetSet
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr mutable_access [[READ]] : $*SubscriptableGetSet to $*[[OPENED:@opened(.*) SubscriptableGetSet]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #SubscriptableGetSet.subscript!setter.1
// CHECK-NEXT: apply [[METH]]<[[OPENED]]>(%0, %0, [[PROJ]])


//===----------------------------------------------------------------------===//
// Calling Archetype Subscripts
//===----------------------------------------------------------------------===//

func use_subscript_archetype_rvalue_get<T : SubscriptableGet>(_ generic : T, idx : Int) -> Int {
  return generic[idx]
}
// CHECK-LABEL: sil hidden @{{.*}}use_subscript_archetype_rvalue_get
// CHECK: bb0(%0 : @trivial $*T, %1 : @trivial $Int):
// CHECK: [[STACK:%[0-9]+]] = alloc_stack $T
// CHECK: copy_addr %0 to [initialization] [[STACK]]
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #SubscriptableGet.subscript!getter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, [[STACK]])
// CHECK-NEXT: destroy_addr [[STACK]] : $*T
// CHECK-NEXT: dealloc_stack [[STACK]] : $*T
// CHECK: } // end sil function '${{.*}}use_subscript_archetype_rvalue_get


func use_subscript_archetype_lvalue_get<T : SubscriptableGetSet>(_ generic: inout T, idx : Int) -> Int {
  return generic[idx]
}
// CHECK-LABEL: sil hidden @{{.*}}use_subscript_archetype_lvalue_get
// CHECK: bb0(%0 : @trivial $*T, %1 : @trivial $Int):
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] %0 : $*T
// CHECK: [[GUARANTEEDSTACK:%[0-9]+]] = alloc_stack $T
// CHECK: copy_addr [[READ]] to [initialization] [[GUARANTEEDSTACK]] : $*T
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #SubscriptableGetSet.subscript!getter.1
// CHECK-NEXT: [[APPLYRESULT:%[0-9]+]] = apply [[METH]]<T>(%1, [[GUARANTEEDSTACK]])
// CHECK-NEXT: destroy_addr [[GUARANTEEDSTACK]] : $*T
// CHECK-NEXT: end_access [[READ]]
// CHECK-NEXT: dealloc_stack [[GUARANTEEDSTACK]] : $*T
// CHECK: return [[APPLYRESULT]]


func use_subscript_archetype_lvalue_set<T : SubscriptableGetSet>(_ generic: inout T, idx : Int) {
  generic[idx] = idx
}
// CHECK-LABEL: sil hidden @{{.*}}use_subscript_archetype_lvalue_set
// CHECK: bb0(%0 : @trivial $*T, %1 : @trivial $Int):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*T
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #SubscriptableGetSet.subscript!setter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, %1, [[WRITE]])


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
// CHECK: [[GLOB:%[0-9]+]] = global_addr @$S9protocols11propertyGetAA18PropertyWithGetter_pvp : $*PropertyWithGetter
// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[GLOB]] : $*PropertyWithGetter
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr immutable_access [[READ]] : $*PropertyWithGetter to $*[[OPENED:@opened(.*) PropertyWithGetter]]
// CHECK: [[COPY:%.*]] = alloc_stack $[[OPENED]]
// CHECK-NEXT: copy_addr [[PROJ]] to [initialization] [[COPY]] : $*[[OPENED]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #PropertyWithGetter.a!getter.1
// CHECK-NEXT: apply [[METH]]<[[OPENED]]>([[COPY]])
// CHECK: end_access [[READ]] : $*PropertyWithGetter

func use_property_lvalue_get() -> Int {
  return propertyGetSet.b
}
// CHECK-LABEL: sil hidden @{{.*}}use_property_lvalue_get
// CHECK: [[GLOB:%[0-9]+]] = global_addr @$S9protocols14propertyGetSetAA24PropertyWithGetterSetter_pvp : $*PropertyWithGetterSetter
// CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[GLOB]] : $*PropertyWithGetterSetter
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr immutable_access [[READ]] : $*PropertyWithGetterSetter to $*[[OPENED:@opened(.*) PropertyWithGetterSetter]]
// CHECK: [[STACK:%[0-9]+]] = alloc_stack $[[OPENED]]
// CHECK: copy_addr [[PROJ]] to [initialization] [[STACK]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #PropertyWithGetterSetter.b!getter.1
// CHECK-NEXT: apply [[METH]]<[[OPENED]]>([[STACK]])

func use_property_lvalue_set(_ x : Int) {
  propertyGetSet.b = x
}

// CHECK-LABEL: sil hidden @{{.*}}use_property_lvalue_set
// CHECK: bb0(%0 : @trivial $Int):
// CHECK: [[GLOB:%[0-9]+]] = global_addr @$S9protocols14propertyGetSetAA24PropertyWithGetterSetter_pvp : $*PropertyWithGetterSetter
// CHECK: [[READ:%.*]] = begin_access [modify] [dynamic] [[GLOB]] : $*PropertyWithGetterSetter
// CHECK: [[PROJ:%[0-9]+]] = open_existential_addr mutable_access [[READ]] : $*PropertyWithGetterSetter to $*[[OPENED:@opened(.*) PropertyWithGetterSetter]]
// CHECK-NEXT: [[METH:%[0-9]+]] = witness_method $[[OPENED]], #PropertyWithGetterSetter.b!setter.1
// CHECK-NEXT: apply [[METH]]<[[OPENED]]>(%0, [[PROJ]])

//===----------------------------------------------------------------------===//
// Calling Archetype Properties
//===----------------------------------------------------------------------===//

func use_property_archetype_rvalue_get<T : PropertyWithGetter>(_ generic : T) -> Int {
  return generic.a
}

// CHECK-LABEL: sil hidden @{{.*}}use_property_archetype_rvalue_get
// CHECK: bb0(%0 : @trivial $*T):
// CHECK: [[STACK:%[0-9]+]] = alloc_stack $T
// CHECK: copy_addr %0 to [initialization] [[STACK]]
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #PropertyWithGetter.a!getter.1
// CHECK-NEXT: apply [[METH]]<T>([[STACK]])
// CHECK-NEXT: destroy_addr [[STACK]]
// CHECK-NEXT: dealloc_stack [[STACK]]
// CHECK: } // end sil function '{{.*}}use_property_archetype_rvalue_get


func use_property_archetype_lvalue_get<T : PropertyWithGetterSetter>(_ generic : T) -> Int {
  return generic.b
}

// CHECK-LABEL: sil hidden @{{.*}}use_property_archetype_lvalue_get
// CHECK: bb0(%0 : @trivial $*T):
// CHECK: [[STACK:%[0-9]+]] = alloc_stack $T
// CHECK: copy_addr %0 to [initialization] [[STACK]] : $*T
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #PropertyWithGetterSetter.b!getter.1
// CHECK-NEXT: apply [[METH]]<T>([[STACK]])
// CHECK-NEXT: destroy_addr [[STACK]] : $*T
// CHECK-NEXT: dealloc_stack [[STACK]] : $*T
// CHECK: } // end sil function '${{.*}}use_property_archetype_lvalue_get


func use_property_archetype_lvalue_set<T : PropertyWithGetterSetter>(_ generic: inout T, v : Int) {
  generic.b = v
}
// CHECK-LABEL: sil hidden @{{.*}}use_property_archetype_lvalue_set
// CHECK: bb0(%0 : @trivial $*T, %1 : @trivial $Int):
// CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*T
// CHECK: [[METH:%[0-9]+]] = witness_method $T, #PropertyWithGetterSetter.b!setter.1
// CHECK-NEXT: apply [[METH]]<T>(%1, [[WRITE]])

//===----------------------------------------------------------------------===//
// Calling Initializers
//===----------------------------------------------------------------------===//
protocol Initializable {
  init(int: Int)
}

// CHECK-LABEL: sil hidden @$S9protocols27use_initializable_archetype{{[_0-9a-zA-Z]*}}F
func use_initializable_archetype<T: Initializable>(_ t: T, i: Int) {
  // CHECK:   [[T_RESULT:%[0-9]+]] = alloc_stack $T
  // CHECK:   [[T_META:%[0-9]+]] = metatype $@thick T.Type
  // CHECK:   [[T_INIT:%[0-9]+]] = witness_method $T, #Initializable.init!allocator.1 : {{.*}} : $@convention(witness_method: Initializable) <τ_0_0 where τ_0_0 : Initializable> (Int, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK:   [[T_RESULT_ADDR:%[0-9]+]] = apply [[T_INIT]]<T>([[T_RESULT]], %1, [[T_META]]) : $@convention(witness_method: Initializable) <τ_0_0 where τ_0_0 : Initializable> (Int, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK:   destroy_addr [[T_RESULT]] : $*T
  // CHECK:   dealloc_stack [[T_RESULT]] : $*T
  // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
  T(int: i)
}

// CHECK: sil hidden @$S9protocols29use_initializable_existential{{[_0-9a-zA-Z]*}}F
func use_initializable_existential(_ im: Initializable.Type, i: Int) {
// CHECK: bb0([[IM:%[0-9]+]] : @trivial $@thick Initializable.Type, [[I:%[0-9]+]] : @trivial $Int):
// CHECK:   [[ARCHETYPE_META:%[0-9]+]] = open_existential_metatype [[IM]] : $@thick Initializable.Type to $@thick (@opened([[N:".*"]]) Initializable).Type
// CHECK:   [[TEMP_VALUE:%[0-9]+]] = alloc_stack $Initializable
// CHECK:   [[TEMP_ADDR:%[0-9]+]] = init_existential_addr [[TEMP_VALUE]] : $*Initializable, $@opened([[N]]) Initializable
// CHECK:   [[INIT_WITNESS:%[0-9]+]] = witness_method $@opened([[N]]) Initializable, #Initializable.init!allocator.1 : {{.*}}, [[ARCHETYPE_META]]{{.*}} : $@convention(witness_method: Initializable) <τ_0_0 where τ_0_0 : Initializable> (Int, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[INIT_RESULT:%[0-9]+]] = apply [[INIT_WITNESS]]<@opened([[N]]) Initializable>([[TEMP_ADDR]], [[I]], [[ARCHETYPE_META]]) : $@convention(witness_method: Initializable) <τ_0_0 where τ_0_0 : Initializable> (Int, @thick τ_0_0.Type) -> @out τ_0_0
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
// CHECK-LABEL: sil private [transparent] [thunk] @$S9protocols15ClassWithGetterCAA08PropertycD0A2aDP1aSivgTW : $@convention(witness_method: PropertyWithGetter) (@in_guaranteed ClassWithGetter) -> Int {
// CHECK: bb0([[C:%.*]] : @trivial $*ClassWithGetter):
// CHECK-NEXT: [[CCOPY_LOADED:%.*]] = load_borrow %0
// CHECK-NEXT: [[FUN:%.*]] = class_method [[CCOPY_LOADED]] : $ClassWithGetter, #ClassWithGetter.a!getter.1 : (ClassWithGetter) -> () -> Int, $@convention(method) (@guaranteed ClassWithGetter) -> Int
// CHECK-NEXT: apply [[FUN]]([[CCOPY_LOADED]])
// CHECK-NEXT: end_borrow [[CCOPY_LOADED]] from %0
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

// CHECK-LABEL: sil private [transparent] [thunk] @$S9protocols21ClassWithGetterSetterCAA08PropertycdE0A2aDP1bSivgTW : $@convention(witness_method: PropertyWithGetterSetter) (@in_guaranteed ClassWithGetterSetter) -> Int {
// CHECK: bb0([[C:%.*]] : @trivial $*ClassWithGetterSetter):
// CHECK-NEXT: [[CCOPY_LOADED:%.*]] = load_borrow %0
// CHECK-NEXT: [[FUN:%.*]] = class_method [[CCOPY_LOADED]] : $ClassWithGetterSetter, #ClassWithGetterSetter.b!getter.1 : (ClassWithGetterSetter) -> () -> Int, $@convention(method) (@guaranteed ClassWithGetterSetter) -> Int
// CHECK-NEXT: apply [[FUN]]([[CCOPY_LOADED]])
// CHECK-NEXT: end_borrow [[CCOPY_LOADED]] from %0
// CHECK-NEXT: return

// Stored variables fulfilling property requirements
//
class ClassWithStoredProperty : PropertyWithGetter {
  var a : Int = 0

  // Make sure that accesses go through the generated accessors for classes.
  func methodUsingProperty() -> Int {
    return a
  }
  // CHECK-LABEL: sil hidden @$S9protocols23ClassWithStoredPropertyC011methodUsingE0SiyF
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $ClassWithStoredProperty):
  // CHECK-NEXT: debug_value [[ARG]]
  // CHECK-NOT: copy_value
  // CHECK-NEXT: [[FUN:%.*]] = class_method [[ARG]] : $ClassWithStoredProperty, #ClassWithStoredProperty.a!getter.1 : (ClassWithStoredProperty) -> () -> Int, $@convention(method) (@guaranteed ClassWithStoredProperty) -> Int
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[FUN]]([[ARG]])
  // CHECK-NOT: destroy_value
  // CHECK-NEXT: return [[RESULT]] : $Int
}

struct StructWithStoredProperty : PropertyWithGetter {
  var a : Int

  // Make sure that accesses aren't going through the generated accessors.
  func methodUsingProperty() -> Int {
    return a
  }
  // CHECK-LABEL: sil hidden @$S9protocols24StructWithStoredPropertyV011methodUsingE0SiyF
  // CHECK: bb0(%0 : @trivial $StructWithStoredProperty):
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
// CHECK-LABEL: sil private [transparent] [thunk] @$S9protocols24StructWithStoredPropertyVAA0eC6GetterA2aDP1aSivgTW : $@convention(witness_method: PropertyWithGetter) (@in_guaranteed StructWithStoredProperty) -> Int {
// CHECK: bb0([[C:%.*]] : @trivial $*StructWithStoredProperty):
// CHECK-NEXT: [[CCOPY_LOADED:%.*]] = load [trivial] [[C]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[FUN:%.*]] = function_ref @$S9protocols24StructWithStoredPropertyV1aSivg : $@convention(method) (StructWithStoredProperty) -> Int
// CHECK-NEXT: apply [[FUN]]([[CCOPY_LOADED]])
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
  // CHECK-LABEL: sil hidden @$S9protocols29StructWithStoredClassPropertyV011methodUsingF0SiyF
  // CHECK: bb0(%0 : @guaranteed $StructWithStoredClassProperty):
  // CHECK-NEXT: debug_value %0
  // CHECK-NEXT: %2 = struct_extract %0 : $StructWithStoredClassProperty, #StructWithStoredClassProperty.a
  // CHECK-NEXT: return %2 : $Int
}

// CHECK-LABEL: sil private [transparent] [thunk] @$S9protocols29StructWithStoredClassPropertyVAA0fC6GetterA2aDP1aSivgTW : $@convention(witness_method: PropertyWithGetter) (@in_guaranteed StructWithStoredClassProperty) -> Int {
// CHECK: bb0([[C:%.*]] : @trivial $*StructWithStoredClassProperty):
// CHECK-NEXT: [[CCOPY_LOADED:%.*]] = load_borrow [[C]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[FUN:%.*]] = function_ref @$S9protocols29StructWithStoredClassPropertyV1aSivg : $@convention(method) (@guaranteed StructWithStoredClassProperty) -> Int
// CHECK-NEXT: apply [[FUN]]([[CCOPY_LOADED]])
// CHECK-NEXT: end_borrow [[CCOPY_LOADED]] from %0
// CHECK-NEXT: return

// rdar://22676810

protocol ExistentialProperty {
  var p: PropertyWithGetterSetter { get set }
}

func testExistentialPropertyRead<T: ExistentialProperty>(_ t: inout T) {
    let b = t.p.b
}
// CHECK-LABEL: sil hidden @$S9protocols27testExistentialPropertyRead{{[_0-9a-zA-Z]*}}F
// CHECK:      [[READ:%.*]] = begin_access [read] [unknown] %0 : $*T
// CHECK:      [[P_TEMP:%.*]] = alloc_stack $PropertyWithGetterSetter
// CHECK:      [[T_TEMP:%.*]] = alloc_stack $T
// CHECK:      copy_addr [[READ]] to [initialization] [[T_TEMP]] : $*T
// CHECK:      [[P_GETTER:%.*]] = witness_method $T, #ExistentialProperty.p!getter.1 :
// CHECK-NEXT: apply [[P_GETTER]]<T>([[P_TEMP]], [[T_TEMP]])
// CHECK-NEXT: destroy_addr [[T_TEMP]]
// CHECK-NEXT: [[OPEN:%.*]] = open_existential_addr immutable_access [[P_TEMP]] : $*PropertyWithGetterSetter to $*[[P_OPENED:@opened(.*) PropertyWithGetterSetter]]
// CHECK-NEXT: [[T0:%.*]] = alloc_stack $[[P_OPENED]]
// CHECK-NEXT: copy_addr [[OPEN]] to [initialization] [[T0]]
// CHECK-NEXT: [[B_GETTER:%.*]] = witness_method $[[P_OPENED]], #PropertyWithGetterSetter.b!getter.1
// CHECK-NEXT: apply [[B_GETTER]]<[[P_OPENED]]>([[T0]])
// CHECK-NEXT: debug_value
// CHECK-NEXT: destroy_addr [[T0]]
// CHECK-NOT:  witness_method
// CHECK:      return

func modify(_ x: inout Int) {}

// Make sure we call the materializeForSet callback with the correct
// generic signature.

func modifyProperty<T : PropertyWithGetterSetter>(_ x: inout T) {
  modify(&x.b)
}
// CHECK-LABEL: sil hidden @$S9protocols14modifyPropertyyyxzAA0C16WithGetterSetterRzlF
// CHECK:      [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*T
// CHECK:      [[WITNESS_FN:%.*]] = witness_method $T, #PropertyWithGetterSetter.b!materializeForSet.1
// CHECK:      [[RESULT:%.*]] = apply [[WITNESS_FN]]<T>
// CHECK:      [[TEMPORARY:%.*]] = tuple_extract [[RESULT]]
// CHECK:      [[CALLBACK:%.*]] = tuple_extract [[RESULT]]
// CHECK:      [[TEMPORARY_ADDR_TMP:%.*]] = pointer_to_address [[TEMPORARY]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:      [[TEMPORARY_ADDR:%.*]] = mark_dependence [[TEMPORARY_ADDR_TMP]] : $*Int on [[WRITE]] : $*T
// CHECK:      [[MODIFY_FN:%.*]] = function_ref @$S9protocols6modifyyySizF
// CHECK:      apply [[MODIFY_FN]]([[TEMPORARY_ADDR]])
// CHECK:      switch_enum [[CALLBACK]] : $Optional<Builtin.RawPointer>, case #Optional.some!enumelt.1: bb1, case #Optional.none!enumelt: bb2
// CHECK:    bb1([[CALLBACK_ADDR:%.*]] : @trivial $Builtin.RawPointer):
// CHECK:      [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]]
// CHECK:      [[METATYPE:%.*]] = metatype $@thick T.Type
// CHECK:      [[TEMPORARY:%.*]] = address_to_pointer [[TEMPORARY_ADDR]] : $*Int to $Builtin.RawPointer
// CHECK:      apply [[CALLBACK]]<T>

public struct Val {
  public var x: Int = 0
}

public protocol Proto {
  var val: Val { get nonmutating set}
}

public func test(_ p: Proto) {
  p.val.x += 1
}

// CHECK-LABEL: sil @$S9protocols4testyyAA5Proto_pF : $@convention(thin) (@in_guaranteed Proto) -> ()
// CHECK: [[OPEN:%.*]] = open_existential_addr immutable_access
// CHECK: [[MAT:%.*]] = witness_method $@opened("{{.*}}") Proto, #Proto.val!materializeForSet
// CHECK: [[BUF:%.*]] = apply [[MAT]]
// CHECK: [[WB:%.*]] = pointer_to_thin_function
// This use looks like it is mutating but really is not. We use to assert in the SIL verifier.
// CHECK: apply [[WB]]{{.*}}({{.*}}[[OPEN]]
// CHECK: return

// CHECK-LABEL: sil_witness_table hidden ClassWithGetter: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: {{.*}} : @$S9protocols15ClassWithGetterCAA08PropertycD0A2aDP1aSivgTW
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden ClassWithGetterSetter: PropertyWithGetterSetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!getter.1: {{.*}} : @$S9protocols21ClassWithGetterSetterCAA08PropertycdE0A2aDP1bSivgTW
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!setter.1: {{.*}} : @$S9protocols21ClassWithGetterSetterCAA08PropertycdE0A2aDP1bSivsTW
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!materializeForSet.1: {{.*}} : @$S9protocols21ClassWithGetterSetterCAA08PropertycdE0A2aDP1bSivmTW
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden ClassWithGetterSetter: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: {{.*}} : @$S9protocols21ClassWithGetterSetterCAA08PropertycD0A2aDP1aSivgTW
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden StructWithStoredProperty: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: {{.*}} : @$S9protocols24StructWithStoredPropertyVAA0eC6GetterA2aDP1aSivgTW
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden StructWithStoredClassProperty: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: {{.*}} : @$S9protocols29StructWithStoredClassPropertyVAA0fC6GetterA2aDP1aSivgTW
// CHECK-NEXT: }

//
// rdar://problem/37031037
//

protocol MethodWithDefaultArgGenerator {}
extension MethodWithDefaultArgGenerator {
  mutating func foo(_ x: Int = 0) {}
}
func invokeMethodWithDefaultArg(x: inout MethodWithDefaultArgGenerator) {
  x.foo()
}
