
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -swift-version 5 -target %target-swift-5.1-abi-triple -Xllvm -sil-full-demangle -parse-as-library -disable-objc-attr-requires-foundation-module -enable-objc-interop %s | %FileCheck %s

// REQUIRES: concurrency

var zero: Int = 0

func use(_: Int) {}

func takeInt(_ a : Int) {}

public struct DidSetWillSetTests {
  // CHECK-LABEL: sil [ossa] @$s9observers010DidSetWillC5TestsV{{[_0-9a-zA-Z]*}}fC
  @MainActor public init(x : Int) {
    // Accesses to didset/willset variables are direct in init methods and dtors.
    a = x
    a = x

    // CHECK: bb0(%0 : $Int, %1 : $@thin DidSetWillSetTests.Type):
    // CHECK:        [[SELF:%.*]] = mark_uninitialized [rootself]
    // CHECK:        [[SELF_LIFETIME:%.*]] = begin_borrow [var_decl] [[SELF]]
    // CHECK:        [[PB_SELF:%.*]] = project_box [[SELF_LIFETIME]]
    // CHECK:        [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB_SELF]]
    // CHECK:        [[P1:%.*]] = struct_element_addr [[WRITE]] : $*DidSetWillSetTests, #DidSetWillSetTests.a
    // CHECK-NEXT:   assign %0 to [[P1]]
    // CHECK:        [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB_SELF]]
    // CHECK:        [[P2:%.*]] = struct_element_addr [[WRITE]] : $*DidSetWillSetTests, #DidSetWillSetTests.a
    // CHECK-NEXT:   assign %0 to [[P2]]

    // In Swift 5 and earlier, the accesses are _not_ direct within a defer that
    // appears in an init/deinit for non-actor types.
    defer { a = x }

    // CHECK-LABEL: sil private [ossa] @$s9observers010DidSetWillC5TestsV1xACSi_tcfc6$deferL_yyF
    // CHECK-NOT:   assign
    // CHECK:       [[SETTER:%.*]] = function_ref @$s9observers010DidSetWillC5TestsV1aSivs
    // CHECK-NEXT:  apply [[SETTER]]
    // CHECK-NOT:   assign
    // CHECK: } // end sil function
  }

  public var a: Int {
    // CHECK-LABEL: sil private [ossa] @$s9observers010DidSetWillC5TestsV1a{{[_0-9a-zA-Z]*}}vw
    willSet(newA) {
      // CHECK: bb0(%0 : $Int, %1 : $*DidSetWillSetTests):
      // CHECK-NEXT: debug_value %0
      // CHECK-NEXT: debug_value %1 : $*DidSetWillSetTests, {{.*}} expr op_deref

      takeInt(a)

      // CHECK: [[READ:%.*]] = begin_access [read] [unknown] %1
      // CHECK-NEXT: [[FIELDPTR:%.*]] = struct_element_addr [[READ]] : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: [[A:%.*]] = load [trivial] [[FIELDPTR]] : $*Int
      // CHECK-NEXT: end_access [[READ]]
      // CHECK: [[TAKEINTFN:%.*]] = function_ref @$s9observers7takeInt{{[_0-9a-zA-Z]*}}F
      // CHECK-NEXT: apply [[TAKEINTFN]]([[A]]) : $@convention(thin) (Int) -> ()

      takeInt(newA)

      // CHECK-NEXT: // function_ref observers.takeInt(Swift.Int) -> ()
      // CHECK-NEXT: [[TAKEINTFN:%.*]] = function_ref @$s9observers7takeInt{{[_0-9a-zA-Z]*}}F
      // CHECK-NEXT: apply [[TAKEINTFN]](%0) : $@convention(thin) (Int) -> ()

      a = zero  // reassign, but don't infinite loop.

      // CHECK-NEXT: // function_ref observers.zero.unsafeMutableAddressor : Swift.Int
      // CHECK-NEXT: [[ZEROFN:%.*]] = function_ref @$s9observers4zero{{[_0-9a-zA-Z]*}}vau
      // CHECK-NEXT: [[ZERORAW:%.*]] = apply [[ZEROFN]]() : $@convention(thin) () -> Builtin.RawPointer
      // CHECK-NEXT: [[ZEROADDR:%.*]] = pointer_to_address [[ZERORAW]] : $Builtin.RawPointer to [strict] $*Int
      // CHECK-NEXT: [[READ:%.*]] = begin_access [read] [dynamic] [[ZEROADDR]] : $*Int
      // CHECK-NEXT: [[ZERO:%.*]] = load [trivial] [[READ]]
      // CHECK-NEXT: end_access [[READ]] : $*Int
      // CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [unknown] %1
      // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr [[WRITE]] : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: assign [[ZERO]] to [[AADDR]]
    }

    // CHECK-LABEL: sil private [ossa] @$s9observers010DidSetWillC5TestsV1a{{[_0-9a-zA-Z]*}}vW
    didSet {
      // CHECK: bb0(%0 : $*DidSetWillSetTests):
      // CHECK-NEXT: debug_value %0 : $*DidSetWillSetTests, {{.*}} expr op_deref

      takeInt(a)

      // CHECK: [[READ:%.*]] = begin_access [read] [unknown] %0
      // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr [[READ]] : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: [[A:%.*]] = load [trivial] [[AADDR]] : $*Int
      // CHECK-NEXT: end_access [[READ]]
      // CHECK-NEXT: // function_ref observers.takeInt(Swift.Int) -> ()
      // CHECK-NEXT: [[TAKEINTFN:%.*]] = function_ref @$s9observers7takeInt{{[_0-9a-zA-Z]*}}F
      // CHECK-NEXT: apply [[TAKEINTFN]]([[A]]) : $@convention(thin) (Int) -> ()

      (self).a = zero  // reassign, but don't infinite loop.

      // CHECK-NEXT: // function_ref observers.zero.unsafeMutableAddressor : Swift.Int
      // CHECK-NEXT: [[ZEROFN:%.*]] = function_ref @$s9observers4zero{{[_0-9a-zA-Z]*}}vau
      // CHECK-NEXT: [[ZERORAW:%.*]] = apply [[ZEROFN]]() : $@convention(thin) () -> Builtin.RawPointer
      // CHECK-NEXT: [[ZEROADDR:%.*]] = pointer_to_address [[ZERORAW]] : $Builtin.RawPointer to [strict] $*Int
      // CHECK-NEXT: [[READ:%.*]] = begin_access [read] [dynamic] [[ZEROADDR]] : $*Int
      // CHECK-NEXT: [[ZERO:%.*]] = load [trivial] [[READ]]
      // CHECK-NEXT: end_access [[READ]] : $*Int
      // CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [unknown] %0
      // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr [[WRITE]] : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: assign [[ZERO]] to [[AADDR]]
    }

    // This is the synthesized getter and setter for the willset/didset variable.

    // CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s9observers010DidSetWillC5TestsV1aSivg
    // CHECK: bb0(%0 : $DidSetWillSetTests):
    // CHECK-NEXT:   debug_value %0
    // CHECK-NEXT:   %2 = struct_extract %0 : $DidSetWillSetTests, #DidSetWillSetTests.a
    // CHECK-NEXT:   return %2 : $Int{{.*}}                      // id: %3


    // CHECK-LABEL: sil [ossa] @$s9observers010DidSetWillC5TestsV1aSivs : $@convention(method) (Int, @inout DidSetWillSetTests) -> () {
    // CHECK: bb0([[NEWVALUE:%.*]] : $Int, %1 : $*DidSetWillSetTests):
    // CHECK-NEXT: debug_value [[NEWVALUE]] : $Int, let, name "value", argno 1
    // CHECK-NEXT: debug_value %1{{.*}} expr op_deref

    // CHECK-NEXT: [[MODIFY_ONE:%.*]] = begin_access [modify] [unknown] %1 : $*DidSetWillSetTests
    // CHECK-NEXT: // function_ref observers.DidSetWillSetTests.a.willset : Swift.Int
    // CHECK-NEXT: [[WILLSETFN:%.*]] = function_ref @$s9observers010DidSetWillC5TestsV1aSivw : $@convention(method) (Int, @inout DidSetWillSetTests) -> ()
    // CHECK-NEXT: [[RESULT:%.*]] = apply [[WILLSETFN]]([[NEWVALUE]], [[MODIFY_ONE]]) : $@convention(method) (Int, @inout DidSetWillSetTests) -> ()
    // CHECK-NEXT: end_access [[MODIFY_ONE]] : $*DidSetWillSetTests

    // CHECK-NEXT: [[MODIFY_TWO:%.*]] = begin_access [modify] [unknown] %1 : $*DidSetWillSetTests
    // CHECK-NEXT: [[ADDR:%.*]] = struct_element_addr [[MODIFY_TWO]] : $*DidSetWillSetTests, #DidSetWillSetTests.a
    // CHECK-NEXT: assign [[NEWVALUE]] to [[ADDR]] : $*Int
    // CHECK-NEXT: end_access [[MODIFY_TWO]] : $*DidSetWillSetTests

    // CHECK-NEXT: [[MODIFY_THREE:%.*]] = begin_access [modify] [unknown] %1 : $*DidSetWillSetTests
    // CHECK-NEXT: // function_ref observers.DidSetWillSetTests.a.didset : Swift.Int
    // CHECK-NEXT: [[DIDSETFN:%.*]] = function_ref @$s9observers010DidSetWillC5TestsV1aSivW : $@convention(method) (@inout DidSetWillSetTests) -> ()
    // CHECK-NEXT: [[RESULT:%.*]] = apply [[DIDSETFN]]([[MODIFY_THREE]]) : $@convention(method) (@inout DidSetWillSetTests) -> ()
    // CHECK-NEXT: end_access [[MODIFY_THREE]] : $*DidSetWillSetTests
    // CHECK-NEXT: [[TUPLE:%.*]] = tuple ()
    // CHECK-NEXT: return [[TUPLE]] : $()
  }

  // CHECK-LABEL: sil hidden [ossa] @$s9observers010DidSetWillC5TestsV8testReadSiyF
  // CHECK:         [[SELF:%.*]] = begin_access [read] [unknown] %0 : $*DidSetWillSetTests
  // CHECK-NEXT:    [[PROP:%.*]] = struct_element_addr [[SELF]] : $*DidSetWillSetTests
  // CHECK-NEXT:    [[LOAD:%.*]] = load [trivial] [[PROP]] : $*Int
  // CHECK-NEXT:    end_access [[SELF]] : $*DidSetWillSetTests
  // CHECK-NEXT:    return [[LOAD]] : $Int
  mutating func testRead() -> Int {
    return a
  }

  // CHECK-LABEL: sil hidden [ossa] @$s9observers010DidSetWillC5TestsV9testWrite5inputySi_tF
  // CHECK:         [[SELF:%.*]] = begin_access [modify] [unknown] %1 : $*DidSetWillSetTests
  // CHECK-NEXT:    // function_ref observers.DidSetWillSetTests.a.setter
  // CHECK-NEXT:    [[SETTER:%.*]] = function_ref @$s9observers010DidSetWillC5TestsV1aSivs
  // CHECK-NEXT:    apply [[SETTER]](%0, [[SELF]])
  // CHECK-NEXT:    end_access [[SELF]] : $*DidSetWillSetTests
  // CHECK-NEXT:    [[RET:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RET]] : $()
  mutating func testWrite(input: Int) {
    a = input
  }

  // CHECK-LABEL: sil hidden [ossa] @$s9observers010DidSetWillC5TestsV13testReadWrite5inputySi_tF
  // CHECK:         [[SELF:%.*]] = begin_access [modify] [unknown] %1 : $*DidSetWillSetTests
  // CHECK-NEXT:    [[TEMP:%.*]] = alloc_stack $Int
  // CHECK-NEXT:    [[PROP:%.*]] = struct_element_addr [[SELF]] : $*DidSetWillSetTests
  // CHECK-NEXT:    [[LOAD:%.*]] = load [trivial] [[PROP]] : $*Int
  // CHECK-NEXT:    store [[LOAD]] to [trivial] [[TEMP]] : $*Int
  // (modification goes here)
  // CHECK:         [[RELOAD:%.*]] = load [trivial] [[TEMP]] : $*Int
  // CHECK-NEXT:    // function_ref observers.DidSetWillSetTests.a.setter
  // CHECK-NEXT:    [[SETTER:%.*]] = function_ref @$s9observers010DidSetWillC5TestsV1aSivs
  // CHECK-NEXT:    apply [[SETTER]]([[RELOAD]], [[SELF]])
  // CHECK-NEXT:    end_access [[SELF]] : $*DidSetWillSetTests
  // CHECK-NEXT:    dealloc_stack [[TEMP]] : $*Int
  // CHECK-NEXT:    [[RET:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RET]] : $()
  mutating func testReadWrite(input: Int) {
    a += input
  }
}

actor Pop {
  var a: Int = 0 {
    didSet {}
  }

  init(input: Int) {
    defer { a = input }
  }
  // In Swift 5, a defer in an actor init or deinit matches its enclosing context,
  // which is direct-to-storage.
  // CHECK-LABEL: sil private [ossa] @$s9observers3PopC5inputACSi_tcfc6$deferL_yyF
  // CHECK-NOT:     apply
  // CHECK:         assign {{%.*}} to {{%.*}} : $*Int
  // CHECK-NOT:     apply
  // CHECK:       } // end sil function

  deinit {
    defer { a = 0 }
  }

  // CHECK-LABEL: sil private [ossa] @$s9observers3PopCfd6$deferL_yyF
  // CHECK:         [[INT:%.*]] = apply {{.*}} : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
  // CHECK-NOT:     apply
  // CHECK:         assign [[INT]] to {{.*}} : $*Int
  // CHECK-NOT:     apply
  // CHECK:       } // end sil function
}

@MainActor
class SaltNVinegar {
  var a: Int = 0 {
    didSet {}
  }

  init(regular val: Int) {
    defer { a = val }

    // In Swift 5, an actor-isolated type has its defers match-up. So, we're
    // direct-to-storage here...
    // CHECK-LABEL: sil private [ossa] @$s9observers12SaltNVinegarC7regularACSi_tcfc6$deferL_yyF
    // CHECK-NOT:     apply
    // CHECK:         assign {{%.*}} to {{%.*}} : $*Int
    // CHECK-NOT:     apply
    // CHECK:       } // end sil function
  }
}

@MainActor(unsafe)
class BBQ {
  var a: Int = 0 {
    didSet {}
  }

  init(regular val: Int) {
    defer { a = val }

    // ... but if the type is using unsafe global-actor isolation, then we preserve
    // the non-direct access in the defer.
    // CHECK-LABEL: sil private [ossa] @$s9observers3BBQC7regularACSi_tcfc6$deferL_yyF
    // CHECK-NOT:     assign
    // CHECK:         [[SETTER:%.*]] = class_method {{%.*}} : $BBQ, #BBQ.a!setter
    // CHECK-NEXT:    apply [[SETTER]]
    // CHECK-NOT:     assign
    // CHECK:       } // end sil function
  }
}

// Test global observing properties.

var global_observing_property : Int = zero {
  // The variable is initialized with "zero".
  // CHECK-LABEL: sil private [global_init_once_fn] [ossa] @{{.*}}WZ : $@convention(c) (Builtin.RawPointer) -> () {
  // CHECK: bb0(%0 : $Builtin.RawPointer):
  // CHECK-NEXT: alloc_global @$s9observers25global_observing_propertySiv
  // CHECK-NEXT: %2 = global_addr @$s9observers25global_observing_propertySivp : $*Int
  // CHECK: observers.zero.unsafeMutableAddressor
  // CHECK: return

  // global_observing_property's setter needs to call didSet.

  // CHECK-LABEL: sil private [ossa] @$s9observers25global_observing_property{{[_0-9a-zA-Z]*}}vW
  didSet {
    // The didSet implementation needs to call takeInt.
    takeInt(global_observing_property)

    // CHECK: function_ref observers.takeInt
    // CHECK-NEXT: function_ref @$s9observers7takeInt{{[_0-9a-zA-Z]*}}F

    // Setting the variable from within its own didSet doesn't recursively call didSet.
    global_observing_property = zero

    // CHECK: // function_ref observers.global_observing_property.unsafeMutableAddressor : Swift.Int
    // CHECK-NEXT: [[ADDRESSOR:%.*]] = function_ref @$s9observers25global_observing_propertySivau : $@convention(thin) () -> Builtin.RawPointer
    // CHECK-NEXT: [[ADDRESS:%.*]] = apply [[ADDRESSOR]]() : $@convention(thin) () -> Builtin.RawPointer
    // CHECK-NEXT: [[POINTER:%.*]] = pointer_to_address [[ADDRESS]] : $Builtin.RawPointer to [strict] $*Int
    // CHECK-NEXT: // function_ref observers.zero.unsafeMutableAddressor : Swift.Int
    // CHECK-NEXT: [[ZEROFN:%.*]] = function_ref @$s9observers4zero{{[_0-9a-zA-Z]*}}vau
    // CHECK-NEXT: [[ZERORAW:%.*]] = apply [[ZEROFN]]() : $@convention(thin) () -> Builtin.RawPointer
    // CHECK-NEXT: [[ZEROADDR:%.*]] = pointer_to_address [[ZERORAW]] : $Builtin.RawPointer to [strict] $*Int
    // CHECK-NEXT: [[READ:%.*]] = begin_access [read] [dynamic] [[ZEROADDR]] : $*Int
    // CHECK-NEXT: [[ZERO:%.*]] = load [trivial] [[READ]]
    // CHECK-NEXT: end_access [[READ]] : $*Int
    // CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[POINTER]] : $*Int
    // CHECK-NEXT: assign [[ZERO]] to [[WRITE]] : $*Int
    // CHECK-NEXT: end_access [[WRITE]] : $*Int
    // CHECK-NOT: function_ref @$s9observers25global_observing_property{{[_0-9a-zA-Z]*}}vW
    // CHECK: end sil function
  }
  // CHECK-LABEL: sil hidden [ossa] @$s9observers25global_observing_property{{[_0-9a-zA-Z]*}}vs
  // CHECK: function_ref observers.global_observing_property.unsafeMutableAddressor
  // CHECK-NEXT:  function_ref @$s9observers25global_observing_property{{[_0-9a-zA-Z]*}}vau
  // CHECK: function_ref observers.global_observing_property.didset
  // CHECK-NEXT: function_ref @$s9observers25global_observing_property{{[_0-9a-zA-Z]*}}vW

}

func force_global_observing_property_setter() {
  let x = global_observing_property
  global_observing_property = x
}

// Test local observing properties.

// CHECK-LABEL: sil hidden [ossa] @$s9observers24local_observing_property{{[_0-9a-zA-Z]*}}SiF
func local_observing_property(_ arg: Int) {
  var localproperty: Int = arg {
    didSet {
      takeInt(localproperty)
      localproperty = zero
    }
  }

  takeInt(localproperty)
  localproperty = arg

  // Alloc and initialize the property to the argument value.
  // CHECK: bb0([[ARG:%[0-9]+]] : $Int)
  // CHECK: [[BOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[L:%.*]] = begin_borrow [var_decl] [[BOX]]
  // CHECK: [[PB:%.*]] = project_box [[L]]
  // CHECK: store [[ARG]] to [trivial] [[PB]]
}

// didSet of localproperty (above)
// Ensure that setting the variable from within its own didSet doesn't recursively call didSet.

// CHECK-LABEL: sil private [ossa] @$s9observers24local_observing_property{{[_0-9a-zA-Z]*}}SiF13localproperty{{[_0-9a-zA-Z]*}}SivW
// CHECK: bb0(%0 : @closureCapture @guaranteed ${ var Int })
// CHECK: [[POINTER:%.*]] = project_box %0 : ${ var Int }, 0
// CHECK: // function_ref observers.zero.unsafeMutableAddressor : Swift.Int
// CHECK-NEXT: [[ZEROFN:%.*]] = function_ref @$s9observers4zero{{[_0-9a-zA-Z]*}}vau
// CHECK-NEXT: [[ZERORAW:%.*]] = apply [[ZEROFN]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK-NEXT: [[ZEROADDR:%.*]] = pointer_to_address [[ZERORAW]] : $Builtin.RawPointer to [strict] $*Int
// CHECK-NEXT: [[READ:%.*]] = begin_access [read] [dynamic] [[ZEROADDR]] : $*Int
// CHECK-NEXT: [[ZERO:%.*]] = load [trivial] [[READ]]
// CHECK-NEXT: end_access [[READ]] : $*Int

// CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [unknown] [[POINTER]] : $*Int
// CHECK-NEXT: assign [[ZERO]] to [[WRITE]] : $*Int
// CHECK-NEXT: end_access [[WRITE]] : $*Int
// CHECK-NOT: function_ref @$s9observers24local_observing_property{{[_0-9a-zA-Z]*}}SiF13localproperty{{[_0-9a-zA-Z]*}}SivW
// CHECK: end sil function

func local_generic_observing_property<T>(_ arg: Int, _: T) {
  var localproperty1: Int = arg {
    didSet {
      takeInt(localproperty1)
    }
  }
  
  takeInt(localproperty1)
  localproperty1 = arg

  var localproperty2: Int = arg {
    didSet {
      _ = T.self
      takeInt(localproperty2)
    }
  }
  
  takeInt(localproperty2)
  localproperty2 = arg
}


// <rdar://problem/16006333> observing properties don't work in @objc classes
@objc
class ObservingPropertyInObjCClass {
  var bounds: Int {
    willSet {}
    didSet {}
  }

  init(b: Int) { bounds = b }
}

// CHECK-LABEL: sil hidden [ossa] @$s9observers32propertyWithDidSetTakingOldValueyyF : $@convention(thin) () -> () {
func propertyWithDidSetTakingOldValue() {
  var p : Int = zero {
    didSet(oldValue) {
      // access to oldValue
      use(oldValue)
      // and newValue.
      use(p)
    }
  }

  p = zero
}

// CHECK-LABEL: sil private [ossa] @$s9observers32propertyWithDidSetTakingOldValueyyF1pL_Sivs
// CHECK: bb0([[ARG1:%.*]] : $Int, [[ARG2:%.*]] : @closureCapture @guaranteed ${ var Int }):
// CHECK-NEXT:  debug_value [[ARG1]] : $Int, let, name "value", argno 1
// CHECK-NEXT:  [[ARG2_PB:%.*]] = project_box [[ARG2]]
// CHECK-NEXT:  debug_value [[ARG2_PB]] : $*Int, var, name "p", argno 2, expr op_deref
// CHECK-NEXT:  [[READ:%.*]] = begin_access [read] [unknown] [[ARG2_PB]]
// CHECK-NEXT:  [[ARG2_PB_VAL:%.*]] = load [trivial] [[READ]] : $*Int
// CHECK-NEXT:  end_access [[READ]]
// CHECK-NEXT:  [[MV_ARG2_PB_VAL:%.*]] = move_value [var_decl] [[ARG2_PB_VAL]] : $Int
// CHECK-NEXT:  [[WRITE:%.*]] = begin_access [modify] [unknown] [[ARG2_PB]]
// CHECK-NEXT:  assign [[ARG1]] to [[WRITE]] : $*Int
// CHECK-NEXT:  end_access [[WRITE]]
// SEMANTIC ARC TODO: Another case where we need to put the mark_function_escape on a new projection after a copy.
// CHECK-NEXT:  mark_function_escape [[ARG2_PB]]
// CHECK-NEXT:  // function_ref
// CHECK-NEXT:  [[FUNC:%.*]] = function_ref @$s9observers32propertyWithDidSetTakingOldValueyyF1pL_SivW : $@convention(thin) (Int, @guaranteed { var Int }) -> ()
// CHECK-NEXT:  %{{.*}} = apply [[FUNC]]([[MV_ARG2_PB_VAL]], [[ARG2]]) : $@convention(thin) (Int, @guaranteed { var Int }) -> ()
// CHECK-NEXT:  extend_lifetime [[MV_ARG2_PB_VAL]] : $Int
// CHECK-NEXT:  %{{.*}} = tuple ()
// CHECK-NEXT:  return %{{.*}} : $()
// CHECK-NEXT:} // end sil function '$s9observers32propertyWithDidSetTakingOldValue{{[_0-9a-zA-Z]*}}'


// <rdar://problem/16406886> Observing properties don't work with ownership types
class Ref {}

struct ObservingPropertiesWithOwnershipTypes {
  unowned var alwaysPresent : Ref {
    didSet {
    }
  }

  init(res: Ref) {
    alwaysPresent = res
  }
}

// CHECK-LABEL: sil private [ossa] @$s9observers37ObservingPropertiesWithOwnershipTypesV13alwaysPresentAA3RefCvW : $@convention(method) (@inout ObservingPropertiesWithOwnershipTypes) -> () {

struct ObservingPropertiesWithOwnershipTypesInferred {
  unowned var alwaysPresent = Ref() {
    didSet {
    }
  }

  weak var maybePresent = nil as Ref? {
    willSet {
    }
  }
}

// CHECK-LABEL: sil private [ossa] @$s9observers45ObservingPropertiesWithOwnershipTypesInferredV13alwaysPresentAA3RefCvW : $@convention(method) (@inout ObservingPropertiesWithOwnershipTypesInferred) -> () {
// CHECK-LABEL: sil private [ossa] @$s9observers45ObservingPropertiesWithOwnershipTypesInferredV12maybePresentAA3RefCSgvw : $@convention(method) (@guaranteed Optional<Ref>, @inout ObservingPropertiesWithOwnershipTypesInferred) -> () {

//<rdar://problem/16620121> Initializing constructor tries to initialize computed property overridden with willSet/didSet
class ObservedBase {
     var printInfo: Ref!
}
class ObservedDerived : ObservedBase {
  override init() {}
  override var printInfo: Ref! {
    didSet { }
  }
}

// CHECK-LABEL: sil private [ossa] @$s9observers15ObservedDerivedC9printInfoAA3RefCSgvW : $@convention(method) (@guaranteed ObservedDerived) -> () {


/// <rdar://problem/26408353> crash when overriding internal property with
/// public property

public class BaseClassWithInternalProperty {
  var x: () = ()
}

public class DerivedClassWithPublicProperty : BaseClassWithInternalProperty {
  public override var x: () {
    didSet {}
  }
}

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s9observers29BaseClassWithInternalPropertyC1xytvg

// CHECK-LABEL: sil [ossa] @$s9observers30DerivedClassWithPublicPropertyC1xytvg
// CHECK:       bb0([[SELF:%.*]] : @guaranteed $DerivedClassWithPublicProperty):
// CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]] : $DerivedClassWithPublicProperty
// CHECK-NEXT:    [[SUPER:%.*]] = upcast [[SELF_COPY]] : $DerivedClassWithPublicProperty to $BaseClassWithInternalProperty
// CHECK-NEXT:    [[BORROWED_SUPER:%.*]] = begin_borrow [[SUPER]]
// CHECK-NEXT:    // function_ref observers.BaseClassWithInternalProperty.x.getter : ()
// CHECK-NEXT:    [[METHOD:%.*]] = function_ref @$s9observers29BaseClassWithInternalPropertyC1xytvg : $@convention(method) (@guaranteed BaseClassWithInternalProperty) -> ()
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[METHOD]]([[BORROWED_SUPER]]) : $@convention(method) (@guaranteed BaseClassWithInternalProperty) -> ()
// CHECK-NEXT:    end_borrow [[BORROWED_SUPER]]
// CHECK-NEXT:    destroy_value [[SUPER]] : $BaseClassWithInternalProperty
// CHECK: } // end sil function '$s9observers30DerivedClassWithPublicPropertyC1xytvg'


// Make sure we use the correct substitutions when referencing a property in a
// generic base class
public class GenericBase<T> {
  var storage: T? = nil
}

public class ConcreteDerived : GenericBase<Int> {
  override var storage: Int? {
    didSet {}
  }
}

// CHECK-LABEL: sil private [ossa] @$s9observers15ConcreteDerivedC7storageSiSgvW : $@convention(method) (@guaranteed ConcreteDerived) -> () {


// Make sure we upcast properly when the overridden property is in an ancestor
// of our superclass
public class BaseObserved {
  var x: Int = 0
}

public class MiddleObserved : BaseObserved {}

public class DerivedObserved : MiddleObserved {
  override var x: Int {
    didSet {}
  }
}

// CHECK-LABEL: sil private [ossa] @$s9observers15DerivedObservedC1xSivW : $@convention(method) (@guaranteed DerivedObserved) -> () {
