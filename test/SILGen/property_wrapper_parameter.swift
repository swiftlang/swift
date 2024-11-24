// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -enable-library-evolution %S/Inputs/def_structA.swift

// This uses '-primary-file' to ensure we're conservative with lazy SIL emission.
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -primary-file %s -I %t | %FileCheck %s

import def_structA

public struct Projection<T> {
  public var wrappedValue: T
}

@propertyWrapper
public struct Wrapper<T> {
  public var wrappedValue: T

  // CHECK-LABEL: sil [ossa] @$s26property_wrapper_parameter7WrapperV12wrappedValueACyxGx_tcfC : $@convention(method) <T> (@in T, @thin Wrapper<T>.Type) -> @out Wrapper<T>
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  public var projectedValue: Projection<T> {
    Projection(wrappedValue: wrappedValue)
  }

  // CHECK-LABEL: sil [ossa] @$s26property_wrapper_parameter7WrapperV14projectedValueACyxGAA10ProjectionVyxG_tcfC : $@convention(method) <T> (@in Projection<T>, @thin Wrapper<T>.Type) -> @out Wrapper<T>
  public init(projectedValue: Projection<T>) {
    self.wrappedValue = projectedValue.wrappedValue
  }
}

// property wrapper backing initializer of value #1 in testSimpleWrapperParameter(value:)
// CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tFACL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

// property wrapper init from projected value of value #1 in testSimpleWrapperParameter(value:)
// CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tFACL_SivpfW : $@convention(thin) (Projection<Int>) -> Wrapper<Int>

// CHECK-LABEL: sil [ossa] @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tF : $@convention(thin) (Wrapper<Int>) -> ()
public func testSimpleWrapperParameter(@Wrapper value: Int) {
  _ = value
  _ = _value
  _ = $value

  // getter of $value #1 in testSimpleWrapperParameter(value:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tF6$valueL_AA10ProjectionVySiGvg : $@convention(thin) (Wrapper<Int>) -> Projection<Int>

  // getter of value #1 in testSimpleWrapperParameter(value:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tFACL_Sivg : $@convention(thin) (Wrapper<Int>) -> Int
}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter28simpleWrapperParameterCaller10projectionyAA10ProjectionVySiG_tF : $@convention(thin) (Projection<Int>) -> ()
func simpleWrapperParameterCaller(projection: Projection<Int>) {
  testSimpleWrapperParameter(value: projection.wrappedValue)
  // CHECK: function_ref @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tFACL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

  testSimpleWrapperParameter($value: projection)
  // CHECK: function_ref @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tFACL_SivpfW : $@convention(thin) (Projection<Int>) -> Wrapper<Int>

  var x: Int = 10
  testSimpleWrapperParameter(value: x)
  // CHECK: function_ref @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tFACL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>
}

// property wrapper backing initializer of value #1 in testGenericWrapper<A>(value:)
// CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlFACL_xvpfP : $@convention(thin) <T> (@in T) -> @out Wrapper<T>

// property wrapper init from projected value of value #1 in testGenericWrapper<A>(value:)
// CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlFACL_xvpfW : $@convention(thin) <T> (@in Projection<T>) -> @out Wrapper<T>

// CHECK-LABEL: sil [ossa] @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlF : $@convention(thin) <T> (@in_guaranteed Wrapper<T>) -> ()
public func testGenericWrapper<T>(@Wrapper value: T) {
}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter20genericWrapperCaller10projectionyAA10ProjectionVySiG_tF : $@convention(thin) (Projection<Int>) -> ()
func genericWrapperCaller(projection: Projection<Int>) {
  testGenericWrapper(value: projection.wrappedValue)
  // CHECK: function_ref @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlFACL_xvpfP : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out Wrapper<τ_0_0>

  testGenericWrapper($value: projection)
  // CHECK: function_ref @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlFACL_xvpfW : $@convention(thin) <τ_0_0> (@in Projection<τ_0_0>) -> @out Wrapper<τ_0_0>

  var x: Int = 10
  testGenericWrapper(value: x)
  // CHECK: function_ref @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlFACL_xvpfP : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out Wrapper<τ_0_0>
}

@propertyWrapper
struct ImplementationDetail<T> {
  var wrappedValue: T

  // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter20ImplementationDetailV12wrappedValueACyxGx_tcfC : $@convention(method) <T> (@in T, @thin ImplementationDetail<T>.Type) -> @out ImplementationDetail<T>
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}


struct TestStructInit {
  // property wrapper backing initializer of number #1 in TestStructInit.init(number:message:)
  // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter14TestStructInitV6number7messageAcA7WrapperVySiG_SStcfcADL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

  // property wrapper init from projected value of number #1 in TestStructInit.init(number:message:)
  // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter14TestStructInitV6number7messageAcA7WrapperVySiG_SStcfcADL_SivpfW : $@convention(thin) (Projection<Int>) -> Wrapper<Int>

  // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter14TestStructInitV6number7messageAcA7WrapperVySiG_SStcfC : $@convention(method) (Wrapper<Int>, @owned String, @thin TestStructInit.Type) -> TestStructInit
  init(@Wrapper number: Int, @ImplementationDetail message: String) {
    // CHECK: debug_value %0 : $Wrapper<Int>, let, name "_number"
    // CHECK: debug_value [[STR:%.*]] : $String, let, name "message"
    // CHECK: alloc_stack $ImplementationDetail<String>
    // CHECK: begin_borrow [[STR]]
    // CHECK" function_ref @$s26property_wrapper_parameter20ImplementationDetailV12wrappedValueACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin ImplementationDetail<τ_0_0>.Type) -> @out ImplementationDetail<τ_0_0>

    _ = number
    _ = _number

    _ = message
    _ = _message

    // getter of number #1 in TestStructInit.init(number:message:)
    // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14TestStructInitV6number7messageAcA7WrapperVySiG_SStcfcADL_Sivg : $@convention(thin) (Wrapper<Int>) -> Int

    // getter of message #1 in TestStructInit.init(number:message:)
    // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14TestStructInitV6number7messageAcA7WrapperVySiG_SStcfcAEL_SSvg : $@convention(thin) (@guaranteed ImplementationDetail<String>) -> @owned String
  }
}

class TestClassInit {
  // property wrapper backing initializer of number #1 in TestClassInit.init(number:message:)
  // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter13TestClassInitC6number7messageAcA7WrapperVySiG_SStcfcADL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

  // property wrapper init from projected value of number #1 in TestClassInit.init(number:message:)
  // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter13TestClassInitC6number7messageAcA7WrapperVySiG_SStcfcADL_SivpfW : $@convention(thin) (Projection<Int>) -> Wrapper<Int>

  // TestClassInit.__allocating_init(number:message:)
  // CHECK-LABEL: sil hidden [exact_self_class] [ossa] @$s26property_wrapper_parameter13TestClassInitC6number7messageAcA7WrapperVySiG_SStcfC : $@convention(method) (Wrapper<Int>, @owned String, @thick TestClassInit.Type) -> @owned TestClassInit
  // CHECK-NOT: alloc_stack $ImplementationDetail<String>

  // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter13TestClassInitC6number7messageAcA7WrapperVySiG_SStcfc : $@convention(method) (Wrapper<Int>, @owned String, @owned TestClassInit) -> @owned TestClassInit
  init(@Wrapper number: Int, @ImplementationDetail message: String) {
    // CHECK: debug_value %0 : $Wrapper<Int>, let, name "_number"
    // CHECK: debug_value [[STR:%.*]] : $String, let, name "message"
    // CHECK: alloc_stack $ImplementationDetail<String>
    // CHECK: begin_borrow [[STR]]

    _ = number
    _ = _number

    _ = message
    _ = _message

    // getter of number #1 in TestClassInit.init(number:message:)
    // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter13TestClassInitC6number7messageAcA7WrapperVySiG_SStcfcADL_Sivg : $@convention(thin) (Wrapper<Int>) -> Int

    // getter of message #1 in TestClassInit.init(number:message:)
    // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter13TestClassInitC6number7messageAcA7WrapperVySiG_SStcfcAEL_SSvg : $@convention(thin) (@guaranteed ImplementationDetail<String>) -> @owned String
  }
}

@propertyWrapper
public struct AutoClosureWrapper<T> {
  public var wrappedValue: T

  // CHECK-LABEL: sil [ossa] @$s26property_wrapper_parameter18AutoClosureWrapperV12wrappedValueACyxGxyXK_tcfC : $@convention(method) <T> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>, @thin AutoClosureWrapper<T>.Type) -> @out AutoClosureWrapper<T>
  public init(wrappedValue: @autoclosure () -> T) {
    self.wrappedValue = wrappedValue()
  }

  public var projectedValue: Projection<T> {
    Projection(wrappedValue: wrappedValue)
  }

  public init(projectedValue: Projection<T>) {
    self.wrappedValue = projectedValue.wrappedValue
  }
}

// property wrapper backing initializer of value #1 in testAutoClosureWrapper<A>(value:)
// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter22testAutoClosureWrapper5valueyAA0efG0VyxG_tlFACL_xvpfP : $@convention(thin) <T> (@owned @noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>) -> @out AutoClosureWrapper<T>
// CHECK: function_ref @$s26property_wrapper_parameter18AutoClosureWrapperV12wrappedValueACyxGxyXK_tcfC : $@convention(method) <τ_0_0> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>, @thin AutoClosureWrapper<τ_0_0>.Type) -> @out AutoClosureWrapper<τ_0_0>

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter22testAutoClosureWrapper5valueyAA0efG0VyxG_tlF : $@convention(thin) <T> (@in_guaranteed AutoClosureWrapper<T>) -> ()
func testAutoClosureWrapper<T>(@AutoClosureWrapper value: T) {
}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter24autoClosureWrapperCalleryyF : $@convention(thin) () -> ()
func autoClosureWrapperCaller() {
  testAutoClosureWrapper(value: 10)
  // CHECK: function_ref @$s26property_wrapper_parameter22testAutoClosureWrapper5valueyAA0efG0VyxG_tlFACL_xvpfP : $@convention(thin) <τ_0_0> (@owned @noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>) -> @out AutoClosureWrapper<τ_0_0>
}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter33testSimpleClosureWrapperParameteryyF : $@convention(thin) () -> ()
func testSimpleClosureWrapperParameter() {
  let closure: (Int) -> Void = { (@Wrapper value) in
    _ = value
    _ = _value
    _ = $value
  }

  closure(10)

  // implicit closure #1 in testSimpleClosureWrapperParameter()
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter33testSimpleClosureWrapperParameteryyFySicfu_ : $@convention(thin) (Int) -> ()

  // closure #1 in implicit closure #1 in testSimpleClosureWrapperParameter()
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter33testSimpleClosureWrapperParameteryyFySicfu_yAA0G0VySiGcfU_ : $@convention(thin) (Wrapper<Int>) -> ()

  // property wrapper backing initializer of value #1 in closure #1 in implicit closure #1 in testSimpleClosureWrapperParameter()
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter33testSimpleClosureWrapperParameteryyFySicfu_yAA0G0VySiGcfU_5valueL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

  // getter of $value #1 in closure #1 in implicit closure #1 in testSimpleClosureWrapperParameter()
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter33testSimpleClosureWrapperParameteryyFySicfu_yAA0G0VySiGcfU_6$valueL_AA10ProjectionVySiGvg : $@convention(thin) (Wrapper<Int>) -> Projection<Int>

  // getter of value #1 in closure #1 in implicit closure #1 in testSimpleClosureWrapperParameter()
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter33testSimpleClosureWrapperParameteryyFySicfu_yAA0G0VySiGcfU_5valueL_Sivg : $@convention(thin) (Wrapper<Int>) -> Int
}

@propertyWrapper
struct NonMutatingSetterWrapper<Value> {
  private var value: Value

  var wrappedValue: Value {
    get { value }
    nonmutating set { }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter24NonMutatingSetterWrapperV12wrappedValueACyxGx_tcfC : $@convention(method) <Value> (@in Value, @thin NonMutatingSetterWrapper<Value>.Type) -> @out NonMutatingSetterWrapper<Value>
  init(wrappedValue: Value) {
    self.value = wrappedValue
  }
}

@propertyWrapper
class ClassWrapper<Value> {
  var wrappedValue: Value

  // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter12ClassWrapperC12wrappedValueACyxGx_tcfc : $@convention(method) <Value> (@in Value, @owned ClassWrapper<Value>) -> @owned ClassWrapper<Value>
  init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2ySS_SitF : $@convention(thin) (@guaranteed String, Int) -> ()
func testNonMutatingSetter(@NonMutatingSetterWrapper value1: String, @ClassWrapper value2: Int) {
  // CHECK: function_ref @$s26property_wrapper_parameter24NonMutatingSetterWrapperV12wrappedValueACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin NonMutatingSetterWrapper<τ_0_0>.Type) -> @out NonMutatingSetterWrapper<τ_0_0>
  // CHECK: debug_value {{.*}} : $NonMutatingSetterWrapper<String>, let, name "_value1"
  // CHECK: function_ref @$s26property_wrapper_parameter12ClassWrapperC12wrappedValueACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thick ClassWrapper<τ_0_0>.Type) -> @owned ClassWrapper<τ_0_0>
  // CHECK: debug_value {{.*}} : $ClassWrapper<Int>, let, name "_value2"

  _ = value1
  value1 = "hello!"

  // getter of value1 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2ySS_SitFACL_SSvg : $@convention(thin) (@guaranteed NonMutatingSetterWrapper<String>) -> @owned String

  // setter of value1 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2ySS_SitFACL_SSvs : $@convention(thin) (@owned String, @guaranteed NonMutatingSetterWrapper<String>) -> ()

  _ = value2
  value2 = 10

  // getter of value2 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2ySS_SitFADL_Sivg : $@convention(thin) (@guaranteed ClassWrapper<Int>) -> Int

  // setter of value2 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2ySS_SitFADL_Sivs : $@convention(thin) (Int, @guaranteed ClassWrapper<Int>) -> ()
}

@propertyWrapper
struct ProjectionWrapper<Value> {
  var wrappedValue: Value

  var projectedValue: ProjectionWrapper<Value> { self }

  init(wrappedValue: Value) { self.wrappedValue = wrappedValue }

  init(projectedValue: ProjectionWrapper<Value>) {
    self.wrappedValue = projectedValue.wrappedValue
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tF : $@convention(thin) (ProjectionWrapper<Int>) -> ()
func testImplicitPropertyWrapper(projection: ProjectionWrapper<Int>) {
  let multiStatement: (ProjectionWrapper<Int>) -> Void = { $value in
    _ = value
    _ = _value
    _ = $value
  }

  multiStatement(projection)

  // implicit closure #1 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFyAFcfu_ : $@convention(thin) (ProjectionWrapper<Int>) -> ()

  // closure #1 in implicit closure #1 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFyAFcfu_yAFcfU_ : $@convention(thin) (ProjectionWrapper<Int>) -> ()

  // property wrapper init from projected value of $value #1 in closure #1 in implicit closure #1 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFyAFcfu_yAFcfU_6$valueL_AFvpfW : $@convention(thin) (ProjectionWrapper<Int>) -> ProjectionWrapper<Int>

  // getter of $value #1 in closure #1 in implicit closure #1 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFyAFcfu_yAFcfU_6$valueL_AFvg : $@convention(thin) (ProjectionWrapper<Int>) -> ProjectionWrapper<Int>

  // getter of value #1 in closure #1 in implicit closure #1 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFyAFcfu_yAFcfU_5valueL_Sivg : $@convention(thin) (ProjectionWrapper<Int>) -> Int

  let _: (ProjectionWrapper<Int>) -> (Int, ProjectionWrapper<Int>) = { $value in
    (value, $value)
  }

  // implicit closure #2 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFSi_AFtAFcfu0_ : $@convention(thin) (ProjectionWrapper<Int>) -> (Int, ProjectionWrapper<Int>)

  // closure #2 in implicit closure #2 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFSi_AFtAFcfu0_Si_AFtAFcfU0_ : $@convention(thin) (ProjectionWrapper<Int>) -> (Int, ProjectionWrapper<Int>)

  // property wrapper init from projected value of $value #1 in closure #2 in implicit closure #2 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFSi_AFtAFcfu0_Si_AFtAFcfU0_6$valueL_AFvpfW : $@convention(thin) (ProjectionWrapper<Int>) -> ProjectionWrapper<Int>

  // getter of $value #1 in closure #2 in implicit closure #2 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFSi_AFtAFcfu0_Si_AFtAFcfU0_6$valueL_AFvg : $@convention(thin) (ProjectionWrapper<Int>) -> ProjectionWrapper<Int>

  // getter of value #1 in closure #2 in implicit closure #2 in testImplicitPropertyWrapper(projection:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFSi_AFtAFcfu0_Si_AFtAFcfU0_5valueL_Sivg : $@convention(thin) (ProjectionWrapper<Int>) -> Int
}

protocol P {}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlF : $@convention(thin) <T where T : P> (@in_guaranteed T) -> ()
func genericContext<T>(_: T) where T: P {
  let _: (ProjectionWrapper<Int>) -> Void = { $value in }

  // implicit closure #1 in genericContext<A>(_:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlFyAA17ProjectionWrapperVySiGcfu_ : $@convention(thin) (ProjectionWrapper<Int>) -> ()

  // This property wrapper generator function should _not_ have a generic signature,
  // because the closure doesn't have one.

  // property wrapper init from projected value of $value #1 in closure #1 in implicit closure #1 in genericContext<A>(_:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlFyAA17ProjectionWrapperVySiGcfu_yAFcfU_6$valueL_AFvpfW : $@convention(thin) (ProjectionWrapper<Int>) -> ProjectionWrapper<Int>

  let _: (ProjectionWrapper<Int>) -> T = { $value in
    fatalError()
  }

  // implicit closure #2 in genericContext<A>(_:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlFxAA17ProjectionWrapperVySiGcfu0_ : $@convention(thin) <T where T : P> (ProjectionWrapper<Int>) -> @out T

  // This property wrapper generator function _should_ have a generic signature, because
  // the closure does have one.

  // property wrapper init from projected value of $value #1 in closure #2 in implicit closure #2 in genericContext<A>(_:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlFxAA17ProjectionWrapperVySiGcfu0_xAFcfU0_6$valueL_AFvpfW : $@convention(thin) <T where T : P> (ProjectionWrapper<Int>) -> ProjectionWrapper<Int>

  // property wrapper backing initializer of a #1 in inner #1 <A>(a:) in genericContext<A>(_:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlF5innerL_1ayAA7WrapperVySiG_tAaCRzlFAEL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlF5innerL_1ayAA7WrapperVySiG_tAaCRzlF : $@convention(thin) (Wrapper<Int>) -> ()
  func inner(@Wrapper a: Int) {}

  inner(a: 1)

  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlF5innerL0_yyAaCRzlF : $@convention(thin) <T where T : P> () -> ()
  func inner() { _ = T.self }

  // property wrapper backing initializer of b #1 in inner #3 <A>(b:) in genericContext<A>(_:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlF5innerL1_1byAA7WrapperVySiG_tAaCRzlFAEL_SivpfP : $@convention(thin) <T where T : P> (Int) -> Wrapper<Int>

  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter14genericContextyyxAA1PRzlF5innerL1_1byAA7WrapperVySiG_tAaCRzlF : $@convention(thin) <T where T : P> (Wrapper<Int>) -> ()
  func inner(@Wrapper b: Int) {
    inner()
  }

  inner(b: 1)
}

struct HasPrivate {
  // property wrapper backing initializer of x #1 in HasPrivate.testPrivateWrapper(x:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter10HasPrivateV04testE7Wrapper{{.*}}LL1xyAA0G0VySiG_tFAFL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

  // property wrapper init from projected value of x #1 in HasPrivate.testPrivateWrapper(x:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter10HasPrivateV04testE7Wrapper{{.*}}LL1xyAA0G0VySiG_tFAFL_SivpfW : $@convention(thin) (Projection<Int>) -> Wrapper<Int>

  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter10HasPrivateV04testE7Wrapper{{.*}}LL1xyAA0G0VySiG_tF : $@convention(method) (Wrapper<Int>, HasPrivate) -> ()
  private func testPrivateWrapper(@Wrapper x: Int) {}

  // property wrapper backing initializer of x #1 in HasPrivate.testFilePrivateWrapper(x:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter10HasPrivateV08testFileE7Wrapper{{.*}}LL1xyAA0H0VySiG_tFAFL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

  // property wrapper init from projected value of x #1 in HasPrivate.testFilePrivateWrapper(x:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter10HasPrivateV08testFileE7Wrapper{{.*}}LL1xyAA0H0VySiG_tFAFL_SivpfW : $@convention(thin) (Projection<Int>) -> Wrapper<Int>

  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter10HasPrivateV08testFileE7Wrapper{{.*}}LL1xyAA0H0VySiG_tF : $@convention(method) (Wrapper<Int>, HasPrivate) -> ()
  fileprivate func testFilePrivateWrapper(@Wrapper x: Int) {}

  func usesWrapperFunctions() {
    // These are needed to ensure we emit the backing initializers. Otherwise
    // lazy SILGen emission is happy to drop them.
    testPrivateWrapper(x: 0)
    testPrivateWrapper($x: Projection(wrappedValue: 0))
    testFilePrivateWrapper(x: 0)
    testFilePrivateWrapper($x: Projection(wrappedValue: 0))
  }
}

@propertyWrapper
public struct PublicWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  public var projectedValue: PublicWrapper<T> {
    return self
  }

  public init(projectedValue: PublicWrapper<T>) {
    self.wrappedValue = projectedValue.wrappedValue
  }
}

// property wrapper backing initializer of value #1 in publicFunc(value:)
// CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter10publicFunc5valueyAA13PublicWrapperVySSG_tFACL_SSvpfP : $@convention(thin) (@owned String) -> @owned PublicWrapper<String>

// property wrapper init from projected value of value #1 in publicFunc(value:)
// CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter10publicFunc5valueyAA13PublicWrapperVySSG_tFACL_SSvpfW : $@convention(thin) (@owned PublicWrapper<String>) -> @owned PublicWrapper<String>

// CHECK-LABEL: sil [ossa] @$s26property_wrapper_parameter10publicFunc5valueyAA13PublicWrapperVySSG_tF : $@convention(thin) (@guaranteed PublicWrapper<String>) -> ()
public func publicFunc(@PublicWrapper value: String) {
}

// property wrapper backing initializer of value #1 in inlinableFunc(value:)
// CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter13inlinableFunc5valueyAA13PublicWrapperVySSG_tFACL_SSvpfP : $@convention(thin) (@owned String) -> @owned PublicWrapper<String>

// property wrapper init from projected value of value #1 in inlinableFunc(value:)
// CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter13inlinableFunc5valueyAA13PublicWrapperVySSG_tFACL_SSvpfW : $@convention(thin) (@owned PublicWrapper<String>) -> @owned PublicWrapper<String>

// CHECK-LABEL: sil [serialized] [ossa] @$s26property_wrapper_parameter13inlinableFunc5valueyAA13PublicWrapperVySSG_tF : $@convention(thin) (@guaranteed PublicWrapper<String>) -> ()
@inlinable func inlinableFunc(@PublicWrapper value: String) {
  _ = publicFunc(value:)

  // implicit closure #1 in inlinableFunc(value:)
  // CHECK: sil shared [serialized] [ossa] @$s26property_wrapper_parameter13inlinableFunc5valueyAA13PublicWrapperVySSG_tFySScfu_ : $@convention(thin) (@guaranteed String) -> ()
  // CHECK: function_ref @$s26property_wrapper_parameter10publicFunc5valueyAA13PublicWrapperVySSG_tFACL_SSvpfP : $@convention(thin) (@owned String) -> @owned PublicWrapper<String>
  // CHECK: function_ref @$s26property_wrapper_parameter10publicFunc5valueyAA13PublicWrapperVySSG_tF : $@convention(thin) (@guaranteed PublicWrapper<String>) -> ()

  // property wrapper init from projected value of $x #1 in closure #1 in implicit closure #1 in inlinableFunc(value:)
  // CHECK: sil shared [serialized] [ossa] @$s26property_wrapper_parameter13inlinableFunc5valueyAA13PublicWrapperVySSG_tFyAEySiGcfu0_yAGcfU_2$xL_AGvpfW : $@convention(thin) (PublicWrapper<Int>) -> PublicWrapper<Int>
  let _: (PublicWrapper<Int>) -> Void = { $x in }
}

@propertyWrapper
struct NonmutatingSetter<Value> {
  var wrappedValue: Value {
    // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter17NonmutatingSetterV12wrappedValuexvg : $@convention(method) <Value> (NonmutatingSetter<Value>) -> @out Value
    get { fatalError() }
    // CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter17NonmutatingSetterV12wrappedValuexvs : $@convention(method) <Value> (@in Value, NonmutatingSetter<Value>) -> ()
    nonmutating set {}
  }
  var projectedValue: Self { self }
  init(wrappedValue: Value) {}
  init(projectedValue: Self) {}
}

func genericClosure<T>(arg: T, _ closure: (T) -> Int) {}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter30testNonmutatingSetterSynthesis5valueyAA0eF0VySiG_tF : $@convention(thin) (NonmutatingSetter<Int>) -> ()
func testNonmutatingSetterSynthesis(@NonmutatingSetter value: Int) {
  genericClosure(arg: $value) { $value in
    (value = 10, value).1
  }

  // closure #1 in implicit closure #1 in testNonmutatingSetterSynthesis(value:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter30testNonmutatingSetterSynthesis5valueyAA0eF0VySiG_tFSiAFcfu_SiAFcfU_ : $@convention(thin) (NonmutatingSetter<Int>) -> Int
  // CHECK: function_ref @$s26property_wrapper_parameter30testNonmutatingSetterSynthesis5valueyAA0eF0VySiG_tFSiAFcfu_SiAFcfU_ACL_Sivs : $@convention(thin) (Int, NonmutatingSetter<Int>) -> ()
  // CHECK: function_ref @$s26property_wrapper_parameter30testNonmutatingSetterSynthesis5valueyAA0eF0VySiG_tFSiAFcfu_SiAFcfU_ACL_Sivg : $@convention(thin) (NonmutatingSetter<Int>) -> Int
  // CHECK: return

  // getter of value #1 in closure #1 in implicit closure #1 in testNonmutatingSetterSynthesis(value:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter30testNonmutatingSetterSynthesis5valueyAA0eF0VySiG_tFSiAFcfu_SiAFcfU_ACL_Sivg : $@convention(thin) (NonmutatingSetter<Int>) -> Int
  // CHECK: function_ref @$s26property_wrapper_parameter17NonmutatingSetterV12wrappedValuexvg : $@convention(method) <τ_0_0> (NonmutatingSetter<τ_0_0>) -> @out τ_0_0

  // setter of value #1 in closure #1 in implicit closure #1 in testNonmutatingSetterSynthesis(value:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter30testNonmutatingSetterSynthesis5valueyAA0eF0VySiG_tFSiAFcfu_SiAFcfU_ACL_Sivs : $@convention(thin) (Int, NonmutatingSetter<Int>) -> ()
  // CHECK: function_ref @$s26property_wrapper_parameter17NonmutatingSetterV12wrappedValuexvs : $@convention(method) <τ_0_0> (@in τ_0_0, NonmutatingSetter<τ_0_0>) -> ()
}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter38testImplicitWrapperWithResilientStructyyF : $@convention(thin) () -> ()
func testImplicitWrapperWithResilientStruct() {
  let _: (ProjectionWrapper<A>) -> Void = { $value in }

  // implicit closure #1 in testImplicitWrapperWithResilientStruct()
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter38testImplicitWrapperWithResilientStructyyFyAA010ProjectionF0Vy11def_structA1AVGcfu_ : $@convention(thin) (@in_guaranteed ProjectionWrapper<A>) -> ()
  // CHECK: [[P:%.*]] = alloc_stack $ProjectionWrapper<A>
  // CHECK: copy_addr %0 to [init] [[P]]
  // CHECK: [[I:%.*]] = function_ref @$s26property_wrapper_parameter38testImplicitWrapperWithResilientStructyyFyAA010ProjectionF0Vy11def_structA1AVGcfu_yAHcfU_6$valueL_AHvpfW : $@convention(thin) (@in ProjectionWrapper<A>) -> @out ProjectionWrapper<A>
  // CHECK: apply [[I]]({{.*}}, [[P]]) : $@convention(thin) (@in ProjectionWrapper<A>) -> @out ProjectionWrapper<A>

  // property wrapper init from projected value of $value #1 in closure #1 in implicit closure #1 in testImplicitWrapperWithResilientStruct()
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter38testImplicitWrapperWithResilientStructyyFyAA010ProjectionF0Vy11def_structA1AVGcfu_yAHcfU_6$valueL_AHvpfW : $@convention(thin) (@in ProjectionWrapper<A>) -> @out ProjectionWrapper<A>
}

func takesAutoclosure(_: @autoclosure () -> Int) {}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter12testCaptures3ref5valueySi_AA7WrapperVySiGtF : $@convention(thin) (Int, Wrapper<Int>) -> ()
func testCaptures(@ClassWrapper ref: Int, @Wrapper value: Int) {
  takesAutoclosure(ref)
  // implicit closure #1 in testCaptures(ref:value:)
  // CHECK-LABEL: sil private [transparent] [ossa] @$s26property_wrapper_parameter12testCaptures3ref5valueySi_AA7WrapperVySiGtFSiyXEfu_ : $@convention(thin) (@guaranteed ClassWrapper<Int>) -> Int

  let _: () -> Void = {
    _ = ref
    ref = 100
  }
  // closure #1 in testCaptures(ref:value:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter12testCaptures3ref5valueySi_AA7WrapperVySiGtFyycfU_ : $@convention(thin) (@guaranteed ClassWrapper<Int>) -> ()

  let _: () -> Projection<Int> = { $value }
  // closure #2 in testCaptures(ref:value:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter12testCaptures3ref5valueySi_AA7WrapperVySiGtFAA10ProjectionVySiGycfU0_ : $@convention(thin) (Wrapper<Int>) -> Projection<Int>

  let _: (ProjectionWrapper<Int>) -> Void = { $x in
    _ = { x }
    _ = { $x }
  }
  // Make sure there are 4 closures here with the right arguments

  // implicit closure #2 in testCaptures(ref:value:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter12testCaptures3ref5valueySi_AA7WrapperVySiGtFyAA010ProjectionH0VySiGcfu0_ : $@convention(thin) (ProjectionWrapper<Int>) -> ()

  // closure #3 in implicit closure #2 in testCaptures(ref:value:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter12testCaptures3ref5valueySi_AA7WrapperVySiGtFyAA010ProjectionH0VySiGcfu0_yAJcfU1_ : $@convention(thin) (ProjectionWrapper<Int>) -> ()

  // closure #1 in closure #2 in implicit closure #2 in testCaptures(ref:value:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter12testCaptures3ref5valueySi_AA7WrapperVySiGtFyAA010ProjectionH0VySiGcfu0_yAJcfU1_SiycfU_ : $@convention(thin) (ProjectionWrapper<Int>) -> Int

  // closure #2 in closure #2 in implicit closure #2 in testCaptures(ref:value:)
  // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter12testCaptures3ref5valueySi_AA7WrapperVySiGtFyAA010ProjectionH0VySiGcfu0_yAJcfU1_AJycfU0_ : $@convention(thin) (ProjectionWrapper<Int>) -> ProjectionWrapper<Int>
}

do {
  @propertyWrapper
  struct Binding<Value> {
    var wrappedValue: Value {
      get { fatalError() }
      nonmutating set { }
    }

    var projectedValue: Self { self }

    init(projectedValue: Self) { self = projectedValue }
  }

  final class Value {
    enum Kind {
    }

    var kind: Binding<Kind> {
      fatalError()
    }
  }

  struct Test {
    var value: Value

    // CHECK-LABEL: sil private [ossa] @$s26property_wrapper_parameter4TestL_V4test5otheryAA7BindingL_VyAA5ValueL_C4KindOG_tF : $@convention(method) (Binding<Value.Kind>, @guaranteed Test) -> ()
    // CHECK: [[CHECK_PROJECTED_VALUE_INIT_1:%.*]] = function_ref @$s26property_wrapper_parameter4TestL_V9checkKind4kindyAA7BindingL_VyAA5ValueL_C0F0OG_tFAEL_AKvpfW
    // CHECK-NEXT: {{.*}} = apply [[CHECK_PROJECTED_VALUE_INIT_1]]({{.*}}) : $@convention(thin) (Binding<Value.Kind>) -> Binding<Value.Kind>
    // CHECK: [[CHECK_PROJECTED_VALUE_INIT_A:%.*]] = function_ref @$s26property_wrapper_parameter4TestL_V15doubleCheckKind1a1byAA7BindingL_VyAA5ValueL_C0G0OG_AMtFAEL_ALvpfW
    // CHECK-NEXT: {{.*}} = apply [[CHECK_PROJECTED_VALUE_INIT_A]]({{.*}}) : $@convention(thin) (Binding<Value.Kind>) -> Binding<Value.Kind>
    // CHECK: [[CHECK_PROJECTED_VALUE_INIT_B:%.*]] = function_ref @$s26property_wrapper_parameter4TestL_V15doubleCheckKind1a1byAA7BindingL_VyAA5ValueL_C0G0OG_AMtFAFL_ALvpfW
    // CHECK-NEXT: {{.*}} = apply [[CHECK_PROJECTED_VALUE_INIT_B]]({{.*}}) : $@convention(thin) (Binding<Value.Kind>) -> Binding<Value.Kind>
    func test(other: Binding<Value.Kind>) {
      checkKind($kind: value.kind) // Ok
      doubleCheckKind($a: value.kind, $b: other) // Ok
    }

    func checkKind(@Binding kind: Value.Kind) {}
    func doubleCheckKind(@Binding a: Value.Kind, @Binding b: Value.Kind) {}
  }
}
