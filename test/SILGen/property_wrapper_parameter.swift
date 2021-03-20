// RUN: %target-swift-emit-silgen %s | %FileCheck %s

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

// CHECK-LABEL: sil [ossa] @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tF : $@convention(thin) (Wrapper<Int>) -> ()
public func testSimpleWrapperParameter(@Wrapper value: Int) {
  _ = value
  _ = _value
  _ = $value

  // property wrapper backing initializer of value #1 in testSimpleWrapperParameter(value:)
  // CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tFACL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

  // property wrapper init from projected value of value #1 in testSimpleWrapperParameter(value:)
  // CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter26testSimpleWrapperParameter5valueyAA0F0VySiG_tFACL_SivpfW : $@convention(thin) (Projection<Int>) -> Wrapper<Int>

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
}

// CHECK-LABEL: sil [ossa] @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlF : $@convention(thin) <T> (@in_guaranteed Wrapper<T>) -> ()
public func testGenericWrapper<T>(@Wrapper value: T) {

  // property wrapper backing initializer of value #1 in testGenericWrapper<A>(value:)
  // CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlFACL_xvpfP : $@convention(thin) <T> (@in T) -> @out Wrapper<T>

  // property wrapper init from projected value of value #1 in testGenericWrapper<A>(value:)
  // CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlFACL_xvpfW : $@convention(thin) <T> (@in Projection<T>) -> @out Wrapper<T>
}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter20genericWrapperCaller10projectionyAA10ProjectionVySiG_tF : $@convention(thin) (Projection<Int>) -> ()
func genericWrapperCaller(projection: Projection<Int>) {
  testGenericWrapper(value: projection.wrappedValue)
  // CHECK: function_ref @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlFACL_xvpfP : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out Wrapper<τ_0_0>

  testGenericWrapper($value: projection)
  // CHECK: function_ref @$s26property_wrapper_parameter18testGenericWrapper5valueyAA0F0VyxG_tlFACL_xvpfW : $@convention(thin) <τ_0_0> (@in Projection<τ_0_0>) -> @out Wrapper<τ_0_0>
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

  init(wrappedValue: Value) {
    self.value = wrappedValue
  }
}

@propertyWrapper
class ClassWrapper<Value> {
  var wrappedValue: Value

  init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2yAA0efG7WrapperVySSG_AA05ClassJ0CySiGtF : $@convention(thin) (@guaranteed NonMutatingSetterWrapper<String>, @guaranteed ClassWrapper<Int>) -> ()
func testNonMutatingSetter(@NonMutatingSetterWrapper value1: String, @ClassWrapper value2: Int) {
  _ = value1
  value1 = "hello!"

  // property wrapper backing initializer of value1 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2yAA0efG7WrapperVySSG_AA05ClassJ0CySiGtFACL_SSvpfP : $@convention(thin) (@owned String) -> @owned NonMutatingSetterWrapper<String>

  // property wrapper backing initializer of value2 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2yAA0efG7WrapperVySSG_AA05ClassJ0CySiGtFADL_SivpfP : $@convention(thin) (Int) -> @owned ClassWrapper<Int>

  // getter of value1 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2yAA0efG7WrapperVySSG_AA05ClassJ0CySiGtFACL_SSvg : $@convention(thin) (@guaranteed NonMutatingSetterWrapper<String>) -> @owned String

  // setter of value1 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2yAA0efG7WrapperVySSG_AA05ClassJ0CySiGtFACL_SSvs : $@convention(thin) (@owned String, @guaranteed NonMutatingSetterWrapper<String>) -> ()

  _ = value2
  value2 = 10

  // getter of value2 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2yAA0efG7WrapperVySSG_AA05ClassJ0CySiGtFADL_Sivg : $@convention(thin) (@guaranteed ClassWrapper<Int>) -> Int

  // setter of value2 #1 in testNonMutatingSetter(value1:value2:)
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter21testNonMutatingSetter6value16value2yAA0efG7WrapperVySSG_AA05ClassJ0CySiGtFADL_Sivs : $@convention(thin) (Int, @guaranteed ClassWrapper<Int>) -> ()
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
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFyAFcfu_yAFcfU_5valueL_Sivg : $@convention(thin) () -> Int

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
  // CHECK: sil private [ossa] @$s26property_wrapper_parameter27testImplicitPropertyWrapper10projectionyAA010ProjectionG0VySiG_tFSi_AFtAFcfu0_Si_AFtAFcfU0_5valueL_Sivg : $@convention(thin) () -> Int
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

// CHECK-LABEL: sil [ossa] @$s26property_wrapper_parameter10publicFunc5valueyAA13PublicWrapperVySSG_tF : $@convention(thin) (@guaranteed PublicWrapper<String>) -> ()
public func publicFunc(@PublicWrapper value: String) {
  // property wrapper backing initializer of value #1 in publicFunc(value:)
  // CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter10publicFunc5valueyAA13PublicWrapperVySSG_tFACL_SSvpfP : $@convention(thin) (@owned String) -> @owned PublicWrapper<String>

  // property wrapper init from projected value of value #1 in publicFunc(value:)
  // CHECK: sil non_abi [serialized] [ossa] @$s26property_wrapper_parameter10publicFunc5valueyAA13PublicWrapperVySSG_tFACL_SSvpfW : $@convention(thin) (@owned PublicWrapper<String>) -> @owned PublicWrapper<String>
}
