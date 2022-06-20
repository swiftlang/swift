// RUN: %target-swift-emit-silgen -enable-sil-opaque-values -Xllvm -sil-full-demangle %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

// Test SILGen -enable-sil-opaque-values with tests that depend on the stdlib.

// FIXME: "HECK" lines all need to be updated for OSSA.

class C {}

func genericInout<T>(_: inout T) {}

func hasVarArg(_ args: Any...) {}

// Test array initialization - we are still (somewhat) using addresses
// ---
// CHECK-LABEL: sil [ossa] @$s20opaque_values_silgen10callVarArgyyF : $@convention(thin) () -> () {
// HECK: %[[APY:.*]] = apply %{{.*}}<Any>(%{{.*}}) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// HECK: %[[BRW:.*]] = begin_borrow %[[APY]]
// HECK: %[[TPL:.*]] = tuple_extract %[[BRW]] : $(Array<Any>, Builtin.RawPointer), 1
// HECK: end_borrow %[[BRW]] : $(Array<Any>, Builtin.RawPointer)
// HECK: destroy_value %[[APY]]
// HECK: %[[PTR:.*]] = pointer_to_address %[[TPL]] : $Builtin.RawPointer to [strict] $*Any
// HECK: [[IOPAQUE:%.*]] = init_existential_value %{{.*}} : $Int, $Int, $Any
// HECK: store [[IOPAQUE]] to [init] %[[PTR]] : $*Any
// HECK: return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen10callVarArgyyF'
public func callVarArg() {
  hasVarArg(3)
}

// Tests For-each statements
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen11forEachStmtyyF : $@convention(thin) () -> () {
// HECK: bb0:
// HECK:   [[PROJ_BOX_ARG:%.*]] = project_box %{{.*}} : ${ var IndexingIterator<Range<Int>> }
// HECK:   [[APPLY_ARG1:%.*]] = apply
// HECK-NOT: alloc_stack $Int
// HECK-NOT: store [[APPLY_ARG1]] to [trivial]
// HECK-NOT: alloc_stack $Range<Int>
// HECK-NOT: dealloc_stack
// HECK:   [[APPLY_ARG2:%.*]] = apply %{{.*}}<Range<Int>>
// HECK:   store [[APPLY_ARG2]] to [trivial] [[PROJ_BOX_ARG]]
// HECK:   br bb1
// HECK: bb1:
// CHECK-NOT: alloc_stack $Optional<Int>
// HECK:   [[APPLY_ARG3:%.*]] = apply %{{.*}}<Range<Int>>
// CHECK-NOT: dealloc_stack
// HECK:   switch_enum [[APPLY_ARG3]]
// HECK: bb2:
// HECK:   br bb3
// HECK: bb3:
// HECK:   return %{{.*}} : $()
// HECK: bb4([[ENUM_ARG:%.*]] : $Int):
// CHECK-NOT:   unchecked_enum_data
// HECK:   br bb1
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen11forEachStmtyyF'
func forEachStmt() {
  for _ in 1..<42 {
  }
}

// Tests that existential boxes can contain opaque types
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen12openExistBoxySSs5Error_pF : $@convention(thin) (@guaranteed Error) -> @owned String {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Error):
// CHECK:   [[OPAQUE_ARG:%.*]] = open_existential_box_value [[ARG]] : $Error to $@opened({{.*}}) Error
// CHECK:   [[ALLOC_OPEN:%.*]] = alloc_stack $@opened({{.*}}) Error
// CHECK:   [[COPY:%.*]] = copy_value [[OPAQUE_ARG]]
// CHECK:   store [[COPY]] to [init] [[ALLOC_OPEN]]
// CHECK:   destroy_addr [[ALLOC_OPEN]]
// CHECK:   dealloc_stack [[ALLOC_OPEN]]
// CHECK-NOT:   destroy_value [[ARG]] : $Error
// CHECK:   return {{.*}} : $String
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen12openExistBoxySSs5Error_pF'
func openExistBox(_ x: Error) -> String {
  return x._domain
}

// Tests conditional value casts and correspondingly generated reabstraction thunk
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen11condFromAnyyyypF : $@convention(thin) (@in_guaranteed Any) -> () {
// HECK: bb0([[ARG:%.*]] : $Any):
// HECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// HECK:   checked_cast_br [[COPY_ARG]] : $Any to $@callee_guaranteed (@in_guaranteed (Int, (Int, (Int, Int)), Int)) -> @out (Int, (Int, (Int, Int)), Int), bb2, bb1
// HECK: bb2([[THUNK_PARAM:%.*]] : $@callee_guaranteed (@in_guaranteed (Int, (Int, (Int, Int)), Int)) -> @out (Int, (Int, (Int, Int)), Int)):
// HECK:   [[THUNK_REF:%.*]] = function_ref @{{.*}} : $@convention(thin) (Int, Int, Int, Int, Int, @guaranteed @callee_guaranteed (@in_guaranteed (Int, (Int, (Int, Int)), Int)) -> @out (Int, (Int, (Int, Int)), Int)) -> (Int, Int, Int, Int, Int)
// HECK:   partial_apply [callee_guaranteed] [[THUNK_REF]]([[THUNK_PARAM]])
// HECK: bb6:
// HECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen11condFromAnyyyypF'
func condFromAny(_ x: Any) {
  if let f = x as? (Int, (Int, (Int, Int)), Int) -> (Int, (Int, (Int, Int)), Int) {
    _ = f(24, (4,(2, 42)), 42)
  }
}

// Tests support for if statements for opaque value(s) under new mode
// ---
protocol EmptyP {}

struct AddressOnlyStruct : EmptyP {}

// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen10addrOnlyIf1xAA6EmptyP_pSb_tF : $@convention(thin) (Bool) -> @out EmptyP {
// HECK: bb0([[ARG:%.*]] : $Bool):
// HECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var EmptyP }, var
// HECK:   [[PROJ_BOX:%.*]] = project_box [[ALLOC_OF_BOX]]
// HECK:   [[APPLY_FOR_BOX:%.*]] = apply %{{.*}}(%{{.*}}) : $@convention(method) (@thin AddressOnlyStruct.Type) -> AddressOnlyStruct
// HECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[APPLY_FOR_BOX]] : $AddressOnlyStruct, $AddressOnlyStruct, $EmptyP
// HECK:   store [[INIT_OPAQUE]] to [init] [[PROJ_BOX]] : $*EmptyP
// HECK:   [[APPLY_FOR_BRANCH:%.*]] = apply %{{.*}}([[ARG]]) : $@convention(method) (Bool) -> Builtin.Int1
// HECK:   cond_br [[APPLY_FOR_BRANCH]], bb2, bb1
// HECK: bb1:
// HECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PROJ_BOX]] : $*EmptyP
// HECK:   [[RETVAL1:%.*]] = load [copy] [[READ]] : $*EmptyP
// HECK:   br bb3([[RETVAL1]] : $EmptyP)
// HECK: bb2:
// HECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PROJ_BOX]] : $*EmptyP
// HECK:   [[RETVAL2:%.*]] = load [copy] [[READ]] : $*EmptyP
// HECK:   br bb3([[RETVAL2]] : $EmptyP)
// HECK: bb3([[RETVAL:%.*]] : $EmptyP):
// HECK:   destroy_value [[ALLOC_OF_BOX]]
// HECK:   return [[RETVAL]] : $EmptyP
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen10addrOnlyIf1xAA6EmptyP_pSb_tF'
func addrOnlyIf(x: Bool) -> EmptyP {
  var a : EmptyP = AddressOnlyStruct()
  genericInout(&a)
  return x ? a : a
}

// Tests LValue of error types / existential boxes
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen12propOfLValueySSs5Error_pF : $@convention(thin) (@guaranteed Error) -> @owned String {
// HECK: bb0([[ARG:%.*]] : $Error):
// HECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var Error }
// HECK:   [[PROJ_BOX:%.*]] = project_box [[ALLOC_OF_BOX]]
// HECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// HECK:   store [[COPY_ARG]] to [init] [[PROJ_BOX]]
// HECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PROJ_BOX]] : $*Error
// HECK:   [[LOAD_BOX:%.*]] = load [copy] [[READ]]
// HECK:   [[OPAQUE_ARG:%.*]] = open_existential_box [[LOAD_BOX]] : $Error to $*@opened({{.*}}) Error
// HECK:   [[LOAD_OPAQUE:%.*]] = load [copy] [[OPAQUE_ARG]]
// HECK:   [[ALLOC_OPEN:%.*]] = alloc_stack $@opened({{.*}}) Error
// HECK:   store [[LOAD_OPAQUE]] to [init] [[ALLOC_OPEN]]
// HECK:   [[RET_VAL:%.*]] = apply {{.*}}<@opened({{.*}}) Error>([[ALLOC_OPEN]])
// HECK:   return [[RET_VAL]] : $String
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen12propOfLValueySSs5Error_pF'
func propOfLValue(_ x: Error) -> String {
  var x = x
  genericInout(&x)
  return x._domain
}

// Test SILGenBuilder.loadCopy().
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen7lastValyxxd_tlF : $@convention(thin) <T> (@guaranteed Array<T>) -> @out T {
// HECK: [[LOAD:%.*]] = load [copy] %{{.*}} : $*T
// HECK: return [[LOAD]] : $T
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen7lastValyxxd_tlF'
func lastVal<T>(_ rest: T...) -> T {
  var minValue: T
  for value in rest {
    minValue = value
  }
  return minValue
}

// Test SILGenFunction::emitPointerToPointer.
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen3foo1pSRyxGSPyxG_tlF : $@convention(thin) <Element> (UnsafePointer<Element>) -> UnsafeBufferPointer<Element> {
// HECK: [[F:%.*]] = function_ref @$sconvertPointerToB8Argumentyq_xB0RzsABR_r0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Pointer, τ_0_1 : _Pointer> (@in_guaranteed τ_0_0) -> @out τ_0_1
// HECK: apply [[F]]<UnsafePointer<Element>, UnsafePointer<Element>>(%0) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Pointer, τ_0_1 : _Pointer> (@in_guaranteed τ_0_0) -> @out τ_0_1
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen3foo1pSRyxGSPyxG_tlF'
func foo<Element>(p: UnsafePointer<Element>) -> UnsafeBufferPointer<Element> {
  return UnsafeBufferPointer(start: p, count: 1)
}

// Test SILBuilder.createLoadBorrow.
// ---
protocol FooP {
  func foo()
}

// CHECK-LABEL: sil private [ossa] @$s20opaque_values_silgen10loadBorrowyyF4FooPL_V3foo3pos7ElementQzSg5IndexQz_tF : $@convention(method) <Elements where Elements : Collection> (@in_guaranteed Elements.Index, @inout FooP<Elements>) -> @out Optional<Elements.Element> {
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Elements.Index, [[ARG1:%.*]] : $*FooP<Elements>):
// HECK: [[READ:%.*]] = begin_access [read] [unknown] [[ARG1]] : $*FooP<Elements>
// HECK: [[LOAD:%.*]] = load [copy] [[READ]] : $*FooP<Elements>
// HECK: end_access [[READ]] : $*FooP<Elements>
// HECK: [[BORROW_LOAD:%.*]] = begin_borrow [[LOAD]]
// HECK: [[EXTRACT:%.*]] = struct_extract [[BORROW_LOAD]] : $FooP<Elements>, #<abstract function>FooP._elements
// HECK: [[COPYELT:%.*]] = copy_value [[EXTRACT]] : $Elements
// HECK: [[COPYIDX:%.*]] = copy_value [[ARG0]] : $Elements.Index
// HECK: [[WT:%.*]] = witness_method $Elements, #Collection.subscript!getter : <Self where Self : Collection> (Self) -> (Self.Index) -> Self.Element : $@convention(witness_method: Collection) <τ_0_0 where τ_0_0 : Collection> (@in_guaranteed τ_0_0.Index, @in_guaranteed τ_0_0) -> @out τ_0_0.Element
// HECK: [[RESULT:%.*]] = apply [[WT]]<Elements>([[COPYIDX]], [[COPYELT]]) : $@convention(witness_method: Collection) <τ_0_0 where τ_0_0 : Collection> (@in_guaranteed τ_0_0.Index, @in_guaranteed τ_0_0) -> @out τ_0_0.Element
// HECK: destroy_value [[COPYELT]] : $Elements
// HECK: [[ENUM_RESULT:%.*]] = enum $Optional<Elements.Element>, #Optional.some!enumelt, [[RESULT]] : $Elements.Element
// HECK: destroy_value [[LOAD]]
// CHECK-NOT: destroy_value [[ARG0]] : $Elements.Index
// HECK: return [[ENUM_RESULT]] : $Optional<Elements.Element>
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen10loadBorrowyyF4FooPL_V3foo3pos7ElementQzSg5IndexQz_tF'
func loadBorrow() {
  struct FooP<Elements : Collection> {
    internal let _elements: Elements

    public mutating func foo(pos: Elements.Index) -> Elements.Element? {
      return _elements[pos]
    }
  }
  var foo = FooP(_elements: [])
  _ = foo.foo(pos: 1)
}


// Tests LogicalPathComponent's writeback for opaque value types
// ---
// Dictionary.subscript.getter
// CHECK-LABEL: sil [always_inline] [ossa] @$sSD20opaque_values_silgenEyq_Sgq_cig : $@convention(method) <Key, Value where Key : Hashable> (@in_guaranteed Value, @guaranteed Dictionary<Key, Value>) -> @out Optional<Value> {
// HECK: bb0([[ARG0:%.*]] : $Value, [[ARG1:%.*]] : $*Dictionary<Key, Value>):
// HECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[ARG1]] : $*Dictionary<Key, Value>
// HECK:   [[OPTIONAL_ALLOC:%.*]] = alloc_stack $Optional<Value>
// HECK:   switch_enum_addr [[OPTIONAL_ALLOC]] : $*Optional<Value>, case #Optional.some!enumelt: bb2, case #Optional.none!enumelt: bb1
// HECK: bb2:
// HECK:   [[OPTIONAL_LOAD:%.*]] = load [take] [[OPTIONAL_ALLOC]] : $*Optional<Value>
// HECK:   apply {{.*}}<Key, Value>([[OPTIONAL_LOAD]], {{.*}}, [[WRITE]]) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@in Optional<τ_0_1>, @in τ_0_1, @inout Dictionary<τ_0_0, τ_0_1>) -> ()
// HECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$sSD20opaque_values_silgenEyq_Sgq_cig'

// Tests materializeForSet's createSetterCallback for opaque values
// ---
// Dictionary.subscript.setter
// CHECK-LABEL: sil [ossa] @$sSD20opaque_values_silgenEyq_Sgq_cis : $@convention(method) <Key, Value where Key : Hashable> (@in Optional<Value>, @in Value, @inout Dictionary<Key, Value>) -> () {
// HECK: bb0([[ARG0:%.*]] : $Builtin.RawPointer, [[ARG1:%.*]] : $*Builtin.UnsafeValueBuffer, [[ARG2:%.*]] : $*Dictionary<Key, Value>, [[ARG3:%.*]] : $@thick Dictionary<Key, Value>.Type):
// HECK:   [[PROJ_VAL1:%.*]] = project_value_buffer $Value in [[ARG1]] : $*Builtin.UnsafeValueBuffer
// HECK:   [[LOAD_VAL1:%.*]] = load [take] [[PROJ_VAL1]] : $*Value
// HECK:   [[ADDR_VAL0:%.*]] = pointer_to_address [[ARG0]] : $Builtin.RawPointer to [strict] $*Optional<Value>
// HECK:   [[LOAD_VAL0:%.*]] = load [take] [[ADDR_VAL0]] : $*Optional<Value>
// HECK:   apply {{.*}}<Key, Value>([[LOAD_VAL0]], [[LOAD_VAL1]], [[ARG2]]) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@in Optional<τ_0_1>, @in τ_0_1, @inout Dictionary<τ_0_0, τ_0_1>) -> ()
// HECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$sSD20opaque_values_silgenEyq_Sgq_cis'
extension Dictionary {
  public subscript(key: Value) -> Value? {
    @inline(__always)
    get {
      return key
    }
    set(newValue) {
    }
  }
  
  public mutating func inoutAccessOfSubscript(key: Value) {
    func increment(x: inout Value) { }

    increment(x: &self[key]!)
  }
}

// Test ownership of multi-case Enum values in the context of to @in thunks.
// ---
// protocol witness for static Swift.Equatable.== infix(A, A) -> Swift.Bool in conformance Swift.FloatingPointSign : Swift.Equatable
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s20opaque_values_silgen17FloatingPointSignOSQAASQ2eeoiySbx_xtFZTW : $@convention(witness_method: Equatable) (@in_guaranteed FloatingPointSign, @in_guaranteed FloatingPointSign, @thick FloatingPointSign.Type) -> Bool {
// HECK: bb0(%0 : $FloatingPointSign, %1 : $FloatingPointSign, %2 : $@thick FloatingPointSign.Type):
// HECK:   %3 = metatype $@thin FloatingPointSign.Type // user: %5
// HECK:   %4 = function_ref @$ss17FloatingPointSignO21__derived_enum_equalsySbAB_ABtFZ : $@convention(method) (FloatingPointSign, FloatingPointSign, @thin FloatingPointSign.Type) -> Bool // user: %5
// HECK:   %5 = apply %4(%0, %1, %3) : $@convention(method) (FloatingPointSign, FloatingPointSign, @thin FloatingPointSign.Type) -> Bool // user: %6
// HECK:   return %5 : $Bool
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen17FloatingPointSignOSQAASQ2eeoiySbx_xtFZTW'
public enum FloatingPointSign {
  /// The sign for a positive value.
  case plus

  /// The sign for a negative value.
  case minus
}

#if os(macOS)
// Test open_existential_value used in a conversion context.
//
// TODO: Subsequent OSSA optimization should optimize away one of both of these copies.
// ---
// CHECK-OSX-LABEL: sil [ossa] @$s20opaque_values_silgen25unsafeDowncastToAnyObject04fromG0yXlyp_tF : $@convention(thin) (@in_guaranteed Any) -> @owned AnyObject {
// CHECK-OSX: bb0(%0 : @guaranteed $Any):
// CHECK-OSX:   [[COPY:%.*]] = copy_value %0 : $Any
// CHECK-OSX:   [[BORROW2:%.*]] = begin_borrow [[COPY]] : $Any
// CHECK-OSX:   [[VAL:%.*]] = open_existential_value [[BORROW2]] : $Any to $@opened
// CHECK-OSX:   [[COPY2:%.*]] = copy_value [[VAL]] : $@opened
// CHECK-OSX:   end_borrow [[BORROW2]] : $Any
// CHECK-OSX:   [[RESULT:%.*]] = apply %{{.*}}<@opened("{{.*}}") Any>([[COPY2]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @owned AnyObject
// CHECK-OSX:   destroy_value [[COPY2]] : $@opened
// CHECK-OSX:   destroy_value [[COPY]] : $Any
// CHECK-OSX-NOT:   destroy_value %0 : $Any
// CHECK-OSX:   return [[RESULT]] : $AnyObject
// CHECK-OSX-LABEL: } // end sil function '$s20opaque_values_silgen25unsafeDowncastToAnyObject04fromG0yXlyp_tF'
public func unsafeDowncastToAnyObject(fromAny any: Any) -> AnyObject {
  return any as AnyObject
}
#endif

#if os(macOS)
// Test open_existential_box_value in a conversion context.
// ---
// CHECK-OSX-LABEL: sil [ossa] @$s20opaque_values_silgen22testOpenExistentialBox1eys5Error_pSg_tF : $@convention(thin) (@guaranteed Optional<Error>) -> () {
// CHECK-OSX: [[BORROW:%.*]] = begin_borrow [lexical] %{{.*}} : $Error
// CHECK-OSX: [[VAL:%.*]] = open_existential_box_value [[BORROW]] : $Error to $@opened
// CHECK-OSX: [[COPY:%.*]] = copy_value [[VAL]] : $@opened
// CHECK-OSX: [[ANY:%.*]] = init_existential_value [[COPY]] : $@opened
// CHECK-OSX: end_borrow [[BORROW]] : $Error
// CHECK-OSX-LABEL: } // end sil function '$s20opaque_values_silgen22testOpenExistentialBox1eys5Error_pSg_tF'
public func testOpenExistentialBox(e: Error?) {
  if let u = e {
    let a: Any = u
    _ = a
  }
}
#endif

// Test passing a +1 RValue to @in_guaranteed.
// ---
public protocol IP {}

public protocol Seq {
  associatedtype Iterator : IP

  func makeIterator() -> Iterator
}

extension Seq where Self.Iterator == Self {
  public func makeIterator() -> Self {
    return self
  }
}

public struct EnumIter<Base : IP> : IP, Seq {
  internal var _base: Base

  public typealias Iterator = EnumIter<Base>
}

// CHECK-LABEL: sil [ossa] @$s20opaque_values_silgen7EnumSeqV12makeIteratorAA0D4IterVy0G0QzGyF : $@convention(method) <Base where Base : Seq> (@in_guaranteed EnumSeq<Base>) -> @out EnumIter<Base.Iterator> {
// HECK: bb0(%0 : @guaranteed $EnumSeq<Base>):
// HECK:  [[MT:%.*]] = metatype $@thin EnumIter<Base.Iterator>.Type
// HECK:  [[FIELD:%.*]] = struct_extract %0 : $EnumSeq<Base>, #EnumSeq._base
// HECK:  [[COPY:%.*]] = copy_value [[FIELD]] : $Base
// HECK:  [[WT:%.*]] = witness_method $Base, #Seq.makeIterator : <Self where Self : Seq> (Self) -> () -> Self.Iterator : $@convention(witness_method: Seq) <τ_0_0 where τ_0_0 : Seq> (@in_guaranteed τ_0_0) -> @out τ_0_0.Iterator
// HECK:  [[ITER:%.*]] = apply [[WT]]<Base>([[COPY]]) : $@convention(witness_method: Seq) <τ_0_0 where τ_0_0 : Seq> (@in_guaranteed τ_0_0) -> @out τ_0_0.Iterator
// HECK:  destroy_value [[COPY]] : $Base
// HECK: [[FN:%.*]] = function_ref @$ss8EnumIterV5_baseAByxGx_tcfC : $@convention(method) <τ_0_0 where τ_0_0 : IP> (@in τ_0_0, @thin EnumIter<τ_0_0>.Type) -> @out EnumIter<τ_0_0>
// HECK:  [[RET:%.*]] = apply [[FN]]<Base.Iterator>([[ITER]], [[MT]]) : $@convention(method) <τ_0_0 where τ_0_0 : IP> (@in τ_0_0, @thin EnumIter<τ_0_0>.Type) -> @out EnumIter<τ_0_0>
// HECK:  return [[RET]] : $EnumIter<Base.Iterator>
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen7EnumSeqV12makeIteratorAA0D4IterVy0G0QzGyF'
public struct EnumSeq<Base : Seq> : Seq {
  public typealias Iterator = EnumIter<Base.Iterator>

  internal var _base: Base

  public func makeIterator() -> Iterator {
    return EnumIter(_base: _base.makeIterator())
  }
}

extension Collection {
  func transformEachElement<U>(_ cl: (Element) -> U) -> [U] {
    return map(cl)
  }
}

extension Array where Element == Int {
  // CHECK-LABEL: sil private [ossa] @$sSa20opaque_values_silgenSiRszlE20incrementEachElementSaySiGyFS2iXEfU_ : {{.*}} {
  // CHECK:       {{bb[0-9]+}}({{%[^,]+}} : $Int):
  // CHECK-LABEL: } // end sil function '$sSa20opaque_values_silgenSiRszlE20incrementEachElementSaySiGyFS2iXEfU_'
  func incrementEachElement() -> [Int] {
    return transformEachElement { element in
      return element + 1
    }
  }
}

// CHECK-LABEL: sil private [ossa] @$s20opaque_values_silgen22anon_read_only_captureyS2iFSiyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> Int {
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen22anon_read_only_captureyS2iFSiyXEfU_'
func anon_read_only_capture(_ x: Int) -> Int {
  var x = x
  return ({ x })()
}


// CHECK-LABEL: sil private [ossa] @$s20opaque_values_silgen22testEmptyReturnClosureyyFyycyKXEfu_yycfU_ : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <()> {
// CHECK-NOT: bb1 
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen22testEmptyReturnClosureyyFyycyKXEfu_yycfU_'
func testEmptyReturnClosure() {
  func bar() {}
  let b = nil ?? { bar() }
}

// Test that PatternMatchEmission::emitIsDispatch can emit a
// class-to-AnyObject cast as a guaranteed scalar cast
// (doesCastPreserveOwnershipForTypes returns true).
//
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen24testCastClassToAnyObjectyyXlAA1CCF : $@convention(thin) (@guaranteed C) -> @owned AnyObject {
// CHECK: bb0(%0 : @guaranteed $C):
// CHECK:   checked_cast_br %0 : $C to AnyObject, bb2, bb1
// CHECK: bb1(%{{.*}} : @guaranteed $C):
// CHECK: bb2(%{{.*}} : @guaranteed $AnyObject):
// CHECK-LABEL: } // end sil function
func testCastClassToAnyObject(_ c: C) -> AnyObject {
  switch (c) {
  case let x as AnyObject:
    _ = x
    break
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen24testCastAnyObjectToClassyAA1CCyXlF : $@convention(thin) (@guaranteed AnyObject) -> @owned C {
// CHECK: bb0(%0 : @guaranteed $AnyObject):
// CHECK:   [[CP:%.*]] = copy_value %0 : $AnyObject
// CHECK:   checked_cast_br [[CP]] : $AnyObject to C, bb1, bb2
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen24testCastAnyObjectToClassyAA1CCyXlF'
func testCastAnyObjectToClass(_ o: AnyObject) -> C {
  switch (o) {
  case let x as C:
    _ = x
    break
  default:
    break
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen024testCastClassArchetypeToF0yAA1CCxRlzClF : $@convention(thin) <T where T : AnyObject> (@guaranteed T) -> @owned C {
// CHECK: bb0(%0 : @guaranteed $T):
// CHECK:   [[CP:%.*]] = copy_value %0 : $T
// CHECK:   checked_cast_br [[CP]] : $T to C, bb1, bb2
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen024testCastClassArchetypeToF0yAA1CCxRlzClF'
func testCastClassArchetypeToClass<T : AnyObject>(_ o: T) -> C {
  switch (o) {
  case let x as C:
    _ = x
    break
  default:
    break
  }
}

class TestGeneric<T> {
  init() {}

  var generic: T
  @_borrowed
  var borrowedGeneric: T
}

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s20opaque_values_silgen11TestGenericC08borrowedE0xvr :
// CHECK: bb0(%0 : @guaranteed $TestGeneric<T>):
// CHECK:   [[REF:%.*]] = ref_element_addr %0 : $TestGeneric<T>, #TestGeneric.borrowedGeneric
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[REF]] : $*T
// CHECK:   [[LD:%.*]] = load_borrow [[ACCESS]] : $*T
// CHECK:   yield [[LD]] : $T, resume bb1, unwind bb2
// CHECK: bb1:
// CHECK:   end_borrow [[LD]] : $T
// CHECK:   end_access [[ACCESS]] : $*T
// CHECK:   [[RES:%.*]] = tuple ()
// CHECK:   return [[RES]] : $()
// CHECK: bb2:
// CHECK:   end_borrow [[LD]] : $T
// CHECK:   end_access [[ACCESS]] : $*T
// CHECK:   unwind
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen11TestGenericC08borrowedE0xvr'

// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen8TestEnumO5_codeSivg :
// CHECK: bb0(%0 : @guaranteed $TestEnum<T>):
// CHECK:   [[COPY:%.*]] = copy_value %0 : $TestEnum<T>
// CHECK:   switch_enum [[COPY]] : $TestEnum<T>, case #TestEnum.invalidValue!enumelt: bb1
// CHECK: bb1([[VAL:%.*]] : @owned $Any):
// CHECK:   destroy_value [[VAL]] : $Any
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen8TestEnumO5_codeSivg'
enum TestEnum<T> {
  case invalidValue(Any)

  var _code: Int {
     switch self {
     case .invalidValue: return 4866
     }
  }
}

