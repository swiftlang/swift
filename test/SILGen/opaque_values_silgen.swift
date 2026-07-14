// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-sil-opaque-values -Xllvm -sil-full-demangle -primary-file %s > %t/output.silgen
// RUN: %FileCheck < %t/output.silgen %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime
// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s

// Test SILGen -enable-sil-opaque-values with tests that depend on the stdlib.

class C {}

struct MyInt {
  var int: Int
}

func genericInout<T>(_: inout T) {}

func hasVarArg(_ args: Any...) {}

@_silgen_name("sink")
func sink<T>(_ t: consuming T) {}
@_silgen_name("source")
func source<T>(_ t: T.Type) -> T

// Test array initialization - we are still (somewhat) using addresses
// ---
// CHECK-LABEL: sil [ossa] @$s20opaque_values_silgen10callVarArgyyF : $@convention(thin) () -> () {
// CHECK: [[ALLOC:%[^,]+]] = apply %{{.*}}<Any>(%{{.*}}) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: ([[ARRAY:%[^,]+]], {{%[^)]+}}) = destructure_tuple [[ALLOC]] : $(Array<Any>, Builtin.RawPointer)
// CHECK: [[BORROW:%[^,]+]] = begin_borrow [[ARRAY]] : $Array<Any>
// CHECK: [[ELT_ADDR:%[^,]+]] = index_addr [projection] {{%[^,]+}} : $*Any, {{%[^,]+}}
// CHECK: [[IOPAQUE:%[^,]+]] = init_existential_value {{%[^,]+}} : $Int, $Int, $Any
// CHECK: store [[IOPAQUE]] to [init] [[ELT_ADDR]] : $*Any
// CHECK: end_borrow [[BORROW]] : $Array<Any>
// CHECK: return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen10callVarArgyyF'
public func callVarArg() {
  hasVarArg(3)
}

// Tests For-each statements
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen11forEachStmtyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[PROJ_BOX_ARG:%.*]] = project_box %{{.*}} : ${ var IndexingIterator<Range<Int>> }
// CHECK-NOT: alloc_stack $Int
// CHECK-NOT: alloc_stack $Range<Int>
// CHECK-NOT: dealloc_stack
// CHECK:   [[ITER:%.*]] = apply %{{.*}}<Range<Int>>({{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : Collection, τ_0_0.Iterator == IndexingIterator<τ_0_0>> (@in τ_0_0) -> @out IndexingIterator<τ_0_0>
// CHECK:   store [[ITER]] to [trivial] [[PROJ_BOX_ARG]]
// CHECK:   br bb1
// CHECK: bb1:
// CHECK-NOT: alloc_stack $Optional<Int>
// CHECK:   [[NEXT:%.*]] = apply %{{.*}}<Range<Int>>
// CHECK-NOT: dealloc_stack
// CHECK:   switch_enum [[NEXT]] : $Optional<Int>, case #Optional.some!enumelt: bb2, case #Optional.none!enumelt: bb3
// CHECK: bb2([[ENUM_ARG:%.*]] : $Int):
// CHECK-NOT:   unchecked_enum_data
// CHECK:   br bb1
// CHECK: bb3:
// CHECK:   return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen11forEachStmtyyF'
func forEachStmt() {
  for _ in 1..<42 {
  }
}

// Tests that existential boxes can contain opaque types
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen12openExistBoxySSs5Error_pF : $@convention(thin) (@guaranteed any Error) -> @owned String {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any Error):
// CHECK:   [[OPAQUE_ARG:%.*]] = open_existential_box_value [[ARG]] : $any Error to $@opened({{.*}}, any Error) Self
// CHECK:   [[ALLOC_OPEN:%.*]] = alloc_stack $@opened({{.*}}, any Error) Self
// CHECK:   [[COPY:%.*]] = copy_value [[OPAQUE_ARG]]
// CHECK:   store [[COPY]] to [init] [[ALLOC_OPEN]]
// CHECK:   destroy_addr [[ALLOC_OPEN]]
// CHECK:   dealloc_stack [[ALLOC_OPEN]]
// CHECK-NOT:   destroy_value [[ARG]] : $any Error
// CHECK:   return {{.*}} : $String
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen12openExistBoxySSs5Error_pF'
func openExistBox(_ x: Error) -> String {
  return x._domain
}

// Tests conditional value casts and correspondingly generated reabstraction thunk
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen11condFromAnyyyypF : $@convention(thin) (@in_guaranteed Any) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Any):
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   checked_cast_br Any in [[COPY_ARG]] : $Any to (Int, (Int, (Int, Int)), Int) -> (Int, (Int, (Int, Int)), Int), [[SUCCESS:bb[0-9]+]], {{bb[0-9]+}}
// CHECK: [[SUCCESS]]([[CAST:%.*]] : @owned $@callee_guaranteed @substituted <{{.*}}> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1, @in_guaranteed τ_0_2) -> @out τ_0_3 for <{{.*}}>):
// CHECK:   [[CONVERTED:%.*]] = convert_function [[CAST]] {{.*}} to $@callee_guaranteed (@in_guaranteed Int, @in_guaranteed (Int, (Int, Int)), @in_guaranteed Int) -> @out (Int, (Int, (Int, Int)), Int)
// CHECK:   [[THUNK_REF:%.*]] = function_ref @{{.*}} : $@convention(thin) (Int, Int, Int, Int, Int, @guaranteed @callee_guaranteed (@in_guaranteed Int, @in_guaranteed (Int, (Int, Int)), @in_guaranteed Int) -> @out (Int, (Int, (Int, Int)), Int)) -> (Int, Int, Int, Int, Int)
// CHECK:   partial_apply [callee_guaranteed] [[THUNK_REF]]([[CONVERTED]])
// CHECK:   return %{{.*}} : $()
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

// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen10addrOnlyIf1xAA6EmptyP_pSb_tF : $@convention(thin) (Bool) -> @out any EmptyP {
// CHECK: bb0([[ARG:%.*]] : $Bool):
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var any EmptyP }, var
// CHECK:   [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[ALLOC_OF_BOX]]
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[BOX_LIFETIME]]
// CHECK:   [[APPLY_FOR_BOX:%.*]] = apply %{{.*}}(%{{.*}}) : $@convention(method) (@thin AddressOnlyStruct.Type) -> AddressOnlyStruct
// CHECK:   [[INIT_OPAQUE:%.*]] = init_existential_value [[APPLY_FOR_BOX]] : $AddressOnlyStruct, $AddressOnlyStruct, $any EmptyP
// CHECK:   store [[INIT_OPAQUE]] to [init] [[PROJ_BOX]] : $*any EmptyP
// CHECK:   [[COND:%.*]] = struct_extract [[ARG]] : $Bool, #Bool._value
// CHECK:   cond_br [[COND]], bb1, bb2
// CHECK: bb1:
// CHECK:   [[READ1:%.*]] = begin_access [read] [unknown] [[PROJ_BOX]] : $*any EmptyP
// CHECK:   [[RETVAL1:%.*]] = load [copy] [[READ1]] : $*any EmptyP
// CHECK:   br bb3([[RETVAL1]] : $any EmptyP)
// CHECK: bb2:
// CHECK:   [[READ2:%.*]] = begin_access [read] [unknown] [[PROJ_BOX]] : $*any EmptyP
// CHECK:   [[RETVAL2:%.*]] = load [copy] [[READ2]] : $*any EmptyP
// CHECK:   br bb3([[RETVAL2]] : $any EmptyP)
// CHECK: bb3([[RETVAL:%.*]] : @owned $any EmptyP):
// CHECK:   end_borrow [[BOX_LIFETIME]]
// CHECK:   destroy_value [[ALLOC_OF_BOX]]
// CHECK:   return [[RETVAL]] : $any EmptyP
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen10addrOnlyIf1xAA6EmptyP_pSb_tF'
func addrOnlyIf(x: Bool) -> EmptyP {
  var a : EmptyP = AddressOnlyStruct()
  genericInout(&a)
  return x ? a : a
}

// Tests LValue of error types / existential boxes
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen12propOfLValueySSs5Error_pF : $@convention(thin) (@guaranteed any Error) -> @owned String {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any Error):
// CHECK:   [[ALLOC_OF_BOX:%.*]] = alloc_box ${ var any Error }
// CHECK:   [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[ALLOC_OF_BOX]]
// CHECK:   [[PROJ_BOX:%.*]] = project_box [[BOX_LIFETIME]]
// CHECK:   [[COPY_ARG:%.*]] = copy_value [[ARG]]
// CHECK:   store [[COPY_ARG]] to [init] [[PROJ_BOX]]
// CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PROJ_BOX]] : $*any Error
// CHECK:   [[LOAD_BOX:%.*]] = load [copy] [[READ]]
// CHECK:   [[BORROW_BOX:%.*]] = begin_borrow [[LOAD_BOX]] : $any Error
// CHECK:   [[OPAQUE_ARG:%.*]] = open_existential_box [[BORROW_BOX]] : $any Error to $*@opened({{.*}}, any Error) Self
// CHECK:   [[LOAD_OPAQUE:%.*]] = load [copy] [[OPAQUE_ARG]]
// CHECK:   [[ALLOC_OPEN:%.*]] = alloc_stack $@opened({{.*}}, any Error) Self
// CHECK:   store {{%[^,]+}} to [init] [[ALLOC_OPEN]]
// CHECK:   [[RET_VAL:%.*]] = apply {{.*}}<@opened({{.*}}, any Error) Self>([[ALLOC_OPEN]])
// CHECK:   return [[RET_VAL]] : $String
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen12propOfLValueySSs5Error_pF'
func propOfLValue(_ x: Error) -> String {
  var x = x
  genericInout(&x)
  return x._domain
}

// Test SILGenBuilder.loadCopy().
// ---
// CHECK-LABEL: sil hidden [ossa] @$s20opaque_values_silgen7lastValyxxd_tlF : $@convention(thin) <T> (@guaranteed Array<T>) -> @out T {
// CHECK: [[LOAD:%.*]] = load [copy] %{{.*}} : $*T
// CHECK: return [[LOAD]] : $T
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
// CHECK: [[F:%.*]] = function_ref @{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Pointer, τ_0_1 : _Pointer> (@in_guaranteed τ_0_0) -> @out τ_0_1
// CHECK: apply [[F]]<UnsafePointer<Element>, UnsafePointer<Element>>(%0) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Pointer, τ_0_1 : _Pointer> (@in_guaranteed τ_0_0) -> @out τ_0_1
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
// CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[ARG1]] : $*FooP<Elements>
// CHECK: [[LOAD:%.*]] = load [copy] [[READ]] : $*FooP<Elements>
// CHECK: end_access [[READ]] : $*FooP<Elements>
// CHECK: [[BORROW_LOAD:%.*]] = begin_borrow [[LOAD]]
// CHECK: [[EXTRACT:%.*]] = struct_extract [[BORROW_LOAD]] : $FooP<Elements>, #<abstract function>FooP._elements
// CHECK: [[COPYELT:%.*]] = copy_value [[EXTRACT]] : $Elements
// CHECK: [[COPYIDX:%.*]] = copy_value [[ARG0]] : $Elements.Index
// CHECK: [[BORROWELT:%.*]] = begin_borrow [[COPYELT]] : $Elements
// CHECK: [[BORROWIDX:%.*]] = begin_borrow [[COPYIDX]] : $Elements.Index
// CHECK: [[WT:%.*]] = witness_method $Elements, #Collection.subscript!read : <Self where Self : Collection> (Self) -> (Self.Index) -> () : $@yield_once @convention(witness_method: Collection) <τ_0_0 where τ_0_0 : Collection> (@in_guaranteed τ_0_0.Index, @in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_0.Element
// CHECK: ([[YIELD:%.*]], [[TOKEN:%.*]]) = begin_apply [[WT]]<Elements>([[BORROWIDX]], [[BORROWELT]]) : $@yield_once @convention(witness_method: Collection) <τ_0_0 where τ_0_0 : Collection> (@in_guaranteed τ_0_0.Index, @in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_0.Element
// CHECK: [[RESULT:%.*]] = copy_value [[YIELD]] : $Elements.Element
// CHECK: end_apply [[TOKEN]]
// CHECK: destroy_value [[COPYELT]] : $Elements
// CHECK: [[ENUM_RESULT:%.*]] = enum $Optional<Elements.Element>, #Optional.some!enumelt, [[RESULT]] : $Elements.Element
// CHECK-NOT: destroy_value [[ARG0]] : $Elements.Index
// CHECK: return [[ENUM_RESULT]] : $Optional<Elements.Element>
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


// Tests LogicalPathComponent's writeback for opaque value types.
//
// The subscript getter/setter are now trivial (the writeback orchestration that
// materializeForSet used to perform lives in the caller, inoutAccessOfSubscript
// below). They serve here to confirm opaque values flow through the subscript
// accessors directly rather than via addresses.
// ---
// Dictionary.subscript.getter
// CHECK-LABEL: sil [heuristic_always_inline] [ossa] @$sSD20opaque_values_silgenEyq_Sgq_cig : $@convention(method) <Key, Value where Key : Hashable> (@in_guaranteed Value, @guaranteed Dictionary<Key, Value>) -> @out Optional<Value> {
// CHECK: bb0([[KEY:%.*]] : @guaranteed $Value, {{%.*}} : @guaranteed $Dictionary<Key, Value>):
// CHECK:   [[COPY:%.*]] = copy_value [[KEY]] : $Value
// CHECK:   [[SOME:%.*]] = enum $Optional<Value>, #Optional.some!enumelt, [[COPY]] : $Value
// CHECK:   return [[SOME]] : $Optional<Value>
// CHECK-LABEL: } // end sil function '$sSD20opaque_values_silgenEyq_Sgq_cig'

// Dictionary.subscript.setter
// CHECK-LABEL: sil [ossa] @$sSD20opaque_values_silgenEyq_Sgq_cis : $@convention(method) <Key, Value where Key : Hashable> (@in Optional<Value>, @in Value, @inout Dictionary<Key, Value>) -> () {
// CHECK: bb0([[NEWVALUE:%.*]] : @owned $Optional<Value>, [[KEY:%.*]] : @owned $Value, {{%.*}} : $*Dictionary<Key, Value>):
// CHECK:   destroy_value [[KEY]] : $Value
// CHECK:   destroy_value [[NEWVALUE]] : $Optional<Value>
// CHECK:   return
// CHECK-LABEL: } // end sil function '$sSD20opaque_values_silgenEyq_Sgq_cis'

// The actual writeback: read through the getter into a temporary, mutate the
// in-place payload, then store it back through the setter. The optionals are
// opaque values stored to/loaded from the temporary; no value is materialized
// to an address except for the temporary itself.
// CHECK-LABEL: sil {{.*}}[ossa] @$sSD20opaque_values_silgenE22inoutAccessOfSubscript3keyyq__tF : $@convention(method) <Key, Value where Key : Hashable> (@in_guaranteed Value, @inout Dictionary<Key, Value>) -> () {
// CHECK:       bb0({{%.*}} : @guaranteed $Value, [[SELF:%.*]] : $*Dictionary<Key, Value>):
// CHECK:         [[WRITE:%.*]] = begin_access [modify] [unknown] [[SELF]] : $*Dictionary<Key, Value>
// CHECK:         [[OPTIONAL_ALLOC:%.*]] = alloc_stack $Optional<Value>
// CHECK:         [[GETTER:%.*]] = function_ref @$sSD20opaque_values_silgenEyq_Sgq_cig
// CHECK:         [[OPTIONAL:%.*]] = apply [[GETTER]]<Key, Value>({{.*}}) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@in_guaranteed τ_0_1, @guaranteed Dictionary<τ_0_0, τ_0_1>) -> @out Optional<τ_0_1>
// CHECK:         store [[OPTIONAL]] to [init] [[OPTIONAL_ALLOC]] : $*Optional<Value>
// CHECK:         switch_enum_addr [[OPTIONAL_ALLOC]] : $*Optional<Value>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: {{bb[0-9]+}}
// CHECK:       [[SOME_BB]]:
// CHECK:         [[PAYLOAD:%.*]] = unchecked_inplace_enum_data_addr [[OPTIONAL_ALLOC]] : $*Optional<Value>, #Optional.some!enumelt
// CHECK:         [[INCREMENT:%.*]] = function_ref @{{.*}}increment
// CHECK:         apply [[INCREMENT]]<Key, Value>([[PAYLOAD]])
// CHECK:         [[WRITTEN:%.*]] = load [take] [[OPTIONAL_ALLOC]] : $*Optional<Value>
// CHECK:         [[SETTER:%.*]] = function_ref @$sSD20opaque_values_silgenEyq_Sgq_cis
// CHECK:         apply [[SETTER]]<Key, Value>([[WRITTEN]], {{.*}}, [[WRITE]]) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@in Optional<τ_0_1>, @in τ_0_1, @inout Dictionary<τ_0_0, τ_0_1>) -> ()
// CHECK:         end_access [[WRITE]] : $*Dictionary<Key, Value>
// CHECK-LABEL: } // end sil function '$sSD20opaque_values_silgenE22inoutAccessOfSubscript3keyyq__tF'
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
// CHECK: bb0(%0 : $FloatingPointSign, %1 : $FloatingPointSign, %2 : $@thick FloatingPointSign.Type):
// CHECK:   [[META:%[^,]+]] = metatype $@thin FloatingPointSign.Type
// CHECK:   [[FN:%[^,]+]] = function_ref @$s20opaque_values_silgen17FloatingPointSignO21__derived_enum_equalsySbAC_ACtFZ : $@convention(method) (FloatingPointSign, FloatingPointSign, @thin FloatingPointSign.Type) -> Bool
// CHECK:   [[RESULT:%[^,]+]] = apply [[FN]](%0, %1, [[META]]) : $@convention(method) (FloatingPointSign, FloatingPointSign, @thin FloatingPointSign.Type) -> Bool
// CHECK:   return [[RESULT]] : $Bool
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
// CHECK-OSX:   [[RESULT:%.*]] = apply %{{.*}}<@opened("{{.*}}", Any) Self>([[COPY2]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @owned AnyObject
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
// CHECK-OSX: [[BORROW:%.*]] = begin_borrow [lexical] [var_decl] %{{.*}} : $any Error
// CHECK-OSX: [[VAL:%.*]] = open_existential_box_value [[BORROW]] : $any Error to $@opened
// CHECK-OSX: [[COPY:%.*]] = copy_value [[VAL]] : $@opened
// CHECK-OSX: [[ANY:%.*]] = init_existential_value [[COPY]] : $@opened
// CHECK-OSX: end_borrow [[BORROW]] : $any Error
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
// CHECK: bb0([[SELF:%.*]] : @guaranteed $EnumSeq<Base>):
// CHECK:  [[MT:%.*]] = metatype $@thin EnumIter<Base.Iterator>.Type
// CHECK:  [[FIELD:%.*]] = struct_extract [[SELF]] : $EnumSeq<Base>, #EnumSeq._base
// CHECK:  [[COPY:%.*]] = copy_value [[FIELD]] : $Base
// CHECK:  [[WT:%.*]] = witness_method $Base, #Seq.makeIterator : <Self where Self : Seq> (Self) -> () -> Self.Iterator : $@convention(witness_method: Seq) <τ_0_0 where τ_0_0 : Seq> (@in_guaranteed τ_0_0) -> @out τ_0_0.Iterator
// CHECK:  [[ITER:%.*]] = apply [[WT]]<Base>([[COPY]]) : $@convention(witness_method: Seq) <τ_0_0 where τ_0_0 : Seq> (@in_guaranteed τ_0_0) -> @out τ_0_0.Iterator
// CHECK:  destroy_value [[COPY]] : $Base
// CHECK: [[FN:%.*]] = function_ref @$s20opaque_values_silgen8EnumIterV5_baseACyxGx_tcfC : $@convention(method) <τ_0_0 where τ_0_0 : IP> (@in τ_0_0, @thin EnumIter<τ_0_0>.Type) -> @out EnumIter<τ_0_0>
// CHECK:  [[RET:%.*]] = apply [[FN]]<Base.Iterator>([[ITER]], [[MT]]) : $@convention(method) <τ_0_0 where τ_0_0 : IP> (@in τ_0_0, @thin EnumIter<τ_0_0>.Type) -> @out EnumIter<τ_0_0>
// CHECK:  return [[RET]] : $EnumIter<Base.Iterator>
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen7EnumSeqV12makeIteratorAA0D4IterVy0G0QzGyF'
public struct EnumSeq<Base : Seq> : Seq {
  public typealias Iterator = EnumIter<Base.Iterator>

  internal var _base: Base

  public func makeIterator() -> Iterator {
    return EnumIter(_base: _base.makeIterator())
  }
}

extension Collection {
  func myMap<T>(_ body: (Element) -> T) -> [T] {
    var result = [T]()
    for element in self {
      result.append(body(element))
    }
    return result
  }

  func transformEachElement<U>(_ cl: (Element) -> U) -> [U] {
    return myMap(cl)
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
// CHECK:   checked_cast_br C in %0 : $C to AnyObject, bb2, bb1
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
// CHECK:   checked_cast_br AnyObject in [[CP]] : $AnyObject to C, bb1, bb2
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
// CHECK:   checked_cast_br T in [[CP]] : $T to C, bb1, bb2
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

public enum EnumWithTwoSameAddressOnlyPayloads<T> {
  case nope
  case yes(T)
  case and(T)

// CHECK-LABEL: sil [ossa] @EnumWithTwoSameAddressOnlyPayloads_getPayload : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[INSTANCE:%[^,]+]] :
// CHECK:         [[RESULT_STORAGE:%[^,]+]] = alloc_stack $T
// CHECK:         [[COPY:%[^,]+]] = copy_value [[INSTANCE]]
// CHECK:         switch_enum [[COPY]] : $EnumWithTwoSameAddressOnlyPayloads<T>, 
// CHECK-SAME:      case #EnumWithTwoSameAddressOnlyPayloads.nope!enumelt: {{bb[0-9]+}}, 
// CHECK-SAME:      case #EnumWithTwoSameAddressOnlyPayloads.yes!enumelt: [[YES_BLOCK:bb[0-9]+]], 
// CHECK-SAME:      case #EnumWithTwoSameAddressOnlyPayloads.and!enumelt: [[AND_BLOCK:bb[0-9]+]]
// CHECK:       [[YES_BLOCK]]([[YES_VALUE:%[^,]+]] :
// CHECK:         [[YES_LIFETIME:%[^,]+]] = move_value [lexical] [var_decl] [[YES_VALUE]]
// CHECK:         [[YES_COPY:%[^,]+]] = copy_value [[YES_LIFETIME]]
// CHECK:         store [[YES_COPY]] to [init] [[RESULT_STORAGE]]
// CHECK:       [[AND_BLOCK]]([[AND_VALUE:%[^,]+]] :
// CHECK:         [[AND_LIFETIME:%[^,]+]] = move_value [lexical] [var_decl] [[AND_VALUE]]
// CHECK:         [[AND_COPY:%[^,]+]] = copy_value [[AND_LIFETIME]]
// CHECK:         store [[AND_COPY]] to [init] [[RESULT_STORAGE]]
// CHECK-LABEL: } // end sil function 'EnumWithTwoSameAddressOnlyPayloads_getPayload'
  public var getPayload: T? {
    @_silgen_name("EnumWithTwoSameAddressOnlyPayloads_getPayload")
    get {
    switch self {
      case .nope:
        return nil
      case .yes(let t), .and(let t):
        return t
    }
    }
  }
}


// Verify exit block arguments are ordered correctly.
// 
// CHECK-LABEL: sil private [ossa] @$s20opaque_values_silgen19duplicate_with_int49condition5valueSi_S2ix_xttSb_xtlFSi_S2ix_xttyXEfU_ : {{.*}} {
// CHECK:         br [[EPILOG:bb[0-9]+]]({{%[^,]+}} : $Int, {{%[^,]+}} : $Int, {{%[^,]+}} : $Int, {{%[^,]+}} : $Value, {{%[^,]+}} : $Value)
// CHECK:         br [[EPILOG]]({{%[^,]+}} : $Int, {{%[^,]+}} : $Int, {{%[^,]+}} : $Int, {{%[^,]+}} : $Value, {{%[^,]+}} : $Value)
// CHECK:       [[EPILOG]]({{%[^,]+}} : $Int, {{%[^,]+}} : $Int, {{%[^,]+}} : $Int, {{%[^,]+}} : @owned $Value, {{%[^,]+}} : @owned $Value):
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen19duplicate_with_int49condition5valueSi_S2ix_xttSb_xtlFSi_S2ix_xttyXEfU_'
func doit<T>(_ f: () -> T) -> T {
  f()
}
@_silgen_name("duplicate_with_int4")
func duplicate_with_int4<Value>(condition: Bool, value: Value) -> (Int, Int, Int, (Value, Value)) {
  doit {
    if condition {
      return (Int(), Int(), Int(), (value, value))
    } else {
      return (Int(), Int(), Int(), (value, value))
    }
  }
}

// Verify tuple rebuilding.
// CHECK-LABEL: sil private [ossa] @$s20opaque_values_silgen10duplicate15valuex_xtx_tlFx_xtyXEfU_ : {{.*}} {
// CHECK:         [[RETVAL:%[^,]+]] = tuple ({{%[^,]+}} : $Value, {{%[^,]+}} : $Value)
// CHECK:         return [[RETVAL]] : $(Value, Value)
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen10duplicate15valuex_xtx_tlFx_xtyXEfU_'
@_silgen_name("duplicate1")
func duplicate1<Value>(value: Value) -> (Value, Value) {
  doit { 
      (value, value)
  }
}
// CHECK-LABEL: sil private [ossa] @$s20opaque_values_silgen10duplicate25valuex3one_x3twotx_tlFxAD_xAEtyXEfU_ : {{.*}} {
// CHECK:         [[RETVAL:%[^,]+]] = tuple $(one: Value, two: Value) ({{%[^,]+}}, {{%[^,]+}})   
// CHECK:         return [[RETVAL]] : $(one: Value, two: Value)           
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen10duplicate25valuex3one_x3twotx_tlFxAD_xAEtyXEfU_'
@_silgen_name("duplicate2")
func duplicate2<Value>(value: Value) -> (one: Value, two: Value) {
  doit { 
      (one: value, two: value)
  }
}
// CHECK-LABEL: sil private [ossa] @$s20opaque_values_silgen19duplicate_with_int15valuex_xSitx_tlFx_xSityXEfU_ : {{.*}} {
// CHECK:         [[RETVAL:%[^,]+]] = tuple ({{%[^,]+}} : $Value, {{%[^,]+}} : $Value, {{%[^,]+}} : $Int)
// CHECK:         return [[RETVAL]]
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen19duplicate_with_int15valuex_xSitx_tlFx_xSityXEfU_'
@_silgen_name("duplicate_with_int1")
func duplicate_with_int1<Value>(value: Value) -> (Value, Value, Int) {
  doit {
    (value, value, 42)
  }
}

// CHECK-LABEL: sil private [ossa] @$s20opaque_values_silgen19duplicate_with_int25valuex_xt_Sitx_tlFx_xt_SityXEfU_ : {{.*}} {
// CHECK:         [[INNER:%[^,]+]] = tuple ({{%[^,]+}} : $Value, {{%[^,]+}} : $Value)           
// CHECK:         [[RETVAL:%[^,]+]] = tuple ([[INNER]] : $(Value, Value), {{%[^,]+}} : $Int)    
// CHECK:         return [[RETVAL]]
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen19duplicate_with_int25valuex_xt_Sitx_tlFx_xt_SityXEfU_'
@_silgen_name("duplicate_with_int2")
func duplicate_with_int2<Value>(value: Value) -> ((Value, Value), Int) {
  doit {
    ((value, value), 42)
  }
}
// CHECK-LABEL: sil private [ossa] @$s20opaque_values_silgen19duplicate_with_int35valueSi_x_x_x_SitxttSitx_tlFSi_x_x_x_SitxttSityXEfU_ : {{.*}} {
// CHECK:         [[INNERMOST:%[^,]+]] = tuple ({{%[^,]+}} : $Value, {{%[^,]+}} : $Int)           
// CHECK:         [[INNERMIDDLE:%[^,]+]] = tuple ({{%[^,]+}} : $Value, [[INNERMOST]] : $(Value, Int), {{%[^,]+}} : $Value) 
// CHECK:         [[INNERLEAST:%[^,]+]] = tuple ({{%[^,]+}} : $Value, [[INNERMIDDLE]] : $(Value, (Value, Int), Value)) 
// CHECK:         [[RETVAL:%[^,]+]] = tuple ({{%[^,]+}} : $Int, [[INNERLEAST]] : $(Value, (Value, (Value, Int), Value)), {{%[^,]+}} : $Int) 
// CHECK:         return [[RETVAL]] : $(Int, (Value, (Value, (Value, Int), Value)), Int) 
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen19duplicate_with_int35valueSi_x_x_x_SitxttSitx_tlFSi_x_x_x_SitxttSityXEfU_'
@_silgen_name("duplicate_with_int3")
func duplicate_with_int3<Value>(value: Value) -> (Int, (Value, (Value, (Value, Int), Value)), Int) {
  doit {
    (42, (value, (value, (value, 43), value)), 44)
  }
}

// Keypaths

indirect enum IndirectEnumWithAReadableIntProperty {
// CHECK-LABEL: sil {{.*}}@$s20opaque_values_silgen36IndirectEnumWithAReadableIntPropertyO1iSivpACTK : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[INSTANCE:%[^,]+]] :
// TODO: Eliminate this copy.
// CHECK:         [[COPY:%[^,]+]] = copy_value [[INSTANCE]]
// CHECK:         [[LIFETIME:%[^,]+]] = begin_borrow [[COPY]]
// CHECK:         [[FN:%[^,]+]] = function_ref @$s20opaque_values_silgen36IndirectEnumWithAReadableIntPropertyO1iSivg : $@convention(method) (@guaranteed IndirectEnumWithAReadableIntProperty) -> Int
// CHECK:         [[RESULT:%[^,]+]] = apply [[FN]]([[LIFETIME]])
// CHECK:         end_borrow [[LIFETIME]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return [[RESULT]]
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen36IndirectEnumWithAReadableIntPropertyO1iSivpACTK'
  var i: Int { 0 }
}

func takeReadableIntKeyPath<T>(_ kp: KeyPath<T, Int>) {
}

func giveReadableIntKeyPathInt() {
  takeReadableIntKeyPath(\IndirectEnumWithAReadableIntProperty.i)
}

indirect enum StructWithAReadableStringProperty {
// CHECK-LABEL: sil {{.*}}@$s20opaque_values_silgen33StructWithAReadableStringPropertyO1sSSvpACTK : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[INSTANCE:%[^,]+]] :
// CHECK:         [[COPY:%[^,]+]] = copy_value [[INSTANCE]]
// CHECK:         [[LIFETIME:%[^,]+]] = begin_borrow [[COPY]]
// CHECK:         [[FN:%[^,]+]] = function_ref @$s20opaque_values_silgen33StructWithAReadableStringPropertyO1sSSvg : $@convention(method) (@guaranteed StructWithAReadableStringProperty) -> @owned String 
// CHECK:         [[RESULT:%[^,]+]] = apply [[FN]]([[LIFETIME]])
// CHECK:         end_borrow [[LIFETIME]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return [[RESULT]]
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen33StructWithAReadableStringPropertyO1sSSvpACTK'
  var s: String { "howdy" }
}

func takeKeyPathString<T>(_ kp: KeyPath<T, String>) {
}

func giveKeyPathString() {
  takeKeyPathString(\StructWithAReadableStringProperty.s)
}

#if os(macOS)
// CHECK-OSX-LABEL: sil {{.*}}@$s20opaque_values_silgen29backDeployingReturningGenericyxxKlFTwb : {{.*}} <T> {{.*}} {
// Ensure that there aren't any "normal" (in the sense of try_apply) blocks that
// take unbound generic parameters (τ_0_0).
// CHECK-OSX-NOT: {{bb[0-9]+}}({{%[^,]+}} : @owned $τ_0_0):
// CHECK-OSX-LABEL: } // end sil function '$s20opaque_values_silgen29backDeployingReturningGenericyxxKlFTwb'
@available(SwiftStdlib 5.1, *)
@backDeployed(before: SwiftStdlib 5.8)
public func backDeployingReturningGeneric<T>(_ t: T) throws -> T { t }
#endif

// CHECK-LABEL: sil {{.*}}[ossa] @SetIntoContainerAtKeyPath : {{.*}} {
// CHECK:       bb0([[CONTAINER_ADDR:%[^,]+]] : {{.*}}, [[KP:%[^,]+]] : {{.*}}, [[VALUE:%[^,]+]] :
// CHECK:         [[KP_COPY:%[^,]+]] = copy_value [[KP]] : $WritableKeyPath<Container, Field>
// CHECK:         [[VALUE_COPY:%[^,]+]] = copy_value [[VALUE]] : $Field
// CHECK:         [[CONTAINER_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[CONTAINER_ADDR]] : $*Container
// CHECK:         [[SETTER:%[^,]+]] = function_ref @swift_setAtWritableKeyPath
// CHECK:         apply [[SETTER]]<Container, Field>([[CONTAINER_ACCESS]], [[KP_COPY]], [[VALUE_COPY]])
// CHECK:         end_access [[CONTAINER_ACCESS]] : $*Container
// CHECK:         destroy_value [[KP_COPY]]
// CHECK-LABEL: } // end sil function 'SetIntoContainerAtKeyPath'
@_silgen_name("SetIntoContainerAtKeyPath")
func set<Container, Field>(into container: inout Container, at keyPath: WritableKeyPath<Container, Field>, _ value: Field) {
  container[keyPath: keyPath] = value
}

// CHECK-LABEL: sil {{.*}}[ossa] @$s20opaque_values_silgen16FormClassKeyPathyyF1QL_C1qSivpADTk : {{.*}} {
// CHECK:       bb0([[VALUE:%[^,]+]] :
// CHECK-SAME:      [[CONTAINER:%[^,]+]] :
// CHECK:         [[CONTAINER_COPY:%[^,]+]] = copy_value [[CONTAINER]]
// CHECK:         [[SETTER:%[^,]+]] = class_method [[CONTAINER_COPY]]
// CHECK:         apply [[SETTER]]([[VALUE]], [[CONTAINER_COPY]])
// CHECK:         destroy_value [[CONTAINER_COPY]]
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen16FormClassKeyPathyyF1QL_C1qSivpADTk'
@_silgen_name("FormClassKeyPath")
func FormClassKeyPath() {
  class Q {
    var q: Int = 0
  }
  _ = \Q.q
}

// CHECK-LABEL: sil {{.*}}[ossa] @UseGetterOnInout : {{.*}} {
// CHECK:       bb0([[CONTAINER_ADDR:%[^,]+]] :
// CHECK:         [[KEYPATH:%[^,]+]] = keypath $WritableKeyPath<MyInt, Int>, (root $MyInt; stored_property #MyInt.int : $Int)
// CHECK:         [[CONTAINER_ACCESS:%[^,]+]] = begin_access [read] [unknown] [[CONTAINER_ADDR]]
// CHECK:         [[KEYPATH_UP:%[^,]+]] = upcast [[KEYPATH]]
// CHECK:         [[CONTAINER:%[^,]+]] = load [trivial] [[CONTAINER_ACCESS]]
// CHECK:         [[GETTER:%[^,]+]] = function_ref @swift_getAtKeyPath
// CHECK:         [[VALUE:%[^,]+]] = apply [[GETTER]]<MyInt, Int>([[CONTAINER]], [[KEYPATH_UP]])
// CHECK:         end_access [[CONTAINER_ACCESS]]
// CHECK:         destroy_value [[KEYPATH_UP]]
// CHECK:         return [[VALUE]] : $Int                                
// CHECK-LABEL: } // end sil function 'UseGetterOnInout'
@_silgen_name("UseGetterOnInout")
func getInout(_ i: inout MyInt) -> Int {
  return i[keyPath: \MyInt.int]
}

protocol IntValuable {
  var value: Int { get }
}
// CHECK-LABEL: sil {{.*}}[ossa] @getFromSubscriptOnLValueArray : {{.*}} {
// CHECK:       bb0([[ARRAY_ADDR:%[^,]+]] :
// CHECK:         [[ARRAY_ACCESS:%[^,]+]] = begin_access [read] [unknown] [[ARRAY_ADDR]]
// CHECK:         [[ARRAY:%[^,]+]] = load_borrow [[ARRAY_ACCESS]]
// CHECK:         [[SUBSCRIPT:%[^,]+]] = function_ref @$sSayxSicig
// CHECK:         [[ELEMENT:%[^,]+]] = apply [[SUBSCRIPT]]<any IntValuable>({{%[^,]+}}, [[ARRAY]])
// CHECK:         end_borrow [[ARRAY]]
// CHECK:         [[ELEMENT_LIFETIME:%[^,]+]] = begin_borrow [[ELEMENT]]
// CHECK:         [[OPEN_ELEMENT:%[^,]+]] = open_existential_value [[ELEMENT_LIFETIME]]
// CHECK:         [[OPEN_ELEMENT_COPY:%[^,]+]] = copy_value [[OPEN_ELEMENT]]
// CHECK:         end_access [[ARRAY_ACCESS]]
// CHECK:         [[OPEN_ELEMENT_LIFETIME:%[^,]+]] = begin_borrow [[OPEN_ELEMENT_COPY]]
// CHECK:         [[GETTER:%[^,]+]] = witness_method {{.*}}, #IntValuable.value!getter
// CHECK:         apply [[GETTER]]<{{.*}}>([[OPEN_ELEMENT_LIFETIME]])
// CHECK:         end_borrow [[OPEN_ELEMENT_LIFETIME]]
// CHECK:         destroy_value [[OPEN_ELEMENT_COPY]]
// CHECK:         end_borrow [[ELEMENT_LIFETIME]]
// CHECK:         destroy_value [[ELEMENT]]
// CHECK-LABEL: } // end sil function 'getFromSubscriptOnLValueArray'
@_silgen_name("getFromSubscriptOnLValueArray")
func getFromSubscriptOnLValueArray(_ array: inout [IntValuable]) {
  _ = array[0].value
}

protocol MutatingFooable {
  mutating func foo()
}

// CHECK-LABEL: sil {{.*}}@callMutatingFooOnInoutExistential : {{.*}} {
// CHECK:       bb0([[EXISTENTIAL_ADDR:%[^,]+]] :
// CHECK:         [[EXISTENTIAL_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[EXISTENTIAL_ADDR]]
// CHECK:         [[OPEN_ADDR:%[^,]+]] = open_existential_addr mutable_access [[EXISTENTIAL_ACCESS]]
// CHECK:         [[FOO:%[^,]+]] = witness_method {{.*}}#MutatingFooable.foo
// CHECK:         apply [[FOO]]<{{.*}}>([[OPEN_ADDR]])
// CHECK:         end_access [[EXISTENTIAL_ACCESS]]
// CHECK-LABEL: } // end sil function 'callMutatingFooOnInoutExistential'
@_silgen_name("callMutatingFooOnInoutExistential")
func callMutatingFooOnInoutExistential(_ i: inout any MutatingFooable) {
  i.foo()
}

// CHECK-LABEL: sil {{.*}}[ossa] @$s20opaque_values_silgen7WeakBoxV1txSgvg : {{.*}} {
// CHECK:       bb0([[INSTANCE:%[^,]+]] :
// CHECK:         [[WEAK_OPTIONAL:%[^,]+]] = struct_extract [[INSTANCE]]
// CHECK:         [[STRONG_OPTIONAL:%[^,]+]] = strong_copy_weak_value [[WEAK_OPTIONAL]]
// CHECK:         return [[STRONG_OPTIONAL]]
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen7WeakBoxV1txSgvg'
// CHECK-LABEL: sil {{.*}}[ossa] @$s20opaque_values_silgen7WeakBoxV1tACyxGxSg_tcfC : {{.*}} {
// CHECK:       bb0([[STRONG_OPTIONAL:%[^,]+]] :
// CHECK:         [[WEAK_OPTIONAL:%[^,]+]] = weak_copy_value [[STRONG_OPTIONAL]]
// CHECK:         destroy_value [[STRONG_OPTIONAL]]
// CHECK:         [[INSTANCE:%[^,]+]] = struct $WeakBox<T> ([[WEAK_OPTIONAL]] :
// CHECK:         return [[INSTANCE]]
// CHECK-LABEL: } // end sil function '$s20opaque_values_silgen7WeakBoxV1tACyxGxSg_tcfC'
struct WeakBox<T : AnyObject> {
  weak var t: T?
}

// CHECK-LABEL: sil {{.*}}[ossa] @intIntoAnyHashableVar : {{.*}} {
// CHECK:         [[VAR_BOX_ADDR:%[^,]+]] = project_box
// CHECK:         [[INT:%[^,]+]] = apply
// CHECK:         [[CONVERT:%[^,]+]] = function_ref @$ss21_convertToAnyHashableys0cD0VxSHRzlF
// CHECK:         [[INSTANCE:%[^,]+]] = apply [[CONVERT]]<Int>([[INT]])
// CHECK:         store [[INSTANCE]] to [init] [[VAR_BOX_ADDR]]
// CHECK-LABEL: } // end sil function 'intIntoAnyHashableVar'
@_silgen_name("intIntoAnyHashableVar")
func intIntoAnyHashableVar() {
  var anyHashable: AnyHashable = 0
}

// CHECK-LABEL: sil {{.*}}[ossa] @intIntoAnyHashableLet : {{.*}} {
// CHECK:         [[INT:%[^,]+]] = apply
// CHECK:         [[CONVERT:%[^,]+]] = function_ref @$ss21_convertToAnyHashableys0cD0VxSHRzlF
// CHECK:         [[INSTANCE:%[^,]+]] = apply [[CONVERT]]<Int>([[INT]])
// CHECK-LABEL: } // end sil function 'intIntoAnyHashableLet'
@_silgen_name("intIntoAnyHashableLet")
func intIntoAnyHashableLet() {
  let anyHashable: AnyHashable = 0
}

// CHECK-LABEL: sil {{.*}}[ossa] @consumeExprOfOwnedAddrOnlyValue : {{.*}} {
// CHECK:       bb0([[T:%[^,]+]] :
// CHECK:         [[T_LIFETIME:%[^,]+]] = begin_borrow [[T]]
// CHECK:         [[T_COPY:%[^,]+]] = copy_value [[T_LIFETIME]]
// CHECK:         [[T_MOVE:%[^,]+]] = move_value [allows_diagnostics] [[T_COPY]]
// CHECK:         [[SINK:%[^,]+]] = function_ref @sink
// CHECK:         apply [[SINK]]<T>([[T_MOVE]])
// CHECK:         end_borrow [[T_LIFETIME]]
// CHECK:         destroy_value [[T]]
// CHECK-LABEL: } // end sil function 'consumeExprOfOwnedAddrOnlyValue'
@_silgen_name("consumeExprOfOwnedAddrOnlyValue")
func consumeExprOfOwnedAddrOnlyValue<T>(_ t: __owned T) {
  sink(consume t)
}

// CHECK-LABEL: sil {{.*}}[ossa] @consumeExprOfLoadExprOfOwnedAddrOnlyLValue : {{.*}} {
// CHECK:         [[VAR:%[^,]+]] = alloc_box $<τ_0_0> { var τ_0_0 } <T>
// CHECK:         [[VAR_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[VAR]]
// CHECK:         [[VAR_ADDR:%[^,]+]] = project_box [[VAR_LIFETIME]]
// CHECK:         store {{%[^,]+}} to [init] [[VAR_ADDR]]
// CHECK:         [[VAR_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[VAR_ADDR]]
// CHECK:         [[TEMPORARY_ADDR:%[^,]+]] = alloc_stack $T
// CHECK:         mark_unresolved_move_addr [[VAR_ACCESS]] to [[TEMPORARY_ADDR]]
// CHECK:         [[TEMPORARY:%[^,]+]] = load [take] [[TEMPORARY_ADDR]]
// CHECK:         end_access [[VAR_ACCESS]]
// CHECK:         [[SINK:%[^,]+]] = function_ref @sink
// CHECK:         apply [[SINK]]<T>([[TEMPORARY]])
// CHECK:         dealloc_stack [[TEMPORARY_ADDR]]
// CHECK:         destroy_value [[VAR]]
// CHECK-LABEL: } // end sil function 'consumeExprOfLoadExprOfOwnedAddrOnlyLValue'
@_silgen_name("consumeExprOfLoadExprOfOwnedAddrOnlyLValue")
func consumeExprOfLoadExprOfOwnedAddrOnlyLValue<T>(_ ty: T.Type) {
  var t = source(ty)
  sink(consume t)
}

struct Twople<T> {
  var storage: (T, T)

// CHECK-LABEL: sil {{.*}}[ossa] @Twople_init_from_t1_t2 : {{.*}} {
// CHECK:       bb0([[T1:%[^,]+]] : 
// CHECK-SAME:      [[T2:%[^,]+]] : 
// CHECK-SAME:  ):
// CHECK:         [[VAR:%[^,]+]] = alloc_box
// CHECK:         [[VAR_UNINIT:%[^,]+]] = mark_uninitialized [rootself] [[VAR]]
// CHECK:         [[VAR_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[VAR_UNINIT]]
// CHECK:         [[VAR_ADDR:%[^,]+]] = project_box [[VAR_LIFETIME]]
// CHECK:         [[T1_BORROW:%[^,]+]] = begin_borrow [[T1]]
// CHECK:         [[T1_COPY:%[^,]+]] = copy_value [[T1_BORROW]]
// CHECK:         [[T2_BORROW:%[^,]+]] = begin_borrow [[T2]]
// CHECK:         [[T2_COPY:%[^,]+]] = copy_value [[T2_BORROW]]
// CHECK:         [[VAR_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[VAR_ADDR]]
// CHECK:         [[STORAGE_ACCESS:%[^,]+]] = struct_element_addr [[VAR_ACCESS]]

//                In opaque values mode, without regard to the fact that T is
//                address-only, a tuple is constructed and assigned into the
//                storage.
// CHECK:         [[TUPLE:%[^,]+]] = tuple (
// CHECK-SAME:        [[T1_COPY]]
// CHECK-SAME:        [[T2_COPY]]
// CHECK-SAME:    )
// CHECK:         assign [[TUPLE]] to [[STORAGE_ACCESS]]

// CHECK:         end_access [[VAR_ACCESS]]
// CHECK:         end_borrow [[T2_BORROW]]
// CHECK:         end_borrow [[T1_BORROW]]
// CHECK:         [[RETVAL:%[^,]+]] = load [copy] [[VAR_ADDR]]
// CHECK:         destroy_value [[T2]]
// CHECK:         destroy_value [[T1]]
// CHECK:         end_borrow [[VAR_LIFETIME]]
// CHECK:         destroy_value [[VAR_UNINIT]]
// CHECK:         return [[RETVAL]]
// CHECK-LABEL: } // end sil function 'Twople_init_from_t1_t2'
  @_silgen_name("Twople_init_from_t1_t2")
  init(t1: T, t2: T) {
    self.storage = (t1, t2)
  }
}

// CHECK-LABEL: sil{{.*}} [ossa] @throwTypedValue : {{.*}} -> @error Err {
// CHECK:       bb0([[E:%[^,]+]] :
// CHECK:         [[SWIFT_WILL_THROW_TYPED:%[^,]+]] = function_ref @swift_willThrowTyped
// CHECK:         apply [[SWIFT_WILL_THROW_TYPED]]<Err>([[E]])
// CHECK:         throw [[E]]
// CHECK-LABEL: } // end sil function 'throwTypedValue'
@_silgen_name("throwTypedValue")
func throwTypedValue(_ e: Err) throws(Err) { throw e }

// CHECK-LABEL: sil{{.*}} [ossa] @callTypedThrowsFunc : {{.*}} -> @error any Error {
// CHECK:       bb0:
// CHECK:         [[TYPED_THROW_FN:%[^,]+]] = function_ref @throwTypedValue
// CHECK:         try_apply [[TYPED_THROW_FN]]({{%[0-9]+}}) : $@convention(thin) (Err) -> @error Err, normal bb1, error bb2
//
// CHECK:       bb1({{%[0-9]+}} : $()):
// CHECK:         return
//
// CHECK:       bb2([[E:%[^,]+]] : $Err):
// CHECK:         [[STACK_ALLOC:%[^,]+]] = alloc_stack $any Error
// CHECK:         [[BOX:%[^,]+]] = project_existential_box $Err
// CHECK:         store [[E]] to [trivial] [[BOX]]
// CHECK:         [[ANY_ERROR:%[^,]+]] = load [take] [[STACK_ALLOC]]
// CHECK:         throw [[ANY_ERROR]]
// CHECK-LABEL: } // end sil function 'callTypedThrowsFunc'
@_silgen_name("callTypedThrowsFunc")
func callTypedThrowsFunc() throws {
  try throwTypedValue(Err())
}

// CHECK-LABEL: sil{{.*}} [ossa] @throwTypedValueGeneric : {{.*}} <GenErr where GenErr : Error> (@in_guaranteed GenErr) -> @error_indirect GenErr {
// CHECK:       bb0([[BORROWED_ERR:%[^,]+]] : @guaranteed $GenErr):
// CHECK:         [[E:%[^,]+]] = copy_value [[BORROWED_ERR]]
// CHECK:         [[SWIFT_WILL_THROW_TYPED:%[^,]+]] = function_ref @swift_willThrowTyped
// CHECK:         apply [[SWIFT_WILL_THROW_TYPED]]<GenErr>([[E]])
// CHECK:         throw [[E]]
// CHECK-LABEL: } // end sil function 'throwTypedValueGeneric'
@_silgen_name("throwTypedValueGeneric")
func throwTypedValueGeneric<GenErr: Error>(_ e: GenErr) throws(GenErr) { throw e }

// CHECK-LABEL: sil{{.*}} [ossa] @callTypedThrowsFuncGeneric : {{.*}} -> @error any Error {
// CHECK:       bb0:
// CHECK:         [[ERR_VAL:%[^,]+]] = apply {{.*}} -> Err
// CHECK:         [[GEN_TYPED_THROW_FN:%[^,]+]] = function_ref @throwTypedValueGeneric
// CHECK:         try_apply [[GEN_TYPED_THROW_FN]]<Err>([[ERR_VAL]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Error> (@in_guaranteed τ_0_0) -> @error_indirect τ_0_0, normal bb1, error bb2
//
// CHECK:       bb1({{%[0-9]+}} : $()):
// CHECK:         return
//
// CHECK:       bb2([[E:%[^,]+]] : $Err):
// CHECK:         [[STACK_ALLOC:%[^,]+]] = alloc_stack $any Error
// CHECK:         [[BOX:%[^,]+]] = project_existential_box $Err
// CHECK:         store [[E]] to [trivial] [[BOX]]
// CHECK:         [[ANY_ERROR:%[^,]+]] = load [take] [[STACK_ALLOC]]
// CHECK:         throw [[ANY_ERROR]]
// CHECK-LABEL: } // end sil function 'callTypedThrowsFuncGeneric'
@_silgen_name("callTypedThrowsFuncGeneric")
func callTypedThrowsFuncGeneric() throws {
  try throwTypedValueGeneric(Err())
}

struct Err : Error {}

// CHECK-LABEL: sil{{.*}} [ossa] @copy_expr_generic : {{.*}} {
// CHECK:       bb0([[E:%[^,]+]] : @guaranteed $T
// CHECK:         [[E_COPY:%[^,]+]] = explicit_copy_value [[E]]
// CHECK:         apply {{.*}}<T>([[E_COPY]])
// CHECK-LABEL: } // end sil function 'copy_expr_generic'
@_silgen_name("copy_expr_generic")
func copy_expr_generic<T>(_ t: T)  {
  eat_generic(copy t)
}
func eat_generic<T>(_ t: consuming T) {}

struct LoadBorrowNC: ~Copyable {
  var x: Int = 0
}
final class LoadBorrowHolder {
  var inner: LoadBorrowNC
  init(inner: consuming LoadBorrowNC) { self.inner = inner }
}
func load_borrow_in_guaranteed_borrowMe<T: ~Copyable>(_ n: borrowing T) {}

// CHECK-LABEL: sil{{.*}} [ossa] @load_borrow_in_guaranteed_caller :
// CHECK:         ref_element_addr {{%[^,]+}}, #LoadBorrowHolder.inner
// CHECK:         mark_unresolved_non_copyable_value [no_consume_or_assign]
// CHECK:         [[BORROWED:%[^,]+]] = load_borrow
// CHECK-NOT:     load [copy]
// CHECK:         apply {{.*}}<LoadBorrowNC>([[BORROWED]])
// CHECK:         end_borrow [[BORROWED]]
// CHECK-LABEL: } // end sil function 'load_borrow_in_guaranteed_caller'
@_silgen_name("load_borrow_in_guaranteed_caller")
func load_borrow_in_guaranteed_caller(_ h: LoadBorrowHolder) {
  load_borrow_in_guaranteed_borrowMe(h.inner)
}


func existentialErrorPassThrough(_ body: () throws(Error) -> Void) throws(Error) {
  try body()
}
@_silgen_name("forceIndirectErrorThunk")
func forceIndirectErrorThunk<T: Error>(_ x: () throws(T) -> Void) throws {
  try existentialErrorPassThrough(x)
}

// CHECK-LABEL: sil{{.*}} [ossa] @forceIndirectErrorThunk : {{.*}} {
// CHECK:       [[CONVERTED:%[^,]+]] = convert_function {{%[^,]+}} : {{.*}} to $@noescape @callee_guaranteed () -> @error_indirect T
// CHECK:       [[THUNK_FN:%[^,]+]] = function_ref @$sxIgzr_s5Error_pIegzo_sAARzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : Error> (@guaranteed @noescape @callee_guaranteed () -> @error_indirect τ_0_0) -> @error any Error
// CHECK:       [[PARTIAL:%[^,]+]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<T>([[CONVERTED]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Error> (@guaranteed @noescape @callee_guaranteed () -> @error_indirect τ_0_0) -> @error any Error
// CHECK:       [[NOESCAPE:%[^,]+]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL]] : $@callee_guaranteed () -> @error any Error to $@noescape @callee_guaranteed () -> @error any Error
// CHECK:       [[PASSTHROUGH_FN:%[^,]+]] = function_ref @$s20opaque_values_silgen27existentialErrorPassThroughyyyyKXEKF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @error any Error) -> @error any Error
// CHECK:       try_apply [[PASSTHROUGH_FN]]([[NOESCAPE]]) : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @error any Error) -> @error any Error, normal {{bb[0-9]+}}, error [[ERROR_BB:bb[0-9]+]]

// CHECK:       [[ERROR_BB]]({{%[^,]+}} : @owned $any Error):
// CHECK: } // end sil function 'forceIndirectErrorThunk'


// CHECK-LABEL: sil{{.*}} [reabstraction_thunk] [ossa] @$sxIgzr_s5Error_pIegzo_sAARzlTR : $@convention(thin) <T where T : Error> (@guaranteed @noescape @callee_guaranteed () -> @error_indirect T) -> @error any Error {
// CHECK:       bb0([[CLOSURE:%[^,]+]] : @guaranteed $@noescape @callee_guaranteed () -> @error_indirect T):
// CHECK-NEXT:    try_apply [[CLOSURE]]() : {{.*}}, normal {{bb[0-9]+}}, error [[ERROR_BB:bb[0-9]+]]

// CHECK:       [[ERROR_BB]]({{%[^,]+}} : @owned $T):
// CHECK:         throw {{%[^,]+}} : $any Error
// CHECK-LABEL: } // end sil function '$sxIgzr_s5Error_pIegzo_sAARzlTR'



struct CCC {
  var members: [Member]
  var isInverted: Bool
}
enum Member {
  case atom(Int)
  case custom(CCC)
  indirect case intersection(CCC, CCC)
}

// Ensure we treat infinite types as non-trivial & loadable (rdar://180569048)
// CHECK-LABEL: sil {{.*}}3CCCV7membersSayAA6MemberOGvs : $@convention(method) (@owned Array<Member>, @inout CCC) -> () {
// CHECK: bb0(%0 : @owned $Array<Member>
// CHECK:   destroy_value %0
// CHECK: } // end sil function
