
// RUN: %target-swift-emit-silgen -parse-as-library -module-name Swift -parse-stdlib -enable-sil-ownership %s | %FileCheck %s

// This test checks specific codegen related to normal arguments being passed at
// +0. Eventually, it should be merged into normal SILGen tests.

/////////////////
// Fake Stdlib //
/////////////////

precedencegroup AssignmentPrecedence {
  assignment: true
}

public protocol ExpressibleByNilLiteral {
  init(nilLiteral: ())
}

protocol IteratorProtocol {
  associatedtype Element
  mutating func next() ->  Element?
}

protocol Sequence {
  associatedtype Element
  associatedtype Iterator : IteratorProtocol where Iterator.Element == Element

  func makeIterator() -> Iterator
}

enum Optional<T> {
case none
case some(T)
}

extension Optional : ExpressibleByNilLiteral {
  public init(nilLiteral: ()) {
    self = .none
  }
}

func _diagnoseUnexpectedNilOptional(_filenameStart: Builtin.RawPointer,
                                    _filenameLength: Builtin.Word,
                                    _filenameIsASCII: Builtin.Int1,
                                    _line: Builtin.Word) {
  // This would usually contain an assert, but we don't need one since we are
  // just emitting SILGen.
}

class Klass {
  init() {}
}

struct Buffer {
  var k: Klass
  init(inK: Klass) {
    k = inK
  }
}

public typealias AnyObject = Builtin.AnyObject

protocol Protocol {
  associatedtype AssocType
  static func useInput(_ input: Builtin.Int32, into processInput: (AssocType) -> ())
}

struct FakeArray<Element> {
  // Just to make this type non-trivial
  var k: Klass

  // We are only interested in this being called. We are not interested in its
  // implementation.
  mutating func append(_ t: Element) {}
}

struct FakeDictionary<Key, Value> {
}

struct FakeDictionaryIterator<Key, Value> {
  var dictionary: FakeDictionary<Key, Value>?

  init(_ newDictionary: FakeDictionary<Key, Value>) {
    dictionary = newDictionary
  }
}

extension FakeDictionaryIterator : IteratorProtocol {
  public typealias Element = (Key, Value)
  public mutating func next() -> Element? {
    return .none
  }
}

extension FakeDictionary : Sequence {
  public typealias Element = (Key, Value)
  public typealias Iterator = FakeDictionaryIterator<Key, Value>
  public func makeIterator() -> FakeDictionaryIterator<Key, Value> {
    return FakeDictionaryIterator(self)
  }
}

public struct Unmanaged<Instance : AnyObject> {
  internal unowned(unsafe) var _value: Instance
}

///////////
// Tests //
///////////

class KlassWithBuffer {
  var buffer: Buffer

  // Make sure that the allocating init forwards into the initializing init at +1.
  // CHECK-LABEL: sil hidden @$Ss15KlassWithBufferC3inKABs0A0C_tcfC : $@convention(method) (@owned Klass, @thick KlassWithBuffer.Type) -> @owned KlassWithBuffer {
  // CHECK: bb0([[ARG:%.*]] : @owned $Klass,
  // CHECK:   [[INITIALIZING_INIT:%.*]] = function_ref @$Ss15KlassWithBufferC3inKABs0A0C_tcfc : $@convention(method) (@owned Klass, @owned KlassWithBuffer) -> @owned KlassWithBuffer
  // CHECK:   apply [[INITIALIZING_INIT]]([[ARG]],
  // CHECK: } // end sil function '$Ss15KlassWithBufferC3inKABs0A0C_tcfC'
  init(inK: Klass = Klass()) {
    buffer = Buffer(inK: inK)
  }

  // This test makes sure that we:
  //
  // 1. Are able to propagate a +0 value value buffer.k into a +0 value and that
  // we then copy that +0 value into a +1 value, before we begin the epilog and
  // then return that value.
  // CHECK-LABEL: sil hidden @$Ss15KlassWithBufferC03getC14AsNativeObjectBoyF : $@convention(method) (@guaranteed KlassWithBuffer) -> @owned Builtin.NativeObject {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $KlassWithBuffer):
  // CHECK:   [[BUF_BOX:%.*]] = alloc_stack $Buffer
  // CHECK:   [[METHOD:%.*]] = class_method [[SELF]] : $KlassWithBuffer, #KlassWithBuffer.buffer!getter.1
  // CHECK:   [[BUF:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:   store [[BUF]] to [init] [[BUF_BOX]]
  // CHECK:   [[GEP:%.*]] = struct_element_addr [[BUF_BOX]] : $*Buffer, #Buffer.k
  // CHECK:   [[BUF_KLASS:%.*]] = load [copy] [[GEP]]
  // CHECK:   destroy_addr [[BUF_BOX]]
  // CHECK:   [[BORROWED_BUF_KLASS:%.*]] = begin_borrow [[BUF_KLASS]]
  // CHECK:   [[CASTED_BORROWED_BUF_KLASS:%.*]] = unchecked_ref_cast [[BORROWED_BUF_KLASS]]
  // CHECK:   [[COPY_CASTED_BORROWED_BUF_KLASS:%.*]] = copy_value [[CASTED_BORROWED_BUF_KLASS]]
  // CHECK:   end_borrow [[BORROWED_BUF_KLASS]]
  // CHECK:   destroy_value [[BUF_KLASS]]
  // CHECK:   return [[COPY_CASTED_BORROWED_BUF_KLASS]]
  // CHECK: } // end sil function '$Ss15KlassWithBufferC03getC14AsNativeObjectBoyF'
  func getBufferAsNativeObject() -> Builtin.NativeObject {
    return Builtin.unsafeCastToNativeObject(buffer.k)
  }
}

struct StructContainingBridgeObject {
  var rawValue: Builtin.BridgeObject

  // CHECK-LABEL: sil hidden @$Ss28StructContainingBridgeObjectV8swiftObjAByXl_tcfC : $@convention(method) (@owned AnyObject, @thin StructContainingBridgeObject.Type) -> @owned StructContainingBridgeObject {
  // CHECK: bb0([[ARG:%.*]] : @owned $AnyObject,
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[CASTED_ARG:%.*]] = unchecked_ref_cast [[BORROWED_ARG]] : $AnyObject to $Builtin.BridgeObject
  // CHECK:   [[COPY_CASTED_ARG:%.*]] = copy_value [[CASTED_ARG]]
  // CHECK:   assign [[COPY_CASTED_ARG]] to
  // CHECK: } // end sil function '$Ss28StructContainingBridgeObjectV8swiftObjAByXl_tcfC'
  init(swiftObj: AnyObject) {
    rawValue = Builtin.reinterpretCast(swiftObj)
  }
}

struct ReabstractionThunkTest : Protocol {
  typealias AssocType = Builtin.Int32

  static func useInput(_ input: Builtin.Int32, into processInput: (AssocType) -> ()) {
    processInput(input)
  }
}

// Make sure that we provide a cleanup to x properly before we pass it to
// result.
extension FakeDictionary {
  // CHECK-LABEL: sil hidden @$Ss14FakeDictionaryV20makeSureToCopyTuplesyyF : $@convention(method) <Key, Value> (FakeDictionary<Key, Value>) -> () {
  // CHECK:   [[X:%.*]] = alloc_stack $(Key, Value), let, name "x"
  // CHECK:   [[INDUCTION_VAR:%.*]] = unchecked_take_enum_data_addr {{%.*}} : $*Optional<(Key, Value)>, #Optional.some!enumelt.1
  // CHECK:   [[INDUCTION_VAR_0:%.*]] = tuple_element_addr [[INDUCTION_VAR]] : $*(Key, Value), 0
  // CHECK:   [[INDUCTION_VAR_1:%.*]] = tuple_element_addr [[INDUCTION_VAR]] : $*(Key, Value), 1
  // CHECK:   [[X_0:%.*]] = tuple_element_addr [[X]] : $*(Key, Value), 0
  // CHECK:   [[X_1:%.*]] = tuple_element_addr [[X]] : $*(Key, Value), 1
  // CHECK:   copy_addr [take] [[INDUCTION_VAR_0]] to [initialization] [[X_0]]
  // CHECK:   copy_addr [take] [[INDUCTION_VAR_1]] to [initialization] [[X_1]]
  // CHECK:   [[X_0:%.*]] = tuple_element_addr [[X]] : $*(Key, Value), 0
  // CHECK:   [[X_1:%.*]] = tuple_element_addr [[X]] : $*(Key, Value), 1
  // CHECK:   [[TMP_X:%.*]] = alloc_stack $(Key, Value)
  // CHECK:   [[TMP_X_0:%.*]] = tuple_element_addr [[TMP_X]] : $*(Key, Value), 0
  // CHECK:   [[TMP_X_1:%.*]] = tuple_element_addr [[TMP_X]] : $*(Key, Value), 1
  // CHECK:   [[TMP_0:%.*]] = alloc_stack $Key
  // CHECK:   copy_addr [[X_0]] to [initialization] [[TMP_0]]
  // CHECK:   copy_addr [take] [[TMP_0]] to [initialization] [[TMP_X_0]]
  // CHECK:   [[TMP_1:%.*]] = alloc_stack $Value
  // CHECK:   copy_addr [[X_1]] to [initialization] [[TMP_1]]
  // CHECK:   copy_addr [take] [[TMP_1]] to [initialization] [[TMP_X_1]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$Ss9FakeArrayV6appendyyxF : $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @inout FakeArray<τ_0_0>) -> ()
  // CHECK:   apply [[FUNC]]<(Key, Value)>([[TMP_X]],
  // CHECK: } // end sil function '$Ss14FakeDictionaryV20makeSureToCopyTuplesyyF'
  func makeSureToCopyTuples() {
    var result = FakeArray<Element>(k: Klass())
    for x in self {
      result.append(x)
    }
  }
}

extension Unmanaged {
  // Just make sure that we do not crash on this.
  func unsafeGuaranteedTest<Result>(
    _ body: (Instance) -> Result
  ) -> Result {
    let (guaranteedInstance, token) = Builtin.unsafeGuaranteed(_value)
    let result = body(guaranteedInstance)
    Builtin.unsafeGuaranteedEnd(token)
    return result
  }
}

// Make sure that we properly forward x into memory and don't crash.
public func forwardIntoMemory(fromNative x: AnyObject, y: Builtin.Word) -> Builtin.BridgeObject {
  // y would normally be 0._builtinWordValue. We don't want to define that
  // conformance.
  let object = Builtin.castToBridgeObject(x, y)
  return object
}

public struct StructWithOptionalAddressOnlyField<T> {
  public let newValue: T?
}

func useStructWithOptionalAddressOnlyField<T>(t: T) -> StructWithOptionalAddressOnlyField<T> {
  return StructWithOptionalAddressOnlyField<T>(newValue: t)
}
