// RUN: %target-swift-frontend -emit-sil -sil-verify-all -enable-experimental-feature MoveOnlyTuples -verify %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -enable-experimental-feature MoveOnlyTuples -enable-sil-opaque-values -verify %s -o /dev/null

// REQUIRES: swift_feature_MoveOnlyTuples

struct Value: ~Copyable {
  var number: Int
}

func borrow(_ value: borrowing Value) {}
func consume(_ value: consuming Value) {}
func borrowGeneric<T: ~Copyable>(_ value: borrowing T) {}

// Even an unused parameter reconstructs a tuple from the borrowed arguments.
func unused(_ value: borrowing (Value, Int)) {}

func borrowElements(_ value: borrowing (Value, Value)) {
  borrow(value.0)
  borrow(value.1)
  borrow(value.0)
}

func nested(_ value: borrowing (first: (Value, Int), second: Value)) {
  borrow(value.first.0)
  borrow(value.second)
  borrow(value.first.0)
}

func borrowCopyableField(_ value: borrowing (Value, Int)) -> Int {
  value.1
}

func borrowWhole(_ value: borrowing (Value, Int)) {
  borrowGeneric(value)
  borrowGeneric(value)
}

func borrowTuple(_ value: borrowing (Value, Int)) {
  borrow(value.0)
}

func borrowArgument(_ value: borrowing (Value, Int)) {
  borrowTuple(value)
  borrowTuple(value)
}

func borrowLiteral(_ value: borrowing Value) {
  borrowTuple((value, 1))
  borrowTuple((value, 2))
}

func copyField(_ value: borrowing (Value, String)) -> String {
  borrowGeneric(value.1)
  return value.1
}

func makeTuple() -> (Value, Int) {
  (Value(number: 1), 2)
}

func temporaryProjections() -> Value {
  borrow(makeTuple().0)
  consume(makeTuple().0)
  _ = makeTuple().1
  return makeTuple().0
}

struct Container: ~Copyable {
  var storage: (Value, Int)
  var computed: (Value, Int) { makeTuple() }
}

func storedProperty(_ value: borrowing Container) {
  borrow(value.storage.0)
  borrow(value.storage.0)
  borrowTuple(value.storage)
}

func computedProperty(_ value: borrowing Container) {
  borrowTuple(value.computed)
  borrowTuple(value.computed)
}

protocol Callback<Arguments>: ~Copyable {
  associatedtype Arguments: ~Copyable
  func call(_ arguments: borrowing Arguments)
}

struct TupleCallback: Callback, ~Copyable {
  func call(_ arguments: borrowing (Value, Int)) {
    borrow(arguments.0)
  }
}

func call<C: Callback & ~Copyable>(
  _ callback: borrowing C, _ arguments: borrowing C.Arguments
) where C.Arguments: ~Copyable {
  callback.call(arguments)
}

func callThroughWitness(_ arguments: borrowing (Value, Int)) {
  let callback = TupleCallback()
  call(callback, arguments)
  call(callback, arguments)
}

func inferred() {
  let value = (Value(number: 1), 2)
  borrow(value.0)
  borrow(value.0)
  consume(value.0)
}

func consumeBorrowed(_ value: borrowing (Value, Int)) { // expected-error{{'value' is borrowed and cannot be consumed}}
  consume(value.0) // expected-note{{consumed here}}
}

func consumeTwice(_ value: consuming (Value, Int)) { // expected-error{{'value' consumed more than once}}
  consume(value.0) // expected-note{{consumed here}}
  consume(value.0) // expected-note{{consumed again here}}
}

func borrowAndConsume(_ x: borrowing (Value, Int), _ y: consuming (Value, Int)) {}
func borrowAndMutate(_ x: borrowing (Value, Int), _ y: inout (Value, Int)) {}

func overlappingConsume(_ value: consuming (Value, Int)) { // expected-error{{'value' used after consume}}
  borrowAndConsume(value, value) // expected-note{{used here}} expected-note{{consumed here}}
}

func overlappingMutation(_ value: inout (Value, Int)) {
  borrowAndMutate(value, &value) // expected-error{{overlapping accesses to 'value', but modification requires exclusive access}} expected-note{{conflicting access is here}}
}

func escapingCapture(_ value: borrowing (Value, Int)) -> () -> Void { // expected-error{{'value' cannot be captured by an escaping closure since it is a borrowed parameter}}
  { borrowTuple(value) } // expected-note{{closure capturing 'value' here}}
}

func storeBorrowedElement(_ value: borrowing Value) { // expected-error{{'value' is borrowed and cannot be consumed}}
  let tuple = (value, 1) // expected-note{{consumed here}}
  borrowTuple(tuple)
}
