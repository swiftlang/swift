// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -solver-enable-diagnose-valid-salvage
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 5 -strict-concurrency=complete -solver-enable-diagnose-valid-salvage
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 6 -solver-enable-diagnose-valid-salvage

// Valid conversions must succeed without relying on diagnostic salvage.

struct Box<T> {} // expected-note 8 {{arguments to generic parameter 'T' ('any Sendable' and 'Any') are expected to be equal}}
struct Shape<each T> {}

struct Source {
  @preconcurrency var legacy: Box<any Sendable>
  var ordinary: Box<any Sendable>
  @preconcurrency var optional: Box<any Sendable>?
  var ordinaryOptional: Box<any Sendable>?
  @preconcurrency var pair: (Box<any Sendable>, Box<any Sendable>)
}

func acceptScalar(_ value: Box<Any>) {}
@preconcurrency func acceptLegacyScalar(_ value: Box<Any>) {}

func acceptTuple(_ value: (Box<Any>, Box<Any>)) {}
@preconcurrency func acceptLegacyTuple(_ value: (Box<Any>, Box<Any>)) {}

func acceptPack<each T>(
  _ value: (repeat Box<each T>), _ shape: Shape<repeat each T>
) {}

@preconcurrency func acceptLegacyPack<each T>(
  _ value: (repeat Box<each T>), _ shape: Shape<repeat each T>
) {}

// Check source annotations separately from the accepting function's annotation.
func testScalar(_ source: Source) {
  acceptScalar(source.legacy)
  acceptLegacyScalar(source.legacy)
  acceptScalar(source.ordinary) // expected-error {{cannot convert value of type 'Box<any Sendable>' to expected argument type 'Box<Any>'}}
  acceptLegacyScalar(source.ordinary) // expected-error {{cannot convert value of type 'Box<any Sendable>' to expected argument type 'Box<Any>'}}
}

func testPack(_ source: Source) {
  acceptPack(source.legacy, Shape<Any>())
  acceptLegacyPack(source.legacy, Shape<Any>())
  acceptPack(source.ordinary, Shape<Any>()) // expected-error {{cannot convert value of type '(_: Box<any Sendable>)' to expected argument type '(_: Box<Any>)'}}
  acceptLegacyPack(source.ordinary, Shape<Any>()) // expected-error {{cannot convert value of type '(_: Box<any Sendable>)' to expected argument type '(_: Box<Any>)'}}
}

// Inspect the source of each element when the argument is a tuple literal.
func testTupleElements(_ source: Source) {
  acceptPack(source.pair, Shape<Any, Any>())
  acceptLegacyPack(source.pair, Shape<Any, Any>())
  acceptPack((source.legacy, source.legacy), Shape<Any, Any>())
  acceptLegacyPack((source.legacy, source.legacy), Shape<Any, Any>())
  acceptLegacyPack((source.legacy, source.ordinary), Shape<Any, Any>()) // expected-error {{cannot convert value of type '(Box<any Sendable>, Box<any Sendable>)' to expected argument type '(Box<Any>, Box<Any>)'}}
}

func testFixedTuple(_ source: Source) {
  acceptTuple(source.pair)
  acceptLegacyTuple(source.pair)
  acceptTuple((source.legacy, source.legacy))
  acceptLegacyTuple((source.legacy, source.legacy))
  acceptLegacyTuple((source.legacy, source.ordinary)) // expected-error {{cannot convert value of type '(Box<any Sendable>, Box<any Sendable>)' to expected argument type '(Box<Any>, Box<Any>)'}}
}

func acceptOptional(_ value: Box<Any>?) {}
@preconcurrency func acceptLegacyOptional(_ value: Box<Any>?) {}

// ApplyArgToParam identifies the conversion without diagnostic GenericType elements.
func testOptional(_ source: Source) {
  acceptOptional(source.optional)
  acceptLegacyOptional(source.optional)
  acceptOptional(source.ordinaryOptional) // expected-error {{cannot convert value of type 'Box<any Sendable>?' to expected argument type 'Box<Any>?'}}
  acceptLegacyOptional(source.ordinaryOptional) // expected-error {{cannot convert value of type 'Box<any Sendable>?' to expected argument type 'Box<Any>?'}}
}

// ContextualType identifies the same conversion in an initializer.
func testOptionalContext(_ source: Source) {
  let _: Box<Any>? = source.optional
}

func optionalResult(_ source: Source) -> Box<Any>? {
  source.optional
}

func acceptGenericOptional<T>(_ value: Box<T>?, _: T.Type) {}

func testGenericOptional(_ source: Source) {
  acceptGenericOptional(source.optional, Any.self)
}

// CoercionOperand identifies the conversion in an explicit coercion.
func testCoercion(_ source: Source) {
  _ = source.legacy as Box<Any>
  _ = source.ordinary as Box<Any> // expected-error {{cannot convert value of type 'Box<any Sendable>' to type 'Box<Any>' in coercion}}
}

// Identity expressions preserve the source declaration.
func testParentheses(_ source: Source) {
  acceptScalar((source.legacy))
  acceptScalar(((source.legacy)))
  acceptScalar(source.legacy.self)
  acceptScalar((source.ordinary)) // expected-error {{cannot convert value of type 'Box<any Sendable>' to expected argument type 'Box<Any>'}}
}

func testContextualSource(_ source: Source) {
  var target = Box<Any>()
  target = source.legacy
  target = source.ordinary // expected-error {{cannot assign value of type 'Box<any Sendable>' to type 'Box<Any>'}}
  _ = target
  let _: (Box<Any>, Box<Any>) = (source.legacy, source.legacy)
}

// ClosureBody identifies the conversion to a single-expression closure result.
func testClosureBody(_ source: Source) {
  let _: () -> Box<Any> = { source.legacy }
  let _: () -> Box<Any> = { source.ordinary } // expected-error {{cannot convert value of type 'Box<any Sendable>' to closure result type 'Box<Any>'}}
}

func acceptInout(_ value: inout Box<Any>) {}

func testInoutSource(_ source: inout Source) {
  acceptInout(&source.legacy)
}

extension Box where T == Any {
  func onlyForAny() {} // expected-note {{'onlyForAny()' declared here}}
}

// Member lookup may satisfy a concrete requirement on its Self type.
func testConstrainedMember(_ source: Source) {
  source.legacy.onlyForAny()
  (source.legacy).onlyForAny()
  source.ordinary.onlyForAny() // expected-error {{referencing instance method 'onlyForAny()' on 'Box<any Sendable>' requires the types 'any Sendable' and 'Any' be equivalent}}
}

extension Box {
  func acceptSameBox(_ value: Box<T>) {}
}

func testMemberGenericArguments(_ source: Source, _ value: Box<Any>) {
  source.legacy.acceptSameBox(value)
}
