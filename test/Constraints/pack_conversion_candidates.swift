// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 5
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 6

// Fix the pack shape through generic arguments and test conversions of the
// entire result.
struct CandidateValues<each Element> {} // expected-note {{arguments to generic parameter 'Element' ('Int' and 'Int?') are expected to be equal}}

func materialize<each T>(
  _ values: CandidateValues<repeat each T>
) -> (repeat each T) {
  fatalError()
}

func fixedShapeResults() {
  let empty: CandidateValues<> = .init()
  let _: Void? = materialize(empty)
  let _: Void? = materialize(CandidateValues<Void>())
  let _: Int? = materialize(CandidateValues<Int>())
  let _: (Int, String)? = materialize(CandidateValues<Int, String>())
  let _: (Int, String)? = materialize(CandidateValues<(Int, String)>())
  let _: Int?? = materialize(CandidateValues<Int?>())
  let _: Any = materialize(CandidateValues<Int, String>())
}

// Inject the entire empty-pack result into Optional even when Optional cannot
// satisfy the pack element requirement.
protocol CandidateLiteral {}
extension Int: CandidateLiteral {}

func constrainedResult<each T: CandidateLiteral>() -> (repeat each T) {
  fatalError()
}

func constrainedResult() -> String { "" }

func emptyConstrainedResult() {
  let _: Void? = constrainedResult()
}

// Distinguish identity key path equality from result conversion when a key path
// is converted to a function.
func identityFunction() {
  let _: (Int) -> Int? = \.self
}

// Preserve implicit result conversion from Never for abstract pack results.
func abstractResult<each T>() -> (repeat each T) {
  fatalError()
}

// A generic pack archetype is not a solver type variable. Preserve conversion
// of the entire abstract tuple to Optional without assuming a singleton pack.
func abstractOptionalResult<each T>(
  _ values: repeat each T
) -> (repeat each T)? {
  (repeat each values)
}

func invalidAbstractScalarResult<each T>(_ values: repeat each T) -> Int {
  (repeat each values)
  // expected-error@-1 {{cannot convert return expression of type '(repeat each T)' to return type 'Int'}}
}

// Converting the entire result must not change the pack shape or allow
// conversions between invariant generic arguments.
func invalidShape() {
  let _: Int? = materialize(CandidateValues<Int, String>())
  // expected-error@-1 {{cannot convert value of type '(Int, String)' to specified type 'Int?'}}
}

func requireOptionalIdentity<each T>(
  _ values: CandidateValues<repeat each T>,
  _ path: KeyPath<(repeat each T), Int?>
) {}

func invalidIdentity() {
  requireOptionalIdentity(CandidateValues<Int>(), \.self)
  // expected-error@-1 {{cannot convert value of type 'KeyPath<Int, Int>' to expected argument type 'KeyPath<Int, Int?>'}}
  // expected-note@-2 {{arguments to generic parameter 'Value' ('Int' and 'Int?') are expected to be equal}}
}

struct Invariant<Value> {}

func requireSame<each T>(
  _ values: CandidateValues<repeat each T>,
  _ result: Invariant<(repeat each T)>
) {}

func invalidInvariant() {
  requireSame(CandidateValues<Int>(), Invariant<Int?>())
  // expected-error@-1 {{cannot convert value of type 'CandidateValues<Int>' to expected argument type 'CandidateValues<Int?>'}}
}

// Infer the pack through an associated type represented by a PackExpansionType
// whose pattern and count are type variables, rather than by a placeholder.
protocol HasPackValue {
  associatedtype Value
}

struct PackValue<each Element>: HasPackValue {
  typealias Value = (repeat each Element)
}

func makePackValue<each T>() -> PackValue<repeat each T> { .init() }
func getPackValue<T: HasPackValue>(_ value: T) -> T.Value { fatalError() }

func inferAssociatedPackResult() {
  let _: Int = getPackValue(makePackValue())
}
