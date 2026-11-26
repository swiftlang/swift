// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -enable-experimental-feature PackExpansionInArrayLiterals

// REQUIRES: swift_feature_PackExpansionInArrayLiterals

protocol P {}

func toAny<each T>(_ t: repeat each T) -> [Any] {
  return [repeat each t]
}

func mixed<each T, each U>(_ t: repeat each T, u: repeat each U) -> [Any] {
  return [0, repeat each t, "middle", repeat each u, 1.0]
}

func existential<each T: P>(_ t: repeat each T) -> [any P] {
  return [repeat each t]
}

// The element type can be inferred from the pattern if it doesn't reference
// any pack elements.
func descriptions<each T: CustomStringConvertible>(_ t: repeat each T) {
  let strings = [repeat (each t).description]
  let _: [String] = strings
  let _: Int = strings // expected-error {{cannot convert value of type '[String]' to specified type 'Int'}}
}

// An array literal inside of a pack expansion can still have an element type
// that references pack elements.
func arrayPerElement<each T>(_ t: repeat each T) -> (repeat [each T]) {
  return (repeat [each t, each t])
}

// The element type of the array can't be a pack element, because the array
// exists outside of the expansion.
func noContext<each T>(_ t: repeat each T) {
  let a = [repeat each t]
  // expected-error@-1 {{heterogeneous collection literal could only be inferred to '[Any]'; add explicit type annotation if this is intentional}}
  _ = a
}

// The element type can't be inferred from a placeholder either: like the
// literal's default element type, the placeholder's type variable is outside
// of the expansion.
func placeholder<each T>(_ t: repeat each T) {
  let _: [_] = [repeat each t] // expected-error {{could not infer type for placeholder}}
  let _: [_?] = [repeat each t] // expected-error {{could not infer type for placeholder}}
}

// Nor can a type variable from an enclosing expression bind to a pack element.
func generic<E>(_ e: [E]) -> [E] { e }
func enclosingGeneric<each T>(_ t: repeat each T) -> [Any] {
  return generic([repeat each t])
}

// Type variables that are part of converting each element can still bind to
// pack elements, e.g. the intermediate type in AnyHashable erasure, or type
// variables inside of the pattern.
func innerTypeVariables<each T: Equatable>(_ t: repeat each T) -> [Bool] {
  return [repeat [each t, each t].contains(each t)]
}

func notConvertible<each T>(_ t: repeat each T) -> [Int] {
  return [repeat each t] // expected-error {{cannot convert value of type 'each T' to expected element type 'Int'}}
}

struct MyList: ExpressibleByArrayLiteral {
  init(arrayLiteral elements: String...) {}
}

func custom<each T>(_ t: repeat each T) -> MyList {
  return ["<", repeat String(describing: each t), ">"]
}

func anyHashable<each T: Hashable>(_ t: repeat each T) -> [AnyHashable] {
  return [repeat each t]
}

func set<each T: Hashable>(_ t: repeat each T) -> Set<AnyHashable> {
  return [repeat each t]
}

func noPackReference() -> [Int] {
  return [repeat 1] // expected-error {{value pack expansion must contain at least one pack reference}}
}

// The element type can't depend on pack elements in other ways either.
struct Box<V> {
  init(_ v: V) {}
}

func boxes<E>(_: [Box<E>]) {}
func functions<E>(_: [(E) -> Void]) {}

func genericArgument<each T>(_ t: repeat each T) {
  boxes([repeat Box(each t)])
  // expected-error@-1 {{array literal element type 'Box<each T>' that depends on pack element type 'each T' is not yet supported}}
  let _: [Box<_>] = [repeat Box(each t)]
  // expected-error@-1 {{array literal element type 'Box<each T>' that depends on pack element type 'each T' is not yet supported}}
}

func functionParameter<each T>(_ fs: repeat @escaping (each T) -> Void) {
  functions([repeat each fs])
  // expected-error@-1 {{array literal element type '(each T) -> Void' that depends on pack element type 'each T' is not yet supported}}
  let _: [(_) -> Void] = [repeat each fs]
  // expected-error@-1 {{array literal element type '(each T) -> Void' that depends on pack element type 'each T' is not yet supported}}
}

@available(SwiftStdlib 6.2, *)
func takesInlineArray(_: InlineArray<1, Any>) {}

@available(SwiftStdlib 6.2, *)
func inlineArray<each T>(_ t: repeat each T) {
  let _: InlineArray<1, Any> = [repeat each t]
  // expected-error@-1 {{value pack expansion in an 'InlineArray' literal is not yet supported}}
  let _: InlineArray<3, Any> = [0, repeat each t]
  // expected-error@-1 {{value pack expansion in an 'InlineArray' literal is not yet supported}}
  let _: InlineArray<_, Any> = [repeat each t]
  // expected-error@-1 {{value pack expansion in an 'InlineArray' literal is not yet supported}}
  let _: InlineArray<1, Any>? = [repeat each t]
  // expected-error@-1 {{value pack expansion in an 'InlineArray' literal is not yet supported}}
  takesInlineArray([repeat each t])
  // expected-error@-1 {{value pack expansion in an 'InlineArray' literal is not yet supported}}
}
