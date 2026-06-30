// RUN: %target-typecheck-verify-swift -enable-experimental-feature ImplicitMemberOnFunctionType

// REQUIRES: swift_feature_ImplicitMemberOnFunctionType

// Implicit member syntax (the leading dot) may look through a function-typed
// expected type '(P...) -> R' to a static member of the return type 'R'.

struct Foo<T> {
  init(_ t: T) {}
  init<P>(_ t: (P) -> T) {}
}

enum X { case a; case b(Int) }

let a1 = Foo<X>(.a)         // uses init(_: T), .a == X.a
let b1 = Foo<X>(.b)         // uses init<P>(_:), .b == X.b
let b2 = Foo<X>(X.b)        // still works: explicit unapplied case
let b3 = Foo<X> { X.b($0) } // still works: explicit closure

// To verify which overload is selected, use free functions whose overloads
// return distinct types and pin the result with a type annotation. If the
// wrong overload were chosen, the annotation would fail to type-check.
func choose(_: X) -> Int { 0 }
func choose<P>(_: (P) -> X) -> String { "" }

let chosenA: Int = choose(.a)    // value overload: '.a' is a value of type X
let chosenB: String = choose(.b) // function overload: '.b' is '(Int) -> X'
_ = chosenA
_ = chosenB

// The motivating DSL shape: two overloads, one taking an event value and one
// taking an event constructor '(Payload) -> Event'.
enum LifeEvent: Hashable {
  case clear
  case toggleCell(Int)
  case running
}

struct XTransition {
  init(on: LifeEvent, to: LifeEvent) {}
  init<P>(on: @escaping (P) -> LifeEvent, to: LifeEvent) {}
}

_ = XTransition(on: .clear, to: .running)      // value overload
_ = XTransition(on: .toggleCell, to: .running) // constructor overload

// Generalizes to any static member whose unapplied type is a function
// returning the contextual type, e.g. a static factory method.
struct Widget {
  static func make(_ x: Int) -> Widget { Widget() }
}

func build(_: Widget) -> Int { 0 }
func build<P>(_: (P) -> Widget) -> String { "" }

let built: String = build(.make) // .make == Widget.make: (Int) -> Widget
_ = built

// Multiple payload elements resolve through the curried constructor type.
enum Multi { case pair(Int, String) }

func sink(_: Multi) -> Int { 0 }
func sink<A, B>(_: (A, B) -> Multi) -> String { "" }

let sunk: String = sink(.pair) // .pair == Multi.pair: (Int, String) -> Multi
_ = sunk

// The new candidate is *strictly lower priority* than a member found directly
// on the expected type. When the same name exists both as a value and as a
// function on the return type, matched against value-vs-function overloads, the
// value interpretation always wins and the program stays unambiguous.
struct Both {
  static var thing: Both { Both() }
  static func thing(_: Int) -> Both { Both() }
}

func pick(_: Both) -> Int { 0 }
func pick<P>(_: (P) -> Both) -> String { "" }

// No contextual type is supplied to the call, so the result is decided purely
// by ranking. The value member is strictly higher priority than the
// function-result member, so this is unambiguous and resolves to the 'Int'
// (value) overload. If the function-result candidate were not lower priority,
// this would be ambiguous, or 'picked' would be 'String' and the annotation
// below would fail.
let picked = pick(.thing)
let _: Int = picked

// A plain non-function contextual type is unaffected: '.a' still resolves
// directly against 'X'.
func takesX(_: X) {}
takesX(.a)
