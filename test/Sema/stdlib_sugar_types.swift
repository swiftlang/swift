// Test using type sugar before the types are defined.

// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-name Swift -parse-as-library %s

struct Dummy {}

let a: Dummy? = .none
let b: Dummy! = .none
let c: [Dummy] = .init()
let d: [Dummy: Dummy] = .init()

enum Optional<Wrapped> {
  case none
  case some(Wrapped)
}

struct Array<Element> {}

struct Dictionary<Key, Value> {}
