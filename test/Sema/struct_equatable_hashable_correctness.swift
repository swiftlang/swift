// RUN: %target-run-simple-swift

// REQUIRES: executable-test

struct Value: Hashable {
  let v: Int
}

guard Value(v: 1) == Value(v: 1) else { fatalError() }
guard Value(v: 1) != Value(v: 2) else { fatalError() }
_ = Value(v: 1).hashValue
_ = Value(v: 2).hashValue

// Try something a little more complex.
struct Pair<T: Hashable, U: Hashable>: Hashable {
  let a: T
  let b: U
}
typealias PSI = Pair<String, Int>

guard PSI(a: "foo", b: 0) == PSI(a: "foo", b: 0) else { fatalError() }
guard PSI(a: "foo", b: 0) != PSI(a: "foo", b: 5) else { fatalError() }
guard PSI(a: "foo", b: 0) != PSI(a: "bar", b: 0) else { fatalError() }
_ = PSI(a: "foo", b: 0).hashValue

// Make sure that if the user overrides the synthesized member, that one gets
// used instead.
struct Overrides: Hashable {
  let a: Int
  var hashValue: Int { return 2 }
  static func == (lhs: Overrides, rhs: Overrides) -> Bool { return true }
}
guard Overrides(a: 4) == Overrides(a: 5) else { fatalError() }
guard Overrides(a: 4).hashValue == 2 else { fatalError() }
guard Overrides(a: 5).hashValue == 2 else { fatalError() }

// ...even in an extension.
struct OverridesInExtension: Hashable {
  let a: Int
}
extension OverridesInExtension {
  var hashValue: Int { return 2 }
  static func == (lhs: OverridesInExtension, rhs: OverridesInExtension) -> Bool { return true }
}
guard OverridesInExtension(a: 4) == OverridesInExtension(a: 5) else { fatalError() }
guard OverridesInExtension(a: 4).hashValue == 2 else { fatalError() }
guard OverridesInExtension(a: 5).hashValue == 2 else { fatalError() }
