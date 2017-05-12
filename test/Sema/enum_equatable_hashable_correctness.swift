// RUN: %target-run-simple-swift

// REQUIRES: executable-test

enum Token: Hashable {
  case string(String)
  case number(Int)
  case comma
  case colon
}

guard Token.string("foo") == .string("foo") else { fatalError() }
guard Token.string("foo") != .string("bar") else { fatalError() }
guard Token.string("foo") != .number(5) else { fatalError() }
guard Token.string("foo") != .comma else { fatalError() }
guard Token.string("foo") != .colon else { fatalError() }
guard Token.number(10) == .number(10) else { fatalError() }
guard Token.number(10) != .number(20) else { fatalError() }
guard Token.number(10) != .comma else { fatalError() }
guard Token.number(10) != .colon else { fatalError() }
guard Token.comma == .comma else { fatalError() }
guard Token.comma != .colon else { fatalError() }
guard Token.comma == .comma else { fatalError() }
_ = Token.string("foo").hashValue
_ = Token.number(5).hashValue
_ = Token.comma.hashValue
_ = Token.colon.hashValue

// Verify that the hash value also depends on the payload. This depends on
// _mixInt not giving us a collision for the values we use below.
struct FixedHashValue: Hashable {
  let hashValue: Int
}
enum Foo: Hashable {
  case a(FixedHashValue)
}
guard Foo.a(FixedHashValue(hashValue: 1)) != .a(FixedHashValue(hashValue: 2)) else { fatalError() }

// Try something a little more complex.
enum Combo<T: Hashable, U: Hashable>: Hashable {
  case none
  case first(T)
  case second(U)
  case both(T, U)
}

guard Combo<String, Int>.none == .none else { fatalError() }
guard Combo<String, Int>.none != .first("foo") else { fatalError() }
guard Combo<String, Int>.none != .second(5) else { fatalError() }
guard Combo<String, Int>.none != .both("foo", 5) else { fatalError() }
guard Combo<String, Int>.first("a") == .first("a") else { fatalError() }
guard Combo<String, Int>.first("a") != .first("b") else { fatalError() }
guard Combo<String, Int>.first("a") != .second(5) else { fatalError() }
guard Combo<String, Int>.second(3) != .second(5) else { fatalError() }
guard Combo<String, Int>.second(5) == .second(5) else { fatalError() }
guard Combo<String, Int>.both("foo", 5) == .both("foo", 5) else { fatalError() }
guard Combo<String, Int>.both("bar", 5) != .both("foo", 5) else { fatalError() }
guard Combo<String, Int>.both("foo", 3) != .both("foo", 5) else { fatalError() }
guard Combo<String, Int>.both("bar", 3) != .both("foo", 5) else { fatalError() }
_ = Combo<String, Int>.none.hashValue
_ = Combo<String, Int>.first("a").hashValue
_ = Combo<String, Int>.second(5).hashValue
_ = Combo<String, Int>.both("foo", 5).hashValue

// Make sure that if the user overrides the synthesized member, that one gets
// used instead.
enum Overrides: Hashable {
  case a(Int), b(String)
  var hashValue: Int { return 2 }
  static func == (lhs: Overrides, rhs: Overrides) -> Bool { return true }
}
guard Overrides.a(4) == .b("foo") else { fatalError() }
guard Overrides.a(4).hashValue == 2 else { fatalError() }
guard Overrides.b("foo").hashValue == 2 else { fatalError() }

// ...even in an extension.
enum OverridesInExtension: Hashable {
  case a(Int), b(String)
}
extension OverridesInExtension {
  var hashValue: Int { return 2 }
  static func == (lhs: OverridesInExtension, rhs: OverridesInExtension) -> Bool { return true }
}
guard OverridesInExtension.a(4) == .b("foo") else { fatalError() }
guard OverridesInExtension.a(4).hashValue == 2 else { fatalError() }
guard OverridesInExtension.b("foo").hashValue == 2 else { fatalError() }
