// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -O -cross-module-optimization -sil-verify-all

// Check that the cross-module-optimization pass can handle instructions which
// operate on local archetypes, like `value_metatype` on an opened existential
// or on an opened pack element.

public protocol P {
  static var name: String { get }
}

public struct S1: P { public static var name: String { "S1" } }
public struct S2: P { public static var name: String { "S2" } }

// The `value_metatype` operates on an opened pack element archetype.
public func namesOfPack<each T: P>(_ values: repeat each T) -> String {
  var s = ""
  for value in repeat each values {
    s += type(of: value).name
  }
  return s
}

public func callNamesOfPack() -> String {
  return namesOfPack(S1(), S2())
}

@inline(__always)
func nameOfValue<T: P>(_ value: T) -> String {
  return type(of: value).name
}

// After inlining `nameOfValue`, the `value_metatype` operates on an opened
// existential archetype.
public func nameOfExistential(_ value: any P) -> String {
  return nameOfValue(value)
}
