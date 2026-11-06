// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -parse-as-library -wmo %s -o /dev/null
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -parse-as-library -wmo -O %s -o /dev/null

// REQUIRES: swift_feature_Embedded

// The same capturing key path pattern used more than once in a module must
// reserve its argument space every time.

public struct S {
  public subscript(i: Int) -> Int { i }
}

@inline(never)
func use(_ k: KeyPath<S, Int>) {}

public func f(_ i: Int) {
  use(\S.[i])
  use(\S.[i])
}

public func g(_ i: Int) {
  use(\S.[i])
}
