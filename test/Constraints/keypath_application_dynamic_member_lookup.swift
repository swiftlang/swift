// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 6

protocol Holder {
  var value: Int { get }
}

protocol Wrapper: Holder {
  associatedtype Base: Holder
  var base: Base { get }
}

extension Wrapper {
  var value: Int { base.value }
}

@dynamicMemberLookup
struct Layer<Base: Holder>: Wrapper {
  var base: Base

  subscript<T>(dynamicMember keyPath: KeyPath<Base, T>) -> T {
    base[keyPath: keyPath]
  }
}

// Direct key path application must type-check while exploring chained dynamic
// member lookup candidates.
func testChainedWrapperForwarding() {
  struct InputBasic: Holder {
    var value: Int
  }
  typealias Basic = Layer<InputBasic>

  @dynamicMemberLookup
  struct InputA: Wrapper {
    var base: Basic

    subscript<T>(dynamicMember keyPath: KeyPath<Basic, T>) -> T {
      base[keyPath: keyPath]
    }
  }

  @dynamicMemberLookup
  struct InputB: Wrapper {
    var base: InputA

    subscript<T>(dynamicMember keyPath: KeyPath<InputA, T>) -> T {
      base[keyPath: keyPath]
    }
  }

  let input = InputB(base: InputA(base: Basic(base: InputBasic(value: 42))))
  let _: Int = input[keyPath: \.value]
  let _: Int = input[keyPath: \.base.base.base.value]
}

struct LiteralRoot {
  var value: Int
}

class LiteralBase {
  var value: Int = 0
}

class LiteralDerived: LiteralBase {}

// Infer the root of key path literals in both read and write applications.
func testLiteralRootInference(
  _ root: LiteralRoot, _ array: [Int], _ derived: LiteralDerived
) {
  let _: Int = root[keyPath: \.value]
  let _: Int = array[keyPath: \.[0]]
  let _: Int = derived[keyPath: \.value]

  var mutableRoot = root
  mutableRoot[keyPath: \.value] = 42
  var mutableArray = array
  mutableArray[keyPath: \.[0]] = 42
  derived[keyPath: \.value] = 42
}
