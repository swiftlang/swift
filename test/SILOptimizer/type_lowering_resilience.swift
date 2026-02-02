// RUN: %target-swift-frontend -emit-sil -sil-verify-all -O %S/Inputs/type_lowering_resilience_other.swift -primary-file %s -enable-library-evolution

@inline(__always) public func generic<T>(_ t: T) {
  _ = Holder<T>(t)
}

public func concrete() {
  generic(Inner())
}

// =============================================================================
// Test self-recursion in TypeConverter::getTypeLowering.

// -----------------------------------------------------------------------------
// Lowering: Stuff -> Array -> Stuff
//
// The toArray() implementation forces SILGen to lower the type.

enum Stuff {
  case array([Stuff])
  case any(Any)

  func toArray() throws -> [Stuff] {
    guard case .array(let array) = self else { return [] }
    return array
  }
}

// -----------------------------------------------------------------------------
// Lowering: StructOfArray -> Array -> Member -> StructOfArray
//
// Lowering the generic parameter of a bound Array is needed to determine whether the type has a custom deinit. But the
// self-recursion does not indicate that StructOfArray requires indirect storage of the members field. And StructOfArray
// should still be considered a trivial type.
//
// The additional Int field and enums types here are all necessary to force SIL verification to break if TypeLowering
// incorrectly caches a nontrivial lowering of StructOfArray.

public struct StructOfArray {
  public var start: Int
  public var members: [Member]

  public init(
    _ start: Int,
    _ members: [Member],
  ) {
    self.start = start
    self.members = members
  }

  public enum Member {
    case custom(StructOfArray)
  }
}

public indirect enum EnumOfStructOfArray {
  case structOfArray(StructOfArray)

  var _associatedValue: StructOfArray {
    switch self {
    case let .structOfArray(v):  return v
    }
  }
}

