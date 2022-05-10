// RUN: %target-typecheck-verify-swift

// This test was reduced from the NonEmpty open-source project; it motivated enabling
// loop normalization by default, since without it, it takes too long to complete.

public protocol WithMinimumCount {}

public protocol NonEmptyProtocol: Swift.Collection, RawRepresentable, WithMinimumCount
where Element == RawValue.Element,
      Index == RawValue.Index,
      Collection == RawValue
{
  associatedtype Collection: Swift.Collection
}

struct G<T>
where T : NonEmptyProtocol,
      T.Collection : NonEmptyProtocol,
      T.Collection.Collection : NonEmptyProtocol,
      T.Collection.Collection.Collection : NonEmptyProtocol,
      T.Collection.Collection.Collection.Collection : NonEmptyProtocol,
      T.Collection.Collection.Collection.Collection.Collection : NonEmptyProtocol,
      T.Collection.Collection.Collection.Collection.Collection.Collection : NonEmptyProtocol,
      T.Collection.Collection.Collection.Collection.Collection.Collection.Collection : NonEmptyProtocol,
      T.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection : NonEmptyProtocol,
      T.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection : NonEmptyProtocol,
      T.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection : NonEmptyProtocol,
      T.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection.Collection : NonEmptyProtocol {}
