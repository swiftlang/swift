// RUN: %target-typecheck-verify-swift

public protocol Collection {
  associatedtype Index
}

public protocol BidirectionalCollection: Collection {}

public protocol RandomAccessCollection: BidirectionalCollection {}

public protocol Numeric {
  associatedtype Magnitude
}

public protocol SignedNumeric: Numeric {}

public protocol BinaryInteger: Numeric {}

public protocol SignedInteger: BinaryInteger, SignedNumeric {}

struct S<X> where X : RandomAccessCollection, X.Index : SignedInteger {}
