// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -module-name Test -experimental-lazy-typecheck

// This file should type check successfully.

// rdar://117442510
public protocol P1 {
  associatedtype Value

  func makeValue() -> Value
  func useProducedValue(_ produceValue: () -> Value)
}

public typealias A1 = S1.Value

public struct S1: P1 {
  public func makeValue() -> Int { return 1 }
  public func useProducedValue(_ produceValue: () -> Value) {
    _ = produceValue()
  }
}

// rdar://56672411
public protocol P2 {
  associatedtype X = Int
  func foo(_ x: X)
}

public typealias A2 = S2.X

public struct S2: P2 {
  public func bar(_ x: X) {}
  public func foo(_ x: X) {}
}

// https://github.com/apple/swift/issues/57355
public protocol P3 {
  associatedtype T
  var a: T { get }
  var b: T { get }
  var c: T { get }
}

public typealias A3 = S3.T

public struct S3: P3 {
  public let a: Int
  public let b: T
  public let c: T
}

// Regression tests
public protocol P4 {
  associatedtype A
  func f(_: A)
}

public typealias A = Int

public typealias A4 = S4.A

public struct S4: P4 {
  public func f(_: A) {}
}

public typealias A5 = OuterGeneric<Int>.Inner.A

public struct OuterGeneric<A> {
  public struct Inner: P4 {
    public func f(_: A) {  }
  }
}

public typealias A6 = OuterNested.Inner.A

public struct OuterNested {
  public struct A {}

  public struct Inner: P4 {
    public func f(_: A) {}
  }
}

public protocol CaseProtocol {
  associatedtype A = Int
  static func a(_: A) -> Self
  static func b(_: A) -> Self
  static func c(_: A) -> Self
}

public typealias A7 = CaseWitness.A

public enum CaseWitness: CaseProtocol {
  case a(_: A)
  case b(_: A)
  case c(_: A)
}

// rdar://119499800 #1
public typealias A8 = Batch.Iterator

public struct Batch: Collection {
  public typealias Element = Int
  public typealias Index = Array<Element>.Index

  var elements: [Element]

  init(_ elements: some Collection<Element>) {
    self.elements = Array(elements)
  }

  public var startIndex: Index { return elements.startIndex }
  public var endIndex: Index { return elements.endIndex }

  public subscript(index: Index) -> Iterator.Element {
    return elements[index]
  }

  public func index(after i: Index) -> Index {
    return elements.index(after: i)
  }
}

// rdar://119499800 #2
public typealias A9 = LogTypes.RawValue

public struct LogTypes: OptionSet {
  public init(rawValue: Self.RawValue) {
    self.rawValue = rawValue
  }

  public let rawValue: Int
}

// rdar://120743365
public struct G<T> {}

public protocol HasAlias {
  typealias A = G<Self>
  associatedtype B

  func f1(_: Self.A, _: Self.B)
  func f2(_: Self.A, _: Self.B)
}

public struct ConformsHasAlias: HasAlias {
  public func f1(_: Self.A, _: Self.B) {}
  public func f2(_: Self.A, _: Int) {}
}

public protocol P10a {
  associatedtype R
}
public protocol P10b {
  associatedtype T: P10a where T == T.R
  var a: T { get }
  var b: T { get }
  var c: T { get }
}
public struct Conformer10: P10b {
  public struct A: P10a {
    public typealias R = A
  }

  public let a: A
  public let b: T.R
  public let c: T.R
}

public protocol P11 {
  associatedtype T
  associatedtype U
  var a: T { get }
  var b: T { get }
  var c: T { get }
  var d: U { get }
}
public struct Conformer11a: P11 {
  public struct A<T> {
    public struct B<U> {}
  }

  public let a: A<Int>.B<Int>
  public let b: A<Self.U>.B<Int>
  public let c: A<U>.B<Int>
  public let d: Int
}
public struct Conformer11b: P11 {
  public struct A<T> {
    public struct B<U> {}
  }

  public let a: A<Int>.B<Int>
  public let b: A<Int>.B<U>
  public let c: A<Int>.B<Self.U>
  public let d: Int
}
