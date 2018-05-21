// RUN: %target-swift-emit-silgen -module-name Swift -parse-stdlib -parse-as-library -enable-sil-ownership %s

// This files contains a minimal implementation for Swift to emit foreach loops
// for a type. It acts partially as a guide for users and since it is in the
// form of a test, it ensures that we will always be able to get this test
// through the type checker.

precedencegroup AssignmentPrecedence { assignment: true }

public protocol ExpressibleByNilLiteral {
  init(nilLiteral: ())
}

protocol IteratorProtocol {
  associatedtype Element
  mutating func next() ->  Element?
}

protocol Sequence {
  associatedtype Element
  associatedtype Iterator : IteratorProtocol where Iterator.Element == Element

  func makeIterator() -> Iterator
}

enum Optional<T> {
case none
case some(T)
}

func _diagnoseUnexpectedNilOptional(_filenameStart: Builtin.RawPointer,
                                    _filenameLength: Builtin.Word,
                                    _filenameIsASCII: Builtin.Int1,
                                    _line: Builtin.Word) {
  // This would usually contain an assert, but we don't need one since we are
  // just emitting SILGen.
}

extension Optional : ExpressibleByNilLiteral {
  public init(nilLiteral: ()) {
    self = .none
  }
}

class FakeCollection<T> {
}

struct FakeCollectionIterator<T> {
  weak var collection: FakeCollection<T>?

  init(_ newCollection: FakeCollection<T>) {
    collection = newCollection
  }
}

extension FakeCollectionIterator : IteratorProtocol {
  public typealias Element = T
  public mutating func next() -> Element? {
    return .none
  }
}

extension FakeCollection : Sequence {
  public typealias Element = T
  public typealias Iterator = FakeCollectionIterator<T>
  public func makeIterator() -> FakeCollectionIterator<T> {
    return FakeCollectionIterator(self)
  }
}

func useT<T>(_ t: T) {}

func iterateFakeCollection<T>(_ x: FakeCollection<T>) {
  for y in x {
    useT(y)
  }
}
