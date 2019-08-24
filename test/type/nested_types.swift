// RUN: %target-typecheck-verify-swift

struct X {
  typealias MyInt = Int
  func getInt() -> MyInt { return 7 }
}

extension X {
  typealias MyReal = Double
  func getFloat() -> MyReal { return 3.14 }
}

protocol MyIteratorProtocol {}
protocol MySequence {
  associatedtype Iterator : MyIteratorProtocol
  func makeIterator() -> Iterator
}

struct IteratorWrapper<I : MyIteratorProtocol> {
  var index: Int
  var elements: I
}

struct SequenceWrapper<T : MySequence> {
  var input : T

  typealias Iterator = IteratorWrapper<T.Iterator>
  func makeIterator() -> Iterator {
    return Iterator(index: 0, elements: input.makeIterator())
  }
}
