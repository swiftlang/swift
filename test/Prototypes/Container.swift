// RUN: %swift -i %s | FileCheck %s

// FIXME: Workaround for <rdar://problem/14396120> Recursive protocol
// requirements.  Everything in _Stream would normally go in Stream.
// Don't use this protocol directly!
protocol _Stream {
  typealias Element
  func next() -> Element?
  func generate() -> Self /* { return self } */
}

/// \brief The "for...in" loop operates on Sequences.  It is
/// unspecified whether "for...in" consumes the sequence on which it
/// operates
protocol Sequence {
  // FIXME: Pending <rdar://problem/14396120>, replace _Stream with
  // Stream below
  typealias StreamType : _Stream

  // "for...in" calls "generate" (name subject to change) on its
  // Sequence to produce a Stream, which it consumes in iteration.
  func generate() -> StreamType
}

/// \brief A Stream is a Sequence that is consumed when iterated
protocol Stream : _Stream, Sequence {
/* FIXME: Pending <rdar://problem/14396120>, inherit _Stream and
   implement nothing here.
  typealias Element
  func next() -> Element?
  func generate() -> Self /* { return self } */
*/
}

/// \brief A Container is a Sequence that is not consumed by
/// iteration; it is also indexable.
protocol Container : Sequence {
  typealias IndexType : ForwardIndex
  
  func startIndex() -> IndexType
  func endIndex() -> IndexType

  // FIXME: Workaround for <rdar://problem/13944228> subscript
  // declaration in protocol unimplemented
  static func _subscript(target: Self, i: IndexType) -> StreamType.Element
}

// ------ demonstration -------

/// \brief count the number of elements in a sequence
func count<S: Sequence>(x: S) -> Int {
  var stream = x.generate()
  var count = 0
  while stream.next() {
    ++count
  }
  return count
}

/// \brief return an Array containing the elements of C in reverse
func reverse<
  C: Container
    where C.IndexType : BidirectionalIndex
>(x: C) -> C.StreamType.Element[] {
  
  var result = Array<C.StreamType.Element>()
  var i = x.endIndex()
  while i != x.startIndex() {
    result.append(C._subscript(x, --i))
  }
  return result
}

/// \brief A stream type that could serve for a Container given that
/// it already had an IndexType.  Because of <rdar://problem/14396120>
/// we'd need to factor _Container out of Container to make it useful,
/// though.
struct ContainedStream<C: Container> : Stream {
  func generate() -> ContainedStream {
    return self
  }
  
  func next() -> C.StreamType.Element? {
    return _position == _elements.endIndex() 
      ? .None : .Some(C._subscript(_elements, _position++))
  }
  var _elements: C
  var _position: C.IndexType
}

/// \brief A wrapper for a BidirectionalIndex that reverses its
/// direction of traversal
struct ReverseIndex<I: BidirectionalIndex> : BidirectionalIndex {
  var _base: I
  
  func succ() -> ReverseIndex {
    return ReverseIndex(_base.pred())
  }
  
  func pred() -> ReverseIndex {
    return ReverseIndex(_base.succ())
  }

  func __equal__(rhs: ReverseIndex) -> Bool {
    return _base == rhs._base
  }
}

/*
Almost works; see <rdar://problem/15609596>

struct Reverse<T: Container where T.IndexType: BidirectionalIndex> : Container {
  typealias IndexType = ReverseIndex<T.IndexType>
  // typealias StreamType = ContainedStream<Reverse>
  
  func generate() -> StreamType {
    return StreamType(_base, _base.endIndex())
  }
  
  struct StreamType : Stream {
    typealias Element = T.StreamType.Element
    
    func generate() -> StreamType {
      return self
    }

    func next() -> T.StreamType.Element? {
      return _position == _elements.startIndex() ? .None : .Some(T._subscript(_elements, --position))
    }
    var _elements: T
    var _position: T.IndexType
  }
  
  func startIndex() -> IndexType {
    return ReverseIndex(_base.endIndex())
  }
  
  func endIndex() -> IndexType {
    return ReverseIndex(_base.startIndex())
  }

  static func _subscript(target: Reverse, i: IndexType) -> StreamType.Element {
    return T._subscript(base, i._base.pred())
  }
  
  var _base: T
}

*/

// CHECK: done!
println("done!")
