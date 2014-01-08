protocol _Collection  {
  typealias IndexType : ForwardIndex

  func startIndex() -> IndexType
  func endIndex() -> IndexType

  // The declaration of Element and _subscript here is a trick used to
  // break a cyclic conformance/deduction that Swift can't handle.  We
  // need something other than a Collection.StreamType.Element that can
  // be used as ContainedStream<T>'s Element.  Here we arrange for the
  // Collection itself to have an Element type that's deducible from
  // its __getitem__ function.  Ideally we'd like to constrain this
  // Element to be the same as Collection.StreamType.Element (see
  // below), but we have no way of expressing it today.
  typealias _Element
  func __getitem__(i: IndexType) -> _Element
}

protocol Collection : _Collection, Sequence {
  func __getitem__(i: IndexType) -> StreamType.Element
}

/// \brief A stream type that could serve for a Collection given that
/// it already had an IndexType.  Because of <rdar://problem/14396120>
/// we've had to factor _Collection out of Collection to make it useful
struct ContainedStream<C: _Collection> : Stream {
  init(seq: C) {
    self._elements = seq
    self._position = seq.startIndex()
  }
  
  func generate() -> ContainedStream {
    return self
  }
  
  @mutating
  func next() -> C._Element? {
    return _position == _elements.endIndex() 
    ? .None : .Some(_elements.__getitem__(_position++))
  }
  var _elements: C
  var _position: C.IndexType
}

func indices<
    Seq : Collection>(seq: Seq) -> Range<Seq.IndexType> {
  return Range(seq.startIndex(), seq.endIndex())
}

struct IndexedStream<
  Seq: Collection, Indices: Sequence
  where Seq.IndexType == Indices.StreamType.Element
  // FIXME: Commenting out "Stream," below causes all kinds of deserialization crashes
> : Stream, Sequence, MultiPassStream {
  var seq : Seq
  var indices : Indices.StreamType

  typealias Element = Seq.StreamType.Element

  @mutating
  func next() -> Element? {
    var result = indices.next()
    return result ? seq.__getitem__(result!) : .None
  }

  // Every Stream is also a single-pass Sequence
  typealias StreamType = IndexedStream
  func generate() -> StreamType {
    return self
  }

  init(seq: Seq, indices: Indices) {
    self.seq = seq
    self.indices = indices.generate()
  }
}


/*
/// \brief Adapt an Collection into an Sequence
struct CollectionSequence<Seq: Collection> 
  : Sequence {
  var seq : Seq

  typealias StreamType = ForwardCollectionStream<Seq>
  func generate() -> StreamType {
    return StreamType(self.seq)
  }

  init(seq: Seq) {
    self.seq = seq
  }
}

struct ReverseCollection<
  Seq: Collection 
  where Seq.IndexType: BidirectionalIndex
> : Collection {
  var seq : Seq

  typealias StreamType = CollectionStream<Seq>
  func generate() -> StreamType {
    return StreamType(self.seq)
  }

  init(seq: Seq) {
    self.seq = seq
  }
}

func elements<
  Seq: Collection>(seq: Seq) -> CollectionSequence<Seq> {
  return CollectionSequence(seq)
}

func reverse<
  Seq: Collection where Seq.IndexType: BidirectionalIndex
>(seq: Seq) -> ReverseCollectionSequence<Seq> {
  return ReverseCollectionSequence(seq)
}

func reverse<
  Seq: Collection where Seq.IndexType: BidirectionalIndex
>(e: CollectionSequence<Seq>) -> ReverseCollectionSequence<Seq> {
  return ReverseCollectionSequence(e.seq)
}

func reverse<
  Seq: Collection where Seq.IndexType: BidirectionalIndex
>(e: ReverseCollectionSequence<Seq>) -> CollectionSequence<Seq> {
  return CollectionSequence(e.seq)
}
*/
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
}

func == <I> (lhs: ReverseIndex<I>, rhs: ReverseIndex<I>) -> Bool {
  return lhs._base == rhs._base
}

struct Reverse<T: Collection where T.IndexType: BidirectionalIndex> : Collection {
  typealias IndexType = ReverseIndex<T.IndexType>
  typealias StreamType = ContainedStream<Reverse>
  
  func generate() -> ContainedStream<Reverse> {
    return ContainedStream(self)
  }
  
  func startIndex() -> IndexType {
    return ReverseIndex(_base.endIndex())
  }
  
  func endIndex() -> IndexType {
    return ReverseIndex(_base.startIndex())
  }

  func __getitem__(i: IndexType) -> T.StreamType.Element {
    return _base.__getitem__(i._base.pred())
  }
  
  var _base: T
}

protocol Sliceable: Collection {
  func __slice__(start: IndexType, finish: IndexType) -> Self
}

func dropFirst<Seq : Sliceable>(seq: Seq) -> Seq {
  return seq.__slice__(seq.startIndex().succ(), seq.endIndex())
}

func dropLast<
  Seq: Sliceable 
  where Seq.IndexType: BidirectionalIndex
>(seq: Seq) -> Seq {
  return seq.__slice__(seq.startIndex(), seq.endIndex().pred())
}

protocol ForwardIndex : Equatable {
  func succ() -> Self
}

@prefix @assignment @transparent
func ++ <T : ForwardIndex> (x: @inout T) -> T {
  x = x.succ()
  return x
}

@postfix @assignment @transparent
func ++ <T : ForwardIndex> (x: @inout T) -> T {
  var ret = x
  x = x.succ()
  return ret
}

protocol BidirectionalIndex : ForwardIndex {
  func pred() -> Self
}

@prefix @assignment @transparent
func -- <T: BidirectionalIndex> (x: @inout T) -> T {
  x = x.pred()
  return x
}


@postfix @assignment @transparent
func -- <T: BidirectionalIndex> (x: @inout T) -> T {
  var ret = x
  x = x.pred()
  return ret
}

protocol RandomAccessIndex : BidirectionalIndex, NumericOperations {
  typealias DistanceType
  type func sub(lhs: Self, rhs: Self) -> (DistanceType, Bool)
  type func sub(lhs: Self, rhs: DistanceType) -> (Self, Bool)
  type func add(lhs: Self, rhs: DistanceType) -> (Self, Bool)
  // FIXME: Disabled pending <rdar://problem/14011860> (Default
  // implementations in protocols)
  func <(lhs: Self, rhs: Self) -> Bool /* {
      return (lhs.sub(rhs)).isNegative()

  } */
}

@transparent
func - <T : RandomAccessIndex>(x: T, y: T) -> T.DistanceType {
  var tmp : (T.DistanceType, Bool) = T.sub(x, y)
  alwaysTrap(tmp.1 == false)
  return tmp.0
}

@transparent
func &- <T : RandomAccessIndex>(x: T, y: T) -> T.DistanceType {
  return T.sub(x, y).0
}

@transparent
func - <T : RandomAccessIndex>(x: T, y: T.DistanceType) -> T {
  var tmp = T.sub(x, y)
  alwaysTrap(tmp.1 == false)
  return tmp.0
}

@transparent
func &- <T : RandomAccessIndex>(x: T, y: T.DistanceType) -> T {
  return T.sub(x, y).0
}

@infix @assignment @transparent
func += <T : RandomAccessIndex> (lhs: @inout T, rhs: T.DistanceType) {
  var tmp = T.add(lhs, rhs)
  alwaysTrap(tmp.1 == false)
  lhs = tmp.0
}

@infix @assignment @transparent
func -= <
  T: RandomAccessIndex where T.DistanceType: SignedNumber
> (lhs: @inout T, rhs: T.DistanceType) {
  var tmp = T.add(lhs, -rhs)
  alwaysTrap(tmp.1 == false)
  lhs = tmp.0
}

@transparent
func + <T : RandomAccessIndex> (lhs: T, rhs: T.DistanceType) -> T {
  var tmp = T.add(lhs, rhs)
  alwaysTrap(tmp.1 == false)
  return tmp.0
}

@transparent
func + <T : RandomAccessIndex> (lhs: T.DistanceType, rhs: T) -> T {
  var tmp = T.add(rhs, lhs)
  alwaysTrap(tmp.1 == false)
  return tmp.0
}

@transparent
func &+ <T : RandomAccessIndex> (lhs: T, rhs: T) -> T {
  return T.add(lhs, rhs).0
}

@transparent
func &+ <T : RandomAccessIndex> (lhs: T, rhs: T.DistanceType) -> T {
  return T.add(lhs, rhs).0
}

@transparent
func &+ <T : RandomAccessIndex> (lhs: T.DistanceType, rhs: T) -> T {
  return T.add(rhs, lhs).0
}

@transparent
func - <T : RandomAccessIndex where T.DistanceType : SignedNumber> (
  lhs: T, rhs: T.DistanceType)
-> T {
  var tmp = T.add(lhs, -rhs)
  alwaysTrap(tmp.1 == false)
  return tmp.0
}

@transparent
func &- <T : RandomAccessIndex where T.DistanceType : SignedNumber> (
  lhs: T, rhs: T.DistanceType)
-> T {
  return T.add(lhs, -rhs).0
}
