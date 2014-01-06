/// \brief An Container is a multi-pass collection.  FIXME: every
/// Container should be enumerable (<rdar://problem/14016705>)
protocol Container {
  typealias Element
  typealias IndexType : ForwardIndex

  func startIndex() -> IndexType
  func endIndex() -> IndexType

  func __getitem__(i: IndexType) -> Element
}

func indices<
    Seq : Container>(seq: Seq) -> Range<Seq.IndexType> {
  return Range(seq.startIndex(), seq.endIndex())
}

// FIXME: Ideally we'd be able to use a generic ContainerStream
// like this one.  Instead, in generic contexts, we use
// ForwardContainerStream et al, below. See
// <rdar://problem/13997019> cluster of crashes.
// 
struct ContainerStream<
  Seq: Container, Indices: Sequence
  where Seq.IndexType == Indices.StreamType.Element
  // FIXME: Commenting out "Stream," below causes all kinds of deserialization crashes
> : Stream, Sequence, MultiPassStream {
  var seq : Seq
  var indices : Indices.StreamType

  typealias Element = Seq.Element

  @mutating
  func next() -> Element? {
    var result = indices.next()
    return result ? seq.__getitem__(result!) : .None
  }

  // Every Stream is also a single-pass Sequence
  typealias StreamType = ContainerStream<Seq, Indices>

  func generate() -> StreamType {
    return self
  }

  init(seq: Seq, indices: Indices) {
    self.seq = seq
    self.indices = indices.generate()
  }
}

// FIXME: Replace with ContainerStream above in generic contexts
// pending <rdar://problem/13997019> cluster of crashes.
struct ForwardContainerStream<
   Seq: Container> : Stream, Sequence {
  typealias Element = Seq.Element
  typealias IndexStream = RangeStream<Seq.IndexType>
  var seq : Seq
  var indices : IndexStream

  init(seq: Seq) {
    self.seq = seq
    self.indices = IndexStream(seq.startIndex(), seq.endIndex())
  }

  @mutating
  func next() -> Element? {
    var result = indices.next()
    return result ? seq.__getitem__(result!) : .None
  }

  // Every Stream is also a single-pass Sequence
  typealias StreamType = ForwardContainerStream<Seq>
  func generate() -> StreamType {
    return self
  }
}

// FIXME: Replace with ContainerStream above in generic contexts
// pending <rdar://problem/13997019> cluster of crashes.
struct ReverseContainerStream<
   Seq: Container where Seq.IndexType: BidirectionalIndex
> : Stream, Sequence {
  typealias Element = Seq.Element
  typealias IndexStream = ReverseRangeStream<Seq.IndexType>
  var seq : Seq
  var indices : IndexStream

  init(seq: Seq) {
    self.seq = seq
    self.indices = IndexStream(seq.startIndex(), seq.endIndex())
  }


  @mutating
  func next() -> Element? {
    var result = indices.next()
    return result ? seq.__getitem__(result!) : .None
  }

  // Every Stream is also a single-pass Sequence
  typealias StreamType = ReverseContainerStream<Seq>
  func generate() -> StreamType {
    return self
  }
}

/// \brief Adapt an Container into an Sequence
struct ContainerSequence<Seq: Container> 
  : Sequence {
  var seq : Seq

  typealias StreamType = ForwardContainerStream<Seq>
  func generate() -> StreamType {
    return StreamType(self.seq)
  }

  init(seq: Seq) {
    self.seq = seq
  }
}

struct ReverseContainerSequence<
  Seq: Container 
  where Seq.IndexType: BidirectionalIndex
> : Sequence {
  var seq : Seq

  typealias StreamType = ReverseContainerStream<Seq>
  func generate() -> StreamType {
    return StreamType(self.seq)
  }

  init(seq: Seq) {
    self.seq = seq
  }
}


func elements<
  Seq: Container>(seq: Seq) -> ContainerSequence<Seq> {
  return ContainerSequence(seq)
}

// FIXME: Several of these overloads can go away when
// rdar://problem/14016705 is addressed.
func reverse<
  Seq: Container where Seq.IndexType: BidirectionalIndex
>(seq: Seq) -> ReverseContainerSequence<Seq> {
  return ReverseContainerSequence(seq)
}

func reverse<
  Seq: Container where Seq.IndexType: BidirectionalIndex
>(e: ContainerSequence<Seq>) -> ReverseContainerSequence<Seq> {
  return ReverseContainerSequence(e.seq)
}

func reverse<
  Seq: Container where Seq.IndexType: BidirectionalIndex
>(e: ReverseContainerSequence<Seq>) -> ContainerSequence<Seq> {
  return ContainerSequence(e.seq)
}

protocol Sliceable: Container {
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
  static func sub(lhs: Self, rhs: Self) -> (DistanceType, Bool)
  static func sub(lhs: Self, rhs: DistanceType) -> (Self, Bool)
  static func add(lhs: Self, rhs: DistanceType) -> (Self, Bool)
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
