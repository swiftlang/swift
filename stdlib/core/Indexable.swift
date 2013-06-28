/// \brief An Indexable is a multi-pass collection.  FIXME: every
/// Indexable should be enumerable (<rdar://problem/14016705>)
protocol Indexable {
  typealias Element
  typealias IndexType : ForwardIndex
  func begin() -> IndexType
  func end() -> IndexType
  func __getitem__(i: IndexType) -> Element
}

func indices<
  Seq: Indexable>(seq: Seq) -> Range<Seq.IndexType> {
  return Range(seq.begin(), seq.end())
}

// FIXME: Ideally we'd be able to use a generic IndexableEnumerator
// like this one.  Instead, in generic contexts, we use
// ForwardIndexableEnumerator et al, below. See
// <rdar://problem/13997019> cluster of crashes.
// 
struct IndexableEnumerator<
  Seq: Indexable, Indices: Enumerable
    requires Seq.IndexType == Indices.EnumeratorType.Element
> : Enumerator, Enumerable {
  var seq : GenericIVar<Seq>
  var indices : GenericIVar<Indices.EnumeratorType>

  typealias Element = Seq.Element
  func isEmpty() -> Bool {
    return indices.value.isEmpty()
  }

  func next() -> Element {
    debugTrap(!isEmpty())
    return seq.value.__getitem__(indices.value.next())
  }

  // Every Enumerator is also a single-pass Enumerable
  typealias EnumeratorType = IndexableEnumerator<Seq,Indices>
  func getEnumeratorType() -> EnumeratorType {
    return this
  }

  constructor(seq: Seq, indices: Indices) {
    this.seq.value = seq
    this.indices.value = indices.getEnumeratorType()
  }
}

// FIXME: Replace with IndexableEnumerator above in generic contexts
// pending <rdar://problem/13997019> cluster of crashes.
struct ForwardIndexableEnumerator<
   Seq: Indexable> : Enumerator, Enumerable {
  typealias Element = Seq.Element
  typealias IndexEnumerator = RangeEnumerator<Seq.IndexType>
  var seq : GenericIVar<Seq>
  var indices : GenericIVar<IndexEnumerator>

  constructor(seq: Seq) {
    this.seq.value = seq
    this.indices.value = IndexEnumerator(seq.begin(), seq.end())
  }

  func isEmpty() -> Bool {
    return indices.value.isEmpty()
  }
  func next() -> Element {
    debugTrap(!isEmpty())
    return seq.value.__getitem__(indices.value.next())
  }

  // Every Enumerator is also a single-pass Enumerable
  typealias EnumeratorType = ForwardIndexableEnumerator<Seq>
  func getEnumeratorType() -> EnumeratorType {
    return this
  }
}

// FIXME: Replace with IndexableEnumerator above in generic contexts
// pending <rdar://problem/13997019> cluster of crashes.
struct ReverseIndexableEnumerator<
   Seq: Indexable requires Seq.IndexType: BidirectionalIndex
> : Enumerator, Enumerable {
  typealias Element = Seq.Element
  typealias IndexEnumerator = ReverseRangeEnumerator<Seq.IndexType>
  var seq : GenericIVar<Seq>
  var indices : GenericIVar<IndexEnumerator>

  constructor(seq: Seq) {
    this.seq.value = seq
    this.indices.value = IndexEnumerator(seq.begin(), seq.end())
  }

  func isEmpty() -> Bool {
    return indices.value.isEmpty()
  }
  func next() -> Element {
    debugTrap(!isEmpty())
    return seq.value.__getitem__(indices.value.next())
  }

  // Every Enumerator is also a single-pass Enumerable
  typealias EnumeratorType = ReverseIndexableEnumerator<Seq>
  func getEnumeratorType() -> EnumeratorType {
    return this
  }
}

/// \brief Adapt an Indexable into an Enumerable
struct IndexableEnumerable<Seq: Indexable> 
  : Enumerable {
  var seq : GenericIVar<Seq>

  typealias EnumeratorType = ForwardIndexableEnumerator<Seq>
  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(this.seq.value)
  }

  constructor(seq: Seq) {
    this.seq.value = seq
  }
}

struct ReverseIndexableEnumerable<
  Seq: Indexable 
  requires Seq.IndexType: BidirectionalIndex
> : Enumerable {
  var seq : GenericIVar<Seq>

  typealias EnumeratorType = ReverseIndexableEnumerator<Seq>
  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(this.seq.value)
  }

  constructor(seq: Seq) {
    this.seq.value = seq
  }
}


func elements<
  Seq: Indexable>(seq: Seq) -> IndexableEnumerable<Seq> {
  return IndexableEnumerable(seq)
}

// FIXME: Several of these overloads can go away when
// rdar://problem/14016705 is addressed.
func reverse<
  Seq: Indexable requires Seq.IndexType: BidirectionalIndex
>(seq: Seq) -> ReverseIndexableEnumerable<Seq> {
  return ReverseIndexableEnumerable(seq)
}

func reverse<
  Seq: Indexable requires Seq.IndexType: BidirectionalIndex
>(e: IndexableEnumerable<Seq>) -> ReverseIndexableEnumerable<Seq> {
  return ReverseIndexableEnumerable(e.seq.value)
}

func reverse<
  Seq: Indexable requires Seq.IndexType: BidirectionalIndex
>(e: ReverseIndexableEnumerable<Seq>) -> IndexableEnumerable<Seq> {
  return IndexableEnumerable(e.seq.value)
}

protocol Sliceable: Indexable {
  func __slice__(start: IndexType, finish: IndexType) -> This
}

func dropFirst<Seq: Sliceable>(seq: Seq) -> Seq {
  return seq.__slice__(seq.begin().succ(), seq.end())
}

func dropLast<
  Seq: Sliceable 
  requires Seq.IndexType: BidirectionalIndex
>(seq: Seq) -> Seq {
  return seq.__slice__(seq.begin(), seq.end().pred())
}
