protocol Indexable {
  typealias Element
  typealias Index : Forward
  func begin() -> Index
  func end() -> Index
  func __getitem__(i: Index) -> Element
}

// FIXME: Ideally we'd be able to use a generic IndexableEnumerator
// like this one.  Instead, in generic contexts, we use
// ForwardIndexableEnumerator et al, below. See
// <rdar://problem/13997019> cluster of crashes.
// 
struct IndexableEnumerator<
  Seq: Indexable, Indices: Enumerable
    requires Seq.Index == Indices.EnumeratorType.Element,
             Seq.Index == Seq.Index.Self
> : Enumerator, Enumerable {
  var seq : GenericIVar<Seq>
  var indices : GenericIVar<Indices.EnumeratorType>

  typealias Element = Seq.Element
  func isEmpty() -> Bool {
    return indices.value.isEmpty()
  }

  func next() -> Element {
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
   Seq: Indexable requires Seq.Index.Self == Seq.Index
> : Enumerator, Enumerable {
  typealias Element = Seq.Element
  typealias IndexEnumerator = RangeEnumerator<Seq.Index>
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
    return seq.value.__getitem__(indices.value.next())
  }

  // Every Enumerator is also a single-pass Enumerable
  typealias EnumeratorType = ForwardIndexableEnumerator<Seq>
  func getEnumeratorType() -> EnumeratorType {
    return this
  }
}
