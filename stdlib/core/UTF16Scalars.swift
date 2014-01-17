func ==(lhs: UTF16Scalars.IndexType, rhs: UTF16Scalars.IndexType) -> Bool {
  return lhs._position == rhs._position
}

struct UTF16Scalars : Sliceable, Sequence {

  // FIXME: This index should probably become bidirectional, as UTF16
  // is traversable in either direction.
  struct IndexType : ForwardIndex {
    func succ() -> IndexType {
      var i = _position
      UTF16.decode { _base[i++] }
      return IndexType(i, _base)
    }
    
    var _position: Int
    var _base: ContiguousString
  }

  func startIndex() -> IndexType {
    return IndexType(_base.startIndex(), _base)
  }
  
  func endIndex() -> IndexType {
    return IndexType(_base.endIndex(), _base)
  }
  
  func __getitem__(i: IndexType) -> UnicodeScalar {
    var scan = i
    return UTF16.decode({ _base[scan._position++] })!
  }

  func __slice__(start: IndexType, end: IndexType) -> UTF16Scalars {
    return UTF16Scalars(_base[start._position..end._position])
  }

  subscript(i: IndexType) -> UnicodeScalar {
    return __getitem__(i)
  }

  subscript(r: Range<IndexType>) -> UTF16Scalars {
    return __slice__(r.startIndex(), r.endIndex())
  }

  struct StreamType : Stream {
    @mutating
    func next() -> UnicodeScalar? {
      return UTF16.decode({ _base.next() })
    }
    var _base: ContiguousString.StreamType
  }
  
  func generate() -> StreamType {
    return StreamType(_base.generate())
  }

  @conversion
  func __conversion() -> String {
    return String(_base)
  }
  
  var _base: ContiguousString
}
