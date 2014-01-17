// A slice of UTF16 code units stored contiguously in memory
struct ContiguousString {
  
  enum Owner {
  case Native(StringBuffer._Storage)
  case Cocoa(_CocoaString)
  case None
  }
  
  init() {
    start = .null()
    count = 0
    owner = .None
  }

  init(buffer: StringBuffer._Storage) {
    self.owner = .Native(buffer)
    self.start = buffer.elementStorage
    self.count = buffer.value.used - start
  }
  
  init(owner: Owner, start: UnsafePointer<UInt16>, count: Int) {
    self.start = start
    self.count = count
    self.owner = owner
  }

  var owner: Owner
  var start: UnsafePointer<UInt16>
  var count: Int
}

extension ContiguousString : Collection {
  func startIndex() -> Int {
    return 0
  }
  func endIndex() -> Int {
    return count
  }

  subscript(i: Int) -> UTF16.CodeUnit {
    assert(i < count)
    return (start + i).get()
  }
  
  func __getitem__(i: Int) -> UTF16.CodeUnit {
    return self[i]
  }

  typealias StreamType = ContainedStream<ContiguousString>
  func generate()
    -> ContainedStream<ContiguousString>
  {
    return ContainedStream(self)
  }
}

extension ContiguousString : Sliceable {
  subscript(slice: Range<Int>) -> ContiguousString {
    var r: Range<Int> = slice
    assert(r.endIndex() <= count)
    return ContiguousString(
      owner: owner, 
      start: start + r.startIndex(), 
      count: r.endIndex() - r.startIndex())
  }
  
  func __slice__(start: Int, finish: Int) -> ContiguousString {
    return self[start..finish]
  }
}
