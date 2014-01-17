struct StringBufferIVars {
  init() {
    used = .null()
    capacity = .null()
  }

  init(
    used: UnsafePointer<UTF16.CodeUnit>,
    capacity: UnsafePointer<UTF16.CodeUnit>
  ) {
    self.used = used
    self.capacity = capacity
  }
  
  var used, capacity: UnsafePointer<UTF16.CodeUnit>
}

// FIXME: Wanted this to be a subclass of
// HeapBuffer<StringBufferIVars,UTF16.CodeUnit>, but
// <rdar://problem/15520519> (Can't call static method of derived
// class of generic class with dependent argument type) prevents it.
struct StringBuffer {
  typealias _Storage = HeapBuffer<StringBufferIVars, UTF16.CodeUnit>

  @conversion
  func __conversion() -> _Storage {
    return _storage
  }
  
  init(storage: _Storage) {
    _storage = storage
  }
  
  init(capacity: Int, initialSize: Int = 0) {
    _storage = _Storage.create(StringBufferIVars(), capacity)
    self.used = _storage.elementStorage + initialSize
    self.capacity = _storage.elementStorage + capacity
  }

  init<
    Encoding: UnicodeCodec, Input: Collection
  where Input.StreamType.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.metatype, input: Input, minimumCapacity: Int = 0
  ) {
    // Determine how many UTF16 code units we'll need
    var utf16Count = 0
    transcode(encoding, UTF16, input.generate(), SinkOf<UTF16.CodeUnit>({
       _ in ++utf16Count;()
    }))

    // Allocate storage
    self = StringBuffer(max(utf16Count, minimumCapacity))

    var used = self.used
    // Fill the storage
    transcode(encoding, UTF16, input.generate(), SinkOf<UTF16.CodeUnit>( {
      (used++).set($0)
    } ))
    self.used = used
  }

  var start : UnsafePointer<UTF16.CodeUnit> {
    return _storage.elementStorage
  }
  
  var used : UnsafePointer<UTF16.CodeUnit> {
    return _storage.value.used
  set(newValue):
    _storage.value.used = newValue
  }

  var capacity : UnsafePointer<UTF16.CodeUnit> {
    return _storage.value.capacity
  set(newValue):
    _storage.value.capacity = newValue
  }

  @mutating
  func grow(
    oldUsed: UnsafePointer<UTF16.CodeUnit>,
    newUsed: UnsafePointer<UTF16.CodeUnit>
  ) -> Bool {
    if capacity < newUsed {
      return false
    }
    // FIXME: this function is currently NOT THREADSAFE.  The test +
    // assignment below should be replaced by a CAS, or we can fall back to
    // checking isUniquelyReferenced, which is more conservative.
    if used == oldUsed {
      used = newUsed
      return true
    }
    return false
  }

  @conversion
  func __conversion() -> AnyObject {
    return _storage
  }
  
  var _storage: HeapBuffer<StringBufferIVars, UTF16.CodeUnit>
}
