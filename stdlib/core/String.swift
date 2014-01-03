/// FIXME: Lots of bounds checking is missing here ////

/// \brief An immutable UTF16 store.  In practice, this will be an
/// immutable NSString that doesn't provide a contiguous buffer of
/// UTF16 (see CFStringGetCharactersPtr).
@class_protocol 
protocol OpaqueUTF16Buffer {
  /// \brief Convert the given subrange to a ContiguousUTF16Slice
  func contiguousSlice(range: Range<Int>, minimumCapacity: Int) -> ContiguousUTF16Slice

  // DANGER: expects a pointer to count() elements worth of storage
  func _read(buffer: UnsafePointer<UTF16.CodeUnit>)
  
  /// \brief Say how many UTF16 code units we store
  func count() -> Int
}


var _EmptyOpaqueUTF16Buffer_instance = EmptyOpaqueUTF16Buffer()

class EmptyOpaqueUTF16Buffer : OpaqueUTF16Buffer {
  
  static func get() -> EmptyOpaqueUTF16Buffer {
    return _EmptyOpaqueUTF16Buffer_instance
  }

  func contiguousSlice(range: Range<Int>, minimumCapacity: Int) -> ContiguousUTF16Slice {
    return ContiguousUTF16Slice()
  }

  func _read(_:UnsafePointer<UTF16.CodeUnit>) {}

  func count() -> Int {
    return 0
  }
  
}

/// \brief A value type holding a subrange of an OpaqueUTF16Buffer
struct OpaqueUTF16Slice {
  /// \brief A slice comprising the whole string
  init(source: OpaqueUTF16Buffer) {
    self.buffer = source
    self.range = Range(0, source.count())
  }

  /// \brief Construct a slice of source bounded by subRange
  init(source: OpaqueUTF16Slice, subRange: Range<Int>) {
    buffer = subRange.isEmpty() ? EmptyOpaqueUTF16Buffer.get() : source.buffer
    self.range = subRange
  }

  subscript(bounds: Range<Int>) -> OpaqueUTF16Slice {
    return OpaqueUTF16Slice(self, bounds)
  }

  /// \brief Convert this whole slice to a contiguous representation
  /// with storage for at least minimumCapacity UTF16 code units.
  /// For efficiency, consider dispatching to Cocoa functions that
  /// accept a range argument, rather than calling this function and
  /// operating on that slice.
  func contiguousSlice(minimumCapacity: Int = 0) -> ContiguousUTF16Slice {
    return buffer.contiguousSlice(range, minimumCapacity)
  }
  
  /// \brief Convert a subRange of this slice to a contiguous
  /// representation.  with storage for at least minimumCapacity UTF16
  /// code units. For efficiency, consider dispatching to Cocoa
  /// functions rather than calling this function and operating on
  /// that slice.
  func contiguousSlice(subRange: Range<Int>, minimumCapacity: Int = 0) -> ContiguousUTF16Slice {
    return buffer.contiguousSlice(subRange, minimumCapacity)
  }
  
  var buffer: OpaqueUTF16Buffer
  var range: Range<Int>
}

// N.B. Only NSString should ever conform to _CocoaString!
//
// This is a hack used to decouple the core stdlib from Foundation
// while still remaining relatively typesafe.  The idea is that we can
// store an NSString in one of these and then brutally bit-cast it to
// any specific _CocoaString type when that's what we need.  This cast
// can be safe only as long as there is only one model of _CocoaString
@class_protocol, @objc
protocol _CocoaString {}

// A slice of UTF16 code units stored contiguously in memory
struct ContiguousUTF16Slice {
  
  enum Owner : LogicValue {
  case Native(StringBuffer._Storage)
  case Cocoa(_CocoaString)
  case Any(AnyObject)
  case None

    @conversion
    func __conversion<T: _CocoaString>() -> T? {
      switch(self) {
      case Cocoa(var nsString):
        return .Some(
          Builtin.castFromObjectPointer(
            Builtin.castToObjectPointer(nsString)
          ))
      default:
        return .None
      }
    }

    func getLogicValue() -> Bool {
      switch(self) {
        case None: return false
        default: return true
      }
    }
  }
  
  init() {
    start = UnsafePointer<UInt16>.null()
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

extension ContiguousUTF16Slice : Indexable, Sequence {
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

  func enumerate()
    -> IndexableGenerator<ContiguousUTF16Slice, Range<Int> >
  {
    return IndexableGenerator(self, indices(self))
  }
}

extension ContiguousUTF16Slice : Sliceable {
  subscript(slice: Range<Int>) -> ContiguousUTF16Slice {
    var r: Range<Int> = slice
    assert(r.endIndex() <= count)
    return ContiguousUTF16Slice(
      owner: owner, 
      start: start + r.startIndex(), 
      count: r.endIndex() - r.startIndex())
  }
  
  func __slice__(start: Int, finish: Int) -> ContiguousUTF16Slice {
    return self[start..finish]
  }
}

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
    var _base: ContiguousUTF16Slice
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

  struct GeneratorType : Generator {
    @mutating
    func next() -> UnicodeScalar? {
      return UTF16.decode({ _base.next() })
    }
    var _base: ContiguousUTF16Slice.GeneratorType
  }
  
  func enumerate() -> GeneratorType {
    return GeneratorType(_base.enumerate())
  }

  @conversion
  func __conversion() -> String {
    return String(_base)
  }
  
  var _base: ContiguousUTF16Slice
}

// FIXME: This can die when our codegen creates UTF-16 literals
struct UTF8StringLiteral {
  var isAscii: Bool
  var base: UnsafePointer<UTF8.CodeUnit>
  var byteCount: Int

  /// \brief Convert the given subrange to a ContiguousUTF16Slice
  func contiguousSlice(range: Range<Int>) -> ContiguousUTF16Slice {
    // This is inefficient, but the code will die soon anyway.
    return contiguousSlice()[range]
  }

  func contiguousSlice() -> ContiguousUTF16Slice {
    return ContiguousUTF16Slice(StringBuffer(UTF8, UnsafeArrayGenerator(base, byteCount)))
  }

  // DANGER: expects a pointer to count() elements worth of storage
  func _read(buffer: UnsafePointer<UTF16.CodeUnit>) {
    var pos = buffer
    transcode(UTF8, UTF16, UnsafeArrayGenerator(base, byteCount), SinkOf({ (pos++).set($0) }))
  }
  
  /// \brief Say how many UTF16 code units we store
  func count() -> Int {
    if isAscii {
      return byteCount
    }
    var utf16Count = 0
    transcode(
      UTF8, UTF16, UnsafeArrayGenerator(base, byteCount),
      SinkOf({ (x:UTF16.CodeUnit)  in ++utf16Count;() })
    )
    return utf16Count
  }

}

struct String {
  init(source: ContiguousUTF16Slice) {
    representation = .Contiguous(source)
  }

  init(source: OpaqueUTF16Slice) {
    representation = .Opaque(source)
  }

  init(source: UTF8StringLiteral) {
    representation = .UTF8Literal(source)
  }

  init() {
    representation = .Contiguous(ContiguousUTF16Slice())
  }
      
  enum Representation {
  case Opaque(OpaqueUTF16Slice)
  case Contiguous(ContiguousUTF16Slice)
  case UTF8Literal(UTF8StringLiteral)
    
    var countUTF16: Int {
      switch(self) {
      case .Opaque(var rep):
        return rep.range.endIndex() - rep.range.startIndex()
      case .Contiguous(var rep):
        return rep.count
      case .UTF8Literal(var rep):
        return rep.count()
      }
    }
  }

  subscript(slice: Range<Int>) -> String {
    switch (representation) {
    case .Opaque(var rep):
      return String(rep[slice])
    case .Contiguous(var rep):
      return String(rep[slice])
    case .UTF8Literal(var rep):
      return String(rep.contiguousSlice(slice))
    }
  }

  func _contiguousSlice(subRange: Range<Int>) -> ContiguousUTF16Slice {
    switch (representation) {
    case .Opaque(var rep):
      return rep.contiguousSlice(subRange)
    case .Contiguous(var rep):
      return rep[subRange]
    case .UTF8Literal(var rep):
      return rep.contiguousSlice(subRange)
    }
  }
  
  func _contiguous() -> ContiguousUTF16Slice {
    switch (representation) {
    case .Opaque(var rep):
      return rep.contiguousSlice()
    case .Contiguous(var rep):
      return rep
    case .UTF8Literal(var rep):
      return rep.contiguousSlice()
    }
  }
  
  
  var representation: Representation
}

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
    Encoding: UnicodeCodec, Input: MultiPassGenerator
  where Input.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.metatype, input: Input, minimumCapacity: Int = 0
  ) {
    // Determine how many UTF16 code units we'll need
    var utf16Count = 0
    transcode(encoding, UTF16, input, SinkOf<UTF16.CodeUnit>({
       _ in ++utf16Count;()
    }))

    // Allocate storage
    self = StringBuffer(max(utf16Count, minimumCapacity))

    // Fill the storage
    transcode(encoding, UTF16, input, SinkOf<UTF16.CodeUnit>( {
      (used++).set($0)
    } ))
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

typealias StringLiteralType = String

extension String : BuiltinUTF16StringLiteralConvertible {
  
  static func _convertFromBuiltinUTF16StringLiteral(
    start: Builtin.RawPointer, numberOfCodeUnits: Builtin.Int64
  ) -> String {
    
    return String(
        ContiguousUTF16Slice(
          .None, UnsafePointer<UTF16.CodeUnit>(start), Int(numberOfCodeUnits)))
  }
}

extension String : BuiltinStringLiteralConvertible {
  
  static func _convertFromBuiltinStringLiteral(
    value: Builtin.RawPointer,
    byteSize: Builtin.Int64,
    isASCII: Builtin.Int1) -> String {

    return String(
      UTF8StringLiteral(
        Bool(isASCII), UnsafePointer<UTF8.CodeUnit>(value), Int(byteSize)))
  }
}

extension String : StringLiteralConvertible {
  static func convertFromStringLiteral(value: String) -> String {
    return String(value.str_value)
  }
}


extension String {
  /// \brief return the number of code units occupied by this string
  /// in the given encoding
  func encodedLength<Encoding: UnicodeCodec>(encoding: Encoding.metatype) -> Int {
    var codeUnitCount = 0
    self.encode(encoding, SinkOf<Encoding.CodeUnit>({ _ in ++codeUnitCount;() }))
    return codeUnitCount
  }
  
  func encode<
    Encoding: UnicodeCodec,
    Output: Sink
    where Encoding.CodeUnit == Output.Element
  >(encoding: Encoding.metatype, output: Output) 
  {
    /// FIXME: consider using Cocoa to encode opaque representations
    return _contiguous().encode(encoding, output)
  }
}

extension ContiguousUTF16Slice {
  func encode<
    Encoding: UnicodeCodec,
    Output: Sink
    where Encoding.CodeUnit == Output.Element
  >(encoding: Encoding.metatype, output: Output) 
  {
    transcode(UTF16, encoding, UnsafeArrayGenerator(start, count), output )
  }
}

extension String: Equatable {
}

func ==(lhs: String, rhs: String) -> Bool {
  // FIXME: should eventually compare Characters
  var lhsContiguous = lhs._contiguous()
  var rhsContiguous = rhs._contiguous()
  
  if lhsContiguous.count != rhsContiguous.count {
    return false
  }
  return swift.equal(lhsContiguous, rhsContiguous)  
}

func <(lhs: String, rhs: String) -> Bool {
  var lhsContiguous = lhs._contiguous()
  var rhsContiguous = rhs._contiguous()
  
  for (c1, c2) in Zip2(lhsContiguous, rhsContiguous) {
    if c1 < c2 {
      return true
    }
    else if c2 < c1 {
      return false
    }
  }
  return lhsContiguous.count < rhsContiguous.count
}

// FIXME: Needed this trivial sink to work around
// <rdar://problem/15666157> Assertion failed:
// (fn->getLoweredFunctionType() == type), function
// getOrCreateSharedFunction
struct UnsafeArraySink<T> : Sink {
  @mutating
  func put(x: T) {
    (pos++).set(x)
  }
  var pos: UnsafePointer<T>
}

// Support for copy-on-write
extension String {

  @mutating
  func _append<
    Encoding: UnicodeCodec, Input: MultiPassGenerator
      where Input.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.metatype, input: Input
  )
  {
    var counter = CountingSink<UTF16.CodeUnit>()
    // Determine how many UTF16 code units we'll need
    transcode(encoding, UTF16, input, counter)
    var inserter = UnsafeArraySink(_growBuffer(_utf16Count + counter.count))
    transcode(encoding, UTF16, input, inserter)
  }

  @mutating
  func _append(rhs: String) {
    var rhsUTF16 = rhs._contiguous()
    var inserter = _growBuffer(_utf16Count + rhsUTF16.count)
    for x in rhsUTF16 {
      (inserter++).set(x)
    }
  }

  @mutating
  func _append(x: UnicodeScalar) {
    var encodedLength = 0
    UTF16.encode(x) { _ in ++encodedLength; () }
    var inserter = _growBuffer(_utf16Count + encodedLength)
    UTF16.encode(x) { (inserter++).set($0) }
  }
  
  var _utf16Count: Int {
    switch(representation) {
    case .Opaque(var rep):
      return rep.range.endIndex() - rep.range.startIndex()
    case .Contiguous(var rep):
      return rep.count
    case .UTF8Literal(var rep):
      return rep.count()
    }
  }

  /// \brief Attempt to claim unused capacity in the String's existing
  /// native buffer, if any.  Return zero and a pointer to the claimed
  /// storage if successful. Otherwise, returns a suggested new
  /// capacity and a null pointer.  
  ///
  /// Note: If successful, effectively appends garbage to the String
  /// until it has newSize UTF16 code units; you must immediately copy
  /// valid UTF16 into that storage.
  ///
  /// Note: if unsuccessful because of insufficient space in an
  /// existing buffer, the suggested new capacity will at least double
  /// the existing buffer's storage
  @mutating
  func _claimCapacity(newSize: Int) -> (Int, UnsafePointer<UTF16.CodeUnit>) {
    // Start by assuming that if we have to allocate a new buffer, it
    // should fit exactly
    var newCapacity = newSize
    
    switch(representation) {
    case .Contiguous(var rep):
      switch (rep.owner) {
      case .Native(var _buffer):
        var buffer = StringBuffer(_buffer)

        // The buffer's "used" field must match this in order to be
        // grown.  Otherwise, some other String is using parts of
        // the buffer beyond our last byte.
        var matchUsed = rep.start + rep.count
        
        // The buffer's "used" field ends up here if grow succeeds
        var finalUsed = rep.start + newSize
        
        // Attempt to claim unused capacity in the buffer
        if (buffer.grow(matchUsed, finalUsed)) {
          rep.count = newSize
          self.representation = .Contiguous(rep)
          return (0, matchUsed)
        }
        else if buffer.capacity < finalUsed {
          // Growth failed because of insufficient storage; double the size
          return (max(2 * (buffer.capacity - buffer.start), newSize), .null())
        }
      default:
      }
    default:
    }
    return (newSize, .null())
  }
  
  /// \brief Ensure that this String references a StringBuffer
  /// having a capacity of at least newSize.  Effectively appends
  /// garbage to the String until it has newSize UTF16 code units.
  /// Returns a pointer to the garbage code units; you must
  /// immediately copy valid UTF16 into that storage.
  @mutating
  func _growBuffer(newSize: Int) -> UnsafePointer<UTF16.CodeUnit> {
    
    var (newCapacity, existingStorage) = _claimCapacity(newSize)
    if !existingStorage.isNull() {
      return existingStorage
    }
    
    // Allocate storage
    var newStorage = ContiguousUTF16Slice(StringBuffer(newCapacity, newSize))

    var oldSize = _utf16Count
    
    switch representation {
    case .Opaque(var rep):
      rep.buffer._read(newStorage.start)
    case .UTF8Literal(var rep):
      rep._read(newStorage.start)
    case .Contiguous(var rep):
      c_memcpy(
        dest: newStorage.start,
        src: rep.start,
        size: UInt64(sizeof(UTF16.CodeUnit) * oldSize)
      )
    }
    
    representation = .Contiguous(newStorage)
    return newStorage.start + oldSize
  }

  init(storage: StringBuffer) {
    representation = .Contiguous(
      ContiguousUTF16Slice(.Native(storage), storage.start, storage.used - storage.start))
  }
}

// FIXME: Move this elsewhere
struct GenerateN<T> : MultiPassGenerator {

  @mutating
  func next() -> T? {
    return (n-- > 0) ? .Some(x) : .None
  }
  var n: Int
  var x: T
}

// Interfaces with a questionable future that are needed in order to
// be a drop-in replacement for String
//
extension String {
  
  init<
    Encoding: UnicodeCodec, Input: MultiPassGenerator
  where Input.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.metatype, input: Input
  )
  {
    self = String(StringBuffer(encoding, input))
  }
  
  init(str_value : StringByteData) {
    self = String(UTF8, UnsafeArrayGenerator(str_value.base, str_value.length))
  }

  init(sz: Int, c: UnicodeScalar) {
    self = String(UTF32, GenerateN<UTF32.CodeUnit>(sz, c.value))
  }

  var str_value: StringByteData {
    var utf8 = self.asUInt8()
    return StringByteData.convertFromHeapArray(
      utf8.base.value, utf8.owner,
      utf8.count.value)
  }
  
  func asUInt8() -> UTF8.CodeUnit[] {
    var result = new UTF8.CodeUnit[encodedLength(UTF8)]
    var len = 0
    encode(UTF8, SinkOf<UTF8.CodeUnit>({ result[len++] = $0 }))
    return result
  }

  func byteLength() -> Int {
    return encodedLength(UTF8)
  }

  func nulTerminatedUTF8() -> StringByteData {
    var buffer = str_value
    var nul: UInt8[] = [0]
    buffer.appendBytes(nul.base, 1)
    swift_keepAlive(nul.owner)
    return buffer
  }

  // FIXME: this typealias should die; it is only needed to satisfy
  // test/NameBinding/library.swift.  That test should be updated to
  // not depend on stdlib details
  typealias CharGeneratorType = UTF16Scalars.GeneratorType
  var chars : UTF16Scalars {
    return UTF16Scalars(_contiguous())
  }
  
  var lines : String[] {
    return split('\n')
  }
  
  func split(separator: UnicodeScalar) -> String[] {
    var scalarSlices = swift.split(chars, { $0 == separator })
    return scalarSlices.map { $0 as String }
  }
  
  var bytes : StringByteData {
    var result = StringByteData(byteLength())
    encode(
      UTF8, SinkOf<UTF8.CodeUnit>(
        {
          var tmp = $0
          result.appendBytes(
            UnsafePointer<UInt8>(Builtin.addressof(&tmp)), 1)
        }
        ))
    return result
  }
  
  func size() -> Int {
    var count = 0
    for c in chars {
      ++count
    }
    return count
  }

  var length: Int {
    return size()
  }
  
  func isEmpty() -> Bool {
    switch (representation) {
    case .Opaque(var rep):
      return rep.range.isEmpty()
    case .UTF8Literal(var rep):
      return rep.byteCount == 0
    case .Contiguous(var rep):
      return rep.count == 0
    }
  }

  static func _from(utf8: StringByteData) -> String {
    return String(UTF8, UnsafeArrayGenerator(utf8.base, utf8.length))
  }
  
  // FIXME: for some reason, making this function an actual overload
  // of subscript breaks tests.  Investigate later.
  func subscript_(rng : IntGeneratorType) -> String {
    return String._from(bytes[rng])
  }

  subscript (idx : Int) -> UnicodeScalar {
    for (i, c) in swift.enumerate(chars) {
      if i == idx {
        return c
      }
    }
    alwaysTrap()
  }
}

extension String : ReplPrintable {
  func replPrint() {
    print('"')
    for c in chars {
      print(c.escape())
    }
    print('"')
  }
}

extension String {
  // FIXME: Locales make this interesting
  var uppercase : String {
    var str_value = self.bytes
    var len = byteLength()
    var resultArray = StringByteData.getNew(len)
    var i = 0
    while i != len {
      var u8 = str_value[i]
      if u8 < 0x80 {
        if (97 .. 123).contains(Int(u8)) {
          resultArray[i] = u8 - 32
        } else {
          resultArray[i] = u8
        }
        i += 1
      } else if u8 < 0xE0 {
        resultArray[i] = u8
        var u8_1 = str_value[i + 1]
        if u8 == 0xC3 && (0xA0 .. 0xBF).contains(Int(u8_1)) && u8_1 != 0xB7 {
          resultArray[i+1] = u8_1 - 0x20
        } else {
          resultArray[i+1] = u8_1
        }
        i += 2
      } else if u8 < 0xF0 {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        i += 3
      } else {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        resultArray[i+3] = str_value[i+3]
        i += 4
      }
    }

    return String._from(resultArray)
  }

  // FIXME: Locales make this interesting
  var lowercase : String {
    var str_value = self.bytes
    var len = byteLength()
    var resultArray = StringByteData.getNew(len)
    var i = 0
    while i != len {
      var u8 = str_value[i]
      if u8 < 0x80 {
        if (65 .. 91).contains(Int(u8)) {
          resultArray[i] = u8 + 32
        } else {
          resultArray[i] = u8
        }
        i += 1
      } else if u8 < 0xE0 {
        resultArray[i] = u8
        var u8_1 = str_value[i + 1]
        if u8 == 0xC3 && (0x80 .. 0x9F).contains(Int(u8_1)) && u8_1 != 0x97 {
          resultArray[i+1] = u8_1 + 0x20
        } else {
          resultArray[i+1] = u8_1
        }
        i += 2
      } else if u8 < 0xF0 {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        i += 3
      } else {
        resultArray[i] = u8
        resultArray[i+1] = str_value[i+1]
        resultArray[i+2] = str_value[i+2]
        resultArray[i+3] = str_value[i+3]
        i += 4
      }
    }

    return String._from(resultArray)
  }

  init(c: UnicodeScalar) {
    self = String(1, c)
  }

  func _isAll(predicate: (UnicodeScalar) -> Bool) -> Bool {
    for c in chars { if !predicate(c) { return false } }

    return true
  }

  func startsWith(prefix: String) -> Bool {
    if prefix.size() > size() { return false }

    return self[0..prefix.size()] == prefix
  }

  func isAlpha() -> Bool { return _isAll({ $0.isAlpha() }) }
  func isDigit() -> Bool { return _isAll({ $0.isDigit() }) }
  func isSpace() -> Bool { return _isAll({ $0.isSpace() }) }
}

extension String : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    var toPrint = self
    if kind == 'u' { toPrint = uppercase }
    else if kind == 'l' { toPrint = lowercase }
    return Format(layout).printToString(toPrint)
  }
}

func +(var lhs: String, rhs: String) -> String {
  if (lhs.isEmpty()) {
    return rhs
  }
  lhs._append(rhs)
  return lhs
}

func +(var lhs: String, rhs: UnicodeScalar) -> String {
  lhs._append(rhs)
  return lhs
}
func +(lhs: UnicodeScalar, rhs: String) -> String {
  var result = String()
  result._append(lhs)
  result._append(rhs)
  return result
}
func +(lhs: UnicodeScalar, rhs: UnicodeScalar) -> String {
  var result = String()
  result._append(lhs)
  result._append(rhs)
  return result
}


// String append
@assignment func += (lhs: @inout String, rhs: String) {
  if (lhs.isEmpty()) {
    lhs = rhs
  }
  else {
    lhs._append(rhs)
  }
}

@assignment func += (lhs: @inout String, rhs: UnicodeScalar) {
  lhs._append(rhs)
}

// Comparison operators
// FIXME: Compare Characters, not code units
extension String : Comparable {  
}

/// \brief Represent a positive integer value in the given radix,
/// writing each UTF-16 code units into stream.  The value of `ten'
/// should be either 'A' or 'a', depending on whether you want upper-
/// or lower-case letters when radix > 10
func _formatPositiveInteger( 
  value: UInt64,
  radix: UInt64,
  ten: UnicodeScalar = 'a') (  stream: (UTF16.CodeUnit)->Void )
{

  if value == 0 {
    return
  }

  _formatPositiveInteger(value / radix, radix, ten)(stream)
  var digit = UInt32(value % radix)
  var baseCharOrd : UInt32 = digit <= 9 ? '0'.value : ten.value - 10
  stream(UTF16.CodeUnit(baseCharOrd + digit))
}

func _formatSignedInteger(
  value: Int64,
  radix: UInt64,
  ten: UnicodeScalar = 'a') (  stream: (UTF16.CodeUnit)->Void ) {
  
  if value == 0 {
    stream (UTF16.CodeUnit('0'.value))
  }
  else {
    if (value < 0) {
      stream(UTF16.CodeUnit('-'.value))
    }
    // Compute the absolute value without causing overflow when value
    // == Int64.min
    let absValue = value < 0 ? UInt64(~value) + 1 : UInt64(value)
    _formatPositiveInteger(absValue, radix, ten)(stream)
  }
}

// Conversions to string from other types.
extension String {
  init(v: Int64, radix: Int = 10, uppercase: Bool = false) {
    var format = _formatSignedInteger(v, UInt64(radix), uppercase ? 'A' : 'a')
    var utf16Count = 0
    format { _ in ++utf16Count;() }
    var buffer = StringBuffer(utf16Count)
    format { buffer.used++.set($0) }
    self = String(buffer)
  }

  init(v : UInt64, radix: Int = 10, uppercase: Bool = false) {
    var format = _formatPositiveInteger(v, UInt64(radix), uppercase ? 'A' : 'a')
    var utf16Count = v == 0 ? 1 : 0
    format { _ in ++utf16Count;() }
    var buffer = StringBuffer(utf16Count)
    format { buffer.used++.set($0) }
    if v == 0 {
      buffer.used++.set(UTF16.CodeUnit('0'.value))
    }
    self = String(buffer)
  }

  init(v : Int8, radix : Int = 10, uppercase : Bool = false) {
    self = String(Int64(v), radix, uppercase)
  }
  init(v : Int16, radix : Int = 10, uppercase : Bool = false) {
    self = String(Int64(v), radix, uppercase)
  }
  init(v : Int32, radix : Int = 10, uppercase : Bool = false) {
    self = String(Int64(v), radix, uppercase)
  }
  init(v : UInt8, radix : Int = 10, uppercase : Bool = false) {
    self = String(UInt64(v), radix, uppercase)
  }
  init(v : UInt16, radix : Int = 10, uppercase : Bool = false) {
    self = String(UInt64(v), radix, uppercase)
  }
  init(v : UInt32, radix : Int = 10, uppercase : Bool = false) {
    self = String(UInt64(v), radix, uppercase)
  }

  init(v : Double) {
    var cCharBuf = Array<UInt8>(256, 0)
    var n = Int(c_print_double(cCharBuf.base.value, v))
    var buffer = StringBuffer(n)
    for i in 0..n {
      buffer.used++.set(UTF16.CodeUnit(cCharBuf[i]))
    }
    self = String(buffer)
  }
  
  init(v : Float) {
    self = String(Double(v))
  }

  init(b : Bool) {
    if b {
      self = "true"
    } else {
      self = "false"
    }
  }
}

// Conversions from string to other types.
extension String {
  /// \brief If the string represents an integer that fits into an Int, returns
  /// the corresponding integer.
  func toInt() -> Int? {
    var scalars = self.chars

    var start = scalars.startIndex()
    if start == scalars.endIndex() {
      return .None
    }
    
    // Interpet '+' or '-' before the number.
    var negativeFactor = -1
    var firstC = scalars[start]
    if (firstC == '+') {
      ++start
    } else if (firstC == '-') {
      ++start
      negativeFactor = 1
    }

    // Interpret the string as an integer.
    // Since Int.min has a larger absolute value, perform addition with
    // negative numbers; detect underflows before they happen. 
    var res : Int = 0
    for c in scalars[start..scalars.endIndex()] {
      if !c.isDigit() {
        // Conversion failed if a non-digit is encountered.
        return .None
      }

      // Underflow occurs if res * 10 < Int.min.
      if res < Int.min / 10 {
        return .None
      }
      res = res * 10

      var d : Int = (c - '0')
      // Underflow occurs if res - d < Int.min.
      if res < Int.min + d {
        return .None
      }
      res = res - d
    }

    // If res is Int.min and the result should be positive, the next
    // operation will overflow.
    if negativeFactor == -1 && res == Int.min {
      return .None
    }

    return .Some(res * negativeFactor)
  }
}

extension String {
  /// \brief Produce a substring of the given string from the given character
  /// index to the end of the string.
  func substr(start: Int) -> String {
    var rng = chars
    var startIndex = rng.startIndex()
    for i in 0..start {
      ++startIndex
    }
    return rng[startIndex..rng.endIndex()]
  }

  /// \brief Split the given string at the given delimiter character, returning 
  /// the strings before and after that character (neither includes the character
  /// found) and a boolean value indicating whether the delimiter was found.
  func splitFirst(delim: UnicodeScalar)
    -> (before: String, after: String, wasFound : Bool)
  {
    var rng = chars
    for i in indices(rng) {
      if rng[i] == delim {
        return (rng[rng.startIndex()..i], rng[i.succ()..rng.endIndex()], true)
      }
    }
    return (self, "", false)
  }

  /// \brief Split the given string at the first character for which the given
  /// predicate returns true. Returns the string before that character, the 
  /// character that matches, the string after that character, and a boolean value
  /// indicating whether any character was found.
  func splitFirstIf(pred: (UnicodeScalar) -> Bool)
    -> (before: String, found: UnicodeScalar, after: String, wasFound: Bool)
  {
    var rng = chars
    for i in indices(rng) {
      if pred(rng[i]) {
        return (rng[rng.startIndex()..i], rng[i], rng[i.succ()..rng.endIndex()], true)
      }
    }
    return (self, '🎃', String(), false)
  }

  /// \brief Split the given string at each occurrence of a character for which
  /// the given predicate evaluates true, returning an array of strings that
  /// before/between/after those delimiters.
  func splitIf(pred: (UnicodeScalar) -> Bool) -> String[] {
    var scalarSlices = swift.split(chars, pred)
    return scalarSlices.map { $0 as String }
  }
}

extension String : Hashable {
  func hashValue() -> Int {
    var r : UInt64 = 5381
    encode(
      UTF8,
      SinkOf<UTF8.CodeUnit> ({
          r = ((r << 5) &+ r) &+ UInt64($0)
        }))
    return Int(r)
  }
}

extension String : StringInterpolationConvertible {
  static func convertFromStringInterpolation(strings: String...) -> String {
    var result = String()
    for str in strings {
      result += str
    }
    return result
  }
}
