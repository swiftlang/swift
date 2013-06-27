// This is just here to provide a type off of which to hang swift_bufferAllocate.
class RawBuffer {}

func [asmname="swift_isUniquelyReferenced"] 
swift_isUniquelyReferenced(objPtr: Builtin.ObjectPointer) -> CBool

func [asmname="swift_bufferAllocate"]
swift_bufferAllocate(bufferType: RawBuffer.metatype, size: Int) -> RawBuffer

func [asmname="swift_bufferHeaderSize"] swift_bufferHeaderSize() -> Int

/// \brief a class containing an ivar "value" of type Value, and
/// containing storage for an array of Element whose size is
/// determined at create time.  
///
/// The analogous C++-ish class template would be:
///
///   template <class Value, class Element>
///   struct HeapBuffer {
///     Value value;
///     Element[];             // length determined at creation time
///     HeapBuffer() = delete
///     static shared_ptr<HeapBuffer> create(Value init, int capacity);
///   }
///
/// Note that the Element array is RAW MEMORY.  You are expected to
/// construct and---if necessary---destroy Elements there yourself,
/// either in a derived class, or it can be in some manager object
/// that owns the HeapBuffer.
///
/// If you need to construct and destroy Elements, using a derived
/// class is a natural choice. However, don't add any ivars because we
/// have no way in the runtime to get the base allocation size of an
/// arbitrary class.  As a result, we will fail to allocate memory for
/// them, and their storage will collide with that of the stored
/// Value.
class HeapBuffer<Value,Element> : RawBuffer {

  typealias Self = HeapBuffer<Value,Element>

  static func roundUpToAlignment(offset: Int, alignment: Int) -> Int {
    return (offset + alignment - 1) / alignment * alignment
  }

  static func _valueOffset() -> Int {
    return roundUpToAlignment(
      swift_bufferHeaderSize(), Int(Builtin.alignof(Value)))
  }

  static func _elementOffset() -> Int {
    return roundUpToAlignment(
      _valueOffset() + Int(Builtin.sizeof(Value)), Int(Builtin.alignof(Element)))
  }

  var _address: UnsafePointer<Int8> {
    return UnsafePointer<Int8>(
      Builtin.bridgeToRawPointer(Builtin.castToObjectPointer(this)))
  }

  var _value: UnsafePointer<Value> {
    return UnsafePointer<Value>(
      Self._valueOffset() + _address)
  }

  var elementStorage: UnsafePointer<Element> {
    return UnsafePointer<Element>(Self._elementOffset() + _address)
  }

  static func create(initializer: Value, capacity: Int) -> Self {
    assert(capacity >= 0)

    var totalSize = Self._elementOffset() + (
        capacity == 0 ? 0 
        : (capacity - 1) * Int(Builtin.strideof(Element)) 
          + Int(Builtin.sizeof(Element)))

    var self = swift_bufferAllocate(Self, totalSize) as! Self
    self._value.init(initializer)
    return self
  }

  destructor {
    this._value.destroy()
  }

  var value : Value {
  get:
    return _value.get()
  set(newValue):
    _value.set(newValue)
  }
}
