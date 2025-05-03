@inline(never)
private func consume<T>(_ t: T) {
    withExtendedLifetime(t) { t in }
}
@usableFromInline
internal func _prespecialize() {
  consume(Array<()>.self)
  consume(Array<(Optional<String>, Any)>.self)
  consume(Array<Any>.self)
  consume(Array<AnyHashable>.self)
  consume(Array<Dictionary<String, Any>>.self)
  consume(Array<Dictionary<String, AnyObject>>.self)
  consume(Array<Int64>.self)
  consume(Array<Int>.self)
  consume(Array<Optional<Any>>.self)
  consume(Array<Optional<String>>.self)
#if _runtime(_ObjC)
  consume(_ArrayBuffer<()>.self)
  consume(_ArrayBuffer<(Optional<String>, Any)>.self)
  consume(_ArrayBuffer<Any>.self)
  consume(_ArrayBuffer<AnyHashable>.self)
  consume(_ArrayBuffer<Dictionary<String, Any>>.self)
  consume(_ArrayBuffer<Dictionary<String, AnyObject>>.self)
  consume(_ArrayBuffer<Int64>.self)
  consume(_ArrayBuffer<Int>.self)
  consume(_ArrayBuffer<Optional<Any>>.self)
  consume(_ArrayBuffer<Optional<String>>.self)
#endif
  consume(ClosedRange<Int>.self)
  consume(ContiguousArray<(AnyHashable, Any)>.self)
  consume(ContiguousArray<(Optional<String>, Any)>.self)
  consume(ContiguousArray<Any>.self)
  consume(ContiguousArray<AnyHashable>.self)
  consume(ContiguousArray<Dictionary<String, AnyObject>>.self)
  consume(ContiguousArray<Int64>.self)
  consume(ContiguousArray<String>.self)
  consume(_ContiguousArrayStorage<(AnyHashable, Any)>.self)
  consume(_ContiguousArrayStorage<(Optional<String>, Any)>.self)
  consume(_ContiguousArrayStorage<Any>.self)
  consume(_ContiguousArrayStorage<AnyHashable>.self)
  consume(_ContiguousArrayStorage<Dictionary<String, AnyObject>>.self)
  consume(_ContiguousArrayStorage<Int64>.self)
  consume(_ContiguousArrayStorage<String>.self)
  consume(Dictionary<String, String>.Index.self)
  consume(Dictionary<String, String>.Iterator.self)
  consume(Dictionary<AnyHashable, Any>.self)
  consume(Dictionary<AnyHashable, Any>.Iterator.self)
  consume(Dictionary<AnyHashable, AnyHashable>.self)
  consume(Dictionary<AnyHashable, AnyHashable>.Iterator.self)
  consume(Dictionary<AnyHashable, AnyObject>.self)
  consume(Dictionary<String, Any>.self)
  consume(Dictionary<String, Any>.Iterator.self)
  consume(Dictionary<String, AnyHashable>.self)
  consume(Dictionary<String, AnyObject>.self)
  consume(Dictionary<String, Array<String>>.self)
  consume(Dictionary<String, Dictionary<String, Any>>.self)
  consume(Dictionary<String, String>.self)
  consume(Dictionary<String, UInt8>.self)
  consume(IndexingIterator<Array<Dictionary<String, AnyObject>>>.self)
  consume(IndexingIterator<Array<Int>>.self)
  consume(IndexingIterator<Array<String>>.self)
  consume(IndexingIterator<ClosedRange<Int>>.self)
  consume(IndexingIterator<Range<Int>>.self)
  consume(IndexingIterator<String.UTF8View>.self)
  consume(Optional<()>.self)
  consume(Optional<(Any, Any)>.self)
  consume(Optional<(AnyHashable, Any)>.self)
  consume(Optional<(AnyHashable, AnyHashable)>.self)
  consume(Optional<(CodingUserInfoKey, Any)>.self)
  consume(Optional<(Int64, Bool)>.self)
  consume(Optional<(Optional<String>, Any)>.self)
  consume(Optional<(String, Any)>.self)
  consume(Optional<(String, AnyObject)>.self)
  consume(Optional<(String, Array<String>)>.self)
  consume(Optional<(String, Int64)>.self)
  consume(Optional<(String, String)>.self)
  consume(Optional<(String, UInt8)>.self)
  consume(Optional<Any>.self)
  consume(Optional<AnyObject>.self)
  consume(Optional<Array<Int64>>.self)
  consume(Optional<Array<String>>.self)
  consume(Optional<Bool>.self)
  consume(Optional<CodingKey>.self)
  consume(Optional<CodingUserInfoKey>.self)
  consume(Optional<CustomDebugStringConvertible>.self)
#if SWIFT_ENABLE_REFLECTION
  consume(Optional<CustomReflectable>.self)
#endif
  consume(Optional<CustomStringConvertible>.self)
  consume(Optional<Dictionary<AnyHashable, Any>>.self)
  consume(Optional<Dictionary<String, Any>>.self)
  consume(Optional<Dictionary<String, AnyObject>>.self)
  consume(Optional<Dictionary<String, Array<String>>>.self)
  consume(Optional<Dictionary<String, String>>.self)
  consume(Optional<Double>.self)
  consume(Optional<Int64>.self)
  consume(Optional<Int8>.self)
#if SWIFT_ENABLE_REFLECTION
  consume(Optional<Mirror.DisplayStyle>.self)
#endif
  consume(Optional<Optional<Int64>>.self)
  consume(Optional<Optional<String>>.self)
  consume(Optional<Set<String>>.self)
  consume(Optional<TextOutputStreamable>.self)
  consume(Optional<UInt8>.self)
  unsafe consume(Optional<UnsafeBufferPointer<AnyObject>>.self)
  consume(PartialRangeFrom<Int>.self)
  consume(Range<String.Index>.self)
  consume(ReversedCollection<Range<Int>>.self)
  consume(ReversedCollection<Range<Int>>.Iterator.self)
  consume(Set<Int>.self)
  consume(Set<String>.self)
  consume(Set<String>.Iterator.self)
  consume(Set<String>.self)
  unsafe consume(Unmanaged<AnyObject>.self)
  unsafe consume(UnsafeBufferPointer<AnyObject>.self)
  unsafe consume(UnsafeBufferPointer<Int8>.self)
  unsafe consume(UnsafePointer<Int8>.self)
}

@_specializeExtension
extension Array {

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _endMutation(),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__endMutation(){ Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _createNewBuffer(bufferIsUnique:minimumCapacity:growForAppend:),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__createNewBuffer(bufferIsUnique: Bool, minimumCapacity: Int, growForAppend: Bool) { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _makeUniqueAndReserveCapacityIfNotUnique(),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__makeUniqueAndReserveCapacityIfNotUnique() { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _appendElementAssumeUniqueAndCapacity(_:newElement:),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__appendElementAssumeUniqueAndCapacity(_: Int, newElement: __owned Element) { Builtin.unreachable() }
}

#if _runtime(_ObjC)
@_specializeExtension
extension _ArrayBuffer {
  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _consumeAndCreateNew(bufferIsUnique:minimumCapacity:growForAppend:),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  func __specialize_class__consumeAndCreateNew(bufferIsUnique: Bool, minimumCapacity: Int, growForAppend: Bool) -> _ArrayBuffer<Element> { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _copyContents(initializing:),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  __consuming func __specialize_class__copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Element>
  ) -> (Iterator, UnsafeMutableBufferPointer<Element>.Index) { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _copyContents(subRange:initializing:),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  __consuming func __specialize_class__copyContents(subRange: Range<Int>, initializing: Swift.UnsafeMutablePointer<Element>) -> Swift.UnsafeMutablePointer<Element> { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _getElementSlowPath(_:),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  func __specialize_class__getElementSlowPath(_ i: Int) -> AnyObject { Builtin.unreachable() }
}
#endif

@_specializeExtension
extension ContiguousArray {
  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _endMutation(),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__endMutation(){ Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _createNewBuffer(bufferIsUnique:minimumCapacity:growForAppend:),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__createNewBuffer(bufferIsUnique: Bool, minimumCapacity: Int, growForAppend: Bool) { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _makeUniqueAndReserveCapacityIfNotUnique(),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__makeUniqueAndReserveCapacityIfNotUnique() { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _appendElementAssumeUniqueAndCapacity(_:newElement:),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__appendElementAssumeUniqueAndCapacity(_: Int, newElement: __owned Element) { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
              target: _reserveCapacityImpl(minimumCapacity:growForAppend:),
              where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__reserveCapacityImpl(minimumCapacity: Int, growForAppend: Bool) { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
              target: _reserveCapacityAssumingUniqueBuffer(oldCount:),
              where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__reserveCapacityAssumingUniqueBuffer(oldCount: Int) { Builtin.unreachable() }

  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
              target: reserveCapacity(_:),
              where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  mutating func __specialize_class__reserveCapacity(_ minimumCapacity: Int) { Builtin.unreachable() }
}

@_specializeExtension
extension _ContiguousArrayBuffer {
  @_specialize(exported: true,
               availability: SwiftStdlib 5.9, *;
               target: _consumeAndCreateNew(bufferIsUnique:minimumCapacity:growForAppend:),
               where @_noMetadata Element : _Class)
  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  func __specialize_class__consumeAndCreateNew(bufferIsUnique: Bool, minimumCapacity: Int, growForAppend: Bool) -> _ContiguousArrayBuffer<Element> { Builtin.unreachable() }
}

@_specializeExtension
extension Dictionary._Variant {
  @_specialize(
    exported: true,
    target: setValue(_:forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == Any)
  @_specialize(
    exported: true,
    target: setValue(_:forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == String)
  @_specialize(
    exported: true,
    target: setValue(_:forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == Any)
  @_specialize(
    exported: true,
    target: setValue(_:forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == AnyHashable)
  @_specialize(
    exported: true,
    target: setValue(_:forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_setValue(_: __owned Value, forKey: Key) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: removeValue(forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == String)
  @_specialize(
    exported: true,
    target: removeValue(forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == Any)
  @_specialize(
    exported: true,
    target: removeValue(forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == Any)
  @_specialize(
    exported: true,
    target: removeValue(forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == AnyHashable)
  @_specialize(
    exported: true,
    target: removeValue(forKey:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_removeValue(forKey: Key) -> Value? {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: isUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == String)
  @_specialize(
    exported: true,
    target: isUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == Any)
  @_specialize(
    exported: true,
    target: isUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == Any)
  @_specialize(
    exported: true,
    target: isUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == AnyHashable)
  @_specialize(
    exported: true,
    target: isUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_isUniquelyReferenced() -> Bool {
    Builtin.unreachable()
  }
}

// FIXME: Crashes in TypeBase::removeArgumentLabels(unsigned int)
// @_specializeExtension
// extension Dictionary {
//   subscript(__specialize_: Key) -> Value? {
//     @available(SwiftStdlib x.y, *)
//     @usebleFromInline
//     get { Builtin.unreachable() }
//     @_specialize(
//       exported: true,
//       target: subscript(_:),
//       availability: SwiftStdlib x.y;
//       where Key == AnyHashable, Value == String)
//     @_specialize(
//       exported: true,
//       target: subscript(_:),
//       availability: SwiftStdlib x.y;
//       where Key == AnyHashable, Value == Any)
//     @_specialize(
//       exported: true,
//       target: subscript(_:),
//       availability: SwiftStdlib x.y;
//       where Key == String, Value == Any)
//     @_specialize(
//       exported: true,
//       target: subscript(_:),
//       availability: SwiftStdlib x.y;
//       where Key == String, Value == AnyHashable)
//     @available(SwiftStdlib x.y, *)
//     @@usebleFromInline
//     set { Builtin.unreachable() }
//   }
// }

@_specializeExtension
extension _NativeDictionary {

  @_specialize(
    exported: true,
    target: _copyOrMoveAndResize(capacity:moveElements:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == String)
  @_specialize(
    exported: true,
    target: _copyOrMoveAndResize(capacity:moveElements:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == Any)
  @_specialize(
    exported: true,
    target: _copyOrMoveAndResize(capacity:moveElements:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == Any)
  @_specialize(
    exported: true,
    target: _copyOrMoveAndResize(capacity:moveElements:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == AnyHashable)
  @_specialize(
    exported: true,
    target: _copyOrMoveAndResize(capacity:moveElements:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__copyOrMoveAndResize(
    capacity: Swift.Int, moveElements: Swift.Bool
  ) { Builtin.unreachable() }

  @_specialize(
    exported: true,
    target: copy(),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == Any)
  @_specialize(
    exported: true,
    target: copy(),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == String)
  @_specialize(
    exported: true,
    target: copy(),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == Any)
  @_specialize(
    exported: true,
    target: copy(),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == AnyHashable)
  @_specialize(
    exported: true,
    target: copy(),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_copy() { Builtin.unreachable() }

  @_specialize(
    exported: true,
    target: mutatingFind(_:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == Any)
  @_specialize(
    exported: true,
    target: mutatingFind(_:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == String)
  @_specialize(
    exported: true,
    target: mutatingFind(_:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == Any)
  @_specialize(
    exported: true,
    target: mutatingFind(_:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == AnyHashable)
  @_specialize(
    exported: true,
    target: mutatingFind(_:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_mutatingFind(
    _ key: Key, isUnique: Bool
  ) -> (bucket: _HashTable.Bucket, found: Bool) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _insert(at:key:value:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == Any)
  @_specialize(
    exported: true,
    target: _insert(at:key:value:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == String)
  @_specialize(
    exported: true,
    target: _insert(at:key:value:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == Any)
  @_specialize(
    exported: true,
    target: _insert(at:key:value:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == AnyHashable)
  @_specialize(
    exported: true,
    target: _insert(at:key:value:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  func __specialize__insert(
    at: Bucket, key: __owned Key, value: __owned Value
  ) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: ensureUnique(isUnique:capacity:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == Any)
  @_specialize(
    exported: true,
    target: ensureUnique(isUnique:capacity:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == String)
  @_specialize(
    exported: true,
    target: ensureUnique(isUnique:capacity:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == Any)
  @_specialize(
    exported: true,
    target: ensureUnique(isUnique:capacity:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == AnyHashable)
  @_specialize(
    exported: true,
    target: ensureUnique(isUnique:capacity:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_ensureUnique(
    isUnique: Bool, capacity: Int
  ) -> Bool {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: uncheckedRemove(at:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == Any)
  @_specialize(
    exported: true,
    target: uncheckedRemove(at:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == AnyHashable, Value == String)
  @_specialize(
    exported: true,
    target: uncheckedRemove(at:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == Any)
  @_specialize(
    exported: true,
    target: uncheckedRemove(at:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == AnyHashable)
  @_specialize(
    exported: true,
    target: uncheckedRemove(at:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Key == String, Value == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_uncheckedRemove(
    at: _HashTable.Bucket, isUnique: Bool
  ) -> (key: Key, value: Value) {
    Builtin.unreachable()
  }
}

@_specializeExtension
extension __RawDictionaryStorage {
  @_specialize(
    exported: true,
    target: find(_:hashValue:),
    availability: SwiftStdlib 5.5, *;
    where T == AnyHashable)
  @_specialize(
    exported: true,
    target: find(_:hashValue:),
    availability: SwiftStdlib 5.5, *;
    where T == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  func __specialize_find<T: Hashable>(
    _: T, hashValue: Int
  ) -> (bucket: Swift._HashTable.Bucket, found: Swift.Bool) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: find(_:),
    availability: SwiftStdlib 5.5, *;
    where T == AnyHashable)
  @_specialize(
    exported: true,
    target: find(_:),
    availability: SwiftStdlib 5.5, *;
    where T == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  func __specialize_find<T: Hashable>(
    _: T
  ) -> (bucket: Swift._HashTable.Bucket, found: Swift.Bool) {
    Builtin.unreachable()
  }
}

@_specializeExtension
extension Array {
  @_specialize(
    exported: true,
    target: _checkSubscript(_:wasNativeTypeChecked:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _checkSubscript(_:wasNativeTypeChecked:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  func __specialize__checkSubscript(
    _: Int, wasNativeTypeChecked: Bool
  ) -> _DependenceToken {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _endMutation(),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _endMutation(),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__endMutation(){ Builtin.unreachable() }

  @_specialize(
    exported: true,
    target: _createNewBuffer(bufferIsUnique:minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _createNewBuffer(bufferIsUnique:minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__createNewBuffer(
    bufferIsUnique: Bool, minimumCapacity: Int, growForAppend: Bool
  ) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _makeUniqueAndReserveCapacityIfNotUnique(),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _makeUniqueAndReserveCapacityIfNotUnique(),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__makeUniqueAndReserveCapacityIfNotUnique() {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _appendElementAssumeUniqueAndCapacity(_:newElement:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _appendElementAssumeUniqueAndCapacity(_:newElement:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__appendElementAssumeUniqueAndCapacity(
    _: Int, newElement: __owned Element
  ) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: append(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: append(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_append(_: __owned Element) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _adoptStorage(_:count:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _adoptStorage(_:count:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  static func __specialize__adoptStorage(
    _: __owned _ContiguousArrayStorage<Element>, count: Int
  ) -> ([Element], UnsafeMutablePointer<Element>) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: append(contentsOf:),
    availability: SwiftStdlib 5.5, *;
    where Element == String, S == [String])
  @_specialize(
    exported: true,
    target: append(contentsOf:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any, S == [Any])
  @_specialize(
    exported: true,
    target: append(contentsOf:),
    availability: SwiftStdlib 5.5, *;
    where Element == String, S == Set<String>)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_append<S: Sequence>(contentsOf: __owned S)
  where S.Element == Element
  {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _reserveCapacityImpl(minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _reserveCapacityImpl(minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__reserveCapacityImpl(
    minimumCapacity: Int, growForAppend: Bool
  ) {
    Builtin.unreachable()
  }
}

#if _runtime(_ObjC)
@_specializeExtension
extension _ArrayBuffer {
  @_specialize(
    exported: true,
    target: _consumeAndCreateNew(bufferIsUnique:minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _consumeAndCreateNew(bufferIsUnique:minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  func __specialize__consumeAndCreateNew(
    bufferIsUnique: Bool, minimumCapacity: Int, growForAppend: Bool
  ) -> _ArrayBuffer<Element> {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: isMutableAndUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: isMutableAndUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_isMutableAndUniquelyReferenced() -> Bool {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: isUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: isUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_isUniquelyReferenced() -> Bool {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: beginCOWMutation(),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: beginCOWMutation(),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_beginCOWMutation() -> Bool {
    Builtin.unreachable()
  }

  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  var __specialize_firstElementAddress: UnsafeMutablePointer<Element> {
    @_specialize(
      exported: true,
      target: firstElementAddress,
      availability: SwiftStdlib 5.5, *;
      where Element == Any)
    @_specialize(
      exported: true,
      target: firstElementAddress,
      availability: SwiftStdlib 5.5, *;
      where Element == String)
    get { Builtin.unreachable() }
  }

  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  var __specialize__native: _ContiguousArrayBuffer<Element> {
    @_specialize(
      exported: true,
      target: _native,
      availability: SwiftStdlib 5.5, *;
      where Element == Any)
    @_specialize(
      exported: true,
      target: _native,
      availability: SwiftStdlib 5.5, *;
      where Element == String)
    get { Builtin.unreachable() }
  }

  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  var __specialize_immutableCount: Int {
    @_specialize(
      exported: true,
      target: immutableCount,
      availability: SwiftStdlib 5.5, *;
      where Element == Any)
    @_specialize(
      exported: true,
      target: immutableCount,
      availability: SwiftStdlib 5.5, *;
      where Element == String)
    get { Builtin.unreachable() }
  }

  // FIXME: Currently triggers a verification error in OwnershipModelEliminator
  // @_specialize(
  //   exported: true,
  //   target: init(_buffer:shiftedToStartIndex:),
  //   availability: SwiftStdlib x.y;
  //   where Element == String)
  // @_specialize(
  //   exported: true,
  //   target: init(_buffer:shiftedToStartIndex:),
  //   availability: SwiftStdlib x.y;
  //   where Element == Any)
  // @available(SwiftStdlib x.y, *)
  // @usableFromInline
  // init(
  //   specialized_buffer: _ContiguousArrayBuffer<Element>,
  //   shiftedToStartIndex: Int
  // ) {
  //   Builtin.unreachable()
  // }
}
#endif

@_specializeExtension
extension ContiguousArray {
  @_specialize(
    exported: true,
    target: _endMutation(),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _endMutation(),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__endMutation(){ Builtin.unreachable() }
  @_specialize(
    exported: true,
    target: _createNewBuffer(bufferIsUnique:minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _createNewBuffer(bufferIsUnique:minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__createNewBuffer(
    bufferIsUnique: Bool, minimumCapacity: Int, growForAppend: Bool
  ) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _makeUniqueAndReserveCapacityIfNotUnique(),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _makeUniqueAndReserveCapacityIfNotUnique(),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__makeUniqueAndReserveCapacityIfNotUnique() {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _appendElementAssumeUniqueAndCapacity(_:newElement:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _appendElementAssumeUniqueAndCapacity(_:newElement:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__appendElementAssumeUniqueAndCapacity(
    _: Int, newElement: __owned Element
  ) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: append(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: append(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_append(_: __owned Element) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: append(contentsOf:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any, S == [Any])
  @_specialize(
    exported: true,
    target: append(contentsOf:),
    availability: SwiftStdlib 5.5, *;
    where Element == String, S == [String])
  @_specialize(
    exported: true,
    target: append(contentsOf:),
    availability: SwiftStdlib 5.5, *;
    where Element == String, S == Set<String>)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_append<S: Sequence>(contentsOf: __owned S)
  where S.Element == Element
  {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _reserveCapacityImpl(minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _reserveCapacityImpl(minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__reserveCapacityImpl(
    minimumCapacity: Int, growForAppend: Bool
  ) {
    Builtin.unreachable()
  }
}

@_specializeExtension
extension _ContiguousArrayBuffer {
  @_specialize(
    exported: true,
    target: _consumeAndCreateNew(bufferIsUnique:minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: _consumeAndCreateNew(bufferIsUnique:minimumCapacity:growForAppend:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  func __specialize__consumeAndCreateNew(
    bufferIsUnique: Bool, minimumCapacity: Int, growForAppend: Bool
  ) -> _ContiguousArrayBuffer<Element> {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: isMutableAndUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Element == Any)
  @_specialize(
    exported: true,
    target: isMutableAndUniquelyReferenced(),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_isMutableAndUniquelyReferenced() -> Bool {
    Builtin.unreachable()
  }

  // FIXME: Currently triggers a verification error in OwnershipModelEliminator
  // @_specialize(
  //   exported: true,
  //   target: init(_buffer:shiftedToStartIndex:),
  //   availability: SwiftStdlib x.y;
  //   where Element == String)
  // @_specialize(
  //   exported: true,
  //   target: init(_buffer:shiftedToStartIndex:),
  //   availability: SwiftStdlib x.y;
  //   where Element == Any)
  // @available(SwiftStdlib x.y, *)
  // @usableFromInline
  // init(
  //   specialized_buffer: _ContiguousArrayBuffer<Element>,
  //   shiftedToStartIndex: Int
  // ) {
  //   Builtin.unreachable()
  // }
}

@_specializeExtension
extension Set {
  @_specialize(
    exported: true,
    target: contains(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: contains(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  func __specialize_contains(_: Element) -> Bool { Builtin.unreachable() }
}

@_specializeExtension
extension Set._Variant {
  @_specialize(
    exported: true,
    target: insert(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: insert(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_insert(
    _: __owned Element
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: remove(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: remove(_:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_remove(_: Element) -> Element? {
    Builtin.unreachable()
  }
}

@_specializeExtension
extension Sequence {
  @_specialize(
    exported: true,
    target: _copyContents(initializing:),
    availability: SwiftStdlib 5.5, *;
    where Self == [String])
  @_specialize(
    exported: true,
    target: _copyContents(initializing:),
    availability: SwiftStdlib 5.5, *;
    where Self == Set<String>)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  __consuming func __specialize__copyContents(
    initializing: Swift.UnsafeMutableBufferPointer<Element>
  ) -> (Iterator, Int) {
    Builtin.unreachable()
  }
}

@_specializeExtension
extension _NativeSet {
  @_specialize(
    exported: true,
    target: copyAndResize(capacity:),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: copyAndResize(capacity:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_copyAndResize(capacity: Int) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: copy(),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: copy(),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_copy() { Builtin.unreachable() }

  @_specialize(
    exported: true,
    target: index(after:),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: index(after:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  func __specialize_index(after: Set<Element>.Index) -> Set<Element>.Index {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: resize(capacity:),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: resize(capacity:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_resize(capacity: Int) { Builtin.unreachable() }

  @_specialize(
    exported: true,
    target: _delete(at:),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: _delete(at:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize__delete(at: _HashTable.Bucket) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: insertNew(_:at:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: insertNew(_:at:isUnique:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  mutating func __specialize_insertNew(
    _: __owned Element, at: _HashTable.Bucket, isUnique: Bool
  ) {
    Builtin.unreachable()
  }

  @_specialize(
    exported: true,
    target: _unsafeInsertNew(_:at:),
    availability: SwiftStdlib 5.5, *;
    where Element == AnyHashable)
  @_specialize(
    exported: true,
    target: _unsafeInsertNew(_:at:),
    availability: SwiftStdlib 5.5, *;
    where Element == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  func __specialize__unsafeInsertNew(
    _: __owned Element, at: _HashTable.Bucket
  ) {
    Builtin.unreachable()
  }
}

@_specializeExtension
extension Optional {
  @_specialize(
    exported: true,
    target: ==(_:_:),
    availability: SwiftStdlib 5.5, *;
    where Wrapped == AnyHashable)
  @_specialize(
    exported: true,
    target: ==(_:_:),
    availability: SwiftStdlib 5.5, *;
    where Wrapped == String)
  @available(SwiftStdlib 5.5, *)
  @usableFromInline
  static func __specialize_equals(lhs: Wrapped?, rhs: Wrapped?) -> Bool {
    Builtin.unreachable()
  }
}

@_specialize(
  exported: true,
  target: _dictionaryUpCast(_:),
  availability: SwiftStdlib 5.5, *;
  where
    DerivedKey == String,
    DerivedValue == AnyHashable,
    BaseKey == AnyHashable,
    BaseValue == Any)
@_specialize(
  exported: true,
  target: _dictionaryUpCast(_:),
  availability: SwiftStdlib 5.5, *;
  where
    DerivedKey == String,
    DerivedValue == AnyHashable,
    BaseKey == String,
    BaseValue == Any)
@available(SwiftStdlib 5.5, *)
@usableFromInline
func __specialize_dictionaryUpCast<
  DerivedKey, DerivedValue, BaseKey, BaseValue
>(
  _ source: Dictionary<DerivedKey, DerivedValue>
) -> Dictionary<BaseKey, BaseValue> {
  Builtin.unreachable()
}

@_specialize(
  exported: true,
  target: _copyCollectionToContiguousArray(_:),
  availability: SwiftStdlib 5.5, *;
  where C == [String])
@_specialize(
  exported: true,
  target: _copyCollectionToContiguousArray(_:),
  availability: SwiftStdlib 5.5, *;
  where C == Set<String>)
@available(SwiftStdlib 5.5, *)
@usableFromInline
func __specialize_copyCollectionToContiguousArray<C: Collection>(
  _ source: C
) -> ContiguousArray<C.Element> {
  Builtin.unreachable()
}
