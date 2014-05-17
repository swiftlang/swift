//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A wrapper around a C pointer to type T.
///
/// This wrapper stores a C pointer to an object of type T, and is
/// intended to be used to interface with C libraries. It provides no
/// automated memory management, and therefore the user must take care
/// to allocate and free memory appropriately.
///
/// For C pointers for which the pointed-to type cannot be represented
/// directly in Swift, the \c COpaquePointer will be used instead.
struct UnsafePointer<T> : BidirectionalIndex, Comparable, Hashable {
  /// The underlying raw (untyped) pointer.
  var value : Builtin.RawPointer

  /// Construct a null pointer.
  init() {
    self.value = Builtin.inttoptr_Word(0.value)
  }

  /// Construct an UnsafePointer from a builtin raw pointer.
  init(_ value : Builtin.RawPointer) {
    self.value = value
  }

  /// Convert from an opaque C pointer to a typed C pointer.
  ///
  /// This is a fundamentally unsafe conversion.
  init(_ other : COpaquePointer) {
    value = other.value
  }

  /// Construct an UnsafePointer from a given address in memory.
  ///
  /// This is a fundamentally unsafe conversion.
  init(_ value : Int) {
    self.value = Builtin.inttoptr_Word(value.value)
  }

  /// Convert from a pointer of a different type.
  ///
  /// This is a fundamentally unsafe conversion.
  init<U>(_ from : UnsafePointer<U>) {
    value = from.value
  }

  static func null() -> UnsafePointer {
    return UnsafePointer()
  }

  static func alloc(num: Int) -> UnsafePointer {
    // Don't bother with overflow checking.
    var size = Int(Builtin.strideof(T.self)) * num
    return UnsafePointer(Builtin.allocRaw(size.value, Builtin.alignof(T.self)))
  }

  func dealloc(num: Int) {
    // Overflow checking is actually not required here.
    var size = Int(Builtin.strideof(T.self)) * num
    Builtin.deallocRaw(value, size.value, Builtin.alignof(T.self))
  }

  /// Access the underlying raw memory, getting and
  /// setting values.
  var pointee : T {
    @transparent get {
      return Builtin.load(value)
    }
    @transparent nonmutating set {
      Builtin.assign(newValue, value)
    }
  }

  /// Initialize the value the pointer points to, to construct
  /// an object where there was no object previously stored.
  func initialize(newvalue: T) {
    Builtin.initialize(newvalue, value)
  }

  /// Retrieve the value the pointer points to, moving it away
  /// from the location referenced in memory.
  ///
  /// Postcondition: The value has been destroyed and the memory must
  /// be initialized before being used again.
  func move() -> T {
    return Builtin.take(value)
  }

  /// Move count values beginning at source into uninitialized memory,
  /// transforming the source values into raw memory, proceeding from
  /// the last value to the first.  Use this for copying ranges into
  /// later memory that may overlap with the source range.
  ///
  /// Requires: either `source` precedes `self` or follows `self + count`.
  func moveInitializeBackwardFrom(source: UnsafePointer, count: Int) {
    _debugPrecondition(
      source <= self || source > self + count,
      "moveInitializeBackwardFrom non-preceding overlapping range; use moveInitializeFrom instead")
    var src = source + count
    var dst = self + count
    while dst != self {
      (--dst).initialize((--src).move())
    }
  }

  /// Assign from count values beginning at source into initialized
  /// memory, transforming the source values into raw memory.
  func moveAssignFrom(source: UnsafePointer, count: Int) {
    _debugPrecondition(
      source > self || source < self - count,
      "moveAssignFrom non-following overlapping range")
    for i in 0..count {
      self[i] = (source + i).move()
    }
  }

  /// Move count values beginning at source into raw memory,
  /// transforming the source values into raw memory.
  func moveInitializeFrom(source: UnsafePointer, count: Int) {
    _debugPrecondition(
      source >= self || source < self - count,
      "moveInitializeFrom non-following overlapping range; use moveInitializeBackwardFrom")
    for i in 0..count {
      (self + i).initialize((source + i).move())
    }
  }

  /// Copy count values beginning at source into raw memory.
  func initializeFrom(source: UnsafePointer, count: Int) {
    _debugPrecondition(
      source >= self || source < self - count,
      "initializeFrom non-following overlapping range")
    for i in 0..count {
      (self + i).initialize(source[i])
    }
  }

  /// Copy the elements of `C` into raw memory.
  func initializeFrom<
    C: Collection where C._Element == T
  >(
    source: C
  ) {
    var p = self
    for x in source {
      p++.initialize((x as T)!)
    }
  }

  /// Destroy the object the pointer points to.
  func destroy() {
    Builtin.destroy(T.self, value)
  }

  /// Destroy the `count` objects the pointer points to.
  func destroy(count: Int) {
    Builtin.destroyArray(T.self, value, count.value)
  }

  func isNull() -> Bool {
    return self == UnsafePointer.null()
  }

  subscript (i : Int) -> T {
    @transparent
    get {
      return (self + i).pointee
    }
    @transparent
    nonmutating set {
      (self + i).pointee = newValue
    }
  }

  //
  // Protocol conformance
  //
  var hashValue: Int {
    return Int(Builtin.ptrtoint_Word(value))
  }
  func succ() -> UnsafePointer {
    return self + 1
  }
  func pred() -> UnsafePointer {
    return self - 1
  }

  //
  // Conversion to C argument pointers
  //

  // FIXME: Should be in an extension, but that doesn't work yet.
  @transparent @conversion
  func __conversion() -> CMutablePointer<T> {
    return CMutablePointer(owner: _nilNativeObject, value: value)
  }
  func __conversion() -> CMutableVoidPointer {
    return CMutableVoidPointer(owner: _nilNativeObject, value: value)
  }
  @transparent @conversion
  func __conversion() -> CConstPointer<T> {
    return CConstPointer(_nilNativeObject, value)
  }
  @transparent @conversion
  func __conversion() -> CConstVoidPointer {
    return CConstVoidPointer(_nilNativeObject, value)
  }
  @transparent @conversion
  func __conversion() -> AutoreleasingUnsafePointer<T> {
    return AutoreleasingUnsafePointer(value)
  }

  /// Construct from a CConstPointer.
  ///
  /// This is an explicit construction because it is not always safe.
  /// It is only allowed to convert an unscoped pointer, that is, one
  /// that does not have a lifetime-guaranteeing owner reference. To use
  /// a scoped pointer as an UnsafePointer, the withUnsafePointer method
  /// must be used instead.
  init(_ cp: CConstPointer<T>) {
    _precondition(!cp.scoped,
      "scoped CConstPointers must be converted using withUnsafePointer")
    self.value = cp.value
  }

  /// Construct from a CMutablePointer.
  ///
  /// This is an explicit construction because it is not always safe.
  /// It is only allowed to convert an unscoped pointer, that is, one
  /// that does not have a lifetime-guaranteeing owner reference. To use
  /// a scoped pointer as an UnsafePointer, the withUnsafePointer method
  /// must be used instead.
  init(_ cm: CMutablePointer<T>) {
    _precondition(!cm.scoped,
      "scoped CMutablePointers must be converted using withUnsafePointer")
    self.value = cm.value
  }

  /// Construct from an AutoreleasingUnsafePointer.
  ///
  /// This is an explicit construction
  /// because it is unsafe--UnsafePointer's store operations assume that
  /// the pointed-to storage has strong ownership, whereas AutoreleasingUnsafePointers
  /// reference +0 storage. Any values stored through the resulting
  /// UnsafePointer must be autoreleased.
  init(_ op: AutoreleasingUnsafePointer<T>) {
    self.value = op.value
  }

  /// Construct from a CConstVoidPointer.
  ///
  /// This is an explicit construction because it is not always safe.
  /// It is only allowed to convert an unscoped pointer, that is, one
  /// that does not have a lifetime-guaranteeing owner reference. To use
  /// a scoped pointer as an UnsafePointer, the withUnsafePointer method
  /// must be used instead.
  init(_ cp: CConstVoidPointer) {
    _precondition(!cp.scoped,
      "scoped CConstPointers must be converted using withUnsafePointer")
    self.value = cp.value
  }

  /// Construct from a CMutableVoidPointer.
  ///
  /// This is an explicit construction because it is not always safe.
  /// It is only allowed to convert an unscoped pointer, that is, one
  /// that does not have a lifetime-guaranteeing owner reference. To use
  /// a scoped pointer as an UnsafePointer, the withUnsafePointer method
  /// must be used instead.
  init(_ cp: CMutableVoidPointer) {
    _precondition(!cp.scoped,
      "scoped CMutableVoidPointers must be converted using withUnsafePointer")
    self.value = cp.value
  }
}

@transparent
func == <T> (lhs: T*, rhs: T*) -> Bool {
  return Bool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

@transparent
func < <T>(lhs: T*, rhs: T*) -> Bool {
  return Bool(Builtin.cmp_ult_RawPointer(lhs.value, rhs.value))
}

@transparent
func + <T>(lhs: T*, rhs: Int) -> T* {
  return UnsafePointer(
    Builtin.gep_Word(lhs.value, (rhs * Int(Builtin.strideof(T.self))).value))
}

@transparent
func + <T>(lhs: Int,
           rhs: T*) -> T* {
  return rhs + lhs
}

@transparent
func - <T>(lhs: T*, rhs: Int) -> T* {
  return lhs + -rhs
}

@transparent
func - <T>(lhs: T*, rhs: T*) -> Int {
  return
    Int(Builtin.sub_Word(Builtin.ptrtoint_Word(lhs.value),
                         Builtin.ptrtoint_Word(rhs.value)))
    / Int(Builtin.strideof(T.self))
}

@transparent
@assignment func += <T>(inout lhs: T*, rhs: Int) {
  lhs = lhs + rhs
}

@transparent
@assignment func -= <T>(inout lhs: T*, rhs: Int) {
  lhs = lhs - rhs
}

extension UnsafePointer : Printable {
  var description: String {
    return ""
  }
}

/// A byte-sized thing that isn't designed to interoperate with
/// any other types; it makes a decent parameter to UnsafePointer when
/// you just want to do bytewise pointer arithmetic.
struct RawByte {
  let _inaccessible: UInt8
}

// Make nil work with UnsafePointer
extension _Nil {
  @transparent
  @conversion func __conversion<T>() -> T* {
    return .null()
  }
}

