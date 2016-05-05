//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Instances of conforming types can be encoded, and appropriately
/// passed, as elements of a C `va_list`.
///
/// This protocol is useful in presenting C "varargs" APIs natively in
/// Swift.  It only works for APIs that have a `va_list` variant, so
/// for example, it isn't much use if all you have is:
///
/// ~~~ c
/// int c_api(int n, ...)
/// ~~~
///
/// Given a version like this, though,
///
/// ~~~ c
/// int c_api(int, va_list arguments)
/// ~~~
///
/// you can write:
///
///     func swiftAPI(_ x: Int, arguments: CVarArg...) -> Int {
///       return withVaList(arguments) { c_api(x, $0) }
///     }
public protocol CVarArg {
  // Note: the protocol is public, but its requirement is stdlib-private.
  // That's because there are APIs operating on CVarArg instances, but
  // defining conformances to CVarArg outside of the standard library is
  // not supported.

  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  var _cVarArgEncoding: [Int] { get }
}

/// Floating point types need to be passed differently on x86_64
/// systems.  CoreGraphics uses this to make CGFloat work properly.
public // SPI(CoreGraphics)
protocol _CVarArgPassedAsDouble : CVarArg {}

/// Some types require alignment greater than Int on some architectures.
public // SPI(CoreGraphics)
protocol _CVarArgAligned : CVarArg {
  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  var _cVarArgAlignment: Int { get }
}

#if arch(x86_64)
let _x86_64CountGPRegisters = 6
let _x86_64CountSSERegisters = 8
let _x86_64SSERegisterWords = 2
let _x86_64RegisterSaveWords = _x86_64CountGPRegisters + _x86_64CountSSERegisters * _x86_64SSERegisterWords
#endif

/// Invoke `body` with a C `va_list` argument derived from `args`.
public func withVaList<R>(_ args: [CVarArg],
  invoke body: @noescape (CVaListPointer) -> R) -> R {
  let builder = _VaListBuilder()
  for a in args {
    builder.append(a)
  }
  return _withVaList(builder, invoke: body)
}

/// Invoke `body` with a C `va_list` argument derived from `builder`.
internal func _withVaList<R>(
  _ builder: _VaListBuilder,
  invoke body: @noescape (CVaListPointer) -> R
) -> R {
  let result = body(builder.va_list())
  _fixLifetime(builder)
  return result
}

#if _runtime(_ObjC)
// Excluded due to use of dynamic casting and Builtin.autorelease, neither
// of which correctly work without the ObjC Runtime right now.
// See rdar://problem/18801510

/// Returns a `CVaListPointer` built from `args` that's backed by
/// autoreleased storage.
///
/// - Warning: This function is best avoided in favor of
///   `withVaList`, but occasionally (i.e. in a `class` initializer) you
///   may find that the language rules don't allow you to use
/// `withVaList` as intended.
@warn_unused_result
public func getVaList(_ args: [CVarArg]) -> CVaListPointer {
  let builder = _VaListBuilder()
  for a in args {
    builder.append(a)
  }
  // FIXME: Use some Swift equivalent of NS_RETURNS_INNER_POINTER if we get one.
  Builtin.retain(builder)
  Builtin.autorelease(builder)
  return builder.va_list()
}
#endif

@warn_unused_result
public func _encodeBitsAsWords<T : CVarArg>(_ x: T) -> [Int] {
  let result = [Int](
    repeating: 0,
    count: (sizeof(T.self) + sizeof(Int.self) - 1) / sizeof(Int.self))
  _sanityCheck(result.count > 0)
  var tmp = x
  // FIXME: use UnsafeMutablePointer.assignFrom() instead of memcpy.
  _memcpy(dest: UnsafeMutablePointer(result._baseAddressIfContiguous!),
          src: UnsafeMutablePointer(Builtin.addressof(&tmp)),
          size: UInt(sizeof(T.self)))
  return result
}

// CVarArg conformances for the integer types.  Everything smaller
// than a CInt must be promoted to CInt or CUnsignedInt before
// encoding.

// Signed types
extension Int : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}

extension Int64 : CVarArg, _CVarArgAligned {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }

  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  public var _cVarArgAlignment: Int {
    // FIXME: alignof differs from the ABI alignment on some architectures
    return alignofValue(self)
  }
}

extension Int32 : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}

extension Int16 : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(CInt(self))
  }
}

extension Int8 : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(CInt(self))
  }
}

// Unsigned types
extension UInt : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}

extension UInt64 : CVarArg, _CVarArgAligned {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }

  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  public var _cVarArgAlignment: Int {
    // FIXME: alignof differs from the ABI alignment on some architectures
    return alignofValue(self)
  }
}

extension UInt32 : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}

extension UInt16 : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(CUnsignedInt(self))
  }
}

extension UInt8 : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(CUnsignedInt(self))
  }
}

extension OpaquePointer : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}

extension UnsafePointer : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}

extension UnsafeMutablePointer : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}

#if _runtime(_ObjC)
extension AutoreleasingUnsafeMutablePointer : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}
#endif

extension Float : _CVarArgPassedAsDouble, _CVarArgAligned {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(Double(self))
  }

  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  public var _cVarArgAlignment: Int {
    // FIXME: alignof differs from the ABI alignment on some architectures
    return alignofValue(Double(self))
  }
}

extension Double : _CVarArgPassedAsDouble, _CVarArgAligned {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }

  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  public var _cVarArgAlignment: Int {
    // FIXME: alignof differs from the ABI alignment on some architectures
    return alignofValue(self)
  }
}

#if !arch(x86_64)

/// An object that can manage the lifetime of storage backing a
/// `CVaListPointer`.
final internal class _VaListBuilder {

  func append(_ arg: CVarArg) {
    // Write alignment padding if necessary.
    // This is needed on architectures where the ABI alignment of some
    // supported vararg type is greater than the alignment of Int, such
    // as non-iOS ARM. Note that we can't use alignof because it
    // differs from ABI alignment on some architectures.
#if arch(arm) && !os(iOS)
    if let arg = arg as? _CVarArgAligned {
      let alignmentInWords = arg._cVarArgAlignment / sizeof(Int)
      let misalignmentInWords = count % alignmentInWords
      if misalignmentInWords != 0 {
        let paddingInWords = alignmentInWords - misalignmentInWords
        appendWords([Int](repeating: -1, count: paddingInWords))
      }
    }
#endif

    // Write the argument's value itself.
    appendWords(arg._cVarArgEncoding)
  }

  @warn_unused_result
  func va_list() -> CVaListPointer {
    // Use Builtin.addressof to emphasize that we are deliberately escaping this
    // pointer and assuming it is safe to do so.
    let emptyAddr = UnsafeMutablePointer<Int>(
      Builtin.addressof(&_VaListBuilder.alignedStorageForEmptyVaLists))
    return CVaListPointer(_fromUnsafeMutablePointer: storage ?? emptyAddr)
  }

  // Manage storage that is accessed as Words
  // but possibly more aligned than that.
  // FIXME: this should be packaged into a better storage type

  func appendWords(_ words: [Int]) {
    let newCount = count + words.count
    if newCount > allocated {
      let oldAllocated = allocated
      let oldStorage = storage
      let oldCount = count

      allocated = max(newCount, allocated * 2)
      let newStorage = allocStorage(wordCount: allocated)
      storage = newStorage
      // count is updated below

      if let allocatedOldStorage = oldStorage {
        newStorage.moveInitializeFrom(allocatedOldStorage, count: oldCount)
        deallocStorage(wordCount: oldAllocated, storage: allocatedOldStorage)
      }
    }

    let allocatedStorage = storage!
    for word in words {
      allocatedStorage[count] = word
      count += 1
    }
  }

  @warn_unused_result
  func rawSizeAndAlignment(_ wordCount: Int) -> (Builtin.Word, Builtin.Word) {
    return ((wordCount * strideof(Int.self))._builtinWordValue,
      requiredAlignmentInBytes._builtinWordValue)
  }

  @warn_unused_result
  func allocStorage(wordCount: Int) -> UnsafeMutablePointer<Int> {
    let (rawSize, rawAlignment) = rawSizeAndAlignment(wordCount)
    let rawStorage = Builtin.allocRaw(rawSize, rawAlignment)
    return UnsafeMutablePointer<Int>(rawStorage)
  }

  func deallocStorage(
    wordCount: Int,
    storage: UnsafeMutablePointer<Int>
  ) {
    let (rawSize, rawAlignment) = rawSizeAndAlignment(wordCount)
    Builtin.deallocRaw(storage._rawValue, rawSize, rawAlignment)
  }

  deinit {
    if let allocatedStorage = storage {
      deallocStorage(wordCount: allocated, storage: allocatedStorage)
    }
  }

  // FIXME: alignof differs from the ABI alignment on some architectures
  let requiredAlignmentInBytes = alignof(Double.self)
  var count = 0
  var allocated = 0
  var storage: UnsafeMutablePointer<Int>? = nil

  static var alignedStorageForEmptyVaLists: Double = 0
}

#else

/// An object that can manage the lifetime of storage backing a
/// `CVaListPointer`.
final internal class _VaListBuilder {

  struct Header {
    var gp_offset = CUnsignedInt(0)
    var fp_offset = CUnsignedInt(_x86_64CountGPRegisters * strideof(Int.self))
    var overflow_arg_area: UnsafeMutablePointer<Int>? = nil
    var reg_save_area: UnsafeMutablePointer<Int>? = nil
  }

  init() {
    // prepare the register save area
    storage = ContiguousArray(repeating: 0, count: _x86_64RegisterSaveWords)
  }

  func append(_ arg: CVarArg) {
    var encoded = arg._cVarArgEncoding

    if arg is _CVarArgPassedAsDouble
      && sseRegistersUsed < _x86_64CountSSERegisters {
      var startIndex = _x86_64CountGPRegisters
           + (sseRegistersUsed * _x86_64SSERegisterWords)
      for w in encoded {
        storage[startIndex] = w
        startIndex += 1
      }
      sseRegistersUsed += 1
    }
    else if encoded.count == 1 && gpRegistersUsed < _x86_64CountGPRegisters {
      storage[gpRegistersUsed] = encoded[0]
      gpRegistersUsed += 1
    }
    else {
      for w in encoded {
        storage.append(w)
      }
    }
  }

  @warn_unused_result
  func va_list() -> CVaListPointer {
    header.reg_save_area = storage._baseAddress
    header.overflow_arg_area
      = storage._baseAddress + _x86_64RegisterSaveWords
    return CVaListPointer(
             _fromUnsafeMutablePointer: UnsafeMutablePointer<Void>(
               Builtin.addressof(&self.header)))
  }

  var gpRegistersUsed = 0
  var sseRegistersUsed = 0

  final  // Property must be final since it is used by Builtin.addressof.
  var header = Header()
  var storage: ContiguousArray<Int>
}

#endif

@available(*, unavailable, renamed: "CVarArg")
public typealias CVarArgType = CVarArg

@available(*, unavailable)
final public class VaListBuilder {}
