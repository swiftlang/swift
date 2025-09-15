
//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A type whose instances can be encoded, and appropriately passed, as
/// elements of a C `va_list`.
///
/// You use this protocol to present a native Swift interface to a C "varargs"
/// API. For example, a program can import a C API like the one defined here:
///
/// ~~~c
/// int c_api(int, va_list arguments)
/// ~~~
///
/// To create a wrapper for the `c_api` function, write a function that takes
/// `CVarArg` arguments, and then call the imported C function using the
/// `withVaList(_:_:)` function:
///
///     func swiftAPI(_ x: Int, arguments: CVarArg...) -> Int {
///         return withVaList(arguments) { c_api(x, $0) }
///     }
///
/// Swift only imports C variadic functions that use a `va_list` for their
/// arguments. C functions that use the `...` syntax for variadic arguments
/// are not imported, and therefore can't be called using `CVarArg` arguments.
///
/// If you need to pass an optional pointer as a `CVarArg` argument, use the
/// `Int(bitPattern:)` initializer to interpret the optional pointer as an
/// `Int` value, which has the same C variadic calling conventions as a pointer
/// on all supported platforms.
///
/// - Note: Declaring conformance to the `CVarArg` protocol for types defined
///   outside the standard library is not supported.
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
protocol _CVarArgPassedAsDouble: CVarArg {}

/// Some types require alignment greater than Int on some architectures.
public // SPI(CoreGraphics)
protocol _CVarArgAligned: CVarArg {
  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  var _cVarArgAlignment: Int { get }
}

#if !_runtime(_ObjC)
/// Some pointers require an alternate object to be retained.  The object
/// that is returned will be used with _cVarArgEncoding and held until
/// the closure is complete.  This is required since autoreleased storage
/// is not available on all platforms.
public protocol _CVarArgObject: CVarArg {
  /// Returns the alternate object that should be encoded.
  var _cVarArgObject: CVarArg { get }
}
#endif

#if arch(x86_64)
@usableFromInline
internal let _countGPRegisters = 6
// Note to future visitors concerning the following SSE register count.
//
// AMD64-ABI section 3.5.7 says -- as recently as v0.99.7, Nov 2014 -- to make
// room in the va_list register-save area for 16 SSE registers (XMM0..15). This
// may seem surprising, because the calling convention of that ABI only uses the
// first 8 SSE registers for argument-passing; why save the other 8?
//
// According to a comment in X86_64ABIInfo::EmitVAArg, in clang's TargetInfo,
// the AMD64-ABI spec is itself in error on this point ("NOTE: 304 is a typo").
// This comment (and calculation) in clang has been there since varargs support
// was added in 2009, in rev be9eb093; so if you're about to change this value
// from 8 to 16 based on reading the spec, probably the bug you're looking for
// is elsewhere.
@usableFromInline
internal let _countFPRegisters = 8
@usableFromInline
internal let _fpRegisterWords = 2
@usableFromInline
internal let _registerSaveWords = _countGPRegisters + _countFPRegisters * _fpRegisterWords
#elseif arch(s390x)
@usableFromInline
internal let _countGPRegisters = 16
@usableFromInline
internal let _registerSaveWords = _countGPRegisters

#elseif arch(arm64) && !(os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS) || os(Windows))
// ARM Procedure Call Standard for aarch64. (IHI0055B)
// The va_list type may refer to any parameter in a parameter list may be in one
// of three memory locations depending on its type and position in the argument
// list :
// 1. GP register save area x0 - x7
// 2. 128-bit FP/SIMD register save area q0 - q7
// 3. Stack argument area
@usableFromInline
internal let _countGPRegisters = 8
@usableFromInline
internal let _countFPRegisters = 8
@usableFromInline
internal let _fpRegisterWords = 16 /  MemoryLayout<Int>.size
@usableFromInline
internal let _registerSaveWords = _countGPRegisters + (_countFPRegisters * _fpRegisterWords)
#endif

#if arch(s390x)
@usableFromInline
internal typealias _VAUInt = CUnsignedLongLong
@usableFromInline
internal typealias _VAInt  = Int64
#else
@usableFromInline
internal typealias _VAUInt = CUnsignedInt
@usableFromInline
internal typealias _VAInt  = Int32
#endif

/// Invokes the given closure with a C `va_list` argument derived from the
/// given array of arguments.
///
/// The pointer passed as an argument to `body` is valid only during the
/// execution of `withVaList(_:_:)`. Do not store or return the pointer for
/// later use.
///
/// If you need to pass an optional pointer as a `CVarArg` argument, use the
/// `Int(bitPattern:)` initializer to interpret the optional pointer as an
/// `Int` value, which has the same C variadic calling conventions as a pointer
/// on all supported platforms.
///
/// - Parameters:
///   - args: An array of arguments to convert to a C `va_list` pointer.
///   - body: A closure with a `CVaListPointer` parameter that references the
///     arguments passed as `args`. If `body` has a return value, that value
///     is also used as the return value for the `withVaList(_:)` function.
///     The pointer argument is valid only for the duration of the function's
///     execution.
/// - Returns: The return value, if any, of the `body` closure parameter.
@inlinable // c-abi
public func withVaList<R>(_ args: [CVarArg],
  _ body: (CVaListPointer) -> R) -> R {
  let builder = unsafe __VaListBuilder()
  for a in args {
    unsafe builder.append(a)
  }
  let result = unsafe _withVaList(builder, body)
  _fixLifetime(args)
  return result
}

/// Invoke `body` with a C `va_list` argument derived from `builder`.
@inlinable // c-abi
internal func _withVaList<R>(
  _ builder: __VaListBuilder,
  _ body: (CVaListPointer) -> R
) -> R {
  let result = unsafe body(builder.va_list())
  unsafe _fixLifetime(builder)
  return result
}

#if _runtime(_ObjC)
// Excluded due to use of dynamic casting and Builtin.autorelease, neither
// of which correctly work without the ObjC Runtime right now.
// See rdar://problem/18801510

/// Returns a `CVaListPointer` that is backed by autoreleased storage, built
/// from the given array of arguments.
///
/// You should prefer `withVaList(_:_:)` instead of this function. In some
/// uses, such as in a `class` initializer, you may find that the language
/// rules do not allow you to use `withVaList(_:_:)` as intended.
///
/// If you need to pass an optional pointer as a `CVarArg` argument, use the
/// `Int(bitPattern:)` initializer to interpret the optional pointer as an
/// `Int` value, which has the same C variadic calling conventions as a pointer
/// on all supported platforms.
///
/// - Parameter args: An array of arguments to convert to a C `va_list`
///   pointer.
/// - Returns: A pointer that can be used with C functions that take a
///   `va_list` argument.
@inlinable // c-abi
public func getVaList(_ args: [CVarArg]) -> CVaListPointer {
  let builder = unsafe __VaListBuilder()
  for a in args {
    unsafe builder.append(a)
  }
  // FIXME: Use some Swift equivalent of NS_RETURNS_INNER_POINTER if we get one.
  unsafe Builtin.retain(builder)
  unsafe Builtin.autorelease(builder)
  return unsafe builder.va_list()
}
#endif

@inlinable // c-abi
public func _encodeBitsAsWords<T>(_ x: T) -> [Int] {
  let result = [Int](
    repeating: 0,
    count: (MemoryLayout<T>.size + MemoryLayout<Int>.size - 1) / MemoryLayout<Int>.size)
  _internalInvariant(!result.isEmpty)
  var tmp = x
  // FIXME: use UnsafeMutablePointer.assign(from:) instead of memcpy.
  unsafe _withUnprotectedUnsafeMutablePointer(to: &tmp) {
    unsafe _memcpy(dest: UnsafeMutablePointer(result._baseAddressIfContiguous!),
            src: $0,
            size: UInt(MemoryLayout<T>.size))
  }
  return result
}

// CVarArg conformances for the integer types.  Everything smaller
// than an Int32 must be promoted to Int32 or CUnsignedInt before
// encoding.

// Signed types
extension Int: CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}

extension Bool: CVarArg {
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(_VAInt(self ? 1:0))
  }
}

extension Int64: CVarArg, _CVarArgAligned {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }

  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  @inlinable // c-abi
  public var _cVarArgAlignment: Int {
    // FIXME: alignof differs from the ABI alignment on some architectures
    return MemoryLayout.alignment(ofValue: self)
  }
}

extension Int32: CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(_VAInt(self))
  }
}

extension Int16: CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(_VAInt(self))
  }
}

extension Int8: CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(_VAInt(self))
  }
}

// Unsigned types
extension UInt: CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }
}

extension UInt64: CVarArg, _CVarArgAligned {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }

  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  @inlinable // c-abi
  public var _cVarArgAlignment: Int {
    // FIXME: alignof differs from the ABI alignment on some architectures
    return MemoryLayout.alignment(ofValue: self)
  }
}

extension UInt32: CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(_VAUInt(self))
  }
}

extension UInt16: CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(_VAUInt(self))
  }
}

extension UInt8: CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(_VAUInt(self))
  }
}

extension OpaquePointer: @unsafe CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return unsafe _encodeBitsAsWords(self)
  }
}

@_preInverseGenerics
extension UnsafePointer: @unsafe CVarArg where Pointee: ~Copyable {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  @_preInverseGenerics
  public var _cVarArgEncoding: [Int] {
    return unsafe _encodeBitsAsWords(self)
  }
}

@_preInverseGenerics
extension UnsafeMutablePointer: @unsafe CVarArg where Pointee: ~Copyable {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  @_preInverseGenerics
  public var _cVarArgEncoding: [Int] {
    return unsafe _encodeBitsAsWords(self)
  }
}

#if _runtime(_ObjC)
extension AutoreleasingUnsafeMutablePointer: @unsafe CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable
  public var _cVarArgEncoding: [Int] {
    return unsafe _encodeBitsAsWords(self)
  }
}
#endif

extension Float: _CVarArgPassedAsDouble, _CVarArgAligned {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(Double(self))
  }

  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  @inlinable // c-abi
  public var _cVarArgAlignment: Int {
    // FIXME: alignof differs from the ABI alignment on some architectures
    return MemoryLayout.alignment(ofValue: Double(self))
  }
}

extension Double: _CVarArgPassedAsDouble, _CVarArgAligned {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // c-abi
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }

  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  @inlinable // c-abi
  public var _cVarArgAlignment: Int {
    // FIXME: alignof differs from the ABI alignment on some architectures
    return MemoryLayout.alignment(ofValue: self)
  }
}

#if !(os(Windows) || os(Android) || ($Embedded && !os(Linux) && !(os(macOS) || os(iOS) || os(watchOS) || os(tvOS)))) && (arch(i386) || arch(x86_64))
extension Float80: CVarArg, _CVarArgAligned {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs.
  @inlinable // FIXME(sil-serialize-all)
  public var _cVarArgEncoding: [Int] {
    return _encodeBitsAsWords(self)
  }

  /// Returns the required alignment in bytes of
  /// the value returned by `_cVarArgEncoding`.
  @inlinable // FIXME(sil-serialize-all)
  public var _cVarArgAlignment: Int {
    // FIXME: alignof differs from the ABI alignment on some architectures
    return MemoryLayout.alignment(ofValue: self)
  }
}
#endif

#if (arch(x86_64) && !os(Windows)) || arch(s390x) || (arch(arm64) && !(os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS) || os(Windows)))

/// An object that can manage the lifetime of storage backing a
/// `CVaListPointer`.
// NOTE: older runtimes called this _VaListBuilder. The two must
// coexist, so it was renamed. The old name must not be used in the new
// runtime.
@_fixed_layout
@usableFromInline // c-abi
@unsafe
final internal class __VaListBuilder {
  #if arch(x86_64) || arch(s390x)
  @frozen // c-abi
  @usableFromInline
  @safe
  internal struct Header {
    @usableFromInline // c-abi
    internal var gp_offset = CUnsignedInt(0)
    @usableFromInline // c-abi
    internal var fp_offset =
      CUnsignedInt(_countGPRegisters * MemoryLayout<Int>.stride)
    @usableFromInline // c-abi
    internal var overflow_arg_area: UnsafeMutablePointer<Int>?
    @usableFromInline // c-abi
    internal var reg_save_area: UnsafeMutablePointer<Int>?

    @inlinable // c-abi
    internal init() {}
  }
  #endif

  @usableFromInline // c-abi
  @safe
  internal var gpRegistersUsed = 0

  @usableFromInline // c-abi
  @safe
  internal var fpRegistersUsed = 0

  #if arch(x86_64) || arch(s390x)
  @usableFromInline // c-abi
  @safe
  final  // Property must be final since it is used by Builtin.addressof.
  internal var header = Header()
  #endif

  @usableFromInline // c-abi
  @safe
  internal var storage: ContiguousArray<Int>

#if !_runtime(_ObjC)
  @usableFromInline // c-abi
  @safe
  internal var retainer = [CVarArg]()
#endif

  @inlinable // c-abi
  internal init() {
    // prepare the register save area
    storage = ContiguousArray(repeating: 0, count: _registerSaveWords)
  }

  @inlinable // c-abi
  deinit {}

  @inlinable // c-abi
  internal func append(_ arg: CVarArg) {
#if !_runtime(_ObjC)
    var arg = arg

    // We may need to retain an object that provides a pointer value.
    if let obj = arg as? _CVarArgObject {
      arg = obj._cVarArgObject
      retainer.append(arg)
    }
#endif

    var encoded = arg._cVarArgEncoding

#if arch(x86_64) || arch(arm64)
    let isDouble = arg is _CVarArgPassedAsDouble

    if isDouble && fpRegistersUsed < _countFPRegisters {
      #if arch(arm64)
        var startIndex = fpRegistersUsed * _fpRegisterWords
      #else
        var startIndex = _countGPRegisters
             + (fpRegistersUsed * _fpRegisterWords)
      #endif
      for w in encoded {
        storage[startIndex] = w
        startIndex += 1
      }
      fpRegistersUsed += 1
    }
    else if encoded.count == 1
      && !isDouble
      && gpRegistersUsed < _countGPRegisters {
      #if arch(arm64)
        let startIndex = ( _fpRegisterWords * _countFPRegisters) + gpRegistersUsed
      #else
        let startIndex = gpRegistersUsed
      #endif
      storage[startIndex] = encoded[0]
      gpRegistersUsed += 1
    }
    else {
      for w in encoded {
        storage.append(w)
      }
    }
#elseif arch(s390x)
    if gpRegistersUsed < _countGPRegisters {
      for w in encoded {
        storage[gpRegistersUsed] = w
        gpRegistersUsed += 1
      }
    } else {
      for w in encoded {
        storage.append(w)
      }
    }
#endif

  }

  @inlinable // c-abi
  internal func va_list() -> CVaListPointer {
    #if arch(x86_64) || arch(s390x)
      unsafe header.reg_save_area = storage._baseAddress
      unsafe header.overflow_arg_area
        = storage._baseAddress + _registerSaveWords
      return unsafe CVaListPointer(
               _fromUnsafeMutablePointer: UnsafeMutableRawPointer(
                 Builtin.addressof(&self.header)))
    #elseif arch(arm64)
      let vr_top = unsafe storage._baseAddress + (_fpRegisterWords * _countFPRegisters)
      let gr_top = unsafe vr_top + _countGPRegisters

      return unsafe CVaListPointer(__stack: gr_top,
                                   __gr_top: gr_top,
                                   __vr_top: vr_top,
                                   __gr_off: -64,
                                   __vr_off: -128)
    #endif
  }
}

#else

/// An object that can manage the lifetime of storage backing a
/// `CVaListPointer`.
// NOTE: older runtimes called this _VaListBuilder. The two must
// coexist, so it was renamed. The old name must not be used in the new
// runtime.
@_fixed_layout
@usableFromInline // c-abi
@unsafe
final internal class __VaListBuilder {

  @inlinable // c-abi
  internal init() {}

  @inlinable // c-abi
  internal func append(_ arg: CVarArg) {
#if !_runtime(_ObjC)
    var arg = arg

    // We may need to retain an object that provides a pointer value.
    if let obj = arg as? _CVarArgObject {
      arg = obj._cVarArgObject
      unsafe retainer.append(arg)
    }
#endif

    // Write alignment padding if necessary.
    // This is needed on architectures where the ABI alignment of some
    // supported vararg type is greater than the alignment of Int, such
    // as non-iOS ARM. Note that we can't use alignof because it
    // differs from ABI alignment on some architectures.
#if (arch(arm) && !os(iOS)) || arch(arm64_32) || arch(wasm32)
    if let arg = arg as? _CVarArgAligned {
      let alignmentInWords = arg._cVarArgAlignment / MemoryLayout<Int>.size
      let misalignmentInWords = unsafe count % alignmentInWords
      if misalignmentInWords != 0 {
        let paddingInWords = alignmentInWords - misalignmentInWords
        unsafe appendWords([Int](repeating: -1, count: paddingInWords))
      }
    }
#endif

    // Write the argument's value itself.
    unsafe appendWords(arg._cVarArgEncoding)
  }

  // NB: This function *cannot* be @inlinable because it expects to project
  // and escape the physical storage of `__VaListBuilder.alignedStorageForEmptyVaLists`.
  // Marking it inlinable will cause it to resiliently use accessors to
  // project `__VaListBuilder.alignedStorageForEmptyVaLists` as a computed
  // property.
  @usableFromInline // c-abi
  internal func va_list() -> CVaListPointer {
    // Use Builtin.addressof to emphasize that we are deliberately escaping this
    // pointer and assuming it is safe to do so.
    let emptyAddr = unsafe UnsafeMutablePointer<Int>(
      Builtin.addressof(&__VaListBuilder.alignedStorageForEmptyVaLists))
    return unsafe CVaListPointer(_fromUnsafeMutablePointer: storage ?? emptyAddr)
  }

  // Manage storage that is accessed as Words
  // but possibly more aligned than that.
  // FIXME: this should be packaged into a better storage type

  @inlinable // c-abi
  internal func appendWords(_ words: [Int]) {
    let newCount = unsafe count + words.count
    if unsafe newCount > allocated {
      let oldAllocated = unsafe allocated
      let oldStorage = unsafe storage
      let oldCount = unsafe count

      unsafe allocated = unsafe max(newCount, allocated * 2)
      let newStorage = unsafe allocStorage(wordCount: allocated)
      unsafe storage = unsafe newStorage
      // count is updated below

      if let allocatedOldStorage = unsafe oldStorage {
        unsafe newStorage.moveInitialize(from: allocatedOldStorage, count: oldCount)
        unsafe deallocStorage(wordCount: oldAllocated, storage: allocatedOldStorage)
      }
    }

    let allocatedStorage = unsafe storage!
    for word in words {
      unsafe allocatedStorage[count] = word
      unsafe count += 1
    }
  }

  @inlinable // c-abi
  internal func rawSizeAndAlignment(
    _ wordCount: Int
  ) -> (Builtin.Word, Builtin.Word) {
    return unsafe ((wordCount * MemoryLayout<Int>.stride)._builtinWordValue,
      requiredAlignmentInBytes._builtinWordValue)
  }

  @inlinable // c-abi
  internal func allocStorage(wordCount: Int) -> UnsafeMutablePointer<Int> {
    let (rawSize, rawAlignment) = unsafe rawSizeAndAlignment(wordCount)
    let rawStorage = Builtin.allocRaw(rawSize, rawAlignment)
    return unsafe UnsafeMutablePointer<Int>(rawStorage)
  }

  @usableFromInline // c-abi
  internal func deallocStorage(
    wordCount: Int,
    storage: UnsafeMutablePointer<Int>
  ) {
    let (rawSize, rawAlignment) = unsafe rawSizeAndAlignment(wordCount)
    Builtin.deallocRaw(storage._rawValue, rawSize, rawAlignment)
  }

  @inlinable // c-abi
  deinit {
    if let allocatedStorage = unsafe storage {
      unsafe deallocStorage(wordCount: allocated, storage: allocatedStorage)
    }
  }

  // FIXME: alignof differs from the ABI alignment on some architectures
  @usableFromInline // c-abi
  internal let requiredAlignmentInBytes = MemoryLayout<Double>.alignment
  @usableFromInline // c-abi
  internal var count = 0
  @usableFromInline // c-abi
  internal var allocated = 0
  @usableFromInline // c-abi
  internal var storage: UnsafeMutablePointer<Int>?

#if !_runtime(_ObjC)
  @usableFromInline // c-abi
  internal var retainer = [CVarArg]()
#endif

  // Some code will call a variadic function without passing variadic parameters
  // where the function will still attempt to read variadic parameters. For
  // example, calling printf with an arbitrary string as the format string. This
  // is inherently unsound, but we'll try to be nice to such code by giving it
  // 64 bytes of zeroes in that case.
  internal static var alignedStorageForEmptyVaLists:
     (Double, Double, Double, Double, Double, Double, Double, Double)
         = (0, 0, 0, 0, 0, 0, 0, 0)
}

#endif

@available(*, unavailable)
extension __VaListBuilder: Sendable {}
