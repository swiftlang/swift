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

#if _runtime(_ObjC)
// Excluded due to use of dynamic casting and Builtin.autorelease, neither
// of which correctly work without the ObjC Runtime right now.
// See rdar://problem/18801510

/// Instances of conforming types can be encoded, and appropriately
/// passed, as elements of a C `va_list`.
///
/// This protocol is useful in presenting C "varargs" APIs natively in
/// Swift.  It only works for APIs that have a `va_list` variant, so
/// for example, it isn't much use if all you have is::
///
///   int f(int n, ...)
///
/// Given a version like this, though, ::
///
///   int f(int, va_list arguments)
///
/// you can write::
///
///   func swiftF(x: Int, arguments: CVarArgType...) -> Int {
///     return withVaList(arguments) { f(x, $0) }
///   }
public protocol CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  func encode() -> [Word]
}

/// Floating point types need to be passed differently on x86_64
/// systems.  CoreGraphics uses this to make CGFloat work properly.
public // SPI(CoreGraphics)
protocol _CVarArgPassedAsDouble : CVarArgType {}

#if arch(x86_64)
let _x86_64CountGPRegisters = 6
let _x86_64CountSSERegisters = 8
let _x86_64SSERegisterWords = 2
let _x86_64RegisterSaveWords = _x86_64CountGPRegisters + _x86_64CountSSERegisters * _x86_64SSERegisterWords
#endif

/// Invoke `f` with a C `va_list` argument derived from `args`.
public func withVaList<R>(args: [CVarArgType],
  @noescape f: CVaListPointer -> R) -> R {
  var builder = VaListBuilder()
  for a in args {
    builder.append(a)
  }
  return withVaList(builder, f)
}

/// Invoke `f` with a C `va_list` argument derived from `builder`.
public func withVaList<R>(builder: VaListBuilder,
  @noescape f: CVaListPointer -> R) -> R {
  let result = f(builder.va_list())
  _fixLifetime(builder)
  return result
}

/// Returns a `CVaListPointer` built from `args` that's backed by
/// autoreleased storage.
///
/// .. Warning:: This function is best avoided in favor of
///    `withVaList`, but occasionally (i.e. in a `class` initializer) you
///    may find that the language rules don't allow you to use
///    `withVaList` as intended.
public func getVaList(args: [CVarArgType]) -> CVaListPointer {
  var builder = VaListBuilder()
  for a in args {
    builder.append(a)
  }
  // FIXME: Use some Swift equivalent of NS_RETURNS_INNER_POINTER if we get one.
  Builtin.retain(builder)
  Builtin.autorelease(builder)
  return builder.va_list()
}

public func _encodeBitsAsWords<T: CVarArgType>(x: T) -> [Word] {
  var result = [Word](
    count: (sizeof(T.self) + sizeof(Word.self) - 1) / sizeof(Word.self),
    repeatedValue: 0)
  var tmp = x
  _memcpy(dest: UnsafeMutablePointer(result._baseAddressIfContiguous),
          src: UnsafeMutablePointer(Builtin.addressof(&tmp)),
          size: UInt(sizeof(T.self)))
  return result
}

// CVarArgType conformances for the integer types.  Everything smaller
// than a CInt must be promoted to CInt or CUnsignedInt before
// encoding.

// Signed types
extension Int : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension Int64 : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension Int32 : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension Int16 : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(CInt(self))
  }
}

extension Int8 : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(CInt(self))
  }
}

// Unsigned types
extension UInt : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension UInt64 : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension UInt32 : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension UInt16 : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(CUnsignedInt(self))
  }
}

extension UInt8 : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(CUnsignedInt(self))
  }
}

extension COpaquePointer : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension CFunctionPointer : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension UnsafePointer : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension UnsafeMutablePointer : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension AutoreleasingUnsafeMutablePointer : CVarArgType {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension Float : _CVarArgPassedAsDouble {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(Double(self))
  }
}

extension Double : _CVarArgPassedAsDouble {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

#if !arch(x86_64)

/// An object that can manage the lifetime of storage backing a
/// `CVaListPointer`
final public class VaListBuilder {
  
  func append(arg: CVarArgType) {
    for x in arg.encode() {
      storage.append(x)
    }
  }
  
  func va_list() -> CVaListPointer {
    return CVaListPointer(
      _fromUnsafeMutablePointer: UnsafeMutablePointer<Void>(
        storage._baseAddressIfContiguous))
  }

  var storage = [Word]()
}

#else

/// An object that can manage the lifetime of storage backing a
/// `CVaListPointer`
final public class VaListBuilder {

  struct Header {
    var gp_offset = CUnsignedInt(0)
    var fp_offset = CUnsignedInt(_x86_64CountGPRegisters * strideof(Word.self))
    var overflow_arg_area: UnsafeMutablePointer<Word> = nil
    var reg_save_area: UnsafeMutablePointer<Word> = nil
  }
  
  init() {
    // prepare the register save area
    storage = Array(count: _x86_64RegisterSaveWords, repeatedValue: 0)
  }

  func append(arg: CVarArgType) {
    var encoded = arg.encode()

    if arg is _CVarArgPassedAsDouble
      && sseRegistersUsed < _x86_64CountSSERegisters {
      var startIndex = _x86_64CountGPRegisters
           + (sseRegistersUsed * _x86_64SSERegisterWords)
      for w in encoded {
        storage[startIndex] = w
        ++startIndex
      }
      ++sseRegistersUsed
    }
    else if encoded.count == 1 && gpRegistersUsed < _x86_64CountGPRegisters {
      storage[gpRegistersUsed++] = encoded[0]
    }
    else {
      for w in encoded {
        storage.append(w)
      }
    }
  }

  func va_list() -> CVaListPointer {
    header.reg_save_area = storage._baseAddressIfContiguous
    header.overflow_arg_area
      = storage._baseAddressIfContiguous + _x86_64RegisterSaveWords
    return CVaListPointer(
             _fromUnsafeMutablePointer: UnsafeMutablePointer<Void>(
               Builtin.addressof(&self.header)))
  }

  var gpRegistersUsed = 0
  var sseRegistersUsed = 0

  final  // Property must be final since it is used by Builtin.addressof.
  var header = Header()
  var storage: [Word]
}

#endif

#endif // _runtime(_ObjC)
