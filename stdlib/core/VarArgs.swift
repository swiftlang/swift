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

public protocol CVarArgType {
  func encode() -> [Word]
}

#if arch(x86_64)
let _x86_64CountGPRegisters = 6
let _x86_64CountSSERegisters = 8
let _x86_64SSERegisterWords = 2
let _x86_64RegisterSaveWords = _x86_64CountGPRegisters + _x86_64CountSSERegisters * _x86_64SSERegisterWords
#endif

public func withVaList<R>(args: [CVarArgType], f: (CVaListPointer)->R) -> R {
  var builder = VaListBuilder()
  for a in args {
    builder.append(a)
  }
  return withVaList(builder, f)
}

public func withVaList<R>(builder: VaListBuilder, f: (CVaListPointer)->R) -> R {
  let result = f(builder.va_list())
  _fixLifetime(builder)
  return result
}

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
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension Int64 : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension Int32 : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension Int16 : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(CInt(self))
  }
}

extension Int8 : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(CInt(self))
  }
}

// Unsigned types
extension UInt : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension UInt64 : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension UInt32 : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension UInt16 : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(CUnsignedInt(self))
  }
}

extension UInt8 : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(CUnsignedInt(self))
  }
}

extension COpaquePointer : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

extension Float : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(Double(self))
  }
}

extension Double : CVarArgType {
  public func encode() -> [Word] {
    return _encodeBitsAsWords(self)
  }
}

#if !arch(x86_64)

final public
class VaListBuilder {
  
  func append(arg: CVarArgType) {
    for x in arg.encode() {
      storage.append(x)
    }
  }
  
  func va_list() -> CVaListPointer {
    return CVaListPointer(
      fromUnsafeMutablePointer: UnsafeMutablePointer<Void>(
        storage._baseAddressIfContiguous))
  }

  var storage = [Word]()
}

#else

final public
class VaListBuilder {

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
    
    if ((arg as? Float) || (arg as? Double))
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
             fromUnsafeMutablePointer: UnsafeMutablePointer<Void>(
               Builtin.addressof(&self.header)))
  }

  var gpRegistersUsed = 0
  var sseRegistersUsed = 0

  final  // Property must be final since it is used by Builtin.addressof.
  var header = Header()
  var storage: [Word]
}

#endif
