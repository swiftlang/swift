protocol CVarArg {
  func encode() -> Word[]
}

let _x86_64CountGPRegisters = 6
let _x86_64CountSSERegisters = 8
let _x86_64SSERegisterWords = 2
let _x86_64RegisterSaveWords = _x86_64CountGPRegisters + _x86_64CountSSERegisters * _x86_64SSERegisterWords

@asmname="swift_runningOnX86_64" func swift_runningOnX86_64() -> Bool

func withVaList<R>(args: CVarArg[], f: (CVaList)->R) -> R {
  var argList: C_va_list = makeC_va_list()
  for a in args {
    argList.append(a)
  }
  return f(argList)
}


// FIXME: workaround for <rdar://problem/15715225>
func sizeof(_:Word.metatype) -> Int {
  return Int(Word(Builtin.sizeof(Word)))
}

func encodeBitsAsWords<T: CVarArg>(x: T) -> Word[] {
  var result = Array<Word>(
    (sizeof(T) + sizeof(Word) - 1) / sizeof(Word), 0)

  c_memcpy(result.base, addressof(&x), UInt64(sizeof(T)))
  return result
}

// CVarArg conformances for the integer types.  Everything smaller
// than a CInt must be promoted to CInt or CUnsignedInt before
// encoding.

// Signed types
extension Int64 : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(self)
  }
}

extension Int32 : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(self)
  }
}

extension Int16 : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(CInt(self))
  }
}

extension Int8 : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(CInt(self))
  }
}

// Unsigned types
extension UInt64 : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(self)
  }
}

extension UInt32 : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(self)
  }
}

extension UInt16 : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(CUnsignedInt(self))
  }
}

extension UInt8 : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(CUnsignedInt(self))
  }
}

extension COpaquePointer : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(self)
  }
}

extension Float : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(Double(self))
  }
}

extension Double : CVarArg {
  func encode() -> Word[] {
    return encodeBitsAsWords(self)
  }
}

protocol C_va_list {
  func append(arg: CVarArg)

  @conversion
  func __conversion() -> COpaquePointer
}

struct BasicC_va_list : C_va_list {
  
  func append(arg: CVarArg) {
    for x in arg.encode() {
      storage.append(x)
    }
  }
  
  @conversion
  func __conversion() -> COpaquePointer {
    return COpaquePointer(storage.base)
  }

  var storage: Word[] = Array<Word>()
}

func &&<T: LogicValue>(a: T, b: @auto_closure () -> T) -> T {
  return a ? b() : a
}

func ||<T: LogicValue>(a: T, b: @auto_closure () -> T) -> T {
  return a ? a : b()
}

struct X86_64_va_list : C_va_list {

  struct Header {
    var gp_offset = CUnsignedInt(0)
    var fp_offset = CUnsignedInt(_x86_64CountGPRegisters * sizeof(Word))
    var overflow_arg_area: UnsafePointer<Word> = UnsafePointer<Word>.null()
    var reg_save_area: UnsafePointer<Word> = UnsafePointer<Word>.null()
  }
  
  init() {
    // prepare the register save area
    storage = Array(_x86_64RegisterSaveWords, Word(0))
  }
  
  func append(arg: CVarArg) {
    var encoded = arg.encode()
    
    if ((arg as Float) || (arg as Double)) && sseRegistersUsed < _x86_64CountSSERegisters {
      var startIndex = _x86_64CountGPRegisters + (sseRegistersUsed * _x86_64SSERegisterWords)
      var endIndex = startIndex + encoded.count
      storage[startIndex..endIndex] = encoded
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

  @conversion
  func __conversion() -> COpaquePointer {
    header.reg_save_area = storage.base
    header.overflow_arg_area = storage.base + _x86_64RegisterSaveWords
    return COpaquePointer(addressof(&self.header))
  }

  var gpRegistersUsed = 0
  var sseRegistersUsed = 0
  var header = Header()
  var storage: Word[]
}

func makeC_va_list() -> C_va_list {
  if swift_runningOnX86_64() {
    return X86_64_va_list()
  }
  else {
    return BasicC_va_list()
  }
}
