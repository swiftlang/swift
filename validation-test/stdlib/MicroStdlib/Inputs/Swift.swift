//
// A bare-bones Swift standard library
//

public enum Optional<Wrapped> {
  case none
  case some(Wrapped)
}

public typealias IntegerLiteralType = Int
public typealias _MaxBuiltinIntegerType = Builtin.Int2048
public typealias _MaxBuiltinFloatType = Builtin.FPIEEE80

public protocol _BuiltinIntegerLiteralConvertible {
  init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType)
}

public protocol _BuiltinFloatLiteralConvertible {
  init(_builtinFloatLiteral value: _MaxBuiltinFloatType)
}

public protocol IntegerLiteralConvertible {
  associatedtype IntegerLiteralType : _BuiltinIntegerLiteralConvertible
  init(integerLiteral value: IntegerLiteralType)
}

public protocol FloatLiteralConvertible {
  associatedtype FloatLiteralType : _BuiltinFloatLiteralConvertible
  init(floatLiteral value: FloatLiteralType)
}

public struct Int : _BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value: Builtin.Word
  public init() {
    self = 0
  }
  public init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType) {
    let builtinValue = Builtin.truncOrBitCast_Int2048_Word(value)
    self.value = builtinValue
  }
  public init(integerLiteral value: IntegerLiteralType) {
    self = value
  }
}
public struct Int32 : _BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value: Builtin.Int32
  public init() {
    self.init(integerLiteral: 0)
  }
  public init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType) {
    let builtinValue = Builtin.truncOrBitCast_Int2048_Int32(value)
    self.value = builtinValue
  }
  public init(integerLiteral value: IntegerLiteralType) {
    let builtinValue = Builtin.truncOrBitCast_Word_Int32(value.value)
    self.value = builtinValue
  }
}
public struct Int8 : _BuiltinIntegerLiteralConvertible, IntegerLiteralConvertible {
  var value: Builtin.Int8
  public init() {
    self.init(integerLiteral: 0)
  }
  public init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType) {
    let builtinValue = Builtin.truncOrBitCast_Int2048_Int8(value)
    self.value = builtinValue
  }
  public init(integerLiteral value: IntegerLiteralType) {
    let builtinValue = Builtin.truncOrBitCast_Word_Int8(value.value)
    self.value = builtinValue
  }
}

public struct UnsafeMutablePointer<T> {
  var value: Builtin.RawPointer

  public init() {
    self.value = Builtin.inttoptr_Word(0.value)
  }
}

public typealias CInt = Int32
public typealias CChar = Int8

@_silgen_name("putchar")
public func putchar(_: CChar)

public func printHello() {
  putchar(0x48)
  putchar(0x65)
  putchar(0x6c)
  putchar(0x6c)
  putchar(0x6f)
  putchar(0x0a)
}

//public var C_ARGC: CInt = CInt()

//public var C_ARGV: UnsafeMutablePointer<UnsafeMutablePointer<Int8>> = UnsafeMutablePointer()

