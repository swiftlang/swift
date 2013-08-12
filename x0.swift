/// \brief A thing into which we can stream text
//
// FIXME: because of <rdar://problem/14410371> (passing an existential
// [byref] doesn't typecheck) and <rdar://problem/14497644> (Generic
// [byref] parameters don't typecheck), most value types can't usefully
// conform to OutputStream.
protocol [class_protocol] OutputStream {
  func append(text: String)
}


/// \brief Every String can be used as a OutputStream
/// directly
//
// Ideally, Streamable could take its
// parameter [byref] and we could pass a String to it directly.
// Today, we lose the contents of the String, so we pass a
// StringStream instead.  See explanation on OutputStream
extension String/*: OutputStream*/ {
  // func append(text: String) { this += text }
}

class StringStream : OutputStream {
  func append(text: String) { buffer += text }
  var buffer: String;
}

// \brief A thing that can be written to an OutputStream
protocol Streamable_ {
  func write<S: OutputStream>(s: S)
}

/// \brief A thing that can be printed in the REPL and the Debugger
///
/// Everything compiler-magically conforms to this protocol.  To
/// change the debug representation for a type, you don't need to
/// declare conformance: simply give the type a debugFormat().
protocol DebugPrintable {
  typealias DebugFormat: Streamable_/* = String*/

  /// \brief Produce a textual representation for the REPL and
  /// Debugger.
  ///
  /// Because String is a Streamable, your implementation of
  /// debugFormat can just return a String.  If you don't like
  /// String's default response to width, precision, and/or alignment,
  /// or if you want to write directly to the OutputStream
  /// for efficiency reasons, (e.g. if your representation is huge),
  /// you can return a custom DebugFormat type.
  ///
  /// NOTE: producing a representation that can be consumed by the
  /// REPL to produce an equivalent object is strongly encouraged
  /// where possible!  For example, String.debugFormat() produces a
  /// representation containing quotes, where special characters are
  /// escaped, etc.  A struct Point { var x, y: Int } might be
  /// represented as "Point(x: 3, y: 5)".
  func debugFormat() -> DebugFormat
}

func toDebugString<T:DebugPrintable>(x: T) -> String {
  var result = StringStream()
  x.debugFormat().write(result)
  return result.buffer
}

/// \brief A thing that can be print()ed and toString()ed.
///
/// Conformance to Printable is explicit, but if you want to use the
/// debugFormat() results for your type's format(), all you need
/// to do is declare conformance to Printable, and there's nothing to
/// implement.
///
/// Note: explicitness here keeps us from automatically polluting
/// completion results for every type with format() and
/// toString() functions.
protocol Printable: DebugPrintable {
  typealias PrintFormat: Streamable_, DebugPrintable/* = DebugFormat*/
  
  /// \brief produce a "pretty" textual representation that can be
  /// distinct from the debug format.  For example,
  /// String.format returns the string itself, without quoting.
  ///
  /// In general you can return a String here, but if you need more
  /// control, we strongly recommend returning a custom Format
  /// type, e.g. a nested struct of your type.  If you're lazy, you
  /// can conform to Streamable directly and just implement its
  /// write() func.
  func format() -> PrintFormat /*{
    return debugFormat()
  }*/
}

func toString<T:Streamable>(x: T) -> String {
  var result = StringStream()
  x.format().write(result)
  return result.buffer
}

func toString<T:Printable>(x: T) -> String {
  var result = StringStream()
  x.format().write(result)
  return result.buffer
}

protocol Streamable : Streamable_, Printable {
/*
  typealias PrintFormat // = This
  func format() -> PrintFormat {
    return this
  }
*/
}

extension String: Streamable {
  typealias DebugFormat = String

  func format() -> String {
    return this
  }
  func debugFormat() -> String {
    // FIXME: escape the characters
    return "\"" + this + "\""
  }
  func write<S: OutputStream>(s: S) {
    s.append(this)
  }
}

protocol StreamAdapter: OutputStream {
  static func adapt(base: OutputStream) -> This
}

class UpcaseStream: StreamAdapter {
  constructor(base: OutputStream) {
    this.base = base
  }

  static func adapt(base: OutputStream) -> UpcaseStream {
    return UpcaseStream(base)
  }

  func append(text: String) { base.append(text.uppercase) }

  var base: OutputStream
}

class DowncaseStream: StreamAdapter {
  constructor(base: OutputStream) {
    this.base = base
  }

  static func adapt(base: OutputStream) -> DowncaseStream {
    return DowncaseStream(base)
  }

  func append(text: String) { base.append(text.uppercase) }

  var base: OutputStream
}

struct Type<T> {}


struct StreamableAdapter<StreamAdapt: StreamAdapter, S: Streamable> : Streamable {
  constructor(base: S) {
    this.base.value = base
  }
  func write<S: OutputStream>(s: S) {
    base.value.write(StreamAdapt.adapt(s))
  }

  typealias PrintFormat = StreamableAdapter
  func format() -> StreamableAdapter {
    return this
  }

  typealias DebugFormat = String
  func debugFormat() -> DebugFormat {
    return "<StreamableAdapter>"
  }
  var base: GenericIVar<S>
}

operator infix ․ {
  associativity left
  precedence 9999
}

func uppercase_<S: Streamable>(s: S) -> StreamableAdapter<UpcaseStream,S> {
  return StreamableAdapter<UpcaseStream,S>(s)
}

typealias uppercase = Type<UpcaseStream>

func [infix] ․<S: Streamable, A: StreamAdapter>(s: S, a: Type<A>) -> StreamableAdapter<A,S> {
  return StreamableAdapter<A,S>(s)
}



class PrintStream : OutputStream {
  func append(s: String) { print(s) }
}

func print<T:Printable>(x: T) {
  x.format().write(PrintStream())
}

func println<T:Printable>(x: T) {
  print(x)
  PrintStream().append("\n")
}

var x0 = "foo"
var x1 = x0․uppercase()
var x2 = toString(x1)
println(x2)

/*
struct Radix {
  var value: Int;
}

oneof Hex {
  hex
}

oneof Binary {
  binary
}

protocol Numeric : IntegerLiteralConvertible, Comparable, SignedNumber, DebugPrintable {
  func __mod__(rhs: This) -> This
  func __div__(rhs: This) -> This
  static func fromInt(x: Int) -> This // FIXME: support ctors in protocols
  func toInt() -> Int
}

extension Int : Numeric {
  func __mod__(rhs: Int) -> Int { 
    return this % rhs 
  }
  func __div__(rhs: Int) -> Int {
    return this / rhs
  }
  static func fromInt(x: Int) -> Int {
    return x
  }
  func toInt() -> Int {
    return this
  }

  typealias DebugFormat = String // FIXME: shouldn't be needed (<rdar://problem/14418181>)
  func debugFormat() -> String {
    return String(this)
  }
}

func abs<T: Numeric>(x: T) -> T {
  return x < 0 ? -x : (x == 0 ? 0 : x);
}

func _writePositive<T:Numeric>( value: T, stream: OutputStream, radix: Int = 10 ) {
  if value == 0 {
    return
  }

  var radix_: T = T.fromInt(radix)
  var rest: T = value.__div__(radix_)
  var buf = _writePositive(rest, stream, radix)
  var digit = UInt32((value.__mod__(radix_)).toInt())
  var baseCharOrd : UInt32 = digit <= 9 ? '0'.value : 'A'.value - 10
  stream.append(String(Char(baseCharOrd + digit)))
}

func _writeSigned<T:Numeric>(value: T, target: OutputStream, radix: Int) {
  var result: String
  if value == 0 {
    target.append("0")
  }
  else {
    var absVal = abs(value)
    if (value < 0) {
      target.append("-")
    }
    _writePositive(absVal, target, radix)
  }
  target.append(result)
}

struct RadixFormat<T: Numeric> : Streamable, Printable {
  constructor(value: T, radix: Int) {
    this.value.value = value
    this.radix = radix
  }

  typealias PrintFormat = RadixFormat
  func format() -> RadixFormat {
    return this
  }

  func write(target: OutputStream) {
    _writeSigned(value.value, target, radix)
  }

  typealias DebugFormat = String
  func debugFormat() -> String {
    toDebugString(radix)
    return "RadixFormat(" + toDebugString(value.value) + ", " + toDebugString(radix) + ")"
  }

  var value : GenericIVar<T>
  var radix : Int
}


func |<T: Numeric>(lhs: T, rhs: Hex) -> RadixFormat<T> {
  return RadixFormat<T>(lhs, 16)
}

func |<T: Numeric>(lhs: T, rhs: Binary) -> RadixFormat<T> {
  return RadixFormat<T>(lhs, 2)
}

func |<T: Numeric>(lhs: T, rhs: Radix) -> RadixFormat<T> {
  return RadixFormat<T>(lhs, rhs.value)
}

var x = 42
var buf = OutputStreamWrapper("")
RadixFormat(x,10).write(buf)
println(buf.realStream.value)

buf.realStream.value += " "
(x | .hex).write(buf)
println(buf.realStream.value)

buf.realStream.value += " "
(x | .binary).write(buf)

println(buf.realStream.value)
println(x | .binary)
println(toDebugString(x | .binary))
println(toDebugString(x))

// This is an error
// println(x | .binary | .hex)

*/
*/
