// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

// Text Formatting Prototype
//
// This file demonstrates the concepts proposed in TextFormatting.rst

// FIXME: Workaround for <rdar://problem/14011860> SubTLF: Default
// implementations in protocols.
operator infix ~> { precedence 255 }

/// \brief A thing into which we can stream text
protocol OutputStream {
  func append(s: String)
}

/// \brief Strings are OutputStreams
extension String: OutputStream {
  func append(text: String) {
    self += text
  }
}

/// \brief A thing that can be written to an OutputStream
protocol Streamable {
  func writeTo<Target: OutputStream>(target: @inout Target)
}

/// \brief A thing that can be printed in the REPL and the Debugger
///
/// Everything compiler-magically conforms to this protocol.  To
/// change the debug representation for a type, you don't need to
/// declare conformance: simply give the type a debugFormat().
protocol DebugPrintable {

  typealias DebugRepresentation : Streamable // = String

  /// \brief Produce a textual representation for the REPL and
  /// Debugger.
  ///
  /// Because String is a Streamable, your implementation of
  /// debugRepresentation can just return a String.  If you want to write
  /// directly to the OutputStream for efficiency reasons,
  /// (e.g. if your representation is huge), you can return a custom
  /// DebugRepresentation type.
  ///
  /// NOTE: producing a representation that can be consumed by the
  /// REPL to produce an equivalent object is strongly encouraged
  /// where possible!  For example, String.debugFormat() produces a
  /// representation containing quotes, where special characters are
  /// escaped, etc.  A struct Point { var x, y: Int } might be
  /// represented as "Point(x: 3, y: 5)".
  func debugFormat() -> DebugRepresentation
}

/// \brief Strings are Streamable
extension String : Streamable {
  func writeTo<Target: OutputStream>(target: @inout Target) {
    target.append(self)
  }
}

// FIXME: Should be a method of DebugPrintable once
// <rdar://problem/14692224> (Default Implementations in Protocols) is
// handled
func toDebugString <T:DebugPrintable> (x: T) -> String {
  var result = ""
  x.debugFormat().writeTo(&result)
  return result
}

// FIXME: The following should be a method of Printable once
// <rdar://problem/14692224> (Default Implementations in Protocols) is
// handled
struct __PrintedFormat {}
func format() -> __PrintedFormat { 
  return __PrintedFormat()
}
@infix func ~> <T:DebugPrintable> (x: T, _: __PrintedFormat) -> T.DebugRepresentation {
  return x.debugFormat()
}

/// \brief A thing that can be print()ed and toString()ed.
///
/// Conformance to Printable is explicit, but if you want to use the
/// debugFormat() results for your type's format(), all you need
/// to do is declare conformance to Printable, and there's nothing to
/// implement.
protocol Printable: DebugPrintable {
  typealias PrintRepresentation: Streamable // = DebugRepresentation
  
  /// \brief produce a "pretty" textual representation that can be
  /// distinct from the debug format.  For example,
  /// String.printRepresentation returns the string itself, without quoting.
  ///
  /// In general you can return a String here, but if you need more
  /// control, we strongly recommend returning a custom Representation
  /// type, e.g. a nested struct of your type.  If you're lazy, you
  /// can conform to Streamable directly and just implement its
  /// write() func.
  @infix func ~> (x: Self, _: __PrintedFormat) -> PrintRepresentation
}


// FIXME: The following should be a method of Printable once
// <rdar://problem/14692224> (Default Implementations in Protocols) is
// handled

/// \brief Simply convert to String
///
/// Don't reimplement this: the default implementation always works.
/// If you must reimplement toString(), make sure its results are
/// consistent with those of format() (i.e. you shouldn't
/// change the behavior).
func toString<T: Printable>(x: T) -> String {
  var result = ""
  (x~>format()).writeTo(&result)
  return result
}

// \brief Streamer for the debug representation of String. When an
// EscapedStringFormat is written to a OutputStream, it adds
// surrounding quotes and escapes special characters.
struct EscapedStringFormat : Streamable {

  init(s: String) {
    self._value = s
  }

  func writeTo<Target: OutputStream>(target: @inout Target) {
    target.append("\"")
    for c in CodePoints(_value) {
      target.append(c.escape())
    }
    target.append("\"")
  }

  var _value: String
}

// FIXME: In theory, this shouldn't be needed
extension String: DebugPrintable {}

extension String : Printable {
  func debugFormat() -> EscapedStringFormat {
    return EscapedStringFormat(self) 
  }

  func format() -> String { 
    return self
  }
}

/// \brief An integral type that can be printed
protocol PrintableInteger : IntegerLiteralConvertible, Comparable, SignedNumber, Printable {
  func %(lhs: Self, rhs: Self) -> Self
  func /(lhs: Self, rhs: Self) -> Self

  // FIXME: Stand-in for constructor pending <rdar://problem/13695680>
  // (Constructor requirements in protocols)
  static func fromInt(x: Int) -> Self
  func toInt() -> Int
}

extension Int : DebugPrintable {
  func debugFormat() -> String { return String(self) }
}

extension Int : PrintableInteger {
  static func fromInt(x: Int) -> Int { return x }
  func toInt() -> Int { return self }

  func getValue() -> Int {
    return self
  }
}

func abs<T: SignedNumber>(x: T) -> T {
  return x.isNegative() ? -x : x
}

struct _formatArgs {
  var radix: Int, fill: String, width: Int
}

func format(radix: Int = 10, fill: String = " ", width: Int = 0) -> _formatArgs {
  return _formatArgs(radix: radix, fill: fill, width: width)
}

// FIXME: this function was a member of RadixFormat, but
// <rdar://problem/15525229> (SIL verification failed: operand of
// 'apply' doesn't match function input type) changed all that.
func _writePositive<T:PrintableInteger, S: OutputStream>( 
  value: T, stream: @inout S, args: _formatArgs) -> Int
{

  if value == 0 {
    return 0
  }

  var radix: T = T.fromInt(args.radix)
  var rest: T = value / radix
  var nDigits = _writePositive(rest, &stream, args)
  var digit = UInt32((value % radix).toInt())
  var baseCharOrd : UInt32 = digit <= 9 ? '0'.value : 'A'.value - 10
  stream.append(String(Char(baseCharOrd + digit)))
  return nDigits + 1
}

// FIXME: this function was a member of RadixFormat, but
// <rdar://problem/15525229> (SIL verification failed: operand of
// 'apply' doesn't match function input type) changed all that.
func _writeSigned<T:PrintableInteger, S: OutputStream>(
  value: T, target: @inout S, args: _formatArgs
) {
  var width = 0
  var result = ""

  if value == 0 {
    result = "0"
    ++width
  }
  else {
    var absVal = abs(value)
    if (value < 0) {
      target.append("-")
      ++width
    }
    width += _writePositive(absVal, &result, args)
  }

  while width < args.width {
    ++width
    target.append(args.fill)
  }
  target.append(result)
}

struct RadixFormat<T: PrintableInteger> : Streamable {
  init(value: T, args: _formatArgs) {
    self.value = value
    self.args = args
  }

  func writeTo<S: OutputStream>(target: @inout S) {
    _writeSigned(value, &target, args)
  }

  typealias DebugRepresentation = String
  func debugFormat() -> String {
    return "RadixFormat(" + toDebugString(value) + ", " + toDebugString(args.radix) + ")"
  }

  var value: T
  var args: _formatArgs
}

@infix func ~> <T:PrintableInteger> (x: T, _: __PrintedFormat) -> RadixFormat<T> {
  return RadixFormat(x, format())
}

@infix func ~> <T:PrintableInteger> (x: T, args: _formatArgs) -> RadixFormat<T> {
  return RadixFormat(x, args)
}

// ==========

//
// print and println
//

struct StdoutStream : OutputStream {
  func append(s: String) { swift.print(s) }
  // debugging only
  func dump() -> String {
    return "<StdoutStream>"
  }
}

func print<Target: OutputStream, T: Streamable>(target: @inout Target, x: T) {
  x.writeTo(&target)
}

func print<Target: OutputStream, T: Printable>(target: @inout Target, x: T) {
  print(&target, x~>format())
}

func print<T: Printable>(x: T) {
  var target = StdoutStream()
  print(&target, x)
}

func print<T: Streamable>(x: T) {
  var target = StdoutStream()
  print(&target, x)
}

func println<Target: OutputStream, T: Printable>(target: @inout Target, x: T) {
  print(&target, x)
  target.append("\n")
}

func println<Target: OutputStream, T: Streamable>(target: @inout Target, x: T) {
  print(&target, x)
  target.append("\n")
}

func println<T: Printable>(x: T) {
  var target = StdoutStream()
  println(&target, x)
}

func println<T: Streamable>(x: T) {
  var target = StdoutStream()
  println(&target, x)
}

extension String {
  init <T: Streamable>(x: T) {
    self = ""
    print(&self, x)
  }
}

// ===========

var x = "fubar\n\tbaz"

println(x)
// CHECK: fubar
// CHECK-NEXT: 	baz

println(toDebugString(x))
// CHECK-NEXT: "fubar\n\tbaz"

println("|\(424242~>format(radix:16, width:8))|") 
// CHECK-NEXT: |   67932|

var zero = "0"
println("|\(-434343~>format(width:8, fill:zero))|")
// CHECK-NEXT: |-0434343|

println("|\(-42~>format(radix:13, width:8))|")
// CHECK-NEXT: |-     33|

println(0xDEADBEEF~>format(radix:16))
// CHECK-NEXT: DEADBEEF
