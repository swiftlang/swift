// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// Text Formatting Prototype
//
// This file demonstrates the concepts proposed in TextFormatting.rst

// FIXME: Workaround for <rdar://problem/14011860> SubTLF: Default
// implementations in protocols.
infix operator ~> { precedence 255 }

/// \brief A thing into which we can stream text
protocol XOutputStream {
  mutating
  func append(text: String)
}

/// \brief Strings are XOutputStreams
extension String: XOutputStream {
  mutating
  func append(text: String) {
    self += text
  }
}

/// \brief A thing that can be written to an XOutputStream
protocol XStreamable {
  func writeTo<Target: XOutputStream>(inout target: Target)
}

/// \brief A thing that can be printed in the REPL and the Debugger
///
/// Everything compiler-magically conforms to this protocol.  To
/// change the debug representation for a type, you don't need to
/// declare conformance: simply give the type a debugFormat().
protocol XDebugPrintable {

  typealias DebugRepresentation : XStreamable // = String

  /// \brief Produce a textual representation for the REPL and
  /// Debugger.
  ///
  /// Because String is a XStreamable, your implementation of
  /// debugRepresentation can just return a String.  If you want to write
  /// directly to the XOutputStream for efficiency reasons,
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

/// \brief Strings are XStreamable
extension String : XStreamable {
  func writeTo<Target: XOutputStream>(inout target: Target) {
    target.append(self)
  }
}

// FIXME: Should be a method of XDebugPrintable once
// <rdar://problem/14692224> (Default Implementations in Protocols) is
// handled
func toDebugString <T:XDebugPrintable> (x: T) -> String {
  var result = ""
  x.debugFormat().writeTo(&result)
  return result
}

// FIXME: The following should be a method of XPrintable once
// <rdar://problem/14692224> (Default Implementations in Protocols) is
// handled
struct __PrintedFormat {}
func format() -> __PrintedFormat {
  return __PrintedFormat()
}
func ~> <T:XDebugPrintable> (x: T, _: __PrintedFormat) -> T.DebugRepresentation {
  return x.debugFormat()
}

/// \brief A thing that can be xprint()ed and toString()ed.
///
/// Conformance to XPrintable is explicit, but if you want to use the
/// debugFormat() results for your type's format(), all you need
/// to do is declare conformance to XPrintable, and there's nothing to
/// implement.
protocol XPrintable: XDebugPrintable {
  typealias PrintRepresentation: XStreamable = DebugRepresentation

  /// \brief produce a "pretty" textual representation that can be
  /// distinct from the debug format.  For example,
  /// String.printRepresentation returns the string itself, without quoting.
  ///
  /// In general you can return a String here, but if you need more
  /// control, we strongly recommend returning a custom Representation
  /// type, e.g. a nested struct of your type.  If you're lazy, you
  /// can conform to XStreamable directly and just implement its
  /// write() func.
  func ~> (x: Self, _: __PrintedFormat) -> PrintRepresentation
}


// FIXME: The following should be a method of XPrintable once
// <rdar://problem/14692224> (Default Implementations in Protocols) is
// handled

/// \brief Simply convert to String
///
/// Don't reimplement this: the default implementation always works.
/// If you must reimplement toString(), make sure its results are
/// consistent with those of format() (i.e. you shouldn't
/// change the behavior).
func toString<T: XPrintable>(x: T) -> String {
  var result = ""
  (x~>format()).writeTo(&result)
  return result
}

// \brief Streamer for the debug representation of String. When an
// EscapedStringFormat is written to a XOutputStream, it adds
// surrounding quotes and escapes special characters.
struct EscapedStringFormat : XStreamable {

  init(_ s: String) {
    self._value = s
  }

  func writeTo<Target: XOutputStream>(inout target: Target) {
    target.append("\"")
    for c in _value.unicodeScalars {
      target.append(c.escape(asASCII: true))
    }
    target.append("\"")
  }

  var _value: String
}

// FIXME: In theory, this shouldn't be needed
extension String: XDebugPrintable {}

extension String : XPrintable {
  func debugFormat() -> EscapedStringFormat {
    return EscapedStringFormat(self)
  }

  func format() -> String {
    return self
  }
}

/// \brief An integral type that can be printed
protocol XPrintableInteger : IntegerLiteralConvertible, Comparable, SignedNumberType, XPrintable {
  func %(lhs: Self, rhs: Self) -> Self
  func /(lhs: Self, rhs: Self) -> Self

  // FIXME: Stand-in for constructor pending <rdar://problem/13695680>
  // (Constructor requirements in protocols)
  static func fromInt(x: Int) -> Self
  func toInt() -> Int
}

extension Int : XDebugPrintable {
  func debugFormat() -> String { return String(self) }
}

extension Int : XPrintableInteger {
  static func fromInt(x: Int) -> Int { return x }
  func toInt() -> Int { return self }

  func getValue() -> Int {
    return self
  }
}

struct _formatArgs {
  var radix: Int, fill: String, width: Int
}

func format(radix radix: Int = 10, fill: String = " ", width: Int = 0) -> _formatArgs {
  return _formatArgs(radix: radix, fill: fill, width: width)
}

// FIXME: this function was a member of RadixFormat, but
// <rdar://problem/15525229> (SIL verification failed: operand of
// 'apply' doesn't match function input type) changed all that.
func _writePositive<T:XPrintableInteger, S: XOutputStream>(
  value: T, inout _ stream: S, _ args: _formatArgs) -> Int
{

  if value == 0 {
    return 0
  }

  var radix: T = T.fromInt(args.radix)
  var rest: T = value / radix
  var nDigits = _writePositive(rest, &stream, args)
  var digit = UInt32((value % radix).toInt())
  var baseCharOrd : UInt32 = digit <= 9 ? UnicodeScalar("0").value 
                                        : UnicodeScalar("A").value - 10
  stream.append(String(UnicodeScalar(baseCharOrd + digit)))
  return nDigits + 1
}

// FIXME: this function was a member of RadixFormat, but
// <rdar://problem/15525229> (SIL verification failed: operand of
// 'apply' doesn't match function input type) changed all that.
func _writeSigned<T:XPrintableInteger, S: XOutputStream>(
  value: T, inout _ target: S, _ args: _formatArgs
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

struct RadixFormat<T: XPrintableInteger> : XStreamable {
  init(value: T, args: _formatArgs) {
    self.value = value
    self.args = args
  }

  func writeTo<S: XOutputStream>(inout target: S) {
    _writeSigned(value, &target, args)
  }

  typealias DebugRepresentation = String
  func debugFormat() -> String {
    return "RadixFormat(" + toDebugString(value) + ", " + toDebugString(args.radix) + ")"
  }

  var value: T
  var args: _formatArgs
}

func ~> <T:XPrintableInteger> (x: T, _: __PrintedFormat) -> RadixFormat<T> {
  return RadixFormat(value: x, args: format())
}

func ~> <T:XPrintableInteger> (x: T, args: _formatArgs) -> RadixFormat<T> {
  return RadixFormat(value: x, args: args)
}

// ==========

//
// xprint and xprintln
//

struct StdoutStream : XOutputStream {
  func append(text: String) { Swift.print(text, appendNewline: false) }
  // debugging only
  func dump() -> String {
    return "<StdoutStream>"
  }
}

func xprint<Target: XOutputStream, T: XStreamable>(inout target: Target, _ x: T) {
  x.writeTo(&target)
}

func xprint<Target: XOutputStream, T: XPrintable>(inout target: Target, _ x: T) {
  xprint(&target, x~>format())
}

func xprint<T: XPrintable>(x: T) {
  var target = StdoutStream()
  xprint(&target, x)
}

func xprint<T: XStreamable>(x: T) {
  var target = StdoutStream()
  xprint(&target, x)
}

func xprintln<Target: XOutputStream, T: XPrintable>(inout target: Target, _ x: T) {
  xprint(&target, x)
  target.append("\n")
}

func xprintln<Target: XOutputStream, T: XStreamable>(inout target: Target, _ x: T) {
  xprint(&target, x)
  target.append("\n")
}

func xprintln<T: XPrintable>(x: T) {
  var target = StdoutStream()
  xprintln(&target, x)
}

func xprintln<T: XStreamable>(x: T) {
  var target = StdoutStream()
  xprintln(&target, x)
}

func xprintln(x: String) {
  var target = StdoutStream()
  x.writeTo(&target)
  "\n".writeTo(&target)
}

extension String {
  init <T: XStreamable>(_ x: T) {
    self = ""
    xprint(&self, x)
  }
}

func toPrettyString<T: XStreamable>(x: T) -> String {
  var result = "|"
  xprint(&result, x)
  result += "|"
  return result
}

// ===========

var x = "fubar\n\tbaz"

xprintln(x)
// CHECK: fubar
// CHECK-NEXT: 	baz

xprintln(toDebugString(x))
// CHECK-NEXT: "fubar\n\tbaz"

xprintln(toPrettyString(424242~>format(radix:16, width:8)))
// CHECK-NEXT: |   67932|

var zero = "0"
xprintln(toPrettyString(-434343~>format(width:8, fill:zero)))
// CHECK-NEXT: |-0434343|

xprintln(toPrettyString(-42~>format(radix:13, width:8)))
// CHECK-NEXT: |-     33|

xprintln(0x1EADBEEF~>format(radix:16))
// CHECK-NEXT: 1EADBEEF

// FIXME: rdar://16168414 this doesn't work in 32-bit
// xprintln(0xDEADBEEF~>format(radix:16))
// CHECK-NEXT-not: DEADBEEF
