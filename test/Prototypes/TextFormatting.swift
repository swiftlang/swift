// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Text Formatting Prototype
//
// This file demonstrates the concepts proposed in TextFormatting.rst
//
// We may not want the following protocol exactly as-is; it overlaps somewhat
// with CustomStringConvertible and CustomDebugStringConvertible, and it's not
// clear whether a protocol is even needed.

/// A type that supports format() and debugFormat().
protocol CustomFormatted : CustomStringConvertible, CustomDebugStringConvertible {

  associatedtype DebugRepresentation : TextOutputStreamable = String
  associatedtype PrintRepresentation : TextOutputStreamable = DebugRepresentation
  
  /// Produce a textual representation for the REPL and
  /// Debugger.
  ///
  /// Because String is a TextOutputStreamable, your implementation of
  /// debugRepresentation can just return a String.  If you want to write
  /// directly to the TextOutputStream for efficiency reasons,
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

  /// produce a "pretty" textual representation that can be
  /// distinct from the debug format.  For example,
  /// String.printRepresentation returns the string itself, without quoting.
  ///
  /// In general you can return a String here, but if you need more
  /// control, we strongly recommend returning a custom Representation
  /// type, e.g. a nested struct of your type.  If you're lazy, you
  /// can conform to TextOutputStreamable directly and just implement its
  /// write() func.
  func format() -> PrintRepresentation
}

extension CustomFormatted where PrintRepresentation == DebugRepresentation {
  func format() -> DebugRepresentation { return debugFormat() }
}

/// CustomFormatted's conformance to CustomStringConvertible and
/// CustomDebugStringConvertible
extension CustomFormatted {
  public var description: String {
    return _description
  }
  
  public var debugDescription: String {
    return _debugDescription
  }

  //===--- Using [debug]description directly might pick up the stdlib -----===//
  //===--- versions and skew test results, so use these for testing -------===//
  var _description: String {
    var result = ""
    x.format().write(to: &result)
    return result
  }
  
  var _debugDescription: String {
    var result = ""
    x.debugFormat().write(to: &result)
    return result
  }
}

extension String : CustomFormatted {
  // Streamer for the debug representation of String. When an
  // EscapedStringFormat is written to a TextOutputStream, it adds
  // surrounding quotes and escapes special characters.
  struct EscapedFormat : TextOutputStreamable {

    init(_ s: String) {
      self._value = s
    }

    func write<Target: TextOutputStream>(to target: inout Target) {
      target.write("\"")
      for c in _value.unicodeScalars {
        target.write(c.escaped(asASCII: true))
      }
      target.write("\"")
    }

    var _value: String
  }

  func debugFormat() -> EscapedFormat {
    return EscapedFormat(self)
  }

  func format() -> String {
    return self
  }
}

//===--- Updated Integer APIs ---------------------------------------------===//
// These will be redundant once the new integer protocols are merged to trunk
// but in the meantime they give us a decent protocol and explicit conversions
// among integers.  You can skip this section if only interested in formatting
//===----------------------------------------------------------------------===//
public typealias Integer = Swift.FixedWidthInteger

extension Integer {
  /// Creates an instance with the same value as `i`
  ///
  /// - Precondition: `i`'s value is representable by `Self`.
  init <N: Integer>(_ i: N) {
    // Implementation based on Egyptian Multiplication of i by 1 allows us to
    // avoid any mixed-type numeric operations, which the current standard
    // library doesn't support.  Note: we also don't have a protocol that
    // provides shift operators, so we use / 2 here.
    var n = i
    self = 0
    if n == 0 { return }
    
    if n < 0 {
      self -= 1 as Self
    }
    else {
      self = 1
    }

    // compute:
    //   abs(self) = the greatest power of 2 that divides i.
    //   n = i / self
    while (n & 1) == 0 {
      self += self
      n = n / 2
    }
    n /= 2 // consume the lowest 1-bit in i
    if n == 0 { return }

    // Accumulate the rest of the 1 bits in i.
    // invariants:
    //   abs(a) is a power of 2
    //   n = i / a
    var a = self + self
    while true {
      if n & 1 != 0 {
        self += a
        if n / 2 == 0 { return }
      }
      n /= 2
      a += a
    }
  }
}
//===--- End Updated Integer APIs -----------------------------------------===//

/// The way all Integer types are formatted.
extension CustomFormatted where Self : Integer {
  func debugFormat() -> IntegerFormat<Self> {
    return format(radix: 10)
  }
  
  func format(
    radix: Int = 10, fill: String = " ", width: Int = 0
  ) -> IntegerFormat<Self> {
    return IntegerFormat(value: self, radix: radix, fill: fill, width: width)
  }
}

/// A textual representation for integers
public struct IntegerFormat<T: Integer> : TextOutputStreamable {
  public init(value: T, radix: Int, fill: String, width: Int) {
    self.value = value
    self.radix = T(radix)
    self.fill = fill
    self.width = width
  }
  
  private var value: T
  private var radix: T, fill: String, width: Int

  private func _writePositive<S: TextOutputStream>(
    _ value: T, _ stream: inout S
  ) -> Int {
    if value == 0 {
      return 0
    }

    let rest: T = value / radix
    let nDigits = _writePositive(rest, &stream)
    let digit = UInt32(value % radix)
    let baseCharOrd : UInt32 = digit <= 9
      ? UnicodeScalar("0").value 
      : UnicodeScalar("A").value - 10
    stream.write(String(UnicodeScalar(baseCharOrd + digit)!))
    return nDigits + 1
  }

  public func write<Target: TextOutputStream>(to target: inout Target) {
    var width = 0
    var result = ""

    if value == 0 {
      result = "0"
      width += 1
    }
    else {
      let absVal = value < 0 ? 0 - value : value
      if (value < 0) {
        target.write("-")
        width += 1
      }
      width += _writePositive(absVal, &result)
    }

    while width < self.width {
      width += 1
      target.write(self.fill)
    }
    target.write(result)
  }
}

extension Int : CustomFormatted {}
extension UInt : CustomFormatted {}

//===--- Adapter that encloses text in vertical bars, for testing ---------===//
/// A textual representation that encloses some base `TextOutputStreamable` in
/// vertical bars, making padding spaces more apparent.
///
/// - Note: This is just for the benefit of the tests
struct Delimited<T: TextOutputStreamable> : TextOutputStreamable {
  var base: T
  
  func write<S: TextOutputStream>(to output: inout S) {
    "|".write(to: &output)
    base.write(to: &output)
    "|".write(to: &output)
  }
}

extension TextOutputStreamable {
  /// A textual representation that encloses the plain textual representation of
  /// `self` in vertical bars, making padding spaces more apparent.
  ///
  /// - Note: This is just for the benefit of the tests
  var delimited : Delimited<Self> {
    return Delimited(base: self)
  }
}
//===--- Tests ------------------------------------------------------------===//

var x = "fubar\n\tbaz"

print(x._description)
// CHECK: fubar
// CHECK-NEXT: 	baz

print(x._debugDescription)
// CHECK-NEXT: "fubar\n\tbaz"

print(424242.format(radix:16, width:8).delimited)
// CHECK-NEXT: |   67932|

var zero = "0"
print((-434343).format(fill:zero, width:8).delimited)
// CHECK-NEXT: |-0434343|

print((-42).format(radix:13, width:8).delimited)
// CHECK-NEXT: |-     33|

print(0x1EADBEEF.format(radix:16).delimited)
// CHECK-NEXT: 1EADBEEF

print((0xDEADBEEF as UInt).format(radix:16).delimited)
// CHECK-NEXT: DEADBEEF
