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
// OutputStream and Formatting logic
//===----------------------------------------------------------------------===//

extension Bool : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    securityCheck(kind == "v")
    return Format(layout).printToString(String(self))
  }
}

extension Int8 : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    return IntMax(self).format(kind, layout: layout)
  }
}

extension UInt8 : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    return UIntMax(self).format(kind, layout: layout)
  }
}

extension Int16 : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    return IntMax(self).format(kind, layout: layout)
  }
}

extension UInt16 : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    return UIntMax(self).format(kind, layout: layout)
  }
}

extension Int32 : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    return IntMax(self).format(kind, layout: layout)
  }
}

extension UInt32 : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    return UIntMax(self).format(kind, layout: layout)
  }
}

extension Int : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    return IntMax(self).format(kind, layout: layout)
  }
}

extension UInt : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    return UIntMax(self).format(kind, layout: layout)
  }
}

extension IntMax : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    var radix = 10
    var uppercase = false
    if kind == "X" { uppercase = true; radix = 16 }
    else if kind == "x" { radix = 16 }
    else if kind == "o" { radix = 8 }
    else if kind != "v" { fatal("Invalid format chacacter") }
    return Format(layout).printToString(
      String(self, radix : radix, uppercase : uppercase))
  }
}

extension UIntMax : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    var radix = 10
    var uppercase = false
    if kind == "X" { uppercase = true; radix = 16 }
    else if kind == "x" { radix = 16 }
    else if kind == "o" { radix = 8 }
    else if kind != "v" { fatal("Invalid format chacacter") }
    return Format(layout).printToString(
      String(self, radix : radix, uppercase : uppercase))
  }
}

extension Float : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    securityCheck(kind == "v")
    return Format(layout).printToString(String(self))
  }
}

extension Double : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    securityCheck(kind == "v")
    return Format(layout).printToString(String(self))
  }
}

//===----------------------------------------------------------------------===//
// Formatted Printing
//===----------------------------------------------------------------------===//

struct Format : ReplPrintable {
  var width : Int
  var isValid : Bool
  var leftJustify : Bool

  init(_ layout: String) {
    isValid = true
    leftJustify = false
    width = 0

    if !layout.isEmpty() {
      leftJustify = layout.startsWith("-")
      var layoutSansJustification = layout.substr(leftJustify ? 1 : 0)
      if !layoutSansJustification.isEmpty() {
        for c in layoutSansJustification.unicodeScalars {
          if !c.isDigit() {
            isValid = false
            break
          }
          width = width * 10 + Int(UInt32(c) - UInt32("0"))
        }
      }
    }
  }

  func printString(s: String) {

/*  Causes problems due to <rdar://problem/11529601>
    if !isValid {
      print(s)
      return
    }
*/
    var padding = max(width - s.size(), 0)
    if !leftJustify {
      for i in 0...padding { print(" ") }
    }
    print(s)
    if leftJustify {
      for i in 0...padding { print(" ") }
    }
  }

  func printToString(s: String) -> String {
    if !isValid {
      return s
    }
    var r: String
    var padding = max(width - s.size(), 0)
    if !leftJustify {
      r = String(count: padding, character: " ")
    } else {
      r = String()
    }
    r = r + s
    if leftJustify {
      r = r + String(count: padding, character: " ")
    }
    return r
  }

  func replPrint() {
    print("valid = \(isValid), leftJustify = \(leftJustify), width=\(width)")
  }
}

// Terminal input & output

@final class Console : OutputStreamable {
  init() { }

  func write(inout buf: UInt8[]) -> Int {
    let count = buf.count
    var r = 0
    for var start = 0; start < count; start += 1024 {
      let slice = buf[start...min(start + 1024, count)]
      r = slice.withUnsafePointerToElements {
        posix_write(1, $0.value, slice.count)
      }
      if r == -1 {
        break
      }
    }
    return r
  }

  func write(buf: String) -> Int {
    var r = 0
    let p = buf.contiguousUTF8
    
    if p != nil {
      let r = posix_write(1, p.value, buf.core.count)
    }
    else {
      var a = NativeArray<UInt8>(count: 1024, value: 0)
      var count = 0
      
      for u in buf.utf8 {
        a[count++] = u
        if count == a.count {
          r = a.withUnsafePointerToElements {
            posix_write(1, $0.value, count)
          }
          if r == -1 {
            break
          }
        }
      }
      
      if count != 0 {
        r = a.withUnsafePointerToElements {
          posix_write(1, $0.value, count)
        }
      }
    }
    
    securityCheck(r != -1)
    return r
  }

  func write(c: UInt8) {
    var buf = new UInt8[1]
    buf[0] = c
    var r = write(&buf)
    securityCheck(r == 1)
  }
}

var con : Console = Console()

protocol FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String
}

func splitFormat(format: String) -> (String, String, UnicodeScalar, String) {
  var (before, afterPercent, foundPercent) = format.splitFirst("%")
  if !foundPercent {
    return (before, "", UnicodeScalar(0), afterPercent)
  }

  var (layout, kind, after, found) = afterPercent.splitFirstIf({ $0.isAlpha() })
  if !found {
    return (before, "", UnicodeScalar(0), afterPercent)
  }

  return (before, layout, kind, after)
}

protocol OutputStreamable {
  func write(inout buf: UInt8[]) -> Int
  // FIXME:  Add default implementation when language allows it:
  func write(buf: String) -> Int
  // {return write(buf.asUTF8())}
}

// FIXME:  Remove the FormattedPrintable[] overload if/when we can forward
//         variadic arguments.
// BLOCKED: <rdar://problem/12134482> Can't forward variadic arguments
func printf(out: OutputStreamable, format: String, args: FormattedPrintable[]) {
  var index = 0
  var remainingFormat = format
  while !remainingFormat.isEmpty() {
    var (before, layout, kind, after) = splitFormat(remainingFormat)
    out.write(before)
    if kind != UnicodeScalar(0) {
      out.write(args[index++].format(kind, layout: layout))
    }
    remainingFormat = after
  }
}

func printf(out: OutputStreamable, format: String, args: FormattedPrintable...) {
  printf(out, format, args)
}

func printf(format: String, args: FormattedPrintable...) {
  printf(con, format, args)
}

// FIXME: This function shouldn't be necessary!
func replPrint<E : Sequence where E.GeneratorType.Element: ReplPrintable>(s: E) {
  print("[")
  var first = true
  var total = 0
  for i in s {
    if first {
      first = false
    } else {
      print(", ")
    }
    i.replPrint()
    total = total + 1
    if total > 50 {
      print(" ...]")
      return
    }
  }
  print("]")
}

func print<E : Sequence where E.GeneratorType.Element: ReplPrintable>(s: E) {
  replPrint(s)
}

// Some very basic output functions.
@asmname("print_Int64") func print(value: Int64)
@asmname("print_UInt64") func print(value: UInt64)
@asmname("print_Double") func print(value: Double)

func print(value: Int) {
  print(Int64(value))
}

func print(value: UInt) {
  print(UInt64(value))
}

func print(value: Float) {
  print(Double(value))
}

func print(value: String) {
  value._encode(UTF8.self, output: SinkOf<UTF8.CodeUnit> { 
    c_putchar(Int32($0)) 
  })
}

func print(value: Character) {
  print(String(value))
}

func print(value: UnicodeScalar) {
  var wc = UInt32(value)
  if wc < 0x000080 {
    c_putchar(Int32(wc))
    return
  } else if wc < 0x000800 {
    c_putchar(Int32(0xC0 | (wc >> 6)))
    c_putchar(Int32(0x80 | (wc & 0x03F)))
    return
  } else if wc < 0x010000 {
    if !(0x00D800 <= wc && wc < 0x00E000) {
      c_putchar(Int32(0xE0 |  (wc >> 12)))
      c_putchar(Int32(0x80 | ((wc & 0x0FC0) >> 6)))
      c_putchar(Int32(0x80 |  (wc & 0x003F)))
      return
    }
  } else if wc < 0x110000 {
    c_putchar(Int32(0xF0 |  (wc >> 18)))
    c_putchar(Int32(0x80 | ((wc & 0x03F000) >> 12)))
    c_putchar(Int32(0x80 | ((wc & 0x000FC0) >> 6)))
    c_putchar(Int32(0x80 |  (wc & 0x00003F)))
    return
  }
  // print(UnicodeScalar(0xFFFD))
  fatal("Invalid Unicode scalar")
}

func print(value: Bool) {
  if value {
    print("true")
  } else {
    print("false")
  }
}

// Some derived output functions.
func println(value: Bool) {
  print(value)
  print("\n")
}
func println(value: Int) {
  print(Int64(value))
  print("\n")
}
func println(value: UInt) {
  print(UInt64(value))
  print("\n")
}
func println(value: UInt8) {
  print(UInt64(value))
  print("\n")
}
func println(value: UInt16) {
  print(UInt64(value))
  print("\n")
}
func println(value: UInt32) {
  print(UInt64(value))
  print("\n")
}
func println(value: UInt64) {
  print(value)
  print("\n")
}
func println(value: Float) {
  print(value)
  print("\n")
}
func println(value: Double) {
  print(value)
  print("\n")
}
func println(value: String) {
  print(value)
  print("\n")
}

func println(value: Character) {
  print(value)
  print("\n")
}

func println(value: UnicodeScalar) {
  print(value)
  print("\n")
}

func println() {
  print("\n")
}

// FIXME: Proof-of-concept kludge to test the runtime basis for a generic
// print method.

/// The runtime has disgustingly inappropriate knowledge of this protocol. If
/// you change this, you must also change swift_printAny.
protocol Printable {
  func printSelf()
}

extension Int : Printable {
  func printSelf() {
    print(self)
  }
}

extension String : Printable {
  func printSelf() {
    print(self)
  }
}

/// Print any Swift object, using its Printable conformance if it has one,
/// falling back to a default implementation.
@asmname("swift_printAny") func printAny<T>(x: T)

extension Array : Printable {
  func printSelf() {
    print("[")
    if !isEmpty {
      printAny(self[0])
      for x in self[1...count] {
        print(", ")
        printAny(x)
      }
    }
    print("]")
  }
}

