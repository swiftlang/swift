//===----------------------------------------------------------------------===//
// OutputStream and Formatting logic
//===----------------------------------------------------------------------===//

extension Int8 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return Int64(this).format(kind, layout)
  }
}

extension UInt8 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return UInt64(this).format(kind, layout)
  }
}

extension Int16 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return UInt64(this).format(kind, layout)
  }
}

extension UInt16 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return UInt64(this).format(kind, layout)
  }
}

extension Int32 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return Int64(this).format(kind, layout)
  }
}

extension UInt32 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return UInt64(this).format(kind, layout)
  }
}

extension Int64 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    var radix = 10
    if kind == 'x' { radix = 16 }
    else if kind == 'o' { radix = 8 }
    return Format(layout).printToString(String(this, radix = radix))
  }
}

extension UInt64 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String{
    var radix = 10
    if kind == 'x' { radix = 16 }
    else if kind == 'o' { radix = 8 }
    return Format(layout).printToString(String(this, radix = radix))
  }
}

extension Int128 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    var radix = 10
    if kind == 'x' { radix = 16 }
    else if kind == 'o' { radix = 8 }
    return Format(layout).printToString(String(this, radix = radix))
  }
}

extension UInt128 : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    var radix = 10
    if kind == 'x' { radix = 16 }
    else if kind == 'o' { radix = 8 }
    return Format(layout).printToString(String(this, radix = radix))
  }
}

extension Float : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return Format(layout).printToString(String(this))
  }
}

extension Double : FormattedPrintable {
  func format(kind : Char, layout : String) -> String {
    return Format(layout).printToString(String(this))
  }
}

//===----------------------------------------------------------------------===//
// Formatted Printing
//===----------------------------------------------------------------------===//

struct Format {
  var width : Int
  var isValid : Bool
  var leftJustify : Bool

  constructor(layout : String) {
    isValid = true
    leftJustify = false
    width = 0

    if !layout.isEmpty() {
      if layout[0] == '-' {
        leftJustify = true
        layout = layout.substr(1)
      }

      if !layout.isEmpty() {
        for c in layout {
          if !c.isDigit() { 
            isValid = false
            break
          }
          width = width * 10 + Int(UInt32(c) - UInt32('0'))
        }
      }
    }
  }

  func printString(s : String) {

/*  Causes problems due to <rdar://problem/11529601>
    if !isValid { 
      print(s)
      return
    }
*/
    var padding = max(width - s.size(), 0)
    if !leftJustify {
      for i in 0..padding { print(' ') }
    }
    print(s)
    if leftJustify {
      for i in 0..padding { print(' ') }
    }
  }

  func printToString(s : String) -> String {
    if !isValid { 
      return s
    }
    var r : String
    var padding = max(width - s.size(), 0)
    if !leftJustify {
      r = String(padding, ' ')
    }
    r = r + s
    if leftJustify {
      r = r + String(padding, ' ')
    }
    return r
  }

  func replPrint() {
    print("valid = \(isValid), leftJustify = \(leftJustify), width=\(width)")
  }
}

// Terminal input & output

class Console {
  func write(buf : UInt8[]) -> Int {
    var r = posix_write(1, buf.base.value, buf.length)
    assert(r != -1)
    return r
  }

  func write(buf : String) -> Int {
    var r = posix_write(1, buf.str_value.base.value, buf.length)
    assert(r != -1)
    return r
  }

  func write(c : UInt8) {
    var buf = new UInt8[1]
    buf[0] = c
    var r = write(buf)
    assert(r == 1)
  }
}

class Keyboard {
  func read(buf : UInt8[]) -> Int {
    var r = posix_read(0, buf.base.value, buf.length)
    assert(r >= 0)
    return r
  }

  func read() -> Int {
    var c = new UInt8[1]
    if read(c) != 1 {
      return -1
    }
    return Int(c[0])
  }
}

extension Keyboard {
  func getline() -> String {
    return getline('\n')
  }

  func getline(delim : Char) -> String {
    var r : String
    var i = read()
    while i != -1 {
      var c = Char(i)
      if c == delim {
        break
      }
      r = r + c
      i = read()
    }
    return r
  }
}

var con : Console = new Console
var kbd : Keyboard = new Keyboard

protocol FormattedPrintable {
  func format(kind : Char, layout : String) -> String
}

func splitFormat(format : String) -> (String, String, Char, String) {
  var (before, afterPercent, foundPercent) = format.splitFirst('%')
  if !foundPercent {
    return (before, "", Char(0), afterPercent)
  }

  var (layout, kind, after, found) = afterPercent.splitFirstIf({ $0.isAlpha() })
  if !found {
    return (before, "", Char(0), afterPercent)    
  }

  return (before, layout, kind, after)
}

protocol OutputStreamable {
  func write(buf : UInt8[]) -> Int
  // FIXME:  Add default implementation when language allows it:
  func write(buf : String) -> Int
  // {return write(buf.asUInt8())}
}

// printf to an OutputStreamable
// FIXME:  Remove the FormattedPrintable[] overload if/when we can forward
//         variadic arguments.
func printf(out : OutputStreamable, format : String, args : FormattedPrintable[]) {
  var index = 0
  while !format.isEmpty() {
    var (before, layout, kind, after) = splitFormat(format)
    out.write(before)
    if kind != Char(0) {
      out.write(args[index].format(kind, layout))
      ++index
    }
    format = after
  }
}

func printf(out : OutputStreamable, format : String, args : FormattedPrintable...) {
  printf(out, format, args)
}

func printf(format : String, args : FormattedPrintable...) {
  printf(con, format, args)
}

protocol ReplPrintable {
  func replPrint()
}
// FIXME: This function shouldn't be necessary!
func replPrintSlice<T : ReplPrintable>(s : T[]) {
  print('[')
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
  print(']')
}

// Some very basic output functions.
func print(val : Int)
func print(val : UInt64)
func print(val : Double)
func print(val : String) {
  var len = val.byteLength()
  for var i = 0; i < len; ++i {
    c_putchar(Int32(val.str_value[i]))
  }
}
func print(val : Char) {
  var wc = UInt32(val)
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
  print(Char(0xFFFD))
}

func print(val : Bool) {
  if val {
    print("true")
  } else {
    print("false")
  }
}

// Some derived output functions.
func println(val : Bool) {
  print(val)
  print('\n')
}
func println(val : Int) {
  print(val)
  print('\n')
}
func println(val : UInt64) {
  print(val)
  print('\n')
}
func println(val : Double) {
  print(val)
  print('\n')
}
func println(val : String) {
  print(val)
  print('\n')
}

func println(val : Char) {
  print(val)
  print('\n')
}
