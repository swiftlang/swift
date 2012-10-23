// RUN: %swift %s -verify -parse-as-library

//===----------------------------------------------------------------------===//
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

import Builtin

// Void is just a type alias for the empty tuple.
typealias Void = ()

//===----------------------------------------------------------------------===//
// Standard Arithmetic Types
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
// Some useful type aliases
//===----------------------------------------------------------------------===//

// Int is just a typealias for Int64.
typealias Int = Int64
typealias UInt = UInt64

// IntegerLiteralType specifies the default type to use for an integer literal
// when the type isn't constrained.
typealias IntegerLiteralType = Int

// FloatLiteralType specifies the default type to use for a floating point
// literal when the type isn't constrained.
typealias FloatLiteralType = Double


// FIXME: Consider adding "int", "double", etc as aliases for Int/Double.  They
// violate the naming convention but lower the barrier to entry.

// CharLiteralType specifies the default type to use for a character literal
// when the type isn't constrained.
typealias CharacterLiteralType = Char

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

//===----------------------------------------------------------------------===//
// UnsafePointer<T> Type
//===----------------------------------------------------------------------===//

struct UnsafePointer<T> {
  var value : Builtin.RawPointer

  func get() -> T {
    return Builtin.load(value)
  }

  func set(newvalue : T) {
    Builtin.assign(newvalue, value)
  }

  func init(newvalue : T) {
    Builtin.init(newvalue, value)
  }

  func move() -> T {
    return Builtin.move(value)
  }

  func destroy() {
    Builtin.destroy(T, value)
  }

  static func alloc(num : Int) -> UnsafePointer<T> {
    typealias Ty = UnsafePointer<T>
    // Don't both with overflow checking.
    var size = Int(Builtin.strideof(T)) * num
    return Ty(Builtin.allocRaw(size.value, Builtin.alignof(T)))
  }

  func dealloc(num : Int) {
    // Overflow checking is actually not required here.
    var size = Int(Builtin.strideof(T)) * num
    Builtin.deallocRaw(value, size.value)
  }
}

func [infix_left=190] + <T>(lhs : UnsafePointer<T>,
                            rhs : Int64) -> UnsafePointer<T> {
  typealias UnsafePtr = UnsafePointer<T>
  return UnsafePtr(Builtin.gep_Int64(lhs.value, (rhs * Int(Builtin.strideof(T))).value))
}

func [infix_left=190] + <T>(lhs : Int64,
                            rhs : UnsafePointer<T>) -> UnsafePointer<T> {
  return rhs + lhs
}

func [infix_left=190] - <T>(lhs : UnsafePointer<T>,
                            rhs : Int64) -> UnsafePointer<T> {
  return lhs + -rhs
}

func [infix_left=190] - <T>(lhs : UnsafePointer<T>,
                            rhs : UnsafePointer<T>) -> Int {
  return Int(Builtin.sub_Int64(Builtin.ptrtoint_Int64(lhs.value),
                               Builtin.ptrtoint_Int64(rhs.value)))
}

func [infix_left=90,assignment] += <T>(lhs : [byref] UnsafePointer<T>, rhs : Int64) {
  lhs = lhs + rhs
}

func [infix_left=90,assignment] -= <T>(lhs : [byref] UnsafePointer<T>, rhs : Int64) {
  lhs = lhs - rhs
}

func [assignment] ++ <T>(a : [byref] UnsafePointer<T>) { a += 1 }
func [assignment] -- <T>(a : [byref] UnsafePointer<T>) { a -= 1 }

func == <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_eq_RawPointer(lhs.value, rhs.value))
}

func != <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ne_RawPointer(lhs.value, rhs.value))
}

func < <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ult_RawPointer(lhs.value, rhs.value))
}

func <= <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ule_RawPointer(lhs.value, rhs.value))
}

func > <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_ugt_RawPointer(lhs.value, rhs.value))
}

func >= <T>(lhs : UnsafePointer<T>, rhs : UnsafePointer<T>) -> Bool {
  return _getBool(Builtin.cmp_uge_RawPointer(lhs.value, rhs.value))
}

typealias UnsafePointerInt = UnsafePointer<Int>
typealias UnsafePointerInt8 = UnsafePointer<Int8>

//===----------------------------------------------------------------------===//
// Slice<T> Type
//===----------------------------------------------------------------------===//

struct Slice<T> : Enumerable {
   var base : UnsafePointer<T>
   var length : Int
   var owner : Builtin.ObjectPointer

   static func convertFromHeapArray(base : Builtin.RawPointer,
                                    owner : Builtin.ObjectPointer,
                                    length : Builtin.Int64) -> Slice<T> {
     typealias UnsafePtr = UnsafePointer<T>
     typealias SliceT = Slice<T>
     return SliceT(UnsafePtr(base), Int(length) & Int64.max(), owner)
   }

   subscript (i : Int) -> T {
     get {
       if i >= length {
         Builtin.trap()
       }

       return (base + i).get()
     }

     set {
       if i >= length {
         Builtin.trap()
       }

       (base + i).set(value)
     }
   }

   // Slicing via subscripting with a range.
   subscript (rng : IntRange) -> Slice<T> {
     get {
       if !(rng.min <= length && rng.max <= length) {
         Builtin.trap()
       }
       typealias SliceT = Slice<T>
       return SliceT(base + rng.min, rng.max - rng.min, owner)
     }

     set {
       if !(value.length == rng.max - rng.min) {
         Builtin.trap()
       }

       // Common case: the elements were updated in place, so we do not have to
       // perform any updates.
       var destStart = base + rng.min
       if value.base == destStart {
         return
       }

       // If the start of the destination slice falls inside the source slice,
       // copy backwards.
       if destStart >= value.base && destStart < value.base + value.length {
         var destEnd = destStart + value.length
         for i in value {
           --destEnd
           destEnd.set(i)
         }

         return
       }

       // Copy the data.
       for i in value {
         destStart.set(i)
         ++destStart
       }
     }
   }

   typealias Elements = Slice<T>
   func getElements() -> Slice<T> { return this }
   
   // FIXME: replPrint doesn't work because T doesn't conform to an
   // appropriate protocol, and we have no way to check it dynamically.
/*
   func replPrint() {
     print('[')
     var first = true
     var total = 0
     for i in this {
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
*/

  func each(f : (T) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : T, f : (T, T) -> T) -> T {
    for i in this { val = f(val, i) }
    return val
  }

  func map(f : (T) -> T) -> T[] {
    var r = new T[length]
    for i in 0 .. length { r[i] = f(this[i]) }
    return r
  }

  func copy() -> T[] {
    var result = new T[length]
    for i in 0..length { result[i] = this[i] }
    return result
  }
}

extension Slice : Range {
  typealias Element = T

  func isEmpty() ->Bool { return length == 0 }
  func getFirstAndAdvance() -> T {
    var prev = base
    base = base + 1
    length = length - 1
    return prev.get()
  }
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

//===----------------------------------------------------------------------===//
// Range protocol and types
//===----------------------------------------------------------------------===//

/// \brief Describes iteration over a sequences of elements that can be 
/// performed once.
protocol Range {
  typealias Element
  func isEmpty() -> Bool
  func getFirstAndAdvance() -> Element
}

protocol Enumerable {
  typealias Elements : Range
  func getElements() -> Elements
}

struct IntRange : Range, Enumerable {
  typealias Element = Int
  var min : Int,
  max : Int

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func isEmpty() -> Bool { return min >= max }
  func contains(x : Int) -> Bool {return min <= x && x < max}
  func contains(x : StringByte) -> Bool {return min <= Int(x) && Int(x) < max}

  func getFirstAndAdvance() -> Int {
    var prev = min
    ++min
    return prev
  }

  typealias Elements = IntRange
  func getElements() -> IntRange { return this }

  func replPrint() {
    print(min)
    print("..")
    print(max)
  }
}

struct ReverseIntRange : Range, Enumerable {
  typealias Element = Int

  var min : Int,
  max : Int

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func isEmpty() -> Bool { return min >= max }
  func contains(x : Int) -> Bool {return min <= x && x < max}
  func getFirstAndAdvance() -> Int { 
    --max
    return max
  }

  typealias Elements = ReverseIntRange
  func getElements() -> ReverseIntRange { return this }

  func replPrint() {
    print("reverse(")
    print(min)
    print("..")
    print(max)
    print(')')
  }
}

func reverse(rng : IntRange) -> ReverseIntRange {
  return ReverseIntRange(rng.min, rng.max)
}

func reverse(rng : ReverseIntRange) -> IntRange {
  return IntRange(rng.min, rng.max)
}

func [infix_left=110] .. (min : Int, max : Int) -> IntRange {
  return IntRange(min, max)
}

struct DoubleRange : Range, Enumerable {
  typealias Element = Double

  var min : Double,
  max : Double,
  stride : Double

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Double) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Double, f : (Double, Double) -> Double) -> Double {
    for i in this { val = f(val, i) }
    return val
  }
  
  // by - To be used as (0.0 .. 10.0).by(.25)
  func by(s : Double) -> DoubleRange {
    var result = this
    result.stride = s
    return result
  }

  func isEmpty() -> Bool { return min >= max }
  func getFirstAndAdvance() -> Double { 
    var prev = min
    min = min + stride
    return prev
  }

  typealias Elements = DoubleRange
  func getElements() -> DoubleRange { return this }

  func replPrint() {
    print(min)
    print("..")
    print(max)

    if stride != 1.0 {
      print(" by ")
      print(stride)
    }
  }

}

func [infix_left=110] .. (min : Double, max : Double) -> DoubleRange {
  return DoubleRange(min, max, 1.0)
}

//===----------------------------------------------------------------------===//
// Misc utilities
//===----------------------------------------------------------------------===//

func swap(a : [byref] Int64, b : [byref] Int64) {
  var c = a
  a = b
  b = c
}

func swap(a : [byref] UInt64, b : [byref] UInt64) {
  var c = a
  a = b
  b = c
}

func assert(fn : [auto_closure] () -> Bool) {
  if !fn() {
    Builtin.trap()
  }
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

func min(x : Int, y : Int, rest : Int...) -> Int {
  var r = x
  if (y < x) {
    r = y
  }
  for z in rest {
    if z < r {
      r = z
    }
  }
  return r;
}

func max(x : Int, y : Int, rest : Int...) -> Int {
  var r = y
  if (y < x) {
    r = x
  }
  for z in rest {
    if z >= r {
      r = z
    }
  }
  return r;
}

//===----------------------------------------------------------------------===//
// Standard protocols
//===----------------------------------------------------------------------===//

typealias Any = protocol<>

protocol Ordered {
 func [infix=170] <(lhs : This, rhs : This) -> Bool
 func [infix=170] <=(lhs : This, rhs : This) -> Bool
 func [infix=170] >=(lhs : This, rhs : This) -> Bool
 func [infix=170] >(lhs : This, rhs : This) -> Bool
}

protocol Equality {
 func [infix=160] ==(lhs : This, rhs : This) -> Bool
 func [infix=160] !=(lhs : This, rhs : This) -> Bool
}

protocol Comparable : Ordered, Equality { }

//===----------------------------------------------------------------------===//
// Objective-C interactions
//===----------------------------------------------------------------------===//

// This violates the naming convention but looks really wrong as Id.
struct id {
  var value : Builtin.ObjCPointer
}

func [asmname="NSLog"]
NSLog(str : NSString, obj : NSString)

func [asmname="swift_NSStringToString"]
convertNSStringToString(object : id, string : [byref] String)

// NSString type for experiments with bridging.  It will eventually be replaced
// with a class type.
struct NSString {
  var value : id

  func [conversion] __conversion() -> String {
    var result : String
    convertNSStringToString(value, &result)
    return result
  }

  // FIXME: Fast-path in compiler/runtime to build a CFConstantString
  static func convertFromStringLiteral(s : String) -> NSString {
    return s
  }

  func replPrint() {
    print(String(this))
  }
}

//===----------------------------------------------------------------------===//
// Extern C functions
//===----------------------------------------------------------------------===//

// FIXME: Once we have an FFI interface, make these have proper function bodies

// The C "exit" and "abort" functions
func [asmname="exit"] exit(exitCode : Int32)
func [asmname="abort"] abort()

func [asmname="putchar"] c_putchar(val : Int32)
func [asmname="print_int"] c_print_int(p : Builtin.RawPointer, buf_len : Int,
                                       x : Int128, Radix : Int) -> UInt64
func [asmname="print_double"] c_print_double(p : Builtin.RawPointer, x : Double)
                                                                       -> UInt64

// Some math stuff.
func [asmname="sqrtf"] sqrt(a : Float) -> Float
func [asmname="sqrt"] sqrt(a : Double) -> Double
func [asmname="sinf"] sin(a : Float) -> Float
func [asmname="sin"] sin(a : Double) -> Double
func [asmname="cosf"] cos(a : Float) -> Float
func [asmname="cos"] cos(a : Double) -> Double
func [asmname="atan2f"] atan2(y : Float, x : Float) -> Float
func [asmname="atan2"] atan2(y : Double, x : Double) -> Double


func [asmname="mach_absolute_time"] mach_absolute_time() -> UInt64

func [asmname="swift_replOutputIsUTF8"] _isUTF8() -> Bool

// Some file stuff

func [asmname="swift_file_open"]
c_file_open(filename : Builtin.RawPointer) -> Int32

func [asmname="swift_file_close"]
c_file_close(fd : Int32) -> Int32

func [asmname="swift_file_read"]
c_file_read(fd : Int32, buf : Builtin.RawPointer, sz : Int) -> Int

func [asmname="swift_file_size"]
c_file_size(filename : Builtin.RawPointer) -> Int

func [asmname="getchar"]
getchar() -> Int32

func [asmname="open"]
posix_open(filename : Builtin.RawPointer, mode : Int32, perm : Int) -> Int32

func [asmname="close"]
posix_close(fd : Int32) -> Int32

func [asmname="lseek"]
posix_seek(fd : Int32, offset : Int, whence : Int32) -> Int

func [asmname="write"]
posix_write(fd : Int32, buf : Builtin.RawPointer, sz : Int) -> Int

func [asmname="read"]
posix_read(fd : Int32, buf : Builtin.RawPointer, sz : Int) -> Int
