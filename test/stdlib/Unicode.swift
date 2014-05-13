// RUN: %target-run-stdlib-swift | FileCheck %s

import Foundation
import Swift

protocol TestableUnicodeCodec : UnicodeCodec {
  typealias CodeUnit : Integer
  class func encodingId() -> NSStringEncoding
  class func name() -> NSString
}

extension UTF8 : TestableUnicodeCodec {
  static func encodingId() -> NSStringEncoding {
    return NSUTF8StringEncoding
  }
  static func name() -> NSString {
    return "UTF8"
  }
}

extension UTF16 : TestableUnicodeCodec {
  static func encodingId() -> NSStringEncoding {
    return NSUTF16LittleEndianStringEncoding
  }
  static func name() -> NSString {
    return "UTF16"
  }
}

extension UTF32 : TestableUnicodeCodec {
  static func encodingId() -> NSStringEncoding {
    return NSUTF32LittleEndianStringEncoding
  }
  static func name() -> NSString {
    return "UTF32"
  }
}

// Backing store for computed var unicodeScalarRanges
var _unicodeScalarRanges  = Array<Range<UInt32>>()

// The valid ranges of Unicode scalar values
var unicodeScalarRanges : Range<UInt32>[] {
  if _unicodeScalarRanges.count == 0 {
    for r in [UInt32(0)...0xD800, 0xE000...0xFDD0, 0xFDF0...0xFFFE] {
      _unicodeScalarRanges.append(r)
    }
    for base in UInt32(0x1)...0x11 {
      _unicodeScalarRanges.append((base << 16)...((base << 16)+0xFFFE))
    }
  }
  return _unicodeScalarRanges
}

var unicodeScalarCount : Int {
  var count = 0
  for r in unicodeScalarRanges {
    count += Int(r.endIndex - r.startIndex)
  }
  return count
}

func nthUnicodeScalar(n: UInt32) -> UnicodeScalar {
  var count: UInt32 = 0
  for r in unicodeScalarRanges {
    count += r.endIndex - r.startIndex
    if count > n {
      return UnicodeScalar(r.endIndex - (count - n))
    }
  }
  fatal("Index out of range")
}
  
// buffer should have a length >= 4
func nsEncode<CodeUnit>(
  var c: UInt32,
  encoding: NSStringEncoding,
  inout buffer: CodeUnit[],
  inout used: Int
) {
  var s = NSString(
    bytes: &c,
    length: 4,
    encoding: NSUTF32LittleEndianStringEncoding)

  s.getBytes(
      &buffer,
      maxLength: buffer.count,
      usedLength: &used,
      encoding: encoding,
      options: NSStringEncodingConversionOptions(0),
      range: NSRange(location: 0, length: s.length),
    remainingRange: nil)
}

// Convert the given numeric value to a hexidecimal string
func hex<T : Integer>(x: T) -> String {
  return "0x" + _int64ToString(x.toIntMax(), radix: 16)
}

// Convert the given sequence of numeric values to a string
// representing their hexidecimal values
func hex<
  S: Sequence
where
  S.GeneratorType.Element : Integer
>(x: S) -> String {
  var r = "["
  var prefix = ""
  for unit in x {
    r += prefix + hex(unit)
    prefix = ", "
  }
  r += "]"
  return r
}

// A Sink that stores the elements written into an Array that can be
// inspected later.
class ArraySink<T: IntegerLiteralConvertible> : Sink {
  init(capacity: Int) {
    storage = Array(count: capacity, value: 0)
  }
  func put(x: T) {
    storage[count++] = x
  }
  func clear() {
    count = 0
  }
  var elements : Slice<T> {
    return storage[0...count]
  }
  var count = 0
  var storage: T[] = Array()
}

@asmname("random") func random() -> UInt32
@asmname("srandomdev") func srandomdev()

// To avoid swamping the buildbot, by default, test only one out of
// testGroupCount cases, selected at random.  You can manually pass
// "--all" on the command line to test everything.
var testGroupCount = 128
srandomdev()
var testGroup = random() % testGroupCount
var testAll = Process.arguments.count > 0 && Process.arguments[0] == "--all"
var minScalarOrd : Int
var maxScalarOrd : Int

if testAll {
  println("Testing all Unicode scalars")
  minScalarOrd = 0
  maxScalarOrd = unicodeScalarCount
}
else {
  println("Testing Unicode scalar group \(testGroup) of \(testGroupCount)")
  minScalarOrd = unicodeScalarCount * testGroup / testGroupCount
  maxScalarOrd = unicodeScalarCount * (testGroup+1) / testGroupCount
}

class CodecTest<Codec: TestableUnicodeCodec> {
  var used = 0
  typealias CodeUnit = Codec.CodeUnit
  var nsEncodeBuffer: CodeUnit[] = Array(count: 4, value: 0)
  var encodeBuffer = ArraySink<CodeUnit>(capacity: 4)

  func testOne(scalar: UnicodeScalar)
  {
    /* Progress reporter
    if (scalar.value % 0x1000) == 0 {
      println("\(hex(scalar.value))")
    }
    */
    
    // Use Cocoa to encode the scalar
    nsEncode(scalar.value, Codec.encodingId(), &nsEncodeBuffer, &used)
    let nsEncoded = nsEncodeBuffer[0...(used/sizeof(CodeUnit.self))]

    var g = nsEncoded.generate()
    var decoded = Codec.decode(&g)
    if decoded! != scalar {
      println("Decoding failed: \(hex(scalar.value)) => \(hex(nsEncoded)) => \(hex(decoded!.value))")
    }
    encodeBuffer.clear()
    Codec.encode(scalar, output: &self.encodeBuffer)
    
    if !equal(nsEncoded, encodeBuffer.elements)  {
      println("Decoding failed: \(hex(nsEncoded)) => \(hex(scalar.value)) => \(hex(encodeBuffer.storage[0]))")
    }
  }
  
  func run() {
    println("testing \(Codec.name())")
    for i in minScalarOrd...maxScalarOrd {
      testOne(nthUnicodeScalar(UInt32(i)))
    }
    println("done.")
  }
}

srandomdev()

// CHECK: testing UTF8
// CHECK-NEXT: done.
CodecTest<UTF8>().run()

// CHECK: testing UTF16
// CHECK-NEXT: done.
CodecTest<UTF16>().run()

// CHECK: testing UTF32
// CHECK-NEXT: done.
CodecTest<UTF32>().run()

func println(a: UTF8.CodeUnit[]) {
  print("[ ")
  var prefix = ""
  for x in a {
    print("\(prefix)\(x)")
    prefix = ", "
  }
  println(" ]")
}

func println(a: UTF16.CodeUnit[]) {
  print("[ ")
  var prefix = ""
  for x in a {
    print("\(prefix)\(x)")
    prefix = ", "
  }
  println(" ]")
}

func additionalUtf16Tests() {
  // CHECK: additionalUtf16Tests
  println("additionalUtf16Tests")
  // CHECK-NEXT: 1
  println(UTF16.width("x"))
  // CHECK-NEXT: 2
  println(UTF16.width("\U00101010"))

  // CHECK-NEXT: 2
  println(UTF16.width("𝄞"))
  // CHECK-NEXT: true
  println(UTF16.leadSurrogate("𝄞") == 0xD834)
  // CHECK-NEXT: true
  println(UTF16.trailSurrogate("𝄞") == 0xDD1E)

  var u8: UTF8.CodeUnit[] = [ 0, 1, 2, 3, 4, 5 ]
  var u16: UTF16.CodeUnit[] = [ 6, 7, 8, 9, 10, 11 ]

  // CHECK-NEXT: [ 0, 1, 2, 9, 10, 11 ]
  UTF16.copy(u8.elementStorage, destination: u16.elementStorage, count: 3)
  println(u16)
  
  // CHECK-NEXT: [ 9, 10, 11, 3, 4, 5 ]
  UTF16.copy(u16.elementStorage + 3, destination: u8.elementStorage, count: 3)
  println(u8)

  // CHECK-NEXT: [ 0, 1, 2, 0, 1, 2 ]
  UTF16.copy(u16.elementStorage, destination: u16.elementStorage + 3, count: 3)
  println(u16)
  
  // CHECK-NEXT: [ 9, 10, 11, 9, 10, 11 ]
  UTF16.copy(u8.elementStorage, destination: u8.elementStorage + 3, count: 3)
  println(u8)

  let (count0, isASCII0) = UTF16.measure(UTF8.self, input: u8.generate())
  // CHECK-NEXT: 6 / true
  println("\(count0) / \(isASCII0)")
  
  let (count1, isASCII1) = UTF16.measure(UTF16.self, input: u16.generate())
  // CHECK-NEXT: 6 / true
  println("\(count1) / \(isASCII1)")

  // "€" == U+20AC.
  u8 = [0xF0, 0xA4, 0xAD, 0xA2]
  let (count2, isASCII2) = UTF16.measure(UTF8.self, input: u8.generate())
  // CHECK-NEXT: 2 / false
  println("\(count2) / \(isASCII2)")  
}
additionalUtf16Tests()
