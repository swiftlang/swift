// RUN: %target-run-simple-swift | FileCheck %s

println(sizeof(String.self) - 3 * sizeof(Int.self))
// CHECK: 0

func testStringToInt() {
  println("test String to Int")
  // CHECK: test String to Int

  var s1 = "  \t 20ddd"
  var i1 : Optional<Int> = s1.toInt()
  if (!i1) {
    println("none")
  }
  // CHECK-NEXT: none

  if (!"".toInt()) {
    println("empty is none")
  }
  // CHECK-NEXT: empty is none

  if ("+20".toInt()! == 20) {
    println("20")
  }
  // CHECK-NEXT: 20

  if ("0".toInt()! == 0) {
    println("0")
  }
  // CHECK-NEXT: 0

  if ("-20".toInt()! == -20) {
    println("-20")
  }
  // CHECK-NEXT: -20

  if (!"-cc20".toInt()) {
    println("none")
  }
  // CHECK-NEXT: none

  if (!"  -20".toInt()) {
    println("none")
  }
  // CHECK-NEXT: none


  if (String(Int.min).toInt()! == Int.min) {
    println("round-trip Int.min")
  }
  // CHECK-NEXT: round-trip Int.min

  if (String(Int.max).toInt()! == Int.max) {
    println("round-trip Int.max")
  }
  // CHECK-NEXT: round-trip Int.max


  // Make a String from an Int, mangle the String's characters, 
  // then print if the new String is or is not still an Int.
  func testConvertabilityOfStringWithModification(
    initialValue: Int, 
    modification: (inout chars: UTF8.CodeUnit[]) -> () ) 
  {
    var chars = String(initialValue).asUTF8()
    modification(chars: &chars)
    var str = String(UTF8.self, input: chars)
    var is_isnot = str.toInt() ? "is" : "is not"
    println("\(str) \(is_isnot) an Int")
  }

  var minChars = String(Int.min).asUTF8()

  testConvertabilityOfStringWithModification(Int.min) { 
    (inout chars: UTF8.CodeUnit[]) in ()
  }
  // CHECK-NEXT: {{-9223372036854775808|-2147483648}} is an Int

  testConvertabilityOfStringWithModification(Int.min) { 
    $0[$0.count-1]--; ()
  }
  // CHECK-NEXT: {{-9223372036854775807|-2147483647}} is an Int

  testConvertabilityOfStringWithModification(Int.min) { 
    $0[$0.count-1]++; ()  // underflow by one
  }
  // CHECK-NEXT: {{-9223372036854775809|-2147483649}} is not an Int

  testConvertabilityOfStringWithModification(Int.min) { 
    $0[2]++; ()  // underflow by lots
  }
  // CHECK-NEXT: {{-9323372036854775808|-2247483648}} is not an Int

  testConvertabilityOfStringWithModification(Int.min) { 
    $0.append("0".asUTF8()[0]); ()  // underflow by adding digits
  }
  // CHECK-NEXT: {{-92233720368547758080|-21474836480}} is not an Int


  testConvertabilityOfStringWithModification(Int.max) { 
    (inout chars: UTF8.CodeUnit[]) in ()
  }
  // CHECK-NEXT: {{9223372036854775807|2147483647}} is an Int

  testConvertabilityOfStringWithModification(Int.max) { 
    $0[$0.count-1]--; ()
  }
  // CHECK-NEXT: {{9223372036854775806|2147483646}} is an Int

  testConvertabilityOfStringWithModification(Int.max) { 
    $0[$0.count-1]++; ()  // overflow by one
  }
  // CHECK-NEXT: {{9223372036854775808|2147483648}} is not an Int

  testConvertabilityOfStringWithModification(Int.max) { 
    $0[1]++; ()  // overflow by lots
  }
  // CHECK-NEXT: {{9323372036854775807|2247483647}} is not an Int

  testConvertabilityOfStringWithModification(Int.max) { 
    $0.append("0".asUTF8()[0]); ()  // overflow by adding digits
  }
  // CHECK-NEXT: {{92233720368547758070|21474836470}} is not an Int


  // Test values lower than min.
  var ui = UInt(Int.max) + 1
  for index in 0...20 {
    ui = ui + UInt(index)
    if ("-\(ui)".toInt()) {
      print(".")
    } else {
      print("*")
    }
  }
  println("lower than min")
  // CHECK-NEXT: .*******************lower than min

  // Test values greater than min.
  ui = UInt(Int.max)
  for index in 0...20 {
    ui = ui - UInt(index)
    if ("-\(ui)".toInt()! == -Int(ui)) {
      print(".")
    } else {
      print("*")
    }
  }
  println("greater than min")
  // CHECK-NEXT: ....................greater than min

  // Test values greater than max.
  ui = UInt(Int.max)
  for index in 0...20 {
    ui = ui + UInt(index)
    if (String(ui).toInt()) {
      print(".")
    } else {
      print("*")
    }
  }
  println("greater than max")
  // CHECK-NEXT: .*******************greater than max

  // Test values lower than max.
  ui = UInt(Int.max)
  for index in 0...20 {
    ui = ui - UInt(index)
    if (String(ui).toInt()! == Int(ui)) {
      print(".")
    } else {
      print("*")
    }
  }
  println("lower than max")
  // CHECK-NEXT: ....................lower than max
}

func testFloats() {
  println("\(1.0)")
  println("\(1.5)")
  println("\(1.0 / (1000000000000))")
  // CHECK: {{^}}1.0{{$}}
  // CHECK-NEXT: {{^}}1.5{{$}}
  // CHECK-NEXT: {{^}}1e-12{{$}}
  println("\(1 / 0.0)")
  println("\(-1 / 0.0)")
  println("\(0 / 0.0)")
  // CHECK-NEXT: {{^}}inf{{$}}
  // CHECK-NEXT: {{^}}-inf{{$}}
  // CHECK-NEXT: {{^}}nan{{$}}
}

// Make sure strings don't grow unreasonably quickly when appended-to
func testGrowth() {
  var s = ""
  var s2 = s

  for i in 0...20 {
    s += "x"
    s2 = s
  }
  // CHECK-NEXT: true
  println(s.core.nativeBuffer!.capacity <= 34)
}

testStringToInt()
testFloats()
testGrowth()

func testCompare() {
  // CHECK: testCompare
  println("testCompare")
  // CHECK: 1
  println("hi".compare("bye"))
  // CHECK: -1
  println("bye".compare("hi"))
  // CHECK: 0
  println("swift".compare("swift"))
  // CHECK: 1
  println("a".compare(""))
  // CHECK: 0
  println("a".compare("a"))
  // CHECK: -1
  println("a".compare("z"))
  // CHECK: 1
  println("aa".compare("a"))
  // CHECK: -1
  println("a".compare("aa"))
  // CHECK: 0
  println("".compare(""))
  // CHECK: -1
  println("a".compare("b"))
  // CHECK: 1
  println("b".compare("a"))
  println("testCompare done")
  // CHECK: testCompare done
}
testCompare()

func testCompareUnicode() {
  // CHECK: testCompareUnicode
  println("testCompareUnicode")
  // CHECK: 1
  println("hi".compare("bye"))
  // CHECK: -1
  println("bye".compare("hi"))
  // CHECK: 0
  println("ראשון".compare("ראשון"))
  // CHECK: 1
  println("א".compare(""))
  // CHECK: 0
  println("א".compare("א"))
  // CHECK: -1
  println("א".compare("ת"))
  // CHECK: 1
  println("אא".compare("א"))
  // CHECK: -1
  println("א".compare("אא"))
  // CHECK: 0
  println("".compare(""))
  // CHECK: -1
  println("א".compare("ב"))
  // CHECK: 1
  println("ב".compare("א"))
  println("testCompareUnicode done")
  // CHECK: testCompareUnicode done
}
testCompareUnicode()

