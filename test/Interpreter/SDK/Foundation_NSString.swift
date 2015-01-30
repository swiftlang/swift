// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

var hello : NSString = "Hello, world!"
// CHECK: Hello, world!
println(hello)

// CHECK: ello,
var helloStr: String = hello as String
println(String(helloStr._core[NSRange(location: 1, length: 5).toRange()!]))

var upperHello = hello.uppercaseString
// CHECK: HELLO, WORLD!
println(upperHello)

// Note: easy way to create an NSDictionary
var strings : NSString = "\"A\" = \"Foo\";\n\"B\" = \"Bar\";\n"
var dict  = strings.propertyListFromStringsFileFormat()!

// Subscripting an NSDictionary.
// CHECK: A -> Foo
println("A -> " + (dict["A"] as! String))
// CHECK: B -> Bar
println("B -> " + (dict["B"] as! String))

// Creating and subscripting an NSMutableArray
var array = NSMutableArray(capacity: 2)
hello = "Hello"
array[0] = hello
array[1] = "world" as NSString

// FIXME: NSString string interpolation doesn't work due to lack of
// overload resolution.
// CHECK: Hello, world!
print(array[0] as! NSString)
print(", ");
print(array[1] as! NSString)
println("!")

// Selectors
assert(NSString.instancesRespondToSelector("init"))
assert(!NSString.instancesRespondToSelector("wobble"))

// Array of strings
var array2 : NSArray = [hello, hello]

// Switch on strings
switch ("world" as NSString).uppercaseString {
case "WORLD":
  print("Found it\n")

default:
  assert(false, "didn't match")
}

// Test string comparisons.
func testComparisons() {
  let ns1: NSString = "foo"
  let ns2: NSString = "bar"

  let nms1: NSMutableString = "foo"
  let nms2: NSMutableString = "bar"

  let s1: String = "foo"
  let s2: String = "bar"

  // Test String [==,!=] String.
  if s1 == s2 { println("s1 == s2") }
  if s1 != s2 { println("s1 != s2") }
  if s1 == s1 { println("s1 == s1") }
  if s1 != s1 { println("s1 != s1") }

  // Test String [==,!=] NSString.
  // This test does not compile, as comparing these 2 types is ambiguous.
  // if s1 == ns2 { println("s1 == ns2") }
  // if s1 != ns2 { println("s1 != ns2") }
  // if s1 == ns1 { println("s1 == ns1") }
  // if s1 != ns1 { println("s1 != ns1") }

  // Test NSString [==,!=] String.
  // This test does not compile, as comparing these 2 types is ambiguous.
  // if ns1 == s2 { println("ns1 == s2") }
  // if ns1 != s2 { println("ns1 != s2") }
  // if ns1 == s1 { println("ns1 == s1") }
  // if ns1 != s1 { println("ns1 != s1") }

  // Test NString [==,!=] NString.
  if ns1 == ns2 { println("ns1 == ns2") }
  if ns1 != ns2 { println("ns1 != ns2") }
  if ns1 == ns1 { println("ns1 == ns1") }
  if ns1 != ns1 { println("ns1 != ns1") }

  // Test NSMutableString [==,!=] String.
  // This test does not compile, as comparing these 2 types is ambiguous.
  // if nms1 == s2 { println("nms1 == s2") }
  // if nms1 != s2 { println("nms1 != s2") }
  // if nms1 == s1 { println("nms1 == s1") }
  // if nms1 != s1 { println("nms1 != s1") }

  // Test NSMutableString [==,!=] NSMutableString.
  if nms1 == nms2 { println("nms1 == nms2") }
  if nms1 != nms2 { println("nms1 != nms2") }
  if nms1 == nms1 { println("nms1 == nms1") }
  if nms1 != nms1 { println("nms1 != nms1") }

  // Test NString [==,!=] NSMutableString.
  if ns1 == nms2 { println("ns1 == nms2") }
  if ns1 != nms2 { println("ns1 != nms2") }
  if ns1 == nms1 { println("ns1 == nms1") }
  if ns1 != nms1 { println("ns1 != nms1") }

  // Test NSMutableString [==,!=] NSString.
  if nms1 == ns2 { println("nms1 == ns2") }
  if nms1 != ns2 { println("nms1 != ns2") }
  if nms1 == ns1 { println("nms1 == ns1") }
  if nms1 != ns1 { println("nms1 != ns1") }
}

// CHECK: s1 != s2
// CHECK: s1 == s1
// CHECK: ns1 != ns2
// CHECK: ns1 == ns1
// CHECK: nms1 != nms2
// CHECK: nms1 == nms1
// CHECK: ns1 != nms2
// CHECK: ns1 == nms1
// CHECK: nms1 != ns2
// CHECK: nms1 == ns1
testComparisons()

// Test overlain variadic methods.
// CHECK-LABEL: Variadic methods:
println("Variadic methods:")
// CHECK-NEXT: x y
println(NSString(format: "%@ %@", "x", "y"))
// CHECK-NEXT: 1{{.*}}024,25
println(NSString(
  format: "%g",
  locale: NSLocale(localeIdentifier: "fr_FR"),
  1024.25
))
// CHECK-NEXT: x y z
println(("x " as NSString).stringByAppendingFormat("%@ %@", "y", "z"))
// CHECK-NEXT: a b c
let s = NSMutableString(string: "a ")
s.appendFormat("%@ %@", "b", "c")
println(s)

let m = NSMutableString.localizedStringWithFormat("<%@ %@>", "q", "r")
// CHECK-NEXT: <q r>
println(m)
m.appendString(" lever")
// CHECK-NEXT: <q r> lever
println(m)
