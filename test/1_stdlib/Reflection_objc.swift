// RUN: rm -rf %t  &&  mkdir %t
//
// RUN: %target-clang %S/Inputs/Mirror/Mirror.mm -c -o %t/Mirror.mm.o -g
// RUN: %target-build-swift -parse-stdlib %s -module-name Reflection -I %S/Inputs/Mirror/ -Xlinker %t/Mirror.mm.o -o %t/a.out
// RUN: %S/timeout.sh 360 %target-run %t/a.out %S/Inputs/shuffle.jpg | FileCheck %s
// REQUIRES: executable_test
// FIXME: timeout wrapper is necessary because the ASan test runs for hours

//
// DO NOT add more tests to this file.  Add them to test/1_stdlib/Runtime.swift.
//

// XFAIL: linux

import Swift
import Foundation

#if os(OSX)
import AppKit

typealias OSImage = NSImage
typealias OSColor = NSColor
typealias OSBezierPath = NSBezierPath
#endif

#if os(iOS) || os(tvOS) || os(watchOS)
import UIKit

typealias OSImage = UIImage
typealias OSColor = UIColor
typealias OSBezierPath = UIBezierPath
#endif

// Check ObjC mirror implementation.
// CHECK-LABEL: ObjC:
print("ObjC:")
// CHECK-NEXT:  <NSObject: {{0x[0-9a-f]+}}>
dump(NSObject())

// CHECK-LABEL: ObjC subclass:
print("ObjC subclass:")
// CHECK-NEXT: woozle wuzzle
dump("woozle wuzzle" as NSString)

// Test a mixed Swift-ObjC hierarchy.
class NSGood : NSObject {
  let x: Int = 22
}
class NSBetter : NSGood {
  let y: String = "333"
}

// CHECK-LABEL: Swift ObjC subclass:
// CHECK-NEXT:    Reflection.NSBetter #0
// CHECK-NEXT:      super: Reflection.NSGood
// CHECK-NEXT:        super: <Reflection.NSBetter: {{0x[0-9a-f]+}}>
print("Swift ObjC subclass:")
dump(NSBetter())

// CHECK-LABEL: ObjC quick look objects:
print("ObjC quick look objects:")

// CHECK-LABEL: ObjC enums:
print("ObjC enums:")

// CHECK-NEXT: We cannot reflect C.NSComparisonResult yet
print("We cannot reflect \(NSComparisonResult.OrderedAscending) yet")

// Don't crash when introspecting framework types such as NSURL.
// <rdar://problem/16592777>
// CHECK-LABEL: NSURL:
// CHECK-NEXT:    file:///Volumes/
print("NSURL:")
dump(NSURL(fileURLWithPath: "/Volumes", isDirectory: true))

// -- Check that quick look Cocoa objects get binned correctly to their
//    associated enum tag.

// CHECK-NEXT: got the expected quick look text
switch reflect("woozle wuzzle" as NSString).quickLookObject {
case .Some(.Text("woozle wuzzle")):
  print("got the expected quick look text")
case _:
  print("got something else")
}

// CHECK-NEXT: foobar
let somesubclassofnsstring = ("foo" + "bar") as NSString
switch reflect(somesubclassofnsstring).quickLookObject {
  case .Some(.Text(let text)): print(text)
  default: print("not the expected quicklook")
}

// CHECK-NEXT: got the expected quick look attributed string
let astr = NSAttributedString(string: "yizzle pizzle")
switch reflect(astr as NSAttributedString).quickLookObject {
case .Some(.AttributedString(let astr2 as NSAttributedString))
where astr === astr2:
  print("got the expected quick look attributed string")
case _:
  print("got something else")
}

// CHECK-NEXT: got the expected quick look int
switch reflect(Int.max as NSNumber).quickLookObject {
case .Some(.Int(+Int64(Int.max))):
  print("got the expected quick look int")
case _:
  print("got something else")
}

// CHECK-NEXT: got the expected quick look uint
switch reflect(NSNumber(unsignedLongLong: UInt64.max)).quickLookObject {
case .Some(.UInt(UInt64.max)):
  print("got the expected quick look uint")
case _:
  print("got something else")
}

// CHECK-NEXT: got the expected quick look double
switch reflect(22.5 as NSNumber).quickLookObject {
case .Some(.Double(22.5)):
  print("got the expected quick look double")
case _:
  print("got something else")
}

// CHECK-NEXT: got the expected quick look float
switch reflect(Float32(1.25)).quickLookObject {
case .Some(.Float(1.25)):
  print("got the expected quick look float")
case _:
  print("got something else")
}

// CHECK-NEXT: got the expected quick look image
// CHECK-NEXT: got the expected quick look color
// CHECK-NEXT: got the expected quick look bezier path

let image = OSImage(contentsOfFile:Process.arguments[1])!
switch reflect(image).quickLookObject {
case .Some(.Image(let image2 as OSImage)) where image === image2:
  print("got the expected quick look image")
case _:
  print("got something else")
}

let color = OSColor.blackColor()
switch reflect(color).quickLookObject {
case .Some(.Color(let color2 as OSColor)) where color === color2:
  print("got the expected quick look color")
case _:
  print("got something else")
}

let path = OSBezierPath()
switch reflect(path).quickLookObject {
case .Some(.BezierPath(let path2 as OSBezierPath)) where path === path2:
  print("got the expected quick look bezier path")
case _:
  print("got something else")
}

let intNSArray : NSArray = [1 as NSNumber,2 as NSNumber,3 as NSNumber,4 as NSNumber,5 as NSNumber]
let intNSArrayMirror = reflect(intNSArray)
// CHECK-NEXT: 5 elements
print(intNSArrayMirror.summary)
// CHECK-NEXT: [0]: 1
print("\(intNSArrayMirror[0].0): \(intNSArrayMirror[0].1.summary)")
// CHECK-NEXT: [4]: 5
print("\(intNSArrayMirror[4].0): \(intNSArrayMirror[4].1.summary)")


let numset = NSSet(objects: 1,2,3,4)
let numsetMirror = reflect(numset)
// CHECK-NEXT: 4 elements
print(numsetMirror.summary)
// CHECK-NEXT: I see all four elements
let num0 = (numsetMirror[0].1.summary)
let num1 = (numsetMirror[1].1.summary)
let num2 = (numsetMirror[2].1.summary)
let num3 = (numsetMirror[3].1.summary)
let have1 = (num0 == "1" || num1 == "1" || num2 == "1" || num3 == "1")
let have2 = (num0 == "2" || num1 == "2" || num2 == "2" || num3 == "2")
let have3 = (num0 == "3" || num1 == "3" || num2 == "3" || num3 == "3")
let have4 = (num0 == "4" || num1 == "4" || num2 == "4" || num3 == "4")
if have1 && have2 && have3 && have4 {
  print("I see all four elements")
} else {
  print("I see \(num0), \(num1), \(num2), \(num3)")
}

// CHECK-NEXT: 42
class MyQLTestClass {
  @objc func debugQuickLookObject() -> AnyObject {
    return (42 as NSNumber)
  }
}

switch reflect(MyQLTestClass()).quickLookObject {
  case .Some(.Int(let value)): print(value)
  case .Some(_): print("non-Int object")
  default: print("None")
}

// CHECK-NEXT: nil is good here
class MyNonQLTestClass {
  func debugQuickLookObject() -> AnyObject {
    return (42 as NSNumber)
  }
}

switch reflect(MyNonQLTestClass()).quickLookObject {
  case .Some(.Int(let value)): print(value)
  case .Some(_): print("non-Int object")
  default: print("nil is good here")
}

// CHECK-NEXT: (3.0, 6.0)
print(reflect(CGPoint(x: 3,y: 6)).summary)
// CHECK-NEXT: (30.0, 60.0)
print(reflect(CGSize(width: 30, height: 60)).summary)
// CHECK-NEXT: (50.0, 60.0, 100.0, 150.0)
print(reflect(CGRect(x: 50, y: 60, width: 100, height: 150)).summary)

// rdar://problem/18513769 -- Make sure that QuickLookObject lookup correctly
// manages memory.

@objc class CanaryBase {
  deinit {
    print("\(self.dynamicType) overboard")
  }

  required init() { }
}

var CanaryHandle = false

class IsDebugQLO : CanaryBase, CustomStringConvertible {
  @objc var description: String {
    return "I'm a QLO"
  }
}

class HasDebugQLO : CanaryBase {
  @objc var debugQuickLookObject: AnyObject {
    return IsDebugQLO()
  }
}

class HasNumberQLO : CanaryBase {
  @objc var debugQuickLookObject: AnyObject {
    let number = NSNumber(integer: 97210)
    return number
  }
}

// Hack to build with both older and newer SDKs.
// rdar://problem/19494514
extension UInt {
  static let OBJC_ASSOCIATION_RETAIN_NONATOMIC: UInt = 1
}

class HasAttributedQLO : CanaryBase {
  @objc var debugQuickLookObject: AnyObject {
    let str = NSAttributedString(string: "attributed string")
    objc_setAssociatedObject(str, &CanaryHandle, CanaryBase(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
    return str
  }
}

class HasStringQLO : CanaryBase {
  @objc var debugQuickLookObject: AnyObject {
    let str = NSString(string: "plain string")
    objc_setAssociatedObject(str, &CanaryHandle, CanaryBase(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
    return str
  }
}

func testQLO<T : CanaryBase>(type: T.Type) {
  autoreleasepool {
    _ = reflect(type()).quickLookObject
  }
}

testQLO(IsDebugQLO.self)
// CHECK-NEXT: IsDebugQLO overboard

testQLO(HasDebugQLO.self)
// CHECK-NEXT: HasDebugQLO overboard
// CHECK-NEXT: IsDebugQLO overboard

testQLO(HasNumberQLO.self)
// CHECK-NEXT: HasNumberQLO overboard
// TODO: tagged numbers are immortal, so we can't reliably check for
//   cleanup here

testQLO(HasAttributedQLO.self)
// CHECK-NEXT: HasAttributedQLO overboard
// CHECK-NEXT: CanaryBase overboard

testQLO(HasStringQLO.self)
// CHECK-NEXT: HasStringQLO overboard
// CHECK-NEXT: CanaryBase overboard

// CHECK-LABEL: and now our song is done
print("and now our song is done")

