// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift -parse-stdlib %s -module-name Reflection -o %t/a.out
// RUN: %S/timeout.sh 360 %target-run %t/a.out %S/Inputs/shuffle.jpg | FileCheck %s
// FIXME: timeout wrapper is necessary because the ASan test runs for hours

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import Swift
import Foundation
import SpriteKit

#if os(OSX)
import AppKit

typealias OSImage = NSImage
typealias OSColor = NSColor
typealias OSBezierPath = NSBezierPath
#endif

#if os(iOS) || os(tvOS)
import UIKit

typealias OSImage = UIImage
typealias OSColor = UIColor
typealias OSBezierPath = UIBezierPath
#endif

// A more interesting struct type.
struct Complex<T> {
  let real, imag: T
}
// CHECK-LABEL: Complex:
println("Complex:")
// CHECK-NEXT:    Reflection.Complex<Swift.Double>
// CHECK-NEXT:      real: 1.5
// CHECK-NEXT:      imag: 0.75
dump(Complex<Double>(real: 1.5, imag: 0.75))
// CHECK-NEXT:    Reflection.Complex<Swift.Double>
// CHECK-NEXT:      real: -1.5
// CHECK-NEXT:      imag: -0.75
dump(Complex<Double>(real: -1.5, imag: -0.75))
// CHECK-NEXT:    Reflection.Complex<Swift.Int>
// CHECK-NEXT:      real: 22
// CHECK-NEXT:      imag: 44
dump(Complex<Int>(real: 22, imag: 44))
// CHECK-NEXT:    Reflection.Complex<Swift.String>
// CHECK-NEXT:      real: is this the real life?
// CHECK-NEXT:      imag: is it just fantasy?
dump(Complex<String>(real: "is this the real life?", 
                     imag: "is it just fantasy?"))


// Test destructuring of a pure Swift class hierarchy.
class Good {
  let x: UInt = 11
  let y: String = "222"
}

class Better : Good {
  let z: Double = 333.5
}

class Best : Better {
  let w: String = "4444"
}

// CHECK-LABEL: Root class:
// CHECK-NEXT:    Reflection.Good #0
// CHECK-NEXT:      x: 11
// CHECK-NEXT:      y: 222
println("Root class:")
dump(Good())

// CHECK-LABEL: Subclass:
// CHECK-NEXT:    Reflection.Best #0
// CHECK-NEXT:      super: Reflection.Better
// CHECK-NEXT:        super: Reflection.Good
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: 222
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: 4444
println("Subclass:")
dump(Best())

// Test protocol types, which reflect as their dynamic types.
// CHECK-LABEL: Any int:
// CHECK-NEXT:    1
println("Any int:")
var any: Any = 1
dump(any)

// CHECK-LABEL: Any class:
// CHECK-NEXT:    Reflection.Best #0
// CHECK-NEXT:      super: Reflection.Better
// CHECK-NEXT:        super: Reflection.Good
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: 222
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: 4444
println("Any class:")
any = Best()
dump(any)
// CHECK-LABEL: second verse
// CHECK-NEXT:    Reflection.Best #0
println("second verse same as the first:")
dump(any)

// CHECK-LABEL: Any double:
// CHECK-NEXT:    2.5
println("Any double:")
any = 2.5
dump(any)

// CHECK-LABEL: Character:
// CHECK-NEXT: a
println("Character:")
println(reflect(Character("a")).summary)

let range = 3...9
// CHECK-NEXT: 3..<10
println(reflect(range).summary)
// CHECK-NEXT: startIndex=3
println("startIndex=\(reflect(range)[0].1.summary)")

protocol Fooable {}
extension Int : Fooable {}
extension Double : Fooable {}

// CHECK-LABEL: Fooable int:
// CHECK-NEXT:    1
println("Fooable int:")
var fooable: Fooable = 1
dump(fooable)

// CHECK-LABEL: Fooable double:
// CHECK-NEXT:    2.5
println("Fooable double:")
fooable = 2.5
dump(fooable)

protocol Barrable : class {}
extension Best: Barrable {}

// CHECK-LABEL: Barrable class:
// CHECK-NEXT:    Reflection.Best #0
// CHECK-NEXT:      super: Reflection.Better
// CHECK-NEXT:        super: Reflection.Good
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: 222
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: 4444
println("Barrable class:")
var barrable: Barrable = Best()
dump(barrable)
// CHECK-LABEL: second verse
// CHECK-NEXT:    Reflection.Best #0
println("second verse same as the first:")
dump(barrable)

// With Reflectable protocols we extract the witness table from the container.
// CHECK-LABEL: Reflectable int:
// CHECK-NEXT:   1
println("Reflectable int:")
var reflectable: Reflectable = 1
dump(reflectable)

// CHECK-NEXT: Logical: true
switch reflect(true).quickLookObject {
  case .None: println("no quicklook")
  case .Some(let ql):
    switch ql {
      case .Logical(let x): println("Logical: \(x)")
      default: println("wrong quicklook type")
    }
}

// CHECK-NEXT: Hello world
println( reflect(Optional<String>("Hello world")).summary )
// CHECK-NEXT: nil
println( reflect(Optional<String>()).summary  )

let intArray = [1,2,3,4,5]
let intArrayMirror = reflect(intArray)
// CHECK-NEXT: 5 elements
println(intArrayMirror.summary)
// CHECK-NEXT: [0]: 1
println("\(intArrayMirror[0].0): \(intArrayMirror[0].1.summary)")
// CHECK-NEXT: [4]: 5
println("\(intArrayMirror[4].0): \(intArrayMirror[4].1.summary)")

enum JustSomeEnum {case A,B}
// CHECK-NEXT: (Enum Value)
println(reflect(JustSomeEnum.A).summary)

var justSomeFunction = { (x:Int)->Int in return x + 1 }
// CHECK-NEXT: (Function)
println(reflect(justSomeFunction).summary)

// CHECK-NEXT: Swift.String
println(reflect(String.self).summary)

// CHECK-NEXT: CollectionOfOne(Howdy Swift!)
// CHECK-NEXT:  - element: Howdy Swift!
dump(CollectionOfOne("Howdy Swift!"))

// CHECK-NEXT: EmptyCollection
var emptyCollectionOfInt: EmptyCollection<Int> = EmptyCollection()
println(reflect(emptyCollectionOfInt).summary)

// CHECK-NEXT: .One
println(reflect(Bit.One).summary)

// CHECK-NEXT: ▿
// CHECK-NEXT: from: 1.0
// CHECK-NEXT: through: 12.15
// CHECK-NEXT: by: 3.14
dump(stride(from: 1.0, through: 12.15, by: 3.14))

// Check ObjC mirror implementation.
// CHECK-LABEL: ObjC:
println("ObjC:")
// CHECK-NEXT:  <NSObject: {{0x[0-9a-f]+}}>
dump(NSObject())

// CHECK-NEXT: UnsafeMutablePointer(nil)
var nilUnsafeMutablePointerString: UnsafeMutablePointer<String> = nil
println(reflect(nilUnsafeMutablePointerString).summary)

// CHECK-NEXT: UnsafeMutablePointer(0x123456)
var randomUnsafeMutablePointerString = UnsafeMutablePointer<String>(
  bitPattern: 0x123456)
println(reflect(randomUnsafeMutablePointerString).summary)

// CHECK-NEXT: Hello panda
var sanePointerString = UnsafeMutablePointer<String>.alloc(1)
sanePointerString.initialize("Hello panda")
println(reflect(sanePointerString.memory).summary)
sanePointerString.destroy()
sanePointerString.dealloc(1)

// CHECK-LABEL: ObjC subclass:
println("ObjC subclass:")
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
println("Swift ObjC subclass:")
dump(NSBetter())

// CHECK-LABEL: ObjC quick look objects:
println("ObjC quick look objects:")

// Don't crash when introspecting framework types such as NSURL.
// <rdar://problem/16592777>
// CHECK-LABEL: NSURL:
// CHECK-NEXT:    file:///Volumes/
println("NSURL:")
dump(NSURL(fileURLWithPath: "/Volumes", isDirectory: true))

// -- Check that quick look Cocoa objects get binned correctly to their
//    associated enum tag.

// CHECK-NEXT: got the expected quick look text
switch reflect("woozle wuzzle" as NSString).quickLookObject {
case .Some(.Text("woozle wuzzle")):
  println("got the expected quick look text")
case _:
  println("got something else")
}

// CHECK-NEXT: foobar
let somesubclassofnsstring = ("foo" + "bar") as NSString
switch reflect(somesubclassofnsstring).quickLookObject {
  case .Some(.Text(let text)): println(text)
  default: println("not the expected quicklook")
}

// CHECK-NEXT: got the expected quick look attributed string
let astr = NSAttributedString(string: "yizzle pizzle")
switch reflect(astr as NSAttributedString).quickLookObject {
case .Some(.AttributedString(let astr2 as NSAttributedString))
where astr === astr2:
  println("got the expected quick look attributed string")
case _:
  println("got something else")
}

// CHECK-NEXT: got the expected quick look int
switch reflect(Int.max as NSNumber).quickLookObject {
case .Some(.Int(+Int64(Int.max))):
  println("got the expected quick look int")
case _:
  println("got something else")
}

// CHECK-NEXT: got the expected quick look uint
switch reflect(NSNumber(unsignedLongLong: UInt64.max)).quickLookObject {
case .Some(.UInt(UInt64.max)):
  println("got the expected quick look uint")
case _:
  println("got something else")
}

// CHECK-NEXT: got the expected quick look double
switch reflect(22.5 as NSNumber).quickLookObject {
case .Some(.Double(22.5)):
  println("got the expected quick look double")
case _:
  println("got something else")
}

// CHECK-NEXT: got the expected quick look float
switch reflect(Float32(1.25)).quickLookObject {
case .Some(.Float(1.25)):
  println("got the expected quick look float")
case _:
  println("got something else")
}

// CHECK-NEXT: got the expected quick look image
// CHECK-NEXT: got the expected quick look color
// CHECK-NEXT: got the expected quick look bezier path

let image = OSImage(contentsOfFile:Process.arguments[1])!
switch reflect(image).quickLookObject {
case .Some(.Image(let image2 as OSImage)) where image === image2:
  println("got the expected quick look image")
case _:
  println("got something else")
}

let color = OSColor.blackColor()
switch reflect(color).quickLookObject {
case .Some(.Color(let color2 as OSColor)) where color === color2:
  println("got the expected quick look color")
case _:
  println("got something else")
}

let path = OSBezierPath()
switch reflect(path).quickLookObject {
case .Some(.BezierPath(let path2 as OSBezierPath)) where path === path2:
  println("got the expected quick look bezier path")
case _:
  println("got something else")
}

let intNSArray : NSArray = [1 as NSNumber,2 as NSNumber,3 as NSNumber,4 as NSNumber,5 as NSNumber]
let intNSArrayMirror = reflect(intNSArray)
// CHECK-NEXT: 5 elements
println(intNSArrayMirror.summary)
// CHECK-NEXT: [0]: 1
println("\(intNSArrayMirror[0].0): \(intNSArrayMirror[0].1.summary)")
// CHECK-NEXT: [4]: 5
println("\(intNSArrayMirror[4].0): \(intNSArrayMirror[4].1.summary)")


let numset = NSSet(objects: 1,2,3,4)
let numsetMirror = reflect(numset)
// CHECK-NEXT: 4 elements
println(numsetMirror.summary)
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
  println("I see all four elements")
} else {
  println("I see \(num0), \(num1), \(num2), \(num3)")
}

// CHECK-NEXT: 42
class MyQLTestClass {
  @objc func debugQuickLookObject() -> AnyObject {
    return (42 as NSNumber)
  }
}

switch reflect(MyQLTestClass()).quickLookObject {
  case .Some(.Int(let value)): println(value)
  case .Some(_): println("non-Int object")
  default: println("None")
}

// CHECK-NEXT nil is good here
class MyNonQLTestClass {
  func debugQuickLookObject() -> AnyObject {
    return (42 as NSNumber)
  }
}

switch reflect(MyQLTestClass()).quickLookObject {
  case .Some(.Int(let value)): println(value)
  case .Some(_): println("non-Int object")
  default: println("nil is good here")
}

// <rdar://problem/17027510>
struct Pear<T, U> { let fst: T; let snd: U }

class SubScene : SKScene {
  let foo = 12_131_415
  let bar = "boom"
  let bas: Pear<Int, [Any?]> = Pear(fst: 219, snd: ["boom", 123, 456.0])
  let zim = 20721
}

// CHECK-LABEL: SKScene subclass:
// CHECK-NEXT: {{.*}}SubScene
// CHECK-NEXT:   super: <SKScene>
// CHECK-NEXT:   foo: 12131415
// CHECK-NEXT:   bar: boom
// CHECK-NEXT:   bas: {{.*}}Pear
// CHECK-NEXT:     fst: 219
// CHECK-NEXT:     snd: 3 elements
// CHECK-NEXT:       [0]: boom
// CHECK-NEXT:         Some: boom
// CHECK-NEXT:       [1]: 123
// CHECK-NEXT:         Some: 123
// CHECK-NEXT:       [2]: 456.0
// CHECK-NEXT:         Some: 456.0
// CHECK-NEXT:   zim: 20721
println("SKScene subclass:")
dump(SubScene())

// CHECK-NEXT: (3.0, 6.0)
println(reflect(CGPoint(x: 3,y: 6)).summary)
// CHECK-NEXT: (30.0, 60.0)
println(reflect(CGSize(width: 30, height: 60)).summary)
// CHECK-NEXT: (50.0, 60.0, 100.0, 150.0)
println(reflect(CGRect(x: 50, y: 60, width: 100, height: 150)).summary)

// rdar://problem/18513769 -- Make sure that QuickLookObject lookup correctly
// manages memory.

@objc class CanaryBase {
  deinit {
    println("\(self.dynamicType) overboard")
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

// Don't crash on types with opaque metadata. rdar://problem/19791252
var rawPointer = unsafeBitCast(0 as Int, Builtin.RawPointer.self)
dump(rawPointer)
// CHECK: - (Opaque Value)

// CHECK-LABEL: and now our song is done
println("and now our song is done")

