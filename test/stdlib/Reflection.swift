// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift %s -module-name Reflection -o %t/a.out
// RUN: %S/timeout.sh 360 %target-run %t/a.out %S/Inputs/shuffle.jpg | FileCheck %s
// FIXME: timeout wrapper is necessary because the ASan test runs for hours

#if os(OSX) || os(iOS)
import Foundation
#endif

#if os(OSX)
import AppKit
#endif
#if os(iOS)
import UIKit
#endif

// A struct type that gets destructured by the default mirror.
struct Matte { 
  let s: String 

  init (_ s: String) {
    self.s = s
  }
}

// CHECK-LABEL: Matte:
println("Matte:")
// Build a String around an interpolation as a way of smoke-testing that
// the internal Mirror implementation gets memory management right.
// CHECK-NEXT:    V10Reflection5Matte (has 1 child)
// CHECK-NEXT:      s: 123
dump(Matte("\(123)"))
// CHECK-NEXT:    V10Reflection5Matte (has 1 child)
// CHECK-NEXT:      s: 456
dump(Matte("456"))

// Structs have no identity and thus no object identifier
// CHECK-NEXT: false
println(reflect(Matte("")).objectIdentifier.getLogicValue())
// The default mirror provides no quick look object
// CHECK-NEXT: false
println(reflect(Matte("")).quickLookObject.getLogicValue())
// CHECK-NEXT: true
println(reflect(Matte("")).disposition == .Struct)

// A more interesting struct type.
struct Complex<T> {
  let real, imag: T
}
// CHECK-LABEL: Complex:
println("Complex:")
// CHECK-NEXT:    V10Reflection7Complex (has 2 children)
// CHECK-NEXT:      real: 1.5
// CHECK-NEXT:      imag: 0.75
dump(Complex<Double>(real: 1.5, imag: 0.75))
// CHECK-NEXT:    V10Reflection7Complex (has 2 children)
// CHECK-NEXT:      real: -1.5
// CHECK-NEXT:      imag: -0.75
dump(Complex<Double>(real: -1.5, imag: -0.75))
// CHECK-NEXT:    V10Reflection7Complex (has 2 children)
// CHECK-NEXT:      real: 22
// CHECK-NEXT:      imag: 44
dump(Complex<Int>(real: 22, imag: 44))
// CHECK-NEXT:    V10Reflection7Complex (has 2 children)
// CHECK-NEXT:      real: is this the real life?
// CHECK-NEXT:      imag: is it just fantasy?
dump(Complex<String>(real: "is this the real life?", 
                     imag: "is it just fantasy?"))

// A type that provides its own mirror.
struct BrilliantMirror : Mirror {
  let _value: Brilliant

  init (_ _value: Brilliant) {
    self._value = _value
  }

  var value: Any {
    return _value
  }

  var valueType: Any.Type {
    return value.dynamicType
  }

  var objectIdentifier: ObjectIdentifier? {
    return ObjectIdentifier(_value)
  }

  var count: Int {
    return 3
  }

  subscript(i: Int) -> (String, Mirror) {
    switch i {
    case 0:
      return ("first", reflect(_value.first))
    case 1:
      return ("second", reflect(_value.second))
    case 2:
      return ("self", self)
    case _:
      _preconditionFailure("child index out of bounds")
    }
  }

  var summary: String {
    return "Brilliant(\(_value.first), \(_value.second))"
  }

  var quickLookObject: QuickLookObject? {
    return nil
  }

  var disposition: MirrorDisposition {
    return .Container
  }
}

class Brilliant : Reflectable {
  let first: Int
  let second: String

  init(_ fst: Int, _ snd: String) {
    self.first = fst
    self.second = snd
  }

  func getMirror() -> Mirror {
    return BrilliantMirror(self)
  }
}

// CHECK-LABEL: Brilliant:
println("Brilliant:")

// CHECK-NEXT:    Brilliant(123, four five six) #0
// CHECK-NEXT:    - first: 123
// CHECK-NEXT:    - second: four five six
// CHECK-NEXT:      self: Brilliant(123, four five six) #0
dump(Brilliant(123, "four five six"))
// CHECK-NEXT:    Brilliant(123, four five six)
dump(Brilliant(123, "four five six"), maxDepth: 0)
// CHECK-NEXT:    Brilliant(123, four five six)
// CHECK-NEXT:    - first: 123
// CHECK-NEXT:    - second: four five six
// CHECK-NEXT:      (1 more child)
dump(Brilliant(123, "four five six"), maxItems: 3)
// CHECK-NEXT:    Brilliant(123, four five six)
// CHECK-NEXT:    - first: 123
// CHECK-NEXT:      (2 more children)
dump(Brilliant(123, "four five six"), maxItems: 2)
// CHECK-NEXT:    Brilliant(123, four five six)
// CHECK-NEXT:      (3 children)
dump(Brilliant(123, "four five six"), maxItems: 1)

// CHECK-NEXT: true
println(reflect(Brilliant(123, "four five six")).disposition == .Container)

// A tuple.
let tuple = (Brilliant(384, "seven six eight"), Matte("nine"))

// CHECK-LABEL: Tuple:
println("Tuple:")
// CHECK-NEXT:   (2 elements)
// CHECK-NEXT:     .0: Brilliant(384, seven six eight) #0
// CHECK-NEXT:        first: 384
// CHECK-NEXT:        second: seven six eight
// CHECK-NEXT:        self: Brilliant(384, seven six eight) #0
// CHECK-NEXT:     .1: V10Reflection5Matte (has 1 child)
// CHECK-NEXT:        s: nine
dump(tuple)
// CHECK-NEXT: false
println(reflect(tuple).quickLookObject.getLogicValue())
// CHECK-NEXT: true
println(reflect(tuple).disposition == .Tuple)

// A tuple of stdlib types with mirrors.
// CHECK-LABEL: Another tuple:
println("Another tuple:")
let tuple2 = (1, 2.5, false, "three")
// CHECK-NEXT:  (4 elements)
// CHECK-NEXT:  - .0: 1
// CHECK-NEXT:  - .1: 2.5
// CHECK-NEXT:  - .2: false
// CHECK-NEXT:  - .3: three
dump(tuple2)

// Check that object identifiers are unique to class instances.
// CHECK-LABEL: Object identity:
println("Object identity:")
let a = Brilliant(1, ""), b = Brilliant(2, "")
// CHECK-NEXT: true
println(ObjectIdentifier(a) == ObjectIdentifier(a))
// CHECK-NEXT: false
println(ObjectIdentifier(a) == ObjectIdentifier(b))

// Check that subclasses inherit their parents' custom mirrors.
class Irradiant: Brilliant {
  init() {
    super.init(400, "")
  }
}

// CHECK-LABEL: Brilliant subclass:
println("Brilliant subclass:")
// CHECK-NEXT:  Brilliant(400, )
dump(Irradiant())

// Check that the primitive Mirror implementation produces appropriately
// unique identifiers for class instances.

// CHECK-LABEL: Object identity:
println("Object identity:")

class DullClass {}
let x = DullClass(), y = DullClass()
// CHECK-NEXT: true
println(reflect(x).objectIdentifier! == reflect(x).objectIdentifier!)
// CHECK-NEXT: false
println(reflect(x).objectIdentifier! == reflect(y).objectIdentifier!)
// CHECK-NEXT: false
println(reflect(x).quickLookObject.getLogicValue())
// CHECK-NEXT: true
println(reflect(x).disposition == .Class)

// Test destructuring of a pure Swift class hierarchy.
class Good {
  let x: Int = 11
  let y: String = "222"
}

class Better: Good {
  let z: Double = 333.5
}

class Best: Better {
  let w: String = "4444"
}

// CHECK-LABEL: Root class:
// CHECK-NEXT:    C10Reflection4Good (has 2 children) #0
// CHECK-NEXT:      x: 11
// CHECK-NEXT:      y: 222
println("Root class:")
dump(Good())

// CHECK-LABEL: Subclass:
// CHECK-NEXT:    C10Reflection4Best (has 2 children) #0
// CHECK-NEXT:      super: C10Reflection6Better (has 2 children)
// CHECK-NEXT:        super: C10Reflection4Good (has 2 children)
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
// CHECK-NEXT:    C10Reflection4Best (has 2 children) #0
// CHECK-NEXT:      super: C10Reflection6Better (has 2 children)
// CHECK-NEXT:        super: C10Reflection4Good (has 2 children)
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: 222
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: 4444
println("Any class:")
any = Best()
dump(any)
// CHECK-LABEL: second verse
// CHECK-NEXT:    C10Reflection4Best (has 2 children) #0
println("second verse same as the first:")
dump(any)

// CHECK-LABEL: Any double:
// CHECK-NEXT:    2.5
println("Any double:")
any = 2.5
dump(any)

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

@class_protocol protocol Barrable {}
extension Best: Barrable {}

// CHECK-LABEL: Barrable class:
// CHECK-NEXT:    C10Reflection4Best (has 2 children) #0
// CHECK-NEXT:      super: C10Reflection6Better (has 2 children)
// CHECK-NEXT:        super: C10Reflection4Good (has 2 children)
// CHECK-NEXT:          x: 11
// CHECK-NEXT:          y: 222
// CHECK-NEXT:        z: 333.5
// CHECK-NEXT:      w: 4444
println("Barrable class:")
var barrable: Barrable = Best()
dump(barrable)
// CHECK-LABEL: second verse
// CHECK-NEXT:    C10Reflection4Best (has 2 children) #0
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

let dict = ["One":1,"Two":2,"Three":3,"Four":4,"Five":5]
// CHECK-NEXT: ▿ 5 key/value pairs
// CHECK-NEXT:   ▿ [0]: (2 elements)
// CHECK-NEXT:     - .0: Two
// CHECK-NEXT:     - .1: 2
// CHECK-NEXT:   ▿ [1]: (2 elements)
// CHECK-NEXT:     - .0: Four
// CHECK-NEXT:     - .1: 4
// CHECK-NEXT:   ▿ [2]: (2 elements)
// CHECK-NEXT:     - .0: Five
// CHECK-NEXT:     - .1: 5
// CHECK-NEXT:   ▿ [3]: (2 elements)
// CHECK-NEXT:     - .0: Three
// CHECK-NEXT:     - .1: 3
// CHECK-NEXT:   ▿ [4]: (2 elements)
// CHECK-NEXT:     - .0: One
// CHECK-NEXT:     - .1: 1
dump(dict)

#if os(OSX) || os(iOS)

// Check ObjC mirror implementation.
// CHECK-LABEL: ObjC:
println("ObjC:")
// CHECK-NEXT:  <NSObject: {{0x[0-9a-f]+}}>
dump(NSObject())

let o = NSObject(), p = NSObject()
// CHECK-NEXT: true
println(reflect(o).objectIdentifier! == reflect(o).objectIdentifier!)
// CHECK-NEXT: false
println(reflect(o).objectIdentifier! == reflect(p).objectIdentifier!)
// CHECK-NEXT: false
println(reflect(o).objectIdentifier! == reflect(y).objectIdentifier!)

// CHECK-LABEL: ObjC subclass:
println("ObjC subclass:")
// CHECK-NEXT: woozle wuzzle
dump("woozle wuzzle" as NSString)

// Test a mixed Swift-ObjC hierarchy.
class NSGood: NSObject {
  let x: Int = 22
}
class NSBetter: NSGood {
  let y: String = "333"
}

// CHECK-LABEL: Swift ObjC subclass:
// CHECK-NEXT:    C10Reflection8NSBetter (has 2 children) #0
// CHECK-NEXT:      super: C10Reflection6NSGood (has 2 children)
// CHECK-NEXT:        super: <_TtC10Reflection8NSBetter: {{0x[0-9a-f]+}}>
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
let somesubclassofnsstring: NSString = "foo" + "bar"
switch reflect(somesubclassofnsstring).quickLookObject {
  case .Some(.Text(let text)): println(text)
  default: println("not the expected quicklook")
}

// CHECK-NEXT: got the expected quick look attributed string
let astr = NSAttributedString(string: "yizzle pizzle")
switch reflect(astr as NSAttributedString).quickLookObject {
case .Some(.AttributedString(let astr2 as NSAttributedString))
where (astr as AnyObject) === astr2:
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

// CHECK-NEXT: got the expected quick look float
switch reflect(22.5 as NSNumber).quickLookObject {
case .Some(.Float(22.5)):
  println("got the expected quick look float")
case _:
  println("got something else")
}

// CHECK-NEXT: got the expected quick look image
// CHECK-NEXT: got the expected quick look color
// CHECK-NEXT: got the expected quick look bezier path

#endif

#if os(OSX)
let image = NSImage(contentsOfFile:Process.arguments[1])
switch reflect(image).quickLookObject {
case .Some(.Image(let image2 as NSImage)) where (image as AnyObject) === image2:
  println("got the expected quick look image")
case _:
  println("got something else")
}

let color = NSColor.blackColor()! // FIXME: unnecessary bang
switch reflect(color).quickLookObject {
case .Some(.Color(let color2 as NSColor)) where (color as AnyObject) === color2:
  println("got the expected quick look color")
case _:
  println("got something else")
}

let path = NSBezierPath()
switch reflect(path).quickLookObject {
case .Some(.BezierPath(let path2 as NSBezierPath)) where (path as AnyObject) === path2:
  println("got the expected quick look bezier path")
case _:
  println("got something else")
}
#endif

#if os(iOS)
let image = UIImage(contentsOfFile:Process.arguments[1])
switch reflect(image).quickLookObject {
case .Some(.Image(let image2 as UIImage)) where image === image2:
  println("got the expected quick look image")
case _:
  println("got something else")
}

let color = UIColor.blackColor()! // FIXME: unnecessary bang
switch reflect(color).quickLookObject {
case .Some(.Color(let color2 as UIColor)) where color === color2:
  println("got the expected quick look color")
case _:
  println("got something else")
}

let path = UIBezierPath()
switch reflect(path).quickLookObject {
case .Some(.BezierPath(let path2 as UIBezierPath)) where path === path2:
  println("got the expected quick look bezier path")
case _:
  println("got something else")
}
#endif

#if os(OSX) || os(iOS)
let intNSArray : NSArray = [1 as NSNumber,2 as NSNumber,3 as NSNumber,4 as NSNumber,5 as NSNumber]
let intNSArrayMirror = reflect(intNSArray)
// CHECK-NEXT: 5 elements
println(intNSArrayMirror.summary)
// CHECK-NEXT: [0]: 1
println("\(intNSArrayMirror[0].0): \(intNSArrayMirror[0].1.summary)")
// CHECK-NEXT: [4]: 5
println("\(intNSArrayMirror[4].0): \(intNSArrayMirror[4].1.summary)")
#endif


#if os(OSX) || os(iOS)
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
#endif

// CHECK-LABEL: and now our song is done
println("and now our song is done")

