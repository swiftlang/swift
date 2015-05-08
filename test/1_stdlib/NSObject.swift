// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation


//===----------------------------------------------------------------------===//
// NSObject ==
//===----------------------------------------------------------------------===//

func printEquality<T : Equatable>(lhs: T, _ rhs: T, _ lhsName: String, _ rhsName: String) {
  if lhs == lhs {
    print("\(lhsName) == \(lhsName)")
  }
  if lhs != lhs {
    print("\(lhsName) != \(lhsName)")
  }
  if lhs == rhs {
    print("\(lhsName) == \(rhsName)")
  }
  if lhs != rhs {
    print("\(lhsName) != \(rhsName)")
  }
}

func printIdentity(lhs: AnyObject, _ rhs: AnyObject, _ lhsName: String, _ rhsName: String) {
  if lhs === lhs {
    print("\(lhsName) === \(lhsName)")
  }
  if lhs !== lhs {
    print("\(lhsName) !== \(lhsName)")
  }
  if lhs === rhs {
    print("\(lhsName) === \(rhsName)")
  }
  if lhs !== rhs {
    print("\(lhsName) !== \(rhsName)")
  }
}


print("NoisyEqual ==")
class NoisyEqual : NSObject {
  override func isEqual(rhs: AnyObject?) -> Bool {
    print("wow much equal")
    return super.isEqual(rhs)
  }
}

let n1 = NoisyEqual.init()
let n2 = NoisyEqual.init()
printIdentity(n1, n2, "n1", "n2")
printEquality(n1, n2, "n1", "n2")
print("done NoisyEqual ==")
// CHECK: NoisyEqual ==
// CHECK-NEXT: n1 === n1
// CHECK-NEXT: n1 !== n2
// CHECK-NEXT: wow much equal
// CHECK-NEXT: n1 == n1
// CHECK-NEXT: wow much equal
// CHECK-NEXT: wow much equal
// CHECK-NEXT: wow much equal
// CHECK-NEXT: n1 != n2
// CHECK-NEXT: done NoisyEqual ==


print("NSObject ==")
let o1 = NSObject.init()
let o2 = NSObject.init()
printIdentity(o1, o2, "o1", "o2")
printEquality(o1, o2, "o1", "o2")
printIdentity(o1, 10, "o1", "10")
printEquality(o1, 10, "o1", "10")
printIdentity(10, o1, "10", "o1")
printEquality(10, o1, "10", "o1")
print("done NSObject ==")
// CHECK: NSObject ==
// CHECK-NEXT: o1 === o1
// CHECK-NEXT: o1 !== o2
// CHECK-NEXT: o1 == o1
// CHECK-NEXT: o1 != o2
// CHECK-NEXT: o1 === o1
// CHECK-NEXT: o1 !== 10
// CHECK-NEXT: o1 == o1
// CHECK-NEXT: o1 != 10
// CHECK-NEXT: 10 === 10
// CHECK-NEXT: 10 !== o1
// CHECK-NEXT: 10 == 10
// CHECK-NEXT: 10 != o1
// CHECK: done NSObject ==


print("NSMutableString ==")
let s1 = NSMutableString.init(string:"hazcam")
let s2 = NSMutableString.init(string:"hazcam")
printIdentity(s1, s2, "s1", "s2")
printEquality(s1, s2, "s1", "s2")
print("mutate")
s2.appendString("navcam")
printIdentity(s1, s2, "s1", "s2")
printEquality(s1, s2, "s1", "s2")
print("done NSMutableString ==")
// CHECK: NSMutableString ==
// CHECK-NEXT: s1 === s1
// CHECK-NEXT: s1 !== s2
// CHECK-NEXT: s1 == s1
// CHECK-NEXT: s1 == s2
// CHECK-NEXT: mutate
// CHECK-NEXT: s1 === s1
// CHECK-NEXT: s1 !== s2
// CHECK-NEXT: s1 == s1
// CHECK-NEXT: s1 != s2
// CHECK-NEXT: done NSMutableString ==


//===----------------------------------------------------------------------===//
// NSObject hashValue
//===----------------------------------------------------------------------===//

func printHashValue<T : Hashable>(x: T, _ name: String) {
  print("\(name) hashes to \(x.hashValue)")
}


print("NSMutableString hashValue")
print("\(s1.hashValue)")
print("\(s1.hash)")
s1.appendString("pancam")
print("\(s1.hashValue)")
print("\(s1.hash)")
print("done NSMutableString hashValue")
// CHECK: NSMutableString hashValue
// CHECK-NEXT: [[H1:[0-9]+]]
// CHECK-NEXT: [[H1]]
// CHECK-NEXT: [[H2:[0-9]+]]
// CHECK-NEXT: [[H2]]
// CHECK-NEXT: done NSMutableString hashValue


class NoisyHash : NSObject {
  override var hash : Int {
    print("so hash")
    return super.hash
  }
}

print("NoisyHash hashValue")
let nh = NoisyHash.init()
printHashValue(nh, "nh")
print("done NoisyHash hashValue")
// CHECK: NoisyHash hashValue
// CHECK-NEXT: so hash
// CHECK-NEXT: nh hashes to {{[0-9]+}}
// CHECK: done NoisyHash hashValue


class ValueLike : NSObject {
  var x: Int

  init(int: Int) {
    x = int
    super.init()
  }

  override func isEqual(rhs: AnyObject?) -> Bool {
    if let rhs2 = rhs as? ValueLike {
      return x == rhs2.x
    }
    return false
  }

  override var hash : Int {
    return x
  }
}

print("ValueLike hashValue")
let sh1 = ValueLike.init(int:10)
let sh2 = ValueLike.init(int:20)
let sh3 = ValueLike.init(int:10)
printIdentity(sh1, sh2, "sh1", "sh2")
printIdentity(sh1, sh3, "sh1", "sh3")
printIdentity(sh2, sh3, "sh2", "sh3")
printEquality(sh1, sh2, "sh1", "sh2")
printEquality(sh1, sh3, "sh1", "sh3")
printEquality(sh2, sh3, "sh2", "sh3")
printEquality(sh1.hashValue, sh2.hashValue, "sh1 hash", "sh2 hash")
printEquality(sh1.hashValue, sh3.hashValue, "sh1 hash", "sh3 hash")
printEquality(sh2.hashValue, sh3.hashValue, "sh2 hash", "sh3 hash")
var dict = Dictionary<ValueLike, Int>()
dict[sh1] = sh1.x
dict[sh2] = sh2.x
print("sh1 \(dict[sh1]!)")
print("sh2 \(dict[sh2]!)")
print("sh3 \(dict[sh3]!)")
print("done ValueLike hashValue")
// CHECK: ValueLike hashValue
// CHECK-NEXT: sh1 === sh1
// CHECK-NEXT: sh1 !== sh2
// CHECK-NEXT: sh1 === sh1
// CHECK-NEXT: sh1 !== sh3
// CHECK-NEXT: sh2 === sh2
// CHECK-NEXT: sh2 !== sh3
// CHECK-NEXT: sh1 == sh1
// CHECK-NEXT: sh1 != sh2
// CHECK-NEXT: sh1 == sh1
// CHECK-NEXT: sh1 == sh3
// CHECK-NEXT: sh2 == sh2
// CHECK-NEXT: sh2 != sh3
// CHECK-NEXT: sh1 hash == sh1 hash
// CHECK-NEXT: sh1 hash != sh2 hash
// CHECK-NEXT: sh1 hash == sh1 hash
// CHECK-NEXT: sh1 hash == sh3 hash
// CHECK-NEXT: sh2 hash == sh2 hash
// CHECK-NEXT: sh2 hash != sh3 hash
// CHECK-NEXT: sh1 10
// CHECK-NEXT: sh2 20
// CHECK-NEXT: sh3 10
// CHECK-NEXT: done ValueLike hashValue

// Native Swift objects should not have nontrivial structors from ObjC's point
// of view.
class NativeSwift {}
class GenericNativeSwift<T> {}

var native: AnyObject = NativeSwift()

if native.respondsToSelector(".cxx_construct") {
  print("SwiftObject has nontrivial constructor")
} else {
  print("no nontrivial constructor") // CHECK-NEXT: no nontrivial constructor
}
if native.respondsToSelector(".cxx_destruct") {
  print("SwiftObject has nontrivial destructor")
} else {
  print("no nontrivial destructor") // CHECK-NEXT: no nontrivial destructor
}

native = GenericNativeSwift<Int>()

if native.respondsToSelector(".cxx_construct") {
  print("SwiftObject has nontrivial constructor")
} else {
  print("no nontrivial constructor") // CHECK-NEXT: no nontrivial constructor
}
if native.respondsToSelector(".cxx_destruct") {
  print("SwiftObject has nontrivial destructor")
} else {
  print("no nontrivial destructor") // CHECK-NEXT: no nontrivial destructor
}

class D : NSObject {}
print(D.self) // CHECK-NEXT: D
print(_getSuperclass(D.self) == NSObject.self) // CHECK-NEXT: true
print(_getSuperclass(_getSuperclass(D.self)!) == nil) // CHECK-NEXT: true
class E : NSString {}
print(  // CHECK-NEXT: true
  _getSuperclass(E.self) == NSString.self)
print( // CHECK-NEXT: true
  _getSuperclass(_getSuperclass(E.self)!) == NSObject.self)
print( // CHECK-NEXT: true
  _getSuperclass(_getSuperclass(_getSuperclass(E.self)!)!) == nil)
