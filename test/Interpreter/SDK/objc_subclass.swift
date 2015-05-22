// RUN: %target-run-simple-swift foo | FileCheck %s

// REQUIRES: objc_interop

import Foundation

class SuperString : NSString {
  var len = Int()

  override init() { super.init() }

  init(_ len:Int) {
    super.init()
    self.len = len
  }

  required init?(coder aDecoder: NSCoder) {
    super.init(coder: aDecoder)
  }

  override var length: Int {
    return len
  }

  override func characterAtIndex(n: Int) -> unichar {
    return unichar(0x30 + n)
  }

  override func substringWithRange(r: NSRange) -> String {
    if (r.location == 0) {
      return SuperString(r.length) as String
    }
    return super.substringWithRange(r)
  }
}

// CHECK: 0123456789
print(SuperString(10))
// CHECK: 0123456789
print(NSString(string: SuperString(10) as String))
// CHECK: 012
print(SuperString(10).substringWithRange(NSRange(location: 0, length: 3)))
// CHECK: 345
print(SuperString(10).substringWithRange(NSRange(location: 3, length: 3)))

class X {
  var label: String

  init(_ label: String) {
    self.label = label
    print("Initializing \(label)");
  }

  deinit {
    print("Destroying \(label)");
  }
}

@requires_stored_property_inits
class A : NSObject {
  var x1 = X("A.x1")
  var x2 = X("A.x2")

  override init() {
    print("Initializing A instance");
  }

  deinit {
    print("Destroying A instance");
  }
}

class B : A {
  var y1 = X("B.y1")
  var y2 = X("B.y2")

  override init() {
    super.init()
    print("Initializing B instance");
  }

  deinit {
    print("Destroying B instance");
  }
}

func testB() -> B {
  return B()
}

// CHECK: Initializing A.x1
// CHECK: Initializing A.x2
// CHECK: Initializing B.y1
// CHECK: Initializing B.y2
// CHECK: Initializing A instance
// CHECK: Initializing B instance
// CHECK: Destroying B instance
// CHECK: Destroying A instance
// CHECK: Destroying B.y1
// CHECK: Destroying B.y2
// CHECK: Destroying A.x1
// CHECK: Destroying A.x2
testB()

// Propagating nil init out of a superclass initialization.
class MyNSData : NSData {
  init?(base64EncodedString str: String) {
    super.init(base64EncodedString:str, 
               options:[])
    print("MyNSData code should not be executed")
  }

  required init?(coder: NSCoder) {
    super.init(coder: coder)
  }
}

// CHECK-NOT: should not be executed
if let myNSData = MyNSData(base64EncodedString:"\n\n\n") {
  print("NSData came back non-nil?")
} else {
  // CHECK: nil MyNSData as expected
  print("nil MyNSData as expected")
}

// Propagating nil out of delegating initialization.
extension NSData {
  convenience init?(myString str: String) {
    self.init(base64EncodedString:str, 
              options:[])
    print("NSData code should not be executed")
  }
}

// CHECK-NOT: NSData code should not be executed
var nsData : NSData! = NSData(myString:"\n\n\n")
if nsData == nil {
  // CHECK: nil NSData as expected
  print("nil NSData as expected")
}

