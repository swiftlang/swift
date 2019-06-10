// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name=test -DENCODE -o %t/encode
// RUN: %target-run %t/encode %t/test.arc
// RUN: plutil -p %t/test.arc | %FileCheck -check-prefix=CHECK-ARCHIVE %s

// RUN: %target-build-swift %s -module-name=test -o %t/decode
// RUN: %target-run %t/decode %t/test.arc --stdlib-unittest-in-process

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: CPU=i386 || CPU=x86_64

// See also archive_attributes_stable_abi.swift, for the stable ABI
// deployment target test.

import Foundation
import StdlibUnittest

struct ABC {
  // CHECK-ARCHIVE-DAG: "$classname" => "nested_class_coding"
  @objc(nested_class_coding)
  class NestedClass : NSObject, NSCoding {
    var i : Int

    init(_ ii: Int) {
      i = ii
    }

    required init(coder aDecoder: NSCoder) {
      i = aDecoder.decodeInteger(forKey: "i")
    }

    func encode(with aCoder: NSCoder) {
      aCoder.encode(i, forKey: "i")
    }
  }
}

// CHECK-ARCHIVE-DAG: "$classname" => "private_class_coding"
@objc(private_class_coding)
private class PrivateClass : NSObject, NSCoding {
  var pi : Int

  init(_ ii: Int) {
    pi = ii
  }

  required init(coder aDecoder: NSCoder) {
    pi = aDecoder.decodeInteger(forKey: "pi")
  }

  func encode(with aCoder: NSCoder) {
    aCoder.encode(pi, forKey: "pi")
  }
}

class GenericClass<T> : NSObject, NSCoding {
  var gi : T? = nil

  override init() {
  }

  required init(coder aDecoder: NSCoder) {
  }

  func encode(with aCoder: NSCoder) {
  }
}

// CHECK-ARCHIVE-DAG: "$classname" => "test.IntClass"
class IntClass : GenericClass<Int> {

  init(ii: Int) {
    super.init()
    gi = ii
  }

  required init(coder aDecoder: NSCoder) {
    super.init(coder: aDecoder)
    gi = aDecoder.decodeInteger(forKey: "gi")
  }

  override func encode(with aCoder: NSCoder) {
    aCoder.encode(gi!, forKey: "gi")
  }
}

// CHECK-ARCHIVE-DAG: "$classname" => "double_class_coding"
@objc(double_class_coding)
class DoubleClass : GenericClass<Double> {

  init(dd: Double) {
    super.init()
    gi = dd
  }

  required init(coder aDecoder: NSCoder) {
    super.init(coder: aDecoder)
    gi = aDecoder.decodeDouble(forKey: "gi")
  }

  override func encode(with aCoder: NSCoder) {
    aCoder.encode(gi!, forKey: "gi")
  }
}

// CHECK-ARCHIVE-DAG: "$classname" => "top_level_coding"
@objc(top_level_coding)
class TopLevel : NSObject, NSCoding {
  var tli : Int

  var nested: ABC.NestedClass?
  fileprivate var priv: PrivateClass?
  var intc : IntClass?
  var doublec : DoubleClass?

  init(_ ii: Int) {
    tli = ii
  }

  required init(coder aDecoder: NSCoder) {
    tli = aDecoder.decodeInteger(forKey: "tli")
    nested = aDecoder.decodeObject(forKey: "nested") as? ABC.NestedClass
    priv = aDecoder.decodeObject(forKey: "priv") as? PrivateClass
    intc = aDecoder.decodeObject(forKey: "int") as? IntClass
    doublec = aDecoder.decodeObject(forKey: "double") as? DoubleClass
  }

  func encode(with aCoder: NSCoder) {
    aCoder.encode(tli, forKey: "tli")
    aCoder.encode(nested, forKey: "nested")
    aCoder.encode(priv, forKey: "priv")
    aCoder.encode(intc, forKey: "int")
    aCoder.encode(doublec, forKey: "double")
  }
}

#if ENCODE
let c = TopLevel(27)
c.nested = ABC.NestedClass(28)
c.priv = PrivateClass(29)
c.intc = IntClass(ii: 42)
c.doublec = DoubleClass(dd: 3.14)

NSKeyedArchiver.archiveRootObject(c, toFile: CommandLine.arguments[1])
#else
var DecodeTestSuite = TestSuite("Decode")

DecodeTestSuite.test("Decode") {
  func doIt() {
    let u = NSKeyedUnarchiver.unarchiveObject(withFile: CommandLine.arguments[1])!
    let x = u as! TopLevel
    expectEqual(27, x.tli)
    expectEqual(28, x.nested!.i)
    expectEqual(29, x.priv!.pi)
    expectEqual(42, x.intc!.gi!)
    expectEqual(3.14, x.doublec!.gi!)
  }

  if CommandLine.arguments[2] == "NEW" {
    if #available(macOS 10.14.4, iOS 12.2, tvOS 12.2, watchOS 5.2, *) {
      doIt()
    }
    return
  }

  doIt()
}

runAllTests()
#endif
