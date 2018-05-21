// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name=test -DENCODE -o %t/encode
// RUN: %target-build-swift %s -module-name=test -o %t/decode
// RUN: %target-build-swift %s -module-name=test -Xfrontend -disable-llvm-optzns -emit-ir | %FileCheck -check-prefix=CHECK-IR %s
// RUN: %target-run %t/encode %t/test.arc
// RUN: plutil -p %t/test.arc | %FileCheck -check-prefix=CHECK-ARCHIVE %s
// RUN: %target-run %t/decode %t/test.arc | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: CPU=i386 || CPU=x86_64
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos

import Foundation

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

func main() {

  let args = CommandLine.arguments

#if ENCODE
  let c = TopLevel(27)
  c.nested = ABC.NestedClass(28)
  c.priv = PrivateClass(29)
  c.intc = IntClass(ii: 42)
  c.doublec = DoubleClass(dd: 3.14)

  NSKeyedArchiver.archiveRootObject(c, toFile: args[1])
#else
  if let u = NSKeyedUnarchiver.unarchiveObject(withFile: args[1]) {
    if let x = u as? TopLevel {
      // CHECK: top-level: 27
      print("top-level: \(x.tli)")
      if let n = x.nested {
        // CHECK: nested: 28
        print("nested: \(n.i)")
      }
      if let p = x.priv {
        // CHECK: private: 29
        print("private: \(p.pi)")
      }
      if let g = x.intc {
        // CHECK: int: 42
        print("int: \(g.gi!)")
      }
      if let d = x.doublec {
        // CHECK: double: 3.14
        print("double: \(d.gi!)")
      }
    } else {
      print(u)
    }
  } else {
    print("nil")
  }
#endif
}

main()

// Check that we eagerly create metadata of generic classes, but not for nested classes.

// CHECK-IR-LABEL: define {{.*}} @_swift_eager_class_initialization
// CHECK-IR-NEXT:  entry:
// CHECK-IR-NEXT:    call {{.*}}IntClassCMa
// CHECK-IR-NEXT:    extractvalue
// CHECK-IR-NEXT:    call void asm
// CHECK-IR-NEXT:    call {{.*}}DoubleClassCMa
// CHECK-IR-NEXT:    extractvalue
// CHECK-IR-NEXT:    call void asm
// CHECK-IR-NEXT:    ret

