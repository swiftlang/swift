// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name=test -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %S/Inputs/test.arc | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos

// This test checks if an archive, produced with the swift 3.1 compiler, can
// still be read with the current compiler.

import Foundation

struct ABC {
  @objc(_TtCV4test3ABC11NestedClass)
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

@objc(_TtC4testP33_25D299289FEB01A726765440D53BD3D112PrivateClass)
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
  var gi : Int

  init(_ ii: Int) {
    gi = ii
  }

  required init(coder aDecoder: NSCoder) {
    gi = aDecoder.decodeInteger(forKey: "gi")
  }

  func encode(with aCoder: NSCoder) {
    aCoder.encode(gi, forKey: "gi")
  }
}

class TopLevel : NSObject, NSCoding {
  var tli : Int

  var nested: ABC.NestedClass?
  fileprivate var priv: PrivateClass?
  var generic : GenericClass<Int>?

  init(_ ii: Int) {
    tli = ii
  }

  required init(coder aDecoder: NSCoder) {
    tli = aDecoder.decodeInteger(forKey: "tli")
    nested = aDecoder.decodeObject(forKey: "nested") as? ABC.NestedClass
    priv = aDecoder.decodeObject(forKey: "priv") as? PrivateClass
    generic = aDecoder.decodeObject(forKey: "generic") as? GenericClass<Int>
  }

  func encode(with aCoder: NSCoder) {
    aCoder.encode(tli, forKey: "tli")
    aCoder.encode(nested, forKey: "nested")
    aCoder.encode(priv, forKey: "priv")
    aCoder.encode(generic, forKey: "generic")
  }
}

func main() {

  let args = CommandLine.arguments

  let g = GenericClass<Int>(42)
#if ENCODE
  // This is how the archive was created with the swift 3.1 compiler.
  let c = TopLevel(27)
  c.nested = ABC.NestedClass(28)
  c.priv = PrivateClass(29)
  c.generic = g

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
      if let g = x.generic {
        // CHECK: generic: 42
        print("generic: \(g.gi)")
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

