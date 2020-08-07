// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

class Base {
  required init() { }

  class func instance() -> Base {
    return self.init()
  }
}

class Middle: Base {
  override class func instance() -> Middle {
    return self.init()
  }
}

class Derived: Middle {
  required init() {
    super.init()
    print("init Derived")
  }
}

struct Maker<C: Base> {
  @inline(never)
  static func create() -> Base {
    return C.instance()
  }
}

// CHECK: init Derived
// CHECK: test.Derived
print(Maker<Derived>.create())

