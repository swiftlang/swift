// RUN: %empty-directory(%t) 
// RUN: %target-build-swift %s -module-name=a -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

@propertyWrapper struct D<Value> {
    var wrappedValue: Value {
      didSet {
        print("set: \(wrappedValue)")
      }
    }

    init(wrappedValue: Value) {
      self.wrappedValue = wrappedValue
      print("init: \(wrappedValue)")
    }

}

struct S<T> {
    @D var a: (Int,Int)
    @D var b: (T,T)
    @D var c: (Int,(Int, Bool))
    @D var d: (T,Int)
    @D var e: (Int,(T, Bool))

    init(_ t: T) {
        // CHECK: init: (27, 28)
        a = (27, 28)
        // CHECK: init: (a.X, a.X)
        b = (t, t)
        // CHECK: init: (27, (28, true))
        c = (27, (28, true))
        // CHECK: init: (a.X, 27)
        d = (t, 27)
        // CHECK: init: (27, (a.X, true))
        e = (27, (t, true))
        // CHECK: set: (27, 28)
        a = (27, 28)
        // CHECK: set: (a.X, a.X)
        b = (t, t)
        // CHECK: set: (27, (28, true))
        c = (27, (28, true))
        // CHECK: set: (a.X, 27)
        d = (t, 27)
        // CHECK: set: (27, (a.X, true))
        e = (27, (t, true))
    }

}

class X {


  static var numInstances = 0

  init() {
    X.numInstances += 1
  }

  deinit {
    X.numInstances -= 1
  }
}

func test() {
  _ = S(X())
}

test()

// CHECK: num instances of X: 0
print("num instances of X: \(X.numInstances)")
