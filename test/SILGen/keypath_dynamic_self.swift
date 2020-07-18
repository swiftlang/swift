// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -sil-verify-all -o %t/a.out %s
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

class Foo {
  var a: Int = 0
  
  var dynamicSelf: Self {
    return self
  }
  
  subscript() -> Self {
    get {
      return self
    }
  }
  
  init() {
    print(\Self.a)
  }
  
  // self is dynamic in a convenience init
  convenience init(a: Int) {
    self.init()
    self[keyPath: \.a] = a
    print(self[keyPath: \.dynamicSelf.a])
  }
}

class Bar: Foo { var b: Int = 0 }

let foo = Foo(a: 1)
let bar = Bar()

print(foo[keyPath: \Foo.dynamicSelf])
print(foo[keyPath: \.dynamicSelf[].a])
print(bar[keyPath: \Bar[].dynamicSelf.b])
