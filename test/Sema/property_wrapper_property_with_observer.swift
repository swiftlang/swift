// RUN: %target-run-simple-swift

// REQUIRES: executable_test

@propertyWrapper 
struct Foo {
  private var _storage: [Int] = []

  init(wrappedValue value: [Int]) {
    self._storage = value
  }

  var wrappedValue: [Int] {
    get { _storage }
    set { _storage = newValue }
  }
}

class Bar {
  @Foo var someArray = [1, 2, 3] {
    willSet {
      print(newValue) 
    }

    didSet {
      print(oldValue)
    }
  }
}

let bar = Bar()
// CHECK: [4, 2, 3]
// CHECK: [1, 2, 3]
bar.someArray[0] = 4
