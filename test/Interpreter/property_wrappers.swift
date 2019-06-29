// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol Observed: AnyObject {
  func broadcastValueWillChange<T>(newValue: T)
}

@propertyWrapper
struct Observable<Value: Equatable> {
  private var stored: Value

  
  init(initialValue: Value) {
    self.stored = initialValue
  }

  var wrappedValue: Value {
    get { fatalError("called wrappedValue getter") }
    set { fatalError("called wrappedValue setter") }
  }
  
  static subscript<EnclosingSelf: Observed>(
      _enclosingInstance observed: EnclosingSelf,
      storage storageKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Self>
    ) -> Value {
    get {
      observed[keyPath: storageKeyPath].stored
    }
    set {
      if observed[keyPath: storageKeyPath].stored != newValue {
        observed.broadcastValueWillChange(newValue: newValue)
      }
      
      observed[keyPath: storageKeyPath].stored = newValue
    }
  }
}

class MyType<T: Equatable>: Observed {
  @Observable var x: T

  init(x: T) {
    self.x = x
  }
  
  func broadcastValueWillChange<T>(newValue: T) {
    print("Value of 'x' is changing from \(x) to \(newValue)")
  }
}

func testMyType(_ myType: MyType<Int>) {
  // CHECK: Value of 'x' is changing from 17 to 42
  myType.x = 42

  // CHECK-NEXT: Value of 'x' is changing from 42 to 25
  myType.x = 42
  myType.x = 25
}

testMyType(MyType(x: 17))
