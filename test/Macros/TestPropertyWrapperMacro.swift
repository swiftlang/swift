// TestPropertyWrapperMacro.swift

// Define a simple property wrapper
@propertyWrapper
struct Simple {
  private var storage: Int
  
  var wrappedValue: Int {
    get { return storage }
    set { storage = newValue }
  }
  
  var projectedValue: String {
    return "Value is \(storage)"
  }
  
  init(wrappedValue initialValue: Int) {
    self.storage = initialValue
    print("Initialized with \(initialValue)")
  }
}

// Use the property wrapper in a struct
struct TestStruct {
  @Simple var x: Int = 42
  
  func testAccess() {
    print("Regular access: \(x)")
    print("Projected access: \($x)")
  }
}

// Basic usage
let test = TestStruct()
test.testAccess()
test.x = 100
test.testAccess()
