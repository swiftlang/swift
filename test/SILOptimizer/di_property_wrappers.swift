// RUN: %empty-directory(%t) 
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T {
    didSet {
      print("  .. set \(wrappedValue)")
    }
  }

  init(wrappedValue initialValue: T) {
    print("  .. init \(initialValue)")
    self.wrappedValue = initialValue
  }
}

protocol IntInitializable {
  init(_: Int)
}

final class Payload : CustomStringConvertible, IntInitializable {
  let payload: Int

  init(_ p: Int) {
    self.payload = p
    print("  + payload alloc \(payload)")
  }

  deinit {
    print("  - payload free \(payload)")
  }

  var description: String {
    return "value = \(payload)"
  }
}

struct IntStruct {
  @Wrapper var wrapped: Int

  init() {
     wrapped = 42
     wrapped = 27
  }

  init(conditional b: Bool) {
     if b {
       self._wrapped = Wrapper(wrappedValue: 32)
     } else {
       wrapped = 42
     }
  }

  init(dynamic b: Bool) {
     if b {
       wrapped = 42
     }
     wrapped = 27
  }
}

final class IntClass {
  @Wrapper var wrapped: Int

  init() {
     wrapped = 42
     wrapped = 27
  }

  init(conditional b: Bool) {
     if b {
       self._wrapped = Wrapper(wrappedValue: 32)
     } else {
       wrapped = 42
     }
  }

  init(dynamic b: Bool) {
     if b {
       wrapped = 42
     }
     wrapped = 27
  }
}

struct RefStruct {
  @Wrapper var wrapped: Payload

  init() {
     wrapped = Payload(42)
     wrapped = Payload(27)
  }

  init(conditional b: Bool) {
     if b {
       self._wrapped = Wrapper(wrappedValue: Payload(32))
     } else {
       wrapped = Payload(42)
     }
  }

  init(dynamic b: Bool) {
     if b {
       wrapped = Payload(42)
     }
     wrapped = Payload(27)
  }
}

final class GenericClass<T : IntInitializable> {
  @Wrapper var wrapped: T

  init() {
     wrapped = T(42)
     wrapped = T(27)
  }

  init(conditional b: Bool) {
     if b {
       self._wrapped = Wrapper(wrappedValue: T(32))
     } else {
       wrapped = T(42)
     }
  }

  init(dynamic b: Bool) {
     if b {
       wrapped = T(42)
     }
     wrapped = T(27)
  }
}


func testIntStruct() {
  // CHECK: ## IntStruct
  print("\n## IntStruct")

  // CHECK-NEXT:   .. init 42
  // CHECK-NEXT:   .. set 27
  let t1 = IntStruct()
  // CHECK-NEXT: 27
  print(t1.wrapped)

  // CHECK-NEXT:   .. init 42
  let t2 = IntStruct(conditional: false)
  // CHECK-NEXT: 42
  print(t2.wrapped)

  // CHECK-NEXT:   .. init 32
  let t3 = IntStruct(conditional: true)
  // CHECK-NEXT: 32
  print(t3.wrapped)

  // CHECK-NEXT:   .. init 27
  let t4 = IntStruct(dynamic: false)
  // CHECK-NEXT: 27
  print(t4.wrapped)

  // CHECK-NEXT:   .. init 42
  // CHECK-NEXT:   .. init 27
  let t5 = IntStruct(dynamic: true)
  // CHECK-NEXT: 27
  print(t5.wrapped)
}

func testIntClass() {
  // CHECK: ## IntClass
  print("\n## IntClass")

  // CHECK-NEXT:   .. init 42
  // CHECK-NEXT:   .. set 27
  let t1 = IntClass()
  // CHECK-NEXT: 27
  print(t1.wrapped)

  // CHECK-NEXT:   .. init 42
  let t2 = IntClass(conditional: false)
  // CHECK-NEXT: 42
  print(t2.wrapped)

  // CHECK-NEXT:   .. init 32
  let t3 = IntClass(conditional: true)
  // CHECK-NEXT: 32
  print(t3.wrapped)

  // CHECK-NEXT:   .. init 27
  let t4 = IntClass(dynamic: false)
  // CHECK-NEXT: 27
  print(t4.wrapped)

  // CHECK-NEXT:   .. init 42
  // CHECK-NEXT:   .. init 27
  let t5 = IntClass(dynamic: true)
  // CHECK-NEXT: 27
  print(t5.wrapped)
}

func testRefStruct() {
  // CHECK: ## RefStruct
  print("\n## RefStruct")

  if true {
    // CHECK-NEXT:   + payload alloc 42
    // CHECK-NEXT:   .. init value = 42
    // CHECK-NEXT:   + payload alloc 27
    // CHECK-NEXT:   .. set value = 27
    // CHECK-NEXT:   - payload free 42
    let t1 = RefStruct()
    // CHECK-NEXT: value = 27
    print(t1.wrapped)
    // CHECK-NEXT:   - payload free 27
  }
  if true {
    // CHECK-NEXT:   + payload alloc 42
    // CHECK-NEXT:   .. init value = 42
    let t2 = RefStruct(conditional: false)
    // CHECK-NEXT: value = 42
    print(t2.wrapped)
    // CHECK-NEXT:   - payload free 42
  }
  if true {
    // CHECK-NEXT:   + payload alloc 32
    // CHECK-NEXT:   .. init value = 32
    let t3 = RefStruct(conditional: true)
    // CHECK-NEXT: value = 32
    print(t3.wrapped)
    // CHECK-NEXT:   - payload free 32
  }
  if true {
    // CHECK-NEXT:   + payload alloc 27
    // CHECK-NEXT:   .. init value = 27
    let t4 = RefStruct(dynamic: false)
    // CHECK-NEXT: value = 27
    print(t4.wrapped)
    // CHECK-NEXT:   - payload free 27
  }
  if true {
    // CHECK-NEXT:   + payload alloc 42
    // CHECK-NEXT:   .. init value = 42
    // CHECK-NEXT:   + payload alloc 27
    // CHECK-NEXT:   - payload free 42
    // CHECK-NEXT:   .. init value = 27
    let t5 = RefStruct(dynamic: true)
    // CHECK-NEXT: value = 27
    print(t5.wrapped)
    // CHECK-NEXT:   - payload free 27
  }
}

func testGenericClass() {
  // CHECK: ## GenericClass
  print("\n## GenericClass")

  if true {
    // CHECK-NEXT:   + payload alloc 42
    // CHECK-NEXT:   .. init value = 42
    // CHECK-NEXT:   + payload alloc 27
    // CHECK-NEXT:   .. set value = 27
    // CHECK-NEXT:   - payload free 42
    let t1 = GenericClass<Payload>()
    // CHECK-NEXT: value = 27
    print(t1.wrapped)
    // CHECK-NEXT:   - payload free 27
  }
  if true {
    // CHECK-NEXT:   + payload alloc 42
    // CHECK-NEXT:   .. init value = 42
    let t2 = GenericClass<Payload>(conditional: false)
    // CHECK-NEXT: value = 42
    print(t2.wrapped)
    // CHECK-NEXT:   - payload free 42
  }
  if true {
    // CHECK-NEXT:   + payload alloc 32
    // CHECK-NEXT:   .. init value = 32
    let t3 = GenericClass<Payload>(conditional: true)
    // CHECK-NEXT: value = 32
    print(t3.wrapped)
    // CHECK-NEXT:   - payload free 32
  }
  if true {
    // CHECK-NEXT:   + payload alloc 27
    // CHECK-NEXT:   .. init value = 27
    let t4 = GenericClass<Payload>(dynamic: false)
    // CHECK-NEXT: value = 27
    print(t4.wrapped)
    // CHECK-NEXT:   - payload free 27
  }
  if true {
    // CHECK-NEXT:   + payload alloc 42
    // CHECK-NEXT:   .. init value = 42
    // CHECK-NEXT:   + payload alloc 27
    // CHECK-NEXT:   - payload free 42
    // CHECK-NEXT:   .. init value = 27
    let t5 = GenericClass<Payload>(dynamic: true)
    // CHECK-NEXT: value = 27
    print(t5.wrapped)
    // CHECK-NEXT:   - payload free 27
  }
}

@propertyWrapper
struct WrapperWithDefaultInit<Value> {
  private var _value: Value? = nil

  init() {
    print("default init called on \(Value.self)")
  }
  
  var wrappedValue: Value {
    get {
      return _value!
    } set {
      print("set value \(newValue)")
      _value = newValue
    }
  }
}

struct UseWrapperWithDefaultInit {
  @WrapperWithDefaultInit<Int>
  var x: Int

  @WrapperWithDefaultInit<String>
  var y: String

  init(y: String) {
    self.y = y
  }
}

func testDefaultInit() {
  // CHECK: ## DefaultInit
  print("\n## DefaultInit")

  let use = UseWrapperWithDefaultInit(y: "hello")
  // CHECK: default init called on Int

  // FIXME: DI should eliminate the following call
  // CHECK: default init called on String
  // CHECK: set value hello
}

// rdar://problem/51581937: DI crash with a property wrapper of an optional
struct OptIntStruct {
  @Wrapper var wrapped: Int?

  init() {
     wrapped = 42
  }
}

func testOptIntStruct() {
  // CHECK: ## OptIntStruct
  print("\n## OptIntStruct")

  let use = OptIntStruct()
  // CHECK-NEXT:   .. init nil
  // CHECK-NEXT:   .. set Optional(42)
}

// rdar://problem/53504653

struct DefaultNilOptIntStruct {
  @Wrapper var wrapped: Int?

  init() {
  }
}
func testDefaultNilOptIntStruct() {
  // CHECK: ## DefaultNilOptIntStruct
  print("\n## DefaultNilOptIntStruct")

  let use = DefaultNilOptIntStruct()
  // CHECK-NEXT:   .. init nil
}

testIntStruct()
testIntClass()
testRefStruct()
testGenericClass()
testDefaultInit()
testOptIntStruct()
testDefaultNilOptIntStruct()
