// RUN: %empty-directory(%t) 
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

@propertyDelegate
struct Wrapper<T> {
  var value: T {
    didSet {
      print("  .. set \(value)")
    }
  }

  init(initialValue: T) {
    print("  .. init \(initialValue)")
    self.value = initialValue
  }
}

@propertyDelegate
final class ClassWrapper<T> {
  var value: T {
    didSet {
      print("  .. set \(value)")
    }
  }

  init(initialValue: T) {
    print("  .. init \(initialValue)")
    self.value = initialValue
  }

  deinit {
    print("  .. deinit \(value)")
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
       self.$wrapped = Wrapper(initialValue: 32)
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
       self.$wrapped = Wrapper(initialValue: 32)
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
       self.$wrapped = Wrapper(initialValue: Payload(32))
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
       self.$wrapped = Wrapper(initialValue: T(32))
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

/*

This does not work yet.

struct IntStructWithClassWrapper {
  @ClassWrapper var wrapped: Int

  init() {
     wrapped = 42
     wrapped = 27
  }

  init(conditional b: Bool) {
     if b {
       self.$wrapped = ClassWrapper(initialValue: 32)
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
*/

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

/*
func testIntStructWithClassWrapper() {
  // CHECK-DISABLED: ## IntStructWithClassWrapper
  print("\n## IntStructWithClassWrapper")

  if true {
    // CHECK-NEXT-DISABLED:   .. init 42
    // CHECK-NEXT-DISABLED:   .. set 27
    let t1 = IntStructWithClassWrapper()
    // CHECK-NEXT-DISABLED: 27
    print(t1.wrapped)
    // CHECK-NEXT-DISABLED:   .. deinit 27
  }
  if true {
    // CHECK-NEXT-DISABLED:   .. init 42
    let t2 = IntStructWithClassWrapper(conditional: false)
    // CHECK-NEXT-DISABLED: 42
    print(t2.wrapped)
    // CHECK-NEXT-DISABLED:   .. deinit 42
  }
  if true {
    // CHECK-NEXT-DISABLED:   .. init 32
    let t3 = IntStructWithClassWrapper(conditional: true)
    // CHECK-NEXT-DISABLED: 32
    print(t3.wrapped)
    // CHECK-NEXT-DISABLED:   .. deinit 32
  }
  if true {
    // CHECK-NEXT-DISABLED:   .. init 27
    let t4 = IntStructWithClassWrapper(dynamic: false)
    // CHECK-NEXT-DISABLED: 27
    print(t4.wrapped)
    // CHECK-NEXT-DISABLED:   .. deinit 27
  }
  if true {
    // CHECK-NEXT-DISABLED:   .. init 42
    // CHECK-NEXT-DISABLED:   .. deinit 42
    // CHECK-NEXT-DISABLED:   .. init 27
    let t5 = IntStructWithClassWrapper(dynamic: true)
    // CHECK-NEXT-DISABLED: 27
    print(t5.wrapped)
    // CHECK-NEXT-DISABLED:   .. deinit 27
  }
}
*/

testIntStruct()
testIntClass()
testRefStruct()
testGenericClass()
//testIntStructWithClassWrapper()

