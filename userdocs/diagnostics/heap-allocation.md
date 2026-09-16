# Heap allocation (HeapAllocation)

Embedded Swift is designed to fit constrained environments. In some cases, it is valuable to ensure that certain programs (or parts of programs) to not allocation memory. The `HeapAllocation` diagnostic group is disabled by default, but can be enabled to identify places in an Embedded Swift program that allocate memory. This includes:

* Creation of instances of class types:
  ```swift
  class C { }
  func createC() -> C { return C() } // warning: creating an instance of type 'C' involves heap allocation
  ```

* Direct allocation of memory through `UnsafeMutablePointer` or similar"
  ```swift
  let p = UnsafeMutableBufferPointer<Double>.allocate(capacity: count) // warning: explicit heap allocation
  ```

* Forming an escaping closure that has captures:
  ```swift
  func acceptClosure(body: @escaping () -> Void) { }

  func passClosure(i: Int) {
    acceptClosure { // warning: escaping closure involves heap allocation
      print(i)
    }
  }
  ```

* Invoking an asynchronous function:
  ```swift
  func f() async { }
  
  func g(i: Int) async {
    await f()
    print(i) // warning: async call involves heap allocation
  }
  ```

* Forming a key path that captures values:
  ```swift
  func getPath<T>(index: Int) -> KeyPath<[T], T> {
    return \[T].[index] // warning: a key path that captures 1 value requires a heap allocationa key
  }
  ```

* Forming an `any` type that requires heap allocation, for example because a large value type is being stored into it:
  ```swift
  protocol P { }
  struct BigType: P { ... }

  func formP() -> any P {
    return BigType() // warning: boxing a value of type 'BigType' into an 'any P' involves heap allocation
  }
  ```

* Forming an indirect enum case:
  ```swift
  enum ComputationTree {
    case literal(Double)
    indirect case binaryOperation(BinaryOperator, ComputationTree, ComputationTree)
  }

  lett add = ComputationTree.binaryOperation(.add, lhs, rhs) // warning: requires heap allocation
  ```
