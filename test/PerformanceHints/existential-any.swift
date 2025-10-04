// RUN: %empty-directory(%t)

// Ensure ignored by-default
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -verify

// Ensure enabled with '-Wwarning'
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -Wwarning PerformanceHints &> %t/output_warn.txt
// RUN: cat %t/output_warn.txt | %FileCheck %s -check-prefix CHECK-WARN

// Ensure escalated with '-Werror'
// RUN: not %target-swift-frontend -typecheck %s -diagnostic-style llvm -Werror PerformanceHints &> %t/output_err.txt
// RUN: cat %t/output_err.txt | %FileCheck %s -check-prefix CHECK-ERR

// CHECK-ERR: error: Performance: 'AnyAnimal' aliases an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'AnimalContainer' aliases an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'AnimalHandler' aliases an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'returnAnimal1()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'returnAnimal2()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'returnAnimal3()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'returnAnimal4()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'other' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: getter for 'animals' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: getter for 'animals' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'releaseAnimal()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: getter for 'subscript(_:)' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals1' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals2' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals3' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals4' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals5' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals6' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animals7' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'container' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: Ignored value has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animalsA' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animalsB' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: Ignored value has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animalsC' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: Ignored value has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: Ignored value has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'tuple' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'handler' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'factory' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: closure returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'transformer' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: closure returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'handlers' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'handler' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'animalThunk' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'value' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'a1' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'a2' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'pet' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: 'owner' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: parameter has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-ERR: error: Performance: parameter has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]

// CHECK-WARN: error: Performance: 'AnyAnimal' aliases an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'AnimalContainer' aliases an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'AnimalHandler' aliases an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'returnAnimal1()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'returnAnimal2()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'returnAnimal3()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'returnAnimal4()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'other' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: getter for 'animals' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: getter for 'animals' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'releaseAnimal()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: getter for 'subscript(_:)' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals1' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals2' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals3' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals4' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals5' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals6' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animals7' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'container' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: Ignored value has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animalsA' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animalsB' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: Ignored value has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animalsC' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: Ignored value has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: Ignored value has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'tuple' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'handler' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animal' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'factory' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: closure returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'transformer' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: closure returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'handlers' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'handler' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'animalThunk' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'value' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'a1' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'a2' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'pet' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: 'owner' has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: parameter has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]
// CHECK-WARN: error: Performance: parameter has an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead. [#PerformanceHints]

protocol Animal {
  var Name: String { get }
}

struct Tiger: Animal {
  let Name: String = "Tiger"
}

struct Panda: Animal {
  let Name: String = "Panda"
}

struct AnimalError: Error {
  let reason: String

  init(reason: String) {
    self.reason = reason
  }
}

struct Container<T> {
  let value: T
}

protocol Person {
}

////////////////////////////////////////////////////////////////////////////////
// Typealias
///////////////////////////////////////////////////////////////////////////////

typealias AnyAnimal = any Animal
typealias AnimalContainer<T> = Container<any Animal>
typealias AnimalHandler = (any Animal) -> (any Animal)?

////////////////////////////////////////////////////////////////////////////////
// Function Return Type
///////////////////////////////////////////////////////////////////////////////

// Regular
func returnAnimal1() -> any Animal { return Tiger() }

// Optional
func returnAnimal2() -> (any Animal)? {
  let n = Int.random(in: 1...100)
  return (n <= 50) ? nil : Tiger()
}

// Throwing
func returnAnimal3() throws -> any Animal {
  let n = Int.random(in: 1...100)
  if n <= 50 {
    throw AnimalError(reason: "All animals are extinct.")
  } else {
    return Tiger()
  }
}

// Async
func returnAnimal4() async -> any Animal {}

////////////////////////////////////////////////////////////////////////////////
// Function Parameter Type
///////////////////////////////////////////////////////////////////////////////

// Regular parameters
func animalParam1(_ animal: any Animal) {}

// Multiple parameters
func animalParam2(_ animal: any Animal, to other: any Animal) {}

// Variadic parameters
func animalParam3(_ animals: any Animal...) {}

// In-out parameters
func animalParam4(_ animal: inout any Animal) {}

////////////////////////////////////////////////////////////////////////////////
// Protocol
////////////////////////////////////////////////////////////////////////////////
protocol AnimalShelter {
  // Property requirement
  var animals: [any Animal] { get }

  // Method parameter requirement
  func admit(_ animal: any Animal)

  // Method return requirement
  func releaseAnimal() -> (any Animal)?

  // Subscript requirement
  subscript(id: String) -> (any Animal)? { get }
}

////////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////////
class Zoo {
  var animals: [any Animal]

  init(with animal: any Animal) {
    self.animals = [animal]
  }

  init(animals: [any Animal]) {
    self.animals = animals
  }
}

////////////////////////////////////////////////////////////////////////////////
// Compound Types
////////////////////////////////////////////////////////////////////////////////
func testCompoundTypes() {
  let animals1: [any Animal] = []
  print(type(of: animals1))

  let animals2: [any Animal] = []
  print(type(of: animals2))

  let animals3: [String: any Animal] = [:]
  print(type(of: animals3))

  let animals4: [String: any Animal] = [:]
  print(type(of: animals4))

  let animals5: (any Animal)? = nil
  print(type(of: animals5))

  let animals6: (any Animal)? = nil
  print(type(of: animals6))

  let animals7: Result<any Animal, Error> = .success(Tiger())
  print(type(of: animals7))

  let container = Container<any Animal>(value: Tiger())
  print(type(of: container))
}

////////////////////////////////////////////////////////////////////////////////
// Tuple
////////////////////////////////////////////////////////////////////////////////
func tupleTest() {
  let _ = ([Tiger() as any Animal], [Panda() as any Animal])

  let (animalsA, animalsB) = ([Tiger() as any Animal], [Panda() as any Animal])
  print(type(of: animalsA))
  print(type(of: animalsB))

  let (_, animalsC) = ([Tiger() as any Animal], [Panda() as any Animal])
  print(type(of: animalsC))

  let (_, _) = ([Tiger() as any Animal], [Panda() as any Animal])

  let tuple: (animal1: any Animal, animal2: any Animal) = (Tiger(), Panda())
  print(type(of: tuple))
}

////////////////////////////////////////////////////////////////////////////////
// Closure
////////////////////////////////////////////////////////////////////////////////
func closureTest() {
  // Closure parameter type
  let handler: (any Animal) -> Void = { animal in print(type(of: animal)) }
  handler(Tiger())

  // Closure return type
  let factory: () -> any Animal = { Tiger() }
  print(type(of: factory()))

  // Both parameter and return types
  let transformer: (any Animal) -> any Animal = { $0 }
  print(type(of: transformer(Tiger())))

  // Escaping closures
  var handlers: [(any Animal) -> Void] = []
  func registerHandler(with handler: @escaping (any Animal) -> Void) {
    handlers.append(handler)
  }

  // Autoclosure
  func registerHandler2(animalThunk: @autoclosure () -> any Animal) {
    handlers[0](animalThunk())
  }
}

////////////////////////////////////////////////////////////////////////////////
// Type casting
///////////////////////////////////////////////////////////////////////////////
protocol A {
}

protocol A1: A {
}

protocol A2: A {
}

struct S1: A1 {
}

struct S2: A2 {
}

func testTypeCasting() {
  let randomNumber = Int.random(in: 1...100)

  let value: any A = randomNumber <= 50 ? S1() : S2()

  let a1 = value as? any A1
  print(type(of: a1))

  let a2 = value as! any A2
  print(type(of: a2))
}

////////////////////////////////////////////////////////////////////////////////
// Enum
///////////////////////////////////////////////////////////////////////////////
enum PetOwnership {
  case owned(pet: any Animal, owner: any Person)
  case stray(any Animal)
  case multiple([any Animal])
}
