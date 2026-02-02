// RUN: %target-typecheck-verify-swift -Werror ExistentialType

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

typealias AnyAnimal = any Animal  // expected-error {{Performance: 'AnyAnimal' aliases an existential type, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
typealias AnimalContainer<T> = Container<any Animal>  // expected-error {{Performance: 'AnimalContainer' aliases an existential type, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
typealias AnimalHandler = (any Animal) -> (any Animal)?  // expected-error {{Performance: 'AnimalHandler' aliases an existential type, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}

////////////////////////////////////////////////////////////////////////////////
// Function Return Type
///////////////////////////////////////////////////////////////////////////////

// Regular
func returnAnimal1() -> any Animal { return Tiger() }  // expected-error {{Performance: 'returnAnimal1()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}

// Optional
func returnAnimal2() -> (any Animal)? {  // expected-error {{Performance: 'returnAnimal2()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  let n = Int.random(in: 1...100)
  return (n <= 50) ? nil : Tiger()
}

// Throwing
func returnAnimal3() throws -> any Animal {  // expected-error {{Performance: 'returnAnimal3()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  let n = Int.random(in: 1...100)
  if n <= 50 {
    throw AnimalError(reason: "All animals are extinct.")
  } else {
    return Tiger()
  }
}

// Async
func returnAnimal4() async -> any Animal {}  // expected-error {{Performance: 'returnAnimal4()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}

////////////////////////////////////////////////////////////////////////////////
// Function Parameter Type
///////////////////////////////////////////////////////////////////////////////

// Regular parameters
func animalParam1(_ animal: any Animal) {}  // expected-error {{Performance: 'animal' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead}}

// Multiple parameters
func animalParam2(
  _ animal: any Animal,  // expected-error {{Performance: 'animal' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  to other: any Animal  // expected-error {{Performance: 'other' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
) {}

// Variadic parameters
func animalParam3(_ animals: any Animal...) {}  // expected-error {{Performance: 'animals' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}

// In-out parameters
func animalParam4(_ animal: inout any Animal) {}  // expected-error {{Performance: 'animal' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}

////////////////////////////////////////////////////////////////////////////////
// Protocol
////////////////////////////////////////////////////////////////////////////////
protocol AnimalShelter {
  var animal: (any Animal)? {  // expected-error {{Performance: 'animal' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
    get  // expected-error {{Performance: getter for 'animal' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  }
  func admit(_ animal: any Animal)  // expected-error {{Performance: 'animal' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  func releaseAnimal() -> (any Animal)?  // expected-error {{Performance: 'releaseAnimal()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  subscript(id: String) -> (any Animal)? { get }  // expected-error {{Performance: getter for 'subscript(_:)' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
}

////////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////////
class Zoo {
  var animals: [any Animal]  // expected-error {{Performance: 'animals' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}

  init(with animal: any Animal) {  // expected-error {{Performance: 'animal' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
    self.animals = [animal]
  }

  init(animals: [any Animal]) {  // expected-error {{Performance: 'animals' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
    self.animals = animals
  }
}

////////////////////////////////////////////////////////////////////////////////
// Compound Types
////////////////////////////////////////////////////////////////////////////////
func testCompoundTypes() {
  let animals1: [any Animal] = []  // expected-error {{Performance: 'animals1' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: animals1))

  let animals2: [any Animal] = []  // expected-error {{Performance: 'animals2' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: animals2))

  let animals3: [String: any Animal] = [:]  // expected-error {{Performance: 'animals3' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: animals3))

  let animals4: [String: any Animal] = [:]  // expected-error {{Performance: 'animals4' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: animals4))

  let animals5: (any Animal)? = nil  // expected-error {{Performance: 'animals5' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: animals5))

  let animals6: (any Animal)? = nil  // expected-error {{Performance: 'animals6' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: animals6))

  let animals7: Result<any Animal, Error> = .success(Tiger())  // expected-error {{Performance: 'animals7' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: animals7))

  let container = Container<any Animal>(value: Tiger())  // expected-error {{Performance: 'container' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: container))
}

////////////////////////////////////////////////////////////////////////////////
// Tuple
////////////////////////////////////////////////////////////////////////////////
func tupleTest() {
  let _ = ([Tiger() as any Animal], [Panda() as any Animal])  // expected-error {{Performance: declaration uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}

  let (
    animalsA,  // expected-error {{Performance: 'animalsA' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
    animalsB  // expected-error {{Performance: 'animalsB' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  ) = ([Tiger() as any Animal], [Panda() as any Animal])
  print(type(of: animalsA))
  print(type(of: animalsB))

  let (
    _,  // expected-error {{Performance: declaration uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
    animalsC  // expected-error {{Performance: 'animalsC' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  ) = ([Tiger() as any Animal], [Panda() as any Animal])

  print(type(of: animalsC))

  let (_, _) = ([Tiger() as any Animal], [Panda() as any Animal])  // expected-error 2 {{Performance: declaration uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}

  let tuple: (animal1: any Animal, animal2: any Animal) = (Tiger(), Panda())  // expected-error {{Performance: 'tuple' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: tuple))
}

////////////////////////////////////////////////////////////////////////////////
// Closure
////////////////////////////////////////////////////////////////////////////////
func closureTest() {
  // Closure parameter type
  let handler: (any Animal) -> Void = {  // expected-error {{Performance: 'handler' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
    animal in print(type(of: animal))  // expected-error {{'animal' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  }
  handler(Tiger())

  // Closure return type
  let factory: () -> any Animal = { Tiger() }  // expected-error {{Performance: 'factory' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}} expected-error {{Performance: closure returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: factory()))

  // Both parameter and return types
  let transformer: (any Animal) -> any Animal = { $0 }  // expected-error {{Performance: 'transformer' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}} expected-error {{Performance: closure returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: transformer(Tiger())))

  // Escaping closures
  var handlers: [(any Animal) -> Void] = []  // expected-error {{Performance: 'handlers' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  func registerHandler(with handler: @escaping (any Animal) -> Void) {  // expected-error {{Performance: 'handler' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
    handlers.append(handler)
  }

  // Autoclosure
  func registerHandler2(animalThunk: @autoclosure () -> any Animal) {  // expected-error {{Performance: 'animalThunk' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
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

  let value: any A = randomNumber <= 50 ? S1() : S2()  // expected-error {{Performance: 'value' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}

  let a1 = value as? any A1  // expected-error {{Performance: 'a1' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: a1))

  let a2 = value as! any A2  // expected-error {{Performance: 'a2' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  print(type(of: a2))
}

////////////////////////////////////////////////////////////////////////////////
// Enum
///////////////////////////////////////////////////////////////////////////////
enum PetOwnership {
  case owned(
    pet: any Animal,  // expected-error {{Performance: 'pet' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
    owner: any Person)  // expected-error {{Performance: 'owner' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  case stray(any Animal)  // expected-error {{Performance: parameter uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  case multiple([any Animal])  // expected-error {{Performance: parameter uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
}

////////////////////////////////////////////////////////////////////////////////
// An existential in parent type
///////////////////////////////////////////////////////////////////////////////
struct Outer<T> {
  struct Inner {}
  var i = Inner()
}

func f() -> Outer<any Animal>.Inner {  // expected-error {{Performance: 'f()' returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  return Outer<any Animal>().i
}

////////////////////////////////////////////////////////////////////////////////
// Implicit AST Nodes
////////////////////////////////////////////////////////////////////////////////

func stringPlusInReduce() {
  let animalKinds = ["Tiger", "Panda", "Tiger", "Dodo"]
  let animals: [any Animal] = // expected-error {{Performance: 'animals' uses an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
  animalKinds.map { animalKind in // expected-error {{Performance: closure returns an existential, leading to heap allocation, reference counting, and dynamic dispatch. Consider using generic constraints or concrete types instead.}}
      switch animalKind {
      case "Tiger":
        return Tiger()
      default:
        return Panda()
      }
    }
  print(animals)
}
