// RUN: %target-resilience-test --no-backward-deployment
// REQUIRES: executable_test

import StdlibUnittest
import protocol_add_requirements


var ProtocolAddRequirementsTest = TestSuite("ProtocolAddRequirements")

struct Halogen : ElementProtocol {
  var x: Int

  func increment() -> Halogen {
    return Halogen(x: x + 1)
  }
}

func ==(h1: Halogen, h2: Halogen) -> Bool {
  return h1.x == h2.x
}

struct AddMethods : AddMethodsProtocol {
  func importantOperation() -> Halogen {
    return Halogen(x: 0)
  }

#if AFTER
  func unimportantOperation() -> Halogen {
    return Halogen(x: 10)
  }
#endif
}

ProtocolAddRequirementsTest.test("AddMethodRequirements") {
  let result = doSomething(AddMethods())

#if BEFORE
  let expected = [0, 1, 2].map(Halogen.init)
#else
  let expected = [0, 10, 11].map(Halogen.init)
#endif
}


struct AddConstructors : AddConstructorsProtocol, Equatable {
  var name: String

  init(name: String) {
    self.name = name
  }
}

func ==(a1: AddConstructors, a2: AddConstructors) -> Bool {
  return a1.name == a2.name
}

ProtocolAddRequirementsTest.test("AddConstructorsProtocol") {
  // Would be nice if [T?] was Equatable...
  if getVersion() == 0 {
    let result = testConstructorProtocol(AddConstructors.self)
    expectEqual(result.count, 1)
    expectEqual(result[0], AddConstructors(name: "Puff"))
  } else {
    let result = testConstructorProtocol(AddConstructors.self)
    expectEqual(result.count, 3)
    expectEqual(result[0], AddConstructors(name: "Meow meow"))
    expectEqual(result[1], nil)
    expectEqual(result[2], AddConstructors(name: "Robster the Lobster"))
  }
}


class Box<T> {
  var value: T

  init(value: T) {
    self.value = value
  }
}

struct AddProperties : AddPropertiesProtocol {
  var speedBox: Box<Int> = Box<Int>(value: 160)
  var gearBox: Box<Int> = Box<Int>(value: 8000)

  var topSpeed: Int {
    get {
      return speedBox.value
    }
    nonmutating set {
      speedBox.value = newValue
    }
  }

  var maxRPM: Int {
    get {
      return gearBox.value
    }
    set {
      gearBox.value = newValue
    }
  }
}

ProtocolAddRequirementsTest.test("AddPropertyRequirements") {
  var x = AddProperties()

  do {
    let expected = (getVersion() == 0
                    ? [160, 8000]
                    : [160, 8000, 80, 40, 6000])
    expectEqual(getProperties(&x), expected)
  }

  setProperties(&x)

  do {
    let expected = (getVersion() == 0
                    ? [320, 15000]
                    : [320, 15000, 160, 80, 13000])
    expectEqual(getProperties(&x), expected)
  }
}


struct AddSubscript<Key : Hashable, Value> : AddSubscriptProtocol {
  var dict: [Key : Value] = [:]

  func get(key key: Key) -> Value {
    return dict[key]!
  }

  mutating func set(key key: Key, value: Value) {
    dict[key] = value
  }
}

ProtocolAddRequirementsTest.test("AddSubscriptRequirements") {
  var t = AddSubscript<String, Int>()
  t.set(key: "B", value: 20)
  doSomething(&t, k1: "A", k2: "B")
  expectEqual(t.get(key: "A"), 20)
}

struct AddAssociatedType<T> : AddAssocTypesProtocol { }

ProtocolAddRequirementsTest.test("AddAssociatedTypeRequirements") {
  let addString = AddAssociatedType<String>()
  let stringResult = doSomethingWithAssocTypes(addString)

  if getVersion() == 0 {
    expectEqual("there are no associated types yet", stringResult)
  } else {
    expectEqual("Wrapper<AddAssociatedType<String>>", stringResult)
  }
}

ProtocolAddRequirementsTest.test("AddAssociatedConformanceRequirements") {
  let addString = AddAssociatedType<String>()
  let stringResult = doSomethingWithAssocConformances(addString)

  if getVersion() == 0 {
    expectEqual("there are no associated conformances yet", stringResult)
  } else {
    expectEqual("I am a wrapper for AddAssociatedType<String>", stringResult)
  }
}

runAllTests()

