import XCTest
import Runtime

enum StructDescriptorTests {
  struct Dog {
    let name: String
    let age: Int
  }
  
  struct GenericDog<T> {
    let one: T
    let age: Int
    let two: T
  }
}

extension RuntimeTests {
  func testStructDescriptor() throws {
    let dog = Metadata(StructDescriptorTests.Dog.self).struct
    let descriptor = dog.descriptor
    
    XCTAssertEqual(descriptor.base.name, "Dog")
  }
  
  func testGenericStructDescriptor() throws {
    let dog = Metadata(StructDescriptorTests.GenericDog<Int>.self).struct
    let descriptor = dog.descriptor
    
    XCTAssertEqual(descriptor.base.name, "GenericDog")
  }
}
