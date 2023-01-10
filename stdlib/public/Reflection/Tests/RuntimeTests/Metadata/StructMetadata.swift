import XCTest
import Runtime

enum StructMetadataTests {
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
  func testStructMetadata() throws {
    let dog = Metadata(StructMetadataTests.Dog.self).struct
    
    XCTAssert(dog.fieldOffsets.elementsEqual([0, 16]))
  }
  
  func testGenericStructMetadata() throws {
    let genericDog = Metadata(StructMetadataTests.GenericDog<String>.self).struct
    
    XCTAssert(genericDog.fieldOffsets.elementsEqual([0, 16, 24]))
  }
}
