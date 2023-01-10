import XCTest
import _Runtime

enum MetadataTests {
  struct Dog {}
  
  enum Cat {}
  
  class Fish {}
}

extension RuntimeTests {
  func testMetadata() throws {
    let structMetadata = Metadata(MetadataTests.Dog.self)
    let enumMetadata = Metadata(MetadataTests.Cat.self)
    let classMetadata = Metadata(MetadataTests.Fish.self)
    let existentialMetadata = Metadata((any Equatable).self)
    let functionMetadata = Metadata((() -> ()).self)
    let metatypeMetadata = Metadata(Int.Type.self)
    let tupleMetadata = Metadata(Void.self)
    
//===----------------------------------------------------------------------===//
// Kinds
//===----------------------------------------------------------------------===//
    
    XCTAssertEqual(structMetadata.kind, .struct)
    XCTAssertEqual(enumMetadata.kind, .enum)
    XCTAssertEqual(classMetadata.kind, .class)
    XCTAssertEqual(existentialMetadata.kind, .existential)
    XCTAssertEqual(functionMetadata.kind, .function)
    XCTAssertEqual(metatypeMetadata.kind, .metatype)
    XCTAssertEqual(tupleMetadata.kind, .tuple)
    
//===----------------------------------------------------------------------===//
// Metadata Equality
//===----------------------------------------------------------------------===//
    
    XCTAssertEqual(structMetadata, structMetadata)
    XCTAssertNotEqual(structMetadata, Metadata(Int.self))
    XCTAssertNotEqual(structMetadata, classMetadata)
  }
}
