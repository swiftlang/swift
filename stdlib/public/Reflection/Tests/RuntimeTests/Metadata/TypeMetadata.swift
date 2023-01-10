import XCTest
import Runtime

enum TypeMetadataTests {
  struct Struct<T> {
    let xyz: T
  }
  
  enum Enum<T> {
    case generic(T)
  }
  
  class Class<T> {
    let xyz: T
    
    init(xyz: T) {
      self.xyz = xyz
    }
  }
  
  class WeirdClass<T>: JSONDecoder {}
}

extension RuntimeTests {
//===----------------------------------------------------------------------===//
// Struct case
//===----------------------------------------------------------------------===//
  
  func testStructTypeMetadata() throws {
    let meta = Metadata(TypeMetadataTests.Struct<Int>.self).type
    
    XCTAssertEqual(meta.descriptor.name, "Struct")
    
    let genericArg = meta.genericArguments.load(as: Any.Type.self)
    
    XCTAssert(genericArg == Int.self)
    
    let fieldType = meta.descriptor.fields[0].typeRef
    XCTAssert(meta.resolve(fieldType) == Int.self)
  }
  
//===----------------------------------------------------------------------===//
// Enum case
//===----------------------------------------------------------------------===//
  
  func testEnumTypeMetadata() throws {
    let meta = Metadata(TypeMetadataTests.Enum<String>.self).type
    
    XCTAssertEqual(meta.descriptor.name, "Enum")
    
    let genericArg = meta.genericArguments.load(as: Any.Type.self)
    
    XCTAssert(genericArg == String.self)
    
    let caseType = meta.descriptor.fields[0].typeRef
    XCTAssert(meta.resolve(caseType) == String.self)
  }
  
//===----------------------------------------------------------------------===//
// Class case
//===----------------------------------------------------------------------===//
  
  func testClassTypeMetadata() throws {
    // Non resilient superclass
    
    let meta = Metadata(TypeMetadataTests.Class<Double>.self).type
    
    XCTAssertEqual(meta.descriptor.name, "Class")
    
    let genericArg = meta.genericArguments.load(as: Any.Type.self)
    
    XCTAssert(genericArg == Double.self)
    
    let fieldTy = meta.descriptor.fields[0].typeRef
    XCTAssert(meta.resolve(fieldTy) == Double.self)
    
    // Resilient superclass
    
    let weirdMeta = Metadata(TypeMetadataTests.WeirdClass<Float>.self).type
    
    XCTAssertEqual(weirdMeta.descriptor.name, "WeirdClass")
    
    let weirdGenericArg = weirdMeta.genericArguments.load(as: Any.Type.self)
    print(weirdGenericArg)
    
    XCTAssert(weirdGenericArg == Float.self)
  }
}
