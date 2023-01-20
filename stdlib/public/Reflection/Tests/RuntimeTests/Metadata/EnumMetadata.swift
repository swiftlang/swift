import XCTest
import _Runtime

enum EnumMetadataTests {
  enum Color {
    case red
    case green
    case blue
    case generic(Int)
  }
  
  enum GenericColor<T> {
    case generic(T)
    case generic2(T, T)
    case weird(T, Int, T)
  }
}

extension RuntimeTests {
  func testEnumMetadata() throws {
    let metadata = Metadata(EnumMetadataTests.Color.self).enum
    
    
  }
  
  func testGenericEnumMetadata() throws {
    
  }
}
