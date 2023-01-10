import XCTest
import Runtime

extension RuntimeTests {
  func testMetatypeMetadata() throws {
    let metatype = Metadata(Int.Type.self).metatype
    let int = Metadata(Int.self)
    
    XCTAssertEqual(metatype.instanceMetadata, int)
  }
}
