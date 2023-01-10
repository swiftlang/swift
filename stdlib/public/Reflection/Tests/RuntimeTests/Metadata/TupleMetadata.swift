import XCTest
import Runtime

extension RuntimeTests {
  func testTupleMetadata() throws {
    // Tuple
    
    let tuple = Metadata((name: String, age: Int, [Double], lastThing: UInt64).self).tuple
    
    let offsets = tuple.elements.map { $0.offset }
    
    XCTAssertEqual(offsets, [0, 16, 24, 32])
    
    let metadatas = tuple.elements.map { $0.metadata }
    
    XCTAssertEqual(metadatas, [
      Metadata(String.self),
      Metadata(Int.self),
      Metadata([Double].self),
      Metadata(UInt64.self)
    ])
    
    let labels = tuple.elements.map { $0.label }
    
    XCTAssertEqual(labels, ["name", "age", "", "lastThing"])
    
    // Tuple2
    
    let tuple2 = Metadata((Int, String, Double).self).tuple
    
    let offsets2 = tuple2.elements.map { $0.offset }
    
    XCTAssertEqual(offsets2, [0, 8, 24])
    
    let metadatas2 = tuple2.elements.map { $0.metadata }
    
    XCTAssertEqual(metadatas2, [
      Metadata(Int.self),
      Metadata(String.self),
      Metadata(Double.self)
    ])
    
    let labels2 = tuple2.elements.map { $0.label }
    
    XCTAssertEqual(labels2, ["", "", ""])
  }
}
