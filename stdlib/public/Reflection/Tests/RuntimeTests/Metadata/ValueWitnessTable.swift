import XCTest
import Runtime

enum ValueWitnessTableTests {
  struct Dog: Equatable {
    let name: String
    let age: Int
  }
}

extension RuntimeTests {
  func testValueWitnessTable() throws {
    let meta = Metadata(ValueWitnessTableTests.Dog.self).struct
    let vwt = meta.vwt
    
//===----------------------------------------------------------------------===//
// Property API
//===----------------------------------------------------------------------===//
    
    XCTAssertEqual(vwt.size, 24)
    XCTAssertEqual(vwt.stride, 24)
    XCTAssertEqual(vwt.flags.alignment, 8)
    XCTAssertEqual(vwt.flags.alignmentMask, 7)
    XCTAssert(vwt.flags.isValueInline)
    XCTAssertFalse(vwt.flags.isPOD)
    XCTAssertFalse(vwt.flags.hasSpareBits)
    XCTAssert(vwt.flags.isBitwiseTakable)
    XCTAssertFalse(vwt.flags.hasEnumWitnesses)
    XCTAssertFalse(vwt.flags.isIncomplete)
    
//===----------------------------------------------------------------------===//
// InitializeBufferWithCopyOfBuffer
//===----------------------------------------------------------------------===//
    
    let anySopie: Any = ValueWitnessTableTests.Dog(name: "Sophie", age: 11)
    var anySophiev2 = AnyExistentialContainer(
      type: ValueWitnessTableTests.Dog.self
    )
    
    withUnsafePointer(to: anySopie) {
      let src = $0
      
      anySophiev2.projectValue {
        let dest = $0
        
        vwt.initializeBufferWithCopyOfBuffer(dest, src)
      }
    }
    
    let concreteSophie = anySopie as! ValueWitnessTableTests.Dog
    let concreteSophiev2 = unsafeBitCast(anySophiev2, to: Any.self) as! ValueWitnessTableTests.Dog
    
    XCTAssertEqual(concreteSophie, concreteSophiev2)
    
//===----------------------------------------------------------------------===//
// Destroy
//===----------------------------------------------------------------------===//
    
    withUnsafeTemporaryAllocation(
      of: ValueWitnessTableTests.Dog.self,
      capacity: 1
    ) {
      $0.initializeElement(at: 0, to: .init(name: "Sparky", age: 25))
      
      vwt.destroy(UnsafeMutableRawPointer($0.baseAddress!))
    }
    
//===----------------------------------------------------------------------===//
// InitializeWithCopy
//===----------------------------------------------------------------------===//
    
    let sophie = ValueWitnessTableTests.Dog(name: "Sophie", age: 11)
    let sophiev2 = withUnsafeTemporaryAllocation(
      of: ValueWitnessTableTests.Dog.self,
      capacity: 1
    ) {
      let destPtr = UnsafeMutableRawPointer($0.baseAddress!)
      
      withUnsafePointer(to: sophie) {
        let srcPtr = UnsafeRawPointer($0)
        
        vwt.initializeWithCopy(destPtr, srcPtr)
      }
      
      return $0[0]
    }
    
    XCTAssertEqual(sophie, sophiev2)
    
//===----------------------------------------------------------------------===//
// AssignWithCopy
//===----------------------------------------------------------------------===//
    
    var assignSparky = ValueWitnessTableTests.Dog(name: "Sparky", age: 25)
    
    withUnsafePointer(to: sophie) {
      let src = UnsafeRawPointer($0)
      
      withUnsafeMutablePointer(to: &assignSparky) {
        let dest = UnsafeMutableRawPointer($0)
        
        vwt.assignWithCopy(dest, src)
      }
    }
    
    XCTAssertEqual(sophie, assignSparky)
    
//===----------------------------------------------------------------------===//
// InitializeWithTake
//===----------------------------------------------------------------------===//
    
    var takenSophie = ValueWitnessTableTests.Dog(name: "Sophie", age: 11)
    
    let newSophie = withUnsafeTemporaryAllocation(
      of: ValueWitnessTableTests.Dog.self,
      capacity: 1
    ) {
      let dest = UnsafeMutableRawPointer($0.baseAddress!)
      
      withUnsafeMutablePointer(to: &takenSophie) {
        let src = UnsafeMutableRawPointer($0)
        
        vwt.initializeWithTake(dest, src)
      }
      
      return $0[0]
    }
    
    XCTAssertEqual(newSophie, sophie)
    
//===----------------------------------------------------------------------===//
// AssignWithTake
//===----------------------------------------------------------------------===//
    
    takenSophie = ValueWitnessTableTests.Dog(name: "Sophie", age: 11)
    assignSparky = ValueWitnessTableTests.Dog(name: "Sparky", age: 25)
    
    withUnsafeMutablePointer(to: &takenSophie) {
      let src = $0
      
      withUnsafeMutablePointer(to: &assignSparky) {
        let dest = $0
        
        vwt.assignWithTake(dest, src)
      }
    }
    
    XCTAssertEqual(assignSparky, sophie)
  }
  
  func testEnumValueWitnessTable() throws {
    
  }
}
