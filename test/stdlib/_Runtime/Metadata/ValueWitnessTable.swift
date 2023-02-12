// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection
// UNSUPPORTED: freestanding
// XFAIL: OS=wasi

import StdlibUnittest
import _Runtime

let suite = TestSuite("ValueWitnessTable")

struct Dog: Equatable {
  let name: String
  let age: Int
}

if #available(SwiftStdlib 5.9, *) {
  suite.test("Basic") {
    let meta = Metadata(Dog.self).struct
    let vwt = meta.vwt

//===----------------------------------------------------------------------===//
// Property API
//===----------------------------------------------------------------------===//

    expectEqual(vwt.size, 24)
    expectEqual(vwt.stride, 24)
    expectEqual(vwt.flags.alignment, 8)
    expectEqual(vwt.flags.alignmentMask, 7)
    expectTrue(vwt.flags.isValueInline)
    expectFalse(vwt.flags.isPOD)
    expectFalse(vwt.flags.hasSpareBits)
    expectTrue(vwt.flags.isBitwiseTakable)
    expectFalse(vwt.flags.hasEnumWitnesses)
    expectFalse(vwt.flags.isIncomplete)

//===----------------------------------------------------------------------===//
// InitializeBufferWithCopyOfBuffer
//===----------------------------------------------------------------------===//

    let anySopie: Any = Dog(name: "Sophie", age: 11)
    var anySophiev2 = AnyExistentialContainer(
      type: Dog.self
    )

    withUnsafePointer(to: anySopie) {
      let src = $0

      anySophiev2.projectValue {
        let dest = $0

        vwt.initializeBufferWithCopyOfBuffer(dest, src)
      }
    }

    let concreteSophie = anySopie as! Dog
    let concreteSophiev2 = unsafeBitCast(anySophiev2, to: Any.self) as! Dog

    expectEqual(concreteSophie, concreteSophiev2)

//===----------------------------------------------------------------------===//
// Destroy
//===----------------------------------------------------------------------===//

    withUnsafeTemporaryAllocation(
      of: Dog.self,
      capacity: 1
    ) {
      $0.initializeElement(at: 0, to: .init(name: "Sparky", age: 25))

      vwt.destroy(UnsafeMutableRawPointer($0.baseAddress!))
    }

//===----------------------------------------------------------------------===//
// InitializeWithCopy
//===----------------------------------------------------------------------===//

    let sophie = Dog(name: "Sophie", age: 11)
    let sophiev2 = withUnsafeTemporaryAllocation(
      of: Dog.self,
      capacity: 1
    ) {
      let destPtr = UnsafeMutableRawPointer($0.baseAddress!)

      withUnsafePointer(to: sophie) {
        let srcPtr = UnsafeRawPointer($0)

        vwt.initializeWithCopy(destPtr, srcPtr)
      }

      return $0[0]
    }

    expectEqual(sophie, sophiev2)

//===----------------------------------------------------------------------===//
// AssignWithCopy
//===----------------------------------------------------------------------===//

    var assignSparky = Dog(name: "Sparky", age: 25)

    withUnsafePointer(to: sophie) {
      let src = UnsafeRawPointer($0)

      withUnsafeMutablePointer(to: &assignSparky) {
        let dest = UnsafeMutableRawPointer($0)

        vwt.assignWithCopy(dest, src)
      }
    }

    expectEqual(sophie, assignSparky)

//===----------------------------------------------------------------------===//
// InitializeWithTake
//===----------------------------------------------------------------------===//

    var takenSophie = Dog(name: "Sophie", age: 11)

    let newSophie = withUnsafeTemporaryAllocation(
      of: Dog.self,
      capacity: 1
    ) {
      let dest = UnsafeMutableRawPointer($0.baseAddress!)

      withUnsafeMutablePointer(to: &takenSophie) {
        let src = UnsafeMutableRawPointer($0)

        vwt.initializeWithTake(dest, src)
      }

      return $0[0]
    }

    expectEqual(newSophie, sophie)

//===----------------------------------------------------------------------===//
// AssignWithTake
//===----------------------------------------------------------------------===//

    takenSophie = Dog(name: "Sophie", age: 11)
    assignSparky = Dog(name: "Sparky", age: 25)

    withUnsafeMutablePointer(to: &takenSophie) {
      let src = $0

      withUnsafeMutablePointer(to: &assignSparky) {
        let dest = $0

        vwt.assignWithTake(dest, src)
      }
    }

    expectEqual(assignSparky, sophie)
  }
}

runAllTests()
