// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

import SwiftShims

struct MetadataAccessFunction {
  let ptr: UnsafeMutableRawPointer

  func callAsFunction(request: Int, args: [Any.Type]) -> MetadataResponse {
    args.withUnsafeBufferPointer {
      $0.baseAddress!.withMemoryRebound(
        to: UnsafeRawPointer?.self,
        capacity: args.count
      ) {
        _swift_metadataAccessorCall(ptr, request, $0, args.count)
      }
    }
  }
}

func callStructAccessor(
  for type: Any.Type,
  with generics: Any.Type...
) -> MetadataResponse {
  let metadata = unsafeBitCast(type, to: UnsafeRawPointer.self)
  let descriptor = metadata.advanced(by: MemoryLayout<Int>.size)
                           .load(as: UnsafeMutableRawPointer.self)
  let accessorLoc = descriptor.advanced(by: MemoryLayout<Int32>.size * 3)
  let accessor = accessorLoc.advanced(by: Int(accessorLoc.load(as: Int32.self)))

  let accessFn = MetadataAccessFunction(ptr: accessor)
  return accessFn(request: 0, args: generics)
}

let int = callStructAccessor(for: Int.self)
// CHECK: Int
print(unsafeBitCast(int.type!, to: Any.Type.self))
// CHECK: 0
print(int.state)

let doubleArray = callStructAccessor(for: [Int].self, with: Double.self)
// CHECK: Array<Double>
print(unsafeBitCast(doubleArray.type!, to: Any.Type.self))
// CHECK: 0
print(doubleArray.state)

let dictOfIntAndDoubleArray = callStructAccessor(
  for: [String: [Int]].self,
  with: Int.self, [Double].self
)
// CHECK: Dictionary<Int, Array<Double>>
print(unsafeBitCast(dictOfIntAndDoubleArray.type!, to: Any.Type.self))
// CHECK: 0
print(dictOfIntAndDoubleArray.state)
