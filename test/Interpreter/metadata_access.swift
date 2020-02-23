// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

@_silgen_name("_swift_metadataAccessorCall")
func _metadataAccessorCall(
  fn: UnsafeRawPointer,
  request: UInt,
  args: UnsafePointer<Any.Type>?,
  count: Int
) -> MetadataResponse

struct MetadataResponse {
  let type: Any.Type

  let state: UInt8
}

struct MetadataAccessFunction {
  let ptr: UnsafeRawPointer

  func callAsFunction(request: UInt, args: [Any.Type]) -> MetadataResponse {
    args.withUnsafeBufferPointer {
      _metadataAccessorCall(
        fn: ptr,
        request: request,
        args: $0.baseAddress!,
        count: $0.count
      )
    }
  }
}

func callStructAccessor(for type: Any.Type, with generics: Any.Type...) {
  let metadata = unsafeBitCast(type, to: UnsafeRawPointer.self)
  let descriptor = metadata.advanced(by: 8).load(as: UnsafeRawPointer.self)
  let accessorLoc = descriptor.advanced(by: 12)
  let accessor = accessorLoc.advanced(by: Int(accessorLoc.load(as: Int32.self)))

  let accessFn = MetadataAccessFunction(ptr: accessor)
  print(accessFn(request: 0, args: generics))
}

// CHECK: MetadataResponse(type: Swift.Int, state: 0)
callStructAccessor(for: Int.self)

// CHECK: MetadataResponse(type: Swift.Array<Swift.Double>, state: 0)
callStructAccessor(for: [Int].self, with: Double.self)

// CHECK: MetadataResponse(type: Swift.Dictionary<Swift.Int, Swift.Array<Swift.Double>>, state: 0)
callStructAccessor(for: [String: [Int]].self, with: Int.self, [Double].self)
