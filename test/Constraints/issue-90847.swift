// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/90847
//
// An overload that refines another lost overload ranking when the two declared
// different typed-throws error types, leaving the call ambiguous.

protocol Foo {}
protocol SpecializedFoo: Foo {}

enum ErrorA: Error { case a }
enum ErrorB: Error { case b }

func bar(foo: some Foo) throws(ErrorA) {}
func bar(foo: some SpecializedFoo) throws(ErrorB) {}

struct ConcreteFoo: SpecializedFoo {}

func callBarGenerically(foo: some SpecializedFoo) throws {
  try bar(foo: foo) // Ok
}

func callBarConcretely() throws {
  try bar(foo: ConcreteFoo()) // Ok
}

// The originally reported case, reduced from mochidev/Bytes: the specialization
// comes from a trailing `where Self:` clause rather than a protocol refinement,
// and the two thrown types are unrelated nested enums.

typealias Byte = UInt8

enum BytesError {
  enum BufferSizeError: Error { case invalidSize }
  enum ContiguousBytes {
    enum BufferSizeError: Error { case invalidSize }
  }
}

protocol ContiguousBytesCollection: Collection where Element == Byte {}

extension Collection where Element == Byte {
  func casting<R>(
    to target: R.Type = R.self,
    targetType: String? = nil
  ) throws(BytesError.ContiguousBytes.BufferSizeError) -> R { fatalError() }

  func casting<R>(
    to target: R.Type = R.self,
    targetType: String? = nil
  ) throws(BytesError.BufferSizeError) -> R where Self: ContiguousBytesCollection { fatalError() }
}

func makeInt<B: ContiguousBytesCollection>(
  bigEndianBytes: B
) throws(BytesError.BufferSizeError) -> Int {
  try bigEndianBytes.casting() // Ok
}
