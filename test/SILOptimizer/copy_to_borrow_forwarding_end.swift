// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -parse-as-library -enable-experimental-feature Lifetimes -primary-file %s -o /dev/null

// REQUIRES: swift_feature_Lifetimes

// rdar://190060113: CopyToBorrowOptimization replaced a `copy_value` whose lifetime ends in a
// `destructure_struct` with only trivial results (no `destroy_value`) by the guaranteed source, which was
// already out of its borrow scope: "Found outside of lifetime use?!"

struct PErr: Error {}

struct PSpan: ~Escapable, ~Copyable {
  var bytes: RawSpan

  @_lifetime(copy bytes)
  init(bytes: RawSpan) {
    self.bytes = bytes
  }

  @_lifetime(copy self)
  func seeking(toOffset start: Int) throws -> PSpan {
    guard start <= bytes.byteCount else { throw PErr() }
    return PSpan(bytes: bytes)
  }

  @_lifetime(copy self)
  func seeking(toRelativeOffset offset: Int) -> PSpan {
    return PSpan(bytes: bytes)
  }

  mutating func consumeByte() -> UInt8 {
    return bytes.unsafeLoad(fromUncheckedByteOffset: 0, as: UInt8.self)
  }
}

func g(_ input: inout PSpan, _ i: Int) throws -> UInt8 {
  var s = try input.seeking(toOffset: i).seeking(toRelativeOffset: i)
  return s.consumeByte()
}
