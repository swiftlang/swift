// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature NonescapableTypes -enable-experimental-feature NoncopyableGenerics
// REQUIRES: asserts
// REQUIRES: nonescapable_types

struct AnotherBufferView : ~Escapable, BitwiseCopyable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
}

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  init(_ bv: borrowing AnotherBufferView) -> dependsOn(bv) Self {
    self.ptr = bv.ptr
    return self
  }
}

struct NonescapableType: ~Escapable {}

func f<T: ~Escapable & BitwiseCopyable>(arg: T) -> NonescapableType {
  NonescapableType()
}
