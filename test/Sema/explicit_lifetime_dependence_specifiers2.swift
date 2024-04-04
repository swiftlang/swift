// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature NonescapableTypes -enable-experimental-feature NoncopyableGenerics -enable-experimental-feature BitwiseCopyable
// REQUIRES: asserts
// REQUIRES: nonescapable_types

struct AnotherBufferView : ~Escapable, _BitwiseCopyable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
}

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  init(_ bv: borrowing AnotherBufferView) -> _borrow(bv) Self {
    self.ptr = bv.ptr
    return self
  }
}

