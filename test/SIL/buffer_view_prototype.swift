// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -disable-experimental-parser-round-trip \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -enable-experimental-feature NoncopyableGenerics \
// RUN:   -enable-experimental-lifetime-dependence-inference \ 
// RUN:   -Xllvm -enable-lifetime-dependence-diagnostics
// REQUIRES: noncopyable_generics

public struct BufferView<Element> : ~Escapable {
  public typealias Index = Int
  public typealias Pointer = UnsafePointer<Element>
  public let baseAddress: Pointer
  public let count: Int

  // TODO: This should be a failable initializer
  // Currently optional is Escapable, so we cant yet write it.
  public init<Storage>(unsafeBuffer: UnsafeBufferPointer<Element>,
                       storage: borrowing Storage)
    -> _borrow(storage) Self {
    let baseAddress = unsafeBuffer.baseAddress!
    self = BufferView<Element>(baseAddress: baseAddress,
                               count: unsafeBuffer.count)
    return self
  }
  // unsafe private API
  @_unsafeNonescapableResult
  init(baseAddress: Pointer, count: Int) {
    precondition(count >= 0, "Count must not be negative")
    self.baseAddress = baseAddress
    self.count = count
  }
  subscript(_ index: Index) -> Element? {
    if (index < 0 || index >= count) {
      return nil
    }
    return baseAddress[index]
  }
}

extension Array {
  // var view: BufferView<Element> {
  //   withUnsafeBufferPointer {
  //     return BufferView(unsafeBuffer: $0, storage: self)
  //   }
  // }
  // TODO: Implementation of getter should not need a temporary
  // rdar://123071321
  var view: BufferView<Element> {
    var _view : BufferView<Element>? 
    withUnsafeBufferPointer {
      _view = BufferView(unsafeBuffer: $0, storage: self)
    }
    return _view!
  }
}

public func array_view_element(a: [Int] , i: Int) -> Int {
  a.view[i]!
}

