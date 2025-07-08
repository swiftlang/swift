//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.3, *)
@frozen
@safe
public struct _RefCell<Value: ~Copyable>: ~Copyable {
  // < 0 = Writing
  // 0 = Unused
  // > 0 = Borrows are outstanding
  @usableFromInline
  let borrows = _Cell(0)

  @usableFromInline
  let value: _UnsafeCell<Value>

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ initialValue: consuming Value) {
    unsafe value = _UnsafeCell(initialValue)
  }
}

@available(SwiftStdlib 6.3, *)
extension _RefCell where Value: ~Copyable {
  @available(SwiftStdlib 6.3, *)
  @frozen
  @safe
  public struct Ref: ~Copyable, ~Escapable {
    @usableFromInline
    let borrows: _Borrow<_Cell<Int>>

    @usableFromInline
    let value: UnsafePointer<Value>

    @available(SwiftStdlib 6.3, *)
    @lifetime(copy borrows)
    @_alwaysEmitIntoClient
    @_transparent
    internal init(borrows: _Borrow<_Cell<Int>>, value: UnsafePointer<Value>) {
      self.borrows = borrows
      unsafe self.value = value
    }

    @available(SwiftStdlib 6.3, *)
    @_alwaysEmitIntoClient
    public subscript() -> Value {
      @_transparent
      unsafeAddress {
        unsafe value
      }
    }

    @_alwaysEmitIntoClient
    @_transparent
    deinit {
      let b = borrows[].get()
      borrows[].set(b - 1)
    }
  }

  @available(SwiftStdlib 6.3, *)
  @lifetime(borrow self)
  @_alwaysEmitIntoClient
  @_transparent
  public func borrow() -> Ref {
    guard borrows.get() >= 0 else {
      _preconditionFailure("Exclusivity violation")
    }

    let b = borrows.get()
    borrows.set(b + 1)
    return unsafe Ref(borrows: &&borrows, value: UnsafePointer(value.address))
  }
}

@available(SwiftStdlib 6.3, *)
extension _RefCell where Value: ~Copyable {
  @available(SwiftStdlib 6.3, *)
  @frozen
  @safe
  public struct MutRef: ~Copyable, ~Escapable {
    @usableFromInline
    let borrows: _Borrow<_Cell<Int>>

    @usableFromInline
    let value: UnsafeMutablePointer<Value>

    @available(SwiftStdlib 6.3, *)
    @lifetime(copy borrows)
    @_alwaysEmitIntoClient
    @_transparent
    internal init(borrows: _Borrow<_Cell<Int>>, value: UnsafeMutablePointer<Value>) {
      self.borrows = borrows
      unsafe self.value = value
    }

    @available(SwiftStdlib 6.3, *)
    @_alwaysEmitIntoClient
    public subscript() -> Value {
      @_transparent
      unsafeAddress {
        unsafe UnsafePointer(value)
      }

      @_transparent
      nonmutating unsafeMutableAddress {
        unsafe value
      }
    }

    @_alwaysEmitIntoClient
    @_transparent
    deinit {
      let b = borrows[].get()
      borrows[].set(b + 1)
    }
  }

  @available(SwiftStdlib 6.3, *)
  @lifetime(borrow self)
  @_alwaysEmitIntoClient
  @_transparent
  public func mutate() -> MutRef {
    guard borrows.get() == 0 else {
      _preconditionFailure("Exclusivity violation")
    }

    let b = borrows.get()
    borrows.set(b - 1)
    return unsafe MutRef(borrows: &&borrows, value: value.address)
  }
}
