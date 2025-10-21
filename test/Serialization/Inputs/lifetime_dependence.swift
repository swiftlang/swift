import Builtin

@frozen
@safe
public struct Ref<T: ~Copyable>: Copyable, ~Escapable {
  @usableFromInline
  let _pointer: UnsafePointer<T>

  @_lifetime(borrow value)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ value: borrowing @_addressable T) {
    unsafe _pointer = UnsafePointer(Builtin.unprotectedAddressOfBorrow(value))
  }

  @_alwaysEmitIntoClient
  public subscript() -> T {
    @_transparent
    unsafeAddress {
      unsafe _pointer
    }
  }
}
