//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Pseudo-namespace for pointer authentication primitives.
internal enum _PtrAuth {
  internal struct Key {
    var _value: Int32

    @_transparent
    init(_value: Int32) {
      self._value = _value
    }

#if _ptrauth(_arm64e)
    @_transparent
    static var ASIA: Key { return Key(_value: 0) }
    @_transparent
    static var ASIB: Key { return Key(_value: 1) }
    @_transparent
    static var ASDA: Key { return Key(_value: 2) }
    @_transparent
    static var ASDB: Key { return Key(_value: 3) }

    /// A process-independent key which can be used to sign code pointers.
    /// Signing and authenticating with this key is a no-op in processes
    /// which disable ABI pointer authentication.
    @_transparent
    static var processIndependentCode: Key { return .ASIA }

    /// A process-specific key which can be used to sign code pointers.
    /// Signing and authenticating with this key is enforced even in processes
    /// which disable ABI pointer authentication.
    @_transparent
    static var processDependentCode: Key { return .ASIB }

    /// A process-independent key which can be used to sign data pointers.
    /// Signing and authenticating with this key is a no-op in processes
    /// which disable ABI pointer authentication.
    @_transparent
    static var processIndependentData: Key { return .ASDA }

    /// A process-specific key which can be used to sign data pointers.
    /// Signing and authenticating with this key is a no-op in processes
    /// which disable ABI pointer authentication.
    @_transparent
    static var processDependentData: Key { return .ASDB }
#elseif _ptrauth(_none)
    /// A process-independent key which can be used to sign code pointers.
    /// Signing and authenticating with this key is a no-op in processes
    /// which disable ABI pointer authentication.
    @_transparent
    static var processIndependentCode: Key { return Key(_value: 0) }

    /// A process-specific key which can be used to sign code pointers.
    /// Signing and authenticating with this key is enforced even in processes
    /// which disable ABI pointer authentication.
    @_transparent
    static var processDependentCode: Key { return Key(_value: 0) }

    /// A process-independent key which can be used to sign data pointers.
    /// Signing and authenticating with this key is a no-op in processes
    /// which disable ABI pointer authentication.
    @_transparent
    static var processIndependentData: Key { return Key(_value: 0) }

    /// A process-specific key which can be used to sign data pointers.
    /// Signing and authenticating with this key is a no-op in processes
    /// which disable ABI pointer authentication.
    @_transparent
    static var processDependentData: Key { return Key(_value: 0) }
#else
  #error("unsupported ptrauth scheme")
#endif
  }

#if _ptrauth(_arm64e)
  /// Blend a pointer and a small integer to form a new extra-data
  /// discriminator.  Not all bits of the inputs are guaranteed to
  /// contribute to the result.
  @_transparent
  static func blend(pointer: UnsafeRawPointer,
                    discriminator: UInt64) -> UInt64 {
    return UInt64(Builtin.int_ptrauth_blend(
                    UInt64(UInt(bitPattern: pointer))._value,
                    discriminator._value))
  }

  /// Sign an unauthenticated pointer.
  @_semantics("no.preserve.debugger") // Relies on inlining this function.
  @_transparent
  static func sign(pointer: UnsafeRawPointer,
                   key: Key,
                   discriminator: UInt64) -> UnsafeRawPointer {
    let bitPattern = UInt64(Builtin.int_ptrauth_sign(
      UInt64(UInt(bitPattern: pointer))._value,
      key._value._value,
      discriminator._value))

    return unsafe UnsafeRawPointer(bitPattern:
      UInt(truncatingIfNeeded: bitPattern)).unsafelyUnwrapped
  }

  /// Authenticate a pointer using one scheme and resign it using another.
  @_transparent
  @_semantics("no.preserve.debugger") // Relies on inlining this function.
  static func authenticateAndResign(pointer: UnsafeRawPointer,
                                oldKey: Key,
                                oldDiscriminator: UInt64,
                                newKey: Key,
                                newDiscriminator: UInt64) -> UnsafeRawPointer {
    let bitPattern = UInt64(Builtin.int_ptrauth_resign(
      UInt64(UInt(bitPattern: pointer))._value,
      oldKey._value._value,
      oldDiscriminator._value,
      newKey._value._value,
      newDiscriminator._value))

    return unsafe UnsafeRawPointer(bitPattern:
      UInt(truncatingIfNeeded: bitPattern)).unsafelyUnwrapped
  }

  /// Get the type-specific discriminator for a function type.
  @_semantics("no.preserve.debugger") // Don't keep the generic version alive
  @_transparent
  static func discriminator<T>(for type: T.Type) -> UInt64 {
    return UInt64(Builtin.typePtrAuthDiscriminator(type))
  }

#elseif _ptrauth(_none)
  /// Blend a pointer and a small integer to form a new extra-data
  /// discriminator.  Not all bits of the inputs are guaranteed to
  /// contribute to the result.
  @_transparent
  static func blend(pointer _: UnsafeRawPointer,
                    discriminator _: UInt64) -> UInt64{
    return 0
  }

  /// Sign an unauthenticated pointer.
  @_transparent
  static func sign(pointer: UnsafeRawPointer,
                   key: Key,
                   discriminator: UInt64) -> UnsafeRawPointer {
    return unsafe pointer
  }

  /// Authenticate a pointer using one scheme and resign it using another.
  @_transparent
  static func authenticateAndResign(pointer: UnsafeRawPointer,
                                oldKey: Key,
                                oldDiscriminator: UInt64,
                                newKey: Key,
                                newDiscriminator: UInt64) -> UnsafeRawPointer {
    return unsafe pointer
  }

  /// Get the type-specific discriminator for a function type.
  @_transparent
  static func discriminator<T>(for type: T.Type) -> UInt64 {
    return 0
  }
#else
  #error("Unsupported ptrauth scheme")
#endif
}

// Helpers for working with authenticated function pointers.

extension UnsafeRawPointer {
  /// Load a function pointer from memory that has been authenticated
  /// specifically for its given address.
  @_semantics("no.preserve.debugger") // Don't keep the generic version alive
  @_transparent
  internal func _loadAddressDiscriminatedFunctionPointer<T>(
    fromByteOffset offset: Int = 0,
    as type: T.Type,
    discriminator: UInt64
  ) -> T {
    let src = unsafe self + offset

    let srcDiscriminator = unsafe _PtrAuth.blend(pointer: src,
                                          discriminator: discriminator)
    let ptr = unsafe src.load(as: UnsafeRawPointer.self)
    let resigned = unsafe _PtrAuth.authenticateAndResign(
      pointer: ptr,
      oldKey: .processIndependentCode,
      oldDiscriminator: srcDiscriminator,
      newKey: .processIndependentCode,
      newDiscriminator: _PtrAuth.discriminator(for: type))

    return unsafe unsafeBitCast(resigned, to: type)
  }

  @_semantics("no.preserve.debugger") // Don't keep the generic version alive
  @_transparent
  internal func _loadAddressDiscriminatedFunctionPointer<T>(
    fromByteOffset offset: Int = 0,
    as type: Optional<T>.Type,
    discriminator: UInt64
  ) -> Optional<T> {
    let src = unsafe self + offset

    let srcDiscriminator = unsafe _PtrAuth.blend(pointer: src,
                                          discriminator: discriminator)
    guard let ptr = unsafe src.load(as: Optional<UnsafeRawPointer>.self) else {
      return nil
    }
    let resigned = unsafe _PtrAuth.authenticateAndResign(
      pointer: ptr,
      oldKey: .processIndependentCode,
      oldDiscriminator: srcDiscriminator,
      newKey: .processIndependentCode,
      newDiscriminator: _PtrAuth.discriminator(for: T.self))

    return unsafe .some(unsafeBitCast(resigned, to: T.self))
  }

}

extension UnsafeMutableRawPointer {
  /// Copy a function pointer from memory that has been authenticated
  /// specifically for its given address.
  internal func _copyAddressDiscriminatedFunctionPointer(
    from src: UnsafeRawPointer,
    discriminator: UInt64
  ) {
    if unsafe src == UnsafeRawPointer(self) { return }

    let srcDiscriminator = unsafe _PtrAuth.blend(pointer: src,
                                          discriminator: discriminator)
    let destDiscriminator = unsafe _PtrAuth.blend(pointer: self,
                                           discriminator: discriminator)

    let ptr = unsafe src.load(as: UnsafeRawPointer.self)
    let resigned = unsafe _PtrAuth.authenticateAndResign(
      pointer: ptr,
      oldKey: .processIndependentCode,
      oldDiscriminator: srcDiscriminator,
      newKey: .processIndependentCode,
      newDiscriminator: destDiscriminator)

    unsafe storeBytes(of: resigned, as: UnsafeRawPointer.self)
  }

  @_transparent
  internal func _storeFunctionPointerWithAddressDiscrimination(
    _ unsignedPointer: UnsafeRawPointer,
    discriminator: UInt64
  ) {
    let destDiscriminator = unsafe _PtrAuth.blend(pointer: self,
                                           discriminator: discriminator)
    let signed = unsafe _PtrAuth.sign(pointer: unsignedPointer,
                               key: .processIndependentCode,
                               discriminator: destDiscriminator)
    unsafe storeBytes(of: signed, as: UnsafeRawPointer.self)
  }
}
