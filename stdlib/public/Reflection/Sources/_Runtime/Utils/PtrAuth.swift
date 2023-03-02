//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

@usableFromInline
internal enum PtrAuth {}

extension PtrAuth {
  @usableFromInline
  internal enum Key {
    @_transparent
    @inline(__always)
    @inlinable
    static var processIndependentCode: Int32 {
      0x0
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var processDependentCode: Int32 {
      0x1
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var processIndependentData: Int32 {
      0x2
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var processDependentData: Int32 {
      0x3
    }
  }
}

extension PtrAuth {
  @usableFromInline
  internal enum SpecialDiscriminator {
    @_transparent
    @inline(__always)
    @inlinable
    static var nonUniqueExtendedExistentialTypeShape: UInt64 {
      0xE798
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var objcIsa: UInt64 {
      0x6AE1
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var objcSuperclass: UInt64 {
      0xB5AB
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var typeDescriptor: UInt64 {
      0xAE86
    }
    
//===----------------------------------------------------------------------===//
// Value Witness Table Discriminators
//===----------------------------------------------------------------------===//
    
    @_transparent
    @inline(__always)
    @inlinable
    static var initializeBufferWithCopyOfBuffer: UInt64 {
      0xDA4A
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var destroy: UInt64 {
      0x04F8
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var initializeWithCopy: UInt64 {
      0xE3BA
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var assignWithCopy: UInt64 {
      0x8751
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var initializeWithTake: UInt64 {
      0x48D8
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var assignWithTake: UInt64 {
      0xEFDA
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var getEnumTagSinglePayload: UInt64 {
      0x60F0
    }
    
    @_transparent
    @inline(__always)
    @inlinable
    static var storeEnumTagSinglePayload: UInt64 {
      0xA0D1
    }
  }
}

extension PtrAuth {
#if _ptrauth(_arm64e)
  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signDescriptor<T: PublicLayout>(_ descriptor: T) -> T {
    let signedBitPattern = UInt64(Builtin.int_ptrauth_sign(
      descriptor.ptr.bitPattern._value,
      Key.processIndependentData._value,
      SpecialDiscriminator.typeDescriptor._value
    ))
    
    return T(signedBitPattern.rawPointer)
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signNonUniqueExtendedExistentialShape(
    _ shape: ExtendedExistentialShape
  ) -> ExtendedExistentialShape {
    let signedBitPattern = UInt64(Builtin.int_ptrauth_sign(
      shape.ptr.bitPattern._value,
      Key.processIndependentData._value,
      SpecialDiscriminator.nonUniqueExtendedExistentialTypeShape._value
    ))
    
    return ExtendedExistentialShape(signedBitPattern.rawPointer)
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signObjcISA(_ isa: Metadata) -> Metadata {
    let signedBitPattern = UInt64(Builtin.int_ptrauth_sign(
      isa.ptr.bitPattern._value,
      Key.processIndependentData._value,
      SpecialDiscriminator.objcIsa._value
    ))
    
    return Metadata(signedBitPattern.rawPointer)
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signSuperclass(_ superclass: Metadata) -> Metadata {
    let signedBitPattern = UInt64(Builtin.int_ptrauth_sign(
      superclass.ptr.bitPattern._value,
      Key.processIndependentData._value,
      SpecialDiscriminator.objcSuperclass._value
    ))
    
    return Metadata(signedBitPattern.rawPointer)
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFn0(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    let signedBitPattern = UInt64(Builtin.int_ptrauth_sign(
      fnPtr.bitPattern._value,
      Key.processIndependentCode._value,
      Builtin.typePtrAuthDiscriminator(
        Metadata.AccessFunction.AccessFn0.self
      )
    ))

    return signedBitPattern.rawPointer
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFn1(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    let signedBitPattern = UInt64(Builtin.int_ptrauth_sign(
      fnPtr.bitPattern._value,
      Key.processIndependentCode._value,
      Builtin.typePtrAuthDiscriminator(
        Metadata.AccessFunction.AccessFn1.self
      )
    ))

    return signedBitPattern.rawPointer
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFn2(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    let signedBitPattern = UInt64(Builtin.int_ptrauth_sign(
      fnPtr.bitPattern._value,
      Key.processIndependentCode._value,
      Builtin.typePtrAuthDiscriminator(
        Metadata.AccessFunction.AccessFn2.self
      )
    ))

    return signedBitPattern.rawPointer
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFn3(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    let signedBitPattern = UInt64(Builtin.int_ptrauth_sign(
      fnPtr.bitPattern._value,
      Key.processIndependentCode._value,
      Builtin.typePtrAuthDiscriminator(
        Metadata.AccessFunction.AccessFn3.self
      )
    ))

    return signedBitPattern.rawPointer
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFnMany(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    let signedBitPattern = UInt64(Builtin.int_ptrauth_sign(
      fnPtr.bitPattern._value,
      Key.processIndependentCode._value,
      Builtin.typePtrAuthDiscriminator(
        Metadata.AccessFunction.AccessFnMany.self
      )
    ))

    return signedBitPattern.rawPointer
  }
#else
  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signDescriptor<T: PublicLayout>(_ descriptor: T) -> T {
    descriptor
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signNonUniqueExtendedExistentialShape(
    _ shape: ExtendedExistentialShape
  ) -> ExtendedExistentialShape {
    shape
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signObjcISA(_ isa: Metadata) -> Metadata {
    isa
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signSuperclass(_ superclass: Metadata) -> Metadata {
    superclass
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFn0(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    fnPtr
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFn1(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    fnPtr
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFn2(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    fnPtr
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFn3(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    fnPtr
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  static func signAccessFnMany(_ fnPtr: UnsafeRawPointer) -> UnsafeRawPointer {
    fnPtr
  }
#endif
}

extension UnsafeRawPointer {
#if _ptrauth(_arm64e)
  @inlinable
  func signedVWTFunc(
    _ discrim: UInt64,
    _ typeDiscrim: UInt64
  ) -> UnsafeRawPointer {
    let blend = UInt64(Builtin.int_ptrauth_blend(
      bitPattern._value,
      discrim._value
    ))
    
    let ptr = load(as: UnsafeRawPointer.self)
    
    let resigned = UInt64(Builtin.int_ptrauth_resign(
      ptr.bitPattern._value,
      PtrAuth.Key.processIndependentCode._value,
      blend._value,
      PtrAuth.Key.processIndependentCode._value,
      typeDiscrim._value
    ))
    
    return resigned.rawPointer
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTInitializeBufferWithCopyOfBuffer: ValueWitnessTable.InitializeBufferWithCopyOfBuffer {
    unsafeBitCast(signedVWTFunc(
      PtrAuth.SpecialDiscriminator.initializeBufferWithCopyOfBuffer,
      UInt64(Builtin.typePtrAuthDiscriminator(
        ValueWitnessTable.InitializeBufferWithCopyOfBuffer.self
      ))
    ))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTDestroy: ValueWitnessTable.Destroy {
    unsafeBitCast(signedVWTFunc(
      PtrAuth.SpecialDiscriminator.destroy,
      UInt64(Builtin.typePtrAuthDiscriminator(
        ValueWitnessTable.Destroy.self
      ))
    ))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTInitializeWithCopy: ValueWitnessTable.InitializeWithCopy {
    unsafeBitCast(signedVWTFunc(
      PtrAuth.SpecialDiscriminator.initializeWithCopy,
      UInt64(Builtin.typePtrAuthDiscriminator(
        ValueWitnessTable.InitializeWithCopy.self
      ))
    ))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTAssignWithCopy: ValueWitnessTable.AssignWithCopy {
    unsafeBitCast(signedVWTFunc(
      PtrAuth.SpecialDiscriminator.assignWithCopy,
      UInt64(Builtin.typePtrAuthDiscriminator(
        ValueWitnessTable.AssignWithCopy.self
      ))
    ))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTInitializeWithTake: ValueWitnessTable.InitializeWithTake {
    unsafeBitCast(signedVWTFunc(
      PtrAuth.SpecialDiscriminator.initializeWithTake,
      UInt64(Builtin.typePtrAuthDiscriminator(
        ValueWitnessTable.InitializeWithTake.self
      ))
    ))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTAssignWithTake: ValueWitnessTable.AssignWithTake {
    unsafeBitCast(signedVWTFunc(
      PtrAuth.SpecialDiscriminator.assignWithTake,
      UInt64(Builtin.typePtrAuthDiscriminator(
        ValueWitnessTable.AssignWithTake.self
      ))
    ))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTGetEnumTagSinglePayload: ValueWitnessTable.GetEnumTagSinglePayload {
    unsafeBitCast(signedVWTFunc(
      PtrAuth.SpecialDiscriminator.getEnumTagSinglePayload,
      UInt64(Builtin.typePtrAuthDiscriminator(
        ValueWitnessTable.GetEnumTagSinglePayload.self
      ))
    ))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTStoreEnumTagSinglePayload: ValueWitnessTable.StoreEnumTagSinglePayload {
    unsafeBitCast(signedVWTFunc(
      PtrAuth.SpecialDiscriminator.storeEnumTagSinglePayload,
      UInt64(Builtin.typePtrAuthDiscriminator(
        ValueWitnessTable.StoreEnumTagSinglePayload.self
      ))
    ))
  }
#else
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTInitializeBufferWithCopyOfBuffer: ValueWitnessTable.InitializeBufferWithCopyOfBuffer {
    unsafeBitCast(unprotectedLoad(as: UnsafeRawPointer.self))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTDestroy: ValueWitnessTable.Destroy {
    unsafeBitCast(unprotectedLoad(as: UnsafeRawPointer.self))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTInitializeWithCopy: ValueWitnessTable.InitializeWithCopy {
    unsafeBitCast(unprotectedLoad(as: UnsafeRawPointer.self))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTAssignWithCopy: ValueWitnessTable.AssignWithCopy {
    unsafeBitCast(unprotectedLoad(as: UnsafeRawPointer.self))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTInitializeWithTake: ValueWitnessTable.InitializeWithTake {
    unsafeBitCast(unprotectedLoad(as: UnsafeRawPointer.self))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTAssignWithTake: ValueWitnessTable.AssignWithTake {
    unsafeBitCast(unprotectedLoad(as: UnsafeRawPointer.self))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTGetEnumTagSinglePayload: ValueWitnessTable.GetEnumTagSinglePayload {
    unsafeBitCast(unprotectedLoad(as: UnsafeRawPointer.self))
  }
  
  @available(SwiftStdlib 5.9, *)
  @inlinable
  var signedVWTStoreEnumTagSinglePayload: ValueWitnessTable.StoreEnumTagSinglePayload {
    unsafeBitCast(unprotectedLoad(as: UnsafeRawPointer.self))
  }
#endif
}
