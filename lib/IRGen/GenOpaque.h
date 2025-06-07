//===--- GenOpaque.h - Swift IR generation for opaque values ----*- C++ -*-===//
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
//
//  This file provides a private interface for interacting with opaque
//  values and their value witnesses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENOPAQUE_H
#define SWIFT_IRGEN_GENOPAQUE_H

namespace llvm {
  class Type;
  class Value;
}

namespace swift {
namespace irgen {
  class Address;
  class IRGenFunction;
  class IRGenModule;
  class TypeInfo;
  enum class ValueWitness : unsigned;
  class WitnessIndex;

  /// Return the size of a fixed buffer.
  Size getFixedBufferSize(IRGenModule &IGM);

  /// Return the alignment of a fixed buffer.
  Alignment getFixedBufferAlignment(IRGenModule &IGM);

  /// Given a witness table (protocol or value), return the address of the slot
  /// for one of the witnesses.
  /// If \p areEntriesRelative is true we are emitting code for a relative
  /// protocol witness table.
  Address slotForLoadOfOpaqueWitness(IRGenFunction &IGF, llvm::Value *table,
                                     WitnessIndex index,
                                     bool areEntriesRelative = false);

  /// Given a witness table (protocol or value), load one of the
  /// witnesses.
  ///
  /// The load is marked invariant. This should not be used in contexts where
  /// the referenced witness table is still undergoing initialization.
  llvm::Value *emitInvariantLoadOfOpaqueWitness(IRGenFunction &IGF,
                                                bool isProtocolWitness,
                                                llvm::Value *table,
                                                WitnessIndex index,
                                                llvm::Value **slot = nullptr);

  /// Given a witness table (protocol or value), load one of the
  /// witnesses.
  ///
  /// The load is marked invariant. This should not be used in contexts where
  /// the referenced witness table is still undergoing initialization.
  llvm::Value *emitInvariantLoadOfOpaqueWitness(IRGenFunction &IGF,
                                                bool isProtocolWitness,
                                                llvm::Value *table,
                                                llvm::Value *index,
                                                llvm::Value **slot = nullptr);

  /// Emit a call to do an 'initializeBufferWithCopyOfBuffer' operation.
  llvm::Value *emitInitializeBufferWithCopyOfBufferCall(IRGenFunction &IGF,
                                                        llvm::Value *metadata,
                                                        Address destBuffer,
                                                        Address srcBuffer);

  /// Emit a call to do an 'initializeBufferWithCopyOfBuffer' operation.
  llvm::Value *emitInitializeBufferWithCopyOfBufferCall(IRGenFunction &IGF,
                                                        SILType T,
                                                        Address destBuffer,
                                                        Address srcBuffer);

  /// Emit a call to do an 'initializeWithCopy' operation.
  void emitInitializeWithCopyCall(IRGenFunction &IGF,
                                  SILType T,
                                  Address destObject,
                                  Address srcObject);
  llvm::Value *emitInitializeWithCopyCall(IRGenFunction &IGF,
                                          llvm::Value *metadata, Address dest,
                                          Address src);

  /// Emit a call to do an 'initializeArrayWithCopy' operation.
  void emitInitializeArrayWithCopyCall(IRGenFunction &IGF,
                                       SILType T,
                                       Address destObject,
                                       Address srcObject,
                                       llvm::Value *count);

  /// Emit a call to do an 'initializeWithTake' operation.
  void emitInitializeWithTakeCall(IRGenFunction &IGF,
                                  SILType T,
                                  Address destObject,
                                  Address srcObject);
  llvm::Value *emitInitializeWithTakeCall(IRGenFunction &IGF,
                                          llvm::Value *metadata, Address dest,
                                          Address src);

  /// Emit a call to do an 'initializeArrayWithTakeNoAlias' operation.
  void emitInitializeArrayWithTakeNoAliasCall(IRGenFunction &IGF, SILType T,
                                              Address destObject,
                                              Address srcObject,
                                              llvm::Value *count);

  /// Emit a call to do an 'initializeArrayWithTakeFrontToBack' operation.
  void emitInitializeArrayWithTakeFrontToBackCall(IRGenFunction &IGF,
                                                  SILType T,
                                                  Address destObject,
                                                  Address srcObject,
                                                  llvm::Value *count);

  /// Emit a call to do an 'initializeArrayWithTakeBackToFront' operation.
  void emitInitializeArrayWithTakeBackToFrontCall(IRGenFunction &IGF,
                                                  SILType T,
                                                  Address destObject,
                                                  Address srcObject,
                                                  llvm::Value *count);

  /// Emit a call to do an 'assignWithCopy' operation.
  void emitAssignWithCopyCall(IRGenFunction &IGF,
                              SILType T,
                              Address destObject,
                              Address srcObject);
  void emitAssignWithCopyCall(IRGenFunction &IGF,
                              llvm::Value *metadata,
                              Address destObject,
                              Address srcObject);

  /// Emit a call to do an 'assignArrayWithCopyNoAlias' operation.
  void emitAssignArrayWithCopyNoAliasCall(IRGenFunction &IGF, SILType T,
                                          Address destObject, Address srcObject,
                                          llvm::Value *count);

  /// Emit a call to do an 'assignArrayWithCopyFrontToBack' operation.
  void emitAssignArrayWithCopyFrontToBackCall(IRGenFunction &IGF, SILType T,
                                              Address destObject,
                                              Address srcObject,
                                              llvm::Value *count);

  /// Emit a call to do an 'assignArrayWithCopyBackToFront' operation.
  void emitAssignArrayWithCopyBackToFrontCall(IRGenFunction &IGF, SILType T,
                                              Address destObject,
                                              Address srcObject,
                                              llvm::Value *count);

  /// Emit a call to do an 'assignWithTake' operation.
  void emitAssignWithTakeCall(IRGenFunction &IGF,
                              SILType T,
                              Address destObject,
                              Address srcObject);

  /// Emit a call to do an 'assignArrayWithTake' operation.
  void emitAssignArrayWithTakeCall(IRGenFunction &IGF, SILType T,
                                   Address destObject, Address srcObject,
                                   llvm::Value *count);

  /// Emit a call to do a 'destroy' operation.
  void emitDestroyCall(IRGenFunction &IGF,
                       SILType T,
                       Address object);

  void emitDestroyCall(IRGenFunction &IGF, llvm::Value *metadata,
                       Address object);

  /// Emit a call to do a 'destroyArray' operation.
  void emitDestroyArrayCall(IRGenFunction &IGF,
                            SILType T,
                            Address object,
                            llvm::Value *count);

  /// Emit a call to the 'getEnumTagSinglePayload' operation.
  llvm::Value *emitGetEnumTagSinglePayloadCall(IRGenFunction &IGF, SILType T,
                                               llvm::Value *numEmptyCases,
                                               Address destObject);

  /// Emit a call to the 'storeEnumTagSinglePayload' operation.
  void emitStoreEnumTagSinglePayloadCall(IRGenFunction &IGF, SILType T,
                                         llvm::Value *whichCase,
                                         llvm::Value *numEmptyCases,
                                         Address destObject);

  /// Emit a call to the 'getEnumTag' operation.
  llvm::Value *emitGetEnumTagCall(IRGenFunction &IGF,
                                  SILType T,
                                  Address srcObject);

  /// Emit a call to the 'destructiveProjectEnumData' operation.
  /// The type must be dynamically known to have enum witnesses.
  void emitDestructiveProjectEnumDataCall(IRGenFunction &IGF,
                                          SILType T,
                                          Address srcObject);

  /// Emit a call to the 'destructiveInjectEnumTag' operation.
  /// The type must be dynamically known to have enum witnesses.
  void emitDestructiveInjectEnumTagCall(IRGenFunction &IGF,
                                        SILType T,
                                        llvm::Value *tag,
                                        Address srcObject);

  /// Emit a load of the 'size' value witness.
  llvm::Value *emitLoadOfSize(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'stride' value witness.
  llvm::Value *emitLoadOfStride(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'alignmentMask' value witness.
  llvm::Value *emitLoadOfAlignmentMask(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'isTriviallyDestroyable' value witness.
  llvm::Value *emitLoadOfIsTriviallyDestroyable(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'isBitwiseTakable' value witness.
  llvm::Value *emitLoadOfIsBitwiseTakable(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'isInline' value witness.
  llvm::Value *emitLoadOfIsInline(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'extraInhabitantCount' value witness.
  llvm::Value *emitLoadOfExtraInhabitantCount(IRGenFunction &IGF, SILType T);

  /// Emit a stored to the 'extraInhabitantCount' value witness.
  void emitStoreOfExtraInhabitantCount(IRGenFunction &IGF, llvm::Value *val,
                                       llvm::Value *metadata);

  /// Returns the IsInline flag and the loaded flags value.
  std::pair<llvm::Value *, llvm::Value *>
  emitLoadOfIsInline(IRGenFunction &IGF, llvm::Value *metadata);

  /// Emits the alignment mask value from a loaded flags value.
  llvm::Value *emitAlignMaskFromFlags(IRGenFunction &IGF, llvm::Value *flags);

  llvm::Value *emitLoadOfSize(IRGenFunction &IGF, llvm::Value *metadata);

  /// Allocate/project/allocate memory for a value of the type in the fixed size
  /// buffer.
  Address emitAllocateValueInBuffer(IRGenFunction &IGF,
                               SILType type,
                               Address buffer);
  Address emitProjectValueInBuffer(IRGenFunction &IGF,
                              SILType type,
                              Address buffer);

  using GetExtraInhabitantTagEmitter =
    llvm::function_ref<llvm::Value*(IRGenFunction &IGF,
                                    Address addr,
                                    llvm::Value *xiCount)>;

  llvm::Constant *
  getOrCreateGetExtraInhabitantTagFunction(IRGenModule &IGM,
                                           SILType objectType,
                                           const TypeInfo &objectTI,
                                           GetExtraInhabitantTagEmitter emit);

  llvm::Value *
  emitGetEnumTagSinglePayloadGenericCall(IRGenFunction &IGF,
                                         SILType payloadType,
                                         const TypeInfo &payloadTI,
                                         llvm::Value *numExtraCases,
                                         Address address,
                                         GetExtraInhabitantTagEmitter emit);

  using StoreExtraInhabitantTagEmitter =
    llvm::function_ref<void(IRGenFunction &IGF,
                            Address addr,
                            llvm::Value *tag,
                            llvm::Value *xiCount)>;

  llvm::Constant *
  getOrCreateStoreExtraInhabitantTagFunction(IRGenModule &IGM,
                                             SILType objectType,
                                             const TypeInfo &objectTI,
                                        StoreExtraInhabitantTagEmitter emit);

  void emitStoreEnumTagSinglePayloadGenericCall(IRGenFunction &IGF,
                                                SILType payloadType,
                                                const TypeInfo &payloadTI,
                                                llvm::Value *index,
                                                llvm::Value *numExtraCases,
                                                Address address,
                                           StoreExtraInhabitantTagEmitter emit);
} // end namespace irgen
} // end namespace swift

#endif
