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
  enum class ValueWitness : unsigned;
  class WitnessIndex;

  /// Return the size of a fixed buffer.
  Size getFixedBufferSize(IRGenModule &IGM);

  /// Return the alignment of a fixed buffer.
  Alignment getFixedBufferAlignment(IRGenModule &IGM);

  /// Given a witness table (protocol or value), load one of the
  /// witnesses.
  ///
  /// The load is marked invariant. This should not be used in contexts where
  /// the referenced witness table is still undergoing initialization.
  llvm::Value *emitInvariantLoadOfOpaqueWitness(IRGenFunction &IGF,
                                                llvm::Value *table,
                                                WitnessIndex index);

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

  /// Emit a call to do an 'initializeBufferWithTakeOfBuffer' operation.
  llvm::Value *emitInitializeBufferWithTakeOfBufferCall(IRGenFunction &IGF,
                                                        llvm::Value *metadata,
                                                        Address destBuffer,
                                                        Address srcBuffer);

  /// Emit a call to do an 'initializeBufferWithTakeOfBuffer' operation.
  llvm::Value *emitInitializeBufferWithTakeOfBufferCall(IRGenFunction &IGF,
                                                        SILType T,
                                                        Address destBuffer,
                                                        Address srcBuffer);

  /// Emit a call to do an 'allocateBuffer' operation.
  llvm::Value *emitAllocateBufferCall(IRGenFunction &IGF,
                                      SILType T,
                                      Address buffer);

  /// Emit a call to do a 'projectBuffer' operation.
  llvm::Value *emitProjectBufferCall(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     Address buffer);

  /// Emit a call to do a 'projectBuffer' operation.
  llvm::Value *emitProjectBufferCall(IRGenFunction &IGF,
                                     SILType T,
                                     Address buffer);

  /// Emit a call to do an 'initializeWithCopy' operation.
  void emitInitializeWithCopyCall(IRGenFunction &IGF,
                                  SILType T,
                                  Address destObject,
                                  Address srcObject);


  /// Emit a call to do an 'initializeBufferWithCopy' operation.
  llvm::Value *emitInitializeBufferWithCopyCall(IRGenFunction &IGF,
                                                SILType T,
                                                Address destBuffer,
                                                Address srcObject);

  /// Emit a call to do an 'initializeBufferWithTake' operation.
  llvm::Value *emitInitializeBufferWithTakeCall(IRGenFunction &IGF,
                                                SILType T,
                                                Address destBuffer,
                                                Address srcObject);

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

  /// Emit a call to do an 'assignWithTake' operation.
  void emitAssignWithTakeCall(IRGenFunction &IGF,
                              SILType T,
                              Address destObject,
                              Address srcObject);

  /// Emit a call to do a 'destroy' operation.
  void emitDestroyCall(IRGenFunction &IGF,
                       SILType T,
                       Address object);

  /// Emit a call to do a 'destroyArray' operation.
  void emitDestroyArrayCall(IRGenFunction &IGF,
                            SILType T,
                            Address object,
                            llvm::Value *count);

  /// Emit a call to do a 'destroyBuffer' operation.
  void emitDestroyBufferCall(IRGenFunction &IGF,
                             llvm::Value *metadata,
                             Address buffer);
  void emitDestroyBufferCall(IRGenFunction &IGF,
                             SILType T,
                             Address buffer);
  
  /// Emit a call to do a 'deallocateBuffer' operation.
  void emitDeallocateBufferCall(IRGenFunction &IGF,
                                llvm::Value *metadata,
                                Address buffer);
  void emitDeallocateBufferCall(IRGenFunction &IGF,
                                SILType T,
                                Address buffer);
  
  /// Emit a call to the 'getExtraInhabitantIndex' operation.
  /// The type must be dynamically known to have extra inhabitant witnesses.
  llvm::Value *emitGetExtraInhabitantIndexCall(IRGenFunction &IGF,
                                               SILType T,
                                               Address srcObject);
  
  /// Emit a call to the 'storeExtraInhabitant' operation.
  /// The type must be dynamically known to have extra inhabitant witnesses.
  llvm::Value *emitStoreExtraInhabitantCall(IRGenFunction &IGF,
                                            SILType T,
                                            llvm::Value *index,
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
                                        unsigned tag,
                                        Address srcObject);

  /// Emit a load of the 'size' value witness.
  llvm::Value *emitLoadOfSize(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'stride' value witness.
  llvm::Value *emitLoadOfStride(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'alignmentMask' value witness.
  llvm::Value *emitLoadOfAlignmentMask(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'isPOD' value witness.
  llvm::Value *emitLoadOfIsPOD(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'isBitwiseTakable' value witness.
  llvm::Value *emitLoadOfIsBitwiseTakable(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'isInline' value witness.
  llvm::Value *emitLoadOfIsInline(IRGenFunction &IGF, SILType T);

  /// Emit a load of the 'hasExtraInhabitants' value witness.
  llvm::Value *emitLoadOfHasExtraInhabitants(IRGenFunction &IGF, SILType T);
  
  /// Emit a load of the 'extraInhabitantCount' value witness.
  /// The type must be dynamically known to have extra inhabitant witnesses.
  llvm::Value *emitLoadOfExtraInhabitantCount(IRGenFunction &IGF, SILType T);

  /// Emit a dynamic alloca call to allocate enough memory to hold an object of
  /// type 'T' and an optional llvm.stackrestore point if 'isInEntryBlock' is
  /// false.
  struct DynamicAlloca {
    llvm::Value *Alloca;
    llvm::Value *SavedSP;
    DynamicAlloca(llvm::Value *A, llvm::Value *SP) : Alloca(A), SavedSP(SP) {}
  };
  DynamicAlloca emitDynamicAlloca(IRGenFunction &IGF, SILType T,
                                  bool isInEntryBlock);

  /// Deallocate dynamic alloca's memory if the stack address has an SP restore
  /// point associated with it.
  void emitDeallocateDynamicAlloca(IRGenFunction &IGF, StackAddress address);

} // end namespace irgen
} // end namespace swift

#endif
