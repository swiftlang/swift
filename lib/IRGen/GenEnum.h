//===--- GenEnum.h - Swift IR Generation For 'enum' Types -------* C++ *-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for algebraic data types (ADTs,
//  or 'enum' types) in Swift.  This includes creating the IR type as
//  well as emitting the basic access operations.
//
//  The current scheme is that all such types with are represented
//  with an initial word indicating the variant, followed by an enum
//  of all the possibilities.  This is obviously completely acceptable
//  to everyone and will not benefit from further refinement.
//
//  As a completely unimportant premature optimization, we do emit
//  types with only a single variant as simple structs wrapping that
//  variant.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_IRGen_GenEnum_H__
#define __SWIFT_IRGen_GenEnum_H__

namespace llvm {
  class BasicBlock;
  class Value;
  class Type;
  
  class BitVector;
}

namespace swift {
  class EnumElementDecl;
  
namespace irgen {
  class IRGenFunction;
  
/// Utility class for packing enum payloads. The payload of a fixed-size, non-
/// trivial enum is represented as an LLVM integer type large enough to
/// hold the largest member of the enum. This class collects individual
/// scalar values, such as from an explosion, into an enum payload.
class PackEnumPayload {
  IRGenFunction &IGF;
  unsigned packedBits = 0;
  unsigned bitSize;
  llvm::Value *packedValue = nullptr;

public:
  PackEnumPayload(IRGenFunction &IGF, unsigned bitSize);
  
  /// Insert a value into the packed value after the previously inserted value,
  /// or at offset zero if this is the first value.
  void add(llvm::Value *v);
  
  /// Insert a value into the packed value at a specific offset.
  void addAtOffset(llvm::Value *v, unsigned bitOffset);
  
  /// Combine a partially packed payload at a different offset into our packing.
  void combine(llvm::Value *v);
  
  /// Get the packed value.
  llvm::Value *get();
  
  /// Get an empty payload value of the given bit size.
  static llvm::Value *getEmpty(IRGenModule &IGM, unsigned bitSize);
};

/// Utility class for packing enum payloads. The payload of a fixed-size, non-
/// trivial enum is represented as an LLVM integer type large enough to
/// hold the largest member of the enum. This class extracts individual
/// scalar values from an enum payload.
class UnpackEnumPayload {
  IRGenFunction &IGF;
  llvm::Value *packedValue;
  unsigned unpackedBits = 0;

public:
  UnpackEnumPayload(IRGenFunction &IGF, llvm::Value *packedValue);
  
  /// Extract a value of the given type that was packed after the previously
  /// claimed value, or at offset zero if this is the first claimed value.
  llvm::Value *claim(llvm::Type *ty);
  
  /// Claim a value at a specific offset inside the value.
  llvm::Value *claimAtOffset(llvm::Type *ty, unsigned bitOffset);
};

/// \brief Emit the dispatch branch(es) for a loadable enum.
void emitSwitchLoadableEnumDispatch(IRGenFunction &IGF,
                                     SILType enumTy,
                                     Explosion &enumValue,
                                     ArrayRef<std::pair<EnumElementDecl*,
                                                      llvm::BasicBlock*>> dests,
                                     llvm::BasicBlock *defaultDest);

/// \brief Emit the dispatch branch(es) for an address-only enum.
void emitSwitchAddressOnlyEnumDispatch(IRGenFunction &IGF,
                                        SILType enumTy,
                                        Address enumAddr,
                                        ArrayRef<std::pair<EnumElementDecl*,
                                                           llvm::BasicBlock*>> dests,
                                        llvm::BasicBlock *defaultDest);

/// \brief Injects a case and its associated data, if any, into a loadable enum
/// value.
void emitInjectLoadableEnum(IRGenFunction &IGF,
                             SILType enumTy,
                             EnumElementDecl *theCase,
                             Explosion &data,
                             Explosion &out);
  
/// \brief Extracts the associated data for an enum case. This is an unchecked
/// operation; the input enum value must be of the given case.
void emitProjectLoadableEnum(IRGenFunction &IGF,
                              SILType enumTy,
                              Explosion &inData,
                              EnumElementDecl *theCase,
                              Explosion &out);

/// \brief Projects the address of the associated data for a case inside a
/// enum, to which a new data value can be stored.
Address emitProjectEnumAddressForStore(IRGenFunction &IGF,
                                        SILType enumTy,
                                        Address enumAddr,
                                        EnumElementDecl *theCase);

/// \brief Projects the address of the associated data for a case inside a
/// enum, clearing any tag bits interleaved into the data area, so that the
/// value inside can be loaded. Does not check that the enum has a value of the
/// given case.
Address emitDestructiveProjectEnumAddressForLoad(IRGenFunction &IGF,
                                                  SILType enumTy,
                                                  Address enumAddr,
                                                  EnumElementDecl *theCase);

/// \brief Stores the tag bits for an enum case to the given address, overlaying
/// the data (if any) stored there.
void emitStoreEnumTagToAddress(IRGenFunction &IGF,
                                SILType enumTy,
                                Address enumAddr,
                                EnumElementDecl *theCase);
  
/// Convert a BitVector to an APInt.
APInt getAPIntFromBitVector(const llvm::BitVector &bits);
  
/// Interleave the occupiedValue and spareValue bits, taking a bit from one
/// or the other at each position based on the spareBits mask.
llvm::ConstantInt *
interleaveSpareBits(IRGenModule &IGM, const llvm::BitVector &spareBits,
                    unsigned bits, unsigned spareValue, unsigned occupiedValue);

/// Gather spare bits into the low bits of a smaller integer value.
llvm::Value *emitGatherSpareBits(IRGenFunction &IGF,
                                 const llvm::BitVector &spareBitMask,
                                 llvm::Value *spareBits,
                                 unsigned resultLowBit,
                                 unsigned resultBitWidth);
/// Scatter spare bits from the low bits of a smaller integer value.
llvm::Value *emitScatterSpareBits(IRGenFunction &IGF,
                                  const llvm::BitVector &spareBitMask,
                                  llvm::Value *packedBits,
                                  unsigned packedLowBit);

}
}

#endif
