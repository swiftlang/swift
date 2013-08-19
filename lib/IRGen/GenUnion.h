//===--- GenUnion.h - Swift IR Generation For 'union' Types -------* C++ *-===//
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
//  or 'union' types) in Swift.  This includes creating the IR type as
//  well as emitting the basic access operations.
//
//  The current scheme is that all such types with are represented
//  with an initial word indicating the variant, followed by a union
//  of all the possibilities.  This is obviously completely acceptable
//  to everyone and will not benefit from further refinement.
//
//  As a completely unimportant premature optimization, we do emit
//  types with only a single variant as simple structs wrapping that
//  variant.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_IRGen_GenUnion_H__
#define __SWIFT_IRGen_GenUnion_H__

namespace llvm {
  class BasicBlock;
  class Value;
  class Type;
}

namespace swift {
  class UnionElementDecl;
  
namespace irgen {
  class IRGenFunction;
  
/// Utility class for packing union payloads. The payload of a fixed-size, non-
/// trivial union is represented as an LLVM integer type large enough to
/// hold the largest member of the union. This class collects individual
/// scalar values, such as from an explosion, into a union payload.
class PackUnionPayload {
  IRGenFunction &IGF;
  unsigned packedBits = 0;
  unsigned bitSize;
  llvm::Value *packedValue = nullptr;

public:
  PackUnionPayload(IRGenFunction &IGF, unsigned bitSize);
  
  /// Insert a value into the packed value after the previously inserted value,
  /// or at offset zero if this is the first value.
  void add(llvm::Value *v);
  
  /// Insert a value into the packed value at a specific offset.
  void addAtOffset(llvm::Value *v, unsigned bitOffset);
  
  /// Get the packed value.
  llvm::Value *get();
  
  /// Get an empty payload value of the given bit size.
  static llvm::Value *getEmpty(IRGenModule &IGM, unsigned bitSize);
};

/// Utility class for packing union payloads. The payload of a fixed-size, non-
/// trivial union is represented as an LLVM integer type large enough to
/// hold the largest member of the union. This class extracts individual
/// scalar values from a union payload.
class UnpackUnionPayload {
  IRGenFunction &IGF;
  llvm::Value *packedValue;
  unsigned unpackedBits = 0;

public:
  UnpackUnionPayload(IRGenFunction &IGF, llvm::Value *packedValue);
  
  /// Extract a value of the given type that was packed after the previously
  /// claimed value, or at offset zero if this is the first claimed value.
  llvm::Value *claim(llvm::Type *ty);
  
  /// Claim a value at a specific offset inside the value.
  llvm::Value *claimAtOffset(llvm::Type *ty, unsigned bitOffset);
};

/// \brief Emit the dispatch branch(es) for a loadable union.
/// Produces a mapping of each UnionElementDecl* to its llvm::Value* argument.
void emitSwitchLoadableUnionDispatch(IRGenFunction &IGF,
                                     SILType unionTy,
                                     Explosion &unionValue,
                                     ArrayRef<std::pair<UnionElementDecl*,
                                                      llvm::BasicBlock*>> dests,
                                     llvm::BasicBlock *defaultDest);

}
}

#endif
