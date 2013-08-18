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

namespace llvm {
  class Value;
  class Type;
}

namespace swift {
namespace irgen {
  class IRGenFunction;
  
/// Utility class for packing union payloads. The payload of a fixed-size, non-
/// trivial union is represented as an LLVM integer type large enough to
/// hold the largest member of the union. This class collects individual
/// scalar values, such as from an explosion, into a union payload.
class PackUnionPayload {
  IRGenFunction &IGF;
  unsigned packedBits = 0;
  // bitSize is inhabited if packedBits is zero; otherwise, packedValue is.
  union {
    unsigned bitSize;
    llvm::Value *packedValue;
  };

public:
  PackUnionPayload(IRGenFunction &IGF, unsigned bitSize);
  
  /// Insert a value into the packed value.
  void add(llvm::Value *v);
  
  /// Insert zero padding bits between values.
  void zeroPad(unsigned bits);
  
  /// Get the packed value.
  llvm::Value *get() const;
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
  
  /// Extract a value of the given type from the next bitSize-sized range of
  /// bits in the value.
  llvm::Value *claim(llvm::Type *ty);
  
  /// Skip padding bits.
  void discardPadding(unsigned bits);
};
  
}
}
