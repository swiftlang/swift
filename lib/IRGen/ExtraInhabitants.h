//===--- ExtraInhabitants.h - Extra inhabitant routines ---------*- C++ -*-===//
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
// This file defines routines for working with extra inhabitants.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_EXTRAINHABITANTS_H
#define SWIFT_IRGEN_EXTRAINHABITANTS_H

#include "IRGen.h"

namespace llvm {
class APInt;
class ConstantInt;
class Value;
}

namespace swift {
namespace irgen {

class Address;
class Alignment;
class IRGenFunction;
class IRGenModule;

/// Whether the zero pointer is a valid value (i.e. not a valid
/// extra inhabitant) of a particular pointer type.
enum IsNullable_t: bool {
  IsNotNullable = false,
  IsNullable = true
};

/// Information about a particular pointer type.
struct PointerInfo {
  Alignment PointeeAlign;
  uint8_t NumReservedLowBits;
  IsNullable_t Nullable;

  static PointerInfo forHeapObject(const IRGenModule &IGM);
  static PointerInfo forFunction(const IRGenModule &IGM);
  static PointerInfo forAligned(Alignment pointeeAlign) {
    return {pointeeAlign, 0, IsNotNullable};
  }

  PointerInfo withNullable(IsNullable_t nullable) const {
    return {PointeeAlign, NumReservedLowBits, nullable};
  }

  /// Return the number of extra inhabitant representations for
  /// pointers with these properties: i.e. the number of values
  /// that do not collide with any valid pointers.
  unsigned getExtraInhabitantCount(const IRGenModule &IGM) const;


  /// Return an indexed extra inhabitant constant for a pointer
  /// with these properties.
  ///
  /// If the pointer appears within a larger aggregate, the
  /// 'bits' and 'offset' arguments can be used to position
  /// the inhabitant within the larger integer constant.
  llvm::APInt getFixedExtraInhabitantValue(const IRGenModule &IGM,
                                           unsigned bits,
                                           unsigned index,
                                           unsigned offset) const;

  /// Given the address of storage for a pointer with these
  /// properties, return the extra inhabitant index of the
  /// value, or -1 if the value is a valid pointer.  Always
  /// produces an i32. 
  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                       Address src) const;

  /// Store an extra inhabitant representation for the given
  /// dynamic extra inhabitant index into the given storage.
  void storeExtraInhabitant(IRGenFunction &IGF,
                            llvm::Value *index,
                            Address dest) const;
};

/*****************************************************************************/

/// \group Extra inhabitants of heap object pointers.

/// Return the number of extra inhabitant representations for heap objects,
/// that is, the number of invalid heap object pointer values that can be used
/// to represent enum tags for enums involving a reference type as a payload.
unsigned getHeapObjectExtraInhabitantCount(const IRGenModule &IGM);
  
/// Return an indexed extra inhabitant constant for a heap object pointer.
///
/// If the pointer appears within a larger aggregate, the 'bits' and 'offset'
/// arguments can be used to position the inhabitant within the larger integer
/// constant.
llvm::APInt getHeapObjectFixedExtraInhabitantValue(const IRGenModule &IGM,
                                                   unsigned bits,
                                                   unsigned index,
                                                   unsigned offset);
  
/// Calculate the index of a heap object extra inhabitant representation stored
/// in memory.
llvm::Value *getHeapObjectExtraInhabitantIndex(IRGenFunction &IGF,
                                               Address src);

/// Calculate an extra inhabitant representation from an index and store it to
/// memory.
void storeHeapObjectExtraInhabitant(IRGenFunction &IGF,
                                    llvm::Value *index,
                                    Address dest);

} // end namespace irgen
} // end namespace swift

#endif
