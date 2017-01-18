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

namespace llvm {
class APInt;
class ConstantInt;
class Value;
}

namespace swift {
namespace irgen {

class Address;
class IRGenFunction;
class IRGenModule;

/*****************************************************************************/

/// \group Extra inhabitants of heap object pointers.

/// Return the number of extra inhabitant representations for heap objects,
/// that is, the number of invalid heap object pointer values that can be used
/// to represent enum tags for enums involving a reference type as a payload.
unsigned getHeapObjectExtraInhabitantCount(IRGenModule &IGM);
  
/// Return an indexed extra inhabitant constant for a heap object pointer.
///
/// If the pointer appears within a larger aggregate, the 'bits' and 'offset'
/// arguments can be used to position the inhabitant within the larger integer
/// constant.
llvm::APInt getHeapObjectFixedExtraInhabitantValue(IRGenModule &IGM,
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

/*****************************************************************************/

/// \group Extra inhabitants of function pointers.

/// Return the number of extra inhabitant representations for function pointers,
/// that is, the number of invalid function pointer values that can be used
/// to represent enum tags for enums involving a reference type as a payload.
unsigned getFunctionPointerExtraInhabitantCount(IRGenModule &IGM);
  
/// Return an indexed extra inhabitant constant for a function pointer.
///
/// If the pointer appears within a larger aggregate, the 'bits' and 'offset'
/// arguments can be used to position the inhabitant within the larger integer
/// constant.
llvm::APInt getFunctionPointerFixedExtraInhabitantValue(IRGenModule &IGM,
                                                        unsigned bits,
                                                        unsigned index,
                                                        unsigned offset);
  
/// Calculate the index of a function pointer extra inhabitant
/// representation stored in memory.
llvm::Value *getFunctionPointerExtraInhabitantIndex(IRGenFunction &IGF,
                                                    Address src);

/// Calculate an extra inhabitant representation from an index and
/// store it to memory.
void storeFunctionPointerExtraInhabitant(IRGenFunction &IGF,
                                         llvm::Value *index,
                                         Address dest);

} // end namespace irgen
} // end namespace swift

#endif
