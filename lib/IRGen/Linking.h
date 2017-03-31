//===--- Linking.h - Common declarations for link information ---*- C++ -*-===//
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
// This file defines structures and routines used when creating global
// entities that are placed in the LLVM module, potentially with
// external linkage.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LINKING_H
#define SWIFT_IRGEN_LINKING_H

#include "swift/IRGen/LinkEntity.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/GlobalValue.h"
#include "DebugTypeInfo.h"
#include "IRGen.h"
#include "IRGenModule.h"
#include "swift/IRGen/ValueWitness.h"

namespace llvm {
  class AttributeSet;
  class Value;
  class FunctionType;
}

namespace swift {  
namespace irgen {
class IRGenModule;

/// Encapsulated information about the linkage of an entity.
class LinkInfo {
  LinkInfo() = default;

  llvm::SmallString<32> Name;
  llvm::GlobalValue::LinkageTypes Linkage;
  llvm::GlobalValue::VisibilityTypes Visibility;
  llvm::GlobalValue::DLLStorageClassTypes DLLStorageClass;
  ForDefinition_t ForDefinition;

public:
  /// Compute linkage information for the given 
  static LinkInfo get(IRGenModule &IGM, const LinkEntity &entity,
                      ForDefinition_t forDefinition);

  StringRef getName() const {
    return Name.str();
  }
  llvm::GlobalValue::LinkageTypes getLinkage() const {
    return Linkage;
  }
  llvm::GlobalValue::VisibilityTypes getVisibility() const {
    return Visibility;
  }
  llvm::GlobalValue::DLLStorageClassTypes getDLLStorage() const {
    return DLLStorageClass;
  }

  llvm::Function *createFunction(IRGenModule &IGM,
                                 llvm::FunctionType *fnType,
                                 llvm::CallingConv::ID cc,
                                 const llvm::AttributeSet &attrs,
                                 llvm::Function *insertBefore = nullptr);


  llvm::GlobalVariable *createVariable(IRGenModule &IGM,
                                  llvm::Type *objectType,
                                  Alignment alignment,
                                  DebugTypeInfo DebugType=DebugTypeInfo(),
                                  Optional<SILLocation> DebugLoc = None,
                                  StringRef DebugName = StringRef());

  bool isUsed() const {
    return ForDefinition && isUsed(Linkage, Visibility, DLLStorageClass);
  }

  static bool isUsed(llvm::GlobalValue::LinkageTypes Linkage,
                     llvm::GlobalValue::VisibilityTypes Visibility,
                     llvm::GlobalValue::DLLStorageClassTypes DLLStorage);
};

} // end namespace irgen
} // end namespace swift

#endif
