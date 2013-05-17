//===--- SILTypeInfo.cpp - Defines the SILTypeInfo class hierarchy --------===//
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

#include "llvm/Support/ErrorHandling.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"

using namespace swift;

SILFunctionTypeInfo *SILFunctionTypeInfo::create(CanType swiftType,
                                       ArrayRef<SILType> inputTypes,
                                       SILType resultType,
                                       ArrayRef<unsigned> uncurriedInputCounts,
                                       bool hasIndirectReturn,
                                       AbstractCC cc,
                                       SILModule &M) {
  // We allocate room for an extra unsigned in the uncurriedInputCounts array,
  // so that we can stuff a leading zero in there and be able to efficiently
  // return both the begins and ends of each uncurried argument group.
  void *buffer = M.allocate(sizeof(SILFunctionTypeInfo)
                                 + sizeof(SILType)*inputTypes.size()
                             + sizeof(unsigned)*(1+uncurriedInputCounts.size()),
                               alignof(SILFunctionTypeInfo));
  SILFunctionTypeInfo *fi = ::new (buffer) SILFunctionTypeInfo(
                                                     swiftType,
                                                     inputTypes.size(),
                                                     resultType,
                                                    uncurriedInputCounts.size(),
                                                    hasIndirectReturn,
                                                     cc);
  memcpy(fi->getInputTypeBuffer(), inputTypes.data(),
         sizeof(SILType) * inputTypes.size());
  fi->getUncurryBuffer()[0] = 0;
  memcpy(fi->getUncurryBuffer()+1, uncurriedInputCounts.data(),
         sizeof(unsigned) * uncurriedInputCounts.size());
  return fi;
}

unsigned SILType::getUncurryLevel() const {
  if (auto *finfo = value.getPointer().dyn_cast<SILFunctionTypeInfo*>())
    return finfo->getUncurryLevel();
  return 0;
}
