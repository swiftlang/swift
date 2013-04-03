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
#include "swift/SIL/SILBase.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"

using namespace swift;

SILFunctionTypeInfo *SILFunctionTypeInfo::create(CanType swiftType,
                                       ArrayRef<SILType> inputTypes,
                                       SILType resultType,
                                       ArrayRef<unsigned> uncurriedInputCounts,
                                       bool hasIndirectReturn,
                                       SILBase &base)
{
  // We allocate room for an extra unsigned in the uncurriedInputCounts array,
  // so that we can stuff a leading zero in there and be able to efficiently
  // return both the begins and ends of each uncurried argument group.
  void *buffer = base.allocate(sizeof(SILFunctionTypeInfo)
                                 + sizeof(SILType)*inputTypes.size()
                                 + sizeof(unsigned)*(1+uncurriedInputCounts.size()),
                               alignof(SILFunctionTypeInfo));
  SILFunctionTypeInfo *fi = ::new (buffer) SILFunctionTypeInfo(
                                                     swiftType,
                                                     inputTypes.size(),
                                                     resultType,
                                                     uncurriedInputCounts.size(),
                                                     hasIndirectReturn);
  memcpy(fi->getInputTypeBuffer(), inputTypes.data(),
         sizeof(SILType) * inputTypes.size());
  fi->getUncurryBuffer()[0] = 0;
  memcpy(fi->getUncurryBuffer()+1, uncurriedInputCounts.data(),
         sizeof(unsigned) * uncurriedInputCounts.size());
  return fi;
}

SILCompoundTypeInfo *SILCompoundTypeInfo::create(CanType swiftType,
                                                 ArrayRef<Element> elements,
                                                 SILBase &base) {
  
  void *buffer = base.allocate(sizeof(SILCompoundTypeInfo)
                                 + sizeof(Element) * elements.size(),
                               alignof(SILCompoundTypeInfo));
  SILCompoundTypeInfo *cti =
    ::new (buffer) SILCompoundTypeInfo(swiftType, elements.size());
  
  memcpy(cti->getElementBuffer(), elements.data(),
         sizeof(Element) * elements.size());
  return cti;
}

size_t SILCompoundTypeInfo::getIndexOfMemberDecl(VarDecl *vd) const {
  // FIXME: use an index of some sort instead of doing linear scan.
  // We can't however use DenseMap because SILTypeInfos are bump-allocated and
  // don't get destructed.
  ArrayRef<Element> elements = getElements();
  for (size_t i = 0, size = elements.size(); i < size; ++i) {
    if (elements[i].decl == vd)
      return i;
  }
  llvm_unreachable("decl is not a member of type");
}

unsigned SILType::getUncurryLevel() const {
  if (auto *info = value.getPointer().dyn_cast<SILTypeInfo*>())
    if (auto *finfo = dyn_cast<SILFunctionTypeInfo>(info))
      return finfo->getUncurryLevel();
  return 0;
}
