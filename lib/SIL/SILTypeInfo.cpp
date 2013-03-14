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
#include "swift/SIL/SILTypeInfo.h"

using namespace swift;

SILFunctionTypeInfo *SILFunctionTypeInfo::create(ArrayRef<SILType> inputTypes,
                                         SILType resultType,
                                         unsigned curriedInputCount,
                                         bool isUncurried,
                                         bool hasIndirectReturn,
                                         SILBase &base)
{
  void *buffer = base.allocate(sizeof(SILFunctionTypeInfo)
                                 + sizeof(SILType) * inputTypes.size(),
                               llvm::AlignOf<SILFunctionTypeInfo>::Alignment);
  SILFunctionTypeInfo *fi = ::new (buffer) SILFunctionTypeInfo(
                                                       inputTypes.size(),
                                                       resultType,
                                                       curriedInputCount,
                                                       isUncurried,
                                                       hasIndirectReturn);
  memcpy(fi->getInputTypeBuffer(), inputTypes.data(),
         sizeof(SILType) * inputTypes.size());
  return fi;
}

SILCompoundTypeInfo *SILCompoundTypeInfo::create(ArrayRef<Element> elements,
                                                 SILBase &base) {
  
  void *buffer = base.allocate(sizeof(SILCompoundTypeInfo)
                                 + sizeof(Element) * elements.size(),
                               llvm::AlignOf<SILCompoundTypeInfo>::Alignment);
  SILCompoundTypeInfo *cti =
    ::new (buffer) SILCompoundTypeInfo(elements.size());
  
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
