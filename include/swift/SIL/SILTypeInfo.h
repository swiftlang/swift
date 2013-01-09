//===--- SILTypeInfo.h - Defines the SILTypeInfo type hierarchy -*- C++ -*-===//
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
// This file defines the SILTypeInfo class hierarchy. SILTypeInfos contain
// detailed information about certain categories of Swift types are represented
// in SIL, such as the calling convention of functions or the layout of
// aggregate types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILTypeInfo_H
#define SWIFT_SIL_SILTypeInfo_H

#include "swift/SIL/SILBase.h"
#include "swift/SIL/SILType.h"

namespace swift {
  class VarDecl;
  
enum class SILTypeInfoKind {
  // SILTypeInfo,
  SILTypeInfo_First,
    SILFunctionTypeInfo = SILTypeInfo_First,
    SILCompoundTypeInfo,
  SILTypeInfo_Last = SILCompoundTypeInfo
};

/// SILTypeInfo - the abstract base class of all SILTypeInfos.
class SILTypeInfo : public SILAllocated<SILTypeInfo> {
public:
  const SILTypeInfoKind kind;

  SILTypeInfo(SILTypeInfoKind kind) : kind(kind) {}
  
  static bool classof(SILTypeInfo const *ti) {
    assert(ti->kind >= SILTypeInfoKind::SILTypeInfo_First
           && ti->kind <= SILTypeInfoKind::SILTypeInfo_Last
           && "invalid SILTypeInfo kind!");
    return true;
  }
};

/// SILFunctionTypeInfo - type info for a FunctionType or PolymorphicFunctionType.
/// Specifies the SIL-level calling convention for the function.
class SILFunctionTypeInfo : public SILTypeInfo {
  llvm::PointerIntPair<SILType, 1, bool> resultTypeAndIndirectReturn;
  size_t inputTypeCount;

  SILType *getInputTypeBuffer() {
    return reinterpret_cast<SILType*>(this+1);
  }
  SILType const *getInputTypeBuffer() const {
    return reinterpret_cast<SILType const *>(this+1);
  }
  
  SILFunctionTypeInfo(unsigned inputTypeCount,
                  SILType resultType,
                  bool hasIndirectReturn)
    : SILTypeInfo(SILTypeInfoKind::SILFunctionTypeInfo),
      resultTypeAndIndirectReturn(resultType, hasIndirectReturn) {}

public:
  static SILFunctionTypeInfo *create(ArrayRef<SILType> inputTypes,
                                 SILType resultType,
                                 bool hasIndirectReturn,
                                 SILBase &base);
  
  ArrayRef<SILType> getInputTypes() const {
    return ArrayRef<SILType>(getInputTypeBuffer(), inputTypeCount);
  }
  
  SILType getResultType() const {
    return resultTypeAndIndirectReturn.getPointer();
  }
  
  bool hasIndirectReturn() const {
    return resultTypeAndIndirectReturn.getInt();
  }
  
  static bool classof(SILTypeInfo const *ti) {
    return ti->kind == SILTypeInfoKind::SILFunctionTypeInfo;
  }
};

/// SILCompoundTypeInfo - type info for a compound type, such as a tuple,
/// struct, or class. Contains the type information for the type's elements
/// indexable by Extract, ElementAddr, or RefElementAddr instructions. For
/// nominal types, also provides a mapping from Swift VarDecls to field
/// indices.
class SILCompoundTypeInfo : public SILTypeInfo {
public:
  struct Element {
    /// The type of the element in the compound type.
    SILType type;
    /// The decl associated with the element, if any. Null for tuple types.
    VarDecl *decl;
  };
private:
  size_t elementCount;
  
  Element *getElementBuffer() {
    return reinterpret_cast<Element*>(this+1);
  }
  
  Element const *getElementBuffer() const {
    return reinterpret_cast<Element const *>(this+1);
  }
  
  SILCompoundTypeInfo(size_t elementCount)
    : SILTypeInfo(SILTypeInfoKind::SILCompoundTypeInfo),
      elementCount(elementCount) {}
  
public:
  static SILCompoundTypeInfo *create(ArrayRef<Element> elements, SILBase &base);
  
  ArrayRef<Element> getElements() const {
    return ArrayRef<Element>(getElementBuffer(), elementCount);
  }
  
  size_t getIndexOfMemberDecl(VarDecl *vd) const;
  
  static bool classof(SILTypeInfo const *ti) {
    return ti->kind == SILTypeInfoKind::SILCompoundTypeInfo;
  }
};

} // end llvm namespace

#endif