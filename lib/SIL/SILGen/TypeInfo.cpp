//===--- TypeInfo.cpp - Type information relevant to SILGen -----*- C++ -*-===//
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

#include "SILGen.h"
#include "TypeInfo.h"
#include "TypeVisitor.h"

namespace swift {
namespace Lowering {

/// LoadableTypeInfoVisitor - Recursively descend into fragile struct and
/// tuple types and visit their element types, storing information about the
/// reference type members in the TypeInfo for the type.
class LoadableTypeInfoVisitor : public TypeVisitor<LoadableTypeInfoVisitor> {
  TypeInfo &theInfo;
  ReferenceTypeElement currentElement;
public:
  LoadableTypeInfoVisitor(TypeInfo &theInfo) : theInfo(theInfo) {}
  
  void pushPath() { currentElement.path.push_back({Type(), 0}); }
  void popPath() { currentElement.path.pop_back(); }
  void setPathType(Type t) { currentElement.path.back().type = t; }
  void advancePath() { ++currentElement.path.back().index; }
  
  void visitType(TypeBase *t) {
    if (t->hasReferenceSemantics()) {
      theInfo.referenceTypeElements.push_back(currentElement);
    }
  }
  
  void visitNominalType(NominalType *t) {
    if (StructDecl *sd = dyn_cast<StructDecl>(t->getDecl())) {
      // FIXME: if this struct has a resilient attribute, mark the TypeInfo
      // as addressOnly and bail.
      
      pushPath();
      for (Decl *d : sd->getMembers())
        if (VarDecl *vd = dyn_cast<VarDecl>(d))
          if (!vd->isProperty()) {
            CanType ct = vd->getType()->getCanonicalType();
            setPathType(ct);
            visit(ct);
            advancePath();
          }
      popPath();
    } else
      this->visitType(t);
  }
  
  void visitTupleType(TupleType *t) {
    pushPath();
    for (TupleTypeElt const &elt : t->getFields()) {
      CanType ct = elt.getType()->getCanonicalType();
      setPathType(ct);
      visit(ct);
      advancePath();
    }
    popPath();
  }
};

void TypeConverter::makeFragileElementsForDecl(TypeInfo &theInfo,
                                               NominalTypeDecl *decl) {
  unsigned elementIndex = 0;
  for (Decl *d : decl->getMembers()) {
    if (VarDecl *vd = dyn_cast<VarDecl>(d)) {
      if (!vd->isProperty()) {
        theInfo.fragileElements[vd->getName()] = {vd->getType(),
          elementIndex};
        ++elementIndex;
      }
    }
  }
}

void TypeConverter::makeFragileElements(TypeInfo &theInfo, CanType t) {
  if (NominalType *nt = t->getAs<NominalType>()) {
    makeFragileElementsForDecl(theInfo, nt->getDecl());
  } else if (BoundGenericType *bgt = t->getAs<BoundGenericType>()) {
    makeFragileElementsForDecl(theInfo, bgt->getDecl());
  }
}
  
TypeInfo const &TypeConverter::makeTypeInfo(CanType t) {
  TypeInfo &theInfo = types[t.getPointer()];
  
  if (t->hasReferenceSemantics()) {
    theInfo.addressOnly = false;
    theInfo.referenceTypeElements.push_back(ReferenceTypeElement());
  } else {
    // walk aggregate types to determine address-only-ness and find reference
    // type elements.
    theInfo.addressOnly = false;
    LoadableTypeInfoVisitor(theInfo).visit(t);
  }
  // If this is a struct or class type, find its fragile elements.
  makeFragileElements(theInfo, t);
  
  return theInfo;
}
  
TypeInfo const &TypeConverter::getTypeInfo(Type t) {
  CanType ct = t->getCanonicalType();
  auto existing = types.find(ct.getPointer());
  if (existing == types.end()) {
    return makeTypeInfo(ct);
  } else
    return existing->second;
}
  
} // namespace Lowering
} // namespace swift
