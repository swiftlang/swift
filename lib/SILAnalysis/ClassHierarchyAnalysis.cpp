//===- ClassHierarchyAnalysis.cpp - Analysis of class hierarchy -*- C++ -*-===//
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

#include "swift/SILAnalysis/ClassHierarchyAnalysis.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

void ClassHierarchyAnalysis::init() {
  // For each class declaration in our V-table list:
  for (auto &VT : M->getVTableList()) {
    ClassDecl *C = VT.getClass();
    // Ignore classes that are at the top of the class hierarchy:
    if (!C->hasSuperclass())
      continue;

    // Add the superclass to the list of inherited classes.
    ClassDecl *Super = C->getSuperclass()->getClassOrBoundGenericClass();
    auto &K = DirectSubclassesCache[Super];
    assert(std::find(K.begin(), K.end(), C) == K.end() &&
           "Class vector must be unique");
    K.push_back(C);
  }
}

/// \brief Get all subclasses subclasses of a given class.
/// Does not include any direct subclasses of given base class.
///
/// \p Base base class, whose direct subclasses are to be excluded
/// \p Current class, whose direct and indirect subclasses are
///    to be collected.
/// \p IndirectSubs placeholder for collected results
void ClassHierarchyAnalysis::getIndirectSubClasses(ClassDecl *Base,
                                                   ClassDecl *Current,
                                                   ClassList& IndirectSubs) {
  // Get direct subclasses
  if (!hasKnownDirectSubclasses(Current))
    return;
  ClassList &Subs = getDirectSubClasses(Current);
  for (auto S : Subs) {
    // Remember this subclass
    if (Base != Current)
      IndirectSubs.push_back(S);
    // Collect subclasses of a given subclass
    getIndirectSubClasses(Base, S, IndirectSubs);
  }
}


ClassHierarchyAnalysis::~ClassHierarchyAnalysis() {}
