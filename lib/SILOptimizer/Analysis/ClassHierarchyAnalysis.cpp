//===--- ClassHierarchyAnalysis.cpp - Class hierarchy analysis ------------===//
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

#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
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

/// \brief Get all subclasses of a given class.
///
/// \p Current class, whose direct and indirect subclasses are
///    to be collected.
/// \p IndirectSubs placeholder for collected results
void ClassHierarchyAnalysis::getIndirectSubClasses(ClassDecl *Cur,
                                                   ClassList &IndirectSubs) {
  unsigned Idx = IndirectSubs.size();

  if (!hasKnownDirectSubclasses(Cur))
    return;

  // Produce a set of all indirect subclasses in a
  // breadth-first order;

  // First add subclasses of direct subclasses.
  for (auto C : getDirectSubClasses(Cur)) {
    // Get direct subclasses
    if (!hasKnownDirectSubclasses(C))
      continue;
    auto &DirectSubclasses = getDirectSubClasses(C);
    // Remember all direct subclasses of the current one.
    for (auto S : DirectSubclasses) {
      IndirectSubs.push_back(S);
    }
  }

  // Then recursively add direct subclasses of already
  // added subclasses.
  while (Idx != IndirectSubs.size()) {
    auto C = IndirectSubs[Idx++];
    // Get direct subclasses
    if (!hasKnownDirectSubclasses(C))
      continue;
    auto &DirectSubclasses = getDirectSubClasses(C);
    // Remember all direct subclasses of the current one.
    for (auto S : DirectSubclasses) {
      IndirectSubs.push_back(S);
    }
  }
}


ClassHierarchyAnalysis::~ClassHierarchyAnalysis() {}
