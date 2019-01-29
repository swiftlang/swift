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
  auto module = M->getSwiftModule();

  // For each class declaration in our V-table list:
  for (auto &VT : M->getVTableList()) {
    ClassDecl *C = VT.getClass();

    while (true) {
      // Ignore classes that are at the top of the class hierarchy:
      if (!C->hasSuperclass())
        break;

      ClassDecl *super = C->getSuperclassDecl();
      auto superModule = super->getModuleContext();

      // Don't bother collecting subclasses for classes from a different module.
      // TODO: cross-module WMO
      if (superModule != module)
        break;

      // Find the superclass's list of direct subclasses.  If it's non-empty,
      // we've previously walked up to the class, so there's no reason to keep
      // walking from this point.
      auto &list = DirectSubclassesCache[super];
      bool shouldVisitSuper = list.empty();

      // Check whether C is already in the list, which can happen
      // if we had a v-table that was a subclass of C.
      // We expect a linear scan to be cheap enough for this.
      if (std::find(list.begin(), list.end(), C) != list.end())
        break;

      list.push_back(C);

      // Keep walking if this is the first time we reached this superclass.
      // We have to do this because the SILModule might not have v-tables for
      // every class in the module.
      if (!shouldVisitSuper)
        break;

      C = super;
    }
  }
}

/// Get all subclasses of a given class.
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
