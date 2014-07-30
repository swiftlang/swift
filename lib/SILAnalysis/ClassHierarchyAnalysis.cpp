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
    InheritedClasses.insert(Super);
  }
}

void ClassHierarchyAnalysis::collectSubClasses(ClassDecl *C,
                                               std::vector<ClassDecl*> &Sub) {
  // TODO: Cache this search.
  assert(Sub.empty() && "Incoming list is not empty");
  for (auto &VT : M->getVTableList()) {
    ClassDecl *CD = VT.getClass();
    // Ignore classes that are at the top of the class hierarchy:
    if (!CD->hasSuperclass())
      continue;

    // Add the superclass to the list of inherited classes.
    ClassDecl *Super = CD->getSuperclass()->getClassOrBoundGenericClass();
    if (Super == C) {
      Sub.push_back(CD);
    }
  }
}

ClassHierarchyAnalysis::~ClassHierarchyAnalysis() {}
