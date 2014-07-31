//===-- ClassHierarchyAnalysis.h - Analysis of Class Hierarchy --*- C++ -*-===//
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

#ifndef SWIFT_SILANALYSIS_CLASSHIERARCHY_H
#define SWIFT_SILANALYSIS_CLASSHIERARCHY_H

#include "swift/SILAnalysis/Analysis.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Debug.h"

namespace swift {

class SILModule;
class ClassDecl;
class ClassHierarchyAnalysis : public SILAnalysis {
public:
  typedef llvm::SmallVector<ClassDecl*, 8> ClassList;
  typedef llvm::SmallPtrSet<ClassDecl *, 32>  ClassSet;


  ClassHierarchyAnalysis(SILModule *Mod) :
  SILAnalysis(AnalysisKind::ClassHierarchyAnalysis), M(Mod) {
      init(); 
    }

  ~ClassHierarchyAnalysis();

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::ClassHierarchyAnalysis;
  }

  virtual void invalidate(InvalidationKind K) {
    if (K >= InvalidationKind::All) {
      SubclassesCache.clear();
      init(); 
    }
  }


  /// \returns a list of the known direct subclasses of a class \p C in
  /// the current module.
  ClassList& getSubClasses(ClassDecl *C) { return SubclassesCache[C]; }

  /// \returns True if the class is inherited by another class in this module.
  bool inheritedInModule(ClassDecl *CD) {
    return SubclassesCache.count(CD);
  }

  virtual void invalidate(SILFunction *F, InvalidationKind K) {
    invalidate(K);
  }

private:
  /// Compute inheritance properties.
  void init();

  /// The module
  SILModule *M;

  /// A cache that maps a class to all of its known subclasses.
  llvm::DenseMap<ClassDecl*, ClassList> SubclassesCache;
};

}
#endif
