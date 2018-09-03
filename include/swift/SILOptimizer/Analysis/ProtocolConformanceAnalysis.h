//===--- ProtocolConformanceAnalysis.h - Protocol Conformance ---*- C++ -*-===//
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
// This analysis collects a set of nominal types (classes, structs, and enums)
// that conform to a protocol during whole module compilation. We only track
// protocols that are non-public.

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_PROTOCOLCONFORMANCE_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_PROTOCOLCONFORMANCE_H

#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

namespace swift {

class SILModule;
class NominalTypeDecl;
class ProtocolDecl;

class ProtocolConformanceAnalysis : public SILAnalysis {
public:
  typedef SmallVector<NominalTypeDecl *, 8> NominalTypeList;
  typedef llvm::DenseMap<ProtocolDecl *, NominalTypeList>
      ProtocolConformanceMap;
  typedef llvm::DenseMap<ProtocolDecl *, NominalTypeDecl *>
      SoleConformingTypeMap;

  ProtocolConformanceAnalysis(SILModule *Mod)
      : SILAnalysis(SILAnalysisKind::ProtocolConformance), M(Mod) {
    init();
  }

  ~ProtocolConformanceAnalysis();

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::ProtocolConformance;
  }

  /// Invalidate all information in this analysis.
  virtual void invalidate() override {}

  /// Invalidate all of the information for a specific function.
  virtual void invalidate(SILFunction *F, InvalidationKind K) override {}

  /// Notify the analysis about a newly created function.
  virtual void notifyAddedOrModifiedFunction(SILFunction *F) override {}

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyWillDeleteFunction(SILFunction *F) override {}

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override {}

  /// Get the nominal types that implement a protocol.
  ArrayRef<NominalTypeDecl *> getConformances(const ProtocolDecl *P) const {
    auto ConformsListIt = ProtocolConformanceCache.find(P);
    return ConformsListIt != ProtocolConformanceCache.end()
               ? ArrayRef<NominalTypeDecl *>(ConformsListIt->second.begin(),
                                             ConformsListIt->second.end())
               : ArrayRef<NominalTypeDecl *>();
  }
  
  /// Traverse ProtocolConformanceMapCache recursively to determine sole
  /// conforming concrete type. 
  NominalTypeDecl *findSoleConformingType(ProtocolDecl *Protocol);

private:
  /// Compute inheritance properties.
  void init();

  /// The module.
  SILModule *M;

  /// A cache that maps a protocol to its conformances.
  ProtocolConformanceMap ProtocolConformanceCache;

  /// A cache that holds SoleConformingType for protocols.
  SoleConformingTypeMap SoleConformingTypeCache;
};

} // namespace swift
#endif
