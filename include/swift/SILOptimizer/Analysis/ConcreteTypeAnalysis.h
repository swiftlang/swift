#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_CONCRETETYPE_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_CONCRETETYPE_H

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

class ConcreteTypeAnalysis : public SILAnalysis {
public:
  typedef SmallVector<NominalTypeDecl *, 8> NominalTypeList;
  typedef llvm::DenseMap<ProtocolDecl *, NominalTypeDecl *>
      ProtocolSoleTypeImplementation;
  typedef llvm::DenseMap<ProtocolDecl *, NominalTypeList>
      ProtocolImplementations;

  ConcreteTypeAnalysis(SILModule *Mod)
      : SILAnalysis(AnalysisKind::ConcreteType), M(Mod) {
    init();
  }

  ~ConcreteTypeAnalysis();

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::ConcreteType;
  }

  /// Invalidate all information in this analysis.
  virtual void invalidate() override {}

  /// Invalidate all of the information for a specific function.
  virtual void invalidate(SILFunction *F, InvalidationKind K) override {}

  /// Notify the analysis about a newly created function.
  virtual void notifyAddFunction(SILFunction *F) override {}

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyDeleteFunction(SILFunction *F) override {}

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override {}

  /// Get the sole class that implements a protocol.
  NominalTypeDecl *getSoleTypeImplementingProtocol(ProtocolDecl *P) {
    return ProtocolSoleTypeImplementationCache[P];
  }

private:
  /// Compute inheritance properties.
  void init();

  /// The module.
  SILModule *M;

  /// A cache that maps a protocol to its sole class conforming to it.
  ProtocolSoleTypeImplementation ProtocolSoleTypeImplementationCache;
};

} // namespace swift
#endif
