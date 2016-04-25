//===--- FunctionSignatureOptUtils.h ----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_FUNCTIONSIGOPTUTILS_H
#define SWIFT_SIL_FUNCTIONSIGOPTUTILS_H

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/Projection.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"

namespace swift {

using ReleaseSet = llvm::DenseSet<SILInstruction *>;

/// A structure that maintains all of the information about a specific
/// SILArgument that we are tracking.
struct ArgumentDescriptor {

  /// The argument that we are tracking original data for.
  SILArgument *Arg;

  /// The original index of this argument.
  unsigned Index;

  /// The original decl of this Argument.
  const ValueDecl *Decl;

  /// Was this parameter originally dead?
  bool IsEntirelyDead;

  /// Should the argument be exploded ?
  bool Explode;

  /// Is this parameter an indirect result?
  bool IsIndirectResult;

  /// If non-null, this is the release in the return block of the callee, which
  /// is associated with this parameter if it is @owned. If the parameter is not
  /// @owned or we could not find such a release in the callee, this is null.
  ReleaseList CalleeRelease;

  /// The same as CalleeRelease, but the release in the throw block, if it is a
  /// function which has a throw block.
  ReleaseList CalleeReleaseInThrowBlock;

  /// The projection tree of this arguments.
  ProjectionTree ProjTree;

  ArgumentDescriptor() = delete;

  /// Initialize this argument descriptor with all information from A that we
  /// use in our optimization.
  ///
  /// *NOTE* We cache a lot of data from the argument and maintain a reference
  /// to the original argument. The reason why we do this is to make sure we
  /// have access to the original argument's state if we modify the argument
  /// when optimizing.
  ArgumentDescriptor(llvm::BumpPtrAllocator &BPA, SILArgument *A,
                     ReleaseSet Releases)
      : Arg(A), Index(A->getIndex()),
        Decl(A->getDecl()), IsEntirelyDead(false), Explode(false),
        IsIndirectResult(A->isIndirectResult()),
        CalleeRelease(), CalleeReleaseInThrowBlock(),
        ProjTree(A->getModule(), BPA, A->getType(), 
        ProjectionTreeNode::LivenessKind::IgnoreEpilogueReleases, 
        Releases) {
    ProjTree.computeUsesAndLiveness(A);
  }

  ArgumentDescriptor(const ArgumentDescriptor &) = delete;
  ArgumentDescriptor(ArgumentDescriptor &&) = default;
  ArgumentDescriptor &operator=(const ArgumentDescriptor &) = delete;
  ArgumentDescriptor &operator=(ArgumentDescriptor &&) = default;

  /// \returns true if this argument's convention is P.
  bool hasConvention(SILArgumentConvention P) const {
    return Arg->hasConvention(P);
  }

  /// Convert the potentially multiple interface params associated with this
  /// argument.
  void
  computeOptimizedInterfaceParams(SmallVectorImpl<SILParameterInfo> &Out) const;

  /// Add potentially multiple new arguments to NewArgs from the caller's apply
  /// or try_apply inst.
  void addCallerArgs(SILBuilder &Builder, FullApplySite FAS,
                     SmallVectorImpl<SILValue> &NewArgs) const;

  /// Add potentially multiple new arguments to NewArgs from the thunk's
  /// function arguments.
  void addThunkArgs(SILBuilder &Builder, SILBasicBlock *BB,
                    SmallVectorImpl<SILValue> &NewArgs) const;

  /// Optimize the argument at ArgOffset and return the index of the next
  /// argument to be optimized.
  ///
  /// The return value makes it easy to SROA arguments since we can return the
  /// amount of SROAed arguments we created.
  unsigned updateOptimizedBBArgs(SILBuilder &Builder, SILBasicBlock *BB,
                                 unsigned ArgOffset);

  bool canOptimizeLiveArg() const {
    return Arg->getType().isObject();
  }

  /// Return true if it's both legal and a good idea to explode this argument.
  bool shouldExplode() const {
    // We cannot optimize the argument.
    if (!canOptimizeLiveArg())
      return false;

    // See if the projection tree consists of potentially multiple levels of
    // structs containing one field. In such a case, there is no point in
    // exploding the argument.
    if (ProjTree.isSingleton())
      return false;

    size_t explosionSize = ProjTree.liveLeafCount();
    return explosionSize >= 1 && explosionSize <= 3;
  }
};

/// A structure that maintains all of the information about a specific
/// direct result that we are tracking.
struct ResultDescriptor {
  /// The original parameter info of this argument.
  SILResultInfo ResultInfo;

  /// If non-null, this is the release in the return block of the callee, which
  /// is associated with this parameter if it is @owned. If the parameter is not
  /// @owned or we could not find such a release in the callee, this is null.
  RetainList CalleeRetain;

  /// Initialize this argument descriptor with all information from A that we
  /// use in our optimization.
  ///
  /// *NOTE* We cache a lot of data from the argument and maintain a reference
  /// to the original argument. The reason why we do this is to make sure we
  /// have access to the original argument's state if we modify the argument
  /// when optimizing.
  ResultDescriptor() {};
  ResultDescriptor(SILResultInfo RI) : ResultInfo(RI), CalleeRetain() {}

  ResultDescriptor(const ResultDescriptor &) = delete;
  ResultDescriptor(ResultDescriptor &&) = default;
  ResultDescriptor &operator=(const ResultDescriptor &) = delete;
  ResultDescriptor &operator=(ResultDescriptor &&) = default;

  /// \returns true if this argument's ParameterConvention is P.
  bool hasConvention(ResultConvention R) const {
    return ResultInfo.getConvention() == R;
  }
};

class FunctionSignatureInfo {
  /// Should this function be optimized.
  bool ShouldOptimize;

  /// Optimizing this function may lead to good performance potential.
  bool HighlyProfitable;

  /// Function currently analyzing.
  SILFunction *F;

  /// The allocator we are using.
  llvm::BumpPtrAllocator &Allocator;

  /// The alias analysis currently using.
  AliasAnalysis *AA;

  /// The rc-identity analysis currently using.
  RCIdentityFunctionInfo *RCFI;

  /// Does any call inside the given function may bind dynamic 'Self' to a
  /// generic argument of the callee.
  bool MayBindDynamicSelf;

  /// Did we decide to change the self argument? If so we need to
  /// change the calling convention 'method' to 'freestanding'.
  bool ShouldModifySelfArgument = false;

  /// A list of structures which present a "view" of precompiled information on
  /// an argument that we will use during our optimization.
  llvm::SmallVector<ArgumentDescriptor, 8> ArgDescList;

  /// Keep a "view" of precompiled information on the direct results
  /// which we will use during our optimization.
  llvm::SmallVector<ResultDescriptor, 4> ResultDescList;


public:
  FunctionSignatureInfo(SILFunction *F, llvm::BumpPtrAllocator &BPA,
                        AliasAnalysis *AA, RCIdentityFunctionInfo *RCFI) :
  ShouldOptimize(false), HighlyProfitable(false), F(F), Allocator(BPA),
  AA(AA), RCFI(RCFI), MayBindDynamicSelf(computeMayBindDynamicSelf(F)) {
    analyze();
  }

  bool shouldOptimize() const { return ShouldOptimize; }
  bool profitableOptimize() const { return HighlyProfitable; }

  void analyze();
  bool analyzeParameters();
  bool analyzeResult();

  /// Returns the mangled name of the function that should be generated from
  /// this function analyzer.
  std::string getOptimizedName() const;

  bool shouldModifySelfArgument() const { return ShouldModifySelfArgument; }
  ArrayRef<ArgumentDescriptor> getArgDescList() const { return ArgDescList; }
  ArrayRef<ResultDescriptor> getResultDescList() {return ResultDescList;}
  SILFunction *getAnalyzedFunction() const { return F; }

private:
  /// Is the given argument required by the ABI?
  ///
  /// Metadata arguments may be required if dynamic Self is bound to any generic
  /// parameters within this function's call sites.
  bool isArgumentABIRequired(SILArgument *Arg) {
    // This implicitly asserts that a function binding dynamic self has a self
    // metadata argument or object from which self metadata can be obtained.
    return MayBindDynamicSelf && (F->getSelfMetadataArgument() == Arg);
  }
};



bool canSpecializeFunction(SILFunction *F);

void 
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      OperandValueArrayRef Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs);

void
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      ArrayRef<SILArgument*> Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs);
void
addRetainsForConvertedDirectResults(SILBuilder &Builder,
                                    SILLocation Loc,
                                    SILValue ReturnValue,
                                    SILInstruction *AI,
                                    ArrayRef<ResultDescriptor> DirectResults);


} // end namespace swift

#endif
