//===--- FunctionSignatureOptUtils.h ----------------------------*- C++ -*-===//
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

/// A structure that maintains all of the information about a specific
/// SILArgument that we are tracking.
struct ArgumentDescriptor {
  /// The argument that we are tracking original data for.
  SILFunctionArgument *Arg;

  /// Parameter Info.
  SILParameterInfo PInfo;

  /// The original index of this argument.
  unsigned Index;

  /// The original decl of this Argument.
  const ValueDecl *Decl;

  /// Was this parameter originally dead?
  bool IsEntirelyDead;

  /// Should the argument be exploded ?
  bool Explode;
  
  /// This parameter is owned to guaranteed.
  bool OwnedToGuaranteed;

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
  ArgumentDescriptor(SILFunctionArgument *A)
      : Arg(A), PInfo(A->getKnownParameterInfo()), Index(A->getIndex()),
        Decl(A->getDecl()), IsEntirelyDead(false), Explode(false),
        OwnedToGuaranteed(false), IsIndirectResult(A->isIndirectResult()),
        CalleeRelease(), CalleeReleaseInThrowBlock(),
        ProjTree(A->getModule(), A->getType()) {}

  ArgumentDescriptor(const ArgumentDescriptor &) = delete;
  ArgumentDescriptor(ArgumentDescriptor &&) = default;
  ArgumentDescriptor &operator=(const ArgumentDescriptor &) = delete;
  ArgumentDescriptor &operator=(ArgumentDescriptor &&) = default;

  /// \returns true if this argument's convention is P.
  bool hasConvention(SILArgumentConvention P) const {
    return Arg->hasConvention(P);
  }

  bool canOptimizeLiveArg() const {
    return Arg->getType().isObject();
  }

  /// Return true if it's both legal and a good idea to explode this argument.
  bool shouldExplode(ConsumedArgToEpilogueReleaseMatcher &ERM) const {
    // We cannot optimize the argument.
    if (!canOptimizeLiveArg())
      return false;

    // See if the projection tree consists of potentially multiple levels of
    // structs containing one field. In such a case, there is no point in
    // exploding the argument.
    //
    // Also, in case of a type can not be exploded, e.g an enum, we treat it
    // as a singleton.
    if (ProjTree.isSingleton())
      return false;

    // If this argument is @owned and we can not find all the releases for it
    // try to explode it, maybe we can find some of the releases and O2G some
    // of its components.
    //
    // This is a potentially a very profitable optimization. Ignore other
    // heuristics.
    if (hasConvention(SILArgumentConvention::Direct_Owned) && 
        ERM.hasSomeReleasesForArgument(Arg))
      return true;

    size_t explosionSize = ProjTree.liveLeafCount();
    return explosionSize >= 1 && explosionSize <= 3;
  }

  llvm::Optional<ValueOwnershipKind>
  getTransformedOwnershipKind(SILType SubTy) {
    if (IsEntirelyDead)
      return None;
    if (SubTy.isTrivial(Arg->getModule()))
      return Optional<ValueOwnershipKind>(ValueOwnershipKind::Trivial);
    if (OwnedToGuaranteed)
      return Optional<ValueOwnershipKind>(ValueOwnershipKind::Guaranteed);
    return Arg->getOwnershipKind();
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
  llvm::SmallSetVector<SILInstruction *, 1> CalleeRetain;

  /// This is owned to guaranteed.
  bool OwnedToGuaranteed;

  /// Initialize this argument descriptor with all information from A that we
  /// use in our optimization.
  ///
  /// *NOTE* We cache a lot of data from the argument and maintain a reference
  /// to the original argument. The reason why we do this is to make sure we
  /// have access to the original argument's state if we modify the argument
  /// when optimizing.
  ResultDescriptor() {}
  ResultDescriptor(SILResultInfo RI) 
    : ResultInfo(RI), CalleeRetain(), OwnedToGuaranteed(false) {}

  ResultDescriptor(const ResultDescriptor &) = delete;
  ResultDescriptor(ResultDescriptor &&) = default;
  ResultDescriptor &operator=(const ResultDescriptor &) = delete;
  ResultDescriptor &operator=(ResultDescriptor &&) = default;

  /// \returns true if this argument's ParameterConvention is P.
  bool hasConvention(ResultConvention R) const {
    return ResultInfo.getConvention() == R;
  }
};

/// Returns true if F is a function which the pass know show to specialize
/// function signatures for.
bool canSpecializeFunction(SILFunction *F);

/// Return true if this argument is used in a non-trivial way.
bool hasNonTrivialNonDebugUse(SILArgument *Arg);

} // end namespace swift

#endif
