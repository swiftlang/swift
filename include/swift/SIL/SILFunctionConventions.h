//===- SILFunctionConventions.h - Defines SILFunctioConventions -*- C++ -*-===//
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
//
// This file defines the SILModuleConventions and SILFunctionConventions
// classes.  These interfaces are used to determine when SIL can represent
// values of a given lowered type by value and when they must be represented by
// address. This is influenced by a SILModule-wide "lowered address" convention,
// which reflects whether the current SIL stage requires lowered addresses.
//
// The primary purpose of this API is mapping the formal SIL parameter and
// result conventions onto the SIL argument types. The "formal" conventions are
// immutably associated with a SILFunctionType--a SIL function's type
// information never changes. The SIL conventions determine how those formal
// conventions will be represented in the body of SIL functions and at call
// sites.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_FUNCTIONCONVENTIONS_H
#define SWIFT_SIL_FUNCTIONCONVENTIONS_H

#include "swift/AST/Types.h"
#include "swift/SIL/SILArgumentConvention.h"
#include "swift/SIL/SILType.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {

/// Transient wrapper for SILParameterInfo and SILResultInfo conventions. This
/// abstraction helps handle the transition from canonical SIL conventions to
/// lowered SIL conventions.
class SILModuleConventions {
  friend SILParameterInfo;
  friend SILResultInfo;
  friend SILFunctionConventions;

  static bool isIndirectSILParam(SILParameterInfo param, bool loweredAddresses);

  static bool isIndirectSILResult(SILResultInfo result, bool loweredAddresses);

  static SILType getSILParamType(SILParameterInfo param, bool loweredAddresses);

  static SILType getSILResultType(SILResultInfo param, bool loweredAddresses);

public:
  static bool isPassedIndirectlyInSIL(SILType type, SILModule &M);

  static bool isReturnedIndirectlyInSIL(SILType type, SILModule &M);

  static SILModuleConventions getLoweredAddressConventions() {
    return SILModuleConventions(true);
  }

private:
  bool loweredAddresses;

  SILModuleConventions(bool loweredAddresses)
      : loweredAddresses(loweredAddresses) {}

public:
  SILModuleConventions(const SILModule &M);

  SILFunctionConventions getFunctionConventions(CanSILFunctionType funcTy);

  bool useLoweredAddresses() const { return loweredAddresses; }

  bool isSILIndirect(SILParameterInfo param) const {
    return isIndirectSILParam(param, loweredAddresses);
  }

  bool isSILIndirect(SILResultInfo result) const {
    return isIndirectSILResult(result, loweredAddresses);
  }

  SILType getSILType(SILParameterInfo param) const {
    return getSILParamType(param, loweredAddresses);
  }

  SILType getSILType(SILResultInfo result) const {
    return getSILResultType(result, loweredAddresses);
  }
};

/// Transient wrapper for SIL-level argument conventions. This abstraction helps
/// handle the transition from canonical SIL conventions to lowered SIL
/// conventions.
class SILFunctionConventions {
public:
  SILModuleConventions silConv;
  CanSILFunctionType funcTy;

  SILFunctionConventions(CanSILFunctionType funcTy, SILModule &M)
      : silConv(M), funcTy(funcTy) {}

  SILFunctionConventions(CanSILFunctionType funcTy,
                         SILModuleConventions silConv)
      : silConv(silConv), funcTy(funcTy) {}

  //===--------------------------------------------------------------------===//
  // SILModuleConventions API for convenience.
  //===--------------------------------------------------------------------===//

  bool useLoweredAddresses() const { return silConv.useLoweredAddresses(); }

  bool isSILIndirect(SILParameterInfo param) const {
    return silConv.isSILIndirect(param);
  }

  bool isSILIndirect(SILResultInfo result) const {
    return silConv.isSILIndirect(result);
  }

  SILType getSILType(SILParameterInfo param) const {
    return silConv.getSILType(param);
  }

  SILType getSILType(SILResultInfo result) const {
    return silConv.getSILType(result);
  }

  //===--------------------------------------------------------------------===//
  // SIL results.
  //===--------------------------------------------------------------------===//

  /// Get the normal result type of an apply that calls this function.
  /// This does not include indirect SIL results.
  SILType getSILResultType() {
    if (silConv.loweredAddresses)
      return funcTy->getDirectFormalResultsType();

    return funcTy->getAllResultsType();
  }

  /// Get the SIL type for the single result which may be direct or indirect.
  SILType getSingleSILResultType() {
    return getSILType(funcTy->getSingleResult());
  }

  /// Get the error result type.
  SILType getSILErrorType() { return getSILType(funcTy->getErrorResult()); }

  /// Returns an array of result info.
  /// Provides convenient access to the underlying SILFunctionType.
  ArrayRef<SILResultInfo> getResults() const {
    return funcTy->getResults();
  }

  /// Get the number of SIL results passed as address-typed arguments.
  unsigned getNumIndirectSILResults() const {
    return silConv.loweredAddresses ? funcTy->getNumIndirectFormalResults() : 0;
  }

  /// Are any SIL results passed as address-typed arguments?
  bool hasIndirectSILResults() const { return getNumIndirectSILResults() != 0; }

  using IndirectSILResultIter = SILFunctionType::IndirectFormalResultIter;
  using IndirectSILResultRange = SILFunctionType::IndirectFormalResultRange;

  /// Return a range of indirect result information for results passed as
  /// address-typed SIL arguments.
  IndirectSILResultRange getIndirectSILResults() const {
    if (silConv.loweredAddresses)
      return funcTy->getIndirectFormalResults();

    auto filter = llvm::make_filter_range(
        makeIteratorRange((const SILResultInfo *)0, (const SILResultInfo *)0),
        SILFunctionType::IndirectFormalResultFilter());
    return makeIteratorRange(filter.begin(), filter.end());
  }

  struct SILResultTypeFunc {
    SILModuleConventions silConv;
    SILResultTypeFunc(SILModuleConventions silConv) : silConv(silConv) {}

    SILType operator()(SILResultInfo result) const {
      return silConv.getSILType(result);
    }
  };

  using IndirectSILResultTypeIter =
      llvm::mapped_iterator<IndirectSILResultIter, SILResultTypeFunc>;
  using IndirectSILResultTypeRange = IteratorRange<IndirectSILResultTypeIter>;

  /// Return a range of SILTypes for each result passed as an address-typed SIL
  /// argument.
  IndirectSILResultTypeRange getIndirectSILResultTypes() const {
    return makeIteratorRange(
        IndirectSILResultTypeIter(getIndirectSILResults().begin(),
                                  SILResultTypeFunc(silConv)),
        IndirectSILResultTypeIter(getIndirectSILResults().end(),
                                  SILResultTypeFunc(silConv)));
  }

  /// Get the number of SIL results directly returned by SIL value.
  unsigned getNumDirectSILResults() const {
    return silConv.loweredAddresses ? funcTy->getNumDirectFormalResults()
                                    : funcTy->getNumResults();
  }

  struct DirectSILResultFilter {
    bool loweredAddresses;
    DirectSILResultFilter(bool loweredAddresses)
        : loweredAddresses(loweredAddresses) {}
    bool operator()(SILResultInfo result) const {
      return !(loweredAddresses && result.isFormalIndirect());
    }
  };
  using DirectSILResultIter =
      llvm::filter_iterator<const SILResultInfo *, DirectSILResultFilter>;
  using DirectSILResultRange = IteratorRange<DirectSILResultIter>;

  /// Return a range of direct result information for results directly returned
  /// by SIL value.
  DirectSILResultRange getDirectSILResults() const {
    auto filter = llvm::make_filter_range(
        funcTy->getResults(), DirectSILResultFilter(silConv.loweredAddresses));
    return makeIteratorRange(filter.begin(), filter.end());
  }

  using DirectSILResultTypeIter =
      llvm::mapped_iterator<DirectSILResultIter, SILResultTypeFunc>;
  using DirectSILResultTypeRange = IteratorRange<DirectSILResultTypeIter>;

  /// Return a range of SILTypes for each result directly returned
  /// by SIL value.
  DirectSILResultTypeRange getDirectSILResultTypes() const {
    return makeIteratorRange(
        DirectSILResultTypeIter(getDirectSILResults().begin(),
                                SILResultTypeFunc(silConv)),
        DirectSILResultTypeIter(getDirectSILResults().end(),
                                SILResultTypeFunc(silConv)));
  }

  //===--------------------------------------------------------------------===//
  // SIL parameters types.
  //===--------------------------------------------------------------------===//

  /// Returns the number of function parameters, not including any formally
  /// indirect results. Provides convenient access to the underlying
  /// SILFunctionType.
  unsigned getNumParameters() const { return funcTy->getNumParameters(); }

  /// Returns an array of parameter info, not including indirect
  /// results. Provides convenient access to the underlying SILFunctionType.
  ArrayRef<SILParameterInfo> getParameters() const {
    return funcTy->getParameters();
  }

  struct SILParameterTypeFunc {
    SILModuleConventions silConv;
    SILParameterTypeFunc(SILModuleConventions silConv) : silConv(silConv) {}

    SILType operator()(SILParameterInfo param) const {
      return silConv.getSILType(param);
    }
  };

  using SILParameterTypeIter =
      llvm::mapped_iterator<const SILParameterInfo *, SILParameterTypeFunc>;
  using SILParameterTypeRange = IteratorRange<SILParameterTypeIter>;

  /// Return a range of SILTypes for each function parameter, not including
  /// indirect results.
  SILParameterTypeRange getParameterSILTypes() const {
    return makeIteratorRange(
        SILParameterTypeIter(funcTy->getParameters().begin(),
                             SILParameterTypeFunc(silConv)),
        SILParameterTypeIter(funcTy->getParameters().end(),
                             SILParameterTypeFunc(silConv)));
  }

  //===--------------------------------------------------------------------===//
  // SILArgument API, including indirect results and parameters.
  //
  // The argument indices below relate to full applies in which the caller and
  // callee indices match. Partial apply indices are shifted on the caller
  // side. See ApplySite::getCallArgIndexOfFirstAppliedArg().
  //===--------------------------------------------------------------------===//

  unsigned getSILArgIndexOfFirstIndirectResult() const { return 0; }

  unsigned getSILArgIndexOfFirstParam() const {
    return getNumIndirectSILResults();
  }

  /// Get the index into formal indirect results corresponding to the given SIL
  /// indirect result argument index.
  unsigned getIndirectFormalResultIndexForSILArg(unsigned argIdx) const {
    assert(argIdx <= getNumIndirectSILResults());
    unsigned curArgIdx = 0;
    unsigned formalIdx = 0;
    for (auto formalResult : funcTy->getIndirectFormalResults()) {
      if (isSILIndirect(formalResult)) {
        if (curArgIdx == argIdx)
          return formalIdx;
        ++curArgIdx;
      }
      ++formalIdx;
    }
    llvm_unreachable("missing indirect formal result for SIL argument.");
  }

  /// Get the total number of arguments for a full apply in SIL of
  /// this function type. This is also the total number of SILArguments
  /// in the entry block.
  unsigned getNumSILArguments() const {
    return getNumIndirectSILResults() + funcTy->getNumParameters();
  }

  SILParameterInfo getParamInfoForSILArg(unsigned index) const {
    assert(index >= getNumIndirectSILResults()
           && index <= getNumSILArguments());
    return funcTy->getParameters()[index - getNumIndirectSILResults()];
  }

  /// Return the SIL argument convention of apply/entry argument at
  /// the given argument index.
  SILArgumentConvention getSILArgumentConvention(unsigned index) const;
  // See SILArgument.h.

  /// Return the SIL type of the apply/entry argument at the given index.
  SILType getSILArgumentType(unsigned index) const {
    assert(index <= getNumSILArguments());
    if (index < getNumIndirectSILResults()) {
      return *std::next(getIndirectSILResultTypes().begin(), index);
    }
    return getSILType(
        funcTy->getParameters()[index - getNumIndirectSILResults()]);
  }
};

inline SILFunctionConventions
SILModuleConventions::getFunctionConventions(CanSILFunctionType funcTy) {
  return SILFunctionConventions(funcTy, *this);
}

inline bool SILModuleConventions::isIndirectSILParam(SILParameterInfo param,
                                                     bool loweredAddresses) {
  switch (param.getConvention()) {
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Owned:
    return false;

  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Guaranteed:
    return loweredAddresses;
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    return true;
  }
  llvm_unreachable("covered switch isn't covered?!");
}

inline bool SILModuleConventions::isIndirectSILResult(SILResultInfo result,
                                                      bool loweredAddresses) {
  switch (result.getConvention()) {
  case ResultConvention::Indirect:
    return loweredAddresses;
  case ResultConvention::Owned:
  case ResultConvention::Unowned:
  case ResultConvention::UnownedInnerPointer:
  case ResultConvention::Autoreleased:
    return false;
  }

  llvm_unreachable("Unhandled ResultConvention in switch.");
}

inline SILType SILModuleConventions::getSILParamType(SILParameterInfo param,
                                                     bool loweredAddresses) {
  return SILModuleConventions::isIndirectSILParam(param, loweredAddresses)
             ? SILType::getPrimitiveAddressType(param.getType())
             : SILType::getPrimitiveObjectType(param.getType());
}

inline SILType SILModuleConventions::getSILResultType(SILResultInfo result,
                                                      bool loweredAddresses) {
  return SILModuleConventions::isIndirectSILResult(result, loweredAddresses)
             ? SILType::getPrimitiveAddressType(result.getType())
             : SILType::getPrimitiveObjectType(result.getType());
}

inline SILType SILParameterInfo::getSILStorageType() const {
  return SILModuleConventions::getSILParamType(*this, true);
}

inline SILType SILResultInfo::getSILStorageType() const {
  return SILModuleConventions::getSILResultType(*this, true);
}

} // end swift namespace

#endif
