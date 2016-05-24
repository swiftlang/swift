//===--- SILArgument.h - SIL BasicBlock Argument Representation -*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILARGUMENT_H
#define SWIFT_SIL_SILARGUMENT_H

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILFunction.h"

namespace swift {
  class SILBasicBlock;
  class SILModule;

/// Conventions for apply operands and function-entry arguments in SIL.
///
/// By design, this is exactly the same as ParameterConvention, plus
/// Indirect_Out.
enum class SILArgumentConvention : uint8_t {
  Indirect_In,
  Indirect_In_Guaranteed,
  Indirect_Inout,
  Indirect_InoutAliasable,
  Indirect_Out,
  Direct_Owned,
  Direct_Unowned,
  Direct_Deallocating,
  Direct_Guaranteed,
};

inline bool isIndirectConvention(SILArgumentConvention convention) {
  return convention <= SILArgumentConvention::Indirect_Out;
}

/// Turn a ParameterConvention into a SILArgumentConvention.
inline SILArgumentConvention getSILArgumentConvention(ParameterConvention conv){
  switch (conv) {
  case ParameterConvention::Indirect_In:
    return SILArgumentConvention::Indirect_In;
  case ParameterConvention::Indirect_Inout:
    return SILArgumentConvention::Indirect_Inout;
  case ParameterConvention::Indirect_InoutAliasable:
    return SILArgumentConvention::Indirect_InoutAliasable;
  case ParameterConvention::Indirect_In_Guaranteed:
    return SILArgumentConvention::Indirect_In_Guaranteed;
  case ParameterConvention::Direct_Unowned:
    return SILArgumentConvention::Direct_Unowned;
  case ParameterConvention::Direct_Guaranteed:
    return SILArgumentConvention::Direct_Guaranteed;
  case ParameterConvention::Direct_Owned:
    return SILArgumentConvention::Direct_Owned;
  case ParameterConvention::Direct_Deallocating:
    return SILArgumentConvention::Direct_Deallocating;
  }
  llvm_unreachable("covered switch isn't covered?!");
}

inline SILArgumentConvention
SILFunctionType::getSILArgumentConvention(unsigned index) const {
  assert(index <= getNumSILArguments());
  auto numIndirectResults = getNumIndirectResults();
  if (index < numIndirectResults) {
    return SILArgumentConvention::Indirect_Out;
  } else {
    auto param = getParameters()[index - numIndirectResults];
    return swift::getSILArgumentConvention(param.getConvention());
  }
}

enum class InoutAliasingAssumption {
  /// Assume that an inout indirect parameter may alias other objects.
  /// This is the safe assumption an optimization should make if it may break
  /// memory safety in case the inout aliasing rule is violation.
  Aliasing,

  /// Assume that an inout indirect parameter cannot alias other objects.
  /// Optimizations should only use this if they can guarantee that they will
  /// not break memory safety even if the inout aliasing rule is violated.
  NotAliasing
};

/// Returns true if \p conv is a not-aliasing indirect parameter.
/// The \p isInoutAliasing specifies what to assume about the inout convention.
/// See InoutAliasingAssumption.
inline bool isNotAliasedIndirectParameter(SILArgumentConvention conv,
                                     InoutAliasingAssumption isInoutAliasing) {
  switch (conv) {
  case SILArgumentConvention::Indirect_In:
  case SILArgumentConvention::Indirect_Out:
  case SILArgumentConvention::Indirect_In_Guaranteed:
    return true;

  case SILArgumentConvention::Indirect_Inout:
    return isInoutAliasing == InoutAliasingAssumption::NotAliasing;

  case SILArgumentConvention::Indirect_InoutAliasable:
  case SILArgumentConvention::Direct_Unowned:
  case SILArgumentConvention::Direct_Guaranteed:
  case SILArgumentConvention::Direct_Owned:
  case SILArgumentConvention::Direct_Deallocating:
    return false;
  }
  llvm_unreachable("covered switch isn't covered?!");
}

class SILArgument : public ValueBase {
  void operator=(const SILArgument &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

  SILBasicBlock *ParentBB;
  const ValueDecl *Decl;
public:
  SILArgument(SILBasicBlock *ParentBB, SILType Ty, const ValueDecl *D=nullptr);
  SILArgument(SILBasicBlock *ParentBB, SILBasicBlock::bbarg_iterator Pos,
              SILType Ty, const ValueDecl *D=nullptr);

  SILArgument(SILFunction::iterator ParentBB, SILType Ty,
              const ValueDecl *D = nullptr)
      : SILArgument(&*ParentBB, Ty, D) {}
  SILArgument(SILFunction::iterator ParentBB, SILBasicBlock::bbarg_iterator Pos,
              SILType Ty, const ValueDecl *D = nullptr)
      : SILArgument(&*ParentBB, Pos, Ty, D) {}

  SILBasicBlock *getParent() { return ParentBB; }
  const SILBasicBlock *getParent() const { return ParentBB; }

  SILFunction *getFunction();
  const SILFunction *getFunction() const;

  SILModule &getModule() const;

  const ValueDecl *getDecl() const { return Decl; }

  /// Returns true if this is a SILArgument of the entry BB of a function.
  bool isFunctionArg() const {
    return getParent()->isEntry();
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SILArgument;
  }

  unsigned getIndex() const {
    ArrayRef<SILArgument *> Args = getParent()->getBBArgs();
    for (unsigned i = 0, e = Args.size(); i != e; ++i)
      if (Args[i] == this)
        return i;
    llvm_unreachable("SILArgument not argument of its parent BB");
  }

  bool isIndirectResult() const {
    assert(isFunctionArg() && "Only function arguments have SILParameterInfo");
    auto numIndirectResults =
      getFunction()->getLoweredFunctionType()->getNumIndirectResults();
    return (getIndex() < numIndirectResults);
  }

  SILArgumentConvention getArgumentConvention() const {
    assert(isFunctionArg() && "Only function arguments have SILParameterInfo");
    return getFunction()->getLoweredFunctionType()
                        ->getSILArgumentConvention(getIndex());
  }

  /// Given that this is an entry block argument, and given that it does
  /// not correspond to an indirect result, return the corresponding
  /// SILParameterInfo.
  SILParameterInfo getKnownParameterInfo() const {
    assert(isFunctionArg() && "Only function arguments have SILParameterInfo");
    auto index = getIndex();
    auto fnType = getFunction()->getLoweredFunctionType();
    auto numIndirectResults = fnType->getNumIndirectResults();
    assert(index >= numIndirectResults && "Cannot be an indirect result");
    auto param = fnType->getParameters()[index - numIndirectResults];
    return param;
  }

  /// Returns the incoming SILValue from the \p BBIndex predecessor of this
  /// argument's parent BB. If the routine fails, it returns an empty SILValue.
  /// Note that for some predecessor terminators the incoming value is not
  /// exactly the argument value. E.g. the incoming value for a switch_enum
  /// payload argument is the enum itself (the operand of the switch_enum).
  SILValue getIncomingValue(unsigned BBIndex);

  /// Returns the incoming SILValue for this argument from BB. If the routine
  /// fails, it returns an empty SILValue.
  /// Note that for some predecessor terminators the incoming value is not
  /// exactly the argument value. E.g. the incoming value for a switch_enum
  /// payload argument is the enum itself (the operand of the switch_enum).
  SILValue getIncomingValue(SILBasicBlock *BB);

  /// Returns true if we were able to find incoming values for each predecessor
  /// of this arguments basic block. The found values are stored in OutArray.
  /// Note that for some predecessor terminators the incoming value is not
  /// exactly the argument value. E.g. the incoming value for a switch_enum
  /// payload argument is the enum itself (the operand of the switch_enum).
  bool getIncomingValues(llvm::SmallVectorImpl<SILValue> &OutArray);

  /// Returns true if we were able to find incoming values for each predecessor
  /// of this arguments basic block. The found values are stored in OutArray.
  /// Note that for some predecessor terminators the incoming value is not
  /// exactly the argument value. E.g. the incoming value for a switch_enum
  /// payload argument is the enum itself (the operand of the switch_enum).
  bool getIncomingValues(
      llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray);

  /// If this SILArgument's parent block has one predecessor, return the
  /// incoming value from that predecessor. Returns SILValue() otherwise.
  /// Note that for some predecessor terminators the incoming value is not
  /// exactly the argument value. E.g. the incoming value for a switch_enum
  /// payload argument is the enum itself (the operand of the switch_enum).
  SILValue getSingleIncomingValue() const;

  /// Returns true if this SILArgument is the self argument of its
  /// function. This means that this will return false always for SILArguments
  /// of SILFunctions that do not have self argument and for non-function
  /// argument SILArguments.
  bool isSelf() const;

  /// Returns true if this SILArgument is passed via the given convention.
  bool hasConvention(SILArgumentConvention P) const {
    return getArgumentConvention() == P;
  }

private:
  // A special constructor, only intended for use in SILBasicBlock::replaceBBArg.
  explicit SILArgument(SILType Ty, const ValueDecl *D =nullptr) :
    ValueBase(ValueKind::SILArgument, Ty), ParentBB(nullptr), Decl(D) {}
  friend class SILBasicBlock;
  void setParent(SILBasicBlock *P) { ParentBB = P; }
};

} // end swift namespace

#endif
