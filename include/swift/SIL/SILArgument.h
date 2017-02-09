//===--- SILArgument.h - SIL BasicBlock Argument Representation -*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILARGUMENT_H
#define SWIFT_SIL_SILARGUMENT_H

#include "swift/Basic/Compiler.h"
#include "swift/SIL/SILArgumentConvention.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILValue.h"

namespace swift {

class SILBasicBlock;
class SILModule;

// Map an argument index onto a SILArgumentConvention.
inline SILArgumentConvention
SILFunctionConventions::getSILArgumentConvention(unsigned index) const {
  assert(index <= getNumSILArguments());
  if (index < getNumIndirectSILResults()) {
    assert(silConv.loweredAddresses);
    return SILArgumentConvention::Indirect_Out;
  } else {
    auto param = funcTy->getParameters()[index - getNumIndirectSILResults()];
    return SILArgumentConvention(param.getConvention());
  }
}

class SILArgument : public ValueBase {
  void operator=(const SILArgument &) = delete;
  void operator delete(void *Ptr, size_t) SWIFT_DELETE_OPERATOR_DELETED

  SILBasicBlock *ParentBB;
  const ValueDecl *Decl;
  ValueOwnershipKind OwnershipKind;

public:
  ValueOwnershipKind getOwnershipKind() const { return OwnershipKind; }
  void setOwnershipKind(ValueOwnershipKind NewKind) { OwnershipKind = NewKind; }

  SILBasicBlock *getParent() { return ParentBB; }
  const SILBasicBlock *getParent() const { return ParentBB; }

  SILFunction *getFunction();
  const SILFunction *getFunction() const;

  SILModule &getModule() const;

  const ValueDecl *getDecl() const { return Decl; }

  static bool classof(const ValueBase *V) {
    return V->getKind() >= ValueKind::First_SILArgument &&
           V->getKind() <= ValueKind::Last_SILArgument;
  }

  unsigned getIndex() const {
    ArrayRef<SILArgument *> Args = getParent()->getArguments();
    for (unsigned i = 0, e = Args.size(); i != e; ++i)
      if (Args[i] == this)
        return i;
    llvm_unreachable("SILArgument not argument of its parent BB");
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

protected:
  SILArgument(ValueKind SubClassKind, SILBasicBlock *ParentBB, SILType Ty,
              ValueOwnershipKind OwnershipKind,
              const ValueDecl *D = nullptr);
  SILArgument(ValueKind SubClassKind, SILBasicBlock *ParentBB,
              SILBasicBlock::arg_iterator Pos, SILType Ty,
              ValueOwnershipKind OwnershipKind,
              const ValueDecl *D = nullptr);

  // A special constructor, only intended for use in
  // SILBasicBlock::replacePHIArg and replaceFunctionArg.
  explicit SILArgument(ValueKind SubClassKind, SILType Ty,
                       ValueOwnershipKind OwnershipKind,
                       const ValueDecl *D = nullptr)
      : ValueBase(SubClassKind, Ty), ParentBB(nullptr), Decl(D), OwnershipKind(OwnershipKind) {}
  void setParent(SILBasicBlock *P) { ParentBB = P; }

  friend SILBasicBlock;
};

class SILPHIArgument : public SILArgument {
public:
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

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SILPHIArgument;
  }

private:
  friend SILBasicBlock;
  SILPHIArgument(SILBasicBlock *ParentBB, SILType Ty, ValueOwnershipKind OwnershipKind,
                 const ValueDecl *D = nullptr)
      : SILArgument(ValueKind::SILPHIArgument, ParentBB, Ty, OwnershipKind, D) {}
  SILPHIArgument(SILBasicBlock *ParentBB, SILBasicBlock::arg_iterator Pos,
                 SILType Ty, ValueOwnershipKind OwnershipKind,
                 const ValueDecl *D = nullptr)
      : SILArgument(ValueKind::SILPHIArgument, ParentBB, Pos, Ty, OwnershipKind, D) {}

  // A special constructor, only intended for use in
  // SILBasicBlock::replacePHIArg.
  explicit SILPHIArgument(SILType Ty, ValueOwnershipKind OwnershipKind,
                          const ValueDecl *D = nullptr)
      : SILArgument(ValueKind::SILPHIArgument, Ty, OwnershipKind, D) {}
};

class SILFunctionArgument : public SILArgument {
public:
  bool isIndirectResult() const {
    auto numIndirectResults =
        getFunction()->getConventions().getNumIndirectSILResults();
    return (getIndex() < numIndirectResults);
  }

  SILArgumentConvention getArgumentConvention() const {
    return getFunction()->getConventions().getSILArgumentConvention(getIndex());
  }

  /// Given that this is an entry block argument, and given that it does
  /// not correspond to an indirect result, return the corresponding
  /// SILParameterInfo.
  SILParameterInfo getKnownParameterInfo() const {
    return getFunction()->getConventions().getParamInfoForSILArg(getIndex());
  }

  /// Returns true if this SILArgument is the self argument of its
  /// function. This means that this will return false always for SILArguments
  /// of SILFunctions that do not have self argument and for non-function
  /// argument SILArguments.
  bool isSelf() const;

  /// Returns true if this SILArgument is passed via the given convention.
  bool hasConvention(SILArgumentConvention P) const {
    return getArgumentConvention() == P;
  }

  static bool classof(const ValueBase *V) {
    return V->getKind() == ValueKind::SILFunctionArgument;
  }

private:
  friend SILBasicBlock;

  SILFunctionArgument(SILBasicBlock *ParentBB, SILType Ty, ValueOwnershipKind OwnershipKind,
                      const ValueDecl *D = nullptr)
      : SILArgument(ValueKind::SILFunctionArgument, ParentBB, Ty, OwnershipKind, D) {}
  SILFunctionArgument(SILBasicBlock *ParentBB, SILBasicBlock::arg_iterator Pos,
                      SILType Ty, ValueOwnershipKind OwnershipKind, const ValueDecl *D = nullptr)
      : SILArgument(ValueKind::SILFunctionArgument, ParentBB, Pos, Ty, OwnershipKind, D) {}

  // A special constructor, only intended for use in
  // SILBasicBlock::replaceFunctionArg.
  explicit SILFunctionArgument(SILType Ty, ValueOwnershipKind OwnershipKind,
                               const ValueDecl *D = nullptr)
      : SILArgument(ValueKind::SILFunctionArgument, Ty, OwnershipKind, D) {}
};

//===----------------------------------------------------------------------===//
// Out of line Definitions for SILArgument to avoid Forward Decl issues
//===----------------------------------------------------------------------===//

inline SILValue SILArgument::getIncomingValue(unsigned BBIndex) {
  if (isa<SILFunctionArgument>(this))
    return SILValue();
  return cast<SILPHIArgument>(this)->getIncomingValue(BBIndex);
}

inline SILValue SILArgument::getIncomingValue(SILBasicBlock *BB) {
  if (isa<SILFunctionArgument>(this))
    return SILValue();
  return cast<SILPHIArgument>(this)->getIncomingValue(BB);
}

inline bool
SILArgument::getIncomingValues(llvm::SmallVectorImpl<SILValue> &OutArray) {
  if (isa<SILFunctionArgument>(this))
    return false;
  return cast<SILPHIArgument>(this)->getIncomingValues(OutArray);
}

inline bool SILArgument::getIncomingValues(
    llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray) {
  if (isa<SILFunctionArgument>(this))
    return false;
  return cast<SILPHIArgument>(this)->getIncomingValues(OutArray);
}

inline SILValue SILArgument::getSingleIncomingValue() const {
  if (isa<SILFunctionArgument>(this))
    return SILValue();
  return cast<SILPHIArgument>(this)->getSingleIncomingValue();
}

} // end swift namespace

#endif
