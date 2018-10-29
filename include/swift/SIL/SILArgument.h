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
class SILUndef;

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

struct SILArgumentKind {
  enum innerty : std::underlying_type<ValueKind>::type {
#define ARGUMENT(ID, PARENT) ID = unsigned(SILNodeKind::ID),
#define ARGUMENT_RANGE(ID, FIRST, LAST) First_##ID = FIRST, Last_##ID = LAST,
#include "swift/SIL/SILNodes.def"
  } value;

  explicit SILArgumentKind(ValueKind kind)
      : value(*SILArgumentKind::fromValueKind(kind)) {}
  SILArgumentKind(innerty value) : value(value) {}
  operator innerty() const { return value; }

  static Optional<SILArgumentKind> fromValueKind(ValueKind kind) {
    switch (kind) {
#define ARGUMENT(ID, PARENT)                                                   \
  case ValueKind::ID:                                                          \
    return SILArgumentKind(ID);
#include "swift/SIL/SILNodes.def"
    default:
      return None;
    }
  }
};

class SILArgument : public ValueBase {
  void operator=(const SILArgument &) = delete;
  void operator delete(void *Ptr, size_t) SWIFT_DELETE_OPERATOR_DELETED

  SILBasicBlock *ParentBB;
  const ValueDecl *Decl;

public:
  ValueOwnershipKind getOwnershipKind() const {
    return static_cast<ValueOwnershipKind>(Bits.SILArgument.VOKind);
  }
  void setOwnershipKind(ValueOwnershipKind NewKind) {
    Bits.SILArgument.VOKind = static_cast<unsigned>(NewKind);
  }

  SILBasicBlock *getParent() { return ParentBB; }
  const SILBasicBlock *getParent() const { return ParentBB; }

  SILFunction *getFunction();
  const SILFunction *getFunction() const;

  SILModule &getModule() const;

  const ValueDecl *getDecl() const { return Decl; }

  static bool classof(const SILInstruction *) = delete;
  static bool classof(const SILUndef *) = delete;
  static bool classof(const SILNode *node) {
    return node->getKind() >= SILNodeKind::First_SILArgument &&
           node->getKind() <= SILNodeKind::Last_SILArgument;
  }

  unsigned getIndex() const {
    ArrayRef<SILArgument *> Args = getParent()->getArguments();
    for (unsigned i = 0, e = Args.size(); i != e; ++i)
      if (Args[i] == this)
        return i;
    llvm_unreachable("SILArgument not argument of its parent BB");
  }

  /// Return true if this block argument is actually a phi argument as
  /// opposed to a cast or projection.
  bool isPhiArgument();

  /// If this argument is a phi, return the incoming phi value for the given
  /// predecessor BB. If this argument is not a phi, return an invalid SILValue.
  SILValue getIncomingPhiValue(SILBasicBlock *predBB);

  /// If this argument is a phi, populate `OutArray` with the incoming phi
  /// values for each predecessor BB. If this argument is not a phi, return
  /// false.
  bool getIncomingPhiValues(llvm::SmallVectorImpl<SILValue> &ReturnedPhiValues);

  /// If this argument is a phi, populate `OutArray` with each predecessor block
  /// and its incoming phi value. If this argument is not a phi, return false.
  bool getIncomingPhiValues(
      llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>>
          &ReturnedPredAndPhiValuePairs);

  /// Returns true if we were able to find a single terminator operand value for
  /// each predecessor of this arguments basic block. The found values are
  /// stored in OutArray.
  ///
  /// Note: this peeks through any projections or cast implied by the
  /// terminator. e.g. the incoming value for a switch_enum payload argument is
  /// the enum itself (the operand of the switch_enum).
  bool getSingleTerminatorOperands(llvm::SmallVectorImpl<SILValue> &OutArray);

  /// Returns true if we were able to find single terminator operand values for
  /// each predecessor of this arguments basic block. The found values are
  /// stored in OutArray alongside their predecessor block.
  ///
  /// Note: this peeks through any projections or cast implied by the
  /// terminator. e.g. the incoming value for a switch_enum payload argument is
  /// the enum itself (the operand of the switch_enum).
  bool getSingleTerminatorOperands(
      llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray);

  /// If this SILArgument's parent block has a single predecessor whose
  /// terminator has a single operand, return the incoming operand of the
  /// predecessor's terminator. Returns SILValue() otherwise.  Note that for
  /// some predecessor terminators the incoming value is not exactly the
  /// argument value. E.g. the incoming value for a switch_enum payload argument
  /// is the enum itself (the operand of the switch_enum).
  SILValue getSingleTerminatorOperand() const;

  /// Return the SILArgumentKind of this argument.
  SILArgumentKind getKind() const {
    return SILArgumentKind(ValueBase::getKind());
  }

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
      : ValueBase(SubClassKind, Ty, IsRepresentative::Yes), ParentBB(nullptr),
        Decl(D) {
    Bits.SILArgument.VOKind = static_cast<unsigned>(OwnershipKind);
  }
  void setParent(SILBasicBlock *P) { ParentBB = P; }

  friend SILBasicBlock;
};

class SILPhiArgument : public SILArgument {
public:
  /// Return true if this is block argument is actually a phi argument as
  /// opposed to a cast or projection.
  bool isPhiArgument();

  /// If this argument is a phi, return the incoming phi value for the given
  /// predecessor BB. If this argument is not a phi, return an invalid SILValue.
  ///
  /// FIXME: Once SILPhiArgument actually implies that it is a phi argument,
  /// this will be guaranteed to return a valid SILValue.
  SILValue getIncomingPhiValue(SILBasicBlock *BB);

  /// If this argument is a phi, populate `OutArray` with the incoming phi
  /// values for each predecessor BB. If this argument is not a phi, return
  /// false.
  ///
  /// FIXME: Once SILPhiArgument actually implies that it is a phi argument,
  /// this will always succeed.
  bool getIncomingPhiValues(llvm::SmallVectorImpl<SILValue> &OutArray);

  /// If this argument is a phi, populate `OutArray` with each predecessor block
  /// and its incoming phi value. If this argument is not a phi, return false.
  ///
  /// FIXME: Once SILPhiArgument actually implies that it is a phi argument,
  /// this will always succeed.
  bool getIncomingPhiValues(
      llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray);

  /// Returns true if we were able to find a single terminator operand value for
  /// each predecessor of this arguments basic block. The found values are
  /// stored in OutArray.
  ///
  /// Note: this peeks through any projections or cast implied by the
  /// terminator. e.g. the incoming value for a switch_enum payload argument is
  /// the enum itself (the operand of the switch_enum).
  bool getSingleTerminatorOperands(llvm::SmallVectorImpl<SILValue> &OutArray);

  /// Returns true if we were able to find single terminator operand values for
  /// each predecessor of this arguments basic block. The found values are
  /// stored in OutArray alongside their predecessor block.
  ///
  /// Note: this peeks through any projections or cast implied by the
  /// terminator. e.g. the incoming value for a switch_enum payload argument is
  /// the enum itself (the operand of the switch_enum).
  bool getSingleTerminatorOperands(
      llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray);

  /// If this SILArgument's parent block has a single predecessor whose
  /// terminator has a single operand, return the incoming operand of the
  /// predecessor's terminator. Returns SILValue() otherwise.  Note that for
  /// some predecessor terminators the incoming value is not exactly the
  /// argument value. E.g. the incoming value for a switch_enum payload argument
  /// is the enum itself (the operand of the switch_enum).
  SILValue getSingleTerminatorOperand() const;

  static bool classof(const SILInstruction *) = delete;
  static bool classof(const SILUndef *) = delete;
  static bool classof(const SILNode *node) {
    return node->getKind() == SILNodeKind::SILPhiArgument;
  }

private:
  friend SILBasicBlock;
  SILPhiArgument(SILBasicBlock *ParentBB, SILType Ty, ValueOwnershipKind OwnershipKind,
                 const ValueDecl *D = nullptr)
      : SILArgument(ValueKind::SILPhiArgument, ParentBB, Ty, OwnershipKind, D) {}
  SILPhiArgument(SILBasicBlock *ParentBB, SILBasicBlock::arg_iterator Pos,
                 SILType Ty, ValueOwnershipKind OwnershipKind,
                 const ValueDecl *D = nullptr)
      : SILArgument(ValueKind::SILPhiArgument, ParentBB, Pos, Ty, OwnershipKind, D) {}

  // A special constructor, only intended for use in
  // SILBasicBlock::replacePHIArg.
  explicit SILPhiArgument(SILType Ty, ValueOwnershipKind OwnershipKind,
                          const ValueDecl *D = nullptr)
      : SILArgument(ValueKind::SILPhiArgument, Ty, OwnershipKind, D) {}
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

  static bool classof(const SILInstruction *) = delete;
  static bool classof(const SILUndef *) = delete;
  static bool classof(const SILNode *node) {
    return node->getKind() == SILNodeKind::SILFunctionArgument;
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

inline bool SILArgument::isPhiArgument() {
  if (auto *phiArg = dyn_cast<SILPhiArgument>(this))
    return phiArg->isPhiArgument();

  return false;
}

inline SILValue SILArgument::getIncomingPhiValue(SILBasicBlock *BB) {
  if (isa<SILFunctionArgument>(this))
    return SILValue();
  return cast<SILPhiArgument>(this)->getIncomingPhiValue(BB);
}

inline bool
SILArgument::getIncomingPhiValues(llvm::SmallVectorImpl<SILValue> &OutArray) {
  if (isa<SILFunctionArgument>(this))
    return false;
  return cast<SILPhiArgument>(this)->getIncomingPhiValues(OutArray);
}

inline bool SILArgument::getIncomingPhiValues(
    llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray) {
  if (isa<SILFunctionArgument>(this))
    return false;
  return cast<SILPhiArgument>(this)->getIncomingPhiValues(OutArray);
}

inline bool SILArgument::getSingleTerminatorOperands(
    llvm::SmallVectorImpl<SILValue> &OutArray) {
  if (isa<SILFunctionArgument>(this))
    return false;
  return cast<SILPhiArgument>(this)->getSingleTerminatorOperands(OutArray);
}

inline bool SILArgument::getSingleTerminatorOperands(
    llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray) {
  if (isa<SILFunctionArgument>(this))
    return false;
  return cast<SILPhiArgument>(this)->getSingleTerminatorOperands(OutArray);
}

} // end swift namespace

#endif
