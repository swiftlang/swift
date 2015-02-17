//===--- SILArgument.h - SIL BasicBlock Argument Representation -*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILARGUMENT_H
#define SWIFT_SIL_SILARGUMENT_H

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILFunction.h"

namespace swift {
  class SILBasicBlock;
  class SILModule;

class SILArgument : public ValueBase {
  void operator=(const SILArgument &) = delete;
  void operator delete(void *Ptr, size_t) = delete;

  SILBasicBlock *ParentBB;
  const ValueDecl *Decl;
public:
  SILArgument(SILBasicBlock *ParentBB, SILType Ty, const ValueDecl *D=nullptr);
  SILArgument(SILBasicBlock *ParentBB, SILBasicBlock::bbarg_iterator Pos,
              SILType Ty, const ValueDecl *D=nullptr);

  /// getType() is ok since this is known to only have one type.
  SILType getType(unsigned i = 0) const { return ValueBase::getType(i); }

  SILBasicBlock *getParent() { return ParentBB; }
  const SILBasicBlock *getParent() const { return ParentBB; }

  SILFunction *getFunction();
  const SILFunction *getFunction() const;

  SILModule &getModule() const;

  const ValueDecl *getDecl() const { return Decl; }

  /// Returns true if this is a SILArgument of the entry BB of a function.
  bool isFunctionArg() const {
    return getParent() == &*getFunction()->begin();
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

  /// Returns the SILParameterInfo for this SILArgument.
  SILParameterInfo getParameterInfo() const {
    assert(isFunctionArg() && "Only function arguments have SILParameterInfo");
    return getFunction()->getLoweredFunctionType()->getParameters()[getIndex()];
  }

  /// Returns the incoming SILValue from the \p BBIndex predecessor of this
  /// argument's parent BB. If the routine fails, it returns an empty SILValue.
  SILValue getIncomingValue(unsigned BBIndex);

  /// Returns the incoming SILValue for this argument from BB. If the routine
  /// fails, it returns an empty SILValue.
  SILValue getIncomingValue(SILBasicBlock *BB);

  /// Returns true if we were able to find incoming values for each predecessor
  /// of this arguments basic block. The found values are stored in OutArray.
  bool getIncomingValues(llvm::SmallVectorImpl<SILValue> &OutArray);

  /// Returns true if we were able to find incoming values for each predecessor
  /// of this arguments basic block. The found values are stored in OutArray.
  bool getIncomingValues(
      llvm::SmallVectorImpl<std::pair<SILBasicBlock *, SILValue>> &OutArray);

  /// Returns true if this SILArgument is the self argument of its
  /// function. This means that this will return false always for SILArguments
  /// of SILFunctions that do not have self argument and for non-function
  /// argument SILArguments.
  bool isSelf() const;

  /// Returns true if this SILArgument is passed via the given convention.
  bool hasConvention(ParameterConvention P) const {
    return getParameterInfo().getConvention() == P;
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
