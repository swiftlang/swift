//===--- SwitchEnumBuilder.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILGEN_SWITCHENUMBUILDER_H
#define SWIFT_SILGEN_SWITCHENUMBUILDER_H

#include "Scope.h"

namespace swift {
namespace Lowering {

class SILGenFunction;

/// A cleanup scope RAII object, like FullExpr, that comes with a JumpDest for a
/// continuation block. It is intended to be used to handle switch cases.
///
/// You *must* call exit() at some point.
///
/// This scope is also exposed to the debug info.
class SwitchCaseFullExpr {
  SILGenFunction &SGF;
  Scope scope;
  CleanupLocation loc;
  NullablePtr<SILBasicBlock> contBlock;

public:
  SwitchCaseFullExpr(SILGenFunction &SGF, CleanupLocation loc);
  SwitchCaseFullExpr(SILGenFunction &SGF, CleanupLocation loc,
                     SILBasicBlock *contBlock);

  ~SwitchCaseFullExpr() = default;

  SwitchCaseFullExpr(const SwitchCaseFullExpr &) = delete;
  SwitchCaseFullExpr &operator=(const SwitchCaseFullExpr &) = delete;

  /// Pop the scope and branch to the cont block.
  void exitAndBranch(SILLocation loc, ArrayRef<SILValue> result = {});

  /// Pop the scope and do not branch to the cont block.
  void exit();
};

/// A class for building switch enums that handles all of the ownership
/// requirements for the user.
///
/// It assumes that the user passes in a block that takes in a ManagedValue and
/// returns a ManagedValue for the blocks exit argument. Should return an empty
/// ManagedValue to signal no result.
class SwitchEnumBuilder {
public:
  using NormalCaseHandler =
      std::function<void(ManagedValue, SwitchCaseFullExpr &)>;
  using DefaultCaseHandler =
      std::function<void(ManagedValue, SwitchCaseFullExpr &)>;

  enum class DefaultDispatchTime { BeforeNormalCases, AfterNormalCases };

private:
  struct NormalCaseData {
    EnumElementDecl *decl;
    SILBasicBlock *block;
    NullablePtr<SILBasicBlock> contBlock;
    NormalCaseHandler handler;
    ProfileCounter count;

    NormalCaseData(EnumElementDecl *decl, SILBasicBlock *block,
                   NullablePtr<SILBasicBlock> contBlock,
                   NormalCaseHandler handler, ProfileCounter count)
        : decl(decl), block(block), contBlock(contBlock), handler(handler),
          count(count) {}
    ~NormalCaseData() = default;
  };

  struct DefaultCaseData {
    SILBasicBlock *block;
    NullablePtr<SILBasicBlock> contBlock;
    DefaultCaseHandler handler;
    DefaultDispatchTime dispatchTime;
    ProfileCounter count;

    DefaultCaseData(SILBasicBlock *block, NullablePtr<SILBasicBlock> contBlock,
                    DefaultCaseHandler handler,
                    DefaultDispatchTime dispatchTime, ProfileCounter count)
        : block(block), contBlock(contBlock), handler(handler),
          dispatchTime(dispatchTime), count(count) {}
    ~DefaultCaseData() = default;
  };

  SILGenBuilder &builder;
  SILLocation loc;
  ManagedValue optional;
  llvm::Optional<DefaultCaseData> defaultBlockData;
  llvm::SmallVector<NormalCaseData, 8> caseDataArray;

public:
  SwitchEnumBuilder(SILGenBuilder &builder, SILLocation loc,
                    ManagedValue optional)
      : builder(builder), loc(loc), optional(optional) {}

  void addDefaultCase(
      SILBasicBlock *defaultBlock, NullablePtr<SILBasicBlock> contBlock,
      DefaultCaseHandler handle,
      DefaultDispatchTime dispatchTime = DefaultDispatchTime::AfterNormalCases,
      ProfileCounter count = ProfileCounter()) {
    defaultBlockData.emplace(defaultBlock, contBlock, handle, dispatchTime,
                             count);
  }

  void addCase(EnumElementDecl *decl, SILBasicBlock *caseBlock,
               NullablePtr<SILBasicBlock> contBlock, NormalCaseHandler handle,
               ProfileCounter count = ProfileCounter()) {
    caseDataArray.emplace_back(decl, caseBlock, contBlock, handle, count);
  }

  void emit() &&;

private:
  SILGenFunction &getSGF() const { return builder.getSILGenFunction(); }
};

} // end Lowering namespace
} // end swift namespace

#endif
