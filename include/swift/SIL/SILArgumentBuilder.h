//===--- SILArgumentBuilder.h --------------------------------*- c++ -*----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILARGUMENTBUILDER_H
#define SWIFT_SIL_SILARGUMENTBUILDER_H

#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"

namespace swift {

class SILPhiArgument;
struct ValueOwnershipKind;
class ValueDecl;
class SILType;

class SILArgumentBuilder {
  SILBasicBlock *block;
  bool disableEntryBlockVerification;

public:
  SILArgumentBuilder(SILBasicBlock *block,
                     bool disableEntryBlockVerification = false)
      : block(block),
        disableEntryBlockVerification(disableEntryBlockVerification) {}

  using arg_iterator = SILBasicBlock::arg_iterator;

  // Creation routines for non function arguments.
#define ARGUMENT_BOILERPLATE(CLASSNAME, PRETTYNAME)                            \
  CLASSNAME *replace##PRETTYNAME(unsigned i, SILType type,                     \
                                 ValueOwnershipKind kind,                      \
                                 const ValueDecl *decl = nullptr) {            \
    auto *newArg =                                                             \
        replaceArgument(SILArgumentKind::CLASSNAME, i, type, kind, decl);      \
    return cast<CLASSNAME>(newArg);                                            \
  }                                                                            \
  CLASSNAME *replace##PRETTYNAME##AndReplaceAllUses(                           \
      unsigned i, SILType type, ValueOwnershipKind kind,                       \
      const ValueDecl *decl = nullptr) {                                       \
    auto *newArg = replaceArgumentAndReplaceAllUses(                           \
        SILArgumentKind::CLASSNAME, i, type, kind, decl);                      \
    return cast<CLASSNAME>(newArg);                                            \
  }                                                                            \
  CLASSNAME *create##PRETTYNAME(SILType type,                                  \
                                ValueOwnershipKind ownershipKind,              \
                                const ValueDecl *decl = nullptr) {             \
    auto *newArg =                                                             \
        createArgument(SILArgumentKind::CLASSNAME, type, ownershipKind, decl); \
    return cast<CLASSNAME>(newArg);                                            \
  }                                                                            \
  CLASSNAME *insert##PRETTYNAME(arg_iterator insertPt, SILType type,           \
                                ValueOwnershipKind ownershipKind,              \
                                const ValueDecl *decl = nullptr) {             \
    auto *newArg = insertArgument(SILArgumentKind::CLASSNAME, insertPt, type,  \
                                  ownershipKind, decl);                        \
    return cast<CLASSNAME>(newArg);                                            \
  }                                                                            \
  CLASSNAME *insert##PRETTYNAME(unsigned index, SILType type,                  \
                                ValueOwnershipKind ownershipKind,              \
                                const ValueDecl *decl = nullptr) {             \
    arg_iterator insertPt = std::next(block->args_begin(), index);             \
    return insert##PRETTYNAME(insertPt, type, ownershipKind, decl);            \
  }
  ARGUMENT_BOILERPLATE(SILFunctionArgument, FunctionArgument)
  ARGUMENT_BOILERPLATE(SILPhiArgument, PhiArgument)
#undef ARGUMENT_BOILERPLATE

private:
  //===---
  // PImpl Declarations for non function arguments
  //

  /// Replace the \p{i}th BB arg with a new BBArg with SILType \p Ty and
  /// ValueDecl \p D.
  ///
  /// NOTE: This assumes that the current argument in position \p i has had its
  /// uses eliminated. To replace/replace all uses with, use
  /// replaceArgumentAndRAUW.
  SILArgument *replaceArgument(SILArgumentKind argKind, unsigned i,
                               SILType type, ValueOwnershipKind kind,
                               const ValueDecl *decl = nullptr);

  /// Replace phi argument \p i and RAUW all uses.
  SILArgument *
  replaceArgumentAndReplaceAllUses(SILArgumentKind argKind, unsigned i,
                                   SILType type, ValueOwnershipKind kind,
                                   const ValueDecl *decl = nullptr);

  /// Allocate a new argument of type \p Ty and append it to the argument
  /// list. Optionally you can pass in a value decl parameter.
  SILArgument *createArgument(SILArgumentKind argKind, SILType type,
                              ValueOwnershipKind ownershipKind,
                              const ValueDecl *decl = nullptr);

  /// Insert a new SILArgument with type \p Ty and \p Decl at
  /// position \p Pos.
  SILArgument *insertArgument(SILArgumentKind argKind, arg_iterator insertPt,
                              SILType type, ValueOwnershipKind ownershipKind,
                              const ValueDecl *decl = nullptr);

  template <typename... ArgTys>
  SILArgument *constructArgumentInternal(SILArgumentKind argKind,
                                         ArgTys &&... argTys);

  void validateEntryBlock(SILArgumentKind kind) const;
};

} // namespace swift

#endif
