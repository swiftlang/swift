//===--- OwnershipConventions.h - Foreign ownership conventions -*- C++ -*-===//
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
//
// This file defines functions for deriving the retain/release conventions of
// non-Swift functions.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_LOWERING_OWNERSHIP_CONVENTIONS_H__
#define __SWIFT_LOWERING_OWNERSHIP_CONVENTIONS_H__

#include "llvm/ADT/SmallBitVector.h"

namespace clang {
  class Decl;
}

namespace swift {
  class SILType;
  class SILFunctionTypeInfo;
  
namespace Lowering {
  enum class SelectorFamily : unsigned;

/// Represents the ownership conventions of a function.
class OwnershipConventions {
public:
  /// The different return modes.
  enum class Return : unsigned {
    /// The return value is retained. Normal Swift convention.
    Retained,
    /// The return value is an autoreleased ObjC object pointer. Normal ObjC
    /// convention.
    Autoreleased,
    /// The return value is an unretained value type.
    Unretained
  };
  
private:
  llvm::SmallBitVector consumedArguments;
  unsigned calleeConsumed : 1;
  Return returnKind;
  
  OwnershipConventions(bool calleeConsumed,
                       llvm::SmallBitVector &&consumedArguments,
                       Return returnKind)
    : consumedArguments(std::move(consumedArguments)),
      calleeConsumed(calleeConsumed),
      returnKind(returnKind)
  {}
  
  static OwnershipConventions getForClangDecl(clang::Decl *method,
                                              SILFunctionTypeInfo *ft);
  static OwnershipConventions getForObjCSelectorFamily(SelectorFamily family,
                                                       SILFunctionTypeInfo *ft);
  
public:
  OwnershipConventions() = default;
  
  /// Derive the ownership conventions for a SILConstant.
  static OwnershipConventions get(SILGenFunction &gen,
                                  SILConstant c,
                                  SILType ty);
  
  /// Derive the default Swift ownership conventions for a SILType, which must
  /// represent a function type.
  static OwnershipConventions getDefault(SILGenFunction &gen,
                                         SILType ty);
  
  /// True if the callee consumes itself when called.
  bool isCalleeConsumed() const { return calleeConsumed; }
  
  /// True if the callee consumes its nth SIL argument.
  bool isArgumentConsumed(unsigned n) const { return consumedArguments[n]; }

  /// Get the return kind of the function.
  Return getReturn() const { return returnKind; }
};
  
} // end namespace Lowering
} // end namespace swift

#endif