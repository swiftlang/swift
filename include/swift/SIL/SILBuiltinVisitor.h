//===--- SILBuiltinVisitor.h ------------------------------------*- C++ -*-===//
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
///
/// \file
///
/// This file contains SILBuiltinVisitor, a visitor for visiting all possible
/// builtins and llvm intrinsics able to be used by BuiltinInst.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBUILTINVISITOR_H
#define SWIFT_SIL_SILBUILTINVISITOR_H

#include "swift/SIL/SILInstruction.h"
#include <type_traits>

namespace swift {

template <typename ImplClass, typename ValueRetTy = void>
class SILBuiltinVisitor {
public:
  ImplClass &asImpl() { return static_cast<ImplClass &>(*this); }

  /// Perform any required pre-processing before visiting.
  ///
  /// Sub-classes can override this method to provide custom pre-processing.
  void beforeVisit(BuiltinInst *BI) {}

  ValueRetTy visit(BuiltinInst *BI) {
    asImpl().beforeVisit(BI);

    if (auto BuiltinKind = BI->getBuiltinKind()) {
      switch (BuiltinKind.getValue()) {
#define BUILTIN(ID, NAME, ATTRS)                                               \
  case BuiltinValueKind::ID:                                                   \
    return asImpl().visit##ID(BI, ATTRS);
#include "swift/AST/Builtins.def"
      case BuiltinValueKind::None:
        llvm_unreachable("None case");
      }
      llvm_unreachable("Not all cases handled?!");
    }

    if (auto IntrinsicID = BI->getIntrinsicID()) {
      return asImpl().visitLLVMIntrinsic(BI, IntrinsicID.getValue());
    }
    llvm_unreachable("Not all cases handled?!");
  }

  ValueRetTy visitLLVMIntrinsic(BuiltinInst *BI, llvm::Intrinsic::ID ID) {
    return ValueRetTy();
  }

  ValueRetTy visitBuiltinValueKind(BuiltinInst *BI, BuiltinValueKind Kind,
                                   StringRef Attrs) {
    return ValueRetTy();
  }

#define BUILTIN(ID, NAME, ATTRS)                                               \
  ValueRetTy visit##ID(BuiltinInst *BI, StringRef) {                           \
    return asImpl().visitBuiltinValueKind(BI, BuiltinValueKind::ID, ATTRS);    \
  }
#include "swift/AST/Builtins.def"
};

} // end swift namespace

#endif
