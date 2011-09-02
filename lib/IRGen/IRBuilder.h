//===--- IRBuilder.h - Swift IR Builder -------------------------*- C++ -*-===//
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
// This file defines Swift's specialization of llvm::IRBuilder.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRBUILDER_H
#define SWIFT_IRGEN_IRBUILDER_H

#include "llvm/Support/IRBuilder.h"
#include "IRGen.h"

namespace swift {
namespace irgen {

typedef llvm::IRBuilder<> IRBuilderBase;

class IRBuilder : public IRBuilderBase {
  // Without this, it keeps resolving to llvm::IRBuilderBase because
  // of the injected class name.
  typedef irgen::IRBuilderBase IRBuilderBase;

  /// The block containing the insertion point when the insertion
  /// point was last cleared.  Used only for preserving block
  /// ordering.
  llvm::BasicBlock *ClearedIP;

public:
  IRBuilder(llvm::LLVMContext &Context)
    : IRBuilderBase(Context), ClearedIP(nullptr) {}

  bool hasValidIP() const { return GetInsertBlock() != nullptr; }

  void ClearInsertionPoint() {
    assert(hasValidIP() && "clearing invalid insertion point!");
    assert(ClearedIP == nullptr);

    /// Whenever we clear the insertion point, remember where we were.
    ClearedIP = GetInsertBlock();
    IRBuilderBase::ClearInsertionPoint();
  }

  void SetInsertPoint(llvm::BasicBlock *BB) {
    ClearedIP = nullptr;
    IRBuilderBase::SetInsertPoint(BB);
  }

  using IRBuilderBase::CreateLoad;
  llvm::LoadInst *CreateLoad(llvm::Value *Addr, Alignment A,
                             const llvm::Twine &Name = "") {
    llvm::LoadInst *Load = CreateLoad(Addr, Name);
    Load->setAlignment(A.getValue());
    return Load;
  }

  using IRBuilderBase::CreateStore;
  llvm::StoreInst *CreateStore(llvm::Value *V, llvm::Value *Addr,
                               Alignment A) {
    llvm::StoreInst *Store = CreateStore(V, Addr);
    Store->setAlignment(A.getValue());
    return Store;
  }

  /// Insert the given basic block after the IP block and move the
  /// insertion point to it.  Only valid if the IP is valid.
  void emitBlock(llvm::BasicBlock *BB);

  /// Insert the given basic block "anywhere".  The IP may be invalid,
  /// in which case the block will be inserted after the block which
  /// contained the IP before the IP was invalidated.
  void emitBlockAnywhere(llvm::BasicBlock *BB);
};

} // end namespace irgen
} // end namespace swift

#endif
