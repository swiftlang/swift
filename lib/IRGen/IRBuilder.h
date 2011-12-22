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
#include "Address.h"
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

  /// Determines if the current location is apparently reachable.  The
  /// invariant we maintain is that the insertion point of the builder
  /// always points within a block unless the current location is
  /// logically unreachable.  All the low-level routines which emit
  /// branches leave the insertion point in the original block, just
  /// after the branch.  High-level routines may then indicate
  /// unreachability by clearing the insertion point.
  bool hasValidIP() const { return GetInsertBlock() != nullptr; }

  /// Determines whether we're currently inserting after a terminator.
  /// This is really just there for asserts.
  bool hasPostTerminatorIP() const {
    return GetInsertBlock() != nullptr &&
           !GetInsertBlock()->empty() &&
           isa<llvm::TerminatorInst>(GetInsertBlock()->back());
  }

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
  llvm::LoadInst *CreateLoad(llvm::Value *addr, Alignment align,
                             const llvm::Twine &name = "") {
    llvm::LoadInst *load = CreateLoad(addr, name);
    load->setAlignment(align.getValue());
    return load;
  }
  llvm::LoadInst *CreateLoad(Address addr) {
    return CreateLoad(addr.getAddress(), addr.getAlignment());
  }

  using IRBuilderBase::CreateStore;
  llvm::StoreInst *CreateStore(llvm::Value *value, llvm::Value *addr,
                               Alignment align) {
    llvm::StoreInst *store = CreateStore(value, addr);
    store->setAlignment(align.getValue());
    return store;
  }
  llvm::StoreInst *CreateStore(llvm::Value *value, Address addr) {
    return CreateStore(value, addr.getAddress(), addr.getAlignment());
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
