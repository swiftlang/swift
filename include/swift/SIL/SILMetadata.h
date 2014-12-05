//===--- SILMetadata.h - Metadata for SIL -----------------------*- C++ -*-===//
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

#ifndef SWIFT_SIL_METADATA_H
#define SWIFT_SIL_METADATA_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"

namespace swift {

class SILModule;

/// SILMetadata is the base class. SILModule will hold the FoldingSet of all
/// SILMetadata for uniquing purpose. Each derived SILMetadata will have its
/// own ID.
class SILMetadata : public llvm::FoldingSetNode {
public:
  enum class MDKind {
    BranchKind
  };
  MDKind getKind() const { return Kind; }
  void Profile(llvm::FoldingSetNodeID &ID);
  static MDKind getMDKind(llvm::StringRef Name);

protected:
  const MDKind  Kind;
  SILMetadata(MDKind kind);
};

/// Describes the branch weight metadata for PGO. It includes an array
/// of uint32_t.
class SILBranchNode : public SILMetadata {
private:
  friend class SILModule;
  unsigned NumOperands;
  SILBranchNode(unsigned numOps);
  SILBranchNode(llvm::ArrayRef<uint32_t> weights);

  llvm::MutableArrayRef<uint32_t> getMutableWeights() {
    return { reinterpret_cast<uint32_t *>(this + 1), NumOperands };
  }

public:
  static SILBranchNode *get(SILModule &M, llvm::ArrayRef<uint32_t> weights);

  llvm::ArrayRef<uint32_t> getWeights() const {
    return { reinterpret_cast<const uint32_t *>(this + 1), NumOperands };
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getWeights());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      llvm::ArrayRef<uint32_t> weights);

  static bool classof(const SILMetadata *S) {
    return S->getKind() == MDKind::BranchKind;
  }
};

} // end swift namespace

#endif
