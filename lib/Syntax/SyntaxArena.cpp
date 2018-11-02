//===--- SyntaxArena.cpp - SyntaxArena implementation -----------*- C++ -*-===//
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

#include "swift/Syntax/SyntaxArena.h"
#include "swift/Syntax/RawSyntax.h"
#include "llvm/ADT/FoldingSet.h"

using namespace swift;
using namespace swift::syntax;

namespace {
class RawSyntaxCacheNode;
}

/// Implementation detail of SyntaxArena.
struct SyntaxArena::Implementation {
  /// Allocator.
  llvm::BumpPtrAllocator Allocator;

  /// List of pointers to the allocated RawSyntax
  std::vector<RawSyntax *> AllocatedRawSyntaxList;

  Implementation() = default;
  void *Allocate(size_t size, size_t alignment) {
    return Allocator.Allocate(size, alignment);
  }

  void *AllocateRawSyntax(size_t size, size_t alignment) {
    void *data = Allocator.Allocate(size, alignment);
    /// Remember the allocated pointers so that we can destruct them.
    AllocatedRawSyntaxList.push_back(static_cast<RawSyntax *>(data));
    return data;
  }

  ~Implementation() {
    // Destruct all allocated RawSyntax. They might contain heap allocated
    // propeties and/or children.
    for (auto *N : AllocatedRawSyntaxList)
      N->~RawSyntax();
  }
};

SyntaxArena::SyntaxArena() : Impl(*new Implementation()){};
SyntaxArena::~SyntaxArena() { delete &Impl; }

llvm::BumpPtrAllocator &SyntaxArena::getAllocator() const {
  return Impl.Allocator;
}

void *SyntaxArena::Allocate(size_t size, size_t alignment) {
  return Impl.Allocate(size, alignment);
}

void *SyntaxArena::AllocateRawSyntax(size_t size, size_t alignment) {
  return Impl.AllocateRawSyntax(size, alignment);
}

namespace {

/// Cache node for RawSyntax.
class RawSyntaxCacheNode : public llvm::FoldingSetNode {

  friend llvm::FoldingSetTrait<RawSyntaxCacheNode>;

  /// Associated RawSyntax.
  RawSyntax *Obj;
  /// FoldingSet node identifier of the associated RawSyntax.
  llvm::FoldingSetNodeIDRef IDRef;

public:
  RawSyntaxCacheNode(RawSyntax *Obj, const llvm::FoldingSetNodeIDRef IDRef)
      : Obj(Obj), IDRef(IDRef) {}

  /// Retrieve assciated RawSyntax.
  RawSyntax *get() { return Obj; }

  // Only allow allocation of Node using the allocator in SyntaxArena.
  void *operator new(size_t Bytes, SyntaxArena &Arena,
                     unsigned Alignment = alignof(RawSyntaxCacheNode)) {
    return Arena.Allocate(Bytes, Alignment);
  }

  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
};

} // namespace

namespace llvm {

/// FoldingSet traits for RawSyntax wrapper.
template <> struct FoldingSetTrait<RawSyntaxCacheNode> {

  static inline void Profile(RawSyntaxCacheNode &X, FoldingSetNodeID &ID) {
    ID.AddNodeID(X.IDRef);
  }

  static inline bool Equals(RawSyntaxCacheNode &X, const FoldingSetNodeID &ID,
                            unsigned, FoldingSetNodeID &) {
    return ID == X.IDRef;
  }
  static inline unsigned ComputeHash(RawSyntaxCacheNode &X,
                                     FoldingSetNodeID &) {
    return X.IDRef.ComputeHash();
  }
};

} // namespace llvm

/// Retrive "token" RawSyntax from the given Arena.
RC<RawSyntax> RawSyntax::getToken(SyntaxArena &Arena, tok TokKind,
                                  OwnedString Text,
                                  llvm::ArrayRef<TriviaPiece> LeadingTrivia,
                                  llvm::ArrayRef<TriviaPiece> TrailingTrivia) {

  return RawSyntax::make(TokKind, Text, LeadingTrivia, TrailingTrivia,
                         SourcePresence::Present, &Arena);
}
