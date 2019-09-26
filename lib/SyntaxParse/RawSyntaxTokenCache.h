//===--- RawSyntaxTokenCache.h - Raw Token Cache ----------------*- C++ -*-===//
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

#ifndef SWIFT_SYNTAX_PARSE_RAWSYNTAXTOKENCACHE_H
#define SWIFT_SYNTAX_PARSE_RAWSYNTAXTOKENCACHE_H

#include "swift/Syntax/SyntaxArena.h"
#include "swift/Syntax/References.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/FoldingSet.h"
#include <vector>

namespace swift {
  enum class tok;
  class OwnedString;

namespace syntax {
  class RawSyntax;
  class TriviaPiece;
}

/// Cache node for RawSyntax.
class RawSyntaxCacheNode : public llvm::FoldingSetNode {

  friend llvm::FoldingSetTrait<RawSyntaxCacheNode>;

  /// Associated RawSyntax.
  RC<syntax::RawSyntax> Obj;
  /// FoldingSet node identifier of the associated RawSyntax.
  llvm::FoldingSetNodeIDRef IDRef;

public:
  RawSyntaxCacheNode(RC<syntax::RawSyntax> &Obj,
                     const llvm::FoldingSetNodeIDRef IDRef)
      : Obj(Obj), IDRef(IDRef) {}

  /// Retrieve assciated RawSyntax.
  RC<syntax::RawSyntax> get() { return Obj; }

  // Only allow allocation of Node using the allocator in SyntaxArena.
  void *operator new(size_t Bytes, RC<syntax::SyntaxArena> &Arena,
                     unsigned Alignment = alignof(RawSyntaxCacheNode)) {
    return Arena->Allocate(Bytes, Alignment);
  }

  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
};

class RawSyntaxTokenCache {
  llvm::FoldingSet<RawSyntaxCacheNode> CachedTokens;
  std::vector<RawSyntaxCacheNode *> CacheNodes;

public:
  RC<syntax::RawSyntax> getToken(RC<syntax::SyntaxArena> &Arena, tok TokKind,
                                 OwnedString Text,
                                 ArrayRef<syntax::TriviaPiece> LeadingTrivia,
                                 ArrayRef<syntax::TriviaPiece> TrailingTrivia);

  ~RawSyntaxTokenCache();
};

} // end namespace swift

namespace llvm {

/// FoldingSet traits for RawSyntax wrapper.
template <> struct FoldingSetTrait<swift::RawSyntaxCacheNode> {

  static inline void Profile(swift::RawSyntaxCacheNode &X,
                             FoldingSetNodeID &ID) {
    ID.AddNodeID(X.IDRef);
  }

  static inline bool Equals(swift::RawSyntaxCacheNode &X,
                            const FoldingSetNodeID &ID,
                            unsigned, FoldingSetNodeID &) {
    return ID == X.IDRef;
  }
  static inline unsigned ComputeHash(swift::RawSyntaxCacheNode &X,
                                     FoldingSetNodeID &) {
    return X.IDRef.ComputeHash();
  }
};

} // end namespace llvm

#endif
