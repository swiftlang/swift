//===--- RawSyntaxTokenCache.cpp - Raw Token Cache --------------*- C++ -*-===//
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

#include "RawSyntaxTokenCache.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/ArrayRef.h"

using namespace swift;
using namespace swift::syntax;

static bool shouldCacheNode(tok TokKind, size_t TextSize,
                            ArrayRef<TriviaPiece> LeadingTrivia,
                            ArrayRef<TriviaPiece> TrailingTrivia) {
  // Is string_literal with >16 length.
  if (TokKind == tok::string_literal && TextSize > 16) {
    return false;
  }

  // Has leading comment trivia et al.
  if (any_of(LeadingTrivia,
             [](const syntax::TriviaPiece &T) { return T.getText().size(); })) {
    return false;
  }

  // Has trailing comment trivia et al.
  if (any_of(TrailingTrivia,
             [](const syntax::TriviaPiece &T) { return T.getText().size(); })) {
    return false;
  }

  // We can cache the node
  return true;
}

RC<RawSyntax>
RawSyntaxTokenCache::getToken(RC<SyntaxArena> &Arena, tok TokKind,
                              OwnedString Text,
                              ArrayRef<TriviaPiece> LeadingTrivia,
                              ArrayRef<TriviaPiece> TrailingTrivia) {
  // Determine whether this token is worth to cache.
  if (!shouldCacheNode(TokKind, Text.size(), LeadingTrivia, TrailingTrivia)) {
    // Do not use cache.
    return RawSyntax::make(TokKind, Text, LeadingTrivia, TrailingTrivia,
                           SourcePresence::Present, Arena);
  }

  // This node is cacheable. Get or create.
  llvm::FoldingSetNodeID ID;
  RawSyntax::Profile(ID, TokKind, Text.str(), LeadingTrivia, TrailingTrivia);

  void *insertPos = nullptr;
  if (auto existing = CachedTokens.FindNodeOrInsertPos(ID, insertPos)) {
    // Found in the cache. Just return it.
    return existing->get();
  }

  // Could not found in the cache. Create it.
  auto Raw = RawSyntax::make(TokKind, Text, LeadingTrivia, TrailingTrivia,
                             SourcePresence::Present, Arena);
  auto IDRef = ID.Intern(Arena->getAllocator());
  auto CacheNode = new (Arena) RawSyntaxCacheNode(Raw, IDRef);
  // Keep track of the created RawSyntaxCacheNode so that we can destruct it
  // later.
  CacheNodes.push_back(CacheNode);
  CachedTokens.InsertNode(CacheNode, insertPos);
  return Raw;
}

RawSyntaxTokenCache::~RawSyntaxTokenCache() {
  // The CachedTokens folding set is no longer used. It does not, however, clean
  // up the RawSyntaxCacheNodes we created for it and would keep a strong
  // reference to their RawSyntax nodes.
  // We thus need to manually destruct the RawSyntaxCacheNodes here.
  // After all RawSyntax nodes in the current arena are disposed of, the
  // RawSyntaxCacheNode will also be destroyed, as they are allocated in that
  // arena.
  for (RawSyntaxCacheNode *Node : CacheNodes) {
    Node->~RawSyntaxCacheNode();
  }
}
