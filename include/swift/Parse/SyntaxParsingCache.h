//===----------- SyntaxParsingCache.h -================----------*- C++ -*-===//
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

#ifndef SWIFT_PARSE_SYNTAXPARSINGCACHE_H
#define SWIFT_PARSE_SYNTAXPARSINGCACHE_H

#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include <unordered_set>

namespace swift {

using namespace swift::syntax;

/// A single edit to the original source file in which a continuous range of
/// characters have been replaced by a new string
struct SourceEdit {
  /// The byte offset from which on characters were replaced.
  size_t Start;

  /// The byte offset to which on characters were replaced.
  size_t End;

  /// The length of the string that replaced the range described above.
  size_t ReplacementLength;

  SourceEdit(size_t Start, size_t End, size_t ReplacementLength)
      : Start(Start), End(End), ReplacementLength(ReplacementLength){};

  /// The length of the range that has been replaced
  size_t originalLength() const { return End - Start; }

  /// Check if the characters replaced by this edit fall into the given range
  /// or are directly adjacent to it
  bool intersectsOrTouchesRange(size_t RangeStart, size_t RangeEnd) {
    return End >= RangeStart && Start <= RangeEnd;
  }
};

struct SyntaxReuseRegion {
  AbsolutePosition Start;
  AbsolutePosition End;
};

class SyntaxParsingCache {
  /// The syntax tree prior to the edit
  SourceFileSyntax OldSyntaxTree;

  /// The edits that were made from the source file that created this cache to
  /// the source file that is now parsed incrementally
  llvm::SmallVector<SourceEdit, 4> Edits;

  /// The IDs of all syntax nodes that got reused are collected in this vector.
  std::unordered_set<SyntaxNodeId> ReusedNodeIds;

public:
  SyntaxParsingCache(SourceFileSyntax OldSyntaxTree)
      : OldSyntaxTree(OldSyntaxTree) {}

  /// Add an edit that transformed the source file which created this cache into
  /// the source file that is now being parsed incrementally. \c Start must be a
  /// position from the *original* source file, and it must not overlap any
  /// other edits previously added. For instance, given:
  ///   (aaa, bbb)
  ///   0123456789
  /// When you want to turn this into:
  ///   (c, dddd)
  ///   0123456789
  /// edits should be: { 1, 4, 1 } and { 6, 9, 4 }.
  void addEdit(size_t Start, size_t End, size_t ReplacementLength);

  /// Check if a syntax node of the given kind at the given position can be
  /// reused for a new syntax tree.
  llvm::Optional<Syntax> lookUp(size_t NewPosition, SyntaxKind Kind);

  const std::unordered_set<SyntaxNodeId> &getReusedNodeIds() const {
    return ReusedNodeIds;
  }

  /// Get the source regions of the new source file, represented by
  /// \p SyntaxTree that have been reused as part of the incremental parse.
  std::vector<SyntaxReuseRegion>
  getReusedRegions(const SourceFileSyntax &SyntaxTree) const;

  /// Translates a post-edit position to a pre-edit position by undoing the
  /// specified edits. Returns \c None if no pre-edit position exists because
  /// the post-edit position has been inserted by an edit.
  /// 
  /// Should not be invoked externally. Only public for testing purposes.
  static Optional<size_t>
  translateToPreEditPosition(size_t PostEditPosition,
                             ArrayRef<SourceEdit> Edits);

private:
  llvm::Optional<Syntax> lookUpFrom(const Syntax &Node, size_t NodeStart,
                                    size_t Position, SyntaxKind Kind);

  bool nodeCanBeReused(const Syntax &Node, size_t Position, size_t NodeStart,
                       SyntaxKind Kind) const;
};

} // namespace swift

#endif // SWIFT_SYNTAX_PARSING_CACHE_H
