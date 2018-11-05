//===--- SyntaxParsingCache.cpp - Incremental syntax parsing lookup--------===//
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

#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Syntax/SyntaxVisitor.h"

using namespace swift;
using namespace swift::syntax;

void SyntaxParsingCache::addEdit(size_t Start, size_t End,
                                 size_t ReplacementLength) {
  assert((Edits.empty() || Edits.back().End <= Start) &&
         "'Start' must be greater than or equal to 'End' of the previous edit");
  Edits.emplace_back(Start, End, ReplacementLength);
}

bool SyntaxParsingCache::nodeCanBeReused(const Syntax &Node, size_t NodeStart,
                                         size_t Position,
                                         SyntaxKind Kind) const {
  // Computing the value of NodeStart on the fly is faster than determining a
  // node's absolute position, but make sure the values match in an assertion
  // build
  assert(NodeStart == Node.getAbsolutePositionBeforeLeadingTrivia().getOffset());

  if (NodeStart != Position)
    return false;
  if (Node.getKind() != Kind)
    return false;

  // Node can also not be reused if an edit has been made in the next token's
  // text, e.g. because `private struct Foo {}` parses as a CodeBlockItem with a
  // StructDecl inside and `private struc Foo {}` parses as two CodeBlockItems
  // one for `private` and one for `struc Foo {}`
  size_t NextLeafNodeLength = 0;
  if (auto NextNode = Node.getData().getNextNode()) {
    auto NextLeafNode = NextNode->getFirstToken();
    auto NextRawNode = NextLeafNode->getRaw();
    assert(NextRawNode->isPresent());
    NextLeafNodeLength += NextRawNode->getTokenText().size();
    for (auto TriviaPiece : NextRawNode->getLeadingTrivia()) {
      NextLeafNodeLength += TriviaPiece.getTextLength();
    }
  }

  auto NodeEnd = NodeStart + Node.getTextLength();
  for (auto Edit : Edits) {
    // Check if this node or the trivia of the next node has been edited. If it
    // has, we cannot reuse it.
    if (Edit.intersectsOrTouchesRange(NodeStart, NodeEnd + NextLeafNodeLength))
      return false;
  }

  return true;
}

llvm::Optional<Syntax> SyntaxParsingCache::lookUpFrom(const Syntax &Node,
                                                      size_t NodeStart,
                                                      size_t Position,
                                                      SyntaxKind Kind) {
  if (nodeCanBeReused(Node, NodeStart, Position, Kind)) {
    return Node;
  }

  // Compute the child's position on the fly
  size_t ChildStart = NodeStart;
  for (size_t I = 0, E = Node.getNumChildren(); I < E; ++I) {
    llvm::Optional<Syntax> Child = Node.getChild(I);
    if (!Child.hasValue() || Child->isMissing()) {
      continue;
    }
    auto ChildEnd = ChildStart + Child->getTextLength();
    if (ChildStart <= Position && Position < ChildEnd) {
      return lookUpFrom(Child.getValue(), ChildStart, Position, Kind);
    }
    // The next child starts where the previous child ended
    ChildStart = ChildEnd;
  }
  return llvm::None;
}

Optional<size_t>
SyntaxParsingCache::translateToPreEditPosition(size_t PostEditPosition,
                                               ArrayRef<SourceEdit> Edits) {
  size_t Position = PostEditPosition;
  for (auto &Edit : Edits) {
    if (Edit.Start > Position)
      // Remaining edits doesn't affect the position. (Edits are sorted)
      break;
    if (Edit.Start + Edit.ReplacementLength > Position)
      // This is a position inserted by the edit, and thus doesn't exist in the
      // pre-edit version of the file.
      return None;

    Position = Position - Edit.ReplacementLength + Edit.originalLength();
  }
  return Position;
}

llvm::Optional<Syntax> SyntaxParsingCache::lookUp(size_t NewPosition,
                                                  SyntaxKind Kind) {
  Optional<size_t> OldPosition = translateToPreEditPosition(NewPosition, Edits);
  if (!OldPosition.hasValue())
    return None;

  auto Node = lookUpFrom(OldSyntaxTree, /*NodeStart=*/0, *OldPosition, Kind);
  if (Node.hasValue()) {
    ReusedNodeIds.insert(Node->getId());
  }
  return Node;
}

std::vector<SyntaxReuseRegion>
SyntaxParsingCache::getReusedRegions(const SourceFileSyntax &SyntaxTree) const {
  /// Determines the reused source regions from reused syntax node IDs
  class ReusedRegionsCollector : public SyntaxVisitor {
    std::unordered_set<SyntaxNodeId> ReusedNodeIds;
    std::vector<SyntaxReuseRegion> ReusedRegions;

    bool didReuseNode(SyntaxNodeId NodeId) {
      return ReusedNodeIds.count(NodeId) > 0;
    }

  public:
    ReusedRegionsCollector(std::unordered_set<SyntaxNodeId> ReusedNodeIds)
        : ReusedNodeIds(ReusedNodeIds) {}

    const std::vector<SyntaxReuseRegion> &getReusedRegions() {
      std::sort(ReusedRegions.begin(), ReusedRegions.end(),
                [](const SyntaxReuseRegion &Lhs,
                   const SyntaxReuseRegion &Rhs) -> bool {
                  return Lhs.Start.getOffset() < Rhs.Start.getOffset();
                });
      return ReusedRegions;
    }

    void visit(Syntax Node) override {
      if (didReuseNode(Node.getId())) {
        // Node has been reused, add it to the list
        auto Start = Node.getAbsolutePositionBeforeLeadingTrivia();
        auto End = Node.getAbsoluteEndPositionAfterTrailingTrivia();
        ReusedRegions.push_back({Start, End});
      } else {
        SyntaxVisitor::visit(Node);
      }
    }

    void collectReusedRegions(SourceFileSyntax Node) {
      assert(ReusedRegions.empty() &&
             "ReusedRegionsCollector cannot be reused");
      Node.accept(*this);
    }
  };

  ReusedRegionsCollector ReuseRegionsCollector(getReusedNodeIds());
  ReuseRegionsCollector.collectReusedRegions(SyntaxTree);
  return ReuseRegionsCollector.getReusedRegions();
}
