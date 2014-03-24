//===--- AST.h - ReST AST nodes -------------------------------------------===//
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

#ifndef LLVM_REST_AST_H
#define LLVM_REST_AST_H

#include "swift/ReST/LineList.h"
#include "llvm/Support/Casting.h"
#include "llvm/ADT/ArrayRef.h"

namespace llvm {
namespace rest {
class ReSTContext;

#define REST_AST_NODE(Id, Parent) class Id;
#include "swift/ReST/ASTNodes.def"

enum class ASTNodeKind : uint8_t {
#define REST_AST_NODE(Id, Parent) Id,
#include "swift/ReST/ASTNodes.def"
};

class alignas(void *) ReSTASTNode {
  ReSTASTNode(const ReSTASTNode &) = delete;
  void operator=(const ReSTASTNode &) = delete;

  ASTNodeKind Kind;

public:
  ReSTASTNode(ASTNodeKind Kind) : Kind(Kind) {}

  ASTNodeKind getKind() const { return Kind; }

  bool isStructuralElement() const {
    return isa<Document>(this) || isa<Section>(this) || isa<Topic>(this) ||
           isa<Sidebar>(this) || isStructuralSubelement();
  }
  bool isStructuralSubelement() const {
    return false; // FIXME
  }
  bool isBibliographicElement() const {
    return false; // FIXME
  }
  bool isDecorativeElement() const {
    return false; // FIXME
  }
  bool isBodyElement() const {
    return false; // FIXME
  }

  // Only allow allocation using the allocator in ReSTContext or by placement
  // new.
  void *operator new(size_t Bytes, ReSTContext &C,
                     unsigned Alignment = alignof(ReSTASTNode));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }

  // Make vanilla new/delete illegal for ReST AST nodes.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;
};

// Structural element.
class Document final : public ReSTASTNode {
  /// Non-null.
  // Title *TheTitle;

  /// Might be null.
  // Subtitle *TheSubtitle;
  // Decoration -- not implemented.
  // Docinfo -- not implemented.

  unsigned NumChildren;

  const ReSTASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const ReSTASTNode *const *>(this + 1);
  }
  ReSTASTNode **getChildrenBuffer() {
    return reinterpret_cast<ReSTASTNode **>(this + 1);
  }

  Document(ArrayRef<ReSTASTNode *> Children);

public:
  static Document *create(ReSTContext &C, ArrayRef<ReSTASTNode *> Children);

  /// Content model:
  /// \code
  ///    %structure.model;
  /// \endcode
  ArrayRef<const ReSTASTNode *> getChildren() const {
    return ArrayRef<const ReSTASTNode *>(getChildrenBuffer(), NumChildren);
  }
  ArrayRef<ReSTASTNode *> getChildren() {
    return ArrayRef<ReSTASTNode *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::Document;
  }
};

// Structural element.
class Section : public ReSTASTNode {
  /// Non-null.
  // Title *TheTitle;

  /// Content model:
  /// \code
  ///    %structure.model;
  /// \endcode
  ArrayRef<ReSTASTNode *> Children;

public:
  Section() : ReSTASTNode(ASTNodeKind::Section) {}

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::Section;
  }
};

// Structural element.
class Topic : public ReSTASTNode {
public:
  Topic() : ReSTASTNode(ASTNodeKind::Topic) {}

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::Topic;
  }
};

// Structural element.
class Sidebar : public ReSTASTNode {
  /// Non-null.
  // Title *TheTitle;

  /// Might be null.
  // Subtitle *TheSubtitle;

  /// Content model:
  /// \code
  ///   (%body.elements; | topic)+
  /// \endcode
  ArrayRef<ReSTASTNode *> Children;

public:
  Sidebar() : ReSTASTNode(ASTNodeKind::Sidebar) {}

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::Sidebar;
  }
};

// Structural subelement.
class Title final : public ReSTASTNode {
  // TextAndInline *Content;

public:
  Title() : ReSTASTNode(ASTNodeKind::Title) {}

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::Title;
  }
};

// Structural subelement.
class Subtitle final : public ReSTASTNode {
  // TextAndInline *Content;

public:
  Subtitle() : ReSTASTNode(ASTNodeKind::Subtitle) {}

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::Subtitle;
  }
};

// Structural subelement.
// class Decoration {};

// Structural subelement.
// class Docinfo {};

// Structural subelement.
class Transition final : public ReSTASTNode {
  // Just the source location.

public:
  Transition() : ReSTASTNode(ASTNodeKind::Transition) {}

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::Transition;
  }
};

// Simple body element.
class Paragraph final : public ReSTASTNode {
  TextAndInline *Content;

public:
  Paragraph(TextAndInline *Content)
      : ReSTASTNode(ASTNodeKind::Paragraph), Content(Content) {}

  const TextAndInline *getContent() const { return Content; }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::Paragraph;
  }
};

// Compound body element.
class BulletList final : public ReSTASTNode {
  unsigned NumItems;

public:
  struct ListItemInfo {
    unsigned FirstChildIndex;
    unsigned NumChildren;
  };

private:
  const ListItemInfo *getItemInfoBuffer() const {
    return reinterpret_cast<const ListItemInfo *>(this + 1);
  }

  ListItemInfo *getItemInfoBuffer() {
    return reinterpret_cast<ListItemInfo *>(this + 1);
  }

  ReSTASTNode *const *getItemChildrenBuffer() const {
    return reinterpret_cast<ReSTASTNode *const *>(getItemInfoBuffer() +
                                                  NumItems);
  }

  ReSTASTNode **getItemChildrenBuffer() {
    return reinterpret_cast<ReSTASTNode **>(getItemInfoBuffer() + NumItems);
  }

  BulletList(ArrayRef<ListItemInfo> ItemInfos,
             ArrayRef<ReSTASTNode *> ItemChildren);

public:
  static BulletList *create(ReSTContext &C, ArrayRef<ListItemInfo> ItemInfos,
                            ArrayRef<ReSTASTNode *> ItemChildren);

  unsigned getNumItems() const { return NumItems; }

  const ListItemInfo &getItemInfo(unsigned i) const {
    assert(i < NumItems);
    return *(getItemInfoBuffer() + i);
  }

  ArrayRef<const ReSTASTNode *> getItemChildren(unsigned i) const {
    assert(i < NumItems);
    const auto &ItemInfo = getItemInfo(i);
    return ArrayRef<const ReSTASTNode *>(getItemChildrenBuffer() +
                                             ItemInfo.FirstChildIndex,
                                         ItemInfo.NumChildren);
  }
  ArrayRef<ReSTASTNode *> getItemChildren(unsigned i) {
    assert(i < NumItems);
    const auto &ItemInfo = getItemInfo(i);
    return ArrayRef<ReSTASTNode *>(getItemChildrenBuffer() +
                                       ItemInfo.FirstChildIndex,
                                   ItemInfo.NumChildren);
  }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::BulletList;
  }
};

// Compound body element.
class EnumeratedList final : public ReSTASTNode {
  unsigned NumItems;

public:
  struct ListItemInfo {
    unsigned FirstChildIndex;
    unsigned NumChildren;
  };

private:
  const ListItemInfo *getItemInfoBuffer() const {
    return reinterpret_cast<const ListItemInfo *>(this + 1);
  }
  ListItemInfo *getItemInfoBuffer() {
    return reinterpret_cast<ListItemInfo *>(this + 1);
  }

  ReSTASTNode *const *getItemChildrenBuffer() const {
    return reinterpret_cast<ReSTASTNode *const *>(getItemInfoBuffer() +
                                                  NumItems);
  }
  ReSTASTNode **getItemChildrenBuffer() {
    return reinterpret_cast<ReSTASTNode **>(getItemInfoBuffer() + NumItems);
  }

  EnumeratedList(ArrayRef<ListItemInfo> ItemInfos,
                 ArrayRef<ReSTASTNode *> ItemChildren);

public:
  static EnumeratedList *create(ReSTContext &C,
                                ArrayRef<ListItemInfo> ItemInfos,
                                ArrayRef<ReSTASTNode *> ItemChildren);

  unsigned getNumItems() const { return NumItems; }

  const ListItemInfo &getItemInfo(unsigned i) const {
    assert(i < NumItems);
    return *(getItemInfoBuffer() + i);
  }

  ArrayRef<const ReSTASTNode *> getItemChildren(unsigned i) const {
    assert(i < NumItems);
    const auto &ItemInfo = getItemInfo(i);
    return ArrayRef<const ReSTASTNode *>(getItemChildrenBuffer() +
                                             ItemInfo.FirstChildIndex,
                                         ItemInfo.NumChildren);
  }
  ArrayRef<ReSTASTNode *> getItemChildren(unsigned i) {
    assert(i < NumItems);
    const auto &ItemInfo = getItemInfo(i);
    return ArrayRef<ReSTASTNode *>(getItemChildrenBuffer() +
                                       ItemInfo.FirstChildIndex,
                                   ItemInfo.NumChildren);
  }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::EnumeratedList;
  }
};

// Body subelement, compound.
class DefinitionListItem final : public ReSTASTNode {
  TextAndInline *Term;

  unsigned NumClassifiers : 16;
  unsigned NumDefinitionChildren : 16;

  TextAndInline *const *getClassifiersBuffer() const {
    return reinterpret_cast<TextAndInline *const *>(this + 1);
  }
  TextAndInline **getClassifiersBuffer() {
    return reinterpret_cast<TextAndInline **>(this + 1);
  }

  ReSTASTNode *const *getDefinitionChildrenBuffer() const {
    return reinterpret_cast<ReSTASTNode *const *>(getClassifiersBuffer() +
                                                  NumClassifiers);
  }
  ReSTASTNode **getDefinitionChildrenBuffer() {
    return reinterpret_cast<ReSTASTNode **>(getClassifiersBuffer() +
                                            NumClassifiers);
  }

  DefinitionListItem(TextAndInline *Term, ArrayRef<TextAndInline *> Classifiers,
                     ArrayRef<ReSTASTNode *> DefinitionChildren);

public:
  static DefinitionListItem *create(ReSTContext &C, TextAndInline *Term,
                                    ArrayRef<TextAndInline *> Classifiers,
                                    ArrayRef<ReSTASTNode *> DefinitionChildren);

  const TextAndInline *getTerm() const { return Term; }
  TextAndInline *getTerm() { return Term; }

  ArrayRef<const TextAndInline *> getClassifiers() const {
    return ArrayRef<const TextAndInline *>(getClassifiersBuffer(),
                                           NumClassifiers);
  }
  ArrayRef<TextAndInline *> getClassifiers() {
    return ArrayRef<TextAndInline *>(getClassifiersBuffer(), NumClassifiers);
  }

  ArrayRef<const ReSTASTNode *> getDefinitionChildren() const {
    return ArrayRef<const ReSTASTNode *>(getDefinitionChildrenBuffer(),
                                         NumDefinitionChildren);
  }
  ArrayRef<ReSTASTNode *> getDefinitionChildren() {
    return ArrayRef<ReSTASTNode *>(getDefinitionChildrenBuffer(),
                                   NumDefinitionChildren);
  }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::DefinitionListItem;
  }
};

// Compound body element.
class DefinitionList final : public ReSTASTNode {
  unsigned NumChildren;

  const DefinitionListItem *const *getChildrenBuffer() const {
    return reinterpret_cast<const DefinitionListItem *const *>(this + 1);
  }
  DefinitionListItem **getChildrenBuffer() {
    return reinterpret_cast<DefinitionListItem **>(this + 1);
  }

  DefinitionList(ArrayRef<DefinitionListItem *> Children);

public:
  static DefinitionList *create(ReSTContext &C,
                                ArrayRef<DefinitionListItem *> Children);

  ArrayRef<const DefinitionListItem *> getChildren() const {
    return ArrayRef<const DefinitionListItem *>(getChildrenBuffer(),
                                                NumChildren);
  }
  ArrayRef<DefinitionListItem *> getChildren() {
    return ArrayRef<DefinitionListItem *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::DefinitionList;
  }
};

// Body subelement, compound.
class Field final : public ReSTASTNode {
  TextAndInline *Name;

  unsigned NumBodyChildren;

  ReSTASTNode *const *getBodyChildrenBuffer() const {
    return reinterpret_cast<ReSTASTNode *const *>(this + 1);
  }
  ReSTASTNode **getBodyChildrenBuffer() {
    return reinterpret_cast<ReSTASTNode **>(this + 1);
  }

  Field(TextAndInline *Name, ArrayRef<ReSTASTNode *> BodyChildren);

public:
  static Field *create(ReSTContext &C, TextAndInline *Name,
                       ArrayRef<ReSTASTNode *> BodyChildren);

  const TextAndInline *getName() const { return Name; }
  TextAndInline *getName() { return Name; }

  ArrayRef<const ReSTASTNode *> getBodyChildren() const {
    return ArrayRef<const ReSTASTNode *>(getBodyChildrenBuffer(),
                                         NumBodyChildren);
  }
  ArrayRef<ReSTASTNode *> getBodyChildren() {
    return ArrayRef<ReSTASTNode *>(getBodyChildrenBuffer(), NumBodyChildren);
  }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::Field;
  }
};

// Compound body element.
class FieldList final : public ReSTASTNode {
  unsigned NumChildren;

  Field *const *getChildrenBuffer() const {
    return reinterpret_cast<Field *const *>(this + 1);
  }
  Field **getChildrenBuffer() { return reinterpret_cast<Field **>(this + 1); }

  FieldList(ArrayRef<Field *> Children);

public:
  static FieldList *create(ReSTContext &C, ArrayRef<Field *> Children);

  ArrayRef<Field *> getChildren() const {
    return ArrayRef<Field *>(getChildrenBuffer(), NumChildren);
  }
  MutableArrayRef<Field *> getChildren() {
    return MutableArrayRef<Field *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::FieldList;
  }
};

// Compound body element.
class BlockQuote final : public ReSTASTNode {
  unsigned NumChildren;

  ReSTASTNode **getChildrenBuffer() {
    return reinterpret_cast<ReSTASTNode **>(this + 1);
  }
  const ReSTASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const ReSTASTNode *const *>(this + 1);
  }

  BlockQuote(ArrayRef<ReSTASTNode *> Children);

public:
  static BlockQuote *create(ReSTContext &C, ArrayRef<ReSTASTNode *> Children);

  ArrayRef<ReSTASTNode *> getChildren() {
    return ArrayRef<ReSTASTNode *>(getChildrenBuffer(), NumChildren);
  }
  ArrayRef<const ReSTASTNode *> getChildren() const {
    return ArrayRef<const ReSTASTNode *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::BlockQuote;
  }
};

class TextAndInline final : public ReSTASTNode {
  // FIXME: use a more specific AST node here?
  ArrayRef<ReSTASTNode *> Children;

  // FIXME: temporary while we don't parse inline markup.
  union {
    LineListRef LL;
    LinePart LP;
  };
  bool IsLinePart;

public:
  TextAndInline(LineListRef LL)
      : ReSTASTNode(ASTNodeKind::TextAndInline), LL(LL), IsLinePart(false) {}

  TextAndInline(LinePart LP)
      : ReSTASTNode(ASTNodeKind::TextAndInline), LP(LP), IsLinePart(true) {}

  bool isLinePart() const { return IsLinePart; }

  LineListRef getLines() const {
    assert(!IsLinePart);
    return LL;
  }

  LinePart getLinePart() const {
    assert(IsLinePart);
    return LP;
  }

  static bool classof(const ReSTASTNode *N) {
    return N->getKind() == ASTNodeKind::TextAndInline;
  }
};
} // namespace rest
} // namespace llvm

#endif // LLVM_REST_AST_H

