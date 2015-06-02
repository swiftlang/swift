//===--- AST.h - Markup AST nodes ---------------------------------------===//
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
#ifndef LLVM_MARKUP_AST_H
#define LLVM_MARKUP_AST_H

#include "llvm/Support/ErrorHandling.h"
#include "swift/Markup/LineList.h"

namespace llvm {
namespace markup {

class MarkupContext;

#define MARKUP_AST_NODE(Id, Parent) class Id;
#define ABSTRACT_MARKUP_AST_NODE(Id, Parent) class Id;
#define MARKUP_AST_NODE_RANGE(Id, FirstId, LastId)
#include "swift/Markup/ASTNodes.def"

enum class ASTNodeKind : uint8_t {
#define MARKUP_AST_NODE(Id, Parent) Id,
#define ABSTRACT_MARKUP_AST_NODE(Id, Parent)
#define MARKUP_AST_NODE_RANGE(Id, FirstId, LastId) \
  First_##Id = FirstId, Last_##Id = LastId,
#include "swift/Markup/ASTNodes.def"
};

class alignas(void *) MarkupASTNode {
  MarkupASTNode(const MarkupASTNode &) = delete;
  void operator=(const MarkupASTNode &) = delete;

protected:
  ASTNodeKind Kind;

public:

  MarkupASTNode(ASTNodeKind Kind) : Kind(Kind) {}

  ASTNodeKind getKind() const { return Kind; }

  void *operator new(size_t Bytes, MarkupContext &MC,
                     unsigned alignment = alignof(MarkupASTNode));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }

  ArrayRef<MarkupASTNode *> getChildren();
  ArrayRef<const MarkupASTNode *> getChildren() const;
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;
};

class MarkupASTNode;

#pragma mark Markdown Nodes

class Document final : public MarkupASTNode {
  size_t NumChildren;

  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }

  Document(ArrayRef<MarkupASTNode*> Children);

public:
  static Document *create(MarkupContext &MC,
                          ArrayRef<MarkupASTNode *> Children);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }



  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Document;
  }
};

class BlockQuote final : public MarkupASTNode {
  size_t NumChildren;

  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }

  BlockQuote(ArrayRef<MarkupASTNode *> Children);
public:
  static BlockQuote *create(MarkupContext &MC, ArrayRef<MarkupASTNode *> Children);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::BlockQuote;
  }
};

class List final : public MarkupASTNode {
  size_t NumChildren;
  bool Ordered;

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }

  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  List(ArrayRef<MarkupASTNode *> Children, bool IsOrdered);

public:
  static List *create(MarkupContext &MC, ArrayRef<MarkupASTNode *> Items,
                      bool IsOrdered);

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  void setChildren(const SmallVectorImpl<MarkupASTNode *> &NewChildren) {
    assert(NewChildren.size() <= NumChildren);
    auto Buffer = getChildrenBuffer();
    for (size_t i = 0; i < NewChildren.size(); ++i)
      Buffer[i] = NewChildren[i];

    NumChildren = NewChildren.size();
  }

  bool isOrdered() const {
    return Ordered;
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::List;
  }
};

class Item final : public MarkupASTNode {
  size_t NumChildren;

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }
  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  Item(ArrayRef<MarkupASTNode *> Children);

public:
  static Item *create(MarkupContext &MC, ArrayRef<MarkupASTNode *> Children);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Item;
  }
};

class CodeBlock final : public MarkupASTNode {
  StringRef LiteralContent;

  CodeBlock(StringRef LiteralContent)
      : MarkupASTNode(ASTNodeKind::CodeBlock),
        LiteralContent(LiteralContent) {}

public:
  static CodeBlock *create(MarkupContext &MC, StringRef LiteralContent);

  StringRef getLiteralContent() const { return LiteralContent; };

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return {};
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return {};
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::CodeBlock;
  }
};

class HTML final : public MarkupASTNode {
  StringRef LiteralContent;
  HTML(StringRef LiteralContent)
      : MarkupASTNode(ASTNodeKind::HTML),
        LiteralContent(LiteralContent) {}
public:
  static HTML *create(MarkupContext &MC, StringRef LiteralContent);
  StringRef getLiteralContent() const { return LiteralContent; };

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::HTML;
  }
};

class Paragraph final : public MarkupASTNode {
  size_t NumChildren;

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }
  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  Paragraph(ArrayRef<MarkupASTNode *> Children);

public:
  static Paragraph *create(MarkupContext &MC,
                           ArrayRef<MarkupASTNode *> Children);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Paragraph;
  }
};

class Header final : public MarkupASTNode {
  size_t NumChildren;
  unsigned Level;

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }
  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  Header(unsigned Level, ArrayRef<MarkupASTNode *> Children);

public:
  static Header *create(MarkupContext &MC, unsigned Level,
                        ArrayRef<MarkupASTNode *> Children);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  unsigned getLevel() const {
    return Level;
  }
  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Header;
  }
};

class HRule final : public MarkupASTNode {
  HRule() : MarkupASTNode(ASTNodeKind::HRule) {}
public:
  static HRule *create(MarkupContext &MC);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return {};
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return {};
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::HRule;
  }
};

class InlineContent : public MarkupASTNode {
public:
  InlineContent(ASTNodeKind Kind) : MarkupASTNode(Kind) {}

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return {};
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return {};
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() >= ASTNodeKind::First_Inline &&
           N->getKind() <= ASTNodeKind::Last_Inline;
  }
};

class Text final : public InlineContent {
  StringRef LiteralContent;
  Text(StringRef LiteralContent)
    : InlineContent(ASTNodeKind::Text),
      LiteralContent(LiteralContent) {}
public:
  static Text *create(MarkupContext &MC, StringRef LiteralContent);
  StringRef getLiteralContent() const { return LiteralContent; };
  void setLiteralContent(StringRef LC) {
    LiteralContent = LC;
  }

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return {};
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return {};
  }

  StringRef str() const {
    return LiteralContent;
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Text;
  }
};

class SoftBreak final : public InlineContent {
  SoftBreak() : InlineContent(ASTNodeKind::SoftBreak) {}
public:
  static SoftBreak *create(MarkupContext &MC);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return {};
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return {};
  }

  StringRef str() const {
    return "\n";
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::SoftBreak;
  }
};

class LineBreak final : public InlineContent {
  LineBreak() : InlineContent(ASTNodeKind::LineBreak) {}
public:
  static LineBreak *create(MarkupContext &MC);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return {};
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return {};
  }

  StringRef str() const {
    return "\n";
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::LineBreak;
  }
};

class Code final : public InlineContent {
  StringRef LiteralContent;

  Code(StringRef LiteralContent)
      : InlineContent(ASTNodeKind::Code),
        LiteralContent(LiteralContent) {}
public:
  static Code *create(MarkupContext &MC, StringRef LiteralContent);

  StringRef getLiteralContent() const { return LiteralContent; };

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return {};
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return {};
  }

  StringRef str() const {
    return LiteralContent;
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Code;
  }
};

class InlineHTML final : public InlineContent {
  StringRef LiteralContent;
  InlineHTML(StringRef LiteralContent)
    : InlineContent(ASTNodeKind::InlineHTML),
      LiteralContent(LiteralContent) {}
public:
  static InlineHTML *create(MarkupContext &MC, StringRef LiteralContent);

  StringRef getLiteralContent() const { return LiteralContent; };

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return {};
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return {};
  }

  StringRef str() const {
    return LiteralContent;
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::InlineHTML;
  }
};

class Emphasis final : public InlineContent {
  size_t NumChildren;

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }
  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  Emphasis(ArrayRef<MarkupASTNode *> Children);
public:
  static Emphasis *create(MarkupContext &MC,
                          ArrayRef<MarkupASTNode *> Children);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Emphasis;
  }
};

class Strong final : public InlineContent {
  size_t NumChildren;

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }
  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  Strong(ArrayRef<MarkupASTNode *> Children);
public:
  static Strong *create(MarkupContext &MC,
                        ArrayRef<MarkupASTNode *> Children);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Strong;
  }
};

class Link final : public InlineContent {
  size_t NumChildren;
  // FIXME: Use StringRef/Line here
  std::string Destination;

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }
  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  Link(std::string Destination, ArrayRef<MarkupASTNode *> Children);

public:
  static Link *create(MarkupContext &MC,
                      std::string Destination,
                      ArrayRef<MarkupASTNode *> Children);

  StringRef getDestination() const {
    return StringRef(Destination.c_str(), Destination.size());
  }

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Link;
  }
};

class Image final : public InlineContent {
  size_t NumChildren;
  // FIXME: Hyperlink destinations can't be wrapped - use a Line
  std::string Destination;

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }
  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  Image(std::string Destination, ArrayRef<MarkupASTNode *> Children);

public:
  static Image *create(MarkupContext &MC,
                      std::string Destination,
                      ArrayRef<MarkupASTNode *> Children);

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  StringRef getDestination() const {
    return StringRef(Destination.c_str(), Destination.size());
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::Image;
  }
};

#pragma mark Private Extensions

class PrivateExtension : public MarkupASTNode {
protected:
  PrivateExtension(ASTNodeKind Kind)
      : MarkupASTNode(Kind) {}
public:

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return {};
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return {};
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() >= ASTNodeKind::First_Private &&
      N->getKind() <= ASTNodeKind::Last_Private;
  }
};

class ParamField final : public PrivateExtension {
  StringRef Name;
  size_t NumChildren;

  MarkupASTNode **getChildrenBuffer() {
    return reinterpret_cast<MarkupASTNode**>(this + 1);
  }
  const MarkupASTNode *const *getChildrenBuffer() const {
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1);
  }

  ParamField(StringRef Name, ArrayRef<MarkupASTNode *> Children);

public:

  static ParamField *create(MarkupContext &MC, StringRef Name,
                            ArrayRef<MarkupASTNode *> Children);

  StringRef getName() const {
    return Name;
  }

  ArrayRef<const MarkupASTNode *> getChildren() const {
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  ArrayRef<MarkupASTNode *> getChildren() {
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren);
  }

  static bool classof(const MarkupASTNode *N) {
    return N->getKind() == ASTNodeKind::ParamField;
  }
};

#define MARKUP_SIMPLE_FIELD(Id, Keyword, XMLKind) \
class Id final : public PrivateExtension { \
  size_t NumChildren; \
\
  MarkupASTNode **getChildrenBuffer() { \
    return reinterpret_cast<MarkupASTNode**>(this + 1); \
  } \
  const MarkupASTNode *const *getChildrenBuffer() const { \
    return reinterpret_cast<const MarkupASTNode *const *>(this + 1); \
  } \
\
  Id(ArrayRef<MarkupASTNode *> Children);\
\
public: \
  static Id *create(MarkupContext &MC, ArrayRef<MarkupASTNode *> Children); \
\
  ArrayRef<const MarkupASTNode *> getChildren() const { \
    return ArrayRef<const MarkupASTNode *>(getChildrenBuffer(), NumChildren); \
  } \
\
  ArrayRef<MarkupASTNode *> getChildren() { \
    return ArrayRef<MarkupASTNode *>(getChildrenBuffer(), NumChildren); \
  } \
\
  static bool classof(const MarkupASTNode *N) { \
    return N->getKind() == ASTNodeKind::Id; \
  } \
};
#include "swift/Markup/SimpleFields.def"

class MarkupASTWalker {
public:
  void walk(const MarkupASTNode *Node) {
    enter(Node);

    if (shouldVisitChildrenOf(Node))
      for (auto Child : Node->getChildren())
        walk(Child);

    exit(Node);
  }

  virtual bool shouldVisitChildrenOf(const MarkupASTNode *Node) {
    return true;
  }

  virtual void enter(const MarkupASTNode *Node) {}
  virtual void exit(const MarkupASTNode *Node) {}

#define MARKUP_AST_NODE(Id, Parent) \
  virtual void visit##Id(const Id *Node) {}
#define ABSTRACT_MARKUP_AST_NODE(Id, Parent)
#define MARKUP_AST_NODE_RANGE(Id, FirstId, LastId)
#include "swift/Markup/ASTNodes.def"

  virtual ~MarkupASTWalker();
};

MarkupASTNode *createSimpleField(MarkupContext &MC, StringRef Tag,
                                 ArrayRef<MarkupASTNode *> Children);

bool isAFieldTag(StringRef Tag);

void dump(const MarkupASTNode *Node, llvm::raw_ostream &OS, unsigned indent = 0);
void printInlinesUnder(const MarkupASTNode *Node, llvm::raw_ostream &OS,
                       bool PrintDecorators = false);
} // namespace markup
} // namespace llvm

#endif // LLVM_MARKUP_AST_H
