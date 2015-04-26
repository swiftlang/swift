#include "swift/Markup/Markup.h"
#include "swift/Markup/AST.h"
#include "llvm/ADT/Optional.h"

using namespace llvm;
using namespace markup;

Document::Document(ArrayRef<MarkupASTNode*> Children)
    : MarkupASTNode(ASTNodeKind::Document),
      NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

Document *Document::create(MarkupContext &MC,
                           ArrayRef<llvm::markup::MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(Document) + Children.size()
      * sizeof(MarkupASTNode *), alignof(Document));
  return new (Mem) Document(Children);
}

BlockQuote::BlockQuote(ArrayRef<MarkupASTNode*> Children)
    : MarkupASTNode(ASTNodeKind::BlockQuote),
      NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

BlockQuote *BlockQuote::create(MarkupContext &MC, ArrayRef<MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(BlockQuote) + Children.size()
      * sizeof(MarkupASTNode *), alignof(BlockQuote));
  return new (Mem) BlockQuote(Children);
}

HTML *HTML::create(MarkupContext &MC, StringRef LiteralContent) {
  void *Mem = MC.allocate(sizeof(HTML), alignof(HTML));
  return new (Mem) HTML(LiteralContent);
}

InlineHTML *InlineHTML::create(MarkupContext &MC, StringRef LiteralContent) {
  void *Mem = MC.allocate(sizeof(InlineHTML), alignof(InlineHTML));
  return new (Mem) InlineHTML(LiteralContent);
}

Code *Code::create(MarkupContext &MC, StringRef LiteralContent) {
  void *Mem = MC.allocate(sizeof(Code), alignof(Code));
  return new (Mem) Code(LiteralContent);
}

CodeBlock *CodeBlock::create(MarkupContext &MC, StringRef LiteralContent) {
  void *Mem = MC.allocate(sizeof(CodeBlock), alignof(CodeBlock));
  return new (Mem) CodeBlock(LiteralContent);
}

List::List(ArrayRef<MarkupASTNode *> Children, bool IsOrdered)
    : MarkupASTNode(ASTNodeKind::List),
      NumChildren(Children.size()),
      Ordered(IsOrdered) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

List *List::create(MarkupContext &MC, ArrayRef<MarkupASTNode *> Children,
                   bool IsOrdered) {
  void *Mem = MC.allocate(sizeof(List) + sizeof(MarkupASTNode)
      * Children.size(), alignof(List));
  return new (Mem) List(Children, IsOrdered);
}

Item::Item(ArrayRef<MarkupASTNode*> Children)
    : MarkupASTNode(ASTNodeKind::Item),
      NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

Item *Item::create(MarkupContext &MC, ArrayRef<MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(Item) + Children.size()
      * sizeof(MarkupASTNode *), alignof(Item));
  return new (Mem) Item(Children);
}

Link::Link(std::string Destination, ArrayRef<MarkupASTNode *> Children)
    : InlineContent(ASTNodeKind::Link),
      NumChildren(Children.size()),
      Destination(Destination) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

Link *Link::create(MarkupContext &MC, std::string Destination,
                   ArrayRef<MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(Link) + Children.size()
      * sizeof(MarkupASTNode *), alignof(Link));
  return new (Mem) Link(Destination, Children);
}

Image::Image(std::string Destination, ArrayRef<MarkupASTNode *> Children)
    : InlineContent(ASTNodeKind::Image),
      NumChildren(Children.size()),
      Destination(Destination) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

Image *Image::create(MarkupContext &MC, std::string Destination,
                     ArrayRef<MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(Image) + Children.size()
      * sizeof(MarkupASTNode *), alignof(Image));
  return new (Mem) Image(Destination, Children);
}

Header::Header(unsigned Level, ArrayRef<MarkupASTNode *> Children)
    : MarkupASTNode(ASTNodeKind::Header),
      NumChildren(Children.size()),
      Level(Level) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

Header *Header::create(MarkupContext &MC, unsigned Level,
                       ArrayRef<MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(Header) + Children.size()
      * sizeof(MarkupASTNode *), alignof(Header));
  return new (Mem) Header(Level, Children);
}

Paragraph::Paragraph(ArrayRef<MarkupASTNode *> Children)
    : MarkupASTNode(ASTNodeKind::Paragraph),
      NumChildren(Children.size()) {
      std::uninitialized_copy(Children.begin(), Children.end(),
                              getChildrenBuffer());
}

Paragraph *Paragraph::create(MarkupContext &MC,
                             ArrayRef<llvm::markup::MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(Paragraph) + Children.size()
      * sizeof(MarkupASTNode *), alignof(Paragraph));
  return new (Mem) Paragraph(Children);
}

HRule *HRule::create(MarkupContext &MC) {
  void *Mem = MC.allocate(sizeof(HRule), alignof(HRule));
  return new (Mem) HRule();
}

Text *Text::create(MarkupContext &MC, StringRef LiteralContent) {
  void *Mem = MC.allocate(sizeof(Text), alignof(Text));
  return new (Mem) Text(LiteralContent);
}

SoftBreak *SoftBreak::create(MarkupContext &MC) {
  void *Mem = MC.allocate(sizeof(SoftBreak), alignof(SoftBreak));
  return new (Mem) SoftBreak();
}

LineBreak *LineBreak::create(MarkupContext &MC) {
  void *Mem = MC.allocate(sizeof(LineBreak), alignof(LineBreak));
  return new (Mem) LineBreak();
}

Emphasis::Emphasis(ArrayRef<MarkupASTNode *> Children)
    : InlineContent(ASTNodeKind::Emphasis), NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

Emphasis *Emphasis::create(MarkupContext &MC,
                           ArrayRef<llvm::markup::MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(Emphasis) + Children.size()
      * sizeof(MarkupASTNode *), alignof(Emphasis));
  return new (Mem) Emphasis(Children);
}

Strong::Strong(ArrayRef<MarkupASTNode *> Children)
    : InlineContent(ASTNodeKind::Strong), NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

Strong *Strong::create(MarkupContext &MC,
                       ArrayRef<llvm::markup::MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(Strong) + Children.size()
      * sizeof(MarkupASTNode *), alignof(Strong));
  return new (Mem) Strong(Children);
}

ParamField::ParamField(StringRef Name, ArrayRef<MarkupASTNode *> Children)
    : PrivateExtension(ASTNodeKind::ParamField),
      Name(Name), NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(), getChildrenBuffer());
}

ParamField *ParamField::create(MarkupContext &MC, StringRef Name,
                               ArrayRef<MarkupASTNode *> Children) {
  void *Mem = MC.allocate(sizeof(ParamField) + Children.size()
      * sizeof(MarkupASTNode *), alignof(ParamField));
  return new (Mem) ParamField(Name, Children);
}

#define MARKUP_SIMPLE_FIELD(Id, Keyword, XMLKind) \
Id *Id::create(MarkupContext &MC, ArrayRef<MarkupASTNode *> Children) { \
  void *Mem = MC.allocate(sizeof(Id) + Children.size() \
      * sizeof(MarkupASTNode *), alignof(Id)); \
  return new (Mem) Id(Children); \
} \
\
Id::Id(ArrayRef<MarkupASTNode *> Children) \
    : PrivateExtension(ASTNodeKind::Id), NumChildren(Children.size()) { \
      std::uninitialized_copy(Children.begin(), Children.end(), \
                          getChildrenBuffer()); \
}
#include "swift/Markup/SimpleFields.def"

ArrayRef<MarkupASTNode *> MarkupASTNode::getChildren() {
  switch (Kind) {
#define MARKUP_AST_NODE(Id, Parent) \
  case ASTNodeKind::Id: \
      return cast<Id>(this)->getChildren();
#define ABSTRACT_MARKUP_AST_NODE(Id, Parent) class Id;
#define MARKUP_AST_NODE_RANGE(Id, FirstId, LastId)
#include "swift/Markup/ASTNodes.def"
  }
}

ArrayRef<const MarkupASTNode *> MarkupASTNode::getChildren() const {
  switch (Kind) {
#define MARKUP_AST_NODE(Id, Parent) \
case ASTNodeKind::Id: \
return cast<Id>(this)->getChildren();
#define ABSTRACT_MARKUP_AST_NODE(Id, Parent) class Id;
#define MARKUP_AST_NODE_RANGE(Id, FirstId, LastId)
#include "swift/Markup/ASTNodes.def"
  }
}

void llvm::markup::printInlinesUnder(const MarkupASTNode *Node,
                                     llvm::raw_ostream &OS) {
  auto printChildren = [](const ArrayRef<const MarkupASTNode *> Children,
                          llvm::raw_ostream &OS) {
    for (auto Child = Children.begin(); Child != Children.end(); Child++)
      llvm::markup::printInlinesUnder(*Child, OS);
  };

  switch (Node->getKind()) {
  case llvm::markup::ASTNodeKind::HTML: {
    auto H = cast<HTML>(Node);
    OS << H->getLiteralContent();
    break;
  }
  case llvm::markup::ASTNodeKind::InlineHTML: {
    auto IH = cast<InlineHTML>(Node);
    OS << IH->getLiteralContent();
    break;
  }
  case llvm::markup::ASTNodeKind::HRule:
    OS << "\n";
    break;
  case llvm::markup::ASTNodeKind::Text: {
    auto T = cast<Text>(Node);
    OS << T->getLiteralContent();
    break;
  }
  case llvm::markup::ASTNodeKind::SoftBreak:
    OS << " ";
    break;
  case llvm::markup::ASTNodeKind::LineBreak:
    OS << "\n";
    break;
  case llvm::markup::ASTNodeKind::Code: {
    auto C = cast<Code>(Node);
    OS << "``" << C->getLiteralContent() << "``";
    break;
  }
  case llvm::markup::ASTNodeKind::CodeBlock: {
    auto CB = cast<CodeBlock>(Node);
    OS << "``" << CB->getLiteralContent() << "``";
    break;
  }
  case llvm::markup::ASTNodeKind::Emphasis: {
    auto E = cast<Emphasis>(Node);
    OS << "*";
    printChildren(E->getChildren(), OS);
    OS << "*";
    break;
  }
  case llvm::markup::ASTNodeKind::Strong: {
    auto S = cast<Strong>(Node);
    OS << "**";
    printChildren(S->getChildren(), OS);
    OS << "**";
    break;
  }
  default:
    printChildren(Node->getChildren(), OS);
  }
  OS.flush();
}

llvm::markup::MarkupASTNode *llvm::markup::createSimpleField(
    MarkupContext &MC,
    StringRef Tag,
    ArrayRef<llvm::markup::MarkupASTNode *> Children) {
  if (false) {

  }
#define MARKUP_SIMPLE_FIELD(Id, Keyword, XMLKind) \
  else if (Tag.compare_lower(#Keyword) == 0) { \
    return Id::create(MC, Children); \
  }
#include "swift/Markup/SimpleFields.def"
  llvm_unreachable("Given tag not for any simple markup field");
}

bool llvm::markup::isAFieldTag(StringRef Tag) {
  if (false) {

  }
#define MARKUP_SIMPLE_FIELD(Id, Keyword, XMLKind) \
  else if (Tag.compare_lower(#Keyword) == 0) { \
    return true; \
  }
#include "swift/Markup/SimpleFields.def"
  return false;
}

void llvm::markup::dump(const MarkupASTNode *Node, llvm::raw_ostream &OS,
                        unsigned indent) {
  auto dumpChildren = [](const ArrayRef<const MarkupASTNode *> Children,
                         llvm::raw_ostream &OS, unsigned indent) {
    OS << "\n";
    for (auto Child = Children.begin(); Child != Children.end(); Child++) {
      llvm::markup::dump(*Child, OS, indent + 1);
      if (Child != Children.end() - 1)
        OS << "\n";
    }
  };

  auto simpleEscapingPrint = [](StringRef LiteralContent,
                                llvm::raw_ostream &OS) {
    OS << "\"";
    for (auto C : LiteralContent) {
      switch (C) {
      case '\n':
        OS << "\\n";
        break;
      case '\r':
        OS << "\\r";
        break;
      case '\t':
        OS << "\\t";
        break;
      case '"':
        OS << "\\\"";
        break;
      default:
        OS << C;
      }
    }
    OS << "\"";
  };

  for (unsigned i = 0; i < indent; ++i) {
    OS << " ";
  }

  OS << "(";
  switch (Node->getKind()) {
  case llvm::markup::ASTNodeKind::Document: {
    OS << "Document: Children=" << Node->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::BlockQuote: {
    OS << "BlockQuote: Children=" << Node->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::List: {
    auto L = cast<List>(Node);
    OS << "List: " << (L->isOrdered() ? "Ordered " : "Unordered ");
    OS << "Items=" << Node->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::Item: {
    OS << "Item: Children=" << Node->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::HTML: {
    auto H = cast<HTML>(Node);
    OS << "HTML: Content=";
    simpleEscapingPrint(H->getLiteralContent(), OS);
    break;
  }
  case llvm::markup::ASTNodeKind::InlineHTML: {
    auto IH = cast<InlineHTML>(Node);
    OS << "InlineHTML: Content=";
    simpleEscapingPrint(IH->getLiteralContent(), OS);
    break;
  }
  case llvm::markup::ASTNodeKind::Paragraph: {
    OS << "Paragraph: Children=" << Node->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::Header: {
    auto H = cast<Header>(Node);
    OS << "Header: Level=" << H->getLevel();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::HRule: {
    OS << "HRule";
    break;
  }
  case llvm::markup::ASTNodeKind::Text: {
    auto T = cast<Text>(Node);
    OS << "Text: Content=";
    simpleEscapingPrint(T->getLiteralContent(), OS);
    break;
  }
  case llvm::markup::ASTNodeKind::SoftBreak: {
    OS << "SoftBreak";
    break;
  }
  case llvm::markup::ASTNodeKind::LineBreak: {
    OS << "LineBreak";
    break;
  }
  case llvm::markup::ASTNodeKind::CodeBlock: {
    auto CB = cast<CodeBlock>(Node);
    OS << "CodeBlock: Content=";
    simpleEscapingPrint(CB->getLiteralContent(), OS);
    break;
  }
  case llvm::markup::ASTNodeKind::Code: {
    auto C = cast<Code>(Node);
    OS << "Code: Content=\"";
    simpleEscapingPrint(C->getLiteralContent(), OS);
    OS << "\"";
    break;
  }
  case llvm::markup::ASTNodeKind::Strong: {
    OS << "Strong: Children=" << Node->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::Emphasis: {
    OS << "Emphasis: Children=" << Node->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::Link: {
    auto L = cast<Link>(Node);
    OS << "Link: Destination=";
    simpleEscapingPrint(L->getDestination(), OS);
    OS << " " << "Children=" << L->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::Image: {
    auto I = cast<Image>(Node);
    OS << "Image: Destination=";
    simpleEscapingPrint(I->getDestination(), OS);
    OS << " " << "Children=" << I->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }
  case llvm::markup::ASTNodeKind::ParamField: {
    auto PF = cast<ParamField>(Node);
    OS << "ParamField: Name=";
    simpleEscapingPrint(PF->getName(), OS);
    OS << " Children=" << PF->getChildren().size();
    dumpChildren(Node->getChildren(), OS, indent + 1);
    break;
  }

#define MARKUP_SIMPLE_FIELD(Id, Keyword, XMLKind) \
  case llvm::markup::ASTNodeKind::Id: { \
    auto Field = cast<Id>(Node); \
    OS << #Id << ": Children=" << Field->getChildren().size(); \
    dumpChildren(Node->getChildren(), OS, indent + 1); \
    break; \
  }
#include "swift/Markup/SimpleFields.def"

  default:
    llvm_unreachable("Can't dump Markup AST Node: unknown node kind");
  }
  OS << ")";
}
