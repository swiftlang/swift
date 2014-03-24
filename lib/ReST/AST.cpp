#include "swift/ReST/AST.h"
#include "swift/ReST/Parser.h"
#include <memory>

using namespace llvm;
using namespace rest;
using namespace llvm::rest::detail;

void *ReSTASTNode::operator new(size_t Bytes, ReSTContext &C,
                                unsigned Alignment) {
  return C.Allocator.Allocate(Bytes, Alignment);
}

Document::Document(ArrayRef<ReSTASTNode *> Children)
    : ReSTASTNode(ASTNodeKind::Document), NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(),
                          getChildrenBuffer());
}

Document *Document::create(ReSTContext &C, ArrayRef<ReSTASTNode *> Children) {
  void *Mem =
      C.allocate(sizeof(Document) + Children.size() * sizeof(ReSTASTNode *),
                 alignof(Document));
  return new (Mem) Document(Children);
}

BulletList::BulletList(ArrayRef<ListItemInfo> ItemInfos,
                       ArrayRef<ReSTASTNode *> ItemChildren)
    : ReSTASTNode(ASTNodeKind::BulletList), NumItems(ItemInfos.size()) {
  std::uninitialized_copy(ItemInfos.begin(), ItemInfos.end(),
                          getItemInfoBuffer());
  std::uninitialized_copy(ItemChildren.begin(), ItemChildren.end(),
                          getItemChildrenBuffer());
#ifndef NDEBUG
  unsigned ExpectedNumChildren = 0;
  for (const auto &Info : ItemInfos)
    ExpectedNumChildren += Info.NumChildren;
  assert(ItemChildren.size() == ExpectedNumChildren);
#endif
}

BulletList *BulletList::create(ReSTContext &C,
                               ArrayRef<ListItemInfo> ItemInfos,
                               ArrayRef<ReSTASTNode *> ItemChildren) {
  void *Mem =
      C.allocate(sizeof(BulletList) + ItemInfos.size() * sizeof(ListItemInfo) +
                     ItemChildren.size() * sizeof(ReSTASTNode *),
                 alignof(BulletList));
  return new (Mem) BulletList(ItemInfos, ItemChildren);
}

EnumeratedList::EnumeratedList(ArrayRef<ListItemInfo> ItemInfos,
                               ArrayRef<ReSTASTNode *> ItemChildren)
    : ReSTASTNode(ASTNodeKind::EnumeratedList), NumItems(ItemInfos.size()) {
  std::uninitialized_copy(ItemInfos.begin(), ItemInfos.end(),
                          getItemInfoBuffer());
  std::uninitialized_copy(ItemChildren.begin(), ItemChildren.end(),
                          getItemChildrenBuffer());
#ifndef NDEBUG
  unsigned ExpectedNumChildren = 0;
  for (const auto &Info : ItemInfos)
    ExpectedNumChildren += Info.NumChildren;
  assert(ItemChildren.size() == ExpectedNumChildren);
#endif
}

EnumeratedList *EnumeratedList::create(ReSTContext &C,
                                       ArrayRef<ListItemInfo> ItemInfos,
                                       ArrayRef<ReSTASTNode *> ItemChildren) {
  void *Mem = C.allocate(sizeof(EnumeratedList) +
                             ItemInfos.size() * sizeof(ListItemInfo) +
                             ItemChildren.size() * sizeof(ReSTASTNode *),
                         alignof(EnumeratedList));
  return new (Mem) EnumeratedList(ItemInfos, ItemChildren);
}

DefinitionListItem::DefinitionListItem(
    TextAndInline *Term, ArrayRef<TextAndInline *> Classifiers,
    ArrayRef<ReSTASTNode *> DefinitionChildren)
    : ReSTASTNode(ASTNodeKind::DefinitionListItem), Term(Term),
      NumClassifiers(Classifiers.size()),
      NumDefinitionChildren(DefinitionChildren.size()) {
  std::uninitialized_copy(Classifiers.begin(), Classifiers.end(),
                          getClassifiersBuffer());
  std::uninitialized_copy(DefinitionChildren.begin(), DefinitionChildren.end(),
                          getDefinitionChildrenBuffer());
}

DefinitionListItem *
DefinitionListItem::create(ReSTContext &C, TextAndInline *Term,
                           ArrayRef<TextAndInline *> Classifiers,
                           ArrayRef<ReSTASTNode *> DefinitionChildren) {
  void *Mem = C.allocate(sizeof(DefinitionListItem) +
                             Classifiers.size() * sizeof(TextAndInline *) +
                             DefinitionChildren.size() * sizeof(ReSTASTNode *),
                         alignof(DefinitionListItem));
  return new (Mem) DefinitionListItem(Term, Classifiers, DefinitionChildren);
}

DefinitionList::DefinitionList(ArrayRef<DefinitionListItem *> Children)
    : ReSTASTNode(ASTNodeKind::DefinitionList), NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(),
                          getChildrenBuffer());
}

DefinitionList *
DefinitionList::create(ReSTContext &C,
                       ArrayRef<DefinitionListItem *> Children) {
  void *Mem = C.allocate(sizeof(DefinitionList) +
                             Children.size() * sizeof(DefinitionListItem *),
                         alignof(DefinitionList));
  return new (Mem) DefinitionList(Children);
}

Field::Field(TextAndInline *Name, ArrayRef<ReSTASTNode *> BodyChildren)
    : ReSTASTNode(ASTNodeKind::Field), Name(Name),
      NumBodyChildren(BodyChildren.size()) {
  std::uninitialized_copy(BodyChildren.begin(), BodyChildren.end(),
                          getBodyChildrenBuffer());
}

Field *Field::create(ReSTContext &C, TextAndInline *Name,
                     ArrayRef<ReSTASTNode *> BodyChildren) {
  void *Mem =
      C.allocate(sizeof(Field) + BodyChildren.size() * sizeof(ReSTASTNode *),
                 alignof(Field));
  return new (Mem) Field(Name, BodyChildren);
}

FieldList::FieldList(ArrayRef<Field *> Children)
    : ReSTASTNode(ASTNodeKind::FieldList), NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(),
                          getChildrenBuffer());
}

FieldList *FieldList::create(ReSTContext &C, ArrayRef<Field *> Children) {
  void *Mem = C.allocate(sizeof(FieldList) + Children.size() * sizeof(Field *),
                         alignof(FieldList));
  return new (Mem) FieldList(Children);
}

BlockQuote::BlockQuote(ArrayRef<ReSTASTNode *> Children)
    : ReSTASTNode(ASTNodeKind::BlockQuote), NumChildren(Children.size()) {
  std::uninitialized_copy(Children.begin(), Children.end(),
                          getChildrenBuffer());
}

BlockQuote *BlockQuote::create(ReSTContext &C,
                               ArrayRef<ReSTASTNode *> Children) {
  void *Mem =
      C.allocate(sizeof(BlockQuote) + Children.size() * sizeof(ReSTASTNode *),
                 alignof(BlockQuote));
  return new (Mem) BlockQuote(Children);
}

