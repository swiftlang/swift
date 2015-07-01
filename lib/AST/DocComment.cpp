//===--- DocComment.cpp - Extraction of doc comments ----------------------===//
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
///
/// \file
/// This file implements extraction of documentation comments from a Swift
/// Markup AST tree.
///
//===----------------------------------------------------------------------===//

#include "swift/AST/Comment.h"
#include "swift/AST/Decl.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/RawComment.h"
#include "swift/Markup/Markup.h"

using namespace swift;

void *DocComment::operator new(size_t Bytes, llvm::markup::MarkupContext &MC,
                               unsigned Alignment) {
  return MC.allocate(Bytes, Alignment);
}

Optional<llvm::markup::ParamField *> extractParamOutlineItem(
    llvm::markup::MarkupContext &MC,
    llvm::markup::MarkupASTNode *Node) {

  auto Item = dyn_cast<llvm::markup::Item>(Node);
  if (!Item)
    return None;

  auto Children = Item->getChildren();
  if (Children.empty())
    return None;

  auto FirstChild = Children.front();
  auto FirstParagraph = dyn_cast<llvm::markup::Paragraph>(FirstChild);
  if (!FirstParagraph)
    return None;

  auto FirstParagraphChildren = FirstParagraph->getChildren();
  if (FirstParagraphChildren.empty())
    return None;

  auto ParagraphText =
      dyn_cast<llvm::markup::Text>(FirstParagraphChildren.front());
  if (!ParagraphText)
    return None;

  StringRef Name;
  StringRef Remainder;
  std::tie(Name, Remainder) = ParagraphText->getLiteralContent().split(':');
  Name = Name.rtrim();

  if (Name.empty())
    return None;

  ParagraphText->setLiteralContent(Remainder.ltrim());

  return llvm::markup::ParamField::create(MC, Name, Children);
}

bool extractParameterOutline(llvm::markup::MarkupContext &MC,
                             llvm::markup::List *L,
                             DocComment::CommentParts &Parts) {
  SmallVector<llvm::markup::MarkupASTNode *, 8> NormalItems;
  auto Children = L->getChildren();
  if (Children.empty())
    return false;

  for (auto Child : Children) {
    auto I = dyn_cast<llvm::markup::Item>(Child);
    if (!I) {
      NormalItems.push_back(Child);
      continue;
    }

    auto ItemChildren = I->getChildren();
    if (ItemChildren.empty()) {
      NormalItems.push_back(Child);
      continue;
    }

    auto FirstChild = ItemChildren.front();
    auto FirstParagraph = dyn_cast<llvm::markup::Paragraph>(FirstChild);
    if (!FirstParagraph) {
      NormalItems.push_back(Child);
      continue;
    }

    auto FirstParagraphChildren = FirstParagraph->getChildren();
    if (FirstParagraphChildren.empty()) {
      NormalItems.push_back(Child);
      continue;
    }

    auto HeadingText
        = dyn_cast<llvm::markup::Text>(FirstParagraphChildren.front());
    if (!HeadingText) {
      NormalItems.push_back(Child);
      continue;
    }

    auto HeadingContent = HeadingText->getLiteralContent();
    if (!HeadingContent.rtrim().equals_lower("parameters:")) {
      NormalItems.push_back(Child);
      continue;
    }

    auto Rest = ArrayRef<llvm::markup::MarkupASTNode *>(
        ItemChildren.begin() + 1, ItemChildren.end());
    if (Rest.empty()) {
      NormalItems.push_back(Child);
      continue;
    }

    for (auto Child : Rest) {
      auto SubList = dyn_cast<llvm::markup::List>(Child);
      if (!SubList)
        continue;

      for (auto SubListChild : SubList->getChildren()) {
        auto Param = extractParamOutlineItem(MC, SubListChild);
        if (Param.hasValue()) {
          Parts.ParamFields.push_back(Param.getValue());
        }
      }
    }
  }

  if (NormalItems.size() != Children.size()) {
    L->setChildren(NormalItems);
  }

  return NormalItems.size() == 0;
}

bool extractSeparatedParams(llvm::markup::MarkupContext &MC,
                            llvm::markup::List *L,
                            DocComment::CommentParts &Parts) {
  SmallVector<llvm::markup::MarkupASTNode *, 8> NormalItems;
  auto Children = L->getChildren();

  for (auto Child : Children) {
    auto I = dyn_cast<llvm::markup::Item>(Child);
    if (!I) {
      NormalItems.push_back(Child);
      continue;
    }

    auto ItemChildren = I->getChildren();
    if (ItemChildren.empty()) {
      NormalItems.push_back(Child);
      continue;
    }

    auto FirstChild = ItemChildren.front();
    auto FirstParagraph = dyn_cast<llvm::markup::Paragraph>(FirstChild);
    if (!FirstParagraph) {
      NormalItems.push_back(Child);
      continue;
    }

    auto FirstParagraphChildren = FirstParagraph->getChildren();
    if (FirstParagraphChildren.empty()) {
      NormalItems.push_back(Child);
      continue;
    }

    auto ParagraphText
        = dyn_cast<llvm::markup::Text>(FirstParagraphChildren.front());
    if (!ParagraphText) {
      NormalItems.push_back(Child);
      continue;
    }

    StringRef ParameterPrefix("parameter ");
    auto ParagraphContent = ParagraphText->getLiteralContent();
    auto PotentialMatch = ParagraphContent.substr(0, ParameterPrefix.size());

    if (!PotentialMatch.startswith_lower(ParameterPrefix)) {
      NormalItems.push_back(Child);
      continue;
    }

    ParagraphContent = ParagraphContent.substr(ParameterPrefix.size());
    ParagraphText->setLiteralContent(ParagraphContent.ltrim());

    auto ParamField = extractParamOutlineItem(MC, I);
    if (ParamField.hasValue())
      Parts.ParamFields.push_back(ParamField.getValue());
    else
      NormalItems.push_back(Child);
  }

  if (NormalItems.size() != Children.size())
    L->setChildren(NormalItems);

  return NormalItems.size() == 0;
}

bool extractSimpleField(llvm::markup::MarkupContext &MC,
                        llvm::markup::List *L,
                        DocComment::CommentParts &Parts) {
  auto Children = L->getChildren();
  SmallVector<llvm::markup::MarkupASTNode *, 8> NormalItems;
  for (auto Child : Children) {
    auto I = dyn_cast<llvm::markup::Item>(Child);
    if (!I) {
      NormalItems.push_back(Child);
      continue;
    }

    auto ItemChildren = I->getChildren();
    if (ItemChildren.empty()) {
      NormalItems.push_back(Child);
      continue;
    }

    auto FirstParagraph
        = dyn_cast<llvm::markup::Paragraph>(ItemChildren.front());
    if (!FirstParagraph) {
      NormalItems.push_back(Child);
      continue;
    }

    auto ParagraphChildren = FirstParagraph->getChildren();
    if (ParagraphChildren.empty()) {
      NormalItems.push_back(Child);
      continue;
    }

    auto ParagraphText
        = dyn_cast<llvm::markup::Text>(ParagraphChildren.front());
    if (!ParagraphText) {
      NormalItems.push_back(Child);
      continue;
    }

    StringRef Tag;
    StringRef Remainder;
    std::tie(Tag, Remainder) = ParagraphText->getLiteralContent().split(':');
    Tag = Tag.ltrim().rtrim();
    Remainder = Remainder.ltrim();

    if (!llvm::markup::isAFieldTag(Tag)) {
      NormalItems.push_back(Child);
      continue;
    }

    ParagraphText->setLiteralContent(Remainder);
    auto Field = llvm::markup::createSimpleField(MC, Tag, ItemChildren);

    if (auto RF = dyn_cast<llvm::markup::ReturnsField>(Field))
      Parts.ReturnsField = RF;
    else if (auto TF = dyn_cast<llvm::markup::ThrowsField>(Field))
      Parts.ThrowsField = TF;
    else
      Parts.BodyNodes.push_back(Field);
  }

  if (NormalItems.size() != Children.size())
    L->setChildren(NormalItems);

  return NormalItems.size() == 0;
}


DocComment::CommentParts extractCommentParts(llvm::markup::MarkupContext &MC,
                                             llvm::markup::Document *Doc){

  DocComment::CommentParts Parts;
  auto Children = Doc->getChildren();
  if (Children.empty())
    return Parts;

  auto FirstParagraph
      = dyn_cast<llvm::markup::Paragraph>(Doc->getChildren().front());
  if (FirstParagraph)
    Parts.Brief = FirstParagraph;

  // Look for special top-level lists
  size_t StartOffset = FirstParagraph == nullptr ? 0 : 1;
  for (auto C = Children.begin() + StartOffset; C != Children.end(); ++C) {
    if (auto L = dyn_cast<llvm::markup::List>(*C)) {
      // Could be one of the following:
      // 1. A parameter outline:
      //    - Parameters:
      //      - x: ...
      //      - y: ...
      // 2. An exploded parameter list:
      //    - parameter x: ...
      //    - parameter y: ...
      // 3. Some other simple field, including "returns" (see SimpleFields.def)
      auto ListNowEmpty = extractParameterOutline(MC, L, Parts);
      ListNowEmpty |= extractSeparatedParams(MC, L, Parts);
      ListNowEmpty |= extractSimpleField(MC, L, Parts);
      if (ListNowEmpty)
        continue; // This drops the empty list from the doc comment body.
    }
    Parts.BodyNodes.push_back(*C);
  }
  return Parts;
}

Optional<DocComment *> swift::getDocComment(llvm::markup::MarkupContext &MC,
                                            const Decl *D) {
  auto RC = D->getRawComment();
  if (RC.isEmpty())
    return None;

  PrettyStackTraceDecl StackTrace("parsing comment for", D);

  llvm::markup::LineList LL = MC.getLineList(RC);
  auto *Doc = llvm::markup::parseDocument(MC, LL);
  auto Parts = extractCommentParts(MC, Doc);
  return new (MC) DocComment(D, Doc, Parts);
}
