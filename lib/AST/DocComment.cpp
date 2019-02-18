//===--- DocComment.cpp - Extraction of doc comments ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/AST/Types.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/RawComment.h"
#include "swift/Markup/Markup.h"

using namespace swift;

void *DocComment::operator new(size_t Bytes, swift::markup::MarkupContext &MC,
                               unsigned Alignment) {
  return MC.allocate(Bytes, Alignment);
}

Optional<swift::markup::ParamField *> extractParamOutlineItem(
    swift::markup::MarkupContext &MC,
    swift::markup::MarkupASTNode *Node) {

  auto Item = dyn_cast<swift::markup::Item>(Node);
  if (!Item)
    return None;

  auto Children = Item->getChildren();
  if (Children.empty())
    return None;

  auto FirstChild = Children.front();
  auto FirstParagraph = dyn_cast<swift::markup::Paragraph>(FirstChild);
  if (!FirstParagraph)
    return None;

  auto FirstParagraphChildren = FirstParagraph->getChildren();
  if (FirstParagraphChildren.empty())
    return None;

  auto ParagraphText =
      dyn_cast<swift::markup::Text>(FirstParagraphChildren.front());
  if (!ParagraphText)
    return None;

  StringRef Name;
  StringRef Remainder;
  std::tie(Name, Remainder) = ParagraphText->getLiteralContent().split(':');
  Name = Name.rtrim();

  if (Name.empty())
    return None;

  ParagraphText->setLiteralContent(Remainder.ltrim());

  return swift::markup::ParamField::create(MC, Name, Children);
}

bool extractParameterOutline(
    swift::markup::MarkupContext &MC, swift::markup::List *L,
    SmallVectorImpl<swift::markup::ParamField *> &ParamFields) {
  SmallVector<swift::markup::MarkupASTNode *, 8> NormalItems;
  auto Children = L->getChildren();
  if (Children.empty())
    return false;

  for (auto Child : Children) {
    auto I = dyn_cast<swift::markup::Item>(Child);
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
    auto FirstParagraph = dyn_cast<swift::markup::Paragraph>(FirstChild);
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
        = dyn_cast<swift::markup::Text>(FirstParagraphChildren.front());
    if (!HeadingText) {
      NormalItems.push_back(Child);
      continue;
    }

    auto HeadingContent = HeadingText->getLiteralContent();
    if (!HeadingContent.rtrim().equals_lower("parameters:")) {
      NormalItems.push_back(Child);
      continue;
    }

    auto Rest = ArrayRef<swift::markup::MarkupASTNode *>(
        ItemChildren.begin() + 1, ItemChildren.end());
    if (Rest.empty()) {
      NormalItems.push_back(Child);
      continue;
    }

    for (auto Child : Rest) {
      auto SubList = dyn_cast<swift::markup::List>(Child);
      if (!SubList)
        continue;

      for (auto SubListChild : SubList->getChildren()) {
        auto Param = extractParamOutlineItem(MC, SubListChild);
        if (Param.hasValue()) {
          ParamFields.push_back(Param.getValue());
        }
      }
    }
  }

  if (NormalItems.size() != Children.size()) {
    L->setChildren(NormalItems);
  }

  return NormalItems.empty();
}

bool extractSeparatedParams(
    swift::markup::MarkupContext &MC, swift::markup::List *L,
    SmallVectorImpl<swift::markup::ParamField *> &ParamFields) {
  SmallVector<swift::markup::MarkupASTNode *, 8> NormalItems;
  auto Children = L->getChildren();

  for (auto Child : Children) {
    auto I = dyn_cast<swift::markup::Item>(Child);
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
    auto FirstParagraph = dyn_cast<swift::markup::Paragraph>(FirstChild);
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
        = dyn_cast<swift::markup::Text>(FirstParagraphChildren.front());
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
      ParamFields.push_back(ParamField.getValue());
    else
      NormalItems.push_back(Child);
  }

  if (NormalItems.size() != Children.size())
    L->setChildren(NormalItems);

  return NormalItems.empty();
}

bool extractSimpleField(
    swift::markup::MarkupContext &MC, swift::markup::List *L,
    swift::markup::CommentParts &Parts,
    SmallVectorImpl<const swift::markup::MarkupASTNode *> &BodyNodes) {
  auto Children = L->getChildren();
  SmallVector<swift::markup::MarkupASTNode *, 8> NormalItems;
  for (auto Child : Children) {
    auto I = dyn_cast<swift::markup::Item>(Child);
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
        = dyn_cast<swift::markup::Paragraph>(ItemChildren.front());
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
        = dyn_cast<swift::markup::Text>(ParagraphChildren.front());
    if (!ParagraphText) {
      NormalItems.push_back(Child);
      continue;
    }

    StringRef Tag;
    StringRef Remainder;
    std::tie(Tag, Remainder) = ParagraphText->getLiteralContent().split(':');
    Tag = Tag.ltrim().rtrim();
    Remainder = Remainder.ltrim();

    if (!swift::markup::isAFieldTag(Tag)) {
      NormalItems.push_back(Child);
      continue;
    }

    ParagraphText->setLiteralContent(Remainder);
    auto Field = swift::markup::createSimpleField(MC, Tag, ItemChildren);

    if (auto RF = dyn_cast<swift::markup::ReturnsField>(Field)) {
      Parts.ReturnsField = RF;
    } else if (auto TF = dyn_cast<swift::markup::ThrowsField>(Field)) {
      Parts.ThrowsField = TF;
    } else if (auto TF = dyn_cast<swift::markup::TagField>(Field)) {
      llvm::SmallString<64> Scratch;
      llvm::raw_svector_ostream OS(Scratch);
      printInlinesUnder(TF, OS);
      Parts.Tags.insert(MC.allocateCopy(OS.str()));
    } else if (auto LKF = dyn_cast<markup::LocalizationKeyField>(Field)) {
      Parts.LocalizationKeyField = LKF;
    } else {
      BodyNodes.push_back(Field);
    }
  }

  if (NormalItems.size() != Children.size())
    L->setChildren(NormalItems);

  return NormalItems.empty();
}

swift::markup::CommentParts
swift::extractCommentParts(swift::markup::MarkupContext &MC,
                    swift::markup::MarkupASTNode *Node) {

  swift::markup::CommentParts Parts;
  auto Children = Node->getChildren();
  if (Children.empty())
    return Parts;

  auto FirstParagraph
      = dyn_cast<swift::markup::Paragraph>(Node->getChildren().front());
  if (FirstParagraph)
    Parts.Brief = FirstParagraph;

  SmallVector<const swift::markup::MarkupASTNode *, 4> BodyNodes;
  SmallVector<swift::markup::ParamField *, 8> ParamFields;

  // Look for special top-level lists
  size_t StartOffset = FirstParagraph == nullptr ? 0 : 1;
  for (auto C = Children.begin() + StartOffset; C != Children.end(); ++C) {
    if (auto L = dyn_cast<swift::markup::List>(*C)) {
      // Could be one of the following:
      // 1. A parameter outline:
      //    - Parameters:
      //      - x: ...
      //      - y: ...
      // 2. An exploded parameter list:
      //    - parameter x: ...
      //    - parameter y: ...
      // 3. Some other simple field, including "returns" (see SimpleFields.def)
      auto ListNowEmpty = extractParameterOutline(MC, L, ParamFields);
      ListNowEmpty |= extractSeparatedParams(MC, L, ParamFields);
      ListNowEmpty |= extractSimpleField(MC, L, Parts, BodyNodes);
      if (ListNowEmpty)
        continue; // This drops the empty list from the doc comment body.
    }
    BodyNodes.push_back(*C);
  }

  // Copy BodyNodes and ParamFields into the MarkupContext.
  Parts.BodyNodes = MC.allocateCopy(llvm::makeArrayRef(BodyNodes));
  Parts.ParamFields = MC.allocateCopy(llvm::makeArrayRef(ParamFields));

  for (auto Param : Parts.ParamFields) {
    auto ParamParts = extractCommentParts(MC, Param);
    Param->setParts(ParamParts);
  }

  return Parts;
}

Optional<DocComment *>
swift::getSingleDocComment(swift::markup::MarkupContext &MC, const Decl *D) {
  PrettyStackTraceDecl StackTrace("parsing comment for", D);

  auto RC = D->getRawComment();
  if (RC.isEmpty())
    return None;

  swift::markup::LineList LL = MC.getLineList(RC);
  auto *Doc = swift::markup::parseDocument(MC, LL);
  auto Parts = extractCommentParts(MC, Doc);
  return new (MC) DocComment(D, Doc, Parts);
}

static Optional<DocComment *>
getAnyBaseClassDocComment(swift::markup::MarkupContext &MC,
                          const ClassDecl *CD,
                          const Decl *D) {
  RawComment RC;

  if (const auto *VD = dyn_cast<ValueDecl>(D)) {
    const auto *BaseDecl = VD->getOverriddenDecl();
    while (BaseDecl) {
      RC = BaseDecl->getRawComment();
      if (!RC.isEmpty()) {
        swift::markup::LineList LL = MC.getLineList(RC);
        auto *Doc = swift::markup::parseDocument(MC, LL);
        auto Parts = extractCommentParts(MC, Doc);

        SmallString<48> RawCascadeText;
        llvm::raw_svector_ostream OS(RawCascadeText);
        OS << "This documentation comment was inherited from ";


        auto *Text = swift::markup::Text::create(MC, MC.allocateCopy(OS.str()));

        auto BaseClass = BaseDecl->getDeclContext()->getSelfClassDecl();

        auto *BaseClassMonospace =
          swift::markup::Code::create(MC,
                                      MC.allocateCopy(BaseClass->getNameStr()));

        auto *Period = swift::markup::Text::create(MC, ".");

        auto *Para = swift::markup::Paragraph::create(MC, {
          Text, BaseClassMonospace, Period
        });
        auto CascadeNote = swift::markup::NoteField::create(MC, {Para});

        SmallVector<const swift::markup::MarkupASTNode *, 8> BodyNodes {
          Parts.BodyNodes.begin(),
          Parts.BodyNodes.end()
        };
        BodyNodes.push_back(CascadeNote);
        Parts.BodyNodes = MC.allocateCopy(llvm::makeArrayRef(BodyNodes));

        return new (MC) DocComment(D, Doc, Parts);
      }

      BaseDecl = BaseDecl->getOverriddenDecl();
    }
  }
  
  return None;
}

static Optional<DocComment *>
getProtocolRequirementDocComment(swift::markup::MarkupContext &MC,
                                 const ProtocolDecl *ProtoExt,
                                 const Decl *D) {

  auto getSingleRequirementWithNonemptyDoc = [](const ProtocolDecl *P,
                                                const ValueDecl *VD)
    -> const ValueDecl * {
      SmallVector<ValueDecl *, 2> Members;
      P->lookupQualified(const_cast<ProtocolDecl *>(P),
                         VD->getFullName(),
                         NLOptions::NL_ProtocolMembers,
                         Members);
    SmallVector<const ValueDecl *, 1> ProtocolRequirements;
    for (auto Member : Members)
      if (isa<ProtocolDecl>(Member->getDeclContext()) &&
          Member->isProtocolRequirement())
        ProtocolRequirements.push_back(Member);

    if (ProtocolRequirements.size() == 1) {
      auto Requirement = ProtocolRequirements.front();
      if (!Requirement->getRawComment().isEmpty())
        return Requirement;
    }

    return nullptr;
  };

  if (const auto *VD = dyn_cast<ValueDecl>(D)) {
    SmallVector<const ValueDecl *, 4> RequirementsWithDocs;
    if (auto Requirement = getSingleRequirementWithNonemptyDoc(ProtoExt, VD))
      RequirementsWithDocs.push_back(Requirement);

    if (RequirementsWithDocs.size() == 1)
      return getSingleDocComment(MC, RequirementsWithDocs.front());
  }
  return None;
}

Optional<DocComment *>
swift::getCascadingDocComment(swift::markup::MarkupContext &MC, const Decl *D) {
  auto Doc = getSingleDocComment(MC, D);
  if (Doc.hasValue())
    return Doc;

  // If this refers to a class member, check to see if any
  // base classes have a doc comment and cascade it to here.
  if (const auto *CD = D->getDeclContext()->getSelfClassDecl())
    if (auto BaseClassDoc = getAnyBaseClassDocComment(MC, CD, D))
      return BaseClassDoc;

  if (const auto *PE = D->getDeclContext()->getExtendedProtocolDecl())
    if (auto ReqDoc = getProtocolRequirementDocComment(MC, PE, D))
      return ReqDoc;

  return None;
}
