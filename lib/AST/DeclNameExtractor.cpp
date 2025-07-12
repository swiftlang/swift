//===--- DeclNameExtractor.cpp ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the DeclNameExtractor utility.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/DeclNameExtractor.h"

using namespace swift;
using namespace Demangle;

DeclName DeclNameExtractor::extractDeclName(Node *node) {
  if (!node)
    return DeclName();
  
  switch (node->getKind()) {
  case Node::Kind::Class:
  case Node::Kind::Structure:
  case Node::Kind::Enum:
  case Node::Kind::Protocol:
  case Node::Kind::TypeAlias:
  case Node::Kind::OtherNominalType:
  case Node::Kind::AssociatedType:
  case Node::Kind::AssociatedTypeRef:
  case Node::Kind::GenericTypeParamDecl:
  case Node::Kind::Variable:
  case Node::Kind::Macro:
    return extractIdentifierName(node);
    
  case Node::Kind::Constructor:
  case Node::Kind::Allocator:
    return DeclName(DeclBaseName::createConstructor());
    
  case Node::Kind::Destructor:
  case Node::Kind::Deallocator:
  case Node::Kind::IsolatedDeallocator:
    return DeclName(DeclBaseName::createDestructor());
    
  case Node::Kind::Module:
    return extractTextName(node);
    
  case Node::Kind::Function:
    return extractFunctionLikeName(node);
    
  case Node::Kind::Subscript:
    return extractFunctionLikeName(node);
    
  default:
    // For any other node types, we can't extract a meaningful name
    return DeclName();
  }
  
}

DeclName DeclNameExtractor::extractIdentifierName(Node *node) {
  auto identifierNode = node->getChild(1);
  if (!identifierNode)
    return DeclName();

  StringRef name;
  StringRef relatedEntityKind;
  Identifier privateDiscriminator;
  if (!extractNameNodeInfo(Ctx, identifierNode, name, relatedEntityKind,
                           privateDiscriminator)) {
    return DeclName();
  }

  return getIdentifier(Ctx, name);
}

DeclName DeclNameExtractor::extractTextName(Node *node) {
  if (!node->hasText())
    return DeclName();
  
  auto identifier = getIdentifier(Ctx, node->getText());
  return DeclName(identifier);
}

DeclName DeclNameExtractor::extractFunctionLikeName(Node *node) {
  assert(node->getKind() == Node::Kind::Function ||
         node->getKind() == Node::Kind::Subscript);
  
  DeclBaseName BaseName;
  if (node->getKind() == Node::Kind::Function)
    BaseName = extractIdentifierName(node).getBaseName();
  else
    BaseName = DeclBaseName::createSubscript();
  
  if (BaseName.empty())
    return DeclName();
  
  // Location of LabelList node if present, otherwise a Type node.
  unsigned LabelListIdx;
  if (node->getKind() == Node::Kind::Function)
    LabelListIdx = 2;
  else
    LabelListIdx = 1;
  
  auto *LabelsOrType = node->getChild(LabelListIdx);
  assert(LabelsOrType != nullptr && (LabelsOrType->getKind() == Node::Kind::LabelList ||
                                     LabelsOrType->getKind() == Node::Kind::Type));
  
  SmallVector<Identifier, 4> ArgLabels;
  if (LabelsOrType->getKind() == Node::Kind::LabelList)
    extractArgLabelsFromLabelList(LabelsOrType, ArgLabels);
  else
    extractArgLabelsFromType(LabelsOrType, ArgLabels);
  
  if (ArgLabels.empty())
    return DeclName(BaseName);
  
  return DeclName(Ctx, BaseName, ArgLabels);
}

void DeclNameExtractor::extractArgLabelsFromLabelList(Node *LabelList,
                                   SmallVectorImpl<Identifier> &ArgLabels) {
  assert(LabelList->getKind() == Node::Kind::LabelList);
  
  for (unsigned i = 0; i < LabelList->getNumChildren(); ++i) {
    auto *Label = LabelList->getChild(i);
    
    assert(Label && (Label->getKind() == Node::Kind::Identifier ||
                     Label->getKind() == Node::Kind::FirstElementMarker));
    
    if (Label->getKind() == Node::Kind::Identifier) {
      ArgLabels.push_back(getIdentifier(Ctx, Label->getText()));
    } else {
      ArgLabels.push_back(Identifier());
    }
  }
}

void DeclNameExtractor::extractArgLabelsFromType(Node *Type, SmallVectorImpl<Identifier> &ArgLabels) {
  auto ArgTuple = Type->findByKind(Demangle::Node::Kind::ArgumentTuple,
                                   /*maxDepth=*/5);
  if (ArgTuple == nullptr)
    return;
  
  auto Params = ArgTuple->getFirstChild();
  auto ParamsType = Params->getFirstChild();
  if (ParamsType == nullptr)
    return;
  
  if (ParamsType->getKind() != Demangle::Node::Kind::Tuple) {
    // A single, unnamed parameter
    ArgLabels.push_back(Identifier());
    return;
  }
  
  // More than one parameter are present
  while (Params && Params->getFirstChild() &&
         Params->getFirstChild()->getKind() != Node::Kind::TupleElement) {
    Params = Params->getFirstChild();
  }
  
  if (Params) {
    for (size_t i = 0; i < Params->getNumChildren(); ++i) {
      ArgLabels.push_back(Identifier());
    }
  }
}

Identifier swift::Demangle::getIdentifier(ASTContext &Ctx, StringRef name) {
  if (name.size() > 1 && name.front() == '`' && name.back() == '`') {
    // Raw identifiers have backticks affixed before mangling. We need to
    // remove those before creating the Identifier for the AST, which doesn't
    // encode the backticks.
    std::string fixedName;
    for (size_t i = 1; i < name.size() - 1; ++i) {
      unsigned char ch = name[i];
      // Raw identifiers have the space (U+0020) replaced with a non-breaking
      // space (U+00A0, UTF-8: 0xC2 0xA0) in their mangling so that parts of
      // the runtime that use space as a delimiter remain compatible with
      // these identifiers. Flip it back.
      if (ch == 0xc2 && i < name.size() - 2 &&
          (unsigned char)name[i + 1] == 0xa0) {
        fixedName.push_back(' ');
        ++i;
      } else {
        fixedName.push_back(ch);
      }
    }
    return Ctx.getIdentifier(fixedName);
  }
  return Ctx.getIdentifier(name);
}

bool swift::Demangle::extractNameNodeInfo(ASTContext &Ctx, Node *node,
                                          StringRef &name,
                                          StringRef &relatedEntityKind,
                                          Identifier &privateDiscriminator) {
  switch (node->getKind()) {
  case Demangle::Node::Kind::Identifier:
    name = node->getText();
    return true;
    
  case Demangle::Node::Kind::PrivateDeclName:
    name = node->getChild(1)->getText();
    privateDiscriminator = getIdentifier(Ctx, node->getChild(0)->getText());
    return true;
    
  case Demangle::Node::Kind::RelatedEntityDeclName:
    name = node->getChild(1)->getText();
    relatedEntityKind = node->getFirstChild()->getText();
    return true;

  default:
    return false;
  }
}
