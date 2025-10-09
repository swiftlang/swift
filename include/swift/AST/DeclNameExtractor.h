//===--- DeclNameExtractor.h - Decl Name Demangling -------------*- C++ -*-===//
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
// This file defines the DeclNameExtractor utility.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Identifier.h"

namespace swift {
namespace Demangle {

class Node;

class DeclNameExtractor {
private:
  ASTContext &Ctx;

public:
  DeclNameExtractor(ASTContext &ctx) : Ctx(ctx) {}

  /// Extract a DeclName from a demangling node
  /// \returns true if a \c DeclName is found, false otherwise.
  bool extractDeclName(Node *node, DeclName &name,
                       Identifier &privateDiscriminator);

private:
  bool extractIdentifierName(Node *node, DeclName &declName,
                             Identifier &privateDiscriminator);
  bool extractFunctionLikeName(Node *node, DeclName &declName,
                               Identifier &privateDiscriminator);
  void extractArgLabelsFromLabelList(Node *LabelList,
                                     SmallVectorImpl<Identifier> &ArgLabels);
  void extractArgLabelsFromType(Node *Type,
                                SmallVectorImpl<Identifier> &ArgLabels);
  DeclBaseName extractOperatorName(Node *node);
};

/// Returns an identifier with the given name, automatically removing any
/// surrounding backticks that are present for raw identifiers.
Identifier getIdentifier(ASTContext &Ctx, StringRef name);

bool extractNameNodeInfo(ASTContext &Ctx, Node *node, StringRef &name,
                         StringRef &relatedEntityKind,
                         Identifier &privateDiscriminator);

} // namespace Demangle
} // namespace swift
