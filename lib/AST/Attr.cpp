//===--- Attr.cpp - Swift Language Attr ASTs ------------------------------===//
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
//  This file implements routines relating to declaration attributes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ASTContext.h"

using namespace swift;

/// A statically-allocated empty set of attributes.
const DeclAttributes NamedDecl::EmptyAttrs;

DeclAttributes &NamedDecl::getMutableAttrs() {
  // If we don't have mutable attribute storage yet, allocate some.
  if (Attrs == &EmptyAttrs)
    Attrs = getASTContext().Allocate<DeclAttributes>(1);
  return *const_cast<DeclAttributes*>(Attrs);
}
