//===--- ExistentialLayout.h - Existential type decomposition ---*- C++ -*-===//
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
//
// This file defines the ExistentialLayout struct.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_EXISTENTIAL_LAYOUT_H
#define SWIFT_EXISTENTIAL_LAYOUT_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
  class ProtocolDecl;
  class ProtocolType;
  class ProtocolCompositionType;

struct ExistentialLayout {
  ExistentialLayout() {
    hasExplicitAnyObject = false;
    containsNonObjCProtocol = false;
    singleProtocol = nullptr;
  }

  ExistentialLayout(ProtocolType *type);
  ExistentialLayout(ProtocolCompositionType *type);

  /// The superclass constraint, if any.
  Type superclass;

  /// Whether the existential contains an explicit '& AnyObject' constraint.
  bool hasExplicitAnyObject : 1;

  /// Whether any protocol members are non-@objc.
  bool containsNonObjCProtocol : 1;

  bool isAnyObject() const;

  bool isObjC() const {
    // FIXME: Does the superclass have to be @objc?
    return ((superclass ||
             hasExplicitAnyObject ||
             getProtocols().size() > 0)
            && !containsNonObjCProtocol);
  }

  /// Whether the existential requires a class, either via an explicit
  /// '& AnyObject' member or because of a superclass or protocol constraint.
  bool requiresClass() const;

  // Does this existential contain the Error protocol?
  bool isExistentialWithError(ASTContext &ctx) const;

  // Does this existential consist of an Error protocol only with no other
  // constraints?
  bool isErrorExistential() const;

  ArrayRef<ProtocolType *> getProtocols() const {
    if (singleProtocol)
      return ArrayRef<ProtocolType *>{&singleProtocol, 1};
    return multipleProtocols;
  }

  LayoutConstraint getLayoutConstraint() const;

private:
  // Inline storage for 'protocols' member above when computing
  // layout of a single ProtocolType
  ProtocolType *singleProtocol;

  /// Zero or more protocol constraints.
  ArrayRef<ProtocolType *> multipleProtocols;
};

}

#endif  // SWIFT_EXISTENTIAL_LAYOUT_H
