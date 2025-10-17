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
// The ExistentialLayout struct describes the in-memory layout of an existential
// type.
//
// It flattens and canonicalizes protocol compositions, and also expands defaults
// for invertible protocols.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_EXISTENTIAL_LAYOUT_H
#define SWIFT_EXISTENTIAL_LAYOUT_H

#include "swift/Basic/ArrayRefView.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Types.h"

namespace swift {
  class ProtocolDecl;
  class ProtocolType;
  class ProtocolCompositionType;

struct ExistentialLayout {
  enum Kind { Class, Error, Opaque };

  ExistentialLayout() {
    hasExplicitAnyObject = false;
    containsObjCProtocol = false;
    containsSwiftProtocol = false;
    representsAnyObject = false;
  }

  ExistentialLayout(CanProtocolType type);
  ExistentialLayout(CanProtocolCompositionType type);
  ExistentialLayout(CanParameterizedProtocolType type);

  /// The explicit superclass constraint, if any.
  Type explicitSuperclass;

  /// Whether the existential contains an explicit '& AnyObject' constraint.
  bool hasExplicitAnyObject : 1;

  /// Whether any protocol members are @objc.
  bool containsObjCProtocol : 1;

  /// Whether any protocol members require a witness table.
  bool containsSwiftProtocol : 1;

  /// Whether this layout is the canonical layout for plain-old 'AnyObject'.
  bool representsAnyObject : 1;

  /// Return the kind of this existential (class/error/opaque).
  Kind getKind() {
    if (requiresClass())
      return Kind::Class;
    if (isErrorExistential())
      return Kind::Error;

    // The logic here is that opaque is the complement of class + error,
    // i.e. we don't have more concrete information guiding the layout
    // and it doesn't fall into the special-case Error representation.
    return Kind::Opaque;
  }

  bool isAnyObject() const { return representsAnyObject; }

  bool isObjC() const {
    // FIXME: Does the superclass have to be @objc?
    return ((explicitSuperclass ||
             hasExplicitAnyObject ||
             containsObjCProtocol) &&
            !containsSwiftProtocol);
  }

  /// Whether the existential requires a class, either via an explicit
  /// '& AnyObject' member or because of a superclass or protocol constraint.
  bool requiresClass() const;

  /// Returns the existential's superclass, if any; this is either an explicit
  /// superclass term in a composition type, or the superclass of one of the
  /// protocols.
  Type getSuperclass() const;

  /// Does this existential contain the Error protocol?
  bool isExistentialWithError(ASTContext &ctx) const;

  /// Does this existential consist of an Error protocol only with no other
  /// constraints?
  bool isErrorExistential() const;

  ArrayRef<ProtocolDecl*> getProtocols() const & {
    return protocols;
  }
  /// The returned ArrayRef points to internal storage, so
  /// calling this on a temporary is likely to be incorrect.
  ArrayRef<ProtocolDecl*> getProtocols() const && = delete;

  /// Determine whether this refers to any non-marker protocols.
  bool containsNonMarkerProtocols() const;

  ArrayRef<ParameterizedProtocolType *> getParameterizedProtocols() const & {
    return parameterized;
  }
  /// The returned ArrayRef points to internal storage, so
  /// calling this on a temporary is likely to be incorrect.
  ArrayRef<ProtocolDecl*> getParameterizedProtocols() const && = delete;

  LayoutConstraint getLayoutConstraint() const;

  /// Whether this layout has any inverses within its signature.
  bool hasInverses() const {
    return !inverses.empty();
  }

  /// Whether this existential needs to have an extended existential shape. This
  /// is relevant for the mangler to mangle as a symbolic link where possible
  /// and for IRGen directly emitting some existentials.
  ///
  /// If 'allowInverses' is false, then regardless of if this existential layout
  /// has inverse requirements those will not influence the need for having a
  /// shape.
  bool needsExtendedShape(bool allowInverses = true) const;

private:
  SmallVector<ProtocolDecl *, 4> protocols;
  SmallVector<ParameterizedProtocolType *, 4> parameterized;
  InvertibleProtocolSet inverses;
};

}

#endif  // SWIFT_EXISTENTIAL_LAYOUT_H
