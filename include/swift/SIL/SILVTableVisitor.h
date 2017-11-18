//===--- SILVTableVisitor.h - Class vtable visitor --------------*- C++ -*-===//
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
// This file defines the SILVTableVisitor class, which is used to generate and
// perform lookups in class method vtables.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILVTABLEVISITOR_H
#define SWIFT_SIL_SILVTABLEVISITOR_H

#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/SIL/TypeLowering.h"

namespace swift {

/// A CRTP class for visiting virtually-dispatched methods of a class.
///
/// You must override these two methods in your subclass:
///
/// - addMethod(SILDeclRef):
///   introduce a new vtable entry
///
/// - addMethodOverride(SILDeclRef baseRef, SILDeclRef derivedRef):
///   update vtable entry for baseRef to call derivedRef
///
template <class T> class SILVTableVisitor {
  Lowering::TypeConverter &Types;

  T &asDerived() { return *static_cast<T*>(this); }

  void maybeAddMethod(FuncDecl *fd) {
    assert(!fd->hasClangNode());

    SILDeclRef constant(fd, SILDeclRef::Kind::Func);
    maybeAddEntry(constant, constant.requiresNewVTableEntry());
  }

  void maybeAddConstructor(ConstructorDecl *cd) {
    assert(!cd->hasClangNode());

    // Required constructors (or overrides thereof) have their allocating entry
    // point in the vtable.
    if (cd->isRequired()) {
      SILDeclRef constant(cd, SILDeclRef::Kind::Allocator);
      maybeAddEntry(constant, constant.requiresNewVTableEntry());
    }

    // All constructors have their initializing constructor in the
    // vtable, which can be used by a convenience initializer.
    SILDeclRef constant(cd, SILDeclRef::Kind::Initializer);
    maybeAddEntry(constant, constant.requiresNewVTableEntry());
  }

  void maybeAddEntry(SILDeclRef declRef, bool needsNewEntry) {
    // Introduce a new entry if required.
    if (needsNewEntry)
      asDerived().addMethod(declRef);

    // Update any existing entries that it overrides.
    auto nextRef = declRef;
    while ((nextRef = nextRef.getNextOverriddenVTableEntry())) {
      auto baseRef = Types.getOverriddenVTableEntry(nextRef);
      asDerived().addMethodOverride(baseRef, declRef);
      nextRef = baseRef;
    }
  }

protected:
  SILVTableVisitor(Lowering::TypeConverter &Types) : Types(Types) {}

  void addVTableEntries(ClassDecl *theClass) {
    // Imported classes do not have a vtable.
    if (!theClass->hasKnownSwiftImplementation())
      return;

    for (auto member : theClass->getMembers()) {
      if (auto *fd = dyn_cast<FuncDecl>(member))
        maybeAddMethod(fd);
      else if (auto *cd = dyn_cast<ConstructorDecl>(member))
        maybeAddConstructor(cd);
      else if (auto *placeholder = dyn_cast<MissingMemberDecl>(member))
        asDerived().addPlaceholder(placeholder);
    }
  }
};

}

#endif
