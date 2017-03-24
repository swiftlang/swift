//===--- SILVTableVisitor.h - Class vtable visitor -------------*- C++ -*-===//
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

namespace swift {

/// A CRTP class for visiting virtually-dispatched methods of a class.
///
/// You must override addMethod(SILDeclRef) in your subclass.
template <class T> class SILVTableVisitor {
  T &asDerived() { return *static_cast<T*>(this); }

  void maybeAddMethod(FuncDecl *fd) {
    assert(!fd->hasClangNode());

    // Observing accessors and addressors don't get vtable entries.
    if (fd->isObservingAccessor() ||
        fd->getAddressorKind() != AddressorKind::NotAddressor)
      return;

    maybeAddEntry(SILDeclRef(fd, SILDeclRef::Kind::Func));
  }

  void maybeAddConstructor(ConstructorDecl *cd) {
    assert(!cd->hasClangNode());

    SILDeclRef initRef(cd, SILDeclRef::Kind::Initializer);

    // Stub constructors don't get a vtable entry unless they were synthesized
    // to override a base class initializer.
    if (cd->hasStubImplementation() &&
        !initRef.getNextOverriddenVTableEntry())
      return;

    // Required constructors (or overrides thereof) have their allocating entry
    // point in the vtable.
    if (cd->isRequired())
      maybeAddEntry(SILDeclRef(cd, SILDeclRef::Kind::Allocator));

    // All constructors have their initializing constructor in the
    // vtable, which can be used by a convenience initializer.
    maybeAddEntry(SILDeclRef(cd, SILDeclRef::Kind::Initializer));
  }

  void maybeAddEntry(SILDeclRef declRef) {
    // If the method overrides something, we don't need a new entry.
    //
    // FIXME: We do, if the override changes the AST type.
    if (declRef.getNextOverriddenVTableEntry()) {
      auto baseRef = declRef.getBaseOverriddenVTableEntry();
      asDerived().addMethodOverride(baseRef, declRef);
      return;
    }

    auto *decl = declRef.getDecl();

    // Final members are always be called directly.
    // Dynamic methods are always accessed by objc_msgSend().
    if (decl->isFinal() || decl->isDynamic())
      return;

    // Special case -- materializeForSet on dynamic storage is not
    // itself dynamic, but should be treated as such for the
    // purpose of constructing the vtable.
    if (auto *fd = dyn_cast<FuncDecl>(decl)) {
      if (fd->getAccessorKind() == AccessorKind::IsMaterializeForSet &&
          fd->getAccessorStorageDecl()->isDynamic())
        return;
    }

    asDerived().addMethod(declRef);
  }

protected:
  void addVTableEntries(ClassDecl *theClass) {
    // Imported classes do not have a vtable.
    if (!theClass->hasKnownSwiftImplementation())
      return;

    for (auto member : theClass->getMembers()) {
      if (auto *fd = dyn_cast<FuncDecl>(member))
        maybeAddMethod(fd);
      else if (auto *cd = dyn_cast<ConstructorDecl>(member))
        maybeAddConstructor(cd);
    }
  }
};

}

#endif
