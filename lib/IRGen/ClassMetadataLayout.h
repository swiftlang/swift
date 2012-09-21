//===--- ClassMetadataLayout.h - CRTP for class metadata --------*- C++ -*-===//
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
// A CRTP helper class for class metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CLASSMETADATALAYOUT_H
#define SWIFT_IRGEN_CLASSMETADATALAYOUT_H

#include "llvm/ADT/SmallVector.h"
#include "swift/AST/Decl.h"
#include "GenProto.h"

namespace swift {
namespace irgen {

class IRGenModule;

/// A CRTP class for laying out class metadata.  Note that this does
/// *not* handle the metadata template stuff.
template <class Impl> class ClassMetadataLayout {
  Impl &asImpl() { return *static_cast<Impl*>(this); }

protected:
  IRGenModule &IGM;

  /// The most-derived class.
  ClassDecl *const TargetClass;

  ClassMetadataLayout(IRGenModule &IGM, ClassDecl *targetClass)
    : IGM(IGM), TargetClass(targetClass) {}

public:
  void layout() {
    // Common fields.
    asImpl().addMetadataFlags();
    asImpl().addValueWitnessTable();
    asImpl().addDestructorFunction();
    asImpl().addSizeFunction();

    // Class-specific fields.
    asImpl().addClassFields(TargetClass);
  }

protected:
  /// Add fields associated with the given class and its bases.
  void addClassFields(ClassDecl *theClass) {
    // TODO: base class

    // TODO: virtual methods

    if (auto generics = theClass->getGenericParamsOfContext()) {
      addGenericClassFields(theClass, *generics);
    }
  }

  /// Add fields related to the generics of this class declaration.
  /// TODO: don't add new fields that are implied by base class
  /// fields.  e.g., if B<T> extends A<T>, the witness for T in A's
  /// section should be enough.
  void addGenericClassFields(ClassDecl *theClass,
                             const GenericParamList &generics) {
    SmallVector<llvm::Type*, 4> signature;
    expandPolymorphicSignature(IGM, generics, signature);
    for (auto type : signature) {
      asImpl().addGenericWitness(theClass, type);
    }
  }
};

} // end namespace irgen
} // end namespace swift

#endif
