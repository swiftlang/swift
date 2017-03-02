//===--- SILWitnessVisitor.h - Witness method table visitor -----*- C++ -*-===//
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
// This file defines the SILWitnessVisitor class, which is used to generate and
// perform lookups in witness method tables for protocols and protocol
// conformances.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILWITNESSVISITOR_H
#define SWIFT_SIL_SILWITNESSVISITOR_H

#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

/// A CRTP class for visiting the witnesses of a protocol.
///
/// The design here is that each entry (or small group of entries)
/// gets turned into a call to the implementation class describing
/// the exact variant of witness.  For example, for member
/// variables, there should be separate callbacks for adding a
/// getter/setter pair, for just adding a getter, and for adding a
/// physical projection (if we decide to support that).
///
/// You must override the following methods:
/// - addOutOfLineBaseProtocol()
/// - addMethod()
/// - addConstructor()
/// - addAssociatedType()

template <class T> class SILWitnessVisitor : public ASTVisitor<T> {
  T &asDerived() { return *static_cast<T*>(this); }

public:
  void visitProtocolDecl(ProtocolDecl *protocol) {
    // Associated types get added after the inherited conformances, but
    // before all the function requirements.
    bool haveAddedAssociatedTypes = false;
    auto addAssociatedTypes = [&] {
      if (haveAddedAssociatedTypes) return;
      haveAddedAssociatedTypes = true;

      for (Decl *member : protocol->getMembers()) {
        if (auto associatedType = dyn_cast<AssociatedTypeDecl>(member)) {
          // TODO: only add associated types when they're new?
          asDerived().addAssociatedType(associatedType);
        }
      }
    };

    for (auto &reqt : protocol->getRequirementSignature()
                              ->getCanonicalSignature()->getRequirements()) {
      switch (reqt.getKind()) {
      // These requirements don't show up in the witness table.
      case RequirementKind::Superclass:
      case RequirementKind::SameType:
      case RequirementKind::Layout:
        continue;

      case RequirementKind::Conformance: {
        auto type = CanType(reqt.getFirstType());
        assert(type->isTypeParameter());
        auto requirement =
          cast<ProtocolType>(CanType(reqt.getSecondType()))->getDecl();

        // ObjC protocols do not have witnesses.
        if (!Lowering::TypeConverter::protocolRequiresWitnessTable(requirement))
          continue;

        // If the type parameter is 'self', consider this to be protocol
        // inheritance.  In the canonical signature, these should all
        // come before any protocol requirements on associated types.
        if (auto parameter = dyn_cast<GenericTypeParamType>(type)) {
          assert(type->isEqual(protocol->getSelfInterfaceType()));
          assert(!haveAddedAssociatedTypes &&
                 "unexpected ordering of conformances");
          assert(parameter->getDepth() == 0 && parameter->getIndex() == 0 &&
                 "non-self type parameter in protocol");
          asDerived().addOutOfLineBaseProtocol(requirement);
          continue;
        }

        // Add the associated types if we haven't yet.
        addAssociatedTypes();

        // Otherwise, add an associated requirement.
        asDerived().addAssociatedConformance(type, requirement);
        continue;
      }
      }
      llvm_unreachable("bad requirement kind");
    }

    // Add the associated types if we haven't yet.
    addAssociatedTypes();

    // Visit the witnesses for the direct members of a protocol.
    for (Decl *member : protocol->getMembers())
      ASTVisitor<T>::visit(member);
  }

  /// Fallback for unexpected protocol requirements.
  void visitDecl(Decl *d) {
#ifndef NDEBUG
    d->print(llvm::errs());
#endif
    llvm_unreachable("unhandled protocol requirement");
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *sd) {
    asDerived().addMethod(sd->getGetter());
    if (sd->isSettable(sd->getDeclContext())) {
      asDerived().addMethod(sd->getSetter());
      if (sd->getMaterializeForSetFunc())
        asDerived().addMethod(sd->getMaterializeForSetFunc());
    }
  }

  void visitConstructorDecl(ConstructorDecl *cd) {
    asDerived().addConstructor(cd);
  }

  void visitFuncDecl(FuncDecl *func) {
    // Accessors are emitted by their var/subscript declaration.
    if (func->isAccessor())
      return;
    asDerived().addMethod(func);
  }

  void visitAssociatedTypeDecl(AssociatedTypeDecl *td) {
    // We already visited these in the first pass.
  }
    
  void visitTypeAliasDecl(TypeAliasDecl *tad) {
    // We don't care about these by themselves for witnesses.
  }

  void visitPatternBindingDecl(PatternBindingDecl *pbd) {
    // We only care about the contained VarDecls.
  }

  void visitIfConfigDecl(IfConfigDecl *icd) {
    // We only care about the active members, which were already subsumed by the
    // enclosing type.
  }
};

} // end namespace swift

#endif
