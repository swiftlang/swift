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
#include "swift/AST/ProtocolAssociations.h"
#include "swift/AST/Types.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/ErrorHandling.h"

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
    // The protocol conformance descriptor gets added first.
    asDerived().addProtocolConformanceDescriptor();

    // Associated types get added after the inherited conformances, but
    // before all the function requirements.
    bool haveAddedAssociatedTypes = false;
    auto addAssociatedTypes = [&] {
      if (haveAddedAssociatedTypes) return;
      haveAddedAssociatedTypes = true;

      SmallVector<AssociatedTypeDecl *, 2> associatedTypes;
      for (Decl *member : protocol->getMembers()) {
        if (auto associatedType = dyn_cast<AssociatedTypeDecl>(member)) {
          // If this is a new associated type (which does not override an
          // existing associated type), add it.
          if (associatedType->getOverriddenDecls().empty())
            associatedTypes.push_back(associatedType);
        }
      }

      // Sort associated types by name, for resilience.
      llvm::array_pod_sort(associatedTypes.begin(), associatedTypes.end(),
                           TypeDecl::compare);

      for (auto *associatedType : associatedTypes) {
        asDerived().addAssociatedType(AssociatedType(associatedType));
      }
    };

    for (const auto &reqt : protocol->getRequirementSignature()) {
      switch (reqt.getKind()) {
      // These requirements don't show up in the witness table.
      case RequirementKind::Superclass:
      case RequirementKind::SameType:
      case RequirementKind::Layout:
        continue;

      case RequirementKind::Conformance: {
        auto type = reqt.getFirstType()->getCanonicalType();
        assert(type->isTypeParameter());
        auto requirement =
          cast<ProtocolType>(reqt.getSecondType()->getCanonicalType())
            ->getDecl();

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
        AssociatedConformance assocConf(protocol, type, requirement);
        asDerived().addAssociatedConformance(assocConf);
        continue;
      }
      }
      llvm_unreachable("bad requirement kind");
    }

    // Add the associated types if we haven't yet.
    addAssociatedTypes();

    if (asDerived().shouldVisitRequirementSignatureOnly())
      return;

    // Visit the witnesses for the direct members of a protocol.
    for (Decl *member : protocol->getMembers())
      ASTVisitor<T>::visit(member);
  }

  /// If true, only the base protocols and associated types will be visited.
  /// The base implementation returns false.
  bool shouldVisitRequirementSignatureOnly() const {
    return false;
  }

  /// Fallback for unexpected protocol requirements.
  void visitDecl(Decl *d) {
    llvm_unreachable("unhandled protocol requirement");
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *sd) {
    sd->visitOpaqueAccessors([&](AccessorDecl *accessor) {
      asDerived().addMethod(SILDeclRef(accessor, SILDeclRef::Kind::Func));
    });
  }

  void visitConstructorDecl(ConstructorDecl *cd) {
    asDerived().addMethod(SILDeclRef(cd, SILDeclRef::Kind::Allocator));
  }

  void visitAccessorDecl(AccessorDecl *func) {
    // Accessors are emitted by visitAbstractStorageDecl, above.
  }

  void visitFuncDecl(FuncDecl *func) {
    assert(!isa<AccessorDecl>(func));
    asDerived().addMethod(SILDeclRef(func, SILDeclRef::Kind::Func));
  }

  void visitMissingMemberDecl(MissingMemberDecl *placeholder) {
    asDerived().addPlaceholder(placeholder);
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

  void visitPoundDiagnosticDecl(PoundDiagnosticDecl *pdd) {
    // We don't care about diagnostics at this stage.
  }
};

} // end namespace swift

#endif
