//===--- SILWitnessVisitor.h - Witness method table visitor -----*- C++ -*-===//
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

template <class T> class SILWitnessVisitor : public ASTVisitor<T> {
  T &asDerived() { return *static_cast<T*>(this); }

public:
  void visitProtocolDecl(ProtocolDecl *protocol) {
    // Visit inherited protocols.
    // TODO: We need to figure out all the guarantees we want here.
    // It would be abstractly good to allow conversion to a base
    // protocol to be trivial, but it's not clear that there's
    // really a structural guarantee we can rely on here.
    for (auto baseProto : protocol->getInheritedProtocols(nullptr)) {
      // ObjC protocols do not have witnesses.
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(baseProto))
        continue;

      asDerived().addOutOfLineBaseProtocol(baseProto);
    }

    /// Visit the witnesses for the direct members of a protocol.
    for (Decl *member : protocol->getMembers())
      ASTVisitor<T>::visit(member);
  }

  /// Fallback for unexpected protocol requirements.
  void visitDecl(Decl *d) {
    d->print(llvm::errs());
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
    asDerived().addAssociatedType(td);
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
