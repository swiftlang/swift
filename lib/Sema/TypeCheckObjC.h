//===--- TypeCheckObjC.h - Type Checking for ObjC interop -------*- C++ -*-===//
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
// This file provides utilities for type-checking interoperability with
// Objective-C.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_TYPE_CHECK_OBJC_H
#define SWIFT_SEMA_TYPE_CHECK_OBJC_H

#include "swift/AST/ForeignErrorConvention.h"
#include "llvm/ADT/Optional.h"

namespace swift {

class AbstractFunctionDecl;
class ASTContext;
class SubscriptDecl;
class TypeChecker;
class ValueDecl;
class VarDecl;
class InFlightDiagnostic;

using llvm::Optional;

/// Describes the reason why are we trying to apply @objc to a declaration.
///
/// Should only affect diagnostics. If you change this enum, also change
/// the OBJC_ATTR_SELECT macro in DiagnosticsSema.def.
class ObjCReason {
public:
  // The kind of reason.
  enum Kind {
    /// Has the '@cdecl' attribute.
    ExplicitlyCDecl,
    /// Has the 'dynamic' modifier.
    ExplicitlyDynamic,
    /// Has an explicit '@objc' attribute.
    ExplicitlyObjC,
    /// Has an explicit '@IBOutlet' attribute.
    ExplicitlyIBOutlet,
    /// Has an explicit '@IBAction' attribute.
    ExplicitlyIBAction,
    /// Has an explicit '@IBSegueAction' attribute.
    ExplicitlyIBSegueAction,
    /// Has an explicit '@NSManaged' attribute.
    ExplicitlyNSManaged,
    /// Is a member of an @objc protocol.
    MemberOfObjCProtocol,
    /// Implicitly-introduced @objc.
    ImplicitlyObjC,
    /// Is an override of an @objc member.
    OverridesObjC,
    /// Is a witness to an @objc protocol requirement.
    WitnessToObjC,
    /// Has an explicit '@IBInspectable' attribute.
    ExplicitlyIBInspectable,
    /// Has an explicit '@GKInspectable' attribute.
    ExplicitlyGKInspectable,
    /// Is it a member of an @objc extension of a class.
    MemberOfObjCExtension,

    // These kinds do not appear in diagnostics.

    /// Is it a member of an @objcMembers class.
    MemberOfObjCMembersClass,
    /// A member of an Objective-C-defined class or subclass.
    MemberOfObjCSubclass,
    /// Is a member of an @objc enum.
    ElementOfObjCEnum,
    /// An accessor to a property.
    Accessor,
  };

private:
  Kind kind;

  /// When the kind is \c WitnessToObjC, the requirement being witnessed.
  ValueDecl * decl = nullptr;

  ObjCReason(Kind kind, ValueDecl *decl) : kind(kind), decl(decl) { }

public:
  /// Implicit conversion from the trivial reason kinds.
  ObjCReason(Kind kind) : kind(kind) {
    assert(kind != WitnessToObjC && "Use ObjCReason::witnessToObjC()");
  }

  /// Retrieve the kind of requirement.
  operator Kind() const { return kind; }

  /// Form a reason specifying that we have a witness to the given @objc
  /// requirement.
  static ObjCReason witnessToObjC(ValueDecl *requirement) {
    return ObjCReason(WitnessToObjC, requirement);
  }

  /// When the entity should be @objc because it is a witness to an @objc
  /// requirement, retrieve the requirement.
  ValueDecl *getObjCRequirement() const {
    assert(kind == WitnessToObjC);
    return decl;
  }
};

/// Determine whether we should diagnose conflicts due to inferring @objc
/// with this particular reason.
bool shouldDiagnoseObjCReason(ObjCReason reason, ASTContext &ctx);

/// Return the %select discriminator for the OBJC_ATTR_SELECT macro used to
/// complain about the correct attribute during @objc inference.
unsigned getObjCDiagnosticAttrKind(ObjCReason reason);

/// Determine whether the given function can be represented in Objective-C,
/// and figure out its foreign error convention (if any).
bool isRepresentableInObjC(const AbstractFunctionDecl *AFD,
                           ObjCReason Reason,
                           Optional<ForeignErrorConvention> &errorConvention);

/// Determine whether the given variable can be represented in Objective-C.
bool isRepresentableInObjC(const VarDecl *VD, ObjCReason Reason);

/// Determine whether the given subscript can be represented in Objective-C.
bool isRepresentableInObjC(const SubscriptDecl *SD, ObjCReason Reason);

/// Check whether the given declaration can be represented in Objective-C.
bool canBeRepresentedInObjC(const ValueDecl *decl);

/// Attach Fix-Its to the given diagnostic that updates the name of the
/// given declaration to the desired target name.
///
/// \returns false if the name could not be fixed.
bool fixDeclarationName(InFlightDiagnostic &diag, const ValueDecl *decl,
                        DeclName targetName);

/// Fix the Objective-C name of the given declaration to match the provided
/// Objective-C selector.
///
/// \param ignoreImpliedName When true, ignore the implied name of the
/// given declaration, because it no longer applies.
///
/// For properties, the selector should be a zero-parameter selector of the
/// given property's name.
bool fixDeclarationObjCName(InFlightDiagnostic &diag, const ValueDecl *decl,
                            Optional<ObjCSelector> nameOpt,
                            Optional<ObjCSelector> targetNameOpt,
                            bool ignoreImpliedName = false);

} // end namespace swift

#endif // SWIFT_SEMA_TYPE_CHECK_OBJC_H
