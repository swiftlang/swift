//===--- TypeCheckAvailability.h - Availability Diagnostics -----*- C++ -*-===//
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

#ifndef SWIFT_SEMA_TYPE_CHECK_AVAILABILITY_H
#define SWIFT_SEMA_TYPE_CHECK_AVAILABILITY_H

#include "swift/AST/AttrKind.h"
#include "swift/AST/AvailabilityConstraint.h"
#include "swift/AST/AvailabilityContext.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include <optional>

namespace swift {
  class ApplyExpr;
  class AvailableAttr;
  class Expr;
  class ClosureExpr;
  class InFlightDiagnostic;
  class Decl;
  class ProtocolConformanceRef;
  class RootProtocolConformance;
  class Stmt;
  class SubstitutionMap;
  class Type;
  class TypeRepr;
  class ValueDecl;

enum class DeclAvailabilityFlag : uint8_t {
  /// Do not diagnose uses of protocols in versions before they were introduced.
  /// We allow a type to conform to a protocol that is less available than the
  /// type itself. This enables a type to retroactively model or directly conform
  /// to a protocol only available on newer OSes and yet still be used on older
  /// OSes.
  AllowPotentiallyUnavailableProtocol = 1 << 0,

  /// Diagnose uses of declarations in versions before they were introduced, but
  /// do not return true to indicate that a diagnostic was emitted.
  ContinueOnPotentialUnavailability = 1 << 1,

  /// If a diagnostic must be emitted, use a variant indicating that the usage
  /// is inout and both the getter and setter must be available.
  ForInout = 1 << 2,

  /// If an error diagnostic would normally be emitted, demote the error to a
  /// warning. Used for ObjC key path components.
  ForObjCKeyPath = 1 << 3,
  
  /// Do not diagnose potential decl unavailability if that unavailability
  /// would only occur at or below the deployment target.
  AllowPotentiallyUnavailableAtOrBelowDeploymentTarget = 1 << 4,
};
using DeclAvailabilityFlags = OptionSet<DeclAvailabilityFlag>;

// This enum must be kept in sync with
// diag::decl_from_hidden_module and
// diag::conformance_from_implementation_only_module.
enum class ExportabilityReason : unsigned {
  General,
  PropertyWrapper,
  ResultBuilder,
  ExtensionWithPublicMembers,
  ExtensionWithConditionalConformances,
  Inheritance
};

/// A description of the restrictions on what declarations can be referenced
/// from the signature or body of a declaration.
///
/// We say a declaration is "exported" if all of the following holds:
///
/// - the declaration is `public` or `@usableFromInline`
/// - the declaration is not `@_spi`
/// - the declaration was not imported from an `@_implementationOnly` import
///
/// The "signature" of a declaration is the set of all types written in the
/// declaration (such as function parameter and return types), but not
/// including the function body.
///
/// The signature of an exported declaration can only reference other
/// exported types.
///
/// The body of an inlinable function can only reference other `public` and
/// `@usableFromInline` declarations; furthermore, if the inlinable
/// function is not `@_spi`, its body can only reference other exported
/// declarations.
///
/// The ExportContext also stores if the location in the program is inside
/// of a function or type body with deprecated or unavailable availability.
/// This allows referencing other deprecated and unavailable declarations,
/// without producing a warning or error, respectively.
class ExportContext {
  DeclContext *DC;
  AvailabilityContext Availability;
  FragileFunctionKind FragileKind;
  unsigned SPI : 1;
  unsigned Exported : 1;
  unsigned Implicit : 1;
  unsigned Reason : 3;

  ExportContext(DeclContext *DC, AvailabilityContext availability,
                FragileFunctionKind kind, bool spi, bool exported,
                bool implicit);

public:

  /// Create an instance describing the types that can be referenced from the
  /// given declaration's signature.
  ///
  /// If the declaration is exported, the resulting context is restricted to
  /// referencing exported types only. Otherwise it can reference anything.
  static ExportContext forDeclSignature(Decl *D);

  /// Create an instance describing the declarations that can be referenced
  /// from the given function's body.
  ///
  /// If the function is inlinable, the resulting context is restricted to
  /// referencing ABI-public declarations only. Furthermore, if the function
  /// is exported, referenced declarations must also be exported. Otherwise
  /// it can reference anything.
  static ExportContext forFunctionBody(DeclContext *DC, SourceLoc loc);

  /// Create an instance describing associated conformances that can be
  /// referenced from the conformance defined by the given DeclContext,
  /// which must be a NominalTypeDecl or ExtensionDecl.
  static ExportContext forConformance(DeclContext *DC, ProtocolDecl *proto);

  /// Produce a new context with the same properties as this one, except
  /// changing the ExportabilityReason. This only affects diagnostics.
  ExportContext withReason(ExportabilityReason reason) const;

  /// Produce a new context with the same properties as this one, except
  /// that if 'exported' is false, the resulting context can reference
  /// declarations that are not exported. If 'exported' is true, the
  /// resulting context is identical to this one.
  ///
  /// That is, this will perform a 'bitwise and' on the 'exported' bit.
  ExportContext withExported(bool exported) const;

  /// Produce a new context with the same properties as this one, except the
  /// availability context is constrained by \p availability if necessary.
  ExportContext
  withRefinedAvailability(const AvailabilityRange &availability) const;

  DeclContext *getDeclContext() const { return DC; }

  AvailabilityContext getAvailability() const { return Availability; }

  AvailabilityRange getAvailabilityRange() const {
    return Availability.getPlatformRange();
  }

  /// If not 'None', the context has the inlinable function body restriction.
  FragileFunctionKind getFragileFunctionKind() const { return FragileKind; }

  /// If true, the context is part of a synthesized declaration, and
  /// availability checking should be disabled.
  bool isImplicit() const { return Implicit; }

  /// If true, the context is SPI and can reference SPI declarations.
  bool isSPI() const { return SPI; }

  /// If true, the context is exported and cannot reference SPI declarations
  /// or declarations from `@_implementationOnly` imports.
  bool isExported() const { return Exported; }

  /// If true, the context is part of a deprecated declaration and can
  /// reference other deprecated declarations without warning.
  bool isDeprecated() const { return Availability.isDeprecated(); }

  std::optional<PlatformKind> getUnavailablePlatformKind() const {
    return Availability.getUnavailablePlatformKind();
  }

  /// If true, the context can only reference exported declarations, either
  /// because it is the signature context of an exported declaration, or
  /// because it is the function body context of an inlinable function.
  bool mustOnlyReferenceExportedDecls() const;

  /// Get the ExportabilityReason for diagnostics. If this is 'None', there
  /// are no restrictions on referencing unexported declarations.
  std::optional<ExportabilityReason> getExportabilityReason() const;

  /// If \p decl is unconditionally unavailable in this context, and the context
  /// is not also unavailable in the same way, then this returns the specific
  /// `@available` attribute that makes the decl unavailable. Otherwise, returns
  /// nullptr.
  const AvailableAttr *shouldDiagnoseDeclAsUnavailable(const Decl *decl) const;
};

/// Check if a declaration is exported as part of a module's external interface.
/// This includes public and @usableFromInline decls.
bool isExported(const ValueDecl *VD);
bool isExported(const ExtensionDecl *ED);
bool isExported(const Decl *D);

/// Diagnose uses of unavailable declarations in expressions.
void diagnoseExprAvailability(const Expr *E, DeclContext *DC);

/// Diagnose uses of unavailable declarations in statements (via patterns, etc)
/// but not expressions.
void diagnoseStmtAvailability(const Stmt *S, DeclContext *DC);

/// Checks both a TypeRepr and a Type, but avoids emitting duplicate
/// diagnostics by only checking the Type if the TypeRepr succeeded.
void diagnoseTypeAvailability(const TypeRepr *TR, Type T, SourceLoc loc,
                              const ExportContext &context,
                              DeclAvailabilityFlags flags = std::nullopt);

bool
diagnoseConformanceAvailability(SourceLoc loc,
                                ProtocolConformanceRef conformance,
                                const ExportContext &context,
                                Type depTy=Type(),
                                Type replacementTy=Type(),
                                bool warnIfConformanceUnavailablePreSwift6 = false,
                                bool preconcurrency = false);

/// Diagnose uses of unavailable declarations. Returns true if a diagnostic
/// was emitted.
bool diagnoseDeclAvailability(const ValueDecl *D, SourceRange R,
                              const Expr *call, const ExportContext &where,
                              DeclAvailabilityFlags flags = std::nullopt);

/// Emit a diagnostic for an available declaration that overrides an
/// unavailable declaration.
void diagnoseOverrideOfUnavailableDecl(ValueDecl *override,
                                       const ValueDecl *base,
                                       const AvailableAttr *attr);

/// Checks whether a declaration should be considered unavailable when referred
/// to in the given declaration context and availability context and, if so,
/// returns a result that describes the unsatisfied constraint.
/// Returns `std::nullopt` if the declaration is available.
std::optional<AvailabilityConstraint>
getUnsatisfiedAvailabilityConstraint(const Decl *decl,
                                     AvailabilityContext availabilityContext);

/// Checks whether a declaration should be considered unavailable when referred
/// to at the given source location in the given decl context and, if so,
/// returns a result that describes the unsatisfied constraint.
/// Returns `std::nullopt` if the declaration is available.
std::optional<AvailabilityConstraint> getUnsatisfiedAvailabilityConstraint(
    const Decl *decl, const DeclContext *referenceDC, SourceLoc referenceLoc);

/// Diagnose uses of the runtime support of the given type, such as
/// type metadata and dynamic casting.
///
/// Returns \c true if a diagnostic was emitted.
bool checkTypeMetadataAvailability(Type type, SourceRange loc,
                                   const DeclContext *DC);

/// Check if \p decl has a introduction version required by -require-explicit-availability
void checkExplicitAvailability(Decl *decl);

} // namespace swift

#endif // SWIFT_SEMA_TYPE_CHECK_AVAILABILITY_H

