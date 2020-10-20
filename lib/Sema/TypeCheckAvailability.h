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

#include "swift/AST/DeclContext.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/OptionSet.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"

namespace swift {
  class ApplyExpr;
  class AvailableAttr;
  class Expr;
  class InFlightDiagnostic;
  class Decl;
  class ProtocolConformanceRef;
  class Stmt;
  class SubstitutionMap;
  class Type;
  class TypeRepr;
  class ValueDecl;
enum class DeclAvailabilityFlag : uint8_t {
  /// Do not diagnose uses of protocols in versions before they were introduced.
  /// Used when type-checking protocol conformances, since conforming to a
  /// protocol that doesn't exist yet is allowed.
  AllowPotentiallyUnavailableProtocol = 1 << 0,

  /// Diagnose uses of declarations in versions before they were introduced, but
  /// do not return true to indicate that a diagnostic was emitted.
  ContinueOnPotentialUnavailability = 1 << 1,

  /// If a diagnostic must be emitted, use a variant indicating that the usage
  /// is inout and both the getter and setter must be available.
  ForInout = 1 << 2,

  /// Do not diagnose uses of declarations in versions before they were
  /// introduced. Used to work around availability-checker bugs.
  AllowPotentiallyUnavailable = 1 << 3,

  /// If an error diagnostic would normally be emitted, demote the error to a
  /// warning. Used for ObjC key path components.
  ForObjCKeyPath = 1 << 4
};
using DeclAvailabilityFlags = OptionSet<DeclAvailabilityFlag>;

// This enum must be kept in sync with
// diag::decl_from_hidden_module and
// diag::conformance_from_implementation_only_module.
enum class ExportabilityReason : unsigned {
  General,
  PropertyWrapper,
  ExtensionWithPublicMembers,
  ExtensionWithConditionalConformances
};

class ExportContext {
  DeclContext *DC;
  FragileFunctionKind FragileKind;
  unsigned SPI : 1;
  unsigned Exported : 1;
  ExportabilityReason Reason;

  ExportContext(DeclContext *DC, FragileFunctionKind kind, bool spi, bool exported);

public:
  static ExportContext forDeclSignature(Decl *D);
  static ExportContext forFunctionBody(DeclContext *DC);

  ExportContext forReason(ExportabilityReason reason) const;
  ExportContext forExported(bool exported) const;

  DeclContext *getDeclContext() const { return DC; }
  FragileFunctionKind getFragileFunctionKind() const { return FragileKind; }

  bool isSPI() const { return SPI; }
  bool isExported() const { return Exported; }

  bool mustOnlyReferenceExportedDecls() const;

  Optional<ExportabilityReason> getExportabilityReason() const;
};

/// Check if a public declaration is part of a module's API; that is, this
/// will return false if the declaration is @_spi or @_implementationOnly.
bool isExported(const ValueDecl *VD);
bool isExported(const Decl *D);

/// Diagnose uses of unavailable declarations in expressions.
void diagAvailability(const Expr *E, DeclContext *DC);

/// Diagnose uses of unavailable declarations in statements (via patterns, etc),
/// without walking into expressions.
void diagAvailability(const Stmt *S, DeclContext *DC);

/// Diagnose uses of unavailable declarations in types.
bool diagnoseTypeReprAvailability(const TypeRepr *T,
                                  ExportContext context,
                                  DeclAvailabilityFlags flags = None);

/// Diagnose uses of unavailable conformances in types.
void diagnoseTypeAvailability(Type T, SourceLoc loc,
                              ExportContext context,
                              DeclAvailabilityFlags flags = None);

/// Checks both a TypeRepr and a Type, but avoids emitting duplicate
/// diagnostics by only checking the Type if the TypeRepr succeeded.
void diagnoseTypeAvailability(const TypeRepr *TR, Type T, SourceLoc loc,
                              ExportContext context,
                              DeclAvailabilityFlags flags = None);

bool
diagnoseConformanceAvailability(SourceLoc loc,
                                ProtocolConformanceRef conformance,
                                ExportContext context);

bool
diagnoseSubstitutionMapAvailability(SourceLoc loc,
                                    SubstitutionMap subs,
                                    ExportContext context);

/// Run the Availability-diagnostics algorithm otherwise used in an expr
/// context, but for non-expr contexts such as TypeDecls referenced from
/// TypeReprs.
bool diagnoseDeclAvailability(const ValueDecl *Decl,
                              SourceRange R,
                              ExportContext context,
                              DeclAvailabilityFlags flags = None);

void diagnoseUnavailableOverride(ValueDecl *override,
                                 const ValueDecl *base,
                                 const AvailableAttr *attr);

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
bool diagnoseExplicitUnavailability(const ValueDecl *D,
                                    SourceRange R,
                                    const DeclContext *DC,
                                    const ApplyExpr *call,
                                    DeclAvailabilityFlags Flags = None);

/// Emit a diagnostic for references to declarations that have been
/// marked as unavailable, either through "unavailable" or "obsoleted:".
bool diagnoseExplicitUnavailability(
    const ValueDecl *D,
    SourceRange R,
    const DeclContext *DC,
    DeclAvailabilityFlags Flags,
    llvm::function_ref<void(InFlightDiagnostic &)> attachRenameFixIts);

/// Check if \p decl has a introduction version required by -require-explicit-availability
void checkExplicitAvailability(Decl *decl);

} // namespace swift

#endif // SWIFT_SEMA_TYPE_CHECK_AVAILABILITY_H

