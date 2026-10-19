//===--- SwiftNameTranslation.h - Swift Name Translation --------*- C++ -*-===//
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

#ifndef SWIFT_NAME_TRANSLATION_H
#define SWIFT_NAME_TRANSLATION_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Identifier.h"
#include <optional>

namespace swift {

class EnumDecl;
class EnumElementDecl;
struct InverseRequirement;
class GenericSignature;
class ValueDecl;

namespace objc_translation {
  enum CustomNamesOnly_t : bool {
    Normal = false,
    CustomNamesOnly = true,
  };

  StringRef getNameForObjC(const ValueDecl *VD,
                           CustomNamesOnly_t customNamesOnly = Normal);

  std::string getErrorDomainStringForObjC(const EnumDecl *ED);

  /// Print the ObjC name of an enum element decl to OS, also allowing the client
  /// to specify a preferred name other than the decl's original name.
  ///
  /// Returns true if the decl has a custom ObjC name (@objc); false otherwise.
  bool printSwiftEnumElemNameInObjC(const EnumElementDecl *EL,
                                    llvm::raw_ostream &OS,
                                    Identifier PreferredName = Identifier());

  /// Get the name for a value decl D if D is exported to ObjC, PreferredName is
  /// specified to perform what-if analysis, shadowing D's original name during
  /// computation.
  ///
  /// Returns a pair of Identifier and ObjCSelector, only one of which is valid.
  std::pair<Identifier, ObjCSelector>
  getObjCNameForSwiftDecl(const ValueDecl *VD, DeclName PreferredName = DeclName());

  /// Returns true if the given value decl D is visible to ObjC of its
  /// own accord (i.e. without considering its context)
  bool isVisibleToObjC(const ValueDecl *VD, AccessLevel minRequiredAccess,
                       bool checkParent = true);

} // end namespace objc_translation

namespace cxx_translation {

using objc_translation::CustomNamesOnly_t;

StringRef
getNameForCxx(const ValueDecl *VD,
              CustomNamesOnly_t customNamesOnly = objc_translation::Normal);

enum RepresentationKind { Representable, ObjCxxOnly, Unsupported };

enum RepresentationError {
  UnrepresentableObjC,
  UnrepresentableAsync,
  UnrepresentableIsolatedInActor,
  UnrepresentableRequiresClientEmission,
  UnrepresentableGeneric,
  UnrepresentableGenericRequirements,
  UnrepresentableThrows,
  UnrepresentableIndirectEnum,
  UnrepresentableEnumCaseType,
  UnrepresentableEnumCaseTuple,
  UnrepresentableProtocol,
  UnrepresentableMoveOnly,
  UnrepresentableMacro,
  UnrepresentableZeroSizedValueType,
};

/// Constructs a diagnostic that describes the given C++ representation error.
Diagnostic diagnoseRepresenationError(RepresentationError error, ValueDecl *vd);

struct DeclRepresentation {
  RepresentationKind kind;
  std::optional<RepresentationError> error;

  /// Returns true if the given Swift node is unsupported in Clang in any
  /// language mode.
  bool isUnsupported() const { return kind == Unsupported; }
};

/// Answers layout questions that only the C++ printer can, but that decide
/// whether a declaration is representable in C++.
class NominalTypeLayoutQueries {
public:
  virtual bool isZeroSized(const NominalTypeDecl *decl) = 0;
  /// Whether the size and alignment are not statically known, so that C++ has
  /// to box the value instead of storing it inline.
  virtual bool isOpaqueLayout(const NominalTypeDecl *decl) = 0;

protected:
  ~NominalTypeLayoutQueries() = default;
};

/// Returns the C++ representation info for the given declaration.
///
/// With a null \p layoutQueries the layout-dependent checks are skipped and the
/// declaration is reported as representable; the printer drops it later.
DeclRepresentation
getDeclRepresentation(const ValueDecl *VD,
                      NominalTypeLayoutQueries *layoutQueries);

/// Returns true if the given value decl is exposable to C++.
inline bool isExposableToCxx(const ValueDecl *VD,
                             NominalTypeLayoutQueries *layoutQueries) {
  return !getDeclRepresentation(VD, layoutQueries).isUnsupported();
}

/// Whether the given noncopyable Swift value type is exposed to C++ as a
/// move-only C++ class.
///
/// This covers the part of "has a fixed layout" that the AST can answer on its
/// own; see \c NominalTypeLayoutQueries::isOpaqueLayout for the rest.
bool isNoncopyableValueTypeExposableToCxx(const NominalTypeDecl *typeDecl);

/// \overload
bool isNoncopyableValueTypeExposableToCxx(Type type);

bool isObjCxxOnly(const ValueDecl *VD);
bool isObjCxxOnly(const clang::Decl *D, const ASTContext &ctx);

/// Returns true if the given value decl D is visible to C++ of its
/// own accord (i.e. without considering its context)
bool isVisibleToCxx(const ValueDecl *VD, AccessLevel minRequiredAccess,
                    bool checkParent = true);

/// Determine whether the given generic signature can be exposed to C++.
bool isExposableToCxx(GenericSignature genericSig);

} // end namespace cxx_translation

} // end namespace swift

#endif
