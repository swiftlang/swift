//===- DeclExportabilityVisitor.h - Swift Language Context ASTs -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the DeclExportabilityVisitor class. "Exportability" refers
// to whether a declaration may be referenced from outside of the module it
// is defined in.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECLEXPORTABILITYVISITOR_H
#define SWIFT_DECLEXPORTABILITYVISITOR_H

#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Decl.h"

namespace swift {

/// This visitor determines whether a declaration is "exportable", meaning whether
/// it can be referenced by other modules. For example, a function with a public
/// access level or with the `@usableFromInline` attribute is exportable.
class DeclExportabilityVisitor
    : public DeclVisitor<DeclExportabilityVisitor, bool> {
public:
  DeclExportabilityVisitor(){};

  bool visit(const Decl *D) {
    // Declarations nested in fragile functions are exported.
    if (D->getDeclContext()->getFragileFunctionKind().kind !=
        FragileFunctionKind::None)
      return true;

    if (auto value = dyn_cast<ValueDecl>(D)) {
      // A decl is exportable if it has a public access level.
      auto accessScope =
          value->getFormalAccessScope(/*useDC=*/nullptr,
                                      /*treatUsableFromInlineAsPublic=*/true);
      if (accessScope.isPublic() || accessScope.isPackage())
        return true;
    }

    return DeclVisitor<DeclExportabilityVisitor, bool>::visit(
        const_cast<Decl *>(D));
  }

  // Force all decl kinds to be handled explicitly.
  bool visitDecl(const Decl *D) = delete;
  bool visitValueDecl(const ValueDecl *valueDecl) = delete;

  bool visitExtensionDecl(const ExtensionDecl *ext) {
    // Extensions must extend exportable types to be exportable.
    auto nominalType = ext->getExtendedNominal();
    if (!nominalType || !visit(nominalType))
      return false;

    // If the extension has any exportable members, then it is exportable.
    auto members = ext->getMembers();
    auto hasSafeMembers =
        std::any_of(members.begin(), members.end(), [&](const Decl *D) -> bool {
          if (auto VD = dyn_cast<ValueDecl>(D))
            return visit(VD);
          return true;
        });
    if (hasSafeMembers)
      return true;

    // If the extension has any exportable conformances, then it is exportable.
    auto protocols = ext->getLocalProtocols(ConformanceLookupKind::All);
    bool hasSafeConformances =
        std::any_of(protocols.begin(), protocols.end(),
                    [this](ProtocolDecl *protocol) { return visit(protocol); });

    if (hasSafeConformances)
      return true;

    // Truly empty extensions are exportable. This can occur in swiftinterfaces,
    // for example.
    if (members.empty() && protocols.size() == 0)
      return true;

    return false;
  }

  bool visitPatternBindingDecl(const PatternBindingDecl *pbd) {
    // Pattern bindings are exportable if any of their var decls are exportable.
    for (auto i : range(pbd->getNumPatternEntries())) {
      if (auto *varDecl = pbd->getAnchoringVarDecl(i)) {
        if (visit(varDecl))
          return true;
      }
    }

    return false;
  }

  bool visitVarDecl(const VarDecl *var) {
    if (var->isLayoutExposedToClients())
      return true;

    // Consider all lazy var storage as exportable.
    // FIXME: We should keep track of what lazy var is associated to the
    //        storage for them to preserve the same accessibility.
    if (var->isLazyStorageProperty())
      return true;

    // Property wrapper storage is as exportable as the wrapped property.
    if (VarDecl *wrapped = var->getOriginalWrappedProperty())
      if (visit(wrapped))
        return true;

    return false;
  }

  bool visitAccessorDecl(const AccessorDecl *accessor) {
    // Accessors are as exportable as their storage.
    return visit(accessor->getStorage());
  }

  // ValueDecls with effectively public access are considered exportable and are
  // handled in visit(Decl *) above. Some specific kinds of ValueDecls with
  // additional, unique rules are handled individually above. ValueDecls that
  // are not effectively public and do not have unique rules are by default not
  // exportable.
#define DEFAULT_TO_ACCESS_LEVEL(KIND)                                          \
  bool visit##KIND##Decl(const KIND##Decl *D) {                                \
    static_assert(std::is_convertible<KIND##Decl *, ValueDecl *>::value,       \
                  #KIND "Decl must be a ValueDecl");                           \
    return false;                                                              \
  }
  DEFAULT_TO_ACCESS_LEVEL(NominalType);
  DEFAULT_TO_ACCESS_LEVEL(OpaqueType);
  DEFAULT_TO_ACCESS_LEVEL(TypeAlias);
  DEFAULT_TO_ACCESS_LEVEL(AssociatedType);
  DEFAULT_TO_ACCESS_LEVEL(AbstractStorage);
  DEFAULT_TO_ACCESS_LEVEL(AbstractFunction);
  DEFAULT_TO_ACCESS_LEVEL(Macro);
  DEFAULT_TO_ACCESS_LEVEL(EnumElement);

#undef DEFAULT_TO_ACCESS_LEVEL

  // There are several kinds of decls which we never expect to encounter in
  // exportability queries.
#define UNREACHABLE(KIND)                                                      \
  bool visit##KIND##Decl(const KIND##Decl *D) {                                \
    llvm_unreachable("unexpected " #KIND "Decl");                              \
    return true;                                                               \
  }
  UNREACHABLE(Module);
  UNREACHABLE(Missing);
  UNREACHABLE(MissingMember);
  UNREACHABLE(GenericTypeParam);
  UNREACHABLE(Param);
  UNREACHABLE(Using);

#undef UNREACHABLE

  // Uninteresting decls are always considered exportable. A kind of decl might
  // always be exportable if it is declared at the top level and access control
  // does not apply to it. Or, a kind of decl could be considered always
  // exportable because it is only found nested within other declarations that
  // have their own access level, in which case we assume that the declaration
  // context has already been checked.
#define UNINTERESTING(KIND)                                                    \
  bool visit##KIND##Decl(const KIND##Decl *D) { return true; }
  UNINTERESTING(TopLevelCode);
  UNINTERESTING(Import);
  UNINTERESTING(PrecedenceGroup);
  UNINTERESTING(EnumCase);
  UNINTERESTING(Operator);
  UNINTERESTING(MacroExpansion);

#undef UNINTERESTING
};

/// Check if a declaration is exported as part of a module's external interface.
/// This includes public and @usableFromInline decls.
/// FIXME: This is legacy that should be subsumed by `DeclExportabilityVisitor`
bool isExported(const Decl *D);

/// A specialization of `isExported` for `ValueDecl`.
bool isExported(const ValueDecl *VD);

/// A specialization of `isExported` for `ExtensionDecl`.
bool isExported(const ExtensionDecl *ED);

/// Returns true if the extension declares any protocol conformances that
/// require the extension to be exported.
bool hasConformancesToPublicProtocols(const ExtensionDecl *ED);

} // end namespace swift

#endif
