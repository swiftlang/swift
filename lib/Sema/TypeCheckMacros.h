//===--- TypeCheckMacros.h - Macros -----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides type checking support for macros.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_TYPECHECKMACROS_H
#define SWIFT_SEMA_TYPECHECKMACROS_H

#include "swift/AST/Attr.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/Type.h"

/// Prefix used for the names of macro expansion buffers, to replace the
/// leading "$s" used for Swift manglings.
#define MACRO_EXPANSION_BUFFER_MANGLING_PREFIX "@__swiftmacro_"

namespace swift {

class CustomAttr;
class Expr;
class MacroDecl;
class MacroExpansionExpr;
class MacroExpansionDecl;
class TypeRepr;

/// Expands the given macro expression and type-check the result with
/// the given expanded type.
///
/// \returns Expansion buffer ID if expansion succeeded, \p None if failed.
std::optional<unsigned> expandMacroExpr(MacroExpansionExpr *mee);

/// Expands the given macro expansion declaration.
///
/// \returns Expansion buffer ID if expansion succeeded, \p None if failed.
std::optional<unsigned> expandFreestandingMacro(MacroExpansionDecl *med);

/// Expand the accessors for the given storage declaration based on the
/// custom attribute that references the given macro.
std::optional<unsigned> expandAccessors(AbstractStorageDecl *storage,
                                        CustomAttr *attr, MacroDecl *macro);

/// Expand the attributes for the given member declaration based
/// on the custom attribute that references the given macro.
///
/// If expansion occurred, returns the macro expansion buffer ID.
std::optional<unsigned> expandAttributes(CustomAttr *attr, MacroDecl *macro,
                                         Decl *member);

/// Expand the synthesized members for the given declaration based on
/// the custom attribute that references the given macro.
///
/// If expansion occurred, returns the macro expansion buffer ID.
std::optional<unsigned> expandMembers(CustomAttr *attr, MacroDecl *macro,
                                      Decl *decl);

/// Expand the peer declarations for the given declaration based on
/// the custom attribute that references the given macro.
///
/// If expansion occurred, returns the macro expansion buffer ID.
std::optional<unsigned> expandPeers(CustomAttr *attr, MacroDecl *macro,
                                    Decl *decl);

/// Expand the extensions for the given declaration based on
/// the custom attribute that references the given macro. Extensions
/// can be produced by either conformance macros or extension macros.
///
/// If expansion occurred, returns the macro expansion buffer ID.
std::optional<unsigned> expandExtensions(CustomAttr *attr, MacroDecl *macro,
                                         MacroRole role,
                                         NominalTypeDecl *nominal);

/// Determine whether an accessor macro with the given attribute only
/// introduces observers like willSet and didSet.
bool accessorMacroOnlyIntroducesObservers(
    MacroDecl *macro, const MacroRoleAttr *attr);

/// Determine whether an accessor macro (defined with the given role attribute)
/// introduces an init accessor.
bool accessorMacroIntroducesInitAccessor(
    MacroDecl *macro, const MacroRoleAttr *attr);

/// Return true if the given macro role does not apply
/// to the declaration kind of \c attachedTo.
bool isInvalidAttachedMacro(MacroRole role,
                            Decl *attachedTo);

} // end namespace swift

#endif /* SWIFT_SEMA_TYPECHECKMACROS_H */

