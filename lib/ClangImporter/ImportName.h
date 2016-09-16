//===--- ImportName.h - Imported Swift names for Clang decls ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides class definitions for naming-related concerns in the
// ClangImporter.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_IMPORT_NAME_H
#define SWIFT_IMPORT_NAME_H

#include "SwiftLookupTable.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ForeignErrorConvention.h"

namespace swift {
namespace importer {
class EnumInfoCache;

/// Information about imported error parameters.
struct ImportedErrorInfo {
  ForeignErrorConvention::Kind Kind;
  ForeignErrorConvention::IsOwned_t IsOwned;

  /// The index of the error parameter.
  unsigned ParamIndex;

  /// Whether the parameter is being replaced with "void"
  /// (vs. removed).
  bool ReplaceParamWithVoid;
};

/// The kind of accessor that an entity will be imported as.
enum class ImportedAccessorKind {
  None = 0,
  PropertyGetter,
  PropertySetter,
  SubscriptGetter,
  SubscriptSetter,
};

/// Describes a name that was imported from Clang.
struct ImportedName {
  /// The imported name.
  DeclName Imported;

  /// Whether this name was explicitly specified via a Clang
  /// swift_name attribute.
  bool HasCustomName = false;

  /// Whether this was one of a special class of Objective-C
  /// initializers for which we drop the variadic argument rather
  /// than refuse to import the initializer.
  bool DroppedVariadic = false;

  /// Whether this is a global being imported as a member
  bool ImportAsMember = false;

  /// What kind of accessor this name refers to, if any.
  ImportedAccessorKind AccessorKind = ImportedAccessorKind::None;

  /// For an initializer, the kind of initializer to import.
  CtorInitializerKind InitKind = CtorInitializerKind::Designated;

  /// The context into which this declaration will be imported.
  ///
  /// When the context into which the declaration will be imported
  /// matches a Clang declaration context (the common case), the
  /// result will be expressed as a declaration context. Otherwise,
  /// if the Clang type is not itself a declaration context (for
  /// example, a typedef that comes into Swift as a strong type),
  /// the type declaration will be provided.
  EffectiveClangContext EffectiveContext;

  /// For names that map Objective-C error handling conventions into
  /// throwing Swift methods, describes how the mapping is performed.
  Optional<ImportedErrorInfo> ErrorInfo;

  /// For a declaration name that makes the declaration into an
  /// instance member, the index of the "Self" parameter.
  Optional<unsigned> SelfIndex = None;

  /// Produce just the imported name, for clients that don't care
  /// about the details.
  operator DeclName() const { return Imported; }

  /// Whether any name was imported.
  explicit operator bool() const { return static_cast<bool>(Imported); }

  /// Whether this declaration is a property accessor (getter or setter).
  bool isPropertyAccessor() const {
    switch (AccessorKind) {
    case ImportedAccessorKind::None:
    case ImportedAccessorKind::SubscriptGetter:
    case ImportedAccessorKind::SubscriptSetter:
      return false;

    case ImportedAccessorKind::PropertyGetter:
    case ImportedAccessorKind::PropertySetter:
      return true;
    }
  }

  /// Whether this declaration is a subscript accessor (getter or setter).
  bool isSubscriptAccessor() const {
    switch (AccessorKind) {
    case ImportedAccessorKind::None:
    case ImportedAccessorKind::PropertyGetter:
    case ImportedAccessorKind::PropertySetter:
      return false;

    case ImportedAccessorKind::SubscriptGetter:
    case ImportedAccessorKind::SubscriptSetter:
      return true;
    }
  }
};

/// Strips a trailing "Notification", if present. Returns {} if name doesn't end
/// in "Notification", or it there would be nothing left.
StringRef stripNotification(StringRef name);

/// Flags that control the import of names in importFullName.
enum class ImportNameFlags {
  /// Suppress the factory-method-as-initializer transformation.
  SuppressFactoryMethodAsInit = 0x01,
  /// Produce the Swift 2 name of the given entity.
  Swift2Name = 0x02,
};

/// Options that control the import of names in importFullName.
typedef OptionSet<ImportNameFlags> ImportNameOptions;

/// The below is a work in progress to make import naming less stateful and tied
/// to the Impl. In it's current form, it is rather unwieldy.
// TODO: refactor into convenience class, multi-versioned, etc.
ImportedName importFullName(const clang::NamedDecl *, ASTContext &SwiftContext,
                            clang::Sema &clangSema,
                            EnumInfoCache &enumInfoCache,
                            PlatformAvailability &platformAvailability,
                            ImportNameOptions options,
                            bool inferImportAsMember);
}
}

#endif
