//===--- ImportName.h - Imported Swift names for Clang decls ----*- C++ -*-===//
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
// This file provides class definitions for naming-related concerns in the
// ClangImporter.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_IMPORT_NAME_H
#define SWIFT_IMPORT_NAME_H

#include "ImportEnumInfo.h"
#include "SwiftLookupTable.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/Version.h"
#include "swift/Parse/Parser.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ForeignAsyncConvention.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "clang/Sema/Sema.h"

namespace swift {
namespace importer {
struct PlatformAvailability;

/// The kind of accessor that an entity will be imported as.
enum class ImportedAccessorKind : unsigned {
  None = 0,
  PropertyGetter,
  PropertySetter,
  SubscriptGetter,
  SubscriptSetter,
};
enum { NumImportedAccessorKindBits = 3 };

/// The name version
class ImportNameVersion : public RelationalOperationsBase<ImportNameVersion> {
  unsigned rawValue : 31;
  unsigned concurrency : 1;

  friend llvm::DenseMapInfo<ImportNameVersion>;

  enum AsConstExpr_t { AsConstExpr };

  constexpr ImportNameVersion() : rawValue(0), concurrency(false) {}
  constexpr ImportNameVersion(unsigned version, AsConstExpr_t)
      : rawValue(version), concurrency(false) {}
  explicit ImportNameVersion(unsigned version, bool concurrency = false)
      : rawValue(version), concurrency(concurrency) {
    assert(version >= 2 && "only Swift 2 and later are supported");
  }
public:
  /// Map a language version into an import name version.
  static ImportNameVersion fromOptions(const LangOptions &langOpts) {
    // We encode the 'rawValue' as just major version numbers with the
    // exception of '4.2', which is a special minor version that can impact
    // importing of names.  We treat that with a rawValue of 5, and treat
    // all major values of 5 or higher as being rawValue = majorversion + 1.
    const auto &version = langOpts.EffectiveLanguageVersion;
    // If the effective version is 4.x, where x >= 2, the import version
    // is 4.2.
    if (version.size() > 1 && version[0] == 4 && version[1] >= 2) {
      return ImportNameVersion::swift4_2();
    }

    unsigned major = version[0];
    return ImportNameVersion(major >= 5 ? major + 1 : major, false);
  }

  unsigned majorVersionNumber() const {
    assert(*this != ImportNameVersion::raw());
    if (*this == ImportNameVersion::swift4_2())
      return 4;
    return rawValue < 5 ? rawValue : rawValue - 1;
  }

  unsigned minorVersionNumber() const {
    assert(*this != ImportNameVersion::raw());
    if (*this == ImportNameVersion::swift4_2())
      return 2;
    return 0;
  }

  llvm::VersionTuple asClangVersionTuple() const {
    assert(*this != ImportNameVersion::raw());
    return llvm::VersionTuple(majorVersionNumber(), minorVersionNumber());
  }

  /// Whether to consider importing functions as 'async'.
  bool supportsConcurrency() const { return concurrency; }

  ImportNameVersion withConcurrency(bool concurrency) const {
    ImportNameVersion result = *this;
    result.concurrency = concurrency;
    return result;
  }

  bool operator==(ImportNameVersion other) const {
    return rawValue == other.rawValue && concurrency == other.concurrency;
  }
  bool operator<(ImportNameVersion other) const {
    return rawValue < other.rawValue ||
        (rawValue == other.rawValue && concurrency < other.concurrency);
  }

  /// Calls \p action for each name version other than this one, first going
  /// backwards until ImportNameVersion::raw(), and then going forwards to
  /// ImportNameVersion::maxVersion().
  ///
  /// This is the most useful order for importing compatibility stubs.
  void forEachOtherImportNameVersion(
      llvm::function_ref<void(ImportNameVersion)> action) const {
    assert(*this >= ImportNameVersion::swift2());

    ImportNameVersion nameVersion = *this;
    assert(!nameVersion.supportsConcurrency());

    // Consider concurrency imports.
    action(nameVersion.withConcurrency(true));

    while (nameVersion > ImportNameVersion::swift2()) {
      --nameVersion.rawValue;
      action(nameVersion);
    }

    action(ImportNameVersion::raw());

    nameVersion = *this;
    while (nameVersion < ImportNameVersion::maxVersion()) {
      ++nameVersion.rawValue;
      action(nameVersion);
    }
  }

  /// Names as they appear in C/ObjC.
  static constexpr inline ImportNameVersion raw() {
    return ImportNameVersion{};
  }

  /// Names as they appeared in Swift 2 family.
  static constexpr inline ImportNameVersion swift2() {
    return ImportNameVersion{2, AsConstExpr};
  }

  /// Names as they appeared in Swift 4.2 family.
  static constexpr inline ImportNameVersion swift4_2() {
    return ImportNameVersion{5, AsConstExpr};
  }

  /// The latest supported version.
  ///
  /// FIXME: All other version information is in Version.h. Can this go there
  /// instead?
  static constexpr inline ImportNameVersion maxVersion() {
    return ImportNameVersion{6, AsConstExpr};
  }

  /// The version which should be used for importing types, which need to have
  /// one canonical definition.
  ///
  /// FIXME: Is this supposed to be the /newest/ version, or a canonical
  /// version that lasts forever as part of the ABI?
  static constexpr inline ImportNameVersion forTypes() {
    return ImportNameVersion::maxVersion();
  }
};

/// Describing a name that was imported from Clang.
class ImportedName {
  friend class NameImporterBase;
  friend class NameImporter;
  /// The context into which this declaration will be imported.
  ///
  /// When the context into which the declaration will be imported
  /// matches a Clang declaration context (the common case), the
  /// result will be expressed as a declaration context. Otherwise,
  /// if the Clang type is not itself a declaration context (for
  /// example, a typedef that comes into Swift as a strong type),
  /// the type declaration will be provided.
  EffectiveClangContext effectiveContext;

  struct Info {
    /// For names that map Objective-C error handling conventions into
    /// throwing Swift methods, describes how the mapping is performed.
    ForeignErrorConvention::Info errorInfo;

    /// For names that map Objective-C completion handlers into async
    /// Swift methods, describes how the mapping is performed.
    ForeignAsyncConvention::Info asyncInfo;

    /// For a declaration name that makes the declaration into an
    /// instance member, the index of the "Self" parameter.
    unsigned selfIndex;

    /// For an initializer, the kind of initializer to import.
    CtorInitializerKind initKind;

    /// What kind of accessor this name refers to, if any.
    ImportedAccessorKind accessorKind : NumImportedAccessorKindBits;

    /// Whether this name was explicitly specified via a Clang
    /// swift_name attribute.
    unsigned hasCustomName : 1;

    /// Whether this was one of a special class of Objective-C
    /// initializers for which we drop the variadic argument rather
    /// than refuse to import the initializer.
    unsigned droppedVariadic : 1;

    /// Whether this is a global being imported as a member
    unsigned importAsMember : 1;

    unsigned hasSelfIndex : 1;

    unsigned hasErrorInfo : 1;

    unsigned hasAsyncInfo : 1;

    unsigned hasAsyncAlternateInfo : 1;

    Info()
        : errorInfo(), selfIndex(), initKind(CtorInitializerKind::Designated),
          accessorKind(ImportedAccessorKind::None), hasCustomName(false),
          droppedVariadic(false), importAsMember(false), hasSelfIndex(false),
          hasErrorInfo(false), hasAsyncInfo(false),
          hasAsyncAlternateInfo(false) {}
  } info;

  /// This either stores the raw identifier info in a
  /// ParsedDeclName, which uses StringRefs for all identifiers, or in a
  /// DeclName, which uses a swift::Identifier.
  struct Storage {
    ParsedDeclName parsedDeclName;
    DeclName declName;
    bool isInitializer = false;
    DeclName getDeclName(ASTContext &ast) {
      if (!declName)
        declName = formDeclName(ast, parsedDeclName.BaseName,
                                parsedDeclName.ArgumentLabels,
                                parsedDeclName.IsFunctionName, isInitializer,
                                parsedDeclName.IsSubscript);
      return declName;
    }
    std::string getName() const;
    explicit operator bool() const {
      return declName ? true : !parsedDeclName.BaseName.empty();
    }
    bool operator==(const Storage &other) const {
      if ((bool)declName ^ (bool)other.declName)
        return false;
      if (declName)
        return declName == other.declName;
      return getName() == other.getName();
    }
  } storage;

public:
  ImportedName() = default;
  DeclName getDeclName(ASTContext &ast) { return storage.getDeclName(ast); }
  DeclName getDeclNameOrNull() const { return storage.declName; }
  DeclName getDeclName() const {
    assert(storage.declName || !(*this));
    return storage.declName;
  }
  ParsedDeclName &getParsedDeclName() {
    assert(!storage.declName);
    return storage.parsedDeclName;
  }

  void setDeclName(StringRef name) { storage.parsedDeclName.BaseName = name; }
  void setDeclName(DeclName name) { storage.declName = name; }
  void setDeclName(ParsedDeclName name) {
    assert(!storage.declName);
    storage.parsedDeclName = name;
  }
  void setIsInitializer(bool value) { storage.isInitializer = value; }

  std::string getName() const { return storage.getName(); }

  explicit operator bool() const { return (bool)storage; }
  bool operator==(const ImportedName &other) const {
    return storage == other.storage;
  }

  /// The context into which this declaration will be imported.
  EffectiveClangContext getEffectiveContext() const { return effectiveContext; }
  void setEffectiveContext(EffectiveClangContext ctx) {
    effectiveContext = ctx;
  }

  /// For an initializer, the kind of initializer to import.
  CtorInitializerKind getInitKind() const { return info.initKind; }

  /// What kind of accessor this name refers to, if any.
  ImportedAccessorKind getAccessorKind() const { return info.accessorKind; }

  /// For names that map Objective-C error handling conventions into
  /// throwing Swift methods, describes how the mapping is performed.
  Optional<ForeignErrorConvention::Info> getErrorInfo() const {
    if (info.hasErrorInfo)
      return info.errorInfo;
    return None;
  }

  /// For names that map Objective-C methods with completion handlers into
  /// async Swift methods, describes how the mapping is performed.
  Optional<ForeignAsyncConvention::Info> getAsyncInfo() const {
    if (info.hasAsyncInfo) {
      assert(!info.hasAsyncAlternateInfo
             && "both regular and alternate async info?");
      return info.asyncInfo;
    }
    return None;
  }

  /// For names with a variant that maps Objective-C methods with completion
  /// handlers into async Swift methods, describes how the mapping is performed.
  ///
  /// That is, if the method imports as both an async method and a completion
  /// handler method, this value is set on the completion handler method's name
  /// and gives you the contents of \c getAsyncInfo() on the async method's
  /// name. It is not set on the async method's name, and it is not set if a
  /// non-async method doesn't have an async equivalent.
  Optional<ForeignAsyncConvention::Info> getAsyncAlternateInfo() const {
    if (info.hasAsyncAlternateInfo) {
      assert(!info.hasAsyncInfo && "both regular and alternate async info?");
      return info.asyncInfo;
    }
    return None;
  }

  /// For a declaration name that makes the declaration into an
  /// instance member, the index of the "Self" parameter.
  Optional<unsigned> getSelfIndex() const {
    if (info.hasSelfIndex)
      return info.selfIndex;
    return None;
  }

  /// Whether this name was explicitly specified via a Clang
  /// swift_name attribute.
  bool hasCustomName() const { return info.hasCustomName; }
  void setHasCustomName() { info.hasCustomName = true; }

  /// Whether this was one of a special class of Objective-C
  /// initializers for which we drop the variadic argument rather
  /// than refuse to import the initializer.
  bool droppedVariadic() const { return info.droppedVariadic; }

  /// Whether this is a global being imported as a member
  bool importAsMember() const { return info.importAsMember; }

  /// Whether this declaration is a property accessor (getter or setter).
  bool isPropertyAccessor() const {
    switch (getAccessorKind()) {
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
    switch (getAccessorKind()) {
    case ImportedAccessorKind::None:
    case ImportedAccessorKind::PropertyGetter:
    case ImportedAccessorKind::PropertySetter:
      return false;

    case ImportedAccessorKind::SubscriptGetter:
    case ImportedAccessorKind::SubscriptSetter:
      return true;
    }

    llvm_unreachable("Invalid ImportedAccessorKind.");
  }
};

/// Strips a trailing "Notification", if present. Returns {} if name doesn't end
/// in "Notification", or it there would be nothing left.
StringRef stripNotification(StringRef name);

/// Describes how a custom name was provided for 'async' import.
enum class CustomAsyncName {
  /// No custom name was provided.
  None,
  /// A custom swift_name (but not swift_async_name) was provided.
  SwiftName,
  /// A custom swift_async_name was provided, which won't have a completion
  /// handler argument label.
  SwiftAsyncName,
};

class NameImporterBase {
public:
  NameImporterBase(DiagnosticEngine &diags, const PlatformAvailability &avail,
               clang::Sema &cSema, bool enableObjCInterop)
      : diags(diags), availability(avail), clangSema(cSema),
        enumInfos(clangSema.getPreprocessor()),
        enableObjCInterop(enableObjCInterop) {}
  virtual ~NameImporterBase(){};

  /// Retrieve the inherited name set for the given Objective-C class.
  const InheritedNameSet *
  getAllPropertyNames(clang::ObjCInterfaceDecl *classDecl, bool forInstance);

  EnumInfo getEnumInfo(const clang::EnumDecl *decl) {
    return enumInfos.getEnumInfo(decl);
  }
  EnumKind getEnumKind(const clang::EnumDecl *decl) {
    return enumInfos.getEnumKind(decl);
  }

  clang::Sema &getClangSema() { return clangSema; }
  clang::ASTContext &getClangContext() {
    return getClangSema().getASTContext();
  }
  clang::Preprocessor &getClangPreprocessor() {
    return getClangSema().getPreprocessor();
  }
  StringScratchSpace &getScratch() { return scratch; }

protected:
  DiagnosticEngine &diags;
  const PlatformAvailability &availability;

  clang::Sema &clangSema;
  StringScratchSpace scratch;

  // TODO: remove when we drop the options (i.e. import all names)
  using CacheKeyType =
      std::pair<const clang::NamedDecl *, ImportNameVersion>;

  /// The set of property names that show up in the defining module of
  /// an Objective-C class.
  llvm::DenseMap<std::pair<const clang::ObjCInterfaceDecl *, char>,
                 std::unique_ptr<InheritedNameSet>> allProperties;

  EnumInfoCache enumInfos;
  bool enableObjCInterop;

  EffectiveClangContext determineEffectiveContext(const clang::NamedDecl *,
                                                  const clang::DeclContext *,
                                                  ImportNameVersion version);

  /// Look for a method that will import to have the same name as the
  /// given method after importing the Nth parameter as an elided error
  /// parameter.
  bool hasErrorMethodNameCollision(const clang::ObjCMethodDecl *method,
                                   unsigned paramIndex,
                                   StringRef suffixToStrip);

  /// Test to see if there is a value with the same name as 'proposedName' in
  /// the same module as the decl
  bool hasNamingConflict(const clang::NamedDecl *decl,
                         const clang::IdentifierInfo *proposedName,
                         const clang::TypedefNameDecl *cfTypedef);

  Optional<ForeignErrorConvention::Info>
  considerErrorImport(const clang::ObjCMethodDecl *clangDecl,
                      StringRef &baseName,
                      SmallVectorImpl<StringRef> &paramNames,
                      ArrayRef<const clang::ParmVarDecl *> params,
                      bool isInitializer, bool hasCustomName);

  Optional<ForeignAsyncConvention::Info>
  considerAsyncImport(const clang::ObjCMethodDecl *clangDecl,
                      StringRef baseName,
                      SmallVectorImpl<StringRef> &paramNames,
                      ArrayRef<const clang::ParmVarDecl *> params,
                      bool isInitializer,
                      Optional<unsigned> explicitCompletionHandlerParamIndex,
                      CustomAsyncName customName,
                      Optional<unsigned> completionHandlerFlagParamIndex,
                      bool completionHandlerFlagIsZeroOnError,
                      Optional<ForeignErrorConvention::Info> errorInfo);

  ImportedName importNameImpl(const clang::NamedDecl *,
                              ImportNameVersion version,
                              clang::DeclarationName);
  /// Necessary because ObjC methods need to recursively import overrides, and
  /// we want to benefit from the caching in NameImporter if it is available.
  virtual ImportedName importNameCached(
      const clang::NamedDecl *decl, ImportNameVersion version,
      clang::DeclarationName preferredName = clang::DeclarationName()) {
    return importNameImpl(decl, version, preferredName);
  }
  /// FIXME: This is redundant with finalize() and can be removed.
  virtual StringRef internString(StringRef s) = 0;
  /// importNameImpl populates the StringRefs in parsedDeclName with pointers to
  /// temporary strings. This method allocates memory for them.
  virtual ImportedName &finalize(ImportedName &result) {
    assert(!result.storage.declName);
    result.storage.parsedDeclName.BaseName =
        internString(result.storage.parsedDeclName.BaseName);
    for (auto &arg : result.storage.parsedDeclName.ArgumentLabels)
      arg = internString(arg);
    return result;
  };
};

/// Class to determine the Swift name of foreign entities. Currently fairly
/// stateless and borrows from the ClangImporter::Implementation, but in the
/// future will be more self-contained and encapsulated.
class NameImporter : public NameImporterBase {
  ASTContext &swiftCtx;
  /// Cache for repeated calls
  llvm::DenseMap<CacheKeyType, ImportedName> importNameCache;

public:
  NameImporter(ASTContext &ctx, const PlatformAvailability &avail,
               clang::Sema &cSema)
      : NameImporterBase(ctx.Diags, avail, cSema,
                         ctx.LangOpts.EnableObjCInterop),
        swiftCtx(ctx) {}

  ASTContext &getContext() { return swiftCtx; }
  const LangOptions &getLangOpts() const { return swiftCtx.LangOpts; }

  Identifier getIdentifier(StringRef name) {
    return swiftCtx.getIdentifier(name);
  }

  /// Determine the Swift name for a Clang decl
  ImportedName
  importName(const clang::NamedDecl *decl, ImportNameVersion version,
             clang::DeclarationName preferredName = clang::DeclarationName());

  /// Imports the name of the given Clang macro into Swift.
  Identifier importMacroName(const clang::IdentifierInfo *clangIdentifier,
                             const clang::MacroInfo *macro);

  /// Attempts to import the name of \p decl with each possible
  /// ImportNameVersion. \p action will be called with each unique name.
  ///
  /// In this case, "unique" means either the full name is distinct or the
  /// effective context is distinct. This method does not attempt to handle
  /// "unresolved" contexts in any special way---if one name references a
  /// particular Clang declaration and the other has an unresolved context
  /// that will eventually reference that declaration, the contexts will still
  /// be considered distinct.
  ///
  /// If \p action returns false, the current name will \e not be added to the
  /// set of seen names.
  ///
  /// The active name for \p activeVerion is always first, followed by the
  /// other names in the order of
  /// ImportNameVersion::forEachOtherImportNameVersion.
  ///
  /// Returns \c true if it fails to import name for the active version.
  bool forEachDistinctImportName(
      const clang::NamedDecl *decl, ImportNameVersion activeVersion,
      llvm::function_ref<bool(ImportedName, ImportNameVersion)> action);

  StringRef internString(StringRef s) override {
    return swiftCtx.getIdentifier(s).str();
  }

  /// This implementation of finalizer creates a DeclName from the
  /// ParsedDeclName.
  ImportedName &finalize(ImportedName &result) override {
    // Force conversion to a DeclName.
    result.getDeclName(swiftCtx);
    return result;
  };

private:
  ImportedName importNameCached(const clang::NamedDecl *decl,
                                ImportNameVersion version,
                                clang::DeclarationName preferredName =
                                    clang::DeclarationName()) override {
    return importName(decl, version, preferredName);
  }
};

}
}

namespace llvm {
/// Provides DenseMapInfo for ImportNameVersion.
template <> struct DenseMapInfo<swift::importer::ImportNameVersion> {
  using ImportNameVersion = swift::importer::ImportNameVersion;
  using DMIU = DenseMapInfo<unsigned>;
  static inline ImportNameVersion getEmptyKey() {
    return (ImportNameVersion)DMIU::getEmptyKey();
  }
  static inline ImportNameVersion getTombstoneKey() {
    return (ImportNameVersion)DMIU::getTombstoneKey();
  }
  static unsigned getHashValue(const ImportNameVersion &Val) {
    return DMIU::getHashValue(Val.rawValue);
  }
  static bool isEqual(const ImportNameVersion &LHS,
                      const ImportNameVersion &RHS) {
    return LHS == RHS;
  }
};

/// Provides DenseMapInfo for ImportedNameVersion.
template <>
struct DenseMapInfo<swift::importer::ImportedName> {
  using ImportedName = swift::importer::ImportedName;
  static inline ImportedName getEmptyKey() {
    ImportedName n;
    n.setDeclName(DenseMapInfo<swift::DeclName>::getEmptyKey());
    return n;
  }
  static inline ImportedName getTombstoneKey() {
    ImportedName n;
    n.setDeclName(DenseMapInfo<swift::DeclName>::getTombstoneKey());
    return n;
  }

  static unsigned getHashValue(const ImportedName &Val) {
    if (auto declName = Val.getDeclNameOrNull())
      return DenseMapInfo<swift::DeclName>::getHashValue(declName);
    auto name = Val.getName();
    return DenseMapInfo<StringRef>::getHashValue(StringRef(name));
  }

  static bool isEqual(const ImportedName &LHS, const ImportedName &RHS) {
    return LHS == RHS;
  }
};
} // namespace llvm

#endif
