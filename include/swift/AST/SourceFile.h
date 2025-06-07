//===--- SourceFile.h - The contents of a source file -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SOURCEFILE_H
#define SWIFT_AST_SOURCEFILE_H

#include "swift/AST/ASTDumper.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/IfConfigClauseRangeInfo.h"
#include "swift/AST/Import.h"
#include "swift/AST/SynthesizedFileUnit.h"
#include "swift/Basic/Debug.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace swift {
class ASTScope;
class AvailabilityScope;
class GeneratedSourceInfo;
class PersistentParserState;
struct SourceFileExtras;
class Token;
enum class DefaultIsolation : uint8_t;

/// Kind of import affecting how a decl can be reexported.
///
/// This is sorted in order of priority in case the same module is imported
/// differently. e.g. a normal import (None) offers more visibility than
/// an @_spiOnly import, which offers more visibility than an
/// @_implementationOnly import. The logic of \c getRestrictedImportKind relies
/// on the order of this enum.
///
/// This is a subset of \c DisallowedOriginKind.
///
/// \sa getRestrictedImportKind
enum class RestrictedImportKind {
  MissingImport,
  ImplementationOnly,
  SPIOnly,
  None // No restriction, i.e. the module is imported publicly.
};

/// Import that limits the access level of imported entities.
using ImportAccessLevel = std::optional<AttributedImport<ImportedModule>>;

/// A file containing Swift source code.
///
/// This is a .swift or .sil file (or a virtual file, such as the contents of
/// the REPL). Since it contains raw source, it must be type checked for IR
/// generation.
class SourceFile final : public FileUnit {
  friend class ParseSourceFileRequest;

public:
  /// Flags that direct how the source file is parsed.
  enum class ParsingFlags : uint8_t {
    /// Whether to disable delayed parsing for nominal type, extension, and
    /// function bodies.
    ///
    /// If set, type and function bodies will be parsed eagerly. Otherwise they
    /// will be lazily parsed when their contents is queried. This lets us avoid
    /// building AST nodes when they're not needed.
    ///
    /// This is set for primary files, since we want to type check all
    /// declarations and function bodies anyway, so there's no benefit in lazy
    /// parsing.
    DisableDelayedBodies = 1 << 0,

    /// Whether to disable evaluating the conditions of #if decls.
    ///
    /// If set, #if decls are parsed as-is. Otherwise, the bodies of any active
    /// clauses are hoisted such that they become sibling nodes with the #if
    /// decl.
    ///
    /// FIXME: When condition evaluation moves to a later phase, remove this
    /// and the associated language option.
    DisablePoundIfEvaluation = 1 << 1,

    /// Whether to save the file's parsed tokens.
    CollectParsedTokens = 1 << 2,

    /// Whether to compute the interface hash of the file.
    EnableInterfaceHash = 1 << 3,

    /// Whether to suppress warnings when parsing. This is set for secondary
    /// files, as they get parsed multiple times.
    SuppressWarnings = 1 << 4,

    /// Ensure that the SwiftSyntax tree round trips correctly.
    RoundTrip = 1 << 5,

    /// Validate the new SwiftSyntax parser diagnostics.
    ValidateNewParserDiagnostics = 1 << 6,

    /// Consider every #if ... #endif region active.
    PoundIfAllActive = 1 << 7,
  };
  using ParsingOptions = OptionSet<ParsingFlags>;

  /// Retrieve the parsing options specified in the LangOptions.
  static ParsingOptions getDefaultParsingOptions(const LangOptions &langOpts);

private:
  std::unique_ptr<SourceLookupCache> Cache;
  SourceLookupCache &getCache() const;

  /// This is the list of modules that are imported by this module.
  ///
  /// This is \c None until it is filled in by the import resolution phase.
  std::optional<ArrayRef<AttributedImport<ImportedModule>>> Imports;

  /// The underlying clang module, if imported in this file.
  ModuleDecl *ImportedUnderlyingModule = nullptr;

  /// Which imports have made use of @preconcurrency.
  llvm::SmallDenseSet<AttributedImport<ImportedModule>>
      PreconcurrencyImportsUsed;

  /// The highest access level of declarations referencing each import.
  llvm::DenseMap<const ModuleDecl *, AccessLevel> ImportsUseAccessLevel;

  /// Imports that should be printed in the module interface even though they
  /// were not written in the source file.
  llvm::SmallDenseSet<ImportedModule> ImplicitImportsForModuleInterface;

  /// Associates a list of source locations to the member declarations that must
  /// be diagnosed as being out of scope due to a missing import.
  using DelayedMissingImportForMemberDiags =
      llvm::SmallDenseMap<const ValueDecl *, std::vector<SourceLoc>>;
  DelayedMissingImportForMemberDiags MissingImportForMemberDiagnostics;

  /// A unique identifier representing this file; used to mark private decls
  /// within the file to keep them from conflicting with other files in the
  /// same module.
  mutable Identifier PrivateDiscriminator;

  /// The root AvailabilityScope for this SourceFile.
  ///
  /// This is set during type checking.
  AvailabilityScope *RootAvailabilityScope = nullptr;

  /// Either the class marked \@NS/UIApplicationMain or the synthesized FuncDecl
  /// that calls main on the type marked @main.
  ValueDecl *MainDecl = nullptr;

  /// The source location of the main type.
  SourceLoc MainDeclDiagLoc;

  /// A hash of all interface-contributing tokens that have been lexed for
  /// this source file.
  ///
  /// We only collect interface hash for primary input files.
  std::optional<Fingerprint> InterfaceHash;

  /// The ID for the memory buffer containing this file's source.
  unsigned BufferID;

  /// The parsing options for the file.
  ParsingOptions ParsingOpts;

  /// Whether this is a primary source file which we'll be generating code for.
  bool IsPrimary;

  /// The scope map that describes this source file.
  NullablePtr<ASTScope> Scope = nullptr;

   /// The set of parsed decls with opaque return types that have not yet
   /// been validated.
   llvm::SetVector<ValueDecl *> UnvalidatedDeclsWithOpaqueReturnTypes;
  
  /// The set of validated opaque return type decls in the source file.
  llvm::SmallVector<OpaqueTypeDecl *, 4> OpaqueReturnTypes;
  llvm::StringMap<OpaqueTypeDecl *> ValidatedOpaqueReturnTypes;
  /// The set of opaque type decls that have not yet been validated.
  ///
  /// \note This is populated as opaque type decls are created. Validation
  /// requires mangling the naming decl, which would lead to circularity
  /// if it were done from OpaqueResultTypeRequest.
  llvm::SetVector<OpaqueTypeDecl *> UnvalidatedOpaqueReturnTypes;

  /// The list of functions defined in this file whose bodies have yet to be
  /// typechecked. They must be held in this list instead of eagerly validated
  /// because their bodies may force us to perform semantic checks of arbitrary
  /// complexity, and we currently cannot handle those checks in isolation. E.g.
  /// we cannot, in general, perform witness matching on singular requirements
  /// unless the entire conformance has been evaluated.
  std::vector<AbstractFunctionDecl *> DelayedFunctions;

  /// The list of top-level items in the source file. This is \c None if
  /// they have not yet been parsed.
  /// FIXME: Once addTopLevelDecl/prependTopLevelDecl
  /// have been removed, this can become an optional ArrayRef.
  std::optional<std::vector<ASTNode>> Items;

  /// The list of hoisted declarations. See Decl::isHoisted().
  /// This is only used by lldb.
  std::vector<Decl *> Hoisted;

  using SeparatelyImportedOverlayMap =
    llvm::SmallDenseMap<ModuleDecl *, llvm::SmallPtrSet<ModuleDecl *, 1>>;

  /// Keys are modules which are shadowed by one or more separately-imported
  /// overlays; values are the list of overlays shadowing them.
  ///
  /// This is used by cross-import overlays to make their members appear to
  /// be part of the underlying module. (ClangImporter overlays use a different
  /// mechanism which is not SourceFile-dependent.)
  SeparatelyImportedOverlayMap separatelyImportedOverlays;

  /// A pointer to PersistentParserState with a function reference to its
  /// deleter to handle the fact that it's forward declared.
  using ParserStatePtr =
      std::unique_ptr<PersistentParserState, void (*)(PersistentParserState *)>;

  /// Stores delayed parser state that code completion needs to be able to
  /// resume parsing at the code completion token in the file.
  ParserStatePtr DelayedParserState =
      ParserStatePtr(/*ptr*/ nullptr, /*deleter*/ nullptr);

  struct IfConfigClauseRangesData {
    /// All the \c #if source ranges in this file.
    std::vector<IfConfigClauseRangeInfo> Ranges;

    /// Whether the elemnts in \c Ranges are sorted in source order within
    /// this file. We flip this to \c false any time a new range gets recorded,
    /// and lazily do the sorting when doing a query.
    bool IsSorted = false;
  };

  /// Stores all the \c #if source range info in this file.
  mutable IfConfigClauseRangesData IfConfigClauseRanges;

  friend class HasImportsMatchingFlagRequest;

  /// Indicates which import options have valid caches. Storage for
  /// \c HasImportsMatchingFlagRequest.
  ImportOptions validCachedImportOptions;

  /// The cached computation of which import flags are present in the file.
  /// Storage for \c HasImportsMatchingFlagRequest.
  ImportOptions cachedImportOptions;

  /// Extra information for the source file, allocated as needed.
  SourceFileExtras *extras = nullptr;

  friend ASTContext;

public:
  /// Appends the given declaration to the end of the top-level decls list. Do
  /// not add any additional uses of this function.
  void addTopLevelDecl(Decl *d);

  /// Prepends a declaration to the top-level decls list.
  ///
  /// FIXME: This entrypoint exists to support LLDB. Calls to this function are
  /// always a mistake, and additional uses should not be added.
  ///
  /// See rdar://58355191
  void prependTopLevelDecl(Decl *d);

  /// Add a hoisted declaration. See Decl::isHoisted().
  void addHoistedDecl(Decl *d);

  /// Retrieves an immutable view of the list of top-level items in this file.
  ArrayRef<ASTNode> getTopLevelItems() const;

  /// Retrieves an immutable view of the list of top-level decls in this file.
  ///
  /// NOTE: Please use getTopLevelItems() instead.
  ArrayRef<Decl *> getTopLevelDecls() const;

  /// Retrieves an immutable view of the list of hoisted decls in this file.
  /// See Decl::isHoisted().
  ArrayRef<Decl *> getHoistedDecls() const;

  /// Retrieves an immutable view of the top-level items if they have already
  /// been parsed, or \c None if they haven't. Should only be used for dumping.
  std::optional<ArrayRef<ASTNode>> getCachedTopLevelItems() const {
    if (!Items)
      return std::nullopt;
    return llvm::ArrayRef(*Items);
  }

  /// Retrieve the parsing options for the file.
  ParsingOptions getParsingOptions() const { return ParsingOpts; }

  /// Whether this source file is a primary file, meaning that we're generating
  /// code for it. Note this method returns \c false in WMO.
  bool isPrimary() const { return IsPrimary; }

  /// Retrieve the \c ExportedSourceFile instance produced by ASTGen, which
  /// includes the SourceFileSyntax node corresponding to this source file.
  void *getExportedSourceFile() const;

  /// Defer type checking of `AFD` to the end of `Sema`
  void addDelayedFunction(AbstractFunctionDecl *AFD);

  /// Typecheck the bodies of all lazily checked functions
  void typeCheckDelayedFunctions();

  /// A mapping from Objective-C selectors to the methods that have
  /// those selectors.
  llvm::DenseMap<ObjCSelector, llvm::TinyPtrVector<AbstractFunctionDecl *>>
    ObjCMethods;

  /// List of Objective-C methods, which is used for checking unintended
  /// Objective-C overrides.
  std::vector<AbstractFunctionDecl *> ObjCMethodList;

  /// An unsatisfied, optional @objc requirement in a protocol conformance.
  using ObjCUnsatisfiedOptReq = std::pair<DeclContext *, AbstractFunctionDecl *>;

  /// List of optional @objc protocol requirements that have gone
  /// unsatisfied, which might conflict with other Objective-C methods.
  std::vector<ObjCUnsatisfiedOptReq> ObjCUnsatisfiedOptReqs;

  /// A selector that is used by two different declarations in the same class.
  struct ObjCMethodConflict {
    NominalTypeDecl *typeDecl;
    ObjCSelector selector;
    bool isInstanceMethod;

    ObjCMethodConflict(NominalTypeDecl *typeDecl, ObjCSelector selector,
                       bool isInstanceMethod)
        : typeDecl(typeDecl), selector(selector),
          isInstanceMethod(isInstanceMethod)
    {}
  };

  /// List of Objective-C member conflicts we have found during type checking.
  llvm::SetVector<ObjCMethodConflict> ObjCMethodConflicts;

  /// Categories (extensions with explicit @objc names) declared in this
  /// source file. They need to be checked for conflicts after type checking.
  llvm::TinyPtrVector<ExtensionDecl *> ObjCCategories;

  /// List of attributes added by access notes, used to emit remarks for valid
  /// ones.
  llvm::DenseMap<ValueDecl *, std::vector<DeclAttribute *>>
      AttrsAddedByAccessNotes;

  /// Describes what kind of file this is, which can affect some type checking
  /// and other behavior.
  const SourceFileKind Kind;

  enum ASTStage_t {
    /// The source file has not had its imports resolved or been type checked.
    Unprocessed,
    /// Import resolution has completed.
    ImportsResolved,
    /// Type checking has completed.
    TypeChecked
  };

  /// Defines what phases of parsing and semantic analysis are complete for a
  /// source file.
  ///
  /// Only files that have been fully processed (i.e. type-checked) will be
  /// forwarded on to IRGen.
  ASTStage_t ASTStage = Unprocessed;

  /// Virtual file paths declared by \c #sourceLocation(file:) declarations in
  /// this source file.
  llvm::SmallVector<Located<StringRef>, 0> VirtualFilePaths;

  /// Returns information about the file paths used for diagnostics and magic
  /// identifiers in this source file, including virtual filenames introduced by
  /// \c #sourceLocation(file:) declarations.
  llvm::StringMap<SourceFilePathInfo> getInfoForUsedFilePaths() const;

  /// Retrieve "extra" information associated with this source file, which is
  /// lazily and separately constructed. Use this for scratch information
  /// that isn't needed for all source files.
  SourceFileExtras &getExtras() const;

  SourceFile(ModuleDecl &M, SourceFileKind K, unsigned bufferID,
             ParsingOptions parsingOpts = {}, bool isPrimary = false);

  ~SourceFile();

  bool hasImports() const {
    return Imports.has_value();
  }

  /// Retrieve an immutable view of the source file's imports.
  ArrayRef<AttributedImport<ImportedModule>> getImports() const {
    return *Imports;
  }

  /// Set the imports for this source file. This gets called by import
  /// resolution.
  void setImports(ArrayRef<AttributedImport<ImportedModule>> imports);

  /// Set the imported underlying clang module for this source file. This gets
  /// called by import resolution.
  void setImportedUnderlyingModule(ModuleDecl *module) {
    assert(!ImportedUnderlyingModule && "underlying module already set");
    ImportedUnderlyingModule = module;
  }

  /// Finds the import declaration that effectively imports a given module in
  /// this source file.
  std::optional<AttributedImport<ImportedModule>>
  findImport(const ModuleDecl *mod) const;

  /// Whether the given import has used @preconcurrency.
  bool hasImportUsedPreconcurrency(
      AttributedImport<ImportedModule> import) const;

  /// Note that the given import has used @preconcurrency/
  void setImportUsedPreconcurrency(
      AttributedImport<ImportedModule> import);

  /// Return the highest access level of the declarations referencing
  /// this import in signature or inlinable code.
  AccessLevel
  getMaxAccessLevelUsingImport(const ModuleDecl *import) const;

  /// Register the requirement that \p mod be imported with an access level
  /// that is at least as permissive as \p accessLevel in order to satisfy
  /// access or exportability checking constraints.
  void registerRequiredAccessLevelForModule(ModuleDecl *mod,
                                            AccessLevel accessLevel);

  enum ImportQueryKind {
    /// Return the results for testable or private imports.
    TestableAndPrivate,
    /// Return the results only for testable imports.
    TestableOnly,
    /// Return the results only for private imports.
    PrivateOnly
  };

  bool
  hasTestableOrPrivateImport(AccessLevel accessLevel, const ValueDecl *ofDecl,
                             ImportQueryKind kind = TestableAndPrivate) const;

  /// Does this source file have any imports with \c flag?
  /// If not, we can fast-path module checks.
  bool hasImportsWithFlag(ImportFlags flag) const;

  /// Enumerates each of the direct imports of \p module in the file.
  using AttributedImportCallback =
      llvm::function_ref<void(AttributedImport<ImportedModule> &)>;
  void forEachImportOfModule(const ModuleDecl *module,
                             AttributedImportCallback callback);

  /// Get the most permissive restriction applied to the imports of \p module.
  RestrictedImportKind getRestrictedImportKind(const ModuleDecl *module) const;

  /// Return the import of \p targetModule from this file with the most
  /// permissive access level.
  ImportAccessLevel getImportAccessLevel(const ModuleDecl *targetModule) const;

  /// Find all SPI names imported from \p importedModule by this file,
  /// collecting the identifiers in \p spiGroups.
  virtual void
  lookupImportedSPIGroups(
                const ModuleDecl *importedModule,
                llvm::SmallSetVector<Identifier, 4> &spiGroups) const override;

  // Is \p targetDecl accessible as an explicitly imported SPI from this file?
  bool isImportedAsSPI(const ValueDecl *targetDecl) const;

  bool shouldCrossImport() const;

  /// Register a separately-imported overlay as shadowing the module that
  /// declares it.
  ///
  /// \returns true if the overlay was added; false if it already existed.
  bool addSeparatelyImportedOverlay(ModuleDecl *overlay,
                                    ModuleDecl *declaring) {
    return std::get<1>(separatelyImportedOverlays[declaring].insert(overlay));
  }

  /// Retrieves a list of separately imported overlays which are shadowing
  /// \p declaring. If any \p overlays are returned, qualified lookups into
  /// \p declaring should be performed into \p overlays instead; since they
  /// are overlays, they will re-export \p declaring, but will also augment it
  /// with additional symbols.
  void getSeparatelyImportedOverlays(
      ModuleDecl *declaring, SmallVectorImpl<ModuleDecl *> &overlays) {
    auto i = separatelyImportedOverlays.find(declaring);
    if (i == separatelyImportedOverlays.end()) return;

    auto &value = std::get<1>(*i);
    overlays.append(value.begin(), value.end());
  }

  SWIFT_DEBUG_DUMPER(dumpSeparatelyImportedOverlays());

  /// Record an import that should be printed in the module interface even
  /// though it was not written in the source file. These imports are needed in
  /// Swift 5 mode to preserve the integrity of swiftinterface files when code
  /// publicly use declarations from modules that were \c `@_implementationOnly`
  /// imported in other source files.
  void addImplicitImportForModuleInterface(ImportedModule module) {
    ImplicitImportsForModuleInterface.insert(module);
  }

  /// Gather implicit imports that should printed in swiftinterfaces for
  /// compatibility with code in some Swift 5 modules.
  void getImplicitImportsForModuleInterface(
      SmallVectorImpl<ImportedModule> &imports) const override;

  /// Add a source location for which a delayed missing import for member
  /// diagnostic should be emited.
  void addDelayedMissingImportForMemberDiagnostic(const ValueDecl *decl,
                                                  SourceLoc loc) {
    MissingImportForMemberDiagnostics[decl].push_back(loc);
  }

  DelayedMissingImportForMemberDiags
  takeDelayedMissingImportForMemberDiagnostics() {
    DelayedMissingImportForMemberDiags diags;
    std::swap(diags, MissingImportForMemberDiagnostics);
    return diags;
  }

  /// Record the source range info for a parsed \c #if clause.
  void recordIfConfigClauseRangeInfo(const IfConfigClauseRangeInfo &range);

  /// Retrieve the source range info for any \c #if clauses in the file.
  ArrayRef<IfConfigClauseRangeInfo> getIfConfigClauseRanges() const;

  /// Retrieve the source range infos for any \c #if clauses contained within a
  /// given source range of this file.
  ArrayRef<IfConfigClauseRangeInfo>
  getIfConfigClausesWithin(SourceRange outer) const;

  /// Record visible declarations for use in code completion and refactoring.
  void cacheVisibleDecls(SmallVectorImpl<ValueDecl *> &&globals) const;

  /// Retrieve visible declarations for use in code completion and refactoring.
  const SmallVectorImpl<ValueDecl *> &getCachedVisibleDecls() const;

  virtual void lookupValue(DeclName name, NLKind lookupKind,
                           OptionSet<ModuleLookupFlags> Flags,
                           SmallVectorImpl<ValueDecl*> &result) const override;

  virtual void lookupVisibleDecls(ImportPath::Access accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const override;

  virtual void lookupClassMembers(ImportPath::Access accessPath,
                                  VisibleDeclConsumer &consumer) const override;
  virtual void
  lookupClassMember(ImportPath::Access accessPath, DeclName name,
                    SmallVectorImpl<ValueDecl*> &results) const override;

  void lookupObjCMethods(
         ObjCSelector selector,
         SmallVectorImpl<AbstractFunctionDecl *> &results) const override;

protected:
  virtual void
  lookupOperatorDirect(Identifier name, OperatorFixity fixity,
                       TinyPtrVector<OperatorDecl *> &results) const override;

  virtual void lookupPrecedenceGroupDirect(
      Identifier name,
      TinyPtrVector<PrecedenceGroupDecl *> &results) const override;

public:
  virtual void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getOperatorDecls(SmallVectorImpl<OperatorDecl *> &results) const override;

  virtual void
  getPrecedenceGroups(SmallVectorImpl<PrecedenceGroupDecl*> &results) const override;

  virtual TypeDecl *lookupLocalType(llvm::StringRef MangledName) const override;

  virtual void
  getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &results) const override;
  virtual void
  getOpaqueReturnTypeDecls(SmallVectorImpl<OpaqueTypeDecl*> &results) const override;

  virtual void
  getImportedModules(SmallVectorImpl<ImportedModule> &imports,
                     ModuleDecl::ImportFilter filter) const override;

  virtual void getExternalMacros(
      SmallVectorImpl<ExternalMacroPlugin> &macros) const override;

  virtual void
  collectLinkLibraries(ModuleDecl::LinkLibraryCallback callback) const override;

  Identifier getDiscriminatorForPrivateDecl(const Decl *D) const override;
  Identifier getPrivateDiscriminator(bool createIfMissing = false) const;
  std::optional<ExternalSourceLocs::RawLocs>
  getExternalRawLocsForDecl(const Decl *D) const override;

  virtual bool walk(ASTWalker &walker) override;

  /// The buffer ID for the file that was imported, or None if there
  /// is no associated buffer.
  unsigned getBufferID() const {
    return BufferID;
  }

  const GeneratedSourceInfo *getGeneratedSourceFileInfo() const;

  /// For source files created to hold the source code created by expanding
  /// a macro, this is the AST node that describes the macro expansion.
  ///
  /// The source location of this AST node is the place in the source that
  /// triggered the creation of the macro expansion whose resulting source
  /// code is in this source file. This will only produce a non-null value when
  /// the \c SourceFileKind is \c MacroExpansion.
  ASTNode getMacroExpansion() const;

  /// For source files created to hold the source code for a macro
  /// expansion, this is the original source range replaced by the macro
  /// expansion.
  SourceRange getMacroInsertionRange() const;

  /// For source files created to hold the source code created by expanding
  /// an attached macro, this is the custom attribute that describes the macro
  /// expansion.
  ///
  /// The source location of this attribute is the place in the source that
  /// triggered the creation of the macro expansion whose resulting source
  /// code is in this source file. This will only produce a non-null value when
  /// the \c SourceFileKind is \c MacroExpansion , and the macro is an attached
  /// macro.
  CustomAttr *getAttachedMacroAttribute() const;

  /// For source files created to hold the source code created by expanding
  /// an attached macro, this is the macro role that the expansion fulfills.
  ///
  /// \Returns the fulfilled macro role, or \c None if this source file is not
  /// for a macro expansion.
  std::optional<MacroRole> getFulfilledMacroRole() const;

  /// When this source file is enclosed within another source file, for example
  /// because it describes a macro expansion, return the source file it was
  /// enclosed in.
  SourceFile *getEnclosingSourceFile() const;

  /// If this file has an enclosing source file (because it is the result of
  /// expanding a macro or default argument), returns the node in the enclosing
  /// file that this file's contents were expanded from.
  ASTNode getNodeInEnclosingSourceFile() const;

  /// If this buffer corresponds to a file on disk, returns the path.
  /// Otherwise, return an empty string.
  StringRef getFilename() const;

  /// Retrieve the source text buffer.
  StringRef getBuffer() const;

  /// Retrieve the scope that describes this source file.
  ASTScope &getScope();

  void clearScope() {
    Scope = nullptr;
  }

  /// Retrieves the previously set delayed parser state, asserting that it
  /// exists.
  PersistentParserState *getDelayedParserState() {
    // Force parsing of the top-level decls, which will set DelayedParserState
    // if necessary.
    // FIXME: Ideally the parser state should be an output of
    // ParseSourceFileRequest, but the evaluator doesn't currently support
    // move-only outputs for cached requests.
    (void)getTopLevelItems();

    auto *state = DelayedParserState.get();
    assert(state && "Didn't set any delayed parser state!");
    return state;
  }

  /// Record delayed parser state for the source file. This is needed for code
  /// completion's second pass.
  void setDelayedParserState(ParserStatePtr &&state) {
    DelayedParserState = std::move(state);
  }

  /// Retrieve default action isolation to be used for this source file.
  /// It's determine based on on top-level `using <<isolation>>` declaration
  /// found in the file.
  std::optional<DefaultIsolation> getDefaultIsolation() const;

  SWIFT_DEBUG_DUMP;
  void
  dump(raw_ostream &os,
       ASTDumpMemberLoading memberLoading = ASTDumpMemberLoading::None) const;

  /// Dumps this source file's AST in JSON format to the given output stream.
  void dumpJSON(raw_ostream &os, ASTDumpMemberLoading memberLoading) const;

  /// Pretty-print the contents of this source file.
  ///
  /// \param Printer The AST printer used for printing the contents.
  /// \param PO Options controlling the printing process.
  void print(ASTPrinter &Printer, const PrintOptions &PO);
  void print(raw_ostream &OS, const PrintOptions &PO);

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::Source;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }

  /// True if this is a "script mode" source file that admits top-level code.
  bool isScriptMode() const {
    switch (Kind) {
    case SourceFileKind::Main:
      return true;

    case SourceFileKind::Library:
    case SourceFileKind::Interface:
    case SourceFileKind::SIL:
    case SourceFileKind::MacroExpansion:
    case SourceFileKind::DefaultArgument:
      return false;
    }
    llvm_unreachable("bad SourceFileKind");
  }

  ValueDecl *getMainDecl() const override { return MainDecl; }
  SourceLoc getMainDeclDiagLoc() const {
    assert(hasMainDecl());
    return MainDeclDiagLoc;
  }
  SourceLoc getMainClassDiagLoc() const {
    assert(hasMainClass());
    return getMainDeclDiagLoc();
  }

  /// Register a "main" class for the module, complaining if there is more than
  /// one.
  ///
  /// Should only be called during type-checking.
  bool registerMainDecl(ValueDecl *mainDecl, SourceLoc diagLoc);

  /// True if this source file has an application entry point.
  ///
  /// This is true if the source file either is in script mode or contains
  /// a designated main class.
  bool hasEntryPoint() const override {
    return isScriptMode() || hasMainDecl();
  }

  ModuleDecl *getUnderlyingModuleIfOverlay() const override {
    return ImportedUnderlyingModule;
  }

  const clang::Module *getUnderlyingClangModule() const override {
    if (!ImportedUnderlyingModule)
      return nullptr;

    return ImportedUnderlyingModule->findUnderlyingClangModule();
  }

  /// Get the root availability scope for the file. The root scope may be
  /// null if the scope tree has not been built yet. Use
  /// `AvailabilityScope::getOrBuildForSourceFile()` to get a built
  /// root of the tree.
  AvailabilityScope *getAvailabilityScope() const;

  /// Set the root availability scope for the file.
  void setAvailabilityScope(AvailabilityScope *scope);

  /// Whether this file can compute an interface hash.
  bool hasInterfaceHash() const {
    return ParsingOpts.contains(ParsingFlags::EnableInterfaceHash);
  }

  /// Retrieve a fingerprint value that summarizes the top-level declarations in
  /// this source file.
  ///
  /// Note that the interface hash merely summarizes the top-level declarations
  /// in this file. Type body fingerprints are currently implemented such that
  /// they divert tokens away from the hasher used for fingerprints. That is,
  /// changes to the bodies of types and extensions will not result in a change
  /// to the source file interface hash.
  Fingerprint getInterfaceHash() const;

  void dumpInterfaceHash(llvm::raw_ostream &out) {
    out << getInterfaceHash() << '\n';
  }

  /// Get this file's interface hash including the type members in the file.
  Fingerprint getInterfaceHashIncludingTypeMembers() const;

  /// If this source file has been told to collect its parsed tokens, retrieve
  /// those tokens.
  ArrayRef<Token> getAllTokens() const;

  /// Whether the parsed tokens of this source file should be saved, allowing
  /// them to be accessed from \c getAllTokens.
  bool shouldCollectTokens() const;

  /// Whether the bodies of types and functions within this file can be lazily
  /// parsed.
  bool hasDelayedBodyParsing() const;

  OpaqueTypeDecl *lookupOpaqueResultType(StringRef MangledName) override;

  /// Do not call when inside an inactive clause (\c
  /// InInactiveClauseEnvironment)) because it will later on result in a lookup
  /// to something that won't be in the ASTScope tree.
  void addUnvalidatedDeclWithOpaqueResultType(ValueDecl *vd) {
    UnvalidatedDeclsWithOpaqueReturnTypes.insert(vd);
  }

  void addOpaqueResultTypeDecl(OpaqueTypeDecl *decl) {
    UnvalidatedOpaqueReturnTypes.insert(decl);
  }

  ArrayRef<OpaqueTypeDecl *> getOpaqueReturnTypeDecls();

  /// Returns true if the source file contains concurrency in the top-level
  bool isAsyncTopLevelSourceFile() const;

  ArrayRef<TypeDecl *> getLocalTypeDecls() const;

  /// Uniquely identifies a source file without exposing its full file path.
  ///
  /// A valid file ID should always be of the format "modulename/filename.swift"
  struct FileIDStr {
    StringRef moduleName;
    StringRef fileName;

    /// Parse a string as a SourceFile::FileIDStr.
    ///
    /// Returns \c nullopt if \param fileID could not be parsed.
    static std::optional<FileIDStr> parse(StringRef fileID);

    /// Whether this SourceFile::FileID matches that of the given \param file.
    bool matches(const SourceFile *file) const;
  };

private:

  /// If not \c None, the underlying vector contains the parsed tokens of this
  /// source file.
  std::optional<ArrayRef<Token>> AllCollectedTokens;
};

inline SourceFile::ParsingOptions operator|(SourceFile::ParsingFlags lhs,
                                            SourceFile::ParsingFlags rhs) {
  return SourceFile::ParsingOptions(lhs) | rhs;
}

inline SourceFile &ModuleDecl::getMainSourceFile() const {
  assert(!getFiles().empty() && "No files in module");
  return *cast<SourceFile>(getFiles().front());
}

inline FileUnit *ModuleDecl::EntryPointInfoTy::getEntryPointFile() const {
  return storage.getPointer();
}
inline void ModuleDecl::EntryPointInfoTy::setEntryPointFile(FileUnit *file) {
  assert(!storage.getPointer());
  storage.setPointer(file);
}

inline bool ModuleDecl::EntryPointInfoTy::hasEntryPoint() const {
  return storage.getPointer();
}

inline bool ModuleDecl::EntryPointInfoTy::markDiagnosedMultipleMainClasses() {
  bool res = storage.getInt().contains(Flags::DiagnosedMultipleMainClasses);
  storage.setInt(storage.getInt() | Flags::DiagnosedMultipleMainClasses);
  return !res;
}

inline bool ModuleDecl::EntryPointInfoTy::markDiagnosedMainClassWithScript() {
  bool res = storage.getInt().contains(Flags::DiagnosedMainClassWithScript);
  storage.setInt(storage.getInt() | Flags::DiagnosedMainClassWithScript);
  return !res;
}

inline void simple_display(llvm::raw_ostream &out, const SourceFile *SF) {
  assert(SF && "Cannot display null source file!");

  out << "source_file " << '\"' << SF->getFilename() << '\"';
}
} // end namespace swift

namespace llvm {

template<>
struct DenseMapInfo<swift::SourceFile::ObjCMethodConflict> {
  using ObjCMethodConflict = swift::SourceFile::ObjCMethodConflict;

  static inline ObjCMethodConflict getEmptyKey() {
    return ObjCMethodConflict(nullptr, {}, false);
  }
  static inline ObjCMethodConflict getTombstoneKey() {
    return ObjCMethodConflict(nullptr, {}, true);
  }
  static inline unsigned getHashValue(ObjCMethodConflict a) {
    return hash_combine(hash_value(a.typeDecl),
                  DenseMapInfo<swift::ObjCSelector>::getHashValue(a.selector),
                  hash_value(a.isInstanceMethod));
  }
  static bool isEqual(ObjCMethodConflict a, ObjCMethodConflict b) {
    return a.typeDecl == b.typeDecl && a.selector == b.selector &&
           a.isInstanceMethod == b.isInstanceMethod;
  }
};

}


#endif
