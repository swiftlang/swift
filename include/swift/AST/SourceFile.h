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

#include "swift/AST/FileUnit.h"
#include "swift/Basic/Debug.h"

namespace swift {

/// A file containing Swift source code.
///
/// This is a .swift or .sil file (or a virtual file, such as the contents of
/// the REPL). Since it contains raw source, it must be parsed and name-bound
/// before being used for anything; a full type-check is also necessary for
/// IR generation.
class SourceFile final : public FileUnit {
public:
  class Impl;
  struct SourceFileSyntaxInfo;

  /// The implicit module import that the SourceFile should get.
  enum class ImplicitModuleImportKind {
    None,
    Builtin,
    Stdlib
  };

  /// Possible attributes for imports in source files.
  enum class ImportFlags {
    /// The imported module is exposed to anyone who imports the parent module.
    Exported = 0x1,

    /// This source file has access to testable declarations in the imported
    /// module.
    Testable = 0x2,

    /// This source file has access to private declarations in the imported
    /// module.
    PrivateImport = 0x4,

    /// The imported module is an implementation detail of this file and should
    /// not be required to be present if the main module is ever imported
    /// elsewhere.
    ///
    /// Mutually exclusive with Exported.
    ImplementationOnly = 0x8
  };

  /// \see ImportFlags
  using ImportOptions = OptionSet<ImportFlags>;

  struct ImportedModuleDesc {
    ModuleDecl::ImportedModule module;
    ImportOptions importOptions;
    StringRef filename;

    ImportedModuleDesc(ModuleDecl::ImportedModule module, ImportOptions options,
                       StringRef filename = {})
        : module(module), importOptions(options), filename(filename) {
      assert(!(importOptions.contains(ImportFlags::Exported) &&
               importOptions.contains(ImportFlags::ImplementationOnly)));
    }
  };

private:
  std::unique_ptr<SourceLookupCache> Cache;
  SourceLookupCache &getCache() const;

  /// This is the list of modules that are imported by this module.
  ///
  /// This is filled in by the Name Binding phase.
  ArrayRef<ImportedModuleDesc> Imports;

  /// A unique identifier representing this file; used to mark private decls
  /// within the file to keep them from conflicting with other files in the
  /// same module.
  mutable Identifier PrivateDiscriminator;

  /// The root TypeRefinementContext for this SourceFile.
  ///
  /// This is set during type checking.
  TypeRefinementContext *TRC = nullptr;

  /// If non-null, used to track name lookups that happen within this file.
  Optional<ReferencedNameTracker> ReferencedNames;

  /// The class in this file marked \@NS/UIApplicationMain.
  ClassDecl *MainClass = nullptr;

  /// The source location of the main class.
  SourceLoc MainClassDiagLoc;

  /// A hash of all interface-contributing tokens that have been lexed for
  /// this source file so far.
  /// We only collect interface hash for primary input files.
  llvm::Optional<llvm::MD5> InterfaceHash;

  /// The ID for the memory buffer containing this file's source.
  ///
  /// May be -1, to indicate no association with a buffer.
  int BufferID;

  /// Does this source file have any implementation-only imports?
  /// If not, we can fast-path module checks.
  bool HasImplementationOnlyImports = false;

  /// The scope map that describes this source file.
  std::unique_ptr<ASTScope> Scope;

  /// The set of validated opaque return type decls in the source file.
  llvm::SmallVector<OpaqueTypeDecl *, 4> OpaqueReturnTypes;
  llvm::StringMap<OpaqueTypeDecl *> ValidatedOpaqueReturnTypes;
  /// The set of parsed decls with opaque return types that have not yet
  /// been validated.
  llvm::SetVector<ValueDecl *> UnvalidatedDeclsWithOpaqueReturnTypes;

  friend ASTContext;
  friend Impl;
public:
  /// The list of top-level declarations in the source file.
  std::vector<Decl*> Decls;

  /// A cache of syntax nodes that can be reused when creating the syntax tree
  /// for this file.
  SyntaxParsingCache *SyntaxParsingCache = nullptr;

  /// The list of local type declarations in the source file.
  llvm::SetVector<TypeDecl *> LocalTypeDecls;

  /// A set of special declaration attributes which require the
  /// Foundation module to be imported to work. If the foundation
  /// module is still not imported by the time type checking is
  /// complete, we diagnose.
  llvm::SetVector<const DeclAttribute *> AttrsRequiringFoundation;

  /// A set of synthesized declarations that need to be type checked.
  llvm::SmallVector<Decl *, 8> SynthesizedDecls;

  /// We might perform type checking on the same source file more than once,
  /// if its the main file or a REPL instance, so keep track of the last
  /// checked synthesized declaration to avoid duplicating work.
  unsigned LastCheckedSynthesizedDecl = 0;

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

  using ObjCMethodConflict = std::tuple<ClassDecl *, ObjCSelector, bool>;

  /// List of Objective-C member conflicts we have found during type checking.
  std::vector<ObjCMethodConflict> ObjCMethodConflicts;

  template <typename T>
  using OperatorMap = llvm::DenseMap<Identifier,llvm::PointerIntPair<T,1,bool>>;

  OperatorMap<InfixOperatorDecl*> InfixOperators;
  OperatorMap<PostfixOperatorDecl*> PostfixOperators;
  OperatorMap<PrefixOperatorDecl*> PrefixOperators;
  OperatorMap<PrecedenceGroupDecl*> PrecedenceGroups;

  /// Describes what kind of file this is, which can affect some type checking
  /// and other behavior.
  const SourceFileKind Kind;

  enum ASTStage_t {
    /// Parsing is underway.
    Parsing,
    /// Parsing has completed.
    Parsed,
    /// Name binding has completed.
    NameBound,
    /// Type checking has completed.
    TypeChecked
  };

  /// Defines what phases of parsing and semantic analysis are complete for a
  /// source file.
  ///
  /// Only files that have been fully processed (i.e. type-checked) will be
  /// forwarded on to IRGen.
  ASTStage_t ASTStage = Parsing;

  SourceFile(ModuleDecl &M, SourceFileKind K, Optional<unsigned> bufferID,
             ImplicitModuleImportKind ModImpKind, bool KeepParsedTokens = false,
             bool KeepSyntaxTree = false);

  ~SourceFile();

  void addImports(ArrayRef<ImportedModuleDesc> IM);

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

  bool hasImplementationOnlyImports() const {
    return HasImplementationOnlyImports;
  }

  bool isImportedImplementationOnly(const ModuleDecl *module) const;

  /// This is a hack for 'main' file parsing and the integrated REPL.
  ///
  /// FIXME: Refactor main file parsing to not pump the parser incrementally.
  /// FIXME: Remove the integrated REPL.
  void clearLookupCache();

  void cacheVisibleDecls(SmallVectorImpl<ValueDecl *> &&globals) const;
  const SmallVectorImpl<ValueDecl *> &getCachedVisibleDecls() const;

  virtual void lookupValue(DeclName name, NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &result) const override;

  virtual void lookupVisibleDecls(ModuleDecl::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const override;

  virtual void lookupClassMembers(ModuleDecl::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) const override;
  virtual void
  lookupClassMember(ModuleDecl::AccessPathTy accessPath, DeclName name,
                    SmallVectorImpl<ValueDecl*> &results) const override;

  void lookupObjCMethods(
         ObjCSelector selector,
         SmallVectorImpl<AbstractFunctionDecl *> &results) const override;

  virtual void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getPrecedenceGroups(SmallVectorImpl<PrecedenceGroupDecl*> &results) const override;

  virtual TypeDecl *lookupLocalType(llvm::StringRef MangledName) const override;

  virtual void
  getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &results) const override;
  virtual void
  getOpaqueReturnTypeDecls(SmallVectorImpl<OpaqueTypeDecl*> &results) const override;

  virtual void
  getImportedModules(SmallVectorImpl<ModuleDecl::ImportedModule> &imports,
                     ModuleDecl::ImportFilter filter) const override;

  virtual void
  collectLinkLibraries(ModuleDecl::LinkLibraryCallback callback) const override;

  Identifier getDiscriminatorForPrivateValue(const ValueDecl *D) const override;
  Identifier getPrivateDiscriminator() const { return PrivateDiscriminator; }
  Optional<BasicDeclLocs> getBasicLocsForDecl(const Decl *D) const override;

  virtual bool walk(ASTWalker &walker) override;

  /// @{

  /// Look up the given operator in this file.
  ///
  /// The file must be name-bound already. If the operator is not found, or if
  /// there is an ambiguity, returns null.
  ///
  /// \param isCascading If true, the lookup of this operator may affect
  /// downstream files.
  InfixOperatorDecl *lookupInfixOperator(Identifier name, bool isCascading,
                                         SourceLoc diagLoc = {});
  PrefixOperatorDecl *lookupPrefixOperator(Identifier name, bool isCascading,
                                           SourceLoc diagLoc = {});
  PostfixOperatorDecl *lookupPostfixOperator(Identifier name, bool isCascading,
                                             SourceLoc diagLoc = {});
  PrecedenceGroupDecl *lookupPrecedenceGroup(Identifier name, bool isCascading,
                                             SourceLoc diagLoc = {});
  /// @}

  ReferencedNameTracker *getReferencedNameTracker() {
    return ReferencedNames ? ReferencedNames.getPointer() : nullptr;
  }
  const ReferencedNameTracker *getReferencedNameTracker() const {
    return ReferencedNames ? ReferencedNames.getPointer() : nullptr;
  }

  void createReferencedNameTracker();

  /// The buffer ID for the file that was imported, or None if there
  /// is no associated buffer.
  Optional<unsigned> getBufferID() const {
    if (BufferID == -1)
      return None;
    return BufferID;
  }

  /// If this buffer corresponds to a file on disk, returns the path.
  /// Otherwise, return an empty string.
  StringRef getFilename() const;

  /// Retrieve the scope that describes this source file.
  ASTScope &getScope();

  SWIFT_DEBUG_DUMP;
  void dump(raw_ostream &os) const;

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
    case SourceFileKind::REPL:
      return true;

    case SourceFileKind::Library:
    case SourceFileKind::Interface:
    case SourceFileKind::SIL:
      return false;
    }
    llvm_unreachable("bad SourceFileKind");
  }

  ClassDecl *getMainClass() const override {
    return MainClass;
  }
  SourceLoc getMainClassDiagLoc() const {
    assert(hasMainClass());
    return MainClassDiagLoc;
  }

  /// Register a "main" class for the module, complaining if there is more than
  /// one.
  ///
  /// Should only be called during type-checking.
  bool registerMainClass(ClassDecl *mainClass, SourceLoc diagLoc);

  /// True if this source file has an application entry point.
  ///
  /// This is true if the source file either is in script mode or contains
  /// a designated main class.
  bool hasEntryPoint() const override {
    return isScriptMode() || hasMainClass();
  }

  /// Get the root refinement context for the file. The root context may be
  /// null if the context hierarchy has not been built yet. Use
  /// TypeChecker::getOrBuildTypeRefinementContext() to get a built
  /// root of the hierarchy.
  TypeRefinementContext *getTypeRefinementContext();

  /// Set the root refinement context for the file.
  void setTypeRefinementContext(TypeRefinementContext *TRC);

  void enableInterfaceHash() {
    assert(!hasInterfaceHash());
    InterfaceHash.emplace();
  }

  bool hasInterfaceHash() const {
    return InterfaceHash.hasValue();
  }

  void recordInterfaceToken(StringRef token) {
    assert(!token.empty());
    InterfaceHash->update(token);
    // Add null byte to separate tokens.
    uint8_t a[1] = {0};
    InterfaceHash->update(a);
  }

  void getInterfaceHash(llvm::SmallString<32> &str) {
    llvm::MD5::MD5Result result;
    InterfaceHash->final(result);
    llvm::MD5::stringifyResult(result, str);
  }

  void dumpInterfaceHash(llvm::raw_ostream &out) {
    llvm::SmallString<32> str;
    getInterfaceHash(str);
    out << str << '\n';
  }

  std::vector<Token> &getTokenVector();

  ArrayRef<Token> getAllTokens() const;

  bool shouldCollectToken() const;

  bool shouldBuildSyntaxTree() const;

  bool canBeParsedInFull() const;

  bool isSuitableForASTScopes() const { return canBeParsedInFull(); }

  syntax::SourceFileSyntax getSyntaxRoot() const;
  void setSyntaxRoot(syntax::SourceFileSyntax &&Root);
  bool hasSyntaxRoot() const;

  OpaqueTypeDecl *lookupOpaqueResultType(StringRef MangledName) override;

  /// Do not call when inside an inactive clause (\c
  /// InInactiveClauseEnvironment)) because it will later on result in a lookup
  /// to something that won't be in the ASTScope tree.
  void addUnvalidatedDeclWithOpaqueResultType(ValueDecl *vd) {
    UnvalidatedDeclsWithOpaqueReturnTypes.insert(vd);
  }

  ArrayRef<OpaqueTypeDecl *> getOpaqueReturnTypeDecls();

private:

  /// If not None, the underlying vector should contain tokens of this source file.
  Optional<std::vector<Token>> AllCorrectedTokens;

  std::unique_ptr<SourceFileSyntaxInfo> SyntaxInfo;
};

inline SourceFile &
ModuleDecl::getMainSourceFile(SourceFileKind expectedKind) const {
  assert(!Files.empty() && "No files added yet");
  assert(cast<SourceFile>(Files.front())->Kind == expectedKind);
  return *cast<SourceFile>(Files.front());
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

#endif
