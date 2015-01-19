//===--- Module.h - Swift Language Module ASTs ------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Module class and its subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MODULE_H
#define SWIFT_MODULE_H

#include "swift/AST/DeclContext.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/RawComment.h"
#include "swift/AST/Type.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/ErrorHandling.h"

namespace clang {
  class Module;
}

namespace swift {
  enum class ArtificialMainKind : uint8_t;
  class ASTContext;
  class ASTWalker;
  class BraceStmt;
  class Decl;
  enum class DeclKind : uint8_t;
  class ExtensionDecl;
  class DebuggerClient;
  class DeclName;
  class DerivedFileUnit;
  class FileUnit;
  class FuncDecl;
  class InfixOperatorDecl;
  class LinkLibrary;
  class LookupCache;
  class ModuleLoader;
  class NameAliasType;
  class NominalTypeDecl;
  class EnumElementDecl;
  class OperatorDecl;
  class PostfixOperatorDecl;
  class PrefixOperatorDecl;
  class ProtocolConformance;
  class ProtocolDecl;
  struct PrintOptions;
  class ReferencedNameTracker;
  class TupleType;
  class Type;
  class TypeRefinementContext;
  class ValueDecl;
  class VarDecl;
  class VisibleDeclConsumer;
  
  /// NLKind - This is a specifier for the kind of name lookup being performed
  /// by various query methods.
  enum class NLKind {
    UnqualifiedLookup,
    QualifiedLookup
  };

/// Constants used to customize name lookup.
enum NameLookupOptions {
  /// Visit supertypes (such as superclasses or inherited protocols)
  /// and their extensions as well as the current extension.
  NL_VisitSupertypes = 0x01,

  /// Consider declarations within protocols to which the context type conforms.
  NL_ProtocolMembers = 0x02,

  /// Remove non-visible declarations from the set of results.
  NL_RemoveNonVisible = 0x04,

  /// Remove overridden declarations from the set of results.
  NL_RemoveOverridden = 0x08,

  /// For existentials involving the special \c AnyObject protocol,
  /// allow lookups to find members of all classes.
  NL_DynamicLookup = 0x10,

  /// Don't check accessibility when doing lookup into a type.
  ///
  /// This option is not valid when performing lookup into a module.
  NL_IgnoreAccessibility = 0x20,

  /// This lookup is known to be a non-cascading dependency, i.e. one that does
  /// not affect downstream files.
  ///
  /// \see NL_KnownDependencyMask
  NL_KnownNonCascadingDependency = 0x40,

  /// This lookup is known to be a cascading dependency, i.e. one that can
  /// affect downstream files.
  ///
  /// \see NL_KnownDependencyMask
  NL_KnownCascadingDependency = 0x80,

  /// This lookup is known to not add any additional dependencies to the
  /// primary source file.
  ///
  /// \see NL_KnownDependencyMask
  NL_KnownNoDependency =
      NL_KnownNonCascadingDependency|NL_KnownCascadingDependency,

  /// A mask of all options controlling how a lookup should be recorded as a
  /// dependency.
  ///
  /// This offers three possible options: NL_KnownNonCascadingDependency,
  /// NL_KnownCascadingDependency, NL_KnownNoDependency, as well as a default
  /// "unspecified" value (0). If the dependency kind is unspecified, the
  /// lookup function will attempt to infer whether it is a cascading or
  /// non-cascading dependency from the decl context.
  NL_KnownDependencyMask = NL_KnownNoDependency,

  /// The default set of options used for qualified name lookup.
  ///
  /// FIXME: Eventually, add NL_ProtocolMembers to this, once all of the
  /// callers can handle it.
  NL_QualifiedDefault = NL_VisitSupertypes | NL_RemoveNonVisible |
                        NL_RemoveOverridden,

  /// The default set of options used for unqualified name lookup.
  NL_UnqualifiedDefault = NL_VisitSupertypes |
                          NL_RemoveNonVisible | NL_RemoveOverridden
};

/// Describes the result of looking for the conformance of a given type
/// to a specific protocol.
enum class ConformanceKind {
  /// The type does not conform to the protocol.
  DoesNotConform,
  /// The type conforms to the protocol, with the given conformance.
  Conforms,
  /// The type is specified to conform to the protocol, but that conformance
  /// has not yet been checked.
  UncheckedConforms
};

/// The result of looking for a specific conformance.
typedef llvm::PointerIntPair<ProtocolConformance *, 2, ConformanceKind>
  LookupConformanceResult;

/// Discriminator for file-units.
enum class FileUnitKind {
  /// For a .swift source file.
  Source,
  /// For the compiler Builtin module.
  Builtin,
  /// A serialized Swift AST.
  SerializedAST,
  /// An imported Clang module.
  ClangModule,
  /// A derived declaration.
  Derived,
};

enum class SourceFileKind {
  Library,  ///< A normal .swift file.
  Main,     ///< A .swift file that can have top-level code.
  REPL,     ///< A virtual file that holds the user's input in the REPL.
  SIL       ///< Came from a .sil file.
};

/// The minimum unit of compilation.
///
/// A module is made up of several file-units, which are all part of the same
/// output binary and logical module (such as a single library or executable).
///
/// \sa FileUnit
class Module : public DeclContext {
public:
  typedef ArrayRef<std::pair<Identifier, SourceLoc>> AccessPathTy;
  typedef std::pair<Module::AccessPathTy, Module*> ImportedModule;
  
  static bool matchesAccessPath(AccessPathTy AccessPath, DeclName Name) {
    assert(AccessPath.size() <= 1 && "can only refer to top-level decls");
  
    return AccessPath.empty()
      || DeclName(AccessPath.front().first).matchesRef(Name);
  }
  
  /// Arbitrarily orders ImportedModule records, for inclusion in sets and such.
  class OrderImportedModules {
  public:
    bool operator()(const ImportedModule &lhs,
                    const ImportedModule &rhs) const {
      if (lhs.second != rhs.second)
        return std::less<const Module *>()(lhs.second, rhs.second);
      if (lhs.first.data() != rhs.first.data())
        return std::less<AccessPathTy::iterator>()(lhs.first.begin(),
                                                   rhs.first.begin());
      return lhs.first.size() < rhs.first.size();
    }
  };

public:
  ASTContext &Ctx;
  Identifier Name;

private:
  /// If non-NULL, an plug-in that should be used when performing external
  /// lookups.
  // FIXME: Do we really need to bloat all modules with this?
  DebuggerClient *DebugClient = nullptr;

  SmallVector<FileUnit *, 2> Files;

  /// Tracks the file that will generate the module's entry point, either
  /// because it contains a class marked with \@UIApplicationMain
  /// or \@NSApplicationMain, or because it is a script file.
  class MainInfoTy {
    enum class Flags {
      DiagnosedMultipleMainClasses = 1 << 0,
      DiagnosedMainClassWithScript = 1 << 1
    };
    llvm::PointerIntPair<FileUnit *, 2, OptionSet<Flags>> storage;
  public:
    MainInfoTy() = default;

    FileUnit *getMainFile() const {
      return storage.getPointer();
    }
    void setMainFile(FileUnit *file) {
      assert(!storage.getPointer());
      storage.setPointer(file);
    }

    bool hasMain() const {
      return storage.getPointer();
    }

    bool markDiagnosedMultipleMainClasses() {
      bool res = storage.getInt().contains(Flags::DiagnosedMultipleMainClasses);
      storage.setInt(storage.getInt() | Flags::DiagnosedMultipleMainClasses);
      return !res;
    }

    bool markDiagnosedMainClassWithScript() {
      bool res = storage.getInt().contains(Flags::DiagnosedMainClassWithScript);
      storage.setInt(storage.getInt() | Flags::DiagnosedMainClassWithScript);
      return !res;
    }
  };

  /// Information about the file responsible for the module's entry point,
  /// if any.
  ///
  /// \see MainInfoTy
  MainInfoTy MainInfo;

  /// The magic __dso_handle variable.
  VarDecl *DSOHandle = nullptr;

  /// Did we complain about multiple main classes yet?
  bool DiagnosedMultipleMainClasses: 1;
  
  /// Did we complain about a main class and a script together yet?
  bool DiagnosedMainClassWithScript: 1;

  Module(Identifier name, ASTContext &ctx);
public:
  Identifier getName() const { return Name; }

  static Module *create(Identifier name, ASTContext &ctx) {
    return new (ctx) Module(name, ctx);
  }

  ArrayRef<FileUnit *> getFiles() {
    return Files;
  }
  ArrayRef<const FileUnit *> getFiles() const {
    return { Files.begin(), Files.size() };
  }

  void addFile(FileUnit &newFile);
  void removeFile(FileUnit &existingFile);

  /// Convenience accessor for clients that know what kind of file they're
  /// dealing with.
  SourceFile &getMainSourceFile(SourceFileKind expectedKind) const;

  /// Convenience accessor for clients that know what kind of file they're
  /// dealing with.
  FileUnit &getMainFile(FileUnitKind expectedKind) const;

  DerivedFileUnit &getDerivedFileUnit() const;

  DebuggerClient *getDebugClient() const { return DebugClient; }
  void setDebugClient(DebuggerClient *R) {
    assert(!DebugClient && "Debugger client already set");
    DebugClient = R;
  }

  /// Retrieve the magic __dso_handle variable.
  VarDecl *getDSOHandle();

  /// Look up a (possibly overloaded) value set at top-level scope
  /// (but with the specified access path, which may come from an import decl)
  /// within the current module.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  void lookupValue(AccessPathTy AccessPath, DeclName Name, NLKind LookupKind,
                   SmallVectorImpl<ValueDecl*> &Result) const;

  /// Find ValueDecls in the module and pass them to the given consumer object.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  void lookupVisibleDecls(AccessPathTy AccessPath,
                          VisibleDeclConsumer &Consumer,
                          NLKind LookupKind) const;

  /// @{

  /// Look up the given operator in this module.
  ///
  /// If the operator is not found, or if there is an ambiguity, returns null.
  InfixOperatorDecl *lookupInfixOperator(Identifier name,
                                         SourceLoc diagLoc = {});
  PrefixOperatorDecl *lookupPrefixOperator(Identifier name,
                                           SourceLoc diagLoc = {});
  PostfixOperatorDecl *lookupPostfixOperator(Identifier name,
                                             SourceLoc diagLoc = {});
  /// @}

  /// Finds all class members defined in this module.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  void lookupClassMembers(AccessPathTy accessPath,
                          VisibleDeclConsumer &consumer) const;

  /// Finds class members defined in this module with the given name.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  void lookupClassMember(AccessPathTy accessPath,
                         DeclName name,
                         SmallVectorImpl<ValueDecl*> &results) const;

  /// Look for the conformance of the given type to the given protocol.
  ///
  /// This routine determines whether the given \c type conforms to the given
  /// \c protocol. It only looks for explicit conformances (which are
  /// required by the language), and will return a \c ProtocolConformance*
  /// describing the conformance.
  ///
  /// During type-checking, it is possible that this routine will find an
  /// explicit declaration of conformance that has not yet been type-checked,
  /// in which case it will return note the presence of an unchecked
  /// conformance.
  ///
  /// \param type The type for which we are computing conformance.
  ///
  /// \param protocol The protocol to which we are computing conformance.
  ///
  /// \param resolver The lazy resolver.
  ///
  /// \returns The result of the conformance search, with a conformance
  /// structure when possible.
  LookupConformanceResult
  lookupConformance(Type type, ProtocolDecl *protocol, LazyResolver *resolver);

  /// Find a member named \p name in \p container that was declared in this
  /// module.
  ///
  /// \p container may be \c this for a top-level lookup.
  ///
  /// If \p privateDiscriminator is non-empty, only matching private decls are
  /// returned; otherwise, only non-private decls are returned.
  void lookupMember(SmallVectorImpl<ValueDecl*> &results,
                    DeclContext *container, DeclName name,
                    Identifier privateDiscriminator) const;

  /// \sa getImportedModules
  enum class ImportFilter {
    All,
    Public,
    Private
  };

  /// Looks up which modules are imported by this module.
  ///
  /// \p filter controls whether public, private, or any imports are included
  /// in this list.
  void getImportedModules(SmallVectorImpl<ImportedModule> &imports,
                          ImportFilter filter = ImportFilter::Public) const;

  /// Finds all top-level decls of this module.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  /// The order of the results is not guaranteed to be meaningful.
  void getTopLevelDecls(SmallVectorImpl<Decl*> &Results) const;

  /// Finds all top-level decls that should be displayed to a client of this
  /// module.
  ///
  /// This includes types, variables, functions, and extensions.
  /// This does a simple local lookup, not recursively looking through imports.
  /// The order of the results is not guaranteed to be meaningful.
  ///
  /// This can differ from \c getTopLevelDecls, e.g. it returns decls from a
  /// shadowed clang module.
  void getDisplayDecls(SmallVectorImpl<Decl*> &results) const;

  /// @{

  /// Perform an action for every module visible from this module.
  ///
  /// This only includes modules with at least one declaration visible: if two
  /// import access paths are incompatible, the indirect module will be skipped.
  ///
  /// \param topLevelAccessPath If present, include the top-level module in the
  ///        results, with the given access path.
  /// \param includePrivateTopLevelImports If true, imports listed in all
  ///        file units within this module are traversed. Otherwise (the
  ///        default), only re-exported imports are traversed.
  /// \param fn A callback of type bool(ImportedModule) or void(ImportedModule).
  ///        Return \c false to abort iteration.
  ///
  /// \return True if the traversal ran to completion, false if it ended early
  ///         due to the callback.
  bool forAllVisibleModules(AccessPathTy topLevelAccessPath,
                            bool includePrivateTopLevelImports,
                            llvm::function_ref<bool(ImportedModule)> fn);

  bool forAllVisibleModules(AccessPathTy topLevelAccessPath,
                            bool includePrivateTopLevelImports,
                            llvm::function_ref<void(ImportedModule)> fn) {
    return forAllVisibleModules(topLevelAccessPath,
                                includePrivateTopLevelImports,
                                [=](const ImportedModule &import) -> bool {
      fn(import);
      return true;
    });
  }

  template <typename Fn>
  bool forAllVisibleModules(AccessPathTy topLevelAccessPath,
                            bool includePrivateTopLevelImports,
                            Fn &&fn) {
    using RetTy = typename std::result_of<Fn(ImportedModule)>::type;
    llvm::function_ref<RetTy(ImportedModule)> wrapped{std::forward<Fn>(fn)};
    return forAllVisibleModules(topLevelAccessPath,
                                includePrivateTopLevelImports,
                                wrapped);
  }

  template <typename Fn>
  bool forAllVisibleModules(AccessPathTy topLevelAccessPath, Fn &&fn) {
    return forAllVisibleModules(topLevelAccessPath, false,
                                std::forward<Fn>(fn));
  }

  /// @}

  using LinkLibraryCallback = llvm::function_ref<void(LinkLibrary)>;

  /// Generate the list of libraries needed to link this module, based on its
  /// imports.
  void collectLinkLibraries(LinkLibraryCallback callback);

  /// Returns true if the two access paths contain the same chain of
  /// identifiers.
  ///
  /// Source locations are ignored here.
  static bool isSameAccessPath(AccessPathTy lhs, AccessPathTy rhs);

  /// \brief Get the path for the file that this module came from, or an empty
  /// string if this is not applicable.
  StringRef getModuleFilename() const;

  /// \returns true if this module is the "swift" standard library module.
  bool isStdlibModule() const;

  /// \returns true if this module is the "builtin" module.
  bool isBuiltinModule() const;

  /// \returns true if this module is a system module; note that the StdLib is
  /// considered a system module.
  bool isSystemModule() const;

  /// \returns true if traversal was aborted, false otherwise.
  bool walk(ASTWalker &Walker);

  /// Register the file responsible for generating this module's entry point.
  bool registerEntryPointFile(FileUnit *file, SourceLoc diagLoc,
                              Optional<ArtificialMainKind> kind);

  /// Returns the associated clang module if one exists.
  const clang::Module *findUnderlyingClangModule();

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::Module;
  }

private:
  // Make placement new and vanilla new/delete illegal for Modules.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
public:
  // Only allow allocation of Modules using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(Module));
};

/// A container for module-scope declarations that itself provides a scope; the
/// smallest unit of code organization.
///
/// FileUnit is an abstract base class; its subclasses represent different
/// sorts of containers that can each provide a set of decls, e.g. a source
/// file. A module can contain several file-units.
class FileUnit : public DeclContext {
  virtual void anchor();

  // FIXME: Stick this in a PointerIntPair.
  const FileUnitKind Kind;

protected:
  FileUnit(FileUnitKind kind, Module &M)
    : DeclContext(DeclContextKind::FileUnit, &M), Kind(kind) {
  }

  virtual ~FileUnit() = default;

public:
  FileUnitKind getKind() const {
    return Kind;
  }

  /// Look up a (possibly overloaded) value set at top-level scope
  /// (but with the specified access path, which may come from an import decl)
  /// within this file.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  virtual void lookupValue(Module::AccessPathTy accessPath, DeclName name,
                           NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &result) const = 0;

  /// Find ValueDecls in the module and pass them to the given consumer object.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  virtual void lookupVisibleDecls(Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const {}

  /// Finds all class members defined in this file.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  virtual void lookupClassMembers(Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) const {}

  /// Finds class members defined in this file with the given name.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  virtual void lookupClassMember(Module::AccessPathTy accessPath,
                                 DeclName name,
                                 SmallVectorImpl<ValueDecl*> &results) const {}

  /// Returns the comment attached to the given declaration.
  ///
  /// This function is an implementation detail for comment serialization.
  /// If you just want to get a comment attached to a decl, use
  /// \c Decl::getRawComment() or \c Decl::getBriefComment().
  virtual Optional<BriefAndRawComment>
  getCommentForDecl(const Decl *D) const {
    return None;
  }

  /// Returns an implementation-defined "discriminator" for \p D, which
  /// distinguishes \p D from other declarations in the same module with the
  /// same name.
  ///
  /// Since this value is used in name mangling, it should be a valid ASCII-only
  /// identifier.
  virtual Identifier
  getDiscriminatorForPrivateValue(const ValueDecl *D) const = 0;

  /// Finds all top-level decls in this file.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  /// The order of the results is not guaranteed to be meaningful.
  virtual void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const {}

  /// Adds all top-level decls to the given vector.
  ///
  /// This includes all decls that should be displayed to clients of the module.
  /// The order of the results is not guaranteed to be meaningful.
  ///
  /// This can differ from \c getTopLevelDecls, e.g. it returns decls from a
  /// shadowed clang module.
  virtual void getDisplayDecls(SmallVectorImpl<Decl*> &results) const {
    getTopLevelDecls(results);
  }

  /// Looks up which modules are imported by this file.
  ///
  /// \p filter controls whether public, private, or any imports are included
  /// in this list.
  virtual void
  getImportedModules(SmallVectorImpl<Module::ImportedModule> &imports,
                     Module::ImportFilter filter) const {}

  /// Generates the list of libraries needed to link this file, based on its
  /// imports.
  virtual void
  collectLinkLibraries(Module::LinkLibraryCallback callback) const {}

  /// @{

  /// Perform an action for every module visible from this file.
  ///
  /// \param fn A callback of type bool(ImportedModule) or void(ImportedModule).
  ///           Return \c false to abort iteration.
  ///
  /// \return True if the traversal ran to completion, false if it ended early
  ///         due to the callback.
  bool
  forAllVisibleModules(llvm::function_ref<bool(Module::ImportedModule)> fn);

  bool
  forAllVisibleModules(llvm::function_ref<void(Module::ImportedModule)> fn) {
    return forAllVisibleModules([=](Module::ImportedModule import) -> bool {
      fn(import);
      return true;
    });
  }
  
  template <typename Fn>
  bool forAllVisibleModules(Fn &&fn) {
    using RetTy = typename std::result_of<Fn(Module::ImportedModule)>::type;
    llvm::function_ref<RetTy(Module::ImportedModule)> wrapped{
      std::forward<Fn>(fn)
    };
    return forAllVisibleModules(wrapped);
  }

  /// @}

  /// True if this file contains the main class for the module.
  bool hasMainClass() const {
    return getMainClass();
  }
  virtual ClassDecl *getMainClass() const {
    assert(hasEntryPoint());
    return nullptr;
  }
  virtual bool hasEntryPoint() const {
    return false;
  }

  /// Returns the associated clang module if one exists.
  virtual const clang::Module *getUnderlyingClangModule() { return nullptr; }

  /// Traverse the decls within this file.
  ///
  /// \returns true if traversal was aborted, false if it completed
  /// successfully.
  virtual bool walk(ASTWalker &walker);

  // Efficiency override for DeclContext::getParentModule().
  Module *getParentModule() const {
    return const_cast<Module *>(cast<Module>(getParent()));
  }

  // Efficiency override for DeclContext::getASTContext().
  ASTContext &getASTContext() const {
    return getParentModule()->Ctx;
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::FileUnit;
  }

private:
  // Make placement new and vanilla new/delete illegal for FileUnits.
  void *operator new(size_t Bytes) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;

protected:
  // Unfortunately we can't remove this altogether because the virtual
  // destructor requires it to be accessible.
  void operator delete(void *Data) throw() {
    llvm_unreachable("Don't use operator delete on a SourceFile");
  }

public:
  // Only allow allocation of FileUnits using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(FileUnit));
};
  
/// A container for a module-level definition derived as part of an implicit
/// protocol conformance.
class DerivedFileUnit final : public FileUnit {
  TinyPtrVector<FuncDecl *> DerivedDecls;

public:
  DerivedFileUnit(Module &M);
  ~DerivedFileUnit() = default;

  void addDerivedDecl(FuncDecl *FD) {
    DerivedDecls.push_back(FD);
  }

  void lookupValue(Module::AccessPathTy accessPath, DeclName name,
                   NLKind lookupKind,
                   SmallVectorImpl<ValueDecl*> &result) const override;
  
  void lookupVisibleDecls(Module::AccessPathTy accessPath,
                          VisibleDeclConsumer &consumer,
                          NLKind lookupKind) const override;
  
  void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const override;

  Identifier
  getDiscriminatorForPrivateValue(const ValueDecl *D) const override {
    llvm_unreachable("no private decls in the derived file unit");
  }

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::Derived;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};
  
/// A file containing Swift source code.
///
/// This is a .swift or .sil file (or a virtual file, such as the contents of
/// the REPL). Since it contains raw source, it must be parsed and name-bound
/// before being used for anything; a full type-check is also necessary for
/// IR generation.
class SourceFile final : public FileUnit {
public:
  class LookupCache;

  /// The implicit module import that the SourceFile should get.
  enum class ImplicitModuleImportKind {
    None,
    Builtin,
    Stdlib
  };

private:
  LookupCache *Cache = nullptr;
  LookupCache &getCache() const;

  /// This is the list of modules that are imported by this module, with the
  /// second element of the pair declaring whether the module is reexported.
  ///
  /// This is filled in by the Name Binding phase.
  ArrayRef<std::pair<Module::ImportedModule, bool>> Imports;

  /// A unique identifier representing this file; used to mark private decls
  /// within the file to keep them from conflicting with other files in the
  /// same module.
  mutable Identifier PrivateDiscriminator;

  /// The root TypeRefinementContext for this SourceFile.
  TypeRefinementContext *TRC = nullptr;

  /// If non-null, used to track name lookups that happen within this file.
  ReferencedNameTracker *ReferencedNames = nullptr;

  /// The class in this file marked \@NS/UIApplicationMain.
  ClassDecl *MainClass = nullptr;

  /// The source location of the main class.
  SourceLoc MainClassDiagLoc;

  /// \brief The ID for the memory buffer containing this file's source.
  ///
  /// May be -1, to indicate no association with a buffer.
  int BufferID;

  friend ASTContext;
  ~SourceFile();
public:
  /// The list of top-level declarations in the source file.
  std::vector<Decl*> Decls;

  /// The first location where an @objc attribute appeared.
  Optional<SourceLoc> FirstObjCAttrLoc;

  template <typename T>
  using OperatorMap = llvm::DenseMap<Identifier,llvm::PointerIntPair<T,1,bool>>;

  OperatorMap<InfixOperatorDecl*> InfixOperators;
  OperatorMap<PostfixOperatorDecl*> PostfixOperators;
  OperatorMap<PrefixOperatorDecl*> PrefixOperators;

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

  SourceFile(Module &M, SourceFileKind K, Optional<unsigned> bufferID,
             ImplicitModuleImportKind ModImpKind);

  ArrayRef<std::pair<Module::ImportedModule, bool>>
  getImports(bool allowUnparsed = false) const {
    assert(allowUnparsed || ASTStage >= Parsed || Kind == SourceFileKind::SIL);
    return Imports;
  }
  void setImports(ArrayRef<std::pair<Module::ImportedModule, bool>> IM) {
    Imports = IM;
  }

  void clearLookupCache();

  void cacheVisibleDecls(SmallVectorImpl<ValueDecl *> &&globals) const;
  const SmallVectorImpl<ValueDecl *> &getCachedVisibleDecls() const;

  virtual void lookupValue(Module::AccessPathTy accessPath, DeclName name,
                           NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &result) const override;

  virtual void lookupVisibleDecls(Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const override;

  virtual void lookupClassMembers(Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) const override;
  virtual void
  lookupClassMember(Module::AccessPathTy accessPath, DeclName name,
                    SmallVectorImpl<ValueDecl*> &results) const override;

  virtual void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getImportedModules(SmallVectorImpl<Module::ImportedModule> &imports,
                     Module::ImportFilter filter) const override;

  virtual void
  collectLinkLibraries(Module::LinkLibraryCallback callback) const override;

  Identifier getDiscriminatorForPrivateValue(const ValueDecl *D) const override;
  Identifier getPrivateDiscriminator() const { return PrivateDiscriminator; }

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
  /// @}

  ReferencedNameTracker *getReferencedNameTracker() {
    return ReferencedNames;
  }
  void setReferencedNameTracker(ReferencedNameTracker *Tracker) {
    assert(!ReferencedNames && "This file already has a name tracker.");
    ReferencedNames = Tracker;
  }

  /// \brief The buffer ID for the file that was imported, or None if there
  /// is no associated buffer.
  Optional<unsigned> getBufferID() const {
    if (BufferID == -1)
      return None;
    return BufferID;
  }

  /// If this buffer corresponds to a file on disk, returns the path.
  /// Otherwise, return an empty string.
  StringRef getFilename() const;

  void dump() const;
  void dump(raw_ostream &os) const;

  /// \brief Pretty-print the contents of this source file.
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

  /// Get the root refinement context for the file.
  TypeRefinementContext *getTypeRefinementContext();

};


/// This represents the compiler's implicitly generated declarations in the
/// Builtin module.
class BuiltinUnit final : public FileUnit {
public:
  class LookupCache;

private:
  LookupCache *Cache = nullptr;
  LookupCache &getCache() const;

  friend ASTContext;
  ~BuiltinUnit() = default;

public:
  explicit BuiltinUnit(Module &M);

  virtual void lookupValue(Module::AccessPathTy accessPath, DeclName name,
                           NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &result) const override;

  Identifier
  getDiscriminatorForPrivateValue(const ValueDecl *D) const override {
    llvm_unreachable("no private values in the Builtin module");
  }

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::Builtin;
  }

  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};

/// Represents an externally-loaded file of some kind.
class LoadedFile : public FileUnit {
protected:
  ~LoadedFile() = default;
  LoadedFile(FileUnitKind Kind, Module &M) noexcept
    : FileUnit(Kind, M) {
    assert(classof(this) && "invalid kind");
  }

public:
  /// Returns an arbitrary string representing the storage backing this file.
  ///
  /// This is usually a filesystem path.
  virtual StringRef getFilename() const;

  /// Look up an operator declaration.
  ///
  /// \param name The operator name ("+", ">>", etc.)
  ///
  /// \param fixity One of PrefixOperator, InfixOperator, or PostfixOperator.
  virtual OperatorDecl *lookupOperator(Identifier name, DeclKind fixity) const {
    return nullptr;
  }

  virtual bool isSystemModule() const { return false; }

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::SerializedAST ||
           file->getKind() == FileUnitKind::ClangModule;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};


inline SourceFile &Module::getMainSourceFile(SourceFileKind expectedKind) const{
  assert(!Files.empty() && "No files added yet");
  assert(cast<SourceFile>(Files.front())->Kind == expectedKind);
  return *cast<SourceFile>(Files.front());
}

inline FileUnit &Module::getMainFile(FileUnitKind expectedKind) const {
  assert(expectedKind != FileUnitKind::Source &&
         "must use specific source kind; see getMainSourceFile");
  assert(!Files.empty() && "No files added yet");
  assert(Files.front()->getKind() == expectedKind);
  return *Files.front();
}

/// Wraps either a swift module or a clang one.
/// FIXME: Should go away once swift modules can support submodules natively.
class ModuleEntity {
  llvm::PointerUnion<const Module *, const clang::Module *> Mod;

public:
  ModuleEntity() = default;
  ModuleEntity(const Module *Mod) : Mod(Mod) {}
  ModuleEntity(const clang::Module *Mod) : Mod(Mod) {}

  StringRef getName() const;
  std::string getFullName() const;

  bool isSystemModule() const;
  bool isBuiltinModule() const;

  explicit operator bool() const { return !Mod.isNull(); }
};

} // end namespace swift

namespace llvm {
  template <>
  class DenseMapInfo<swift::Module::ImportedModule> {
    using Module = swift::Module;
  public:
    static Module::ImportedModule getEmptyKey() {
      return {{}, llvm::DenseMapInfo<Module *>::getEmptyKey()};
    }
    static Module::ImportedModule getTombstoneKey() {
      return {{}, llvm::DenseMapInfo<Module *>::getTombstoneKey()};
    }

    static unsigned getHashValue(const Module::ImportedModule &val) {
      auto pair = std::make_pair(val.first.size(), val.second);
      return llvm::DenseMapInfo<decltype(pair)>::getHashValue(pair);
    }

    static bool isEqual(const Module::ImportedModule &lhs,
                        const Module::ImportedModule &rhs) {
      return lhs.second == rhs.second &&
             Module::isSameAccessPath(lhs.first, rhs.first);
    }
  };
}

#endif
