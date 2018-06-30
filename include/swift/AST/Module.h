//===--- Module.h - Swift Language Module ASTs ------------------*- C++ -*-===//
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
// This file defines the Module class and its subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MODULE_H
#define SWIFT_MODULE_H

#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/LookupKinds.h"
#include "swift/AST/RawComment.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/Type.h"
#include "swift/Basic/Compiler.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MD5.h"

namespace clang {
  class Module;
}

namespace swift {
  enum class ArtificialMainKind : uint8_t;
  class ASTContext;
  class ASTScope;
  class ASTWalker;
  class BraceStmt;
  class Decl;
  class DeclAttribute;
  class TypeDecl;
  enum class DeclKind : uint8_t;
  class ExtensionDecl;
  class DebuggerClient;
  class DeclName;
  class FileUnit;
  class FuncDecl;
  class InfixOperatorDecl;
  class LinkLibrary;
  class LookupCache;
  class ModuleLoader;
  class NominalTypeDecl;
  class EnumElementDecl;
  class OperatorDecl;
  class PostfixOperatorDecl;
  class PrefixOperatorDecl;
  class ProtocolConformance;
  class ProtocolDecl;
  struct PrintOptions;
  class ReferencedNameTracker;
  class Token;
  class TupleType;
  class Type;
  class TypeRefinementContext;
  class ValueDecl;
  class VarDecl;
  class VisibleDeclConsumer;
  
namespace syntax {
  class SourceFileSyntax;
}

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
};

enum class SourceFileKind {
  Library,  ///< A normal .swift file.
  Main,     ///< A .swift file that can have top-level code.
  REPL,     ///< A virtual file that holds the user's input in the REPL.
  SIL,      ///< Came from a .sil file.
  Interface ///< Came from a .swiftinterface file, representing another module.
};

/// Discriminator for resilience strategy.
enum class ResilienceStrategy : unsigned {
  /// Public nominal types: fragile
  /// Non-inlinable function bodies: resilient
  ///
  /// This is the default behavior without any flags.
  Default,

  /// Public nominal types: resilient
  /// Non-inlinable function bodies: resilient
  ///
  /// This is the behavior with -enable-resilience.
  Resilient
};

/// The minimum unit of compilation.
///
/// A module is made up of several file-units, which are all part of the same
/// output binary and logical module (such as a single library or executable).
///
/// \sa FileUnit
class ModuleDecl : public DeclContext, public TypeDecl {
public:
  typedef ArrayRef<std::pair<Identifier, SourceLoc>> AccessPathTy;
  typedef std::pair<ModuleDecl::AccessPathTy, ModuleDecl*> ImportedModule;
  
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
        return std::less<const ModuleDecl *>()(lhs.second, rhs.second);
      if (lhs.first.data() != rhs.first.data())
        return std::less<AccessPathTy::iterator>()(lhs.first.begin(),
                                                   rhs.first.begin());
      return lhs.first.size() < rhs.first.size();
    }
  };

  /// Produces the components of a given module's full name in reverse order.
  ///
  /// For a Swift module, this will only ever have one component, but an
  /// imported Clang module might actually be a submodule.
  class ReverseFullNameIterator {
  public:
    // Make this look like a valid STL iterator.
    using difference_type = int;
    using value_type = StringRef;
    using pointer = StringRef *;
    using reference = StringRef;
    using iterator_category = std::forward_iterator_tag;

  private:
    PointerUnion<const ModuleDecl *, const /* clang::Module */ void *> current;
  public:
    ReverseFullNameIterator() = default;
    explicit ReverseFullNameIterator(const ModuleDecl *M);
    explicit ReverseFullNameIterator(const clang::Module *clangModule) {
      current = clangModule;
    }

    StringRef operator*() const;
    ReverseFullNameIterator &operator++();

    friend bool operator==(ReverseFullNameIterator left,
                           ReverseFullNameIterator right) {
      return left.current == right.current;
    }
    friend bool operator!=(ReverseFullNameIterator left,
                           ReverseFullNameIterator right) {
      return !(left == right);
    }

    /// This is a convenience function that writes the entire name, in forward
    /// order, to \p out.
    void printForward(raw_ostream &out) const;
  };

private:
  /// If non-NULL, a plug-in that should be used when performing external
  /// lookups.
  // FIXME: Do we really need to bloat all modules with this?
  DebuggerClient *DebugClient = nullptr;

  SmallVector<FileUnit *, 2> Files;

  /// Tracks the file that will generate the module's entry point, either
  /// because it contains a class marked with \@UIApplicationMain
  /// or \@NSApplicationMain, or because it is a script file.
  class EntryPointInfoTy {
    enum class Flags {
      DiagnosedMultipleMainClasses = 1 << 0,
      DiagnosedMainClassWithScript = 1 << 1
    };
    llvm::PointerIntPair<FileUnit *, 2, OptionSet<Flags>> storage;
  public:
    EntryPointInfoTy() = default;

    FileUnit *getEntryPointFile() const {
      return storage.getPointer();
    }
    void setEntryPointFile(FileUnit *file) {
      assert(!storage.getPointer());
      storage.setPointer(file);
    }

    bool hasEntryPoint() const {
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
  /// \see EntryPointInfoTy
  EntryPointInfoTy EntryPointInfo;

  struct {
    unsigned TestingEnabled : 1;
    unsigned FailedToLoad : 1;
    unsigned ResilienceStrategy : 1;
    unsigned HasResolvedImports : 1;
  } Flags;

  ModuleDecl(Identifier name, ASTContext &ctx);

public:
  static ModuleDecl *create(Identifier name, ASTContext &ctx) {
    return new (ctx) ModuleDecl(name, ctx);
  }

  using Decl::getASTContext;

  ArrayRef<FileUnit *> getFiles() {
    return Files;
  }
  ArrayRef<const FileUnit *> getFiles() const {
    return { Files.begin(), Files.size() };
  }

  bool isClangModule() const;
  void addFile(FileUnit &newFile);
  void removeFile(FileUnit &existingFile);

  /// Convenience accessor for clients that know what kind of file they're
  /// dealing with.
  SourceFile &getMainSourceFile(SourceFileKind expectedKind) const;

  /// Convenience accessor for clients that know what kind of file they're
  /// dealing with.
  FileUnit &getMainFile(FileUnitKind expectedKind) const;

  DebuggerClient *getDebugClient() const { return DebugClient; }
  void setDebugClient(DebuggerClient *R) {
    assert(!DebugClient && "Debugger client already set");
    DebugClient = R;
  }

  /// Returns true if this module was or is being compiled for testing.
  bool isTestingEnabled() const {
    return Flags.TestingEnabled;
  }
  void setTestingEnabled(bool enabled = true) {
    Flags.TestingEnabled = enabled;
  }

  /// Returns true if there was an error trying to load this module.
  bool failedToLoad() const {
    return Flags.FailedToLoad;
  }
  void setFailedToLoad(bool failed = true) {
    Flags.FailedToLoad = failed;
  }

  bool hasResolvedImports() const {
    return Flags.HasResolvedImports;
  }
  void setHasResolvedImports() {
    Flags.HasResolvedImports = true;
  }

  ResilienceStrategy getResilienceStrategy() const {
    return ResilienceStrategy(Flags.ResilienceStrategy);
  }
  void setResilienceStrategy(ResilienceStrategy strategy) {
    Flags.ResilienceStrategy = unsigned(strategy);
  }

  /// Look up a (possibly overloaded) value set at top-level scope
  /// (but with the specified access path, which may come from an import decl)
  /// within the current module.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  void lookupValue(AccessPathTy AccessPath, DeclName Name, NLKind LookupKind,
                   SmallVectorImpl<ValueDecl*> &Result) const;

  /// Look up a local type declaration by its mangled name.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  TypeDecl *lookupLocalType(StringRef MangledName) const;

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
  PrecedenceGroupDecl *lookupPrecedenceGroup(Identifier name,
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
  /// \c protocol.
  ///
  /// \param type The type for which we are computing conformance.
  ///
  /// \param protocol The protocol to which we are computing conformance.
  ///
  /// \returns The result of the conformance search, which will be
  /// None if the type does not conform to the protocol or contain a
  /// ProtocolConformanceRef if it does conform.
  Optional<ProtocolConformanceRef>
  lookupConformance(Type type, ProtocolDecl *protocol);

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

  /// Find all Objective-C methods with the given selector.
  void lookupObjCMethods(
         ObjCSelector selector,
         SmallVectorImpl<AbstractFunctionDecl *> &results) const;

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

  /// Looks up which modules are imported by this module, ignoring any that
  /// won't contain top-level decls.
  ///
  /// This is a performance hack. Do not use for anything but name lookup.
  /// May go away in the future.
  void
  getImportedModulesForLookup(SmallVectorImpl<ImportedModule> &imports) const;

  /// Uniques the items in \p imports, ignoring the source locations of the
  /// access paths.
  ///
  /// The order of items in \p imports is \e not preserved.
  static void removeDuplicateImports(SmallVectorImpl<ImportedModule> &imports);

  /// Finds all top-level decls of this module.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  /// The order of the results is not guaranteed to be meaningful.
  void getTopLevelDecls(SmallVectorImpl<Decl*> &Results) const;

  /// Finds all local type decls of this module.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  /// The order of the results is not guaranteed to be meaningful.
  void getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &Results) const;

  /// Finds all precedence group decls of this module.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  /// The order of the results is not guaranteed to be meaningful.
  void getPrecedenceGroups(SmallVectorImpl<PrecedenceGroupDecl*> &Results) const;

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
  /// Modules that can't be used for lookup (including Clang submodules at the
  /// time this comment was written) are also skipped under certain
  /// circumstances.
  ///
  /// \param topLevelAccessPath If present, include the top-level module in the
  ///        results, with the given access path.
  /// \param fn A callback of type bool(ImportedModule) or void(ImportedModule).
  ///        Return \c false to abort iteration.
  ///
  /// \return True if the traversal ran to completion, false if it ended early
  ///         due to the callback.
  bool forAllVisibleModules(AccessPathTy topLevelAccessPath,
                            llvm::function_ref<bool(ImportedModule)> fn);

  bool forAllVisibleModules(AccessPathTy topLevelAccessPath,
                            llvm::function_ref<void(ImportedModule)> fn) {
    return forAllVisibleModules(topLevelAccessPath,
                                [=](const ImportedModule &import) -> bool {
      fn(import);
      return true;
    });
  }

  template <typename Fn>
  bool forAllVisibleModules(AccessPathTy topLevelAccessPath,
                            Fn &&fn) {
    using RetTy = typename std::result_of<Fn(ImportedModule)>::type;
    llvm::function_ref<RetTy(ImportedModule)> wrapped{std::forward<Fn>(fn)};
    return forAllVisibleModules(topLevelAccessPath, wrapped);
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

  /// \returns true if this module is the "SwiftShims" module;
  bool isSwiftShimsModule() const;

  /// \returns true if this module is the "builtin" module.
  bool isBuiltinModule() const;

  /// \returns true if this module is the "SwiftOnoneSupport" module;
  bool isOnoneSupportModule() const;

  /// \returns true if this module is a system module; note that the StdLib is
  /// considered a system module.
  bool isSystemModule() const;

  /// \returns true if traversal was aborted, false otherwise.
  bool walk(ASTWalker &Walker);

  /// Register the file responsible for generating this module's entry point.
  ///
  /// \returns true if there was a problem adding this file.
  bool registerEntryPointFile(FileUnit *file, SourceLoc diagLoc,
                              Optional<ArtificialMainKind> kind);

  /// \returns true if this module has a main entry point.
  bool hasEntryPoint() const {
    return EntryPointInfo.hasEntryPoint();
  }

  /// Returns the associated clang module if one exists.
  const clang::Module *findUnderlyingClangModule() const;

  /// Returns a generator with the components of this module's full,
  /// hierarchical name.
  ///
  /// For a Swift module, this will only ever have one component, but an
  /// imported Clang module might actually be a submodule.
  ReverseFullNameIterator getReverseFullModuleName() const {
    return ReverseFullNameIterator(this);
  }

  SourceRange getSourceRange() const { return SourceRange(); }

  static bool classof(const DeclContext *DC) {
    if (auto D = DC->getAsDecl())
      return classof(D);
    return false;
  }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Module;
  }

private:
  // Make placement new and vanilla new/delete illegal for Modules.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() SWIFT_DELETE_OPERATOR_DELETED;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
public:
  // Only allow allocation of Modules using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, const ASTContext &C,
                     unsigned Alignment = alignof(ModuleDecl));
};

static inline unsigned alignOfFileUnit();

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
  FileUnit(FileUnitKind kind, ModuleDecl &M)
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
  virtual void lookupValue(ModuleDecl::AccessPathTy accessPath, DeclName name,
                           NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &result) const = 0;

  /// Look up a local type declaration by its mangled name.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  virtual TypeDecl *lookupLocalType(StringRef MangledName) const {
    return nullptr;
  }

  /// Directly look for a nested type declared within this module inside the
  /// given nominal type (including any extensions).
  ///
  /// This is a fast-path hack to avoid circular dependencies in deserialization
  /// and the Clang importer.
  ///
  /// Private and fileprivate types should not be returned by this lookup.
  virtual TypeDecl *lookupNestedType(Identifier name,
                                     const NominalTypeDecl *parent) const {
    return nullptr;
  }

  /// Find ValueDecls in the module and pass them to the given consumer object.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  virtual void lookupVisibleDecls(ModuleDecl::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const {}

  /// Finds all class members defined in this file.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  virtual void lookupClassMembers(ModuleDecl::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) const {}

  /// Finds class members defined in this file with the given name.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  virtual void lookupClassMember(ModuleDecl::AccessPathTy accessPath,
                                 DeclName name,
                                 SmallVectorImpl<ValueDecl*> &results) const {}

  /// Find all Objective-C methods with the given selector.
  virtual void lookupObjCMethods(
                 ObjCSelector selector,
                 SmallVectorImpl<AbstractFunctionDecl *> &results) const = 0;

  /// Returns the comment attached to the given declaration.
  ///
  /// This function is an implementation detail for comment serialization.
  /// If you just want to get a comment attached to a decl, use
  /// \c Decl::getRawComment() or \c Decl::getBriefComment().
  virtual Optional<CommentInfo>
  getCommentForDecl(const Decl *D) const {
    return None;
  }

  virtual Optional<StringRef>
  getGroupNameForDecl(const Decl *D) const {
    return None;
  }

  virtual Optional<StringRef>
  getSourceFileNameForDecl(const Decl *D) const {
    return None;
  }

  virtual Optional<unsigned>
  getSourceOrderForDecl(const Decl *D) const {
    return None;
  }

  virtual Optional<StringRef>
  getGroupNameByUSR(StringRef USR) const {
    return None;
  }

  virtual void collectAllGroups(std::vector<StringRef> &Names) const {}

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


  /// Finds all precedence group decls in this file.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  /// The order of the results is not guaranteed to be meaningful.
  virtual void
  getPrecedenceGroups(SmallVectorImpl<PrecedenceGroupDecl*> &Results) const {}

  /// Finds all local type decls in this file.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  /// The order of the results is not guaranteed to be meaningful.
  virtual void getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &results) const {}

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
  getImportedModules(SmallVectorImpl<ModuleDecl::ImportedModule> &imports,
                     ModuleDecl::ImportFilter filter) const {}

  /// \see ModuleDecl::getImportedModulesForLookup
  virtual void getImportedModulesForLookup(
      SmallVectorImpl<ModuleDecl::ImportedModule> &imports) const {
    return getImportedModules(imports, ModuleDecl::ImportFilter::Public);
  }

  /// Generates the list of libraries needed to link this file, based on its
  /// imports.
  virtual void
  collectLinkLibraries(ModuleDecl::LinkLibraryCallback callback) const {}

  /// @{

  /// Perform an action for every module visible from this file.
  ///
  /// \param fn A callback of type bool(ImportedModule) or void(ImportedModule).
  ///           Return \c false to abort iteration.
  ///
  /// \return True if the traversal ran to completion, false if it ended early
  ///         due to the callback.
  bool
  forAllVisibleModules(llvm::function_ref<bool(ModuleDecl::ImportedModule)> fn);

  bool
  forAllVisibleModules(llvm::function_ref<void(ModuleDecl::ImportedModule)> fn) {
    return forAllVisibleModules([=](ModuleDecl::ImportedModule import) -> bool {
      fn(import);
      return true;
    });
  }
  
  template <typename Fn>
  bool forAllVisibleModules(Fn &&fn) {
    using RetTy = typename std::result_of<Fn(ModuleDecl::ImportedModule)>::type;
    llvm::function_ref<RetTy(ModuleDecl::ImportedModule)> wrapped{
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
  virtual const clang::Module *getUnderlyingClangModule() const {
    return nullptr;
  }

  /// Traverse the decls within this file.
  ///
  /// \returns true if traversal was aborted, false if it completed
  /// successfully.
  virtual bool walk(ASTWalker &walker);

  // Efficiency override for DeclContext::getParentModule().
  ModuleDecl *getParentModule() const {
    return const_cast<ModuleDecl *>(cast<ModuleDecl>(getParent()));
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
                     unsigned Alignment = alignOfFileUnit());
};

static inline unsigned alignOfFileUnit() {
  return alignof(FileUnit&);
}
  
/// A file containing Swift source code.
///
/// This is a .swift or .sil file (or a virtual file, such as the contents of
/// the REPL). Since it contains raw source, it must be parsed and name-bound
/// before being used for anything; a full type-check is also necessary for
/// IR generation.
class SourceFile final : public FileUnit {
public:
  class LookupCache;
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
    Testable = 0x2
  };

  /// \see ImportFlags
  using ImportOptions = OptionSet<ImportFlags>;

private:
  std::unique_ptr<LookupCache> Cache;
  LookupCache &getCache() const;

  /// This is the list of modules that are imported by this module.
  ///
  /// This is filled in by the Name Binding phase.
  ArrayRef<std::pair<ModuleDecl::ImportedModule, ImportOptions>> Imports;

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

  /// \brief The ID for the memory buffer containing this file's source.
  ///
  /// May be -1, to indicate no association with a buffer.
  int BufferID;

  /// The list of protocol conformances that were "used" within this
  /// source file.
  llvm::SetVector<NormalProtocolConformance *> UsedConformances;

  /// The scope map that describes this source file.
  ASTScope *Scope = nullptr;

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

  void
  addImports(ArrayRef<std::pair<ModuleDecl::ImportedModule, ImportOptions>> IM);

  bool hasTestableImport(const ModuleDecl *module) const;

  void clearLookupCache();

  void cacheVisibleDecls(SmallVectorImpl<ValueDecl *> &&globals) const;
  const SmallVectorImpl<ValueDecl *> &getCachedVisibleDecls() const;

  virtual void lookupValue(ModuleDecl::AccessPathTy accessPath, DeclName name,
                           NLKind lookupKind,
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

  virtual void
  getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &results) const override;

  virtual void
  getImportedModules(SmallVectorImpl<ModuleDecl::ImportedModule> &imports,
                     ModuleDecl::ImportFilter filter) const override;

  virtual void
  collectLinkLibraries(ModuleDecl::LinkLibraryCallback callback) const override;

  Identifier getDiscriminatorForPrivateValue(const ValueDecl *D) const override;
  Identifier getPrivateDiscriminator() const { return PrivateDiscriminator; }

  virtual bool walk(ASTWalker &walker) override;

  /// Note that the given conformance was used by this source file.
  void addUsedConformance(NormalProtocolConformance *conformance) {
    UsedConformances.insert(conformance);
  }

  /// Retrieve the set of conformances that were used in this source
  /// file.
  ArrayRef<NormalProtocolConformance *> getUsedConformances() const {
    return UsedConformances.getArrayRef();
  }

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

  /// Retrieve the scope that describes this source file.
  ASTScope &getScope();

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

  syntax::SourceFileSyntax getSyntaxRoot() const;
  void setSyntaxRoot(syntax::SourceFileSyntax &&Root);
  bool hasSyntaxRoot() const;

private:

  /// If not None, the underlying vector should contain tokens of this source file.
  Optional<std::vector<Token>> AllCorrectedTokens;

  std::unique_ptr<SourceFileSyntaxInfo> SyntaxInfo;
};


/// This represents the compiler's implicitly generated declarations in the
/// Builtin module.
class BuiltinUnit final : public FileUnit {
public:
  class LookupCache;

private:
  std::unique_ptr<LookupCache> Cache;
  LookupCache &getCache() const;

  friend ASTContext;
  ~BuiltinUnit() = default;

public:
  explicit BuiltinUnit(ModuleDecl &M);

  virtual void lookupValue(ModuleDecl::AccessPathTy accessPath, DeclName name,
                           NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &result) const override;

  /// Find all Objective-C methods with the given selector.
  void lookupObjCMethods(
         ObjCSelector selector,
         SmallVectorImpl<AbstractFunctionDecl *> &results) const override;

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
  LoadedFile(FileUnitKind Kind, ModuleDecl &M) noexcept
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

  /// Look up a precedence group.
  ///
  /// \param name The precedence group name.
  virtual PrecedenceGroupDecl *lookupPrecedenceGroup(Identifier name) const {
    return nullptr;
  }

  virtual bool isSystemModule() const { return false; }

  /// Retrieve the set of generic signatures stored within this module.
  ///
  /// \returns \c true if this module file supports retrieving all of the
  /// generic signatures, \c false otherwise.
  virtual bool getAllGenericSignatures(
                 SmallVectorImpl<GenericSignature*> &genericSignatures) {
    return false;
  }

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::SerializedAST ||
           file->getKind() == FileUnitKind::ClangModule;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};


inline SourceFile &
ModuleDecl::getMainSourceFile(SourceFileKind expectedKind) const {
  assert(!Files.empty() && "No files added yet");
  assert(cast<SourceFile>(Files.front())->Kind == expectedKind);
  return *cast<SourceFile>(Files.front());
}

inline FileUnit &ModuleDecl::getMainFile(FileUnitKind expectedKind) const {
  assert(expectedKind != FileUnitKind::Source &&
         "must use specific source kind; see getMainSourceFile");
  assert(!Files.empty() && "No files added yet");
  assert(Files.front()->getKind() == expectedKind);
  return *Files.front();
}

/// Wraps either a swift module or a clang one.
/// FIXME: Should go away once swift modules can support submodules natively.
class ModuleEntity {
  llvm::PointerUnion<const ModuleDecl *, const /* clang::Module */ void *> Mod;

public:
  ModuleEntity() = default;
  ModuleEntity(const ModuleDecl *Mod) : Mod(Mod) {}
  ModuleEntity(const clang::Module *Mod) : Mod(static_cast<const void *>(Mod)){}

  StringRef getName() const;
  std::string getFullName() const;

  bool isSystemModule() const;
  bool isBuiltinModule() const;
  const ModuleDecl *getAsSwiftModule() const;
  const clang::Module *getAsClangModule() const;

  void *getOpaqueValue() const {
    assert(!Mod.isNull());
    return Mod.getOpaqueValue();
  }

  explicit operator bool() const { return !Mod.isNull(); }
};

inline bool DeclContext::isModuleContext() const {
  if (auto D = getAsDecl())
    return ModuleDecl::classof(D);
  return false;
}

inline bool DeclContext::isModuleScopeContext() const {
  if (ParentAndKind.getInt() == ASTHierarchy::FileUnit)
    return true;
  return isModuleContext();
}

} // end namespace swift

namespace llvm {
  template <>
  class DenseMapInfo<swift::ModuleDecl::ImportedModule> {
    using ModuleDecl = swift::ModuleDecl;
  public:
    static ModuleDecl::ImportedModule getEmptyKey() {
      return {{}, llvm::DenseMapInfo<ModuleDecl *>::getEmptyKey()};
    }
    static ModuleDecl::ImportedModule getTombstoneKey() {
      return {{}, llvm::DenseMapInfo<ModuleDecl *>::getTombstoneKey()};
    }

    static unsigned getHashValue(const ModuleDecl::ImportedModule &val) {
      auto pair = std::make_pair(val.first.size(), val.second);
      return llvm::DenseMapInfo<decltype(pair)>::getHashValue(pair);
    }

    static bool isEqual(const ModuleDecl::ImportedModule &lhs,
                        const ModuleDecl::ImportedModule &rhs) {
      return lhs.second == rhs.second &&
             ModuleDecl::isSameAccessPath(lhs.first, rhs.first);
    }
  };
}

#endif
