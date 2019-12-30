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
  class SyntaxParsingCache;
  class ASTScope;
  class SourceLookupCache;

  namespace syntax {
  class SourceFileSyntax;
}
namespace ast_scope {
class ASTSourceFileScope;
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
  /// A Clang module imported from DWARF.
  DWARFModule
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
  /// This is the behavior with -enable-library-evolution.
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
    void printForward(raw_ostream &out, StringRef delim = ".") const;
  };

private:
  /// If non-NULL, a plug-in that should be used when performing external
  /// lookups.
  // FIXME: Do we really need to bloat all modules with this?
  DebuggerClient *DebugClient = nullptr;

  SmallVector<FileUnit *, 2> Files;

  std::unique_ptr<SourceLookupCache> Cache;
  SourceLookupCache &getSourceLookupCache() const;

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

    FileUnit *getEntryPointFile() const;
    void setEntryPointFile(FileUnit *file);
    bool hasEntryPoint() const;

    bool markDiagnosedMultipleMainClasses();
    bool markDiagnosedMainClassWithScript();
  };

  /// Information about the file responsible for the module's entry point,
  /// if any.
  ///
  /// \see EntryPointInfoTy
  EntryPointInfoTy EntryPointInfo;

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
    return Bits.ModuleDecl.TestingEnabled;
  }
  void setTestingEnabled(bool enabled = true) {
    Bits.ModuleDecl.TestingEnabled = enabled;
  }

  // Returns true if this module is compiled with implicit dynamic.
  bool isImplicitDynamicEnabled() const {
    return Bits.ModuleDecl.ImplicitDynamicEnabled;
  }
  void setImplicitDynamicEnabled(bool enabled = true) {
    Bits.ModuleDecl.ImplicitDynamicEnabled = enabled;
  }

  /// Returns true if this module was or is begin compile with
  /// `-enable-private-imports`.
  bool arePrivateImportsEnabled() const {
    return Bits.ModuleDecl.PrivateImportsEnabled;
  }
  void setPrivateImportsEnabled(bool enabled = true) {
    Bits.ModuleDecl.PrivateImportsEnabled = true;
  }

  /// Returns true if there was an error trying to load this module.
  bool failedToLoad() const {
    return Bits.ModuleDecl.FailedToLoad;
  }
  void setFailedToLoad(bool failed = true) {
    Bits.ModuleDecl.FailedToLoad = failed;
  }

  bool hasResolvedImports() const {
    return Bits.ModuleDecl.HasResolvedImports;
  }
  void setHasResolvedImports() {
    Bits.ModuleDecl.HasResolvedImports = true;
  }

  ResilienceStrategy getResilienceStrategy() const {
    return ResilienceStrategy(Bits.ModuleDecl.RawResilienceStrategy);
  }
  void setResilienceStrategy(ResilienceStrategy strategy) {
    Bits.ModuleDecl.RawResilienceStrategy = unsigned(strategy);
  }

  /// \returns true if this module is a system module; note that the StdLib is
  /// considered a system module.
  bool isSystemModule() const {
    return Bits.ModuleDecl.IsSystemModule;
  }
  void setIsSystemModule(bool flag = true) {
    Bits.ModuleDecl.IsSystemModule = flag;
  }

  /// Returns true if this module is a non-Swift module that was imported into
  /// Swift.
  ///
  /// Right now that's just Clang modules.
  bool isNonSwiftModule() const {
    return Bits.ModuleDecl.IsNonSwiftModule;
  }
  /// \see #isNonSwiftModule
  void setIsNonSwiftModule(bool flag = true) {
    Bits.ModuleDecl.IsNonSwiftModule = flag;
  }

  bool isResilient() const {
    return getResilienceStrategy() != ResilienceStrategy::Default;
  }

  /// Look up a (possibly overloaded) value set at top-level scope
  /// (but with the specified access path, which may come from an import decl)
  /// within the current module.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  void lookupValue(DeclName Name, NLKind LookupKind,
                   SmallVectorImpl<ValueDecl*> &Result) const;

  /// Look up a local type declaration by its mangled name.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  TypeDecl *lookupLocalType(StringRef MangledName) const;

  /// Look up an opaque return type by the mangled name of the declaration
  /// that defines it.
  OpaqueTypeDecl *lookupOpaqueResultType(StringRef MangledName);
  
  /// Find ValueDecls in the module and pass them to the given consumer object.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  void lookupVisibleDecls(AccessPathTy AccessPath,
                          VisibleDeclConsumer &Consumer,
                          NLKind LookupKind) const;

  /// This is a hack for 'main' file parsing and the integrated REPL.
  ///
  /// FIXME: Refactor main file parsing to not pump the parser incrementally.
  /// FIXME: Remove the integrated REPL.
  void clearLookupCache();

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
  ProtocolConformanceRef lookupConformance(Type type, ProtocolDecl *protocol);

  /// Look for the conformance of the given existential type to the given
  /// protocol.
  ProtocolConformanceRef lookupExistentialConformance(Type type,
                                                      ProtocolDecl *protocol);

  /// Exposes TypeChecker functionality for querying protocol conformance.
  /// Returns a valid ProtocolConformanceRef only if all conditional
  /// requirements are successfully resolved.
  ProtocolConformanceRef conformsToProtocol(Type sourceTy,
                                            ProtocolDecl *targetProtocol);

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
  enum class ImportFilterKind {
    /// Include imports declared with `@_exported`.
    Public = 1 << 0,
    /// Include "regular" imports with no special annotation.
    Private = 1 << 1,
    /// Include imports declared with `@_implementationOnly`.
    ImplementationOnly = 1 << 2
  };
  /// \sa getImportedModules
  using ImportFilter = OptionSet<ImportFilterKind>;

  /// Looks up which modules are imported by this module.
  ///
  /// \p filter controls whether public, private, or any imports are included
  /// in this list.
  void getImportedModules(SmallVectorImpl<ImportedModule> &imports,
                          ImportFilter filter = ImportFilterKind::Public) const;

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

  /// Finds top-level decls of this module filtered by their attributes.
  ///
  /// This does a simple local lookup, not recursively looking through imports.
  /// The order of the results is not guaranteed to be meaningful.
  ///
  /// \param Results Vector collecting the decls.
  ///
  /// \param matchAttributes Check on the attributes of a decl to
  /// filter which decls to fully deserialize. Only decls with accepted
  /// attributes are deserialized and added to Results.
  void getTopLevelDeclsWhereAttributesMatch(
               SmallVectorImpl<Decl*> &Results,
               llvm::function_ref<bool(DeclAttributes)> matchAttributes) const;

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

  using LinkLibraryCallback = llvm::function_ref<void(LinkLibrary)>;

  /// Generate the list of libraries needed to link this module, based on its
  /// imports.
  void collectLinkLibraries(LinkLibraryCallback callback) const;

  /// Returns true if the two access paths contain the same chain of
  /// identifiers.
  ///
  /// Source locations are ignored here.
  static bool isSameAccessPath(AccessPathTy lhs, AccessPathTy rhs);

  /// Get the path for the file that this module came from, or an empty
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

/// Extract the source location from the given module declaration.
inline SourceLoc extractNearestSourceLoc(const ModuleDecl *mod) {
  return extractNearestSourceLoc(static_cast<const Decl *>(mod));
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
