//===--- ASTContext.h - AST Context Object ----------------------*- C++ -*-===//
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
// This file defines the ASTContext interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTCONTEXT_H
#define SWIFT_AST_ASTCONTEXT_H

#include "llvm/Support/DataTypes.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Type.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Malloc.h"
#include "swift/Basic/Optional.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <functional>
#include <memory>
#include <utility>
#include <vector>

namespace llvm {
  class BumpPtrAllocator;
}

namespace clang {
  class Decl;
  class MacroInfo;
}

namespace swift {
  class ASTContext;
  class ASTMutationListener;
  class BoundGenericType;
  class Decl;
  class ExtensionDecl;
  class FuncDecl;
  class LazyResolver;
  class SourceLoc;
  class Type;
  class TupleType;
  class FunctionType;
  class ArchetypeType;
  class ArrayType;
  class Identifier;
  class Module;
  class ModuleLoader;
  class NominalTypeDecl;
  class TupleTypeElt;
  class EnumElementDecl;
  class ProtocolDecl;
  class SubstitutableType;
  class SourceManager;
  class ValueDecl;
  class DiagnosticEngine;
  class Substitution;
  class TypeCheckerDebugConsumer;

  enum class KnownProtocolKind : uint8_t;

  typedef llvm::PointerUnion<const clang::Decl *, clang::MacroInfo *> ClangNode;

/// \brief The arena in which a particular ASTContext allocation will go.
enum class AllocationArena {
  /// \brief The permanent arena, which is tied to the lifetime of
  /// the ASTContext.
  ///
  /// All global declarations and types need to be allocated into this arena.
  /// At present, everything that is not a type involving a type variable is
  /// allocated in this arena.
  Permanent,
  /// \brief The constraint solver's temporary arena, which is tied to the
  /// lifetime of a particular instance of the constraint solver.
  ///
  /// Any type involving a type variable is allocated in this arena.
  ConstraintSolver
};

/// \brief Introduces a new constraint checker arena, whose lifetime is
/// tied to the lifetime of this RAII object.
class ConstraintCheckerArenaRAII {
  ASTContext &Self;
  void *Data;

public:
  /// \brief Introduces a new constraint checker arena, supplanting any
  /// existing constraint checker arena.
  ///
  /// \param self The ASTContext into which this constraint checker arena
  /// will be installed.
  ///
  /// \param allocator The allocator used for allocating any data that
  /// goes into the constraint checker arena.
  ConstraintCheckerArenaRAII(ASTContext &self,
                             llvm::BumpPtrAllocator &allocator);

  ConstraintCheckerArenaRAII(const ConstraintCheckerArenaRAII &) = delete;
  ConstraintCheckerArenaRAII(ConstraintCheckerArenaRAII &&) = delete;

  ConstraintCheckerArenaRAII &
  operator=(const ConstraintCheckerArenaRAII &) = delete;

  ConstraintCheckerArenaRAII &
  operator=(ConstraintCheckerArenaRAII &&) = delete;

  ~ConstraintCheckerArenaRAII();
};

/// \brief Describes either a nominal type declaration or an extension
/// declaration.
typedef llvm::PointerUnion<NominalTypeDecl *, ExtensionDecl *>
  TypeOrExtensionDecl;

/// An entry in the protocol conformance map.
///
/// The pointer is the actual conformance providing the witnesses used to
/// provide conformance. The Boolean indicates whether the type explicitly
/// conforms to the protocol. A non-null conformance with a false Bool occurs
/// when error recovery has suggested implicit conformance.
typedef llvm::PointerIntPair<ProtocolConformance *, 1, bool> ConformanceEntry;

/// ASTContext - This object creates and owns the AST objects.
class ASTContext {
  ASTContext(const ASTContext&) = delete;
  void operator=(const ASTContext&) = delete;

public:
  // Members that should only be used by ASTContext.cpp.
  struct Implementation;
  Implementation &Impl;
  
  friend class ConstraintCheckerArenaRAII;
public:
  ASTContext(LangOptions &langOpts, SourceManager &SourceMgr,
             DiagnosticEngine &Diags);
  ~ASTContext();

  /// \brief The language options used for translation.
  LangOptions &LangOpts;

  /// \brief The source manager object.
  SourceManager &SourceMgr;

  /// Diags - The diagnostics engine.
  DiagnosticEngine &Diags;

  /// LoadedModules - The set of modules we have loaded.
  llvm::StringMap<Module*> LoadedModules;

  /// The builtin module.
  Module * const TheBuiltinModule;

  /// The standard library module.
  mutable Module *TheStdlibModule = nullptr;

  /// The name of the standard library module "swift".
  Identifier StdlibModuleName;

  /// The identifier "self".
  Identifier SelfIdentifier;

  /// ImportSearchPaths - The paths to search for imports in.
  std::vector<std::string> ImportSearchPaths;

  // FIXME: Once DenseMap learns about move semantics, use std::unique_ptr
  // and remove the explicit delete loop in the destructor.
  typedef llvm::DenseMap<std::pair<CanType, ProtocolDecl *>, 
                         ConformanceEntry> ConformsToMap;
  
  /// ConformsTo - Caches the results of checking whether a given (canonical)
  /// type conforms to a given protocol.
  ConformsToMap ConformsTo;

  /// \brief The list of external definitions imported by this context.
  llvm::SetVector<Decl *> ExternalDefinitions;

  /// FIXME: HACK HACK HACK
  /// This state should be tracked somewhere else.
  unsigned LastCheckedExternalDefinition = 0;

  /// A consumer of type checker debug output.
  std::unique_ptr<TypeCheckerDebugConsumer> TypeCheckerDebug;

  /// Associates a conforming decl to its protocol conformance decls.
  llvm::DenseMap<ValueDecl *, llvm::TinyPtrVector<ValueDecl *>>
     ConformingDeclMap;

  /// Cache for names of canonical GenericTypeParamTypes.
  mutable llvm::DenseMap<unsigned, Identifier>
    CanonicalGenericTypeParamTypeNames;
  
private:
  /// \brief The current generation number, which reflects the number of
  /// times that external modules have been loaded.
  ///
  /// Various places in the AST, such as the set of extensions associated with
  /// a nominal type, keep track of the generation number they saw and will
  /// automatically update when they are out of date.
  unsigned CurrentGeneration = 0;

public:
  /// \brief Retrieve the allocator for the given arena.
  llvm::BumpPtrAllocator &
  getAllocator(AllocationArena arena = AllocationArena::Permanent) const;

  /// Allocate - Allocate memory from the ASTContext bump pointer.
  void *Allocate(unsigned long bytes, unsigned alignment,
                 AllocationArena arena = AllocationArena::Permanent) const {
    if (LangOpts.UseMalloc)
      return AlignedAlloc(bytes, alignment);
    
    return getAllocator(arena).Allocate(bytes, alignment);
  }


  template <typename T>
  T *Allocate(unsigned numElts,
              AllocationArena arena = AllocationArena::Permanent) const {
    T *res = (T*)Allocate(sizeof(T)*numElts, __alignof__(T), arena);
    for (unsigned i = 0; i != numElts; ++i)
      new (res+i) T();
    return res;
  }

  /// Allocate a copy of the specified object.
  template <typename T>
  typename std::remove_reference<T>::type *AllocateObjectCopy(T &&t,
              AllocationArena arena = AllocationArena::Permanent) const {
    // This function can not be named AllocateCopy because it would always win
    // overload resolution over the AllocateCopy(ArrayRef<T>).
    using TNoRef = typename std::remove_reference<T>::type;
    TNoRef *res = (TNoRef*)Allocate(sizeof(TNoRef), __alignof__(TNoRef), arena);
    new (res) TNoRef(std::forward<T>(t));
    return res;
  }

  template <typename T, typename It>
  T *AllocateCopy(It start, It end,
                  AllocationArena arena = AllocationArena::Permanent) const {
    T *res = (T*)Allocate(sizeof(T)*(end-start), __alignof__(T), arena);
    for (unsigned i = 0; start != end; ++start, ++i)
      new (res+i) T(*start);
    return res;
  }

  template<typename T, size_t N>
  MutableArrayRef<T> AllocateCopy(
      T (&array)[N],
      AllocationArena arena = AllocationArena::Permanent) const {
    return MutableArrayRef<T>(AllocateCopy<T>(array, array+N, arena), N);
  }

  template<typename T>
  MutableArrayRef<T> AllocateCopy(
      ArrayRef<T> array,
      AllocationArena arena = AllocationArena::Permanent) const {
    return MutableArrayRef<T>(AllocateCopy<T>(array.begin(),array.end(), arena),
                              array.size());
  }


  template<typename T>
  ArrayRef<T> AllocateCopy(
      const SmallVectorImpl<T> &vec,
      AllocationArena arena = AllocationArena::Permanent) const {
    return AllocateCopy(ArrayRef<T>(vec), arena);
  }

  template<typename T>
  MutableArrayRef<T>
  AllocateCopy(SmallVectorImpl<T> &vec,
               AllocationArena arena = AllocationArena::Permanent) const {
    return AllocateCopy(MutableArrayRef<T>(vec), arena);
  }

  StringRef AllocateCopy(
      StringRef Str,
      AllocationArena arena = AllocationArena::Permanent) const {
    ArrayRef<char> Result =
        AllocateCopy(llvm::makeArrayRef(Str.data(), Str.size()), arena);
    return StringRef(Result.data(), Result.size());
  }

  template<typename T, typename Vector, typename Set>
  MutableArrayRef<T>
  AllocateCopy(llvm::SetVector<T, Vector, Set> setVector,
               AllocationArena arena = AllocationArena::Permanent) const {
    return MutableArrayRef<T>(AllocateCopy<T>(setVector.begin(),
                                              setVector.end(),
                                              arena),
                              setVector.size());
  }

  /// getIdentifier - Return the uniqued and AST-Context-owned version of the
  /// specified string.
  Identifier getIdentifier(StringRef Str) const;

  /// Retrieve the declaration of swift.Slice<T>.
  NominalTypeDecl *getSliceDecl() const;

  /// Retrieve the declaration of swift.Optional<T>.
  NominalTypeDecl *getOptionalDecl() const;

  /// Retrieve the declaration of swift._doesOptionalHaveValue.
  FuncDecl *getDoesOptionalHaveValueDecl(LazyResolver *resolver) const;

  /// Retrieve the declaration of swift._getOptionalValue.
  FuncDecl *getGetOptionalValueDecl(LazyResolver *resolver) const;

  /// Retrieve the declaration of swift._injectValueIntoOptional.
  FuncDecl *getInjectValueIntoOptionalDecl(LazyResolver *resolver) const;

  /// Retrieve the declaration of swift._injectNothingIntoOptional.
  FuncDecl *getInjectNothingIntoOptionalDecl(LazyResolver *resolver) const;

  /// Check whether the standary library provides all the correct
  /// intrinsic support for Optional<T>.
  ///
  /// If this is true, the four methods above all promise to return
  /// non-null.
  bool hasOptionalIntrinsics(LazyResolver *resolver) const;

  /// Retrieve the declaration of swift._getBool.
  FuncDecl *getGetBoolDecl(LazyResolver *resolver) const;

  /// \brief Look for the declaration with the given name within the
  /// swift module.
  void lookupInSwiftModule(StringRef name,
                           SmallVectorImpl<ValueDecl *> &results) const;

  /// Retrieve a specific, known protocol.
  ProtocolDecl *getProtocol(KnownProtocolKind kind) const;

  /// \brief Add a new mutation listener to this AST context.
  ///
  /// Mutation listeners will receive events when the AST is updated, e.g.,
  /// due to the module importer.
  void addMutationListener(ASTMutationListener &listener);

  /// \brief Remove the given mutation listener from this AST context.
  void removeMutationListener(ASTMutationListener &listener);

  /// \brief Notify all of the mutation listeners that the given declaration
  /// was just added.
  void addedExternalDecl(Decl *decl);

  /// Add a cleanup function to be called when the ASTContext is deallocated.
  void addCleanup(std::function<void(void)> cleanup);

  //===--------------------------------------------------------------------===//
  // Diagnostics Helper functions
  //===--------------------------------------------------------------------===//

  bool hadError() const;
  
  //===--------------------------------------------------------------------===//
  // Type manipulation routines.
  //===--------------------------------------------------------------------===//

  // Builtin type and simple types that are used frequently.
  const CanType TheErrorType;       /// TheErrorType - This is the error singleton.
  const CanType TheEmptyTupleType;  /// TheEmptyTupleType - This is "()"
  const CanType TheObjectPointerType; /// Builtin.ObjectPointer
  const CanType TheObjCPointerType; /// Builtin.ObjCPointer
  const CanType TheRawPointerType;  /// Builtin.RawPointer
  
  const CanType TheIEEE32Type;     /// TheIEEE32Type  - 32-bit IEEE floating point
  const CanType TheIEEE64Type;     /// TheIEEE64Type  - 64-bit IEEE floating point
  
  // Target specific types.
  const CanType TheIEEE16Type;     /// TheIEEE16Type  - 16-bit IEEE floating point
  const CanType TheIEEE80Type;     /// TheIEEE80Type  - 80-bit IEEE floating point
  const CanType TheIEEE128Type;    /// TheIEEE128Type - 128-bit IEEE floating point
  const CanType ThePPC128Type;     /// ThePPC128Type  - 128-bit PowerPC 2xDouble

  /// \brief Adds a module loader to this AST context.
  ///
  /// \param loader The new module loader, which will be added after any
  ///               existing module loaders.
  /// \param isClang \c true if this module loader is responsible for loading
  ///                Clang modules, which are special-cased in some parts of the
  ///                compiler.
  void addModuleLoader(llvm::IntrusiveRefCntPtr<ModuleLoader> loader,
                       bool isClang = false);

  /// \brief Load extensions to the given nominal type from the external
  /// module loaders.
  ///
  /// \param nominal The nominal type whose extensions should be loaded.
  ///
  /// \param previousGeneration The previous generation number. The AST already
  /// contains extensions loaded from any generation up to and including this
  /// one.
  void loadExtensions(NominalTypeDecl *nominal, unsigned previousGeneration);

  /// \brief Retrieve the Clang module loader for this ASTContext.
  ///
  /// If there is no Clang module loader, returns a null smart pointer.
  llvm::IntrusiveRefCntPtr<ModuleLoader> getClangModuleLoader() const;

  /// \returns a module with a given name that was already loaded.  If the
  /// module was not loaded, returns nullptr.
  Module *getLoadedModule(
      ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath) const;

  Module *getLoadedModule(Identifier ModuleName) const;

  /// \brief Attempts to load a module into this ASTContext.
  ///
  /// If a module by this name has already been loaded, the existing module will
  /// be returned.
  ///
  /// \returns The requested module, or NULL if the module cannot be found.
  Module *getModule(ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath);

  Module *getStdlibModule() const;

  /// \brief Retrieve the current generation number, which reflects the
  /// number of times a module import has caused mass invalidation of
  /// lookup tables.
  ///
  /// Various places in the AST keep track of the generation numbers at which
  /// their own information is valid, such as the list of extensions associated
  /// with a nominal type.
  unsigned getCurrentGeneration() const { return CurrentGeneration; }

  /// \brief Increase the generation number, implying that various lookup
  /// tables have been significantly altered by the introduction of a new
  /// module import.
  ///
  /// \returns the previous generation number.
  unsigned bumpGeneration() { return CurrentGeneration++; }

  /// \brief Record that the given nominal type or extension thereof conforms
  /// to the given compiler-known protocol.
  void recordConformance(KnownProtocolKind kind, Decl *decl);

  /// \brief Retrieve the set of nominal types and extensions thereof that
  /// conform to the given compiler-known protocol.
  ArrayRef<Decl *> getTypesThatConformTo(KnownProtocolKind kind);

  /// \brief Produce a "normal" conformance for a nominal type.
  NormalProtocolConformance *
  getConformance(Type conformingType,
                 ProtocolDecl *protocol,
                 Module *containingModule,
                 WitnessMap &&witnesses,
                 TypeWitnessMap &&typeWitnesses,
                 InheritedConformanceMap &&inheritedConformances,
                 ArrayRef<ValueDecl *> defaultedDefinitions);

  /// \brief Produce a specialized conformance, which takes a generic
  /// conformance and substitutes
  ///
  /// \param type The type for which we are retrieving the conformance.
  ///
  /// \param generic The generic conformance.
  ///
  /// \param substitutions The set of substitutions required to produce the
  /// specialized conformance from the generic conformance.
  ///
  /// \param typeWitnesses The set of type witnesses used by the specialized
  /// conformance.
  SpecializedProtocolConformance *
  getSpecializedConformance(Type type,
                            ProtocolConformance *generic,
                            ArrayRef<Substitution> substitutions,
                            TypeWitnessMap &&typeWitnesses);

  /// \brief Produce an inherited conformance, for subclasses of a type
  /// that already conforms to a protocol.
  ///
  /// \param type The type for which we are retrieving the conformance.
  ///
  /// \param inherited The inherited conformance.
  InheritedProtocolConformance *
  getInheritedConformance(Type type, ProtocolConformance *inherited);

  /// \brief Create trivial substitutions for the given bound generic type.
  Optional<ArrayRef<Substitution>>
  createTrivialSubstitutions(BoundGenericType *BGT) const;

  /// Record compiler-known protocol information in the AST.
  void recordKnownProtocols(Module *Stdlib);

  /// Associates a conforming decl to its protocol requirement decl.
  void recordConformingDecl(ValueDecl *ConformingD, ValueDecl *ConformanceD);

  /// Returns the protocol requirement decls for a conforming decl.
  ArrayRef<ValueDecl *> getConformances(ValueDecl *D);

private:
  friend class Decl;
  ClangNode getClangNode(const Decl *decl);
  void setClangNode(const Decl *decl, ClangNode node);

  friend class BoundGenericType;

  /// \brief Retrieve the substitutions for a bound generic type, if known.
  Optional<ArrayRef<Substitution>>
  getSubstitutions(BoundGenericType *Bound) const;

  /// \brief Set the substitutions for the given bound generic type.
  void setSubstitutions(BoundGenericType *Bound,
                        ArrayRef<Substitution> Subs) const;
};
  
} // end namespace swift

#endif
