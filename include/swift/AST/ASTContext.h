//===--- ASTContext.h - AST Context Object ----------------------*- C++ -*-===//
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
// This file defines the ASTContext interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTCONTEXT_H
#define SWIFT_AST_ASTCONTEXT_H

#include "llvm/Support/DataTypes.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/AST/SubstitutionList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Malloc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Allocator.h"
#include <functional>
#include <memory>
#include <utility>
#include <vector>

namespace clang {
  class Decl;
  class MacroInfo;
  class Module;
  class ObjCInterfaceDecl;
}

namespace swift {
  class ASTContext;
  enum class Associativity : unsigned char;
  class BoundGenericType;
  class ClangNode;
  class Decl;
  class DeclContext;
  class DefaultArgumentInitializer;
  class ExtensionDecl;
  class ForeignRepresentationInfo;
  class FuncDecl;
  class InFlightDiagnostic;
  class IterableDeclContext;
  class LazyAbstractFunctionData;
  class LazyGenericTypeData;
  class LazyContextData;
  class LazyMemberLoader;
  class LazyIterableDeclContextData;
  class LazyResolver;
  class PatternBindingDecl;
  class PatternBindingInitializer;
  class SourceFile;
  class SourceLoc;
  class Type;
  class TypeVariableType;
  class TupleType;
  class FunctionType;
  class GenericSignatureBuilder;
  class ArchetypeType;
  class Identifier;
  class InheritedNameSet;
  class ModuleDecl;
  class ModuleLoader;
  class NominalTypeDecl;
  class NormalProtocolConformance;
  class InheritedProtocolConformance;
  class SpecializedProtocolConformance;
  enum class ProtocolConformanceState;
  class Pattern;
  enum PointerTypeKind : unsigned;
  class PrecedenceGroupDecl;
  class TupleTypeElt;
  class EnumElementDecl;
  enum OptionalTypeKind : unsigned;
  class ProtocolDecl;
  class SubstitutableType;
  class SourceManager;
  class ValueDecl;
  class DiagnosticEngine;
  class Substitution;
  class TypeCheckerDebugConsumer;
  struct RawComment;
  class DocComment;
  class SILBoxType;
  class TypeAliasDecl;
  class VarDecl;

  enum class KnownProtocolKind : uint8_t;

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

/// Lists the set of "known" Foundation entities that are used in the
/// compiler.
///
/// While the names of Foundation types aren't likely to change in
/// Objective-C, their mapping into Swift can. Therefore, when
/// referring to names of Foundation entities in Swift, use this enum
/// and \c ASTContext::getSwiftName or \c ASTContext::getSwiftId.
enum class KnownFoundationEntity {
#define FOUNDATION_ENTITY(Name) Name,
#include "swift/AST/KnownFoundationEntities.def"
};

/// Retrieve the Foundation entity kind for the given Objective-C
/// entity name.
Optional<KnownFoundationEntity> getKnownFoundationEntity(StringRef name);

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

class SILLayout; // From SIL

/// \brief Describes either a nominal type declaration or an extension
/// declaration.
typedef llvm::PointerUnion<NominalTypeDecl *, ExtensionDecl *>
  TypeOrExtensionDecl;

/// ASTContext - This object creates and owns the AST objects.
class ASTContext {
  ASTContext(const ASTContext&) = delete;
  void operator=(const ASTContext&) = delete;

public:
  // Members that should only be used by ASTContext.cpp.
  struct Implementation;
  Implementation &Impl;
  
  friend ConstraintCheckerArenaRAII;
public:
  ASTContext(LangOptions &langOpts, SearchPathOptions &SearchPathOpts,
             SourceManager &SourceMgr, DiagnosticEngine &Diags);
  ~ASTContext();

  /// \brief The language options used for translation.
  LangOptions &LangOpts;

  /// \brief The search path options used by this AST context.
  SearchPathOptions &SearchPathOpts;

  /// \brief The source manager object.
  SourceManager &SourceMgr;

  /// Diags - The diagnostics engine.
  DiagnosticEngine &Diags;

  /// The set of top-level modules we have loaded.
  /// This map is used for iteration, therefore it's a MapVector and not a
  /// DenseMap.
  llvm::MapVector<Identifier, ModuleDecl*> LoadedModules;

  /// The builtin module.
  ModuleDecl * const TheBuiltinModule;

  /// The standard library module.
  mutable ModuleDecl *TheStdlibModule = nullptr;

  /// The name of the standard library module "Swift".
  Identifier StdlibModuleName;

  /// The name of the SwiftShims module "SwiftShims".
  Identifier SwiftShimsModuleName;

  // Define the set of known identifiers.
#define IDENTIFIER_WITH_NAME(Name, IdStr) Identifier Id_##Name;
#include "swift/AST/KnownIdentifiers.def"

  /// \brief The list of external definitions imported by this context.
  llvm::SetVector<Decl *> ExternalDefinitions;

  /// FIXME: HACK HACK HACK
  /// This state should be tracked somewhere else.
  unsigned LastCheckedExternalDefinition = 0;

  /// A consumer of type checker debug output.
  std::unique_ptr<TypeCheckerDebugConsumer> TypeCheckerDebug;

  /// Cache for names of canonical GenericTypeParamTypes.
  mutable llvm::DenseMap<unsigned, Identifier>
    CanonicalGenericTypeParamTypeNames;

  /// Cache of remapped types (useful for diagnostics).
  llvm::StringMap<Type> RemappedTypes;

private:
  /// \brief The current generation number, which reflects the number of
  /// times that external modules have been loaded.
  ///
  /// Various places in the AST, such as the set of extensions associated with
  /// a nominal type, keep track of the generation number they saw and will
  /// automatically update when they are out of date.
  unsigned CurrentGeneration = 0;

  friend class Pattern;

  /// Mapping from patterns that store interface types that will be lazily
  /// resolved to contextual types, to the declaration context in which the
  /// pattern resides.
  llvm::DenseMap<const Pattern *, DeclContext *>
    DelayedPatternContexts;

public:
  /// \brief Retrieve the allocator for the given arena.
  llvm::BumpPtrAllocator &
  getAllocator(AllocationArena arena = AllocationArena::Permanent) const;

  /// Allocate - Allocate memory from the ASTContext bump pointer.
  void *Allocate(unsigned long bytes, unsigned alignment,
                 AllocationArena arena = AllocationArena::Permanent) const {
    if (bytes == 0)
      return nullptr;

    if (LangOpts.UseMalloc)
      return AlignedAlloc(bytes, alignment);
    
    return getAllocator(arena).Allocate(bytes, alignment);
  }

  template <typename T>
  T *Allocate(AllocationArena arena = AllocationArena::Permanent) const {
    T *res = (T *) Allocate(sizeof(T), alignof(T), arena);
    new (res) T();
    return res;
  }

  template <typename T>
  MutableArrayRef<T> AllocateUninitialized(unsigned NumElts,
              AllocationArena Arena = AllocationArena::Permanent) const {
    T *Data = (T *) Allocate(sizeof(T) * NumElts, alignof(T), Arena);
    return { Data, NumElts };
  }

  template <typename T>
  MutableArrayRef<T> Allocate(unsigned numElts,
              AllocationArena arena = AllocationArena::Permanent) const {
    T *res = (T *) Allocate(sizeof(T) * numElts, alignof(T), arena);
    for (unsigned i = 0; i != numElts; ++i)
      new (res+i) T();
    return {res, numElts};
  }

  /// Allocate a copy of the specified object.
  template <typename T>
  typename std::remove_reference<T>::type *AllocateObjectCopy(T &&t,
              AllocationArena arena = AllocationArena::Permanent) const {
    // This function cannot be named AllocateCopy because it would always win
    // overload resolution over the AllocateCopy(ArrayRef<T>).
    using TNoRef = typename std::remove_reference<T>::type;
    TNoRef *res = (TNoRef *) Allocate(sizeof(TNoRef), alignof(TNoRef), arena);
    new (res) TNoRef(std::forward<T>(t));
    return res;
  }

  template <typename T, typename It>
  T *AllocateCopy(It start, It end,
                  AllocationArena arena = AllocationArena::Permanent) const {
    T *res = (T*)Allocate(sizeof(T)*(end-start), alignof(T), arena);
    for (unsigned i = 0; start != end; ++start, ++i)
      new (res+i) T(*start);
    return res;
  }

  template<typename T, size_t N>
  MutableArrayRef<T> AllocateCopy(T (&array)[N],
      AllocationArena arena = AllocationArena::Permanent) const {
    return MutableArrayRef<T>(AllocateCopy<T>(array, array+N, arena), N);
  }

  template<typename T>
  MutableArrayRef<T> AllocateCopy(ArrayRef<T> array,
      AllocationArena arena = AllocationArena::Permanent) const {
    return MutableArrayRef<T>(AllocateCopy<T>(array.begin(),array.end(), arena),
                              array.size());
  }


  template<typename T>
  ArrayRef<T> AllocateCopy(const SmallVectorImpl<T> &vec,
      AllocationArena arena = AllocationArena::Permanent) const {
    return AllocateCopy(ArrayRef<T>(vec), arena);
  }

  template<typename T>
  MutableArrayRef<T>
  AllocateCopy(SmallVectorImpl<T> &vec,
               AllocationArena arena = AllocationArena::Permanent) const {
    return AllocateCopy(MutableArrayRef<T>(vec), arena);
  }

  StringRef AllocateCopy(StringRef Str,
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

  /// Retrieve the lazy resolver for this context.
  LazyResolver *getLazyResolver() const;

  /// Set the lazy resolver for this context.
  void setLazyResolver(LazyResolver *resolver);

  /// getIdentifier - Return the uniqued and AST-Context-owned version of the
  /// specified string.
  Identifier getIdentifier(StringRef Str) const;

  /// Decide how to interpret two precedence groups.
  Associativity associateInfixOperators(PrecedenceGroupDecl *left,
                                        PrecedenceGroupDecl *right) const;

  /// Retrieve the declaration of Swift.Error.
  ProtocolDecl *getErrorDecl() const;
  CanType getExceptionType() const;
  
#define KNOWN_STDLIB_TYPE_DECL(NAME, DECL_CLASS, NUM_GENERIC_PARAMS) \
  /** Retrieve the declaration of Swift.NAME. */ \
  DECL_CLASS *get##NAME##Decl() const;
#include "swift/AST/KnownStdlibTypes.def"

  /// Retrieve the declaration of Swift.Optional or ImplicitlyUnwrappedOptional.
  EnumDecl *getOptionalDecl(OptionalTypeKind kind) const;

  /// Retrieve the declaration of Swift.Optional<T>.Some.
  EnumElementDecl *getOptionalSomeDecl() const;
  
  /// Retrieve the declaration of Swift.Optional<T>.None.
  EnumElementDecl *getOptionalNoneDecl() const;

  /// Retrieve the declaration of Swift.ImplicitlyUnwrappedOptional<T>.Some.
  EnumElementDecl *getImplicitlyUnwrappedOptionalSomeDecl() const;

  /// Retrieve the declaration of Swift.ImplicitlyUnwrappedOptional<T>.None.
  EnumElementDecl *getImplicitlyUnwrappedOptionalNoneDecl() const;

  EnumElementDecl *getOptionalSomeDecl(OptionalTypeKind kind) const;
  EnumElementDecl *getOptionalNoneDecl(OptionalTypeKind kind) const;

  /// Retrieve the declaration of the "pointee" property of a pointer type.
  VarDecl *getPointerPointeePropertyDecl(PointerTypeKind ptrKind) const;

  /// Retrieve the type Swift.Never.
  CanType getNeverType() const;

  /// Retrieve the declaration of Swift.Void.
  TypeAliasDecl *getVoidDecl() const;

  /// Retrieve the declaration of ObjectiveC.ObjCBool.
  StructDecl *getObjCBoolDecl() const;

  /// Retrieve the declaration of Foundation.NSError.
  ClassDecl *getNSErrorDecl() const;

  // Declare accessors for the known declarations.
#define FUNC_DECL(Name, Id) \
  FuncDecl *get##Name(LazyResolver *resolver) const;
#include "swift/AST/KnownDecls.def"

  /// Check whether the standard library provides all the correct
  /// intrinsic support for Optional<T>.
  ///
  /// If this is true, the four methods above all promise to return
  /// non-null.
  bool hasOptionalIntrinsics(LazyResolver *resolver) const;

  /// Check whether the standard library provides all the correct
  /// intrinsic support for UnsafeMutablePointer<T> function arguments.
  ///
  /// If this is true, the methods getConvert*ToPointerArgument
  /// all promise to return non-null.
  bool hasPointerArgumentIntrinsics(LazyResolver *resolver) const;

  /// Check whether the standard library provides all the correct
  /// intrinsic support for array literals.
  ///
  /// If this is true, the method getAllocateUninitializedArray
  /// promises to return non-null.
  bool hasArrayLiteralIntrinsics(LazyResolver *resolver) const;

  /// Retrieve the declaration of Swift._getBool.
  FuncDecl *getGetBoolDecl(LazyResolver *resolver) const;

  /// Retrieve the declaration of Swift.==(Int, Int) -> Bool.
  FuncDecl *getEqualIntDecl() const;
  
  /// Retrieve the declaration of Swift._unimplementedInitializer.
  FuncDecl *getUnimplementedInitializerDecl(LazyResolver *resolver) const;

  /// Retrieve the declaration of Swift._undefined.
  FuncDecl *getUndefinedDecl(LazyResolver *resolver) const;

  // Retrieve the declaration of Swift._stdlib_isOSVersionAtLeast.
  FuncDecl *getIsOSVersionAtLeastDecl(LazyResolver *resolver) const;
  
  /// Look for the declaration with the given name within the
  /// Swift module.
  void lookupInSwiftModule(StringRef name,
                           SmallVectorImpl<ValueDecl *> &results) const;

  /// Retrieve a specific, known protocol.
  ProtocolDecl *getProtocol(KnownProtocolKind kind) const;
  
  /// Determine whether the given nominal type is one of the standard
  /// library or Cocoa framework types that is known to be bridged by another
  /// module's overlay, for layering or implementation detail reasons.
  bool isTypeBridgedInExternalModule(NominalTypeDecl *nominal) const;

  /// Get the Objective-C type that a Swift type bridges to, if any.
  /// 
  /// \param dc The context in which bridging is occurring.
  /// \param type The Swift for which we are querying bridging behavior.
  /// \param bridgedValueType The specific value type that is bridged,
  /// which will usually by the same as \c type.
  Type getBridgedToObjC(const DeclContext *dc, Type type,
                        Type *bridgedValueType = nullptr) const;

  /// Determine whether the given Swift type is representable in a
  /// given foreign language.
  ForeignRepresentationInfo
  getForeignRepresentationInfo(NominalTypeDecl *nominal,
                               ForeignLanguage language,
                               const DeclContext *dc);

  /// Add a declaration to a list of declarations that need to be emitted
  /// as part of the current module or source file, but are otherwise not
  /// nested within it.
  void addExternalDecl(Decl *decl);

  /// Add a cleanup function to be called when the ASTContext is deallocated.
  void addCleanup(std::function<void(void)> cleanup);

  /// Add a cleanup to run the given object's destructor when the ASTContext is
  /// deallocated.
  template<typename T>
  void addDestructorCleanup(T &object) {
    addCleanup([&object]{ object.~T(); });
  }

  //===--------------------------------------------------------------------===//
  // Diagnostics Helper functions
  //===--------------------------------------------------------------------===//

  bool hadError() const;
  
  //===--------------------------------------------------------------------===//
  // Type manipulation routines.
  //===--------------------------------------------------------------------===//

  // Builtin type and simple types that are used frequently.
  const CanType TheErrorType;             /// This is the ErrorType singleton.
  const CanType TheUnresolvedType;        /// This is the UnresolvedType singleton.
  const CanType TheEmptyTupleType;        /// This is '()', aka Void
  const CanType TheAnyType;               /// This is 'Any', the empty protocol composition
  const CanType TheNativeObjectType;      /// Builtin.NativeObject
  const CanType TheBridgeObjectType;      /// Builtin.BridgeObject
  const CanType TheUnknownObjectType;     /// Builtin.UnknownObject
  const CanType TheRawPointerType;        /// Builtin.RawPointer
  const CanType TheUnsafeValueBufferType; /// Builtin.UnsafeValueBuffer
  
  const CanType TheIEEE32Type;            /// 32-bit IEEE floating point
  const CanType TheIEEE64Type;            /// 64-bit IEEE floating point
  
  // Target specific types.
  const CanType TheIEEE16Type;            /// 16-bit IEEE floating point
  const CanType TheIEEE80Type;            /// 80-bit IEEE floating point
  const CanType TheIEEE128Type;           /// 128-bit IEEE floating point
  const CanType ThePPC128Type;            /// 128-bit PowerPC 2xDouble
  
  /// Adds a search path to SearchPathOpts, unless it is already present.
  ///
  /// Does any proper bookkeeping to keep all module loaders up to date as well.
  void addSearchPath(StringRef searchPath, bool isFramework, bool isSystem);

  /// \brief Adds a module loader to this AST context.
  ///
  /// \param loader The new module loader, which will be added after any
  ///               existing module loaders.
  /// \param isClang \c true if this module loader is responsible for loading
  ///                Clang modules, which are special-cased in some parts of the
  ///                compiler.
  void addModuleLoader(std::unique_ptr<ModuleLoader> loader,
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

  /// \brief Load the methods within the given class that produce
  /// Objective-C class or instance methods with the given selector.
  ///
  /// \param classDecl The class in which we are searching for @objc methods.
  /// The search only considers this class and its extensions; not any
  /// superclasses.
  ///
  /// \param selector The selector to search for.
  ///
  /// \param isInstanceMethod Whether we are looking for an instance method
  /// (vs. a class method).
  ///
  /// \param previousGeneration The previous generation with which this
  /// callback was invoked. The list of methods will already contain all of
  /// the results from generations up and including \c previousGeneration.
  ///
  /// \param methods The list of @objc methods in this class that have this
  /// selector and are instance/class methods as requested. This list will be
  /// extended with any methods found in subsequent generations.
  void loadObjCMethods(ClassDecl *classDecl,
                       ObjCSelector selector,
                       bool isInstanceMethod,
                       unsigned previousGeneration,
                       llvm::TinyPtrVector<AbstractFunctionDecl *> &methods);

  /// \brief Retrieve the Clang module loader for this ASTContext.
  ///
  /// If there is no Clang module loader, returns a null pointer.
  /// The loader is owned by the AST context.
  ClangModuleLoader *getClangModuleLoader() const;

  /// Asks every module loader to verify the ASTs it has loaded.
  ///
  /// Does nothing in non-asserts (NDEBUG) builds.
  void verifyAllLoadedModules() const;

  /// \brief Check whether the module with a given name can be imported without
  /// importing it.
  ///
  /// Note that even if this check succeeds, errors may still occur if the
  /// module is loaded in full.
  bool canImportModule(std::pair<Identifier, SourceLoc> ModulePath);

  /// \returns a module with a given name that was already loaded.  If the
  /// module was not loaded, returns nullptr.
  ModuleDecl *getLoadedModule(
      ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath) const;

  ModuleDecl *getLoadedModule(Identifier ModuleName) const;

  /// \brief Attempts to load a module into this ASTContext.
  ///
  /// If a module by this name has already been loaded, the existing module will
  /// be returned.
  ///
  /// \returns The requested module, or NULL if the module cannot be found.
  ModuleDecl *getModule(ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath);

  ModuleDecl *getModuleByName(StringRef ModuleName);

  /// Returns the standard library module, or null if the library isn't present.
  ///
  /// If \p loadIfAbsent is true, the ASTContext will attempt to load the module
  /// if it hasn't been set yet.
  ModuleDecl *getStdlibModule(bool loadIfAbsent = false);

  ModuleDecl *getStdlibModule() const {
    return const_cast<ASTContext *>(this)->getStdlibModule(false);
  }

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

  /// \brief Produce a "normal" conformance for a nominal type.
  NormalProtocolConformance *
  getConformance(Type conformingType,
                 ProtocolDecl *protocol,
                 SourceLoc loc,
                 DeclContext *dc,
                 ProtocolConformanceState state);

  /// Produce a new normal conformance for a property behavior.
  NormalProtocolConformance *
  getBehaviorConformance(Type conformingType,
                         Type conformingInterfaceType,
                         ProtocolDecl *protocol,
                         SourceLoc loc,
                         AbstractStorageDecl *storage,
                         ProtocolConformanceState state);

  /// A callback used to produce a diagnostic for an ill-formed protocol
  /// conformance that was type-checked before we're actually walking the
  /// conformance itself, along with a bit indicating whether this diagnostic
  /// produces an error.
  struct DelayedConformanceDiag {
    ValueDecl *Requirement;
    std::function<void()> Callback;
    bool IsError;
  };

  /// Add a delayed diagnostic produced while type-checking a
  /// particular protocol conformance.
  void addDelayedConformanceDiag(NormalProtocolConformance *conformance,
                                 DelayedConformanceDiag fn);

  /// Retrieve the delayed-conformance diagnostic callbacks for the
  /// given normal protocol conformance.
  std::vector<DelayedConformanceDiag>
  takeDelayedConformanceDiags(NormalProtocolConformance *conformance);

  /// \brief Produce a specialized conformance, which takes a generic
  /// conformance and substitutes
  ///
  /// \param type The type for which we are retrieving the conformance.
  ///
  /// \param generic The generic conformance.
  ///
  /// \param substitutions The set of substitutions required to produce the
  /// specialized conformance from the generic conformance.
  SpecializedProtocolConformance *
  getSpecializedConformance(Type type,
                            ProtocolConformance *generic,
                            SubstitutionList substitutions);

  /// \brief Produce an inherited conformance, for subclasses of a type
  /// that already conforms to a protocol.
  ///
  /// \param type The type for which we are retrieving the conformance.
  ///
  /// \param inherited The inherited conformance.
  InheritedProtocolConformance *
  getInheritedConformance(Type type, ProtocolConformance *inherited);

  /// \brief Create trivial substitutions for the given bound generic type.
  Optional<SubstitutionList>
  createTrivialSubstitutions(BoundGenericType *BGT,
                             DeclContext *gpContext) const;

  /// Record compiler-known protocol information in the AST.
  void recordKnownProtocols(ModuleDecl *Stdlib);
  
  /// \brief Retrieve the substitutions for a bound generic type, if known.
  Optional<SubstitutionList>
  getSubstitutions(TypeBase *type, DeclContext *gpContext) const;

  /// Get the lazy data for the given declaration.
  ///
  /// \param lazyLoader If non-null, the lazy loader to use when creating the
  /// lazy data. The pointer must either be null or be consistent
  /// across all calls for the same \p func.
  LazyContextData *getOrCreateLazyContextData(const Decl *decl,
                                              LazyMemberLoader *lazyLoader);

  /// Get the lazy function data for the given generic type.
  ///
  /// \param lazyLoader If non-null, the lazy loader to use when creating the
  /// generic type data. The pointer must either be null or be consistent
  /// across all calls for the same \p type.
  LazyGenericTypeData *getOrCreateLazyGenericTypeData(
                                                  const GenericTypeDecl *type,
                                                  LazyMemberLoader *lazyLoader);

  /// Get the lazy function data for the given abstract function.
  ///
  /// \param lazyLoader If non-null, the lazy loader to use when creating the
  /// function data. The pointer must either be null or be consistent
  /// across all calls for the same \p func.
  LazyAbstractFunctionData *getOrCreateLazyFunctionContextData(
                                              const AbstractFunctionDecl *func,
                                              LazyMemberLoader *lazyLoader);

  /// Get the lazy iterable context for the given iterable declaration context.
  ///
  /// \param lazyLoader If non-null, the lazy loader to use when creating the
  /// iterable context data. The pointer must either be null or be consistent
  /// across all calls for the same \p idc.
  LazyIterableDeclContextData *getOrCreateLazyIterableContextData(
                                              const IterableDeclContext *idc,
                                              LazyMemberLoader *lazyLoader);

  /// \brief Returns memory usage of this ASTContext.
  size_t getTotalMemory() const;
  
  /// \brief Returns memory used exclusively by constraint solver.
  size_t getSolverMemory() const;

  /// Complain if @objc or dynamic is used without importing Foundation.
  void diagnoseAttrsRequiringFoundation(SourceFile &SF);

  /// Note that the given method produces an Objective-C method.
  void recordObjCMethod(AbstractFunctionDecl *method);

  /// Diagnose any Objective-C method overrides that aren't reflected
  /// as overrides in Swift.
  bool diagnoseUnintendedObjCMethodOverrides(SourceFile &sf);

  /// Note that there is a conflict between different definitions that
  /// produce the same Objective-C method.
  void recordObjCMethodConflict(ClassDecl *classDecl, ObjCSelector selector,
                                bool isInstance);

  /// Diagnose all conflicts between members that have the same
  /// Objective-C selector in the same class.
  ///
  /// \param sf The source file for which we are diagnosing conflicts.
  ///
  /// \returns true if there were any conflicts diagnosed.
  bool diagnoseObjCMethodConflicts(SourceFile &sf);

  /// Note that an optional @objc requirement has gone unsatisfied by
  /// a conformance to its protocol.
  ///
  /// \param dc The declaration context in which the conformance occurs.
  /// \param req The optional requirement.
  void recordObjCUnsatisfiedOptReq(DeclContext *dc, AbstractFunctionDecl *req);

  /// Diagnose any unsatisfied @objc optional requirements of
  /// protocols that conflict with methods.
  bool diagnoseObjCUnsatisfiedOptReqConflicts(SourceFile &sf);

  /// Retrieve the Swift name for the given Foundation entity, where
  /// "NS" prefix stripping will apply under omit-needless-words.
  StringRef getSwiftName(KnownFoundationEntity kind);

  /// Retrieve the Swift identifier for the given Foundation entity, where
  /// "NS" prefix stripping will apply under omit-needless-words.
  Identifier getSwiftId(KnownFoundationEntity kind) {
    return getIdentifier(getSwiftName(kind));
  }

  /// Collect visible clang modules from the ClangModuleLoader. These modules are
  /// not necessarily loaded.
  void getVisibleTopLevelClangModules(SmallVectorImpl<clang::Module*> &Modules) const;

  /// Retrieve or create the stored generic signature builder for the given
  /// canonical generic signature and module.
  GenericSignatureBuilder *getOrCreateGenericSignatureBuilder(CanGenericSignature sig,
                                                ModuleDecl *mod);

  /// Retrieve or create the canonical generic environment of a canonical
  /// generic signature builder.
  GenericEnvironment *getOrCreateCanonicalGenericEnvironment(
                                                     GenericSignatureBuilder *builder,
                                                     ModuleDecl &module);

  /// Retrieve the inherited name set for the given class.
  const InheritedNameSet *getAllPropertyNames(ClassDecl *classDecl,
                                              bool forInstance);

  /// Retrieve the inherited name set for the given Objective-C class.
  const InheritedNameSet *getAllPropertyNames(
                            clang::ObjCInterfaceDecl *classDecl,
                            bool forInstance);

  /// Retrieve a generic signature with a single unconstrained type parameter,
  /// like `<T>`.
  CanGenericSignature getSingleGenericParameterSignature() const;
  
  /// Whether our effective Swift version is in the Swift 3 family
  bool isSwiftVersion3() const { return LangOpts.isSwiftVersion3(); }

private:
  friend Decl;
  Optional<RawComment> getRawComment(const Decl *D);
  void setRawComment(const Decl *D, RawComment RC);

  Optional<StringRef> getBriefComment(const Decl *D);
  void setBriefComment(const Decl *D, StringRef Comment);

  friend TypeBase;

  /// \brief Set the substitutions for the given bound generic type.
  void setSubstitutions(TypeBase *type,
                        DeclContext *gpContext,
                        SubstitutionList Subs) const;

  friend ArchetypeType;

  /// Provide context-level uniquing for SIL lowered type layouts and boxes.
  friend SILLayout;
  friend SILBoxType;
};

/// Retrieve information about the given Objective-C method for
/// diagnostic purposes, to be used with OBJC_DIAG_SELECT in
/// DiagnosticsSema.def.
std::pair<unsigned, DeclName> getObjCMethodDiagInfo(
                                AbstractFunctionDecl *method);

/// Attach Fix-Its to the given diagnostic that updates the name of the
/// given declaration to the desired target name.
///
/// \returns false if the name could not be fixed.
bool fixDeclarationName(InFlightDiagnostic &diag, ValueDecl *decl,
                        DeclName targetName);

/// Fix the Objective-C name of the given declaration to match the provided
/// Objective-C selector.
///
/// \param ignoreImpliedName When true, ignore the implied name of the
/// given declaration, because it no longer applies.
///
/// For properties, the selector should be a zero-parameter selector of the
/// given property's name.
bool fixDeclarationObjCName(InFlightDiagnostic &diag, ValueDecl *decl,
                            Optional<ObjCSelector> targetNameOpt,
                            bool ignoreImpliedName = false);

} // end namespace swift

#endif
