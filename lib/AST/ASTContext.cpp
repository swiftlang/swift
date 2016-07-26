//===--- ASTContext.cpp - ASTContext Implementation -----------------------===//
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
//  This file implements the ASTContext class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "ForeignRepresentationInfo.h"
#include "swift/Strings.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/AST.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/RawComment.h"
#include "swift/AST/TypeCheckerDebugConsumer.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h" // bad dependency
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Support/Allocator.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSwitch.h"
#include <algorithm>
#include <memory>

using namespace swift;

LazyResolver::~LazyResolver() = default;
DelegatingLazyResolver::~DelegatingLazyResolver() = default;
void ModuleLoader::anchor() {}
void ClangModuleLoader::anchor() {}

llvm::StringRef swift::getProtocolName(KnownProtocolKind kind) {
  switch (kind) {
#define PROTOCOL_WITH_NAME(Id, Name) \
  case KnownProtocolKind::Id: \
    return Name;
#include "swift/AST/KnownProtocols.def"
  }
  llvm_unreachable("bad KnownProtocolKind");
}

namespace {
  typedef std::tuple<ClassDecl *, ObjCSelector, bool> ObjCMethodConflict;

  /// An unsatisfied, optional @objc requirement in a protocol conformance.
  typedef std::pair<DeclContext *, AbstractFunctionDecl *>
    ObjCUnsatisfiedOptReq;

  enum class SearchPathKind : uint8_t {
    Import = 1 << 0,
    Framework = 1 << 1
  };
}

using AssociativityCacheType =
  llvm::DenseMap<std::pair<PrecedenceGroupDecl *, PrecedenceGroupDecl *>,
                 Associativity>;

struct ASTContext::Implementation {
  Implementation();
  ~Implementation();

  llvm::BumpPtrAllocator Allocator; // used in later initializations

  /// The set of cleanups to be called when the ASTContext is destroyed.
  std::vector<std::function<void(void)>> Cleanups;

  /// The last resolver.
  LazyResolver *Resolver = nullptr;

  llvm::StringMap<char, llvm::BumpPtrAllocator&> IdentifierTable;

  /// The declaration of Swift.AssignmentPrecedence.
  PrecedenceGroupDecl *AssignmentPrecedence = nullptr;

  /// The declaration of Swift.CastingPrecedence.
  PrecedenceGroupDecl *CastingPrecedence = nullptr;

  /// The declaration of Swift.FunctionArrowPrecedence.
  PrecedenceGroupDecl *FunctionArrowPrecedence = nullptr;

  /// The declaration of Swift.TernaryPrecedence.
  PrecedenceGroupDecl *TernaryPrecedence = nullptr;

  /// The declaration of Swift.DefaultPrecedence.
  PrecedenceGroupDecl *DefaultPrecedence = nullptr;

  /// The declaration of Swift.Bool.
  NominalTypeDecl *BoolDecl = nullptr;

  /// The declaration of Swift.Int.
  NominalTypeDecl *IntDecl = nullptr;

  /// The declaration of Swift.UInt.
  NominalTypeDecl *UIntDecl = nullptr;

  /// The declaration of Swift.Float.
  NominalTypeDecl *FloatDecl = nullptr;

  /// The declaration of Swift.Double.
  NominalTypeDecl *DoubleDecl = nullptr;

  /// The declaration of Swift.String.
  NominalTypeDecl *StringDecl = nullptr;

  /// The declaration of Swift.Array<T>.
  NominalTypeDecl *ArrayDecl = nullptr;

  /// The declaration of Swift.Set<T>.
  NominalTypeDecl *SetDecl = nullptr;

  /// The declaration of Swift.Sequence<T>.
  NominalTypeDecl *SequenceDecl = nullptr;

  /// The declaration of Swift.Dictionary<T>.
  NominalTypeDecl *DictionaryDecl = nullptr;

  /// The declaration of Swift.Optional<T>.
  EnumDecl *OptionalDecl = nullptr;

  /// The declaration of Swift.Optional<T>.Some.
  EnumElementDecl *OptionalSomeDecl = nullptr;

  /// The declaration of Swift.Optional<T>.None.
  EnumElementDecl *OptionalNoneDecl = nullptr;

  /// The declaration of Swift.OptionSet.
  NominalTypeDecl *OptionSetDecl = nullptr;

  /// The declaration of Swift.ImplicitlyUnwrappedOptional<T>.Some.
  EnumElementDecl *ImplicitlyUnwrappedOptionalSomeDecl = nullptr;

  /// The declaration of Swift.ImplicitlyUnwrappedOptional<T>.None.
  EnumElementDecl *ImplicitlyUnwrappedOptionalNoneDecl = nullptr;
  
  /// The declaration of Swift.UnsafeMutableRawPointer.
  NominalTypeDecl *UnsafeMutableRawPointerDecl = nullptr;
  VarDecl *UnsafeMutableRawPointerMemoryDecl = nullptr;

  /// The declaration of Swift.UnsafeRawPointer.
  NominalTypeDecl *UnsafeRawPointerDecl = nullptr;
  VarDecl *UnsafeRawPointerMemoryDecl = nullptr;

  /// The declaration of Swift.UnsafeMutablePointer<T>.
  NominalTypeDecl *UnsafeMutablePointerDecl = nullptr;
  VarDecl *UnsafeMutablePointerMemoryDecl = nullptr;
  
  /// The declaration of Swift.UnsafePointer<T>.
  NominalTypeDecl *UnsafePointerDecl = nullptr;
  VarDecl *UnsafePointerMemoryDecl = nullptr;
  
  /// The declaration of Swift.OpaquePointer.
  NominalTypeDecl *OpaquePointerDecl = nullptr;
  
  /// The declaration of Swift.AutoreleasingUnsafeMutablePointer<T>.
  NominalTypeDecl *AutoreleasingUnsafeMutablePointerDecl = nullptr;
  VarDecl *AutoreleasingUnsafeMutablePointerMemoryDecl = nullptr;

  /// The declaration of Swift.Unmanaged<T>
  NominalTypeDecl *UnmanagedDecl = nullptr;

  /// The declaration of Swift.Never.
  NominalTypeDecl *NeverDecl = nullptr;

  /// The declaration of Swift.Void.
  TypeAliasDecl *VoidDecl = nullptr;

  /// The declaration of ObjectiveC.ObjCBool.
  StructDecl *ObjCBoolDecl = nullptr;

  /// The declaration of Foundation.NSError.
  ClassDecl *NSErrorDecl = nullptr;

  // Declare cached declarations for each of the known declarations.
#define FUNC_DECL(Name, Id) FuncDecl *Get##Name = nullptr;
#include "swift/AST/KnownDecls.def"
  
  /// The declaration of Swift.ImplicitlyUnwrappedOptional<T>.
  EnumDecl *ImplicitlyUnwrappedOptionalDecl = nullptr;

  /// func _getBool(Builtin.Int1) -> Bool
  FuncDecl *GetBoolDecl = nullptr;
  
  /// func ==(Int, Int) -> Bool
  FuncDecl *EqualIntDecl = nullptr;

  /// func _unimplementedInitializer(className: StaticString).
  FuncDecl *UnimplementedInitializerDecl = nullptr;

  /// func _undefined<T>(msg: StaticString, file: StaticString, line: UInt) -> T
  FuncDecl *UndefinedDecl = nullptr;

  /// func _stdlib_isOSVersionAtLeast(Builtin.Word,Builtin.Word, Builtin.word)
  //    -> Builtin.Int1
  FuncDecl *IsOSVersionAtLeastDecl = nullptr;
  
  /// \brief The set of known protocols, lazily populated as needed.
  ProtocolDecl *KnownProtocols[NumKnownProtocols] = { };

  /// \brief The various module loaders that import external modules into this
  /// ASTContext.
  SmallVector<std::unique_ptr<swift::ModuleLoader>, 4> ModuleLoaders;

  /// \brief The module loader used to load Clang modules.
  ClangModuleLoader *TheClangModuleLoader = nullptr;

  /// \brief Map from Swift declarations to raw comments.
  llvm::DenseMap<const Decl *, RawComment> RawComments;

  /// \brief Map from Swift declarations to brief comments.
  llvm::DenseMap<const Decl *, StringRef> BriefComments;

  /// \brief Map from local declarations to their discriminators.
  /// Missing entries implicitly have value 0.
  llvm::DenseMap<const ValueDecl *, unsigned> LocalDiscriminators;

  /// \brief Map from declarations to foreign error conventions.
  /// This applies to both actual imported functions and to @objc functions.
  llvm::DenseMap<const AbstractFunctionDecl *,
                 ForeignErrorConvention> ForeignErrorConventions;

  /// Cache of previously looked-up precedence queries.
  AssociativityCacheType AssociativityCache;

  /// Map from normal protocol conformances to diagnostics that have
  /// been delayed until the conformance is fully checked.
  llvm::DenseMap<NormalProtocolConformance *,
                 std::vector<ASTContext::DelayedConformanceDiag>>
    DelayedConformanceDiags;

  /// Conformance loaders for declarations that have them.
  llvm::DenseMap<Decl *, std::pair<LazyMemberLoader *, uint64_t>>
    ConformanceLoaders;

  /// \brief A cached unused pattern-binding initializer context.
  PatternBindingInitializer *UnusedPatternBindingContext = nullptr;

  /// \brief A cached unused default-argument initializer context.
  DefaultArgumentInitializer *UnusedDefaultArgumentContext = nullptr;

  /// Mapping from archetypes with lazily-resolved nested types to the
  /// archetype builder and potential archetype corresponding to that
  /// archetype.
  llvm::DenseMap<const ArchetypeType *, 
                 std::pair<ArchetypeBuilder *,
                           ArchetypeBuilder::PotentialArchetype *>>
    LazyArchetypes;

  /// \brief Stored archetype builders.
  llvm::DenseMap<std::pair<GenericSignature *, ModuleDecl *>,
                 std::unique_ptr<ArchetypeBuilder>> ArchetypeBuilders;

  /// The set of property names that show up in the defining module of a
  /// class.
  llvm::DenseMap<std::pair<const ClassDecl *, char>,
                 std::unique_ptr<InheritedNameSet>> AllProperties;

  /// The set of property names that show up in the defining module of
  /// an Objective-C class.
  llvm::DenseMap<std::pair<const clang::ObjCInterfaceDecl *, char>,
                 std::unique_ptr<InheritedNameSet>> AllPropertiesObjC;

  /// \brief Structure that captures data that is segregated into different
  /// arenas.
  struct Arena {
    llvm::FoldingSet<TupleType> TupleTypes;
    llvm::DenseMap<std::pair<Type,char>, MetatypeType*> MetatypeTypes;
    llvm::DenseMap<std::pair<Type,char>,
                   ExistentialMetatypeType*> ExistentialMetatypeTypes;
    llvm::DenseMap<std::pair<Type,std::pair<Type,unsigned>>, FunctionType*>
      FunctionTypes;
    llvm::DenseMap<Type, ArraySliceType*> ArraySliceTypes;
    llvm::DenseMap<std::pair<Type, Type>, DictionaryType *> DictionaryTypes;
    llvm::DenseMap<Type, OptionalType*> OptionalTypes;
    llvm::DenseMap<Type, ImplicitlyUnwrappedOptionalType*> ImplicitlyUnwrappedOptionalTypes;
    llvm::DenseMap<Type, ParenType*> ParenTypes;
    llvm::DenseMap<uintptr_t, ReferenceStorageType*> ReferenceStorageTypes;
    llvm::DenseMap<Type, LValueType*> LValueTypes;
    llvm::DenseMap<Type, InOutType*> InOutTypes;
    llvm::DenseMap<std::pair<Type, Type>, SubstitutedType *> SubstitutedTypes;
    llvm::DenseMap<std::pair<Type, void*>, DependentMemberType *>
      DependentMemberTypes;
    llvm::DenseMap<Type, DynamicSelfType *> DynamicSelfTypes;
    llvm::FoldingSet<EnumType> EnumTypes;
    llvm::FoldingSet<StructType> StructTypes;
    llvm::FoldingSet<ClassType> ClassTypes;
    llvm::FoldingSet<UnboundGenericType> UnboundGenericTypes;
    llvm::FoldingSet<BoundGenericType> BoundGenericTypes;
    llvm::FoldingSet<ProtocolType> ProtocolTypes;

    llvm::DenseMap<std::pair<TypeBase *, DeclContext *>,
                   ArrayRef<Substitution>>
      BoundGenericSubstitutions;

    /// The set of normal protocol conformances.
    llvm::FoldingSet<NormalProtocolConformance> NormalConformances;

    /// The set of specialized protocol conformances.
    llvm::FoldingSet<SpecializedProtocolConformance> SpecializedConformances;

    /// The set of inherited protocol conformances.
    llvm::FoldingSet<InheritedProtocolConformance> InheritedConformances;

    ~Arena() {
      for (auto &conformance : SpecializedConformances)
        conformance.~SpecializedProtocolConformance();
      for (auto &conformance : InheritedConformances)
        conformance.~InheritedProtocolConformance();

      // Call the normal conformance destructors last since they could be
      // referenced by the other conformance types.
      for (auto &conformance : NormalConformances)
        conformance.~NormalProtocolConformance();
    }

    size_t getTotalMemory() const;
  };

  llvm::DenseMap<Module*, ModuleType*> ModuleTypes;
  llvm::DenseMap<std::pair<unsigned, unsigned>, GenericTypeParamType *>
    GenericParamTypes;
  llvm::FoldingSet<GenericFunctionType> GenericFunctionTypes;
  llvm::FoldingSet<SILFunctionType> SILFunctionTypes;
  llvm::DenseMap<CanType, SILBlockStorageType *> SILBlockStorageTypes;
  llvm::DenseMap<CanType, SILBoxType *> SILBoxTypes;
  llvm::DenseMap<BuiltinIntegerWidth, BuiltinIntegerType*> IntegerTypes;
  llvm::FoldingSet<ProtocolCompositionType> ProtocolCompositionTypes;
  llvm::FoldingSet<BuiltinVectorType> BuiltinVectorTypes;
  llvm::FoldingSet<GenericSignature> GenericSignatures;
  llvm::FoldingSet<DeclName::CompoundDeclName> CompoundNames;
  llvm::DenseMap<UUID, ArchetypeType *> OpenedExistentialArchetypes;

  /// List of Objective-C member conflicts we have found during type checking.
  std::vector<ObjCMethodConflict> ObjCMethodConflicts;

  /// List of optional @objc protocol requirements that have gone
  /// unsatisfied, which might conflict with other Objective-C methods.
  std::vector<ObjCUnsatisfiedOptReq> ObjCUnsatisfiedOptReqs;

  /// List of Objective-C methods created by the type checker (and not
  /// by the Clang importer or deserialized), which is used for
  /// checking unintended Objective-C overrides.
  std::vector<AbstractFunctionDecl *> ObjCMethods;

  /// A cache of information about whether particular nominal types
  /// are representable in a foreign language.
  llvm::DenseMap<NominalTypeDecl *, ForeignRepresentationInfo>
    ForeignRepresentableCache;

  llvm::StringMap<OptionSet<SearchPathKind>> SearchPathsSet;

  /// \brief The permanent arena.
  Arena Permanent;

  /// Temporary arena used for a constraint solver.
  struct ConstraintSolverArena : public Arena {
    /// The allocator used for all allocations within this arena.
    llvm::BumpPtrAllocator &Allocator;

    /// Callback used to get a type member of a type variable.
    GetTypeVariableMemberCallback GetTypeMember;

    ConstraintSolverArena(llvm::BumpPtrAllocator &allocator,
                          GetTypeVariableMemberCallback &&getTypeMember)
      : Allocator(allocator), GetTypeMember(std::move(getTypeMember)) { }

    ConstraintSolverArena(const ConstraintSolverArena &) = delete;
    ConstraintSolverArena(ConstraintSolverArena &&) = delete;
    ConstraintSolverArena &operator=(const ConstraintSolverArena &) = delete;
    ConstraintSolverArena &operator=(ConstraintSolverArena &&) = delete;
  };

  /// \brief The current constraint solver arena, if any.
  std::unique_ptr<ConstraintSolverArena> CurrentConstraintSolverArena;

  Arena &getArena(AllocationArena arena) {
    switch (arena) {
    case AllocationArena::Permanent:
      return Permanent;

    case AllocationArena::ConstraintSolver:
      assert(CurrentConstraintSolverArena && "No constraint solver active?");
      return *CurrentConstraintSolverArena;
    }
    llvm_unreachable("bad AllocationArena");
  }
};

ASTContext::Implementation::Implementation()
 : IdentifierTable(Allocator) {}
ASTContext::Implementation::~Implementation() {
  for (auto &cleanup : Cleanups)
    cleanup();
}

ConstraintCheckerArenaRAII::
ConstraintCheckerArenaRAII(ASTContext &self, llvm::BumpPtrAllocator &allocator,
                           GetTypeVariableMemberCallback getTypeMember)
  : Self(self), Data(self.Impl.CurrentConstraintSolverArena.release())
{
  Self.Impl.CurrentConstraintSolverArena.reset(
    new ASTContext::Implementation::ConstraintSolverArena(
          allocator,
          std::move(getTypeMember)));
}

ConstraintCheckerArenaRAII::~ConstraintCheckerArenaRAII() {
  Self.Impl.CurrentConstraintSolverArena.reset(
    (ASTContext::Implementation::ConstraintSolverArena *)Data);
}

static Module *createBuiltinModule(ASTContext &ctx) {
  auto M = Module::create(ctx.getIdentifier("Builtin"), ctx);
  M->addFile(*new (ctx) BuiltinUnit(*M));
  return M;
}

ASTContext::ASTContext(LangOptions &langOpts, SearchPathOptions &SearchPathOpts,
                       SourceManager &SourceMgr, DiagnosticEngine &Diags)
  : Impl(*new Implementation()),
    LangOpts(langOpts),
    SearchPathOpts(SearchPathOpts),
    SourceMgr(SourceMgr),
    Diags(Diags),
    TheBuiltinModule(createBuiltinModule(*this)),
    StdlibModuleName(getIdentifier(STDLIB_NAME)),
    SwiftShimsModuleName(getIdentifier(SWIFT_SHIMS_NAME)),
    TypeCheckerDebug(new StderrTypeCheckerDebugConsumer()),
    TheErrorType(new (*this, AllocationArena::Permanent) ErrorType(*this)),
    TheUnresolvedType(new (*this, AllocationArena::Permanent)
                      UnresolvedType(*this)),
    TheEmptyTupleType(TupleType::get(ArrayRef<TupleTypeElt>(), *this)),
    TheAnyType(ProtocolCompositionType::get(*this, ArrayRef<Type>())),
    TheNativeObjectType(new (*this, AllocationArena::Permanent)
                           BuiltinNativeObjectType(*this)),
    TheBridgeObjectType(new (*this, AllocationArena::Permanent)
                           BuiltinBridgeObjectType(*this)),
    TheUnknownObjectType(new (*this, AllocationArena::Permanent)
                         BuiltinUnknownObjectType(*this)),
    TheRawPointerType(new (*this, AllocationArena::Permanent)
                        BuiltinRawPointerType(*this)),
    TheUnsafeValueBufferType(new (*this, AllocationArena::Permanent)
                               BuiltinUnsafeValueBufferType(*this)),
    TheIEEE32Type(new (*this, AllocationArena::Permanent)
                    BuiltinFloatType(BuiltinFloatType::IEEE32,*this)),
    TheIEEE64Type(new (*this, AllocationArena::Permanent)
                    BuiltinFloatType(BuiltinFloatType::IEEE64,*this)),
    TheIEEE16Type(new (*this, AllocationArena::Permanent)
                    BuiltinFloatType(BuiltinFloatType::IEEE16,*this)),
    TheIEEE80Type(new (*this, AllocationArena::Permanent)
                    BuiltinFloatType(BuiltinFloatType::IEEE80,*this)),
    TheIEEE128Type(new (*this, AllocationArena::Permanent)
                    BuiltinFloatType(BuiltinFloatType::IEEE128, *this)),
    ThePPC128Type(new (*this, AllocationArena::Permanent)
                    BuiltinFloatType(BuiltinFloatType::PPC128, *this)) {

  // Initialize all of the known identifiers.
#define IDENTIFIER_WITH_NAME(Name, IdStr) Id_##Name = getIdentifier(IdStr);
#include "swift/AST/KnownIdentifiers.def"

  // Record the initial set of search paths.
  for (StringRef path : SearchPathOpts.ImportSearchPaths)
    Impl.SearchPathsSet[path] |= SearchPathKind::Import;
  for (StringRef path : SearchPathOpts.FrameworkSearchPaths)
    Impl.SearchPathsSet[path] |= SearchPathKind::Framework;
}

ASTContext::~ASTContext() {
  delete &Impl;
}

llvm::BumpPtrAllocator &ASTContext::getAllocator(AllocationArena arena) const {
  switch (arena) {
  case AllocationArena::Permanent:
    return Impl.Allocator;

  case AllocationArena::ConstraintSolver:
    assert(Impl.CurrentConstraintSolverArena.get() != nullptr);
    return Impl.CurrentConstraintSolverArena->Allocator;
  }
  llvm_unreachable("bad AllocationArena");
}

LazyResolver *ASTContext::getLazyResolver() const {
  return Impl.Resolver;
}

/// Set the lazy resolver for this context.
void ASTContext::setLazyResolver(LazyResolver *resolver) {
  if (resolver) {
    assert(Impl.Resolver == nullptr && "already have a resolver");
    Impl.Resolver = resolver;
  } else {
    assert(Impl.Resolver != nullptr && "no resolver to remove");
    Impl.Resolver = resolver;

    // DelayedConformanceDiags callbacks contain pointers to the TypeChecker, so
    // they must be removed when the TypeChecker goes away.
    Impl.DelayedConformanceDiags.clear();
  }
}

/// getIdentifier - Return the uniqued and AST-Context-owned version of the
/// specified string.
Identifier ASTContext::getIdentifier(StringRef Str) const {
  // Make sure null pointers stay null.
  if (Str.data() == nullptr) return Identifier(0);

  auto I = Impl.IdentifierTable.insert(std::make_pair(Str, char())).first;
  return Identifier(I->getKeyData());
}

void ASTContext::lookupInSwiftModule(
                   StringRef name,
                   SmallVectorImpl<ValueDecl *> &results) const {
  Module *M = getStdlibModule();
  if (!M)
    return;

  // Find all of the declarations with this name in the Swift module.
  auto identifier = getIdentifier(name);
  M->lookupValue({ }, identifier, NLKind::UnqualifiedLookup, results);
}

/// Find the generic implementation declaration for the named syntactic-sugar
/// type.
static NominalTypeDecl *findStdlibType(const ASTContext &ctx, StringRef name,
                                       unsigned genericParams) {
  // Find all of the declarations with this name in the Swift module.
  SmallVector<ValueDecl *, 1> results;
  ctx.lookupInSwiftModule(name, results);
  for (auto result : results) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(result)) {
      auto params = nominal->getGenericParams();
      if (genericParams == (params == nullptr ? 0 : params->size())) {
        // We found it.
        return nominal;
      }
    }
  }
  return nullptr;
}

NominalTypeDecl *ASTContext::getBoolDecl() const {
  if (!Impl.BoolDecl)
    Impl.BoolDecl = findStdlibType(*this, "Bool", 0);
  return Impl.BoolDecl;
}

NominalTypeDecl *ASTContext::getIntDecl() const {
  if (!Impl.IntDecl)
    Impl.IntDecl = findStdlibType(*this, "Int", 0);
  return Impl.IntDecl;
}

NominalTypeDecl *ASTContext::getUIntDecl() const {
  if (!Impl.UIntDecl)
    Impl.UIntDecl = findStdlibType(*this, "UInt", 0);
  return Impl.UIntDecl;
}

NominalTypeDecl *ASTContext::getFloatDecl() const {
  if (!Impl.FloatDecl)
    Impl.FloatDecl = findStdlibType(*this, "Float", 0);
  return Impl.FloatDecl;
}

NominalTypeDecl *ASTContext::getDoubleDecl() const {
  if (!Impl.DoubleDecl)
    Impl.DoubleDecl = findStdlibType(*this, "Double", 0);
  return Impl.DoubleDecl;
}

NominalTypeDecl *ASTContext::getStringDecl() const {
  if (!Impl.StringDecl)
    Impl.StringDecl = findStdlibType(*this, "String", 0);
  return Impl.StringDecl;
}

CanType ASTContext::getExceptionType() const {
  if (auto exn = getErrorDecl()) {
    return exn->getDeclaredType()->getCanonicalType();
  } else {
    // Use Builtin.NativeObject just as a stand-in.
    return TheNativeObjectType;
  }
}

NominalTypeDecl *ASTContext::getErrorDecl() const {
  return getProtocol(KnownProtocolKind::Error);
}

NominalTypeDecl *ASTContext::getArrayDecl() const {
  if (!Impl.ArrayDecl)
    Impl.ArrayDecl = findStdlibType(*this, "Array", 1);
  return Impl.ArrayDecl;
}

NominalTypeDecl *ASTContext::getSetDecl() const {
  if (!Impl.SetDecl)
    Impl.SetDecl = findStdlibType(*this, "Set", 1);
  return Impl.SetDecl;
}

NominalTypeDecl *ASTContext::getSequenceDecl() const {
  if (!Impl.SequenceDecl)
    Impl.SequenceDecl = findStdlibType(*this, "Sequence", 1);
  return Impl.SequenceDecl;
}

NominalTypeDecl *ASTContext::getDictionaryDecl() const {
  if (!Impl.DictionaryDecl)
    Impl.DictionaryDecl = findStdlibType(*this, "Dictionary", 2);
  return Impl.DictionaryDecl;
}

EnumDecl *ASTContext::getOptionalDecl(OptionalTypeKind kind) const {
  switch (kind) {
  case OTK_None:
    llvm_unreachable("not optional");
  case OTK_ImplicitlyUnwrappedOptional:
    return getImplicitlyUnwrappedOptionalDecl();
  case OTK_Optional:
    return getOptionalDecl();
  }
}

EnumDecl *ASTContext::getOptionalDecl() const {
  if (!Impl.OptionalDecl)
    Impl.OptionalDecl
      = dyn_cast_or_null<EnumDecl>(findStdlibType(*this, "Optional", 1));

  return Impl.OptionalDecl;
}

static EnumElementDecl *findEnumElement(EnumDecl *e, Identifier name) {
  for (auto elt : e->getAllElements()) {
    if (elt->getName() == name)
      return elt;
  }
  return nullptr;
}

EnumElementDecl *ASTContext::getOptionalSomeDecl(OptionalTypeKind kind) const {
  switch (kind) {
  case OTK_Optional:
    return getOptionalSomeDecl();
  case OTK_ImplicitlyUnwrappedOptional:
    return getImplicitlyUnwrappedOptionalSomeDecl();
  case OTK_None:
    llvm_unreachable("getting Some decl for non-optional type?");
  }
  llvm_unreachable("bad OTK");
}

EnumElementDecl *ASTContext::getOptionalNoneDecl(OptionalTypeKind kind) const {
  switch (kind) {
  case OTK_Optional:
    return getOptionalNoneDecl();
  case OTK_ImplicitlyUnwrappedOptional:
    return getImplicitlyUnwrappedOptionalNoneDecl();
  case OTK_None:
    llvm_unreachable("getting None decl for non-optional type?");
  }
  llvm_unreachable("bad OTK");
}

EnumElementDecl *ASTContext::getOptionalSomeDecl() const {
  if (!Impl.OptionalSomeDecl)
    Impl.OptionalSomeDecl = findEnumElement(getOptionalDecl(), Id_some);
  return Impl.OptionalSomeDecl;
}

EnumElementDecl *ASTContext::getOptionalNoneDecl() const {
  if (!Impl.OptionalNoneDecl)
    Impl.OptionalNoneDecl = findEnumElement(getOptionalDecl(), Id_none);
  return Impl.OptionalNoneDecl;
}

EnumDecl *ASTContext::getImplicitlyUnwrappedOptionalDecl() const {
  if (!Impl.ImplicitlyUnwrappedOptionalDecl)
    Impl.ImplicitlyUnwrappedOptionalDecl
      = dyn_cast_or_null<EnumDecl>(
          findStdlibType(*this, "ImplicitlyUnwrappedOptional", 1));

  return Impl.ImplicitlyUnwrappedOptionalDecl;
}

EnumElementDecl *ASTContext::getImplicitlyUnwrappedOptionalSomeDecl() const {
  if (!Impl.ImplicitlyUnwrappedOptionalSomeDecl)
    Impl.ImplicitlyUnwrappedOptionalSomeDecl =
      findEnumElement(getImplicitlyUnwrappedOptionalDecl(), Id_some);
  return Impl.ImplicitlyUnwrappedOptionalSomeDecl;
}

EnumElementDecl *ASTContext::getImplicitlyUnwrappedOptionalNoneDecl() const {
  if (!Impl.ImplicitlyUnwrappedOptionalNoneDecl)
    Impl.ImplicitlyUnwrappedOptionalNoneDecl =
      findEnumElement(getImplicitlyUnwrappedOptionalDecl(), Id_none);
  return Impl.ImplicitlyUnwrappedOptionalNoneDecl;
}

NominalTypeDecl *ASTContext::getOptionSetDecl() const {
  if (!Impl.OptionSetDecl)
    Impl.OptionSetDecl = findStdlibType(*this, "OptionSet", 1);
  return Impl.OptionSetDecl;
}

NominalTypeDecl *ASTContext::getUnsafeMutableRawPointerDecl() const {
  if (!Impl.UnsafeMutableRawPointerDecl)
    Impl.UnsafeMutableRawPointerDecl = findStdlibType(
      *this, "UnsafeMutableRawPointer", 0);
  
  return Impl.UnsafeMutableRawPointerDecl;
}

NominalTypeDecl *ASTContext::getUnsafeRawPointerDecl() const {
  if (!Impl.UnsafeRawPointerDecl)
    Impl.UnsafeRawPointerDecl = findStdlibType(
      *this, "UnsafeRawPointer", 0);

  return Impl.UnsafeRawPointerDecl;
}

NominalTypeDecl *ASTContext::getUnsafeMutablePointerDecl() const {
  if (!Impl.UnsafeMutablePointerDecl)
    Impl.UnsafeMutablePointerDecl = findStdlibType(
      *this, "UnsafeMutablePointer", 1);

  return Impl.UnsafeMutablePointerDecl;
}

NominalTypeDecl *ASTContext::getOpaquePointerDecl() const {
  if (!Impl.OpaquePointerDecl)
    Impl.OpaquePointerDecl
      = findStdlibType(*this, "OpaquePointer", 0);
  
  return Impl.OpaquePointerDecl;
}

NominalTypeDecl *ASTContext::getUnsafePointerDecl() const {
  if (!Impl.UnsafePointerDecl)
    Impl.UnsafePointerDecl
      = findStdlibType(*this, "UnsafePointer", 1);
  
  return Impl.UnsafePointerDecl;
}

NominalTypeDecl *ASTContext::getAutoreleasingUnsafeMutablePointerDecl() const {
  if (!Impl.AutoreleasingUnsafeMutablePointerDecl)
    Impl.AutoreleasingUnsafeMutablePointerDecl
      = findStdlibType(*this, "AutoreleasingUnsafeMutablePointer", 1);
  
  return Impl.AutoreleasingUnsafeMutablePointerDecl;
}

NominalTypeDecl *ASTContext::getUnmanagedDecl() const {
  if (!Impl.UnmanagedDecl)
    Impl.UnmanagedDecl = findStdlibType(*this, "Unmanaged", 1);
  
  return Impl.UnmanagedDecl;
}

static VarDecl *getPointeeProperty(VarDecl *&cache,
                           NominalTypeDecl *(ASTContext::*getNominal)() const,
                                  const ASTContext &ctx) {
  if (cache) return cache;

  // There must be a generic type with one argument.
  NominalTypeDecl *nominal = (ctx.*getNominal)();
  if (!nominal) return nullptr;
  auto generics = nominal->getGenericParams();
  if (!generics) return nullptr;
  if (generics->size() != 1) return nullptr;

  // There must be a property named "pointee".
  auto identifier = ctx.getIdentifier("pointee");
  auto results = nominal->lookupDirect(identifier);
  if (results.size() != 1) return nullptr;

  // The property must have type T.
  VarDecl *property = dyn_cast<VarDecl>(results[0]);
  if (!property) return nullptr;
  if (!property->getType()->isEqual(generics->getPrimaryArchetypes()[0]))
    return nullptr;

  cache = property;
  return property;
}

VarDecl *
ASTContext::getPointerPointeePropertyDecl(PointerTypeKind ptrKind) const {
  switch (ptrKind) {
  case PTK_UnsafeMutableRawPointer:
    return getPointeeProperty(Impl.UnsafeMutableRawPointerMemoryDecl,
                             &ASTContext::getUnsafeMutableRawPointerDecl,
                             *this);
  case PTK_UnsafeRawPointer:
    return getPointeeProperty(Impl.UnsafeRawPointerMemoryDecl,
                             &ASTContext::getUnsafeRawPointerDecl,
                             *this);
  case PTK_UnsafeMutablePointer:
    return getPointeeProperty(Impl.UnsafeMutablePointerMemoryDecl,
                             &ASTContext::getUnsafeMutablePointerDecl,
                             *this);
  case PTK_UnsafePointer:
    return getPointeeProperty(Impl.UnsafePointerMemoryDecl,
                             &ASTContext::getUnsafePointerDecl,
                             *this);
  case PTK_AutoreleasingUnsafeMutablePointer:
    return getPointeeProperty(Impl.AutoreleasingUnsafeMutablePointerMemoryDecl,
                         &ASTContext::getAutoreleasingUnsafeMutablePointerDecl,
                             *this);
  }
  llvm_unreachable("bad pointer kind");
}

NominalTypeDecl *ASTContext::getNeverDecl() const {
  if (!Impl.NeverDecl)
    Impl.NeverDecl = findStdlibType(*this, "Never", 0);
  
  return Impl.NeverDecl;
}

CanType ASTContext::getNeverType() const {
  return getNeverDecl()->getDeclaredType()->getCanonicalType();
}

TypeAliasDecl *ASTContext::getVoidDecl() const {
  if (Impl.VoidDecl) {
    return Impl.VoidDecl;
  }

  // Go find 'Void' in the Swift module.
  SmallVector<ValueDecl *, 1> results;
  lookupInSwiftModule("Void", results);
  for (auto result : results) {
    if (auto typeAlias = dyn_cast<TypeAliasDecl>(result)) {
      Impl.VoidDecl = typeAlias;
      return typeAlias;
    }
  }

  return Impl.VoidDecl;
}

StructDecl *ASTContext::getObjCBoolDecl() const {
  if (!Impl.ObjCBoolDecl) {
    SmallVector<ValueDecl *, 1> results;
    auto *Context = const_cast<ASTContext *>(this);
    if (Module *M = Context->getModuleByName(Id_ObjectiveC.str())) {
      M->lookupValue({ }, getIdentifier("ObjCBool"), NLKind::UnqualifiedLookup,
                     results);
      for (auto result : results) {
        if (auto structDecl = dyn_cast<StructDecl>(result)) {
          if (structDecl->getGenericParams() == nullptr) {
            Impl.ObjCBoolDecl = structDecl;
            break;
          }
        }
      }
    }
  }

  return Impl.ObjCBoolDecl;
}

ClassDecl *ASTContext::getNSErrorDecl() const {
  if (!Impl.NSErrorDecl) {
    if (Module *M = getLoadedModule(Id_Foundation)) {
      // Note: use unqualified lookup so we find NSError regardless of
      // whether it's defined in the Foundation module or the Clang
      // Foundation module it imports.
      UnqualifiedLookup lookup(getIdentifier("NSError"), M, nullptr);
      if (auto type = lookup.getSingleTypeResult()) {
        if (auto classDecl = dyn_cast<ClassDecl>(type)) {
          if (classDecl->getGenericParams() == nullptr) {
            Impl.NSErrorDecl = classDecl;
          }
        }
      }
    }
  }

  return Impl.NSErrorDecl;
}

ProtocolDecl *ASTContext::getProtocol(KnownProtocolKind kind) const {
  // Check whether we've already looked for and cached this protocol.
  unsigned index = (unsigned)kind;
  assert(index < NumKnownProtocols && "Number of known protocols is wrong");
  if (Impl.KnownProtocols[index])
    return Impl.KnownProtocols[index];

  // Find all of the declarations with this name in the appropriate module.
  SmallVector<ValueDecl *, 1> results;

  // _BridgedNSError, _BridgedStoredNSError, and _ErrorCodeProtocol
  // are in the Foundation module.
  if (kind == KnownProtocolKind::BridgedNSError ||
      kind == KnownProtocolKind::BridgedStoredNSError ||
      kind == KnownProtocolKind::ErrorCodeProtocol) {
    Module *foundation =
        const_cast<ASTContext *>(this)->getLoadedModule(Id_Foundation);
    if (!foundation)
      return nullptr;

    auto identifier = getIdentifier(getProtocolName(kind));
    foundation->lookupValue({ }, identifier, NLKind::UnqualifiedLookup,
                            results);
  } else {
    lookupInSwiftModule(getProtocolName(kind), results);
  }

  for (auto result : results) {
    if (auto protocol = dyn_cast<ProtocolDecl>(result)) {
      Impl.KnownProtocols[index] = protocol;
      return protocol;
    }
  }

  return nullptr;
}

/// Find the implementation for the given "intrinsic" library function.
static FuncDecl *findLibraryIntrinsic(const ASTContext &ctx,
                                      StringRef name,
                                      LazyResolver *resolver) {
  SmallVector<ValueDecl *, 1> results;
  ctx.lookupInSwiftModule(name, results);
  if (results.size() == 1) {
    if (auto FD = dyn_cast<FuncDecl>(results.front())) {
      if (resolver)
        resolver->resolveDeclSignature(FD);
      return FD;
    }
  }
  return nullptr;
}

static CanType stripImmediateLabels(CanType type) {
  while (auto tuple = dyn_cast<TupleType>(type)) {
    if (tuple->getNumElements() == 1) {
      type = tuple.getElementType(0);
    } else {
      break;
    }
  }
  return type;
}

/// Check whether the given function is non-generic.
static bool isNonGenericIntrinsic(FuncDecl *fn, CanType &input,
                                  CanType &output) {
  auto fnType = dyn_cast<FunctionType>(fn->getInterfaceType()
                                         ->getCanonicalType());
  if (!fnType)
    return false;

  input = stripImmediateLabels(fnType.getInput());
  output = stripImmediateLabels(fnType.getResult());
  return true;
}

/// Check whether the given type is Builtin.Int1.
static bool isBuiltinInt1Type(CanType type) {
  if (auto intType = dyn_cast<BuiltinIntegerType>(type))
    return intType->isFixedWidth() && intType->getFixedWidth() == 1;
  return false;
}

/// Check whether the given type is Builtin.Word.
static bool isBuiltinWordType(CanType type) {
  if (auto intType = dyn_cast<BuiltinIntegerType>(type))
    return intType->getWidth().isPointerWidth();
  return false;
}

FuncDecl *ASTContext::getGetBoolDecl(LazyResolver *resolver) const {
  if (Impl.GetBoolDecl)
    return Impl.GetBoolDecl;

  // Look for the function.
  CanType input, output;
  auto decl = findLibraryIntrinsic(*this, "_getBool", resolver);
  if (!decl || !isNonGenericIntrinsic(decl, input, output))
    return nullptr;

  // Input must be Builtin.Int1
  if (!isBuiltinInt1Type(input))
    return nullptr;

  // Output must be a global type named Bool.
  auto nominalType = dyn_cast<NominalType>(output);
  if (!nominalType ||
      nominalType.getParent() ||
      nominalType->getDecl()->getName().str() != "Bool")
    return nullptr;

  Impl.GetBoolDecl = decl;
  return decl;
}

FuncDecl *ASTContext::getEqualIntDecl(LazyResolver *resolver) const {
  if (Impl.EqualIntDecl)
    return Impl.EqualIntDecl;
  
  CanType intType = getIntDecl()->getDeclaredType().getCanonicalTypeOrNull();
  CanType boolType = getBoolDecl()->getDeclaredType().getCanonicalTypeOrNull();
  SmallVector<ValueDecl *, 30> equalFuncs;
  lookupInSwiftModule("==", equalFuncs);
  
  // Find the overload for Int.
  for (ValueDecl *vd : equalFuncs) {
    // All "==" decls should be functions, but who knows...
    FuncDecl *funcDecl = dyn_cast<FuncDecl>(vd);
    if (!funcDecl || funcDecl->getDeclContext()->isTypeContext())
      continue;
    
    if (resolver)
      resolver->resolveDeclSignature(funcDecl);

    CanType input, resultType;
    if (!isNonGenericIntrinsic(funcDecl, input, resultType))
      continue;
    
    // Check for the signature: (Int, Int) -> Bool
    auto tType = dyn_cast<TupleType>(input.getPointer());
    assert(tType);
    if (tType->getNumElements() != 2)
      continue;

    CanType argType1 = tType->getElementType(0).getCanonicalTypeOrNull();
    CanType argType2 = tType->getElementType(1).getCanonicalTypeOrNull();
    if (argType1 == intType && argType2 == intType && resultType == boolType) {
      Impl.EqualIntDecl = funcDecl;
      return funcDecl;
    }
  }
  return nullptr;
}

FuncDecl *
ASTContext::getUnimplementedInitializerDecl(LazyResolver *resolver) const {
  if (Impl.UnimplementedInitializerDecl)
    return Impl.UnimplementedInitializerDecl;

  // Look for the function.
  CanType input, output;
  auto decl = findLibraryIntrinsic(*this, "_unimplementedInitializer",
                                   resolver);
  if (!decl || !isNonGenericIntrinsic(decl, input, output))
    return nullptr;

  // FIXME: Check inputs and outputs.

  Impl.UnimplementedInitializerDecl = decl;
  return decl;
}

FuncDecl *
ASTContext::getUndefinedDecl(LazyResolver *resolver) const {
  if (Impl.UndefinedDecl)
    return Impl.UndefinedDecl;

  // Look for the function.
  CanType input, output;
  auto decl = findLibraryIntrinsic(*this, "_undefined", resolver);
  if (!decl)
    return nullptr;

  Impl.UndefinedDecl = decl;
  return decl;
}

FuncDecl *ASTContext::getIsOSVersionAtLeastDecl(LazyResolver *resolver) const {
  if (Impl.IsOSVersionAtLeastDecl)
    return Impl.IsOSVersionAtLeastDecl;

  // Look for the function.
  CanType input, output;
  auto decl =
      findLibraryIntrinsic(*this, "_stdlib_isOSVersionAtLeast", resolver);
  if (!decl || !isNonGenericIntrinsic(decl, input, output))
    return nullptr;

  // Input must be (Builtin.Word, Builtin.Word, Builtin.Word)
  auto inputTuple = dyn_cast<TupleType>(input);
  if (!inputTuple || inputTuple->getNumElements() != 3 ||
      !isBuiltinWordType(
          inputTuple->getElementType(0).getCanonicalTypeOrNull()) ||
      !isBuiltinWordType(
          inputTuple->getElementType(1).getCanonicalTypeOrNull()) ||
      !isBuiltinWordType(
          inputTuple->getElementType(2).getCanonicalTypeOrNull())) {
    return nullptr;
  }

  // Output must be Builtin.Int1
  if (!isBuiltinInt1Type(output))
    return nullptr;

  Impl.IsOSVersionAtLeastDecl = decl;
  return decl;
}

static bool isHigherPrecedenceThan(PrecedenceGroupDecl *a,
                                   PrecedenceGroupDecl *b) {
  assert(a != b && "exact match should already have been filtered");

  SmallVector<PrecedenceGroupDecl*, 4> stack;

  // Compute the transitive set of precedence groups that are
  // explicitly lower than 'b', including 'b' itself.  This is expected
  // to be very small, since it's only legal in downstream modules.
  SmallPtrSet<PrecedenceGroupDecl*, 4> targets;
  targets.insert(b);
  stack.push_back(b);
  do {
    auto cur = stack.pop_back_val();
    for (auto &rel : cur->getLowerThan()) {
      auto group = rel.Group;

      // If we ever see 'a', we're done.
      if (group == a) return true;

      // Protect against invalid ASTs where the group isn't actually set.
      if (!group) continue;

      // If we've already inserted this, don't add it to the queue.
      if (!targets.insert(group).second) continue;

      stack.push_back(group);
    }
  } while (!stack.empty());

  // Walk down the higherThan relationships from 'a' and look for
  // anything in the set we just built.
  stack.push_back(a);
  do {
    auto cur = stack.pop_back_val();
    assert(!targets.count(cur));

    for (auto &rel : cur->getHigherThan()) {
      auto group = rel.Group;

      if (!group) continue;

      // If we ever see a group that's in the targets set, we're done.
      if (targets.count(group)) return true;

      stack.push_back(group);
    }
  } while (!stack.empty());

  return false;
}

static Associativity computeAssociativity(AssociativityCacheType &cache,
                                          PrecedenceGroupDecl *left,
                                          PrecedenceGroupDecl *right) {
  auto it = cache.find({left, right});
  if (it != cache.end()) return it->second;

  auto result = Associativity::None;
  if (isHigherPrecedenceThan(left, right))
    result = Associativity::Left;
  else if (isHigherPrecedenceThan(right, left))
    result = Associativity::Right;
  cache.insert({{left, right}, result});
  return result;
}

Associativity
ASTContext::associateInfixOperators(PrecedenceGroupDecl *left,
                                    PrecedenceGroupDecl *right) const {
  // If the operators are in the same precedence group, use the group's
  // associativity.
  if (left == right) {
    return left->getAssociativity();
  }

  // This relationship is antisymmetric, so we can canonicalize to avoid
  // computing it twice.  Arbitrarily, if the pointer value of 'left'
  // is greater than the pointer value of 'right', we flip them and
  // then flip the result.

  if (uintptr_t(left) < uintptr_t(right)) {
    return computeAssociativity(Impl.AssociativityCache, left, right);
  }

  switch (computeAssociativity(Impl.AssociativityCache, right, left)) {
  case Associativity::Left: return Associativity::Right;
  case Associativity::Right: return Associativity::Left;
  case Associativity::None: return Associativity::None;
  }
  llvm_unreachable("bad associativity");
}

// Find library intrinsic function.
static FuncDecl *findLibraryFunction(const ASTContext &ctx, FuncDecl *&cache, 
                                     StringRef name, LazyResolver *resolver) {
  if (cache) return cache;

  // Look for a generic function.
  cache = findLibraryIntrinsic(ctx, name, resolver);
  return cache;
}

#define FUNC_DECL(Name, Id)                                         \
FuncDecl *ASTContext::get##Name(LazyResolver *resolver) const {     \
  return findLibraryFunction(*this, Impl.Get##Name, Id, resolver);  \
}
#include "swift/AST/KnownDecls.def"

bool ASTContext::hasOptionalIntrinsics(LazyResolver *resolver) const {
  return getOptionalDecl() &&
         getOptionalSomeDecl() &&
         getOptionalNoneDecl() &&
         getDiagnoseUnexpectedNilOptional(resolver);
}

bool ASTContext::hasPointerArgumentIntrinsics(LazyResolver *resolver) const {
  return getUnsafeMutableRawPointerDecl()
    && getUnsafeRawPointerDecl()
    && getUnsafeMutablePointerDecl()
    && getUnsafePointerDecl()
    && (!LangOpts.EnableObjCInterop || getAutoreleasingUnsafeMutablePointerDecl())
    && getConvertPointerToPointerArgument(resolver)
    && getConvertMutableArrayToPointerArgument(resolver)
    && getConvertConstArrayToPointerArgument(resolver)
    && getConvertConstStringToUTF8PointerArgument(resolver)
    && getConvertInOutToPointerArgument(resolver);
}

bool ASTContext::hasArrayLiteralIntrinsics(LazyResolver *resolver) const {
  return getArrayDecl()
    && getAllocateUninitializedArray(resolver)
    && getDeallocateUninitializedArray(resolver);
}

void ASTContext::addExternalDecl(Decl *decl) {
  ExternalDefinitions.insert(decl);
}

void ASTContext::addCleanup(std::function<void(void)> cleanup) {
  Impl.Cleanups.push_back(std::move(cleanup));
}

bool ASTContext::hadError() const {
  return Diags.hadAnyError();
}

/// \brief Retrieve the arena from which we should allocate storage for a type.
static AllocationArena getArena(RecursiveTypeProperties properties) {
  bool hasTypeVariable = properties.hasTypeVariable();
  return hasTypeVariable? AllocationArena::ConstraintSolver
                        : AllocationArena::Permanent;
}

Optional<ArrayRef<Substitution>>
ASTContext::createTrivialSubstitutions(BoundGenericType *BGT,
                                       DeclContext *gpContext) const {
  assert(gpContext && "No generic parameter context");
  assert(BGT->isCanonical() && "Requesting non-canonical substitutions");
  auto Params = gpContext->getGenericParamsOfContext()->getParams();
  assert(Params.size() == 1);
  auto Param = Params[0];
  assert(Param->getArchetype() && "Not type-checked yet");
  (void) Param;
  Substitution Subst(BGT->getGenericArgs()[0], {});
  auto Substitutions = AllocateCopy(llvm::makeArrayRef(Subst));
  auto arena = getArena(BGT->getRecursiveProperties());
  Impl.getArena(arena).BoundGenericSubstitutions
    .insert(std::make_pair(std::make_pair(BGT, gpContext), Substitutions));
  return Substitutions;
}

Optional<ArrayRef<Substitution>>
ASTContext::getSubstitutions(TypeBase *type,
                             DeclContext *gpContext) const {
  assert(gpContext && "Missing generic parameter context");
  auto arena = getArena(type->getRecursiveProperties());
  assert(type->isCanonical() && "Requesting non-canonical substitutions");
  auto &boundGenericSubstitutions
    = Impl.getArena(arena).BoundGenericSubstitutions;
  auto known = boundGenericSubstitutions.find({type, gpContext});
  if (known != boundGenericSubstitutions.end())
    return known->second;

  // We can trivially create substitutions for Array and Optional.
  if (auto bound = dyn_cast<BoundGenericType>(type))
    if (bound->getDecl() == getArrayDecl() ||
        bound->getDecl() == getOptionalDecl())
      return createTrivialSubstitutions(bound, gpContext);

  return None;
}

void ASTContext::setSubstitutions(TypeBase* type,
                                  DeclContext *gpContext,
                                  ArrayRef<Substitution> Subs) const {
  auto arena = getArena(type->getRecursiveProperties());
  auto &boundGenericSubstitutions
    = Impl.getArena(arena).BoundGenericSubstitutions;
  assert(type->isCanonical() && "Requesting non-canonical substitutions");
  assert(boundGenericSubstitutions.count({type, gpContext}) == 0 &&
         "Already have substitutions?");
  boundGenericSubstitutions[{type, gpContext}] = Subs;
}

Type ASTContext::getTypeVariableMemberType(TypeVariableType *baseTypeVar,
                                           AssociatedTypeDecl *assocType) {
  auto &arena = *Impl.CurrentConstraintSolverArena;
  return arena.GetTypeMember(baseTypeVar, assocType);
}

void ASTContext::addSearchPath(StringRef searchPath, bool isFramework) {
  OptionSet<SearchPathKind> &loaded = Impl.SearchPathsSet[searchPath];
  auto kind = isFramework ? SearchPathKind::Framework : SearchPathKind::Import;
  if (loaded.contains(kind))
    return;
  loaded |= kind;

  if (isFramework)
    SearchPathOpts.FrameworkSearchPaths.push_back(searchPath);
  else
    SearchPathOpts.ImportSearchPaths.push_back(searchPath);

  if (auto *clangLoader = getClangModuleLoader())
    clangLoader->addSearchPath(searchPath, isFramework);
}

void ASTContext::addModuleLoader(std::unique_ptr<ModuleLoader> loader,
                                 bool IsClang) {
  if (IsClang) {
    assert(!Impl.TheClangModuleLoader && "Already have a Clang module loader");
    Impl.TheClangModuleLoader =
      static_cast<ClangModuleLoader *>(loader.get());
  }
  Impl.ModuleLoaders.push_back(std::move(loader));
}

void ASTContext::loadExtensions(NominalTypeDecl *nominal,
                                unsigned previousGeneration) {
  for (auto &loader : Impl.ModuleLoaders) {
    loader->loadExtensions(nominal, previousGeneration);
  }
}

void ASTContext::loadObjCMethods(
       ClassDecl *classDecl,
       ObjCSelector selector,
       bool isInstanceMethod,
       unsigned previousGeneration,
       llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) {
  for (auto &loader : Impl.ModuleLoaders) {
    loader->loadObjCMethods(classDecl, selector, isInstanceMethod,
                            previousGeneration, methods);
  }
}

void ASTContext::verifyAllLoadedModules() const {
#ifndef NDEBUG
  for (auto &loader : Impl.ModuleLoaders)
    loader->verifyAllModules();

  for (auto &topLevelModulePair : LoadedModules) {
    Module *M = topLevelModulePair.second;
    assert(!M->getFiles().empty() || M->failedToLoad());
  }
#endif
}

ClangModuleLoader *ASTContext::getClangModuleLoader() const {
  return Impl.TheClangModuleLoader;
}

static void recordKnownProtocol(Module *Stdlib, StringRef Name,
                                KnownProtocolKind Kind) {
  Identifier ID = Stdlib->getASTContext().getIdentifier(Name);
  UnqualifiedLookup Lookup(ID, Stdlib, nullptr, /*NonCascading=*/true,
                           SourceLoc(), /*IsType=*/true);
  if (auto Proto
        = dyn_cast_or_null<ProtocolDecl>(Lookup.getSingleTypeResult()))
    Proto->setKnownProtocolKind(Kind);
}

void ASTContext::recordKnownProtocols(Module *Stdlib) {
#define PROTOCOL_WITH_NAME(Id, Name) \
  recordKnownProtocol(Stdlib, Name, KnownProtocolKind::Id);
#include "swift/AST/KnownProtocols.def"
}

Module *ASTContext::getLoadedModule(
    ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath) const {
  assert(!ModulePath.empty());

  // TODO: Swift submodules.
  if (ModulePath.size() == 1) {
    return getLoadedModule(ModulePath[0].first);
  }
  return nullptr;
}

Module *ASTContext::getLoadedModule(Identifier ModuleName) const {
  return LoadedModules.lookup(ModuleName);
}

void ASTContext::getVisibleTopLevelClangModules(
    SmallVectorImpl<clang::Module*> &Modules) const {
  getClangModuleLoader()->getClangPreprocessor().getHeaderSearchInfo().
    collectAllModules(Modules);
}

ArchetypeBuilder *ASTContext::getOrCreateArchetypeBuilder(
                    CanGenericSignature sig,
                    ModuleDecl *mod) {
  // Check whether we already have an archetype builder for this
  // signature and module.
  auto known = Impl.ArchetypeBuilders.find({sig, mod});
  if (known != Impl.ArchetypeBuilders.end())
    return known->second.get();

  // Create a new archetype builder with the given signature.
  auto builder = new ArchetypeBuilder(*mod, Diags);
  builder->addGenericSignature(sig, /*adoptArchetypes=*/false,
                               /*treatRequirementsAsExplicit=*/true);
  
  // Store this archetype builder.
  Impl.ArchetypeBuilders[{sig, mod}]
    = std::unique_ptr<ArchetypeBuilder>(builder);
  return builder;
}

void ASTContext::setArchetypeBuilder(CanGenericSignature sig,
                                     ModuleDecl *mod,
                                     std::unique_ptr<ArchetypeBuilder> builder) {
  if (Impl.ArchetypeBuilders.find({sig, mod})
        == Impl.ArchetypeBuilders.end()) {
    Impl.ArchetypeBuilders[{sig, mod}] = move(builder);
  }
}

Module *
ASTContext::getModule(ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath) {
  assert(!ModulePath.empty());

  if (auto *M = getLoadedModule(ModulePath))
    return M;

  auto moduleID = ModulePath[0];
  for (auto &importer : Impl.ModuleLoaders) {
    if (Module *M = importer->loadModule(moduleID.second, ModulePath)) {
      if (ModulePath.size() == 1 &&
          (ModulePath[0].first == StdlibModuleName ||
           ModulePath[0].first == Id_Foundation))
        recordKnownProtocols(M);
      return M;
    }
  }

  return nullptr;
}

Module *ASTContext::getModuleByName(StringRef ModuleName) {
  SmallVector<std::pair<Identifier, SourceLoc>, 4>
  AccessPath;
  while (!ModuleName.empty()) {
    StringRef SubModuleName;
    std::tie(SubModuleName, ModuleName) = ModuleName.split('.');
    AccessPath.push_back({ getIdentifier(SubModuleName), SourceLoc() });
  }
  return getModule(AccessPath);
}

Module *ASTContext::getStdlibModule(bool loadIfAbsent) {
  if (TheStdlibModule)
    return TheStdlibModule;

  if (loadIfAbsent) {
    auto mutableThis = const_cast<ASTContext*>(this);
    TheStdlibModule =
      mutableThis->getModule({ std::make_pair(StdlibModuleName, SourceLoc()) });
  } else {
    TheStdlibModule = getLoadedModule(StdlibModuleName);
  }
  return TheStdlibModule;
}

Optional<RawComment> ASTContext::getRawComment(const Decl *D) {
  auto Known = Impl.RawComments.find(D);
  if (Known == Impl.RawComments.end())
    return None;

  return Known->second;
}

void ASTContext::setRawComment(const Decl *D, RawComment RC) {
  Impl.RawComments[D] = RC;
}

Optional<StringRef> ASTContext::getBriefComment(const Decl *D) {
  auto Known = Impl.BriefComments.find(D);
  if (Known == Impl.BriefComments.end())
    return None;

  return Known->second;
}

void ASTContext::setBriefComment(const Decl *D, StringRef Comment) {
  Impl.BriefComments[D] = Comment;
}

unsigned ValueDecl::getLocalDiscriminator() const {
  assert(getDeclContext()->isLocalContext());
  auto &discriminators = getASTContext().Impl.LocalDiscriminators;
  auto it = discriminators.find(this);
  if (it == discriminators.end())
    return 0;
  return it->second;
}

void ValueDecl::setLocalDiscriminator(unsigned index) {
  assert(getDeclContext()->isLocalContext());
  if (!index) {
    assert(!getASTContext().Impl.LocalDiscriminators.count(this));
    return;
  }
  getASTContext().Impl.LocalDiscriminators.insert({this, index});
}

PatternBindingInitializer *
ASTContext::createPatternBindingContext(DeclContext *parent) {
  // Check for an existing context we can re-use.
  if (auto existing = Impl.UnusedPatternBindingContext) {
    Impl.UnusedPatternBindingContext = nullptr;
    existing->reset(parent);
    return existing;
  }

  return new (*this) PatternBindingInitializer(parent);
}
void ASTContext::destroyPatternBindingContext(PatternBindingInitializer *DC) {
  // There isn't much value in caching more than one of these.
  Impl.UnusedPatternBindingContext = DC;
}

DefaultArgumentInitializer *
ASTContext::createDefaultArgumentContext(DeclContext *fn, unsigned index) {
  // Check for an existing context we can re-use.
  if (auto existing = Impl.UnusedDefaultArgumentContext) {
    Impl.UnusedDefaultArgumentContext = nullptr;
    existing->reset(fn, index);
    return existing;
  }

  return new (*this) DefaultArgumentInitializer(fn, index);
}
void ASTContext::destroyDefaultArgumentContext(DefaultArgumentInitializer *DC) {
  // There isn't much value in caching more than one of these.
  Impl.UnusedDefaultArgumentContext = DC;
}

NormalProtocolConformance *
ASTContext::getBehaviorConformance(Type conformingType,
                                   Type conformingInterfaceType,
                                   ProtocolDecl *protocol,
                                   SourceLoc loc,
                                   AbstractStorageDecl *storage,
                                   ProtocolConformanceState state) {
  auto conformance = new (*this, AllocationArena::Permanent)
    NormalProtocolConformance(conformingType, conformingInterfaceType,
                              protocol, loc, storage, state);
  return conformance;
}

NormalProtocolConformance *
ASTContext::getConformance(Type conformingType,
                           ProtocolDecl *protocol,
                           SourceLoc loc,
                           DeclContext *dc,
                           ProtocolConformanceState state) {
  llvm::FoldingSetNodeID id;
  NormalProtocolConformance::Profile(id, protocol, dc);

  // Did we already record the normal conformance?
  void *insertPos;
  auto &normalConformances =
    Impl.getArena(AllocationArena::Permanent).NormalConformances;
  if (auto result = normalConformances.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new normal protocol conformance.
  auto result
    = new (*this, AllocationArena::Permanent)
      NormalProtocolConformance(conformingType, protocol, loc, dc, state);
  normalConformances.InsertNode(result, insertPos);

  return result;
}

SpecializedProtocolConformance *
ASTContext::getSpecializedConformance(Type type,
                                      ProtocolConformance *generic,
                                      ArrayRef<Substitution> substitutions) {
  llvm::FoldingSetNodeID id;
  SpecializedProtocolConformance::Profile(id, type, generic);

  // Figure out which arena this conformance should go into.
  AllocationArena arena = getArena(type->getRecursiveProperties());

  // Did we already record the specialized conformance?
  void *insertPos;
  auto &specializedConformances = Impl.getArena(arena).SpecializedConformances;
  if (auto result = specializedConformances.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new specialized conformance.
  substitutions = AllocateCopy(substitutions, arena);
  auto result
    = new (*this, arena) SpecializedProtocolConformance(type, generic,
                                                        substitutions);
  specializedConformances.InsertNode(result, insertPos);
  return result;
}

InheritedProtocolConformance *
ASTContext::getInheritedConformance(Type type, ProtocolConformance *inherited) {
  llvm::FoldingSetNodeID id;
  InheritedProtocolConformance::Profile(id, type, inherited);

  // Figure out which arena this conformance should go into.
  AllocationArena arena = getArena(type->getRecursiveProperties());

  // Did we already record the normal protocol conformance?
  void *insertPos;
  auto &inheritedConformances = Impl.getArena(arena).InheritedConformances;
  if (auto result
        = inheritedConformances.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new normal protocol conformance.
  auto result = new (*this, arena) InheritedProtocolConformance(type, inherited);
  inheritedConformances.InsertNode(result, insertPos);
  return result;
}

void ASTContext::recordConformanceLoader(Decl *decl, LazyMemberLoader *resolver,
                                         uint64_t contextData) {
  assert(Impl.ConformanceLoaders.count(decl) == 0 &&
         "already recorded conformance loader");
  Impl.ConformanceLoaders[decl] = { resolver, contextData };
}

std::pair<LazyMemberLoader *, uint64_t> ASTContext::takeConformanceLoader(
                                          Decl *decl) {
  auto known = Impl.ConformanceLoaders.find(decl);
  auto result = known->second;
  Impl.ConformanceLoaders.erase(known);
  return result;
}

void ASTContext::addDelayedConformanceDiag(
       NormalProtocolConformance *conformance,
       DelayedConformanceDiag fn) {
  Impl.DelayedConformanceDiags[conformance].push_back(std::move(fn));
}

std::vector<ASTContext::DelayedConformanceDiag>
ASTContext::takeDelayedConformanceDiags(NormalProtocolConformance *conformance){
  std::vector<ASTContext::DelayedConformanceDiag> result;
  auto known = Impl.DelayedConformanceDiags.find(conformance);
  if (known != Impl.DelayedConformanceDiags.end()) {
    result = std::move(known->second);
    Impl.DelayedConformanceDiags.erase(known);
  }
  return result;
}

size_t ASTContext::getTotalMemory() const {
  size_t Size = sizeof(*this) +
    // LoadedModules ?
    // ExternalDefinitions ?
    llvm::capacity_in_bytes(CanonicalGenericTypeParamTypeNames) +
    // RemappedTypes ?
    sizeof(Impl) +
    Impl.Allocator.getTotalMemory() +
    Impl.Cleanups.capacity() +
    llvm::capacity_in_bytes(Impl.ModuleLoaders) +
    llvm::capacity_in_bytes(Impl.RawComments) +
    llvm::capacity_in_bytes(Impl.BriefComments) +
    llvm::capacity_in_bytes(Impl.LocalDiscriminators) +
    llvm::capacity_in_bytes(Impl.ModuleTypes) +
    llvm::capacity_in_bytes(Impl.GenericParamTypes) +
    // Impl.GenericFunctionTypes ?
    // Impl.SILFunctionTypes ?
    llvm::capacity_in_bytes(Impl.SILBlockStorageTypes) +
    llvm::capacity_in_bytes(Impl.SILBoxTypes) +
    llvm::capacity_in_bytes(Impl.IntegerTypes) +
    // Impl.ProtocolCompositionTypes ?
    // Impl.BuiltinVectorTypes ?
    // Impl.GenericSignatures ?
    // Impl.CompoundNames ?
    Impl.OpenedExistentialArchetypes.getMemorySize() +
    Impl.Permanent.getTotalMemory();

    Size += getSolverMemory();

    return Size;
}

size_t ASTContext::getSolverMemory() const {
  size_t Size = 0;
  
  if (Impl.CurrentConstraintSolverArena) {
    Size += Impl.CurrentConstraintSolverArena->getTotalMemory();
  }
  
  return Size;
}

size_t ASTContext::Implementation::Arena::getTotalMemory() const {
  return sizeof(*this) +
    // TupleTypes ?
    llvm::capacity_in_bytes(MetatypeTypes) +
    llvm::capacity_in_bytes(ExistentialMetatypeTypes) +
    llvm::capacity_in_bytes(FunctionTypes) +
    llvm::capacity_in_bytes(ArraySliceTypes) +
    llvm::capacity_in_bytes(DictionaryTypes) +
    llvm::capacity_in_bytes(OptionalTypes) +
    llvm::capacity_in_bytes(ImplicitlyUnwrappedOptionalTypes) +
    llvm::capacity_in_bytes(ParenTypes) +
    llvm::capacity_in_bytes(ReferenceStorageTypes) +
    llvm::capacity_in_bytes(LValueTypes) +
    llvm::capacity_in_bytes(InOutTypes) +
    llvm::capacity_in_bytes(SubstitutedTypes) +
    llvm::capacity_in_bytes(DependentMemberTypes) +
    llvm::capacity_in_bytes(DynamicSelfTypes) +
    // EnumTypes ?
    // StructTypes ?
    // ClassTypes ?
    // UnboundGenericTypes ?
    // BoundGenericTypes ?
    llvm::capacity_in_bytes(BoundGenericSubstitutions);
    // NormalConformances ?
    // SpecializedConformances ?
    // InheritedConformances ?
}

namespace {
  /// Produce a deterministic ordering of the given declarations.
  class OrderDeclarations {
    SourceManager &SrcMgr;

  public:
    OrderDeclarations(SourceManager &srcMgr) : SrcMgr(srcMgr) { }

    bool operator()(ValueDecl *lhs, ValueDecl *rhs) const {
      // If the declarations come from different modules, order based on the
      // module.
      Module *lhsModule = lhs->getDeclContext()->getParentModule();
      Module *rhsModule = rhs->getDeclContext()->getParentModule();
      if (lhsModule != rhsModule) {
        return lhsModule->getName().str() < rhsModule->getName().str();
      }

      // If the two declarations are in the same source file, order based on
      // location within that source file.
      SourceFile *lhsSF = lhs->getDeclContext()->getParentSourceFile();
      SourceFile *rhsSF = rhs->getDeclContext()->getParentSourceFile();
      if (lhsSF == rhsSF) {
        // If only one location is valid, the valid location comes first.
        if (lhs->getLoc().isValid() != rhs->getLoc().isValid()) {
          return lhs->getLoc().isValid();
        }

        // Prefer the declaration that comes first in the source file.
        return SrcMgr.isBeforeInBuffer(lhs->getLoc(), rhs->getLoc());
      }

      // The declarations are in different source files (or unknown source
      // files) of the same module. Order based on name.
      // FIXME: This isn't a total ordering.
      return lhs->getFullName() < rhs->getFullName();
    }
  };

  /// Produce a deterministic ordering of the given declarations with
  /// a bias that favors declarations in the given source file and
  /// members of a class.
  class OrderDeclarationsWithSourceFileAndClassBias {
    SourceManager &SrcMgr;
    SourceFile &SF;

  public:
    OrderDeclarationsWithSourceFileAndClassBias(SourceManager &srcMgr, 
                                                SourceFile &sf)
      : SrcMgr(srcMgr), SF(sf) { }

    bool operator()(ValueDecl *lhs, ValueDecl *rhs) const {
      // Check whether the declarations are in a class.
      bool lhsInClass = isa<ClassDecl>(lhs->getDeclContext());
      bool rhsInClass = isa<ClassDecl>(rhs->getDeclContext());
      if (lhsInClass != rhsInClass)
        return lhsInClass;

      // If the two declarations are in different source files, and one of those
      // source files is the source file we're biasing toward, prefer that
      // declaration.
      SourceFile *lhsSF = lhs->getDeclContext()->getParentSourceFile();
      SourceFile *rhsSF = rhs->getDeclContext()->getParentSourceFile();
      if (lhsSF != rhsSF) {
        if (lhsSF == &SF) return true;
        if (rhsSF == &SF) return false;
      }

      // Fall back to the normal deterministic ordering.
      return OrderDeclarations(SrcMgr)(lhs, rhs);
    }
  };
}

/// Compute the information used to describe an Objective-C redeclaration.
std::pair<unsigned, DeclName> swift::getObjCMethodDiagInfo(
                                AbstractFunctionDecl *member) {
  if (isa<ConstructorDecl>(member))
    return { 0 + member->isImplicit(), member->getFullName() };

  if (isa<DestructorDecl>(member))
    return { 2 + member->isImplicit(), member->getFullName() };

  auto func = cast<FuncDecl>(member);
  switch (func->getAccessorKind()) {
  case AccessorKind::IsAddressor:
  case AccessorKind::IsDidSet:
  case AccessorKind::IsMaterializeForSet:
  case AccessorKind::IsMutableAddressor:
  case AccessorKind::IsWillSet:
    llvm_unreachable("Not an Objective-C entry point");

  case AccessorKind::IsGetter:
    if (auto var = dyn_cast<VarDecl>(func->getAccessorStorageDecl()))
      return { 5, var->getFullName() };

    return { 6, Identifier() };

  case AccessorKind::IsSetter:
    if (auto var = dyn_cast<VarDecl>(func->getAccessorStorageDecl()))
      return { 7, var->getFullName() };
    return { 8, Identifier() };

  case AccessorKind::NotAccessor:
    // Normal method.
    return { 4, func->getFullName() };
  }
}

bool swift::fixDeclarationName(InFlightDiagnostic &diag, ValueDecl *decl,
                               DeclName targetName) {
  if (decl->isImplicit()) return false;
  if (decl->getFullName() == targetName) return false;

  // Handle properties directly.
  if (auto var = dyn_cast<VarDecl>(decl)) {
    // Replace the name.
    SmallString<64> scratch;
    diag.fixItReplace(var->getNameLoc(), targetName.getString(scratch));
    return false;
  }

  // We only handle functions from here on.
  auto func = dyn_cast<AbstractFunctionDecl>(decl);
  if (!func) return true;

  auto name = func->getFullName();

  // Fix the name of the function itself.
  if (name.getBaseName() != targetName.getBaseName()) {
    diag.fixItReplace(func->getLoc(), targetName.getBaseName().str());
  }

  // Fix the argument names that need fixing.
  assert(name.getArgumentNames().size()
          == targetName.getArgumentNames().size());
  auto params = func->getParameterList(func->getDeclContext()->isTypeContext());
  for (unsigned i = 0, n = name.getArgumentNames().size(); i != n; ++i) {
    auto origArg = name.getArgumentNames()[i];
    auto targetArg = targetName.getArgumentNames()[i];

    if (origArg == targetArg)
      continue;

    auto *param = params->get(i);

    // The parameter has an explicitly-specified API name, and it's wrong.
    if (param->getArgumentNameLoc() != param->getLoc() &&
        param->getArgumentNameLoc().isValid()) {
      // ... but the internal parameter name was right. Just zap the
      // incorrect explicit specialization.
      if (param->getName() == targetArg) {
        diag.fixItRemoveChars(param->getArgumentNameLoc(),
                              param->getLoc());
        continue;
      }

      // Fix the API name.
      StringRef targetArgStr = targetArg.empty()? "_" : targetArg.str();
      diag.fixItReplace(param->getArgumentNameLoc(), targetArgStr);
      continue;
    }

    // The parameter did not specify a separate API name. Insert one.
    if (targetArg.empty())
      diag.fixItInsert(param->getLoc(), "_ ");
    else {
      llvm::SmallString<8> targetArgStr;
      targetArgStr += targetArg.str();
      targetArgStr += ' ';
      diag.fixItInsert(param->getLoc(), targetArgStr);
    }
  }

  return false;
}

bool swift::fixDeclarationObjCName(InFlightDiagnostic &diag, ValueDecl *decl,
                                   Optional<ObjCSelector> targetNameOpt,
                                   bool ignoreImpliedName) {
  // Subscripts cannot be renamed, so handle them directly.
  if (isa<SubscriptDecl>(decl)) {
    diag.fixItInsert(decl->getAttributeInsertionLoc(/*forModifier=*/false),
                     "@objc ");
    return false;
  }

  // Determine the Objective-C name of the declaration.
  ObjCSelector name = *decl->getObjCRuntimeName();
  auto targetName = *targetNameOpt;

  // Dig out the existing '@objc' attribute on the witness. We don't care
  // about implicit ones because they don't have useful source location
  // information.
  auto attr = decl->getAttrs().getAttribute<ObjCAttr>();
  if (attr && attr->isImplicit())
    attr = nullptr;

  // If there is an @objc attribute with an explicit, incorrect witness
  // name, go fix the witness name.
  if (attr && name != targetName &&
      attr->hasName() && !attr->isNameImplicit()) {
    // Find the source range covering the full name.
    SourceLoc startLoc;
    if (attr->getNameLocs().empty())
      startLoc = attr->getRParenLoc();
    else
      startLoc = attr->getNameLocs().front();

    // Replace the name with the name of the requirement.
    SmallString<64> scratch;
    diag.fixItReplaceChars(startLoc, attr->getRParenLoc(),
                           targetName.getString(scratch));
    return false;
  }

  // We need to create or amend an @objc attribute with the appropriate name.

  // Form the Fix-It text.
  SourceLoc startLoc;
  SmallString<64> fixItText;
  {
    assert((!attr || !attr->hasName() || attr->isNameImplicit() ||
            name == targetName) && "Nothing to diagnose!");
    llvm::raw_svector_ostream out(fixItText);

    // If there is no @objc attribute, we need to add our own '@objc'.
    if (!attr) {
      startLoc = decl->getAttributeInsertionLoc(/*forModifier=*/false);
      out << "@objc";
    } else {
      startLoc = Lexer::getLocForEndOfToken(decl->getASTContext().SourceMgr,
                                            attr->getRange().End);
    }

    // If the names of the witness and requirement differ, we need to
    // specify the name.
    if (name != targetName || ignoreImpliedName) {
      out << "(";
      out << targetName;
      out << ")";
    }

    if (!attr)
      out << " ";
  }

  diag.fixItInsert(startLoc, fixItText);
  return false;
}

void ASTContext::diagnoseAttrsRequiringFoundation(SourceFile &SF) {
  bool ImportsFoundationModule = false;

  if (SF.Kind == SourceFileKind::SIL ||
      !LangOpts.EnableObjCAttrRequiresFoundation)
    return;

  SF.forAllVisibleModules([&](Module::ImportedModule import) {
    if (import.second->getName() == Id_Foundation)
      ImportsFoundationModule = true;
  });

  if (ImportsFoundationModule)
    return;

  for (auto &Attr : SF.AttrsRequiringFoundation) {
    // If we've already diagnosed this attribute, keep going.
    if (!Attr.second)
      continue;

    Diags.diagnose(Attr.second->getLocation(),
                   diag::attr_used_without_required_module,
                   Attr.second, Id_Foundation)
      .highlight(Attr.second->getRangeWithAt());

    // Don't diagnose this again.
    Attr.second = nullptr;
  }
}

void ASTContext::recordObjCMethod(AbstractFunctionDecl *func) {
  // If this method comes from Objective-C, ignore it.
  if (func->hasClangNode())
    return;

  Impl.ObjCMethods.push_back(func);
}

/// Lookup for an Objective-C method with the given selector in the
/// given class type or any of its superclasses.
static AbstractFunctionDecl *lookupObjCMethodInType(
                               Type classType,
                               ObjCSelector selector,
                               bool isInstanceMethod,
                               bool isInitializer,
                               SourceManager &srcMgr,
                               bool inheritingInits = true) {
  // Dig out the declaration of the class.
  auto classDecl = classType->getClassOrBoundGenericClass();
  if (!classDecl)
    return nullptr;

  // Look for an Objective-C method in this class.
  auto methods = classDecl->lookupDirect(selector, isInstanceMethod);
  if (!methods.empty()) {
    // If we aren't inheriting initializers, remove any initializers from the
    // list.
    if (!inheritingInits &&
        std::find_if(methods.begin(), methods.end(),
                     [](AbstractFunctionDecl *func) {
                       return isa<ConstructorDecl>(func);
                     }) != methods.end()) {
      SmallVector<AbstractFunctionDecl *, 4> nonInitMethods;
      std::copy_if(methods.begin(), methods.end(),
                   std::back_inserter(nonInitMethods),
                   [&](AbstractFunctionDecl *func) {
                     return !isa<ConstructorDecl>(func);
                   });
      if (nonInitMethods.empty())
        return nullptr;

      return *std::min_element(nonInitMethods.begin(), nonInitMethods.end(),
                               OrderDeclarations(srcMgr));
    }

    return *std::min_element(methods.begin(), methods.end(),
                             OrderDeclarations(srcMgr));
  }

  // Recurse into the superclass.
  if (!classDecl->hasSuperclass())
    return nullptr;

  // Determine whether we are (still) inheriting initializers.
  inheritingInits = inheritingInits &&
                    classDecl->inheritsSuperclassInitializers(nullptr);
  if (isInitializer && !inheritingInits)
    return nullptr;

  return lookupObjCMethodInType(classDecl->getSuperclass(), selector,
                                isInstanceMethod, isInitializer, srcMgr,
                                inheritingInits);
}

void AbstractFunctionDecl::setForeignErrorConvention(
                                         const ForeignErrorConvention &conv) {
  assert(hasThrows() && "setting error convention on non-throwing decl");
  auto &conventionsMap = getASTContext().Impl.ForeignErrorConventions;
  assert(!conventionsMap.count(this) && "error convention already set");
  conventionsMap.insert({this, conv});
}

Optional<ForeignErrorConvention>
AbstractFunctionDecl::getForeignErrorConvention() const {
  if (!isObjC() && !getAttrs().hasAttribute<CDeclAttr>())
    return None;
  if (!hasThrows())
    return None;
  auto &conventionsMap = getASTContext().Impl.ForeignErrorConventions;
  auto it = conventionsMap.find(this);
  if (it == conventionsMap.end()) return None;
  return it->second;
}

bool ASTContext::diagnoseUnintendedObjCMethodOverrides(SourceFile &sf) {
  // Capture the methods in this source file.
  llvm::SmallVector<AbstractFunctionDecl *, 4> methods;
  auto captureMethodInSourceFile = [&](AbstractFunctionDecl *method) -> bool {
    if (method->getDeclContext()->getParentSourceFile() == &sf) {
      methods.push_back(method);
      return true;
    }

    return false;
  };
  Impl.ObjCMethods.erase(std::remove_if(Impl.ObjCMethods.begin(),
                                        Impl.ObjCMethods.end(),
                                        captureMethodInSourceFile),
                         Impl.ObjCMethods.end());

  // If no Objective-C methods were defined in this file, we're done.
  if (methods.empty())
    return false;

  // Sort the methods by declaration order.
  std::sort(methods.begin(), methods.end(), OrderDeclarations(SourceMgr));

  // For each Objective-C method declared in this file, check whether
  // it overrides something in one of its superclasses. We
  // intentionally don't respect access control here, since everything
  // is visible to the Objective-C runtime.
  bool diagnosedAny = false;
  for (auto method : methods) {
    // If the method has an @objc override, we don't need to do any
    // more checking.
    if (auto overridden = method->getOverriddenDecl()) {
      if (overridden->isObjC())
        continue;
    }

    // Skip deinitializers.
    if (isa<DestructorDecl>(method))
      continue;

    // Skip invalid declarations.
    if (method->isInvalid())
      continue;

    // Skip declarations with an invalid 'override' attribute on them.
    if (auto attr = method->getAttrs().getAttribute<OverrideAttr>(true)) {
      if (attr->isInvalid())
        continue;
    }

    auto classDecl =
      method->getDeclContext()->getAsClassOrClassExtensionContext();
    if (!classDecl)
      continue; // error-recovery path, only

    if (!classDecl->hasSuperclass())
      continue;

    // Look for a method that we have overridden in one of our
    // superclasses.
    // Note: This should be treated as a lookup for intra-module dependency
    // purposes, but a subclass already depends on its superclasses and any
    // extensions for many other reasons.
    auto selector = method->getObjCSelector(nullptr);
    AbstractFunctionDecl *overriddenMethod
      = lookupObjCMethodInType(classDecl->getSuperclass(),
                               selector,
                               method->isObjCInstanceMethod(),
                               isa<ConstructorDecl>(method),
                               SourceMgr);
    if (!overriddenMethod)
      continue;

    // Ignore stub implementations.
    if (auto overriddenCtor = dyn_cast<ConstructorDecl>(overriddenMethod)) {
      if (overriddenCtor->hasStubImplementation())
        continue;
    }

    // Diagnose the override.
    auto methodDiagInfo = getObjCMethodDiagInfo(method);
    auto overriddenDiagInfo = getObjCMethodDiagInfo(overriddenMethod);
    Diags.diagnose(method, diag::objc_override_other,
                   methodDiagInfo.first,
                   methodDiagInfo.second,
                   overriddenDiagInfo.first,
                   overriddenDiagInfo.second,
                   selector,
                   overriddenMethod->getDeclContext()
                     ->getDeclaredInterfaceType());
    const ValueDecl *overriddenDecl = overriddenMethod;
    if (overriddenMethod->isImplicit())
      if (auto func = dyn_cast<FuncDecl>(overriddenMethod))
        if (auto storage = func->getAccessorStorageDecl())
          overriddenDecl = storage;
    Diags.diagnose(overriddenDecl, diag::objc_declared_here,
                   overriddenDiagInfo.first, overriddenDiagInfo.second);

    diagnosedAny = true;
  }

  return diagnosedAny;
}

void ASTContext::recordObjCMethodConflict(ClassDecl *classDecl,
                                          ObjCSelector selector,
                                          bool isInstance) {
  Impl.ObjCMethodConflicts.push_back(std::make_tuple(classDecl, selector,
                                                     isInstance));
}

/// Retrieve the source file for the given Objective-C member conflict.
static MutableArrayRef<AbstractFunctionDecl *>
getObjCMethodConflictDecls(const ObjCMethodConflict &conflict) {
  ClassDecl *classDecl = std::get<0>(conflict);
  ObjCSelector selector = std::get<1>(conflict);
  bool isInstanceMethod = std::get<2>(conflict);

  return classDecl->lookupDirect(selector, isInstanceMethod);
}

/// Given a set of conflicting Objective-C methods, remove any methods
/// that are legitimately overridden in Objective-C, i.e., because
/// they occur in different modules, one is defined in the class, and
/// the other is defined in an extension (category) thereof.
static void removeValidObjCConflictingMethods(
              MutableArrayRef<AbstractFunctionDecl *> &methods) {
  // Erase any invalid or stub declarations. We don't want to complain about
  // them, because we might already have complained about
  // redeclarations based on Swift matching.
  auto newEnd = std::remove_if(methods.begin(), methods.end(),
                               [&](AbstractFunctionDecl *method) {
                                 if (method->isInvalid())
                                   return true;

                                 if (auto func = dyn_cast<FuncDecl>(method)) {
                                   if (func->isAccessor()) {
                                     return func->getAccessorStorageDecl()
                                             ->isInvalid();
                                   }

                                   return false;
                                 } 
                                 
                                 if (auto ctor 
                                       = dyn_cast<ConstructorDecl>(method)) {
                                   if (ctor->hasStubImplementation())
                                     return true;

                                   return false;
                                 }

                                 return false;
                               });
  methods = methods.slice(0, newEnd - methods.begin());
}

/// Determine whether we should associate a conflict among the given
/// set of methods with the specified source file.
static bool shouldAssociateConflictWithSourceFile(
              SourceFile &sf, 
              ArrayRef<AbstractFunctionDecl *> methods) {
  bool anyInSourceFile = false;
  bool anyInOtherSourceFile = false;
  bool anyClassMethodsInSourceFile = false;
  for (auto method : methods) {
    // Skip methods in the class itself; we want to only diagnose
    // those if there is a conflict within that file.
    if (isa<ClassDecl>(method->getDeclContext())) {
      if (method->getParentSourceFile() == &sf)
        anyClassMethodsInSourceFile = true;
      continue;
    }

    if (method->getParentSourceFile() == &sf)
      anyInSourceFile = true;
    else
      anyInOtherSourceFile = true;
  }

  return anyInSourceFile || 
    (!anyInOtherSourceFile && anyClassMethodsInSourceFile);
}

bool ASTContext::diagnoseObjCMethodConflicts(SourceFile &sf) {
  // If there were no conflicts, we're done.
  if (Impl.ObjCMethodConflicts.empty())
    return false;

  // Partition the set of conflicts to put the conflicts that involve
  // this source file at the end.
  auto firstLocalConflict
    = std::partition(Impl.ObjCMethodConflicts.begin(),
                     Impl.ObjCMethodConflicts.end(),
                     [&](const ObjCMethodConflict &conflict) -> bool {
                       auto decls = getObjCMethodConflictDecls(conflict);
                       if (shouldAssociateConflictWithSourceFile(sf, decls)) {
                         // It's in this source file. Sort the conflict
                         // declarations. We'll use this later.
                         std::sort(
                           decls.begin(), decls.end(),
                           OrderDeclarationsWithSourceFileAndClassBias(
                             SourceMgr, sf));

                         return false;
                       }

                       return true;
                     });

  // If there were no local conflicts, we're done.
  unsigned numLocalConflicts
    = Impl.ObjCMethodConflicts.end() - firstLocalConflict;
  if (numLocalConflicts == 0)
    return false;

  // Sort the set of conflicts so we get a deterministic order for
  // diagnostics. We use the first conflicting declaration in each set to
  // perform the sort.
  MutableArrayRef<ObjCMethodConflict> localConflicts(&*firstLocalConflict,
                                                     numLocalConflicts);
  std::sort(localConflicts.begin(), localConflicts.end(),
            [&](const ObjCMethodConflict &lhs, const ObjCMethodConflict &rhs) {
              OrderDeclarations ordering(SourceMgr);
              return ordering(getObjCMethodConflictDecls(lhs)[1],
                              getObjCMethodConflictDecls(rhs)[1]);
            });

  // Diagnose each conflict.
  bool anyConflicts = false;
  for (const ObjCMethodConflict &conflict : localConflicts) {
    ObjCSelector selector = std::get<1>(conflict);

    auto methods = getObjCMethodConflictDecls(conflict);

    // Prune out cases where it is acceptable to have a conflict.
    removeValidObjCConflictingMethods(methods);
    if (methods.size() < 2)
      continue;

    // Diagnose the conflict.
    anyConflicts = true;

    // If the first method has a valid source location but the first conflicting
    // declaration does not, swap them so the primary diagnostic has a useful
    // source location.
    if (methods[1]->getLoc().isInvalid() && methods[0]->getLoc().isValid()) {
      std::swap(methods[0], methods[1]);
    }

    auto originalMethod = methods.front();
    auto conflictingMethods = methods.slice(1);

    auto origDiagInfo = getObjCMethodDiagInfo(originalMethod);
    for (auto conflictingDecl : conflictingMethods) {
      auto diagInfo = getObjCMethodDiagInfo(conflictingDecl);

      const ValueDecl *originalDecl = originalMethod;
      if (originalMethod->isImplicit())
        if (auto func = dyn_cast<FuncDecl>(originalMethod))
          if (auto storage = func->getAccessorStorageDecl())
            originalDecl = storage;

      if (diagInfo == origDiagInfo) {
        Diags.diagnose(conflictingDecl, diag::objc_redecl_same,
                       diagInfo.first, diagInfo.second, selector);
        Diags.diagnose(originalDecl, diag::invalid_redecl_prev,
                       originalDecl->getName());
      } else {
        Diags.diagnose(conflictingDecl, diag::objc_redecl,
                       diagInfo.first,
                       diagInfo.second,
                       origDiagInfo.first,
                       origDiagInfo.second,
                       selector);
        Diags.diagnose(originalDecl, diag::objc_declared_here,
                       origDiagInfo.first, origDiagInfo.second);
      }
    }
  }

  // Erase the local conflicts from the list of conflicts.
  Impl.ObjCMethodConflicts.erase(firstLocalConflict,
                                 Impl.ObjCMethodConflicts.end());

  return anyConflicts;
}

void ASTContext::recordObjCUnsatisfiedOptReq(DeclContext *dc,
                                             AbstractFunctionDecl *req) {
  Impl.ObjCUnsatisfiedOptReqs.push_back(ObjCUnsatisfiedOptReq(dc, req));
}

/// Retrieve the source location associated with this declaration
/// context.
static SourceLoc getDeclContextLoc(DeclContext *dc) {
  if (auto ext = dyn_cast<ExtensionDecl>(dc))
    return ext->getLoc();

  return cast<NominalTypeDecl>(dc)->getLoc();
}

bool ASTContext::diagnoseObjCUnsatisfiedOptReqConflicts(SourceFile &sf) {
  // If there are no unsatisfied, optional @objc requirements, we're done.
  if (Impl.ObjCUnsatisfiedOptReqs.empty())
    return false;

  // Partition the set of unsatisfied requirements to put the
  // conflicts that involve this source file at the end.
  auto firstLocalReq
    = std::partition(Impl.ObjCUnsatisfiedOptReqs.begin(),
                     Impl.ObjCUnsatisfiedOptReqs.end(),
                     [&](const ObjCUnsatisfiedOptReq &unsatisfied) -> bool {
                       return &sf != unsatisfied.first->getParentSourceFile();
                     });

  // If there were no local unsatisfied requirements, we're done.
  unsigned numLocalReqs
    = Impl.ObjCUnsatisfiedOptReqs.end() - firstLocalReq;
  if (numLocalReqs == 0)
    return false;

  // Sort the set of local unsatisfied requirements, so we get a
  // deterministic order for diagnostics.
  MutableArrayRef<ObjCUnsatisfiedOptReq> localReqs(&*firstLocalReq,
                                                   numLocalReqs);
  std::sort(localReqs.begin(), localReqs.end(),
            [&](const ObjCUnsatisfiedOptReq &lhs,
                const ObjCUnsatisfiedOptReq &rhs) -> bool {
              return SourceMgr.isBeforeInBuffer(getDeclContextLoc(lhs.first),
                                                getDeclContextLoc(rhs.first));
            });

  // Check each of the unsatisfied optional requirements.
  bool anyDiagnosed = false;
  for (const auto &unsatisfied : localReqs) {
    // Check whether there is a conflict here.
    ClassDecl *classDecl =
      unsatisfied.first->getAsClassOrClassExtensionContext();
    auto req = unsatisfied.second;
    auto selector = req->getObjCSelector();
    bool isInstanceMethod = req->isInstanceMember();
    // FIXME: Also look in superclasses?
    auto conflicts = classDecl->lookupDirect(selector, isInstanceMethod);
    if (conflicts.empty())
      continue;

    // Diagnose the conflict.
    auto reqDiagInfo = getObjCMethodDiagInfo(unsatisfied.second);
    auto conflictDiagInfo = getObjCMethodDiagInfo(conflicts[0]);
    auto protocolName
      = cast<ProtocolDecl>(req->getDeclContext())->getFullName();
    Diags.diagnose(conflicts[0],
                   diag::objc_optional_requirement_conflict,
                   conflictDiagInfo.first,
                   conflictDiagInfo.second,
                   reqDiagInfo.first,
                   reqDiagInfo.second,
                   selector,
                   protocolName);

    /// Local function to determine if the given declaration is an accessor.
    auto isAccessor = [](ValueDecl *decl) -> bool {
      if (auto func = dyn_cast<FuncDecl>(decl))
        return func->isAccessor();

      return false;
    };

    // Fix the name of the witness, if we can.
    if (req->getFullName() != conflicts[0]->getFullName() &&
        req->getKind() == conflicts[0]->getKind() &&
        isAccessor(req) == isAccessor(conflicts[0])) {
      // They're of the same kind: fix the name.
      unsigned kind;
      if (isa<ConstructorDecl>(req))
        kind = 1;
      else if (auto func = dyn_cast<FuncDecl>(req)) {
        if (func->isAccessor())
          kind = isa<SubscriptDecl>(func->getAccessorStorageDecl()) ? 3 : 2;
        else
          kind = 0;
      } else {
        llvm_unreachable("unhandled @objc declaration kind");
      }

      auto diag = Diags.diagnose(conflicts[0],
                                 diag::objc_optional_requirement_swift_rename,
                                 kind, req->getFullName());

      // Fix the Swift name.
      fixDeclarationName(diag, conflicts[0], req->getFullName());

      // Fix the '@objc' attribute, if needed.
      if (!conflicts[0]->canInferObjCFromRequirement(req))
        fixDeclarationObjCName(diag, conflicts[0], req->getObjCRuntimeName(),
                               /*ignoreImpliedName=*/true);
    }

    // @nonobjc will silence this warning.
    bool hasExplicitObjCAttribute = false;
    if (auto objcAttr = conflicts[0]->getAttrs().getAttribute<ObjCAttr>())
      hasExplicitObjCAttribute = !objcAttr->isImplicit();
    if (!hasExplicitObjCAttribute)
      Diags.diagnose(conflicts[0], diag::optional_req_near_match_nonobjc, true)
        .fixItInsert(
          conflicts[0]->getAttributeInsertionLoc(/*forModifier=*/false),
          "@nonobjc ");

    Diags.diagnose(getDeclContextLoc(unsatisfied.first),
                   diag::protocol_conformance_here,
                   true,
                   classDecl->getFullName(),
                   protocolName);
    Diags.diagnose(req, diag::protocol_requirement_here,
                   reqDiagInfo.second);

    anyDiagnosed = true;
  }

  // Erase the local unsatisfied requirements from the list.
  Impl.ObjCUnsatisfiedOptReqs.erase(firstLocalReq,
                                    Impl.ObjCUnsatisfiedOptReqs.end());

  return anyDiagnosed;
}

Optional<KnownFoundationEntity> swift::getKnownFoundationEntity(StringRef name){
  return llvm::StringSwitch<Optional<KnownFoundationEntity>>(name)
#define FOUNDATION_ENTITY(Name) .Case(#Name, KnownFoundationEntity::Name)
#include "swift/AST/KnownFoundationEntities.def"
    .Default(None);
}

bool swift::nameConflictsWithStandardLibrary(KnownFoundationEntity entity) {
  switch (entity) {
  case KnownFoundationEntity::NSArray:
  case KnownFoundationEntity::NSDictionary:
  case KnownFoundationEntity::NSInteger:
  case KnownFoundationEntity::NSRange:
  case KnownFoundationEntity::NSSet:
  case KnownFoundationEntity::NSString:
  case KnownFoundationEntity::NSCopying:
  case KnownFoundationEntity::NSError:
  case KnownFoundationEntity::NSErrorPointer:
  case KnownFoundationEntity::NSNumber:
  case KnownFoundationEntity::NSObject:
  case KnownFoundationEntity::NSUInteger:
  case KnownFoundationEntity::NSURL:
  case KnownFoundationEntity::NSZone:
    return true;

  case KnownFoundationEntity::NSStringEncoding:
    return false;
  }
}

StringRef ASTContext::getSwiftName(KnownFoundationEntity kind) {
  StringRef objcName;
  switch (kind) {
#define FOUNDATION_ENTITY(Name) case KnownFoundationEntity::Name:  \
    objcName = #Name;                                             \
    break;
#include "swift/AST/KnownFoundationEntities.def"
  }

  // If we're omitting needless words and the name won't conflict with
  // something in the standard library, strip the prefix off the Swift
  // name.
  if (LangOpts.StripNSPrefix &&
      !nameConflictsWithStandardLibrary(kind))
    return objcName.substr(2);

  return objcName;
}

void ASTContext::dumpArchetypeContext(ArchetypeType *archetype,
                                      unsigned indent) const {
  dumpArchetypeContext(archetype, llvm::errs(), indent);
}

void ASTContext::dumpArchetypeContext(ArchetypeType *archetype,
                                      llvm::raw_ostream &os,
                                      unsigned indent) const {
  auto knownDC = ArchetypeContexts.find(archetype);
  if (knownDC != ArchetypeContexts.end())
    knownDC->second->printContext(os, indent);
}

//===----------------------------------------------------------------------===//
// Type manipulation routines.
//===----------------------------------------------------------------------===//

// Simple accessors.
Type ErrorType::get(const ASTContext &C) { return C.TheErrorType; }

BuiltinIntegerType *BuiltinIntegerType::get(BuiltinIntegerWidth BitWidth,
                                            const ASTContext &C) {
  BuiltinIntegerType *&Result = C.Impl.IntegerTypes[BitWidth];
  if (Result == 0)
    Result = new (C, AllocationArena::Permanent) BuiltinIntegerType(BitWidth,C);
  return Result;
}

BuiltinVectorType *BuiltinVectorType::get(const ASTContext &context,
                                          Type elementType,
                                          unsigned numElements) {
  llvm::FoldingSetNodeID id;
  BuiltinVectorType::Profile(id, elementType, numElements);

  void *insertPos;
  if (BuiltinVectorType *vecType
        = context.Impl.BuiltinVectorTypes.FindNodeOrInsertPos(id, insertPos))
    return vecType;

  assert(elementType->isCanonical() && "Non-canonical builtin vector?");
  BuiltinVectorType *vecTy
    = new (context, AllocationArena::Permanent)
       BuiltinVectorType(context, elementType, numElements);
  context.Impl.BuiltinVectorTypes.InsertNode(vecTy, insertPos);
  return vecTy;
}


ParenType *ParenType::get(const ASTContext &C, Type underlying) {
  auto properties = underlying->getRecursiveProperties();
  auto arena = getArena(properties);
  ParenType *&Result = C.Impl.getArena(arena).ParenTypes[underlying];
  if (Result == 0) {
    Result = new (C, arena) ParenType(underlying, properties);
  }
  return Result;
}

CanTupleType TupleType::getEmpty(const ASTContext &C) {
  return cast<TupleType>(CanType(C.TheEmptyTupleType));
}

void TupleType::Profile(llvm::FoldingSetNodeID &ID,
                        ArrayRef<TupleTypeElt> Fields) {
  ID.AddInteger(Fields.size());
  for (const TupleTypeElt &Elt : Fields) {
    ID.AddPointer(Elt.NameAndVariadic.getOpaqueValue());
    ID.AddPointer(Elt.getType().getPointer());
  }
}

/// getTupleType - Return the uniqued tuple type with the specified elements.
Type TupleType::get(ArrayRef<TupleTypeElt> Fields, const ASTContext &C) {
  if (Fields.size() == 1 && !Fields[0].isVararg() && !Fields[0].hasName())
    return ParenType::get(C, Fields[0].getType());

  RecursiveTypeProperties properties;
  for (const TupleTypeElt &Elt : Fields) {
    if (Elt.getType())
      properties |= Elt.getType()->getRecursiveProperties();
  }

  auto arena = getArena(properties);


  void *InsertPos = 0;
  // Check to see if we've already seen this tuple before.
  llvm::FoldingSetNodeID ID;
  TupleType::Profile(ID, Fields);

  if (TupleType *TT
        = C.Impl.getArena(arena).TupleTypes.FindNodeOrInsertPos(ID,InsertPos))
    return TT;

  // Make a copy of the fields list into ASTContext owned memory.
  TupleTypeElt *FieldsCopy =
    C.AllocateCopy<TupleTypeElt>(Fields.begin(), Fields.end(), arena);

  bool IsCanonical = true;   // All canonical elts means this is canonical.
  for (const TupleTypeElt &Elt : Fields) {
    if (Elt.getType().isNull() || !Elt.getType()->isCanonical()) {
      IsCanonical = false;
      break;
    }
  }

  Fields = ArrayRef<TupleTypeElt>(FieldsCopy, Fields.size());

  TupleType *New = new (C, arena) TupleType(Fields, IsCanonical ? &C : 0,
                                            properties);
  C.Impl.getArena(arena).TupleTypes.InsertNode(New, InsertPos);
  return New;
}

void UnboundGenericType::Profile(llvm::FoldingSetNodeID &ID,
                                 GenericTypeDecl *TheDecl, Type Parent) {
  ID.AddPointer(TheDecl);
  ID.AddPointer(Parent.getPointer());
}

UnboundGenericType *UnboundGenericType::
get(GenericTypeDecl *TheDecl, Type Parent, const ASTContext &C) {
  llvm::FoldingSetNodeID ID;
  UnboundGenericType::Profile(ID, TheDecl, Parent);
  void *InsertPos = 0;
  RecursiveTypeProperties properties;
  if (Parent) properties |= Parent->getRecursiveProperties();
  auto arena = getArena(properties);

  if (auto unbound = C.Impl.getArena(arena).UnboundGenericTypes
                        .FindNodeOrInsertPos(ID, InsertPos))
    return unbound;

  auto result = new (C, arena) UnboundGenericType(TheDecl, Parent, C,
                                                  properties);
  C.Impl.getArena(arena).UnboundGenericTypes.InsertNode(result, InsertPos);
  return result;
}

void BoundGenericType::Profile(llvm::FoldingSetNodeID &ID,
                               NominalTypeDecl *TheDecl, Type Parent,
                               ArrayRef<Type> GenericArgs,
                               RecursiveTypeProperties &properties) {
  ID.AddPointer(TheDecl);
  ID.AddPointer(Parent.getPointer());
  if (Parent) properties |= Parent->getRecursiveProperties();
  ID.AddInteger(GenericArgs.size());
  for (Type Arg : GenericArgs) {
    ID.AddPointer(Arg.getPointer());
    properties |= Arg->getRecursiveProperties();
  }
}

BoundGenericType::BoundGenericType(TypeKind theKind,
                                   NominalTypeDecl *theDecl,
                                   Type parent,
                                   ArrayRef<Type> genericArgs,
                                   const ASTContext *context,
                                   RecursiveTypeProperties properties)
  : TypeBase(theKind, context, properties),
    TheDecl(theDecl), Parent(parent), GenericArgs(genericArgs)
{
}

BoundGenericType *BoundGenericType::get(NominalTypeDecl *TheDecl,
                                        Type Parent,
                                        ArrayRef<Type> GenericArgs) {
  ASTContext &C = TheDecl->getDeclContext()->getASTContext();
  llvm::FoldingSetNodeID ID;
  RecursiveTypeProperties properties;
  BoundGenericType::Profile(ID, TheDecl, Parent, GenericArgs, properties);

  auto arena = getArena(properties);

  void *InsertPos = 0;
  if (BoundGenericType *BGT =
        C.Impl.getArena(arena).BoundGenericTypes.FindNodeOrInsertPos(ID,
                                                                     InsertPos))
    return BGT;

  ArrayRef<Type> ArgsCopy = C.AllocateCopy(GenericArgs, arena);
  bool IsCanonical = !Parent || Parent->isCanonical();
  if (IsCanonical) {
    for (Type Arg : GenericArgs) {
      if (!Arg->isCanonical()) {
        IsCanonical = false;
        break;
      }
    }
  }

  BoundGenericType *newType;
  if (auto theClass = dyn_cast<ClassDecl>(TheDecl)) {
    newType = new (C, arena) BoundGenericClassType(theClass, Parent, ArgsCopy,
                                                   IsCanonical ? &C : 0,
                                                   properties);
  } else if (auto theStruct = dyn_cast<StructDecl>(TheDecl)) {
    newType = new (C, arena) BoundGenericStructType(theStruct, Parent, ArgsCopy,
                                                    IsCanonical ? &C : 0,
                                                    properties);
  } else {
    auto theEnum = cast<EnumDecl>(TheDecl);
    newType = new (C, arena) BoundGenericEnumType(theEnum, Parent, ArgsCopy,
                                                  IsCanonical ? &C : 0,
                                                  properties);
  }
  C.Impl.getArena(arena).BoundGenericTypes.InsertNode(newType, InsertPos);

  return newType;
}

NominalType *NominalType::get(NominalTypeDecl *D, Type Parent, const ASTContext &C) {
  switch (D->getKind()) {
  case DeclKind::Enum:
    return EnumType::get(cast<EnumDecl>(D), Parent, C);
  case DeclKind::Struct:
    return StructType::get(cast<StructDecl>(D), Parent, C);
  case DeclKind::Class:
    return ClassType::get(cast<ClassDecl>(D), Parent, C);
  case DeclKind::Protocol: {
    return ProtocolType::get(cast<ProtocolDecl>(D), C);
  }

  default:
    llvm_unreachable("Not a nominal declaration!");
  }
}

EnumType::EnumType(EnumDecl *TheDecl, Type Parent, const ASTContext &C,
                     RecursiveTypeProperties properties)
  : NominalType(TypeKind::Enum, &C, TheDecl, Parent, properties) { }

EnumType *EnumType::get(EnumDecl *D, Type Parent, const ASTContext &C) {
  llvm::FoldingSetNodeID id;
  EnumType::Profile(id, D, Parent);

  RecursiveTypeProperties properties;
  if (Parent) properties |= Parent->getRecursiveProperties();
  auto arena = getArena(properties);

  void *insertPos = 0;
  if (auto enumTy
        = C.Impl.getArena(arena).EnumTypes.FindNodeOrInsertPos(id, insertPos))
    return enumTy;

  auto enumTy = new (C, arena) EnumType(D, Parent, C, properties);
  C.Impl.getArena(arena).EnumTypes.InsertNode(enumTy, insertPos);
  return enumTy;
}

void EnumType::Profile(llvm::FoldingSetNodeID &ID, EnumDecl *D, Type Parent) {
  ID.AddPointer(D);
  ID.AddPointer(Parent.getPointer());
}

StructType::StructType(StructDecl *TheDecl, Type Parent, const ASTContext &C,
                       RecursiveTypeProperties properties)
  : NominalType(TypeKind::Struct, &C, TheDecl, Parent, properties) { }

StructType *StructType::get(StructDecl *D, Type Parent, const ASTContext &C) {
  llvm::FoldingSetNodeID id;
  StructType::Profile(id, D, Parent);

  RecursiveTypeProperties properties;
  if (Parent) properties |= Parent->getRecursiveProperties();
  auto arena = getArena(properties);

  void *insertPos = 0;
  if (auto structTy
        = C.Impl.getArena(arena).StructTypes.FindNodeOrInsertPos(id, insertPos))
    return structTy;

  auto structTy = new (C, arena) StructType(D, Parent, C, properties);
  C.Impl.getArena(arena).StructTypes.InsertNode(structTy, insertPos);
  return structTy;
}

void StructType::Profile(llvm::FoldingSetNodeID &ID, StructDecl *D, Type Parent) {
  ID.AddPointer(D);
  ID.AddPointer(Parent.getPointer());
}

ClassType::ClassType(ClassDecl *TheDecl, Type Parent, const ASTContext &C,
                     RecursiveTypeProperties properties)
  : NominalType(TypeKind::Class, &C, TheDecl, Parent, properties) { }

ClassType *ClassType::get(ClassDecl *D, Type Parent, const ASTContext &C) {
  llvm::FoldingSetNodeID id;
  ClassType::Profile(id, D, Parent);

  RecursiveTypeProperties properties;
  if (Parent) properties |= Parent->getRecursiveProperties();
  auto arena = getArena(properties);

  void *insertPos = 0;
  if (auto classTy
        = C.Impl.getArena(arena).ClassTypes.FindNodeOrInsertPos(id, insertPos))
    return classTy;

  auto classTy = new (C, arena) ClassType(D, Parent, C, properties);
  C.Impl.getArena(arena).ClassTypes.InsertNode(classTy, insertPos);
  return classTy;
}

void ClassType::Profile(llvm::FoldingSetNodeID &ID, ClassDecl *D, Type Parent) {
  ID.AddPointer(D);
  ID.AddPointer(Parent.getPointer());
}

ProtocolCompositionType *
ProtocolCompositionType::build(const ASTContext &C, ArrayRef<Type> Protocols) {
  // Check to see if we've already seen this protocol composition before.
  void *InsertPos = 0;
  llvm::FoldingSetNodeID ID;
  ProtocolCompositionType::Profile(ID, Protocols);
  if (ProtocolCompositionType *Result
        = C.Impl.ProtocolCompositionTypes.FindNodeOrInsertPos(ID, InsertPos))
    return Result;

  bool isCanonical = true;
  for (Type t : Protocols) {
    if (!t->isCanonical())
      isCanonical = false;
  }

  // Create a new protocol composition type.
  ProtocolCompositionType *New
    = new (C, AllocationArena::Permanent)
        ProtocolCompositionType(isCanonical ? &C : nullptr,
                                C.AllocateCopy(Protocols));
  C.Impl.ProtocolCompositionTypes.InsertNode(New, InsertPos);
  return New;
}

ReferenceStorageType *ReferenceStorageType::get(Type T, Ownership ownership,
                                                const ASTContext &C) {
  assert(ownership != Ownership::Strong &&
         "ReferenceStorageType is unnecessary for strong ownership");
  assert(!T->hasTypeVariable()); // not meaningful in type-checker
  auto properties = T->getRecursiveProperties();
  auto arena = getArena(properties);

  auto key = uintptr_t(T.getPointer()) | unsigned(ownership);
  auto &entry = C.Impl.getArena(arena).ReferenceStorageTypes[key];
  if (entry) return entry;


  switch (ownership) {
  case Ownership::Strong: llvm_unreachable("not possible");
  case Ownership::Unowned:
    return entry =
      new (C, arena) UnownedStorageType(T, T->isCanonical() ? &C : 0,
                                        properties);
  case Ownership::Weak:
    return entry =
      new (C, arena) WeakStorageType(T, T->isCanonical() ? &C : 0,
                                     properties);
  case Ownership::Unmanaged:
    return entry =
      new (C, arena) UnmanagedStorageType(T, T->isCanonical() ? &C : 0,
                                          properties);
  }
  llvm_unreachable("bad ownership");
}

AnyMetatypeType::AnyMetatypeType(TypeKind kind, const ASTContext *C,
                                 RecursiveTypeProperties properties,
                                 Type instanceType,
                                 Optional<MetatypeRepresentation> repr)
    : TypeBase(kind, C, properties), InstanceType(instanceType) {
  if (repr) {
    AnyMetatypeTypeBits.Representation = static_cast<char>(*repr) + 1;
  } else {
    AnyMetatypeTypeBits.Representation = 0;
  }
}

MetatypeType *MetatypeType::get(Type T, Optional<MetatypeRepresentation> Repr,
                                const ASTContext &Ctx) {
  auto properties = T->getRecursiveProperties();
  auto arena = getArena(properties);

  char reprKey;
  if (Repr.hasValue())
    reprKey = static_cast<char>(*Repr) + 1;
  else
    reprKey = 0;

  MetatypeType *&Entry = Ctx.Impl.getArena(arena).MetatypeTypes[{T, reprKey}];
  if (Entry) return Entry;

  return Entry = new (Ctx, arena) MetatypeType(T,
                                               T->isCanonical() ? &Ctx : 0,
                                               properties, Repr);
}

MetatypeType::MetatypeType(Type T, const ASTContext *C,
                           RecursiveTypeProperties properties,
                           Optional<MetatypeRepresentation> repr)
  : AnyMetatypeType(TypeKind::Metatype, C, properties, T, repr) {
}

ExistentialMetatypeType *
ExistentialMetatypeType::get(Type T, Optional<MetatypeRepresentation> repr,
                             const ASTContext &ctx) {
  auto properties = T->getRecursiveProperties();
  auto arena = getArena(properties);

  char reprKey;
  if (repr.hasValue())
    reprKey = static_cast<char>(*repr) + 1;
  else
    reprKey = 0;

  auto &entry = ctx.Impl.getArena(arena).ExistentialMetatypeTypes[{T, reprKey}];
  if (entry) return entry;

  return entry = new (ctx, arena) ExistentialMetatypeType(T,
                                               T->isCanonical() ? &ctx : 0,
                                               properties, repr);
}

ExistentialMetatypeType::ExistentialMetatypeType(Type T,
                                                 const ASTContext *C,
                                       RecursiveTypeProperties properties,
                                       Optional<MetatypeRepresentation> repr)
  : AnyMetatypeType(TypeKind::ExistentialMetatype, C, properties, T, repr) {
  if (repr) {
    assert(*repr != MetatypeRepresentation::Thin &&
           "creating a thin existential metatype?");
    assert(getASTContext().LangOpts.EnableObjCInterop ||
           *repr != MetatypeRepresentation::ObjC);
  }
}

ModuleType *ModuleType::get(Module *M) {
  ASTContext &C = M->getASTContext();

  ModuleType *&Entry = C.Impl.ModuleTypes[M];
  if (Entry) return Entry;

  return Entry = new (C, AllocationArena::Permanent) ModuleType(M, C);
}

DynamicSelfType *DynamicSelfType::get(Type selfType, const ASTContext &ctx) {
  auto properties = selfType->getRecursiveProperties();
  assert(properties.isMaterializable() && "non-materializable dynamic self?");
  auto arena = getArena(properties);

  auto &dynamicSelfTypes = ctx.Impl.getArena(arena).DynamicSelfTypes;
  auto known = dynamicSelfTypes.find(selfType);
  if (known != dynamicSelfTypes.end())
    return known->second;

  auto result = new (ctx, arena) DynamicSelfType(selfType, ctx, properties);
  dynamicSelfTypes.insert({selfType, result});
  return result;
}

static void checkFunctionRecursiveProperties(Type Input,
                                             Type Result) {
  // TODO: Would be nice to be able to assert these, but they trip during
  // constraint solving:
  //assert(!Input->getRecursiveProperties().isLValue()
  //       && "function should not take lvalues directly as parameters");
  //assert(Result->getRecursiveProperties().isMaterializable()
  //       && "function return should be materializable");
}

static RecursiveTypeProperties getFunctionRecursiveProperties(Type Input,
                                                              Type Result) {
  checkFunctionRecursiveProperties(Input, Result);

  auto properties = Input->getRecursiveProperties()
                  | Result->getRecursiveProperties();
  properties &= ~RecursiveTypeProperties::IsNotMaterializable;
  return properties;
}

// For now, generic function types cannot be dependent (in fact,
// they erase dependence) or contain type variables, and they're
// always materializable.
static RecursiveTypeProperties
getGenericFunctionRecursiveProperties(Type Input, Type Result) {
  checkFunctionRecursiveProperties(Input, Result);

  static_assert(RecursiveTypeProperties::BitWidth == 10,
                "revisit this if you add new recursive type properties");
  RecursiveTypeProperties properties;
  if (Result->getRecursiveProperties().hasDynamicSelf())
    properties |= RecursiveTypeProperties::HasDynamicSelf;
  return properties;
}

AnyFunctionType *AnyFunctionType::withExtInfo(ExtInfo info) const {
  if (isa<FunctionType>(this))
    return FunctionType::get(getInput(), getResult(), info);
  if (auto *polyFnTy = dyn_cast<PolymorphicFunctionType>(this))
    return PolymorphicFunctionType::get(getInput(), getResult(),
                                        &polyFnTy->getGenericParams(), info);
  if (auto *genFnTy = dyn_cast<GenericFunctionType>(this))
    return GenericFunctionType::get(genFnTy->getGenericSignature(),
                                    getInput(), getResult(), info);

  static_assert(3 - 1 ==
                  static_cast<int>(TypeKind::Last_AnyFunctionType) -
                    static_cast<int>(TypeKind::First_AnyFunctionType),
                "unhandled function type");
  llvm_unreachable("unhandled function type");
}

FunctionType *FunctionType::get(Type Input, Type Result,
                                const ExtInfo &Info) {
  auto properties = getFunctionRecursiveProperties(Input, Result);
  auto arena = getArena(properties);
  uint16_t attrKey = Info.getFuncAttrKey();

  const ASTContext &C = Input->getASTContext();

  FunctionType *&Entry
    = C.Impl.getArena(arena).FunctionTypes[{Input, {Result, attrKey} }];
  if (Entry) return Entry;

  return Entry = new (C, arena) FunctionType(Input, Result,
                                             properties,
                                             Info);
}

// If the input and result types are canonical, then so is the result.
FunctionType::FunctionType(Type input, Type output,
                           RecursiveTypeProperties properties,
                           const ExtInfo &Info)
: AnyFunctionType(TypeKind::Function,
                  (input->isCanonical() && output->isCanonical()) ?
                  &input->getASTContext() : 0,
                  input, output,
                  properties,
                  Info)
{ }


/// FunctionType::get - Return a uniqued function type with the specified
/// input and result.
PolymorphicFunctionType *PolymorphicFunctionType::get(Type input, Type output,
                                                      GenericParamList *params,
                                                      const ExtInfo &Info) {
  auto properties = getFunctionRecursiveProperties(input, output);
  auto arena = getArena(properties);

  const ASTContext &C = input->getASTContext();

  return new (C, arena) PolymorphicFunctionType(input, output, params,
                                                Info, C, properties);
}

PolymorphicFunctionType::PolymorphicFunctionType(Type input, Type output,
                                                 GenericParamList *params,
                                                 const ExtInfo &Info,
                                                 const ASTContext &C,
                                        RecursiveTypeProperties properties)
  : AnyFunctionType(TypeKind::PolymorphicFunction,
                    (input->isCanonical() && output->isCanonical()) ?&C : 0,
                    input, output, properties,
                    Info),
    Params(params)
{
  assert(!input->hasTypeVariable() && !output->hasTypeVariable());
}

void GenericFunctionType::Profile(llvm::FoldingSetNodeID &ID,
                                  GenericSignature *sig,
                                  Type input,
                                  Type result,
                                  const ExtInfo &info) {
  ID.AddPointer(sig);
  ID.AddPointer(input.getPointer());
  ID.AddPointer(result.getPointer());
  ID.AddInteger(info.getFuncAttrKey());
}

GenericFunctionType *
GenericFunctionType::get(GenericSignature *sig,
                         Type input,
                         Type output,
                         const ExtInfo &info) {
  assert(sig && "no generic signature for generic function type?!");
  assert(!input->hasTypeVariable() && !output->hasTypeVariable());

  llvm::FoldingSetNodeID id;
  GenericFunctionType::Profile(id, sig, input, output, info);

  const ASTContext &ctx = input->getASTContext();

  // Do we already have this generic function type?
  void *insertPos;
  if (auto result
        = ctx.Impl.GenericFunctionTypes.FindNodeOrInsertPos(id, insertPos)) {
    return result;
  }

  // We have to construct this generic function type. Determine whether
  // it's canonical.  Unfortunately, isCanonicalTypeInContext can cause
  // new GenericFunctionTypes to be created and thus invalidate our insertion
  // point.
  auto &moduleForCanonicality = *ctx.TheBuiltinModule;
  bool isCanonical = sig->isCanonical()
    && sig->isCanonicalTypeInContext(input, moduleForCanonicality)
    && sig->isCanonicalTypeInContext(output, moduleForCanonicality);

  if (auto result
        = ctx.Impl.GenericFunctionTypes.FindNodeOrInsertPos(id, insertPos)) {
    return result;
  }

  // Allocate storage for the object.
  void *mem = ctx.Allocate(sizeof(GenericFunctionType),
                           alignof(GenericFunctionType));

  auto properties = getGenericFunctionRecursiveProperties(input, output);
  auto result = new (mem) GenericFunctionType(sig, input, output, info,
                                              isCanonical ? &ctx : nullptr,
                                              properties);

  ctx.Impl.GenericFunctionTypes.InsertNode(result, insertPos);
  return result;
}

GenericFunctionType::GenericFunctionType(
                       GenericSignature *sig,
                       Type input,
                       Type result,
                       const ExtInfo &info,
                       const ASTContext *ctx,
                       RecursiveTypeProperties properties)
  : AnyFunctionType(TypeKind::GenericFunction, ctx, input, result,
                    properties, info),
    Signature(sig)
{}

GenericTypeParamType *GenericTypeParamType::get(unsigned depth, unsigned index,
                                                const ASTContext &ctx) {
  auto known = ctx.Impl.GenericParamTypes.find({ depth, index });
  if (known != ctx.Impl.GenericParamTypes.end())
    return known->second;

  auto result = new (ctx, AllocationArena::Permanent)
                  GenericTypeParamType(depth, index, ctx);
  ctx.Impl.GenericParamTypes[{depth, index}] = result;
  return result;
}

ArrayRef<GenericTypeParamType *> GenericFunctionType::getGenericParams() const{
  return Signature->getGenericParams();
}

/// Retrieve the requirements of this polymorphic function type.
ArrayRef<Requirement> GenericFunctionType::getRequirements() const {
  return Signature->getRequirements();
}

void SILFunctionType::Profile(llvm::FoldingSetNodeID &id,
                              GenericSignature *genericParams,
                              ExtInfo info,
                              ParameterConvention calleeConvention,
                              ArrayRef<SILParameterInfo> params,
                              ArrayRef<SILResultInfo> results,
                              Optional<SILResultInfo> errorResult) {
  id.AddPointer(genericParams);
  id.AddInteger(info.getFuncAttrKey());
  id.AddInteger(unsigned(calleeConvention));
  id.AddInteger(params.size());
  for (auto param : params)
    param.profile(id);
  id.AddInteger(results.size());
  for (auto result : results)
    result.profile(id);

  // Just allow the profile length to implicitly distinguish the
  // presence of an error result.
  if (errorResult) errorResult->profile(id);
}

SILFunctionType::SILFunctionType(GenericSignature *genericSig,
                                 ExtInfo ext,
                                 ParameterConvention calleeConvention,
                                 ArrayRef<SILParameterInfo> params,
                                 ArrayRef<SILResultInfo> allResults,
                                 ArrayRef<SILResultInfo> directResults,
                                 ArrayRef<SILResultInfo> indirectResults,
                                 Optional<SILResultInfo> errorResult,
                                 const ASTContext &ctx,
                                 RecursiveTypeProperties properties)
  : TypeBase(TypeKind::SILFunction, &ctx, properties),
    GenericSig(genericSig) {
  bool hasCombinedResults =
    (!directResults.empty() && !indirectResults.empty());

  SILFunctionTypeBits.HasErrorResult = errorResult.hasValue();
  SILFunctionTypeBits.HasCombinedResults = hasCombinedResults;
  SILFunctionTypeBits.ExtInfo = ext.Bits;
  NumParameters = params.size();
  NumDirectResults = directResults.size();
  NumIndirectResults = indirectResults.size();
  assert(!isIndirectParameter(calleeConvention));
  SILFunctionTypeBits.CalleeConvention = unsigned(calleeConvention);

  memcpy(getMutableParameters().data(), params.data(),
         params.size() * sizeof(SILParameterInfo));
  memcpy(getMutableAllResults().data(), allResults.data(),
         allResults.size() * sizeof(SILResultInfo));
  if (hasCombinedResults) {
    memcpy(getMutableDirectResults().data(), directResults.data(),
           directResults.size() * sizeof(SILResultInfo));
    memcpy(getMutableIndirectResults().data(), indirectResults.data(),
           indirectResults.size() * sizeof(SILResultInfo));
  }
  if (errorResult)
    getMutableErrorResult() = *errorResult;

  if (hasSILResultCache())
    getMutableSILResultCache() = CanType();

#ifndef NDEBUG
  // Make sure the interface types are sane.
  if (genericSig) {
    for (auto gparam : genericSig->getGenericParams()) {
      (void)gparam;
      assert(gparam->isCanonical() && "generic signature is not canonicalized");
    }

    for (auto param : getParameters()) {
      (void)param;
      assert(!param.getType()->hasArchetype()
             && "interface type of generic type should not contain context archetypes");
    }
    for (auto result : getAllResults()) {
      (void)result;
      assert(!result.getType()->hasArchetype()
             && "interface type of generic type should not contain context archetypes");
    }
    if (hasErrorResult()) {
      assert(!getErrorResult().getType()->hasArchetype()
             && "interface type of generic type should not contain context archetypes");
    }
  }

  // Make sure the direct and indirect results are sane.
  assert(allResults.size() == directResults.size() + indirectResults.size());
  unsigned directIndex = 0, indirectIndex = 0;
  for (auto result : allResults) {
    if (result.isDirect()) {
      assert(directResults[directIndex++] == result);
    } else {
      assert(indirectResults[indirectIndex++] == result);
    }
  }
#endif
}

CanSILBlockStorageType SILBlockStorageType::get(CanType captureType) {
  ASTContext &ctx = captureType->getASTContext();
  auto found = ctx.Impl.SILBlockStorageTypes.find(captureType);
  if (found != ctx.Impl.SILBlockStorageTypes.end())
    return CanSILBlockStorageType(found->second);
  
  void *mem = ctx.Allocate(sizeof(SILBlockStorageType),
                           alignof(SILBlockStorageType));
  
  SILBlockStorageType *storageTy = new (mem) SILBlockStorageType(captureType);
  ctx.Impl.SILBlockStorageTypes.insert({captureType, storageTy});
  return CanSILBlockStorageType(storageTy);
}

CanSILBoxType SILBoxType::get(CanType boxType) {
  ASTContext &ctx = boxType->getASTContext();
  auto found = ctx.Impl.SILBoxTypes.find(boxType);
  if (found != ctx.Impl.SILBoxTypes.end())
    return CanSILBoxType(found->second);
  
  void *mem = ctx.Allocate(sizeof(SILBlockStorageType),
                           alignof(SILBlockStorageType));
  
  auto storageTy = new (mem) SILBoxType(boxType);
  ctx.Impl.SILBoxTypes.insert({boxType, storageTy});
  return CanSILBoxType(storageTy);
}

CanSILFunctionType SILFunctionType::get(GenericSignature *genericSig,
                                    ExtInfo ext, ParameterConvention callee,
                                    ArrayRef<SILParameterInfo> params,
                                    ArrayRef<SILResultInfo> allResults,
                                    Optional<SILResultInfo> errorResult,
                                    const ASTContext &ctx) {
  llvm::FoldingSetNodeID id;
  SILFunctionType::Profile(id, genericSig, ext, callee,
                           params, allResults, errorResult);

  // Do we already have this generic function type?
  void *insertPos;
  if (auto result
        = ctx.Impl.SILFunctionTypes.FindNodeOrInsertPos(id, insertPos))
    return CanSILFunctionType(result);

  // All SILFunctionTypes are canonical.

  SmallVector<SILResultInfo, 4> directResults;
  SmallVector<SILResultInfo, 4> indirectResults;
  for (auto result : allResults) {
    if (result.isDirect()) {
      directResults.push_back(result);
    } else {
      indirectResults.push_back(result);
    }
  }
  bool hasCombinedResults =
    (!directResults.empty() && !indirectResults.empty());

  // Allocate storage for the object.
  size_t bytes = sizeof(SILFunctionType)
               + sizeof(SILParameterInfo) * params.size()
               + sizeof(SILResultInfo) * allResults.size()
               + (hasCombinedResults
                  ? sizeof(SILResultInfo) * allResults.size()
                  : 0)
               + (errorResult ? sizeof(SILResultInfo) : 0)
               + (directResults.size() > 1 ? sizeof(CanType) : 0);
  void *mem = ctx.Allocate(bytes, alignof(SILFunctionType));

  RecursiveTypeProperties properties;
  static_assert(RecursiveTypeProperties::BitWidth == 10,
                "revisit this if you add new recursive type properties");
  for (auto &param : params)
    properties |= param.getType()->getRecursiveProperties();
  for (auto &result : allResults)
    properties |= result.getType()->getRecursiveProperties();
  if (errorResult)
    properties |= errorResult->getType()->getRecursiveProperties();

  // FIXME: If we ever have first-class polymorphic values, we'll need to
  // revisit this.
  if (genericSig)
    properties.removeHasTypeParameter();

  auto fnType =
    new (mem) SILFunctionType(genericSig, ext, callee, params, allResults,
                              directResults, indirectResults, errorResult,
                              ctx, properties);
  ctx.Impl.SILFunctionTypes.InsertNode(fnType, insertPos);
  return CanSILFunctionType(fnType);
}


ArraySliceType *ArraySliceType::get(Type base) {
  auto properties = base->getRecursiveProperties();
  auto arena = getArena(properties);

  const ASTContext &C = base->getASTContext();

  ArraySliceType *&entry = C.Impl.getArena(arena).ArraySliceTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) ArraySliceType(C, base, properties);
}

DictionaryType *DictionaryType::get(Type keyType, Type valueType) {
  auto properties = keyType->getRecursiveProperties() 
                  | valueType->getRecursiveProperties();
  auto arena = getArena(properties);

  const ASTContext &C = keyType->getASTContext();

  DictionaryType *&entry
    = C.Impl.getArena(arena).DictionaryTypes[{keyType, valueType}];
  if (entry) return entry;

  return entry = new (C, arena) DictionaryType(C, keyType, valueType, 
                                               properties);
}

Type OptionalType::get(OptionalTypeKind which, Type valueType) {
  switch (which) {
  // It wouldn't be unreasonable for this method to just ignore
  // OTK_None if we made code more convenient to write.
  case OTK_None: llvm_unreachable("building a non-optional type!");
  case OTK_Optional: return OptionalType::get(valueType);
  case OTK_ImplicitlyUnwrappedOptional: return ImplicitlyUnwrappedOptionalType::get(valueType);
  }
  llvm_unreachable("bad optional type kind");
}

OptionalType *OptionalType::get(Type base) {
  auto properties = base->getRecursiveProperties();
  auto arena = getArena(properties);

  const ASTContext &C = base->getASTContext();

  OptionalType *&entry = C.Impl.getArena(arena).OptionalTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) OptionalType(C, base, properties);
}

ImplicitlyUnwrappedOptionalType *ImplicitlyUnwrappedOptionalType::get(Type base) {
  auto properties = base->getRecursiveProperties();
  auto arena = getArena(properties);

  const ASTContext &C = base->getASTContext();

  auto *&entry = C.Impl.getArena(arena).ImplicitlyUnwrappedOptionalTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) ImplicitlyUnwrappedOptionalType(C, base, properties);
}

ProtocolType *ProtocolType::get(ProtocolDecl *D, const ASTContext &C) {
  // Protocol types can never be nested inside other types, but we should
  // model this anyway to fix some compiler crashes when computing
  // substitutions on invalid code.
  Type Parent;

  llvm::FoldingSetNodeID id;
  ProtocolType::Profile(id, D, Parent);

  RecursiveTypeProperties properties;
  if (Parent) properties |= Parent->getRecursiveProperties();
  auto arena = getArena(properties);

  void *insertPos = 0;
  if (auto protoTy
        = C.Impl.getArena(arena).ProtocolTypes.FindNodeOrInsertPos(id, insertPos))
    return protoTy;

  auto protoTy = new (C, arena) ProtocolType(D, C);
  C.Impl.getArena(arena).ProtocolTypes.InsertNode(protoTy, insertPos);

  return protoTy;
}

ProtocolType::ProtocolType(ProtocolDecl *TheDecl, const ASTContext &Ctx)
  : NominalType(TypeKind::Protocol, &Ctx, TheDecl, /*Parent=*/Type(),
                RecursiveTypeProperties()) { }

void ProtocolType::Profile(llvm::FoldingSetNodeID &ID, ProtocolDecl *D,
                           Type Parent) {
  ID.AddPointer(D);
  ID.AddPointer(Parent.getPointer());
}

LValueType *LValueType::get(Type objectTy) {
  assert(!objectTy->is<ErrorType>() &&
         "cannot have ErrorType wrapped inside LValueType");
  assert(!objectTy->is<LValueType>() && !objectTy->is<InOutType>() &&
         "cannot have 'inout' or @lvalue wrapped inside an @lvalue");

  auto properties = objectTy->getRecursiveProperties()
                    | RecursiveTypeProperties::IsLValue;
  auto arena = getArena(properties);

  auto &C = objectTy->getASTContext();
  auto &entry = C.Impl.getArena(arena).LValueTypes[objectTy];
  if (entry)
    return entry;

  const ASTContext *canonicalContext = objectTy->isCanonical() ? &C : nullptr;
  return entry = new (C, arena) LValueType(objectTy, canonicalContext,
                                           properties);
}

InOutType *InOutType::get(Type objectTy) {
  assert(!objectTy->is<ErrorType>() &&
         "cannot have ErrorType wrapped inside InOutType");
  assert(!objectTy->is<LValueType>() && !objectTy->is<InOutType>() &&
         "cannot have 'inout' or @lvalue wrapped inside an 'inout'");

  auto properties = objectTy->getRecursiveProperties() |
                     RecursiveTypeProperties::HasInOut;

  properties &= ~RecursiveTypeProperties::IsLValue;
  auto arena = getArena(properties);

  auto &C = objectTy->getASTContext();
  auto &entry = C.Impl.getArena(arena).InOutTypes[objectTy];
  if (entry)
    return entry;

  const ASTContext *canonicalContext = objectTy->isCanonical() ? &C : nullptr;
  return entry = new (C, arena) InOutType(objectTy, canonicalContext,
                                          properties);
}

/// Return a uniqued substituted type.
SubstitutedType *SubstitutedType::get(Type Original, Type Replacement,
                                      const ASTContext &C) {
  auto properties = Replacement->getRecursiveProperties();
  auto arena = getArena(properties);

  SubstitutedType *&Known
    = C.Impl.getArena(arena).SubstitutedTypes[{Original, Replacement}];
  if (!Known) {
    Known = new (C, arena) SubstitutedType(Original, Replacement,
                                           properties);
  }
  return Known;
}

DependentMemberType *DependentMemberType::get(Type base, Identifier name,
                                              const ASTContext &ctx) {
  auto properties = base->getRecursiveProperties();
  properties |= RecursiveTypeProperties::HasTypeParameter;
  auto arena = getArena(properties);

  llvm::PointerUnion<Identifier, AssociatedTypeDecl *> stored(name);
  auto *&known = ctx.Impl.getArena(arena).DependentMemberTypes[
                                            {base, stored.getOpaqueValue()}];
  if (!known) {
    const ASTContext *canonicalCtx = base->isCanonical() ? &ctx : nullptr;
    known = new (ctx, arena) DependentMemberType(base, name, canonicalCtx,
                                                 properties);
  }
  return known;
}

DependentMemberType *DependentMemberType::get(Type base,
                                              AssociatedTypeDecl *assocType,
                                              const ASTContext &ctx) {
  auto properties = base->getRecursiveProperties();
  properties |= RecursiveTypeProperties::HasTypeParameter;
  auto arena = getArena(properties);

  llvm::PointerUnion<Identifier, AssociatedTypeDecl *> stored(assocType);
  auto *&known = ctx.Impl.getArena(arena).DependentMemberTypes[
                                            {base, stored.getOpaqueValue()}];
  if (!known) {
    const ASTContext *canonicalCtx = base->isCanonical() ? &ctx : nullptr;
    known = new (ctx, arena) DependentMemberType(base, assocType, canonicalCtx,
                                                 properties);
  }
  return known;
}

CanArchetypeType ArchetypeType::getOpened(Type existential,
                                        Optional<UUID> knownID) {
  auto &ctx = existential->getASTContext();
  auto &openedExistentialArchetypes = ctx.Impl.OpenedExistentialArchetypes;
  // If we know the ID already...
  if (knownID) {
    // ... and we already have an archetype for that ID, return it.
    auto found = openedExistentialArchetypes.find(*knownID);
    
    if (found != openedExistentialArchetypes.end()) {
      auto result = found->second;
      assert(result->getOpenedExistentialType()->isEqual(existential) &&
             "Retrieved the wrong opened existential type?");
      return CanArchetypeType(result);
    }
  } else {
    // Create a new ID.
    knownID = UUID::fromTime();
  }

  auto arena = AllocationArena::Permanent;
  llvm::SmallVector<ProtocolDecl *, 4> conformsTo;
  assert(existential->isExistentialType());
  existential->getAnyExistentialTypeProtocols(conformsTo);

  // Tail-allocate space for the UUID.
  void *archetypeBuf = ctx.Allocate(totalSizeToAlloc<UUID>(1),
                                    alignof(ArchetypeType), arena);
  
  auto result = ::new (archetypeBuf) ArchetypeType(ctx, existential,
                                       ctx.AllocateCopy(conformsTo),
                                       existential->getSuperclass(nullptr));
  result->setOpenedExistentialID(*knownID);
  openedExistentialArchetypes[*knownID] = result;

  return CanArchetypeType(result);
}

CanType ArchetypeType::getAnyOpened(Type existential) {
  if (auto metatypeTy = existential->getAs<ExistentialMetatypeType>()) {
    auto instanceTy = metatypeTy->getInstanceType();
    return CanMetatypeType::get(ArchetypeType::getAnyOpened(instanceTy));
  }
  assert(existential->isExistentialType());
  return ArchetypeType::getOpened(existential);
}

void *ExprHandle::operator new(size_t Bytes, ASTContext &C,
                            unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

ExprHandle *ExprHandle::get(ASTContext &Context, Expr *E) {
  return new (Context) ExprHandle(E);
}

void TypeLoc::setInvalidType(ASTContext &C) {
  TAndValidBit.setPointerAndInt(ErrorType::get(C), true);
}

namespace {
class raw_capturing_ostream : public raw_ostream {
  std::string Message;
  uint64_t Pos;
  CapturingTypeCheckerDebugConsumer &Listener;

public:
  raw_capturing_ostream(CapturingTypeCheckerDebugConsumer &Listener)
      : Listener(Listener) {}

  ~raw_capturing_ostream() {
    flush();
  }

  void write_impl(const char *Ptr, size_t Size) override {
    Message.append(Ptr, Size);
    Pos += Size;

    // Check if we have at least one complete line.
    size_t LastNewline = StringRef(Message).rfind('\n');
    if (LastNewline == StringRef::npos)
      return;
    Listener.handleMessage(StringRef(Message.data(), LastNewline + 1));
    Message.erase(0, LastNewline + 1);
  }

  uint64_t current_pos() const override {
    return Pos;
  }
};
} // unnamed namespace

TypeCheckerDebugConsumer::~TypeCheckerDebugConsumer() { }

CapturingTypeCheckerDebugConsumer::CapturingTypeCheckerDebugConsumer()
    : Log(new raw_capturing_ostream(*this)) {
  Log->SetUnbuffered();
}

CapturingTypeCheckerDebugConsumer::~CapturingTypeCheckerDebugConsumer() {
  delete Log;
}

void GenericSignature::Profile(llvm::FoldingSetNodeID &ID,
                               ArrayRef<GenericTypeParamType *> genericParams,
                               ArrayRef<Requirement> requirements) {
  for (auto p : genericParams)
    ID.AddPointer(p);

  for (auto &reqt : requirements) {
    ID.AddPointer(reqt.getFirstType().getPointer());
    if (reqt.getKind() != RequirementKind::WitnessMarker)
      ID.AddPointer(reqt.getSecondType().getPointer());
    ID.AddInteger(unsigned(reqt.getKind()));
  }
}

GenericSignature *GenericSignature::get(ArrayRef<GenericTypeParamType *> params,
                                        ArrayRef<Requirement> requirements,
                                        bool isKnownCanonical) {
  if (params.empty() && requirements.empty())
    return nullptr;

  // Check for an existing generic signature.
  llvm::FoldingSetNodeID ID;
  GenericSignature::Profile(ID, params, requirements);

  auto &ctx = getASTContext(params, requirements);
  void *insertPos;
  if (auto *sig = ctx.Impl.GenericSignatures.FindNodeOrInsertPos(ID,
                                                                 insertPos)) {
    if (isKnownCanonical)
      sig->CanonicalSignatureOrASTContext = &ctx;

    return sig;
  }

  // Allocate and construct the new signature.
  size_t bytes = totalSizeToAlloc<GenericTypeParamType *, Requirement>(
      params.size(), requirements.size());
  void *mem = ctx.Allocate(bytes, alignof(GenericSignature));
  auto newSig = new (mem) GenericSignature(params, requirements,
                                           isKnownCanonical);
  ctx.Impl.GenericSignatures.InsertNode(newSig, insertPos);
  return newSig;
}

void DeclName::CompoundDeclName::Profile(llvm::FoldingSetNodeID &id,
                                         Identifier baseName,
                                         ArrayRef<Identifier> argumentNames) {
  id.AddPointer(baseName.get());
  id.AddInteger(argumentNames.size());
  for (auto arg : argumentNames)
    id.AddPointer(arg.get());
}

void DeclName::initialize(ASTContext &C, Identifier baseName,
                          ArrayRef<Identifier> argumentNames) {
  if (argumentNames.size() == 0) {
    SimpleOrCompound = IdentifierAndCompound(baseName, true);
    return;
  }

  llvm::FoldingSetNodeID id;
  CompoundDeclName::Profile(id, baseName, argumentNames);

  void *insert = nullptr;
  if (CompoundDeclName *compoundName
        = C.Impl.CompoundNames.FindNodeOrInsertPos(id, insert)) {
    SimpleOrCompound = compoundName;
    return;
  }

  size_t size =
      CompoundDeclName::totalSizeToAlloc<Identifier>(argumentNames.size());
  auto buf = C.Allocate(size, alignof(CompoundDeclName));
  auto compoundName = new (buf) CompoundDeclName(baseName,argumentNames.size());
  std::uninitialized_copy(argumentNames.begin(), argumentNames.end(),
                          compoundName->getArgumentNames().begin());
  SimpleOrCompound = compoundName;
  C.Impl.CompoundNames.InsertNode(compoundName, insert);
}

/// Build a compound value name given a base name and a set of argument names
/// extracted from a parameter list.
DeclName::DeclName(ASTContext &C, Identifier baseName,
                   ParameterList *paramList) {
  SmallVector<Identifier, 4> names;
  
  for (auto P : *paramList)
    names.push_back(P->getArgumentName());
  initialize(C, baseName, names);
}

/// Find the implementation of the named type in the given module.
static NominalTypeDecl *findUnderlyingTypeInModule(ASTContext &ctx, 
                                                   Identifier name,
                                                   Module *module) {
  // Find all of the declarations with this name in the Swift module.
  SmallVector<ValueDecl *, 1> results;
  module->lookupValue({ }, name, NLKind::UnqualifiedLookup, results);
  for (auto result : results) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(result))
      return nominal;

    // Look through typealiases.
    if (auto typealias = dyn_cast<TypeAliasDecl>(result)) {
      if (auto resolver = ctx.getLazyResolver())
        resolver->resolveDeclSignature(typealias);
      return typealias->getUnderlyingType()->getAnyNominal();
    }
  }

  return nullptr;
}

ForeignRepresentationInfo
ASTContext::getForeignRepresentationInfo(NominalTypeDecl *nominal,
                                         ForeignLanguage language,
                                         const DeclContext *dc) {
  if (Impl.ForeignRepresentableCache.empty()) {
    // Local function to add a type with the given name and module as
    // trivially-representable.
    auto addTrivial = [&](Identifier name, Module *module,
                          bool allowOptional = false) {
      if (auto type = findUnderlyingTypeInModule(*this, name, module)) {
        auto info = ForeignRepresentationInfo::forTrivial();
        if (allowOptional)
          info = ForeignRepresentationInfo::forTrivialWithOptional();
        Impl.ForeignRepresentableCache.insert({type, info});
      }
    };

    // Pre-populate the foreign-representable cache with known types.
    if (auto stdlib = getStdlibModule()) {
      addTrivial(getIdentifier("OpaquePointer"), stdlib, true);

      // Builtin types
      // FIXME: Layering violation to use the ClangImporter's define.
#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME) \
      addTrivial(getIdentifier(#SWIFT_TYPE_NAME), stdlib);
#include "swift/ClangImporter/BuiltinMappedTypes.def"
    }

    if (auto darwin = getLoadedModule(Id_Darwin)) {
      // Note: DarwinBoolean is odd because it's bridged to Bool in APIs,
      // but can also be trivially bridged.
      addTrivial(getIdentifier("DarwinBoolean"), darwin);
    }

    if (auto objectiveC = getLoadedModule(Id_ObjectiveC)) {
      addTrivial(Id_Selector, objectiveC, true);

      // Note: ObjCBool is odd because it's bridged to Bool in APIs,
      // but can also be trivially bridged.
      addTrivial(getIdentifier("ObjCBool"), objectiveC);

      addTrivial(getSwiftId(KnownFoundationEntity::NSZone), objectiveC, true);
    }

    if (auto coreGraphics = getLoadedModule(getIdentifier("CoreGraphics"))) {
      addTrivial(Id_CGFloat, coreGraphics);
    }

    // Pull SIMD types of size 2...4 from the SIMD module, if it exists.
    // FIXME: Layering violation to use the ClangImporter's define.
    const unsigned SWIFT_MAX_IMPORTED_SIMD_ELEMENTS = 4;
    if (auto simd = getLoadedModule(Id_simd)) {
#define MAP_SIMD_TYPE(BASENAME, _, __)                                  \
      {                                                                 \
        char name[] = #BASENAME "0";                                    \
        for (unsigned i = 2; i <= SWIFT_MAX_IMPORTED_SIMD_ELEMENTS; ++i) { \
          *(std::end(name) - 2) = '0' + i;                              \
          addTrivial(getIdentifier(name), simd);                        \
        }                                                               \
      }
#include "swift/ClangImporter/SIMDMappedTypes.def"      
    }
  }

  // Determine whether we know anything about this nominal type
  // yet. If we've never seen this nominal type before, or if we have
  // an out-of-date negative cached value, we'll have to go looking.
  auto known = Impl.ForeignRepresentableCache.find(nominal);
  if (known == Impl.ForeignRepresentableCache.end() ||
      (known->second.getKind() == ForeignRepresentableKind::None &&
       known->second.getGeneration() < CurrentGeneration)) {
    Optional<ForeignRepresentationInfo> result;

    // Look for a conformance to _ObjectiveCBridgeable.
    //
    // FIXME: We're implicitly depending on the fact that lookupConformance
    // is global, ignoring the module we provide for it.
    if (auto objcBridgeable
          = getProtocol(KnownProtocolKind::ObjectiveCBridgeable)) {
      if (auto conformance
            = dc->getParentModule()->lookupConformance(
                nominal->getDeclaredType(), objcBridgeable,
                getLazyResolver())) {
        result =
            ForeignRepresentationInfo::forBridged(conformance->getConcrete());
      }
    }

    // Error is bridged to NSError, when it's available.
    if (nominal == getErrorDecl() && getNSErrorDecl())
      result = ForeignRepresentationInfo::forBridgedError();

    // If we didn't find anything, mark the result as "None".
    if (!result)
      result = ForeignRepresentationInfo::forNone(CurrentGeneration);
    
    // Cache the result.
    known = Impl.ForeignRepresentableCache.insert({ nominal, *result }).first;
  }

  // Map a cache entry to a result for this specific 
  auto entry = known->second;
  if (entry.getKind() == ForeignRepresentableKind::None)
    return entry;

  // Extract the protocol conformance.
  auto conformance = entry.getConformance();

  // If the conformance is not visible, fail.
  if (conformance && !conformance->isVisibleFrom(dc))
    return ForeignRepresentationInfo::forNone();

  // Language-specific filtering.
  switch (language) {
  case ForeignLanguage::C:
    // Ignore _ObjectiveCBridgeable conformances in C.
    if (conformance &&
        conformance->getProtocol()->isSpecificProtocol(
          KnownProtocolKind::ObjectiveCBridgeable))
      return ForeignRepresentationInfo::forNone();

    // Ignore error bridging in C.
    if (entry.getKind() == ForeignRepresentableKind::BridgedError)
      return ForeignRepresentationInfo::forNone();

    SWIFT_FALLTHROUGH;

  case ForeignLanguage::ObjectiveC:
    return entry;
  }
}

bool ASTContext::isStandardLibraryTypeBridgedInFoundation(
     NominalTypeDecl *nominal) const {
  return (nominal == getBoolDecl() ||
          nominal == getIntDecl() ||
          nominal == getUIntDecl() ||
          nominal == getFloatDecl() ||
          nominal == getDoubleDecl() ||
          nominal == getArrayDecl() ||
          nominal == getDictionaryDecl() ||
          nominal == getSetDecl() ||
          nominal == getStringDecl() ||
          nominal == getErrorDecl() ||
          // Weird one-off case where CGFloat is bridged to NSNumber.
          nominal->getName() == Id_CGFloat);
}

Optional<Type>
ASTContext::getBridgedToObjC(const DeclContext *dc, Type type,
                             LazyResolver *resolver,
                             Type *bridgedValueType) const {
  if (type->isBridgeableObjectType()) {
    if (bridgedValueType) *bridgedValueType = type;

    return type;
  }

  // Whitelist certain types even if Foundation is not imported, to ensure
  // that casts from AnyObject to one of these types are not optimized away.
  //
  // Outside of these standard library types to which Foundation
  // bridges, an _ObjectiveCBridgeable conformance can only be added
  // in the same module where the Swift type itself is defined, so the
  // optimizer will be guaranteed to see the conformance if it exists.
  bool knownBridgedToObjC = false;
  if (auto ntd = type->getAnyNominal())
    knownBridgedToObjC = isStandardLibraryTypeBridgedInFoundation(ntd);

  // TODO: Under id-as-any, container bridging is unconstrained. This check can
  // go away.
  if (!LangOpts.EnableIdAsAny) {
    // If the type is generic, check whether its generic arguments are also
    // bridged to Objective-C.
    if (auto bgt = type->getAs<BoundGenericType>()) {
      for (auto arg : bgt->getGenericArgs()) {
        if (arg->hasTypeVariable())
          continue;

        if (!getBridgedToObjC(dc, arg, resolver))
          return None;
      }
    }
  }

  if (auto metaTy = type->getAs<MetatypeType>())
    if (metaTy->getInstanceType()->mayHaveSuperclass())
      return type;

  if (auto existentialMetaTy = type->getAs<ExistentialMetatypeType>())
    if (existentialMetaTy->getInstanceType()->isObjCExistentialType())
      return type;

  // Check whether the type is an existential that contains
  // Error. If so, it's bridged to NSError.
  if (type->isExistentialWithError()) {
    if (auto nsErrorDecl = getNSErrorDecl()) {
      // The corresponding value type is Error.
      if (bridgedValueType)
        *bridgedValueType = getErrorDecl()->getDeclaredInterfaceType();

      return nsErrorDecl->getDeclaredInterfaceType();
    }
  }

  // Try to find a conformance that will enable bridging.
  auto findConformance =
    [&](KnownProtocolKind known) -> Optional<ProtocolConformanceRef> {
      // Find the protocol.
      auto proto = getProtocol(known);
      if (!proto) return None;

      return dc->getParentModule()->lookupConformance(type, proto, resolver);
    };

  // Do we conform to _ObjectiveCBridgeable?
  if (auto conformance
        = findConformance(KnownProtocolKind::ObjectiveCBridgeable)) {
    // The corresponding value type is... the type.
    if (bridgedValueType)
      *bridgedValueType = type;

    // Find the Objective-C class type we bridge to.
    return ProtocolConformance::getTypeWitnessByName(
             type, conformance->getConcrete(), Id_ObjectiveCType,
             resolver);
  }

  // Do we conform to Error?
  if (findConformance(KnownProtocolKind::Error)) {
    // The corresponding value type is Error.
    if (bridgedValueType)
      *bridgedValueType = getErrorDecl()->getDeclaredInterfaceType();

    // Bridge to NSError.
    if (auto nsErrorDecl = getNSErrorDecl())
      return nsErrorDecl->getDeclaredInterfaceType();
  }


  // If we haven't imported Foundation but this is a whitelisted type,
  // behave as above.
  if (knownBridgedToObjC)
    return Type();
  return None;
}

std::pair<ArchetypeBuilder *, ArchetypeBuilder::PotentialArchetype *>
ASTContext::getLazyArchetype(const ArchetypeType *archetype) {
  auto known = Impl.LazyArchetypes.find(archetype);
  assert(known != Impl.LazyArchetypes.end());
  return known->second;
}

void ASTContext::registerLazyArchetype(
       const ArchetypeType *archetype,
       ArchetypeBuilder &builder,
       ArchetypeBuilder::PotentialArchetype *potentialArchetype) {
  assert(Impl.LazyArchetypes.count(archetype) == 0);
  Impl.LazyArchetypes[archetype] = { &builder, potentialArchetype };
}

void ASTContext::unregisterLazyArchetype(const ArchetypeType *archetype) {
  auto known = Impl.LazyArchetypes.find(archetype);
  assert(known != Impl.LazyArchetypes.end());
  Impl.LazyArchetypes.erase(known);
}

const InheritedNameSet *ASTContext::getAllPropertyNames(ClassDecl *classDecl,
                                                        bool forInstance) {
  // If this class was defined in Objective-C, perform the lookup based on
  // the Objective-C class.
  if (auto objcClass = dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                         classDecl->getClangDecl())) {
    return getAllPropertyNames(
             const_cast<clang::ObjCInterfaceDecl *>(objcClass),
             forInstance);
  }

  // If we already have this information, return it.
  auto known = Impl.AllProperties.find({classDecl, forInstance});
  if (known != Impl.AllProperties.end()) return known->second.get();

  // Otherwise, get information from our superclass first.
  if (auto resolver = getLazyResolver())
    resolver->resolveSuperclass(classDecl);

  const InheritedNameSet *parentSet = nullptr;
  if (auto superclass = classDecl->getSuperclass()) {
    if (auto superclassDecl = superclass->getClassOrBoundGenericClass()) {
      parentSet = getAllPropertyNames(superclassDecl, forInstance);
    }
  }

  // Create the set of properties.
  known = Impl.AllProperties.insert(
            { std::pair<const ClassDecl *, char>(classDecl, forInstance),
              llvm::make_unique<InheritedNameSet>(parentSet) }).first;

  // Local function to add properties from the given set.
  auto addProperties = [&](DeclRange members) {
    for (auto member : members) {
      auto var = dyn_cast<VarDecl>(member);
      if (!var || var->getName().empty()) continue;
      if (var->isInstanceMember() != forInstance) continue;

      known->second->add(var->getName().str());
    }
  };

  // Collect property names from the class.
  addProperties(classDecl->getMembers());

  // Collect property names from all extensions in the same module as the class.
  auto module = classDecl->getParentModule();
  for (auto ext : classDecl->getExtensions()) {
    if (ext->getParentModule() != module) continue;
    addProperties(ext->getMembers());
  }

  return known->second.get();
}

const InheritedNameSet *ASTContext::getAllPropertyNames(
                          clang::ObjCInterfaceDecl *classDecl,
                          bool forInstance) {
  classDecl = classDecl->getCanonicalDecl();

  // If we already have this information, return it.
  auto known = Impl.AllPropertiesObjC.find({classDecl, forInstance});
  if (known != Impl.AllPropertiesObjC.end()) return known->second.get();

  // Otherwise, get information from our superclass first.
  const InheritedNameSet *parentSet = nullptr;
  if (auto superclassDecl = classDecl->getSuperClass()) {
    parentSet = getAllPropertyNames(superclassDecl, forInstance);
  }

  // Create the set of properties.
  known = Impl.AllPropertiesObjC.insert(
            { std::pair<const clang::ObjCInterfaceDecl *, char>(classDecl,
                                                                forInstance),
              llvm::make_unique<InheritedNameSet>(parentSet) }).first;

  // Local function to add properties from the given set.
  auto addProperties = [&](clang::DeclContext::decl_range members) {
    for (auto member : members) {
      // Add Objective-C property names.
      if (auto property = dyn_cast<clang::ObjCPropertyDecl>(member)) {
        if (forInstance)
          known->second->add(property->getName());
        continue;
      }

      // Add no-parameter, non-void method names.
      if (auto method = dyn_cast<clang::ObjCMethodDecl>(member)) {
        if (method->getSelector().isUnarySelector() &&
            !method->getReturnType()->isVoidType() &&
            !method->hasRelatedResultType() &&
            method->isInstanceMethod() == forInstance) {
          known->second->add(method->getSelector().getNameForSlot(0));
          continue;
        }
      }
    }
  };

  // Dig out the class definition.
  auto classDef = classDecl->getDefinition();
  if (!classDef) return known->second.get();

  // Collect property names from the class definition.
  addProperties(classDef->decls());

  // Dig out the module that owns the class definition.
  auto module = classDef->getImportedOwningModule();
  if (module) module = module->getTopLevelModule();

  // Collect property names from all categories and extensions in the same
  // module as the class.
  for (auto category : classDef->known_categories()) {
    auto categoryModule = category->getImportedOwningModule();
    if (categoryModule) categoryModule = categoryModule->getTopLevelModule();
    if (module != categoryModule) continue;

    addProperties(category->decls());
  }

  return known->second.get();
}
