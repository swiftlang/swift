//===--- ASTContext.cpp - ASTContext Implementation -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the ASTContext class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "ForeignRepresentationInfo.h"
#include "SubstitutionMapStorage.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/RawComment.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/SILLayout.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeCheckerDebugConsumer.h"
#include "swift/Basic/Compiler.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h" // bad dependency
#include "swift/Syntax/SyntaxArena.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Compiler.h"
#include <algorithm>
#include <memory>

using namespace swift;

#define DEBUG_TYPE "ASTContext"
STATISTIC(NumRegisteredGenericSignatureBuilders,
          "# of generic signature builders successfully registered");
STATISTIC(NumRegisteredGenericSignatureBuildersAlready,
          "# of generic signature builders already registered");
STATISTIC(NumCollapsedSpecializedProtocolConformances,
          "# of specialized protocol conformances collapsed");

/// Define this to 1 to enable expensive assertions of the
/// GenericSignatureBuilder.
#define SWIFT_GSB_EXPENSIVE_ASSERTIONS 0

LazyResolver::~LazyResolver() = default;
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
} // end anonymous namespace

using AssociativityCacheType =
  llvm::DenseMap<std::pair<PrecedenceGroupDecl *, PrecedenceGroupDecl *>,
                 Associativity>;

#define FOR_KNOWN_FOUNDATION_TYPES(MACRO) \
  MACRO(NSError) \
  MACRO(NSNumber) \
  MACRO(NSValue)

struct ASTContext::Implementation {
  Implementation();
  ~Implementation();

  llvm::BumpPtrAllocator Allocator; // used in later initializations

  /// The set of cleanups to be called when the ASTContext is destroyed.
  std::vector<std::function<void(void)>> Cleanups;

  /// The last resolver.
  LazyResolver *Resolver = nullptr;

  /// The lazy parsers for various input files. We may have separate
  /// lazy parsers for imported module files and source files.
  llvm::SmallPtrSet<LazyMemberParser*, 2> lazyParsers;

  // FIXME: This is a StringMap rather than a StringSet because StringSet
  // doesn't allow passing in a pre-existing allocator.
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

  /// The AnyObject type.
  CanType AnyObjectType;

#define KNOWN_STDLIB_TYPE_DECL(NAME, DECL_CLASS, NUM_GENERIC_PARAMS) \
  /** The declaration of Swift.NAME. */ \
  DECL_CLASS *NAME##Decl = nullptr;
#include "swift/AST/KnownStdlibTypes.def"

  /// The declaration of '+' function for two RangeReplaceableCollection.
  FuncDecl *PlusFunctionOnRangeReplaceableCollection = nullptr;

  /// The declaration of '+' function for two String.
  FuncDecl *PlusFunctionOnString = nullptr;

  /// The declaration of Swift.Optional<T>.Some.
  EnumElementDecl *OptionalSomeDecl = nullptr;

  /// The declaration of Swift.Optional<T>.None.
  EnumElementDecl *OptionalNoneDecl = nullptr;

  /// The declaration of Swift.UnsafeMutableRawPointer.memory.
  VarDecl *UnsafeMutableRawPointerMemoryDecl = nullptr;

  /// The declaration of Swift.UnsafeRawPointer.memory.
  VarDecl *UnsafeRawPointerMemoryDecl = nullptr;

  /// The declaration of Swift.UnsafeMutablePointer<T>.memory.
  VarDecl *UnsafeMutablePointerMemoryDecl = nullptr;
  
  /// The declaration of Swift.UnsafePointer<T>.memory.
  VarDecl *UnsafePointerMemoryDecl = nullptr;
  
  /// The declaration of Swift.AutoreleasingUnsafeMutablePointer<T>.memory.
  VarDecl *AutoreleasingUnsafeMutablePointerMemoryDecl = nullptr;

  /// The declaration of Swift.Void.
  TypeAliasDecl *VoidDecl = nullptr;
  
  /// The declaration of ObjectiveC.ObjCBool.
  StructDecl *ObjCBoolDecl = nullptr;

#define CACHE_FOUNDATION_DECL(NAME) \
  /** The declaration of Foundation.NAME. */ \
  ClassDecl *NAME##Decl = nullptr;
FOR_KNOWN_FOUNDATION_TYPES(CACHE_FOUNDATION_DECL)
#undef CACHE_FOUNDATION_DECL

  // Declare cached declarations for each of the known declarations.
#define FUNC_DECL(Name, Id) FuncDecl *Get##Name = nullptr;
#include "swift/AST/KnownDecls.def"
  
  /// func ==(Int, Int) -> Bool
  FuncDecl *EqualIntDecl = nullptr;

  /// func _hashValue<H: Hashable>(for: H) -> Int
  FuncDecl *HashValueForDecl = nullptr;

  /// func append(Element) -> void
  FuncDecl *ArrayAppendElementDecl = nullptr;

  /// func reserveCapacityForAppend(newElementsCount: Int)
  FuncDecl *ArrayReserveCapacityDecl = nullptr;

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

  /// Map from normal protocol conformances to missing witnesses that have
  /// been delayed until the conformance is fully checked, so that we can
  /// issue a fixit that fills the entire protocol stub.
  llvm::DenseMap<NormalProtocolConformance *, std::vector<ValueDecl*>>
    DelayedMissingWitnesses;

  /// Stores information about lazy deserialization of various declarations.
  llvm::DenseMap<const DeclContext *, LazyContextData *> LazyContexts;

  /// Stored generic signature builders for canonical generic signatures.
  llvm::DenseMap<GenericSignature *, std::unique_ptr<GenericSignatureBuilder>>
    GenericSignatureBuilders;

  /// Canonical generic environments for canonical generic signatures.
  ///
  /// The keys are the generic signature builders in \c GenericSignatureBuilders.
  llvm::DenseMap<GenericSignatureBuilder *, GenericEnvironment *>
    CanonicalGenericEnvironments;
  
  /// The single-parameter generic signature with no constraints, <T>.
  CanGenericSignature SingleGenericParameterSignature;

  /// The existential signature <T : P> for each P.
  llvm::DenseMap<CanType, CanGenericSignature> ExistentialSignatures;

  /// Overridden declarations.
  llvm::DenseMap<const ValueDecl *, ArrayRef<ValueDecl *>> Overrides;

  /// Default witnesses.
  llvm::DenseMap<std::pair<const ProtocolDecl *, ValueDecl *>, Witness>
    DefaultWitnesses;

  /// Default type witnesses for protocols.
  llvm::DenseMap<std::pair<const ProtocolDecl *, AssociatedTypeDecl *>, Type>
    DefaultTypeWitnesses;

  /// Default associated conformance witnesses for protocols.
  llvm::DenseMap<std::tuple<const ProtocolDecl *, CanType, ProtocolDecl *>,
                 ProtocolConformanceRef>
    DefaultAssociatedConformanceWitnesses;

  /// \brief Structure that captures data that is segregated into different
  /// arenas.
  struct Arena {
    llvm::DenseMap<Type, ErrorType *> ErrorTypesWithOriginal;
    llvm::FoldingSet<NameAliasType> NameAliasTypes;
    llvm::FoldingSet<TupleType> TupleTypes;
    llvm::DenseMap<std::pair<Type,char>, MetatypeType*> MetatypeTypes;
    llvm::DenseMap<std::pair<Type,char>,
                   ExistentialMetatypeType*> ExistentialMetatypeTypes;
    llvm::DenseMap<Type, ArraySliceType*> ArraySliceTypes;
    llvm::DenseMap<std::pair<Type, Type>, DictionaryType *> DictionaryTypes;
    llvm::DenseMap<Type, OptionalType*> OptionalTypes;
    llvm::DenseMap<std::pair<Type, unsigned>, ParenType*> ParenTypes;
    llvm::DenseMap<uintptr_t, ReferenceStorageType*> ReferenceStorageTypes;
    llvm::DenseMap<Type, LValueType*> LValueTypes;
    llvm::DenseMap<Type, InOutType*> InOutTypes;
    llvm::DenseMap<std::pair<Type, void*>, DependentMemberType *>
      DependentMemberTypes;
    llvm::DenseMap<Type, DynamicSelfType *> DynamicSelfTypes;
    llvm::FoldingSet<EnumType> EnumTypes;
    llvm::FoldingSet<StructType> StructTypes;
    llvm::FoldingSet<ClassType> ClassTypes;
    llvm::FoldingSet<UnboundGenericType> UnboundGenericTypes;
    llvm::FoldingSet<BoundGenericType> BoundGenericTypes;
    llvm::FoldingSet<ProtocolType> ProtocolTypes;
    llvm::FoldingSet<ProtocolCompositionType> ProtocolCompositionTypes;
    llvm::FoldingSet<LayoutConstraintInfo> LayoutConstraints;

    /// The set of function types.
    llvm::FoldingSet<FunctionType> FunctionTypes;

    /// The set of normal protocol conformances.
    llvm::FoldingSet<NormalProtocolConformance> NormalConformances;

    // The set of self protocol conformances.
    llvm::DenseMap<ProtocolDecl*, SelfProtocolConformance*> SelfConformances;

    /// The set of specialized protocol conformances.
    llvm::FoldingSet<SpecializedProtocolConformance> SpecializedConformances;

    /// The set of inherited protocol conformances.
    llvm::FoldingSet<InheritedProtocolConformance> InheritedConformances;

    /// The set of substitution maps (uniqued by their storage).
    llvm::FoldingSet<SubstitutionMap::Storage> SubstitutionMaps;

    ~Arena() {
      for (auto &conformance : SpecializedConformances)
        conformance.~SpecializedProtocolConformance();
      // Work around MSVC warning: local variable is initialized but
      // not referenced.
#if SWIFT_COMPILER_IS_MSVC
#pragma warning (disable: 4189)
#endif
      for (auto &conformance : InheritedConformances)
        conformance.~InheritedProtocolConformance();
#if SWIFT_COMPILER_IS_MSVC
#pragma warning (default: 4189)
#endif

      // Call the normal conformance destructors last since they could be
      // referenced by the other conformance types.
      for (auto &conformance : NormalConformances)
        conformance.~NormalProtocolConformance();
    }

    size_t getTotalMemory() const;
  };

  llvm::DenseMap<ModuleDecl*, ModuleType*> ModuleTypes;
  llvm::DenseMap<std::pair<unsigned, unsigned>, GenericTypeParamType *>
    GenericParamTypes;
  llvm::FoldingSet<GenericFunctionType> GenericFunctionTypes;
  llvm::FoldingSet<SILFunctionType> SILFunctionTypes;
  llvm::DenseMap<CanType, SILBlockStorageType *> SILBlockStorageTypes;
  llvm::FoldingSet<SILBoxType> SILBoxTypes;
  llvm::DenseMap<BuiltinIntegerWidth, BuiltinIntegerType*> IntegerTypes;
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

    ConstraintSolverArena(llvm::BumpPtrAllocator &allocator)
      : Allocator(allocator) { }

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
  
  llvm::FoldingSet<SILLayout> SILLayouts;

  RC<syntax::SyntaxArena> TheSyntaxArena;
};

ASTContext::Implementation::Implementation()
    : IdentifierTable(Allocator),
      TheSyntaxArena(new syntax::SyntaxArena()) {}
ASTContext::Implementation::~Implementation() {
  delete Resolver;

  for (auto &cleanup : Cleanups)
    cleanup();
}

ConstraintCheckerArenaRAII::
ConstraintCheckerArenaRAII(ASTContext &self, llvm::BumpPtrAllocator &allocator)
  : Self(self), Data(self.getImpl().CurrentConstraintSolverArena.release())
{
  Self.getImpl().CurrentConstraintSolverArena.reset(
    new ASTContext::Implementation::ConstraintSolverArena(allocator));
}

ConstraintCheckerArenaRAII::~ConstraintCheckerArenaRAII() {
  Self.getImpl().CurrentConstraintSolverArena.reset(
    (ASTContext::Implementation::ConstraintSolverArena *)Data);
}

static ModuleDecl *createBuiltinModule(ASTContext &ctx) {
  auto M = ModuleDecl::create(ctx.getIdentifier(BUILTIN_NAME), ctx);
  M->addFile(*new (ctx) BuiltinUnit(*M));
  M->setHasResolvedImports();
  return M;
}

inline ASTContext::Implementation &ASTContext::getImpl() const {
  auto pointer = reinterpret_cast<char*>(const_cast<ASTContext*>(this));
  auto offset = llvm::alignAddr((void*)sizeof(*this), alignof(Implementation));
  return *reinterpret_cast<Implementation*>(pointer + offset);
}

void ASTContext::operator delete(void *Data) throw() {
  AlignedFree(Data);
}

ASTContext *ASTContext::get(LangOptions &langOpts,
                            SearchPathOptions &SearchPathOpts,
                            SourceManager &SourceMgr,
                            DiagnosticEngine &Diags) {
  // If more than two data structures are concatentated, then the aggregate
  // size math needs to become more complicated due to per-struct alignment
  // constraints.
  auto align = std::max(alignof(ASTContext), alignof(Implementation));
  auto size = llvm::alignTo(sizeof(ASTContext) + sizeof(Implementation), align);
  auto mem = AlignedAlloc(size, align);
  auto impl = reinterpret_cast<void*>((char*)mem + sizeof(ASTContext));
  impl = reinterpret_cast<void*>(llvm::alignAddr(impl,alignof(Implementation)));
  new (impl) Implementation();
  return new (mem) ASTContext(langOpts, SearchPathOpts, SourceMgr, Diags);
}

ASTContext::ASTContext(LangOptions &langOpts, SearchPathOptions &SearchPathOpts,
                       SourceManager &SourceMgr, DiagnosticEngine &Diags)
  : LangOpts(langOpts),
    SearchPathOpts(SearchPathOpts),
    SourceMgr(SourceMgr),
    Diags(Diags),
    evaluator(Diags, langOpts.EvaluatorCycleDiagnostics),
    TheBuiltinModule(createBuiltinModule(*this)),
    StdlibModuleName(getIdentifier(STDLIB_NAME)),
    SwiftShimsModuleName(getIdentifier(SWIFT_SHIMS_NAME)),
    TypeCheckerDebug(new StderrTypeCheckerDebugConsumer()),
    TheErrorType(
      new (*this, AllocationArena::Permanent)
        ErrorType(*this, Type(), RecursiveTypeProperties::HasError)),
    TheUnresolvedType(new (*this, AllocationArena::Permanent)
                      UnresolvedType(*this)),
    TheEmptyTupleType(TupleType::get(ArrayRef<TupleTypeElt>(), *this)),
    TheAnyType(ProtocolCompositionType::get(*this, ArrayRef<Type>(),
                                            /*HasExplicitAnyObject=*/false)),
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
    TheSILTokenType(new (*this, AllocationArena::Permanent)
                      SILTokenType(*this)),
    TheIntegerLiteralType(new (*this, AllocationArena::Permanent)
                               BuiltinIntegerLiteralType(*this)),
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
    getImpl().SearchPathsSet[path] |= SearchPathKind::Import;
  for (const auto &framepath : SearchPathOpts.FrameworkSearchPaths)
    getImpl().SearchPathsSet[framepath.Path] |= SearchPathKind::Framework;

  // Register any request-evaluator functions available at the AST layer.
  registerAccessRequestFunctions(evaluator);
  registerNameLookupRequestFunctions(evaluator);
}

ASTContext::~ASTContext() {
  // Emit evaluator dependency graph if requested.
  auto graphPath = LangOpts.RequestEvaluatorGraphVizPath;
  if (!graphPath.empty()) {
    evaluator.emitRequestEvaluatorGraphViz(graphPath);
  }
  getImpl().~Implementation();
}

llvm::BumpPtrAllocator &ASTContext::getAllocator(AllocationArena arena) const {
  switch (arena) {
  case AllocationArena::Permanent:
    return getImpl().Allocator;

  case AllocationArena::ConstraintSolver:
    assert(getImpl().CurrentConstraintSolverArena != nullptr);
    return getImpl().CurrentConstraintSolverArena->Allocator;
  }
  llvm_unreachable("bad AllocationArena");
}

/// Set a new stats reporter.
void ASTContext::setStatsReporter(UnifiedStatsReporter *stats) {
  Stats = stats;
  evaluator.setStatsReporter(stats);

  if (stats) {
    stats->getFrontendCounters().NumASTBytesAllocated =
        getAllocator().getBytesAllocated();
  }
}

RC<syntax::SyntaxArena> ASTContext::getSyntaxArena() const {
  return getImpl().TheSyntaxArena;
}

LazyResolver *ASTContext::getLazyResolver() const {
  return getImpl().Resolver;
}

/// Set the lazy resolver for this context.
void ASTContext::setLazyResolver(LazyResolver *resolver) {
  if (auto existing = getImpl().Resolver)
    delete existing;

  getImpl().Resolver = resolver;
}

void ASTContext::addLazyParser(LazyMemberParser *lazyParser) {
  getImpl().lazyParsers.insert(lazyParser);
}

void ASTContext::removeLazyParser(LazyMemberParser *lazyParser) {
  auto removed = getImpl().lazyParsers.erase(lazyParser);
  (void)removed;
  assert(removed && "Removing an non-existing lazy parser.");
}

/// getIdentifier - Return the uniqued and AST-Context-owned version of the
/// specified string.
Identifier ASTContext::getIdentifier(StringRef Str) const {
  // Make sure null pointers stay null.
  if (Str.data() == nullptr)
    return Identifier(nullptr);

  auto I = getImpl().IdentifierTable.insert(std::make_pair(Str, char())).first;
  return Identifier(I->getKeyData());
}

void ASTContext::lookupInSwiftModule(
                   StringRef name,
                   SmallVectorImpl<ValueDecl *> &results) const {
  ModuleDecl *M = getStdlibModule();
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

FuncDecl *ASTContext::getPlusFunctionOnRangeReplaceableCollection() const {
  if (getImpl().PlusFunctionOnRangeReplaceableCollection) {
    return getImpl().PlusFunctionOnRangeReplaceableCollection;
  }
  // Find all of the declarations with this name in the Swift module.
  SmallVector<ValueDecl *, 1> Results;
  lookupInSwiftModule("+", Results);
  for (auto Result : Results) {
    if (auto *FD = dyn_cast<FuncDecl>(Result)) {
      if (!FD->getOperatorDecl())
        continue;
      for (auto Req: FD->getGenericRequirements()) {
        if (Req.getKind() == RequirementKind::Conformance &&
              Req.getSecondType()->getNominalOrBoundGenericNominal() ==
            getRangeReplaceableCollectionDecl()) {
          getImpl().PlusFunctionOnRangeReplaceableCollection = FD;
        }
      }
    }
  }
  return getImpl().PlusFunctionOnRangeReplaceableCollection;
}

FuncDecl *ASTContext::getPlusFunctionOnString() const {
  if (getImpl().PlusFunctionOnString) {
    return getImpl().PlusFunctionOnString;
  }
  // Find all of the declarations with this name in the Swift module.
  SmallVector<ValueDecl *, 1> Results;
  lookupInSwiftModule("+", Results);
  for (auto Result : Results) {
    if (auto *FD = dyn_cast<FuncDecl>(Result)) {
      if (!FD->getOperatorDecl())
        continue;
      auto ResultType = FD->getResultInterfaceType();
      if (ResultType->getNominalOrBoundGenericNominal() != getStringDecl())
        continue;
      auto ParamList = FD->getParameters();
      if (ParamList->size() != 2)
        continue;
      auto CheckIfStringParam = [this](ParamDecl* Param) {
        auto Type = Param->getInterfaceType()->getNominalOrBoundGenericNominal();
        return Type == getStringDecl();
      };
      if (CheckIfStringParam(ParamList->get(0)) &&
          CheckIfStringParam(ParamList->get(1))) {
        getImpl().PlusFunctionOnString = FD;
        break;
      }
    }
  }
  return getImpl().PlusFunctionOnString;
}

#define KNOWN_STDLIB_TYPE_DECL(NAME, DECL_CLASS, NUM_GENERIC_PARAMS) \
  DECL_CLASS *ASTContext::get##NAME##Decl() const { \
    if (!getImpl().NAME##Decl) \
      getImpl().NAME##Decl = dyn_cast_or_null<DECL_CLASS>( \
        findStdlibType(*this, #NAME, NUM_GENERIC_PARAMS)); \
    return getImpl().NAME##Decl; \
  }
#include "swift/AST/KnownStdlibTypes.def"

CanType ASTContext::getExceptionType() const {
  if (auto exn = getErrorDecl()) {
    return exn->getDeclaredType()->getCanonicalType();
  } else {
    // Use Builtin.NativeObject just as a stand-in.
    return TheNativeObjectType;
  }
}

ProtocolDecl *ASTContext::getErrorDecl() const {
  return getProtocol(KnownProtocolKind::Error);
}

EnumElementDecl *ASTContext::getOptionalSomeDecl() const {
  if (!getImpl().OptionalSomeDecl)
    getImpl().OptionalSomeDecl = getOptionalDecl()->getUniqueElement(/*hasVal*/true);
  return getImpl().OptionalSomeDecl;
}

EnumElementDecl *ASTContext::getOptionalNoneDecl() const {
  if (!getImpl().OptionalNoneDecl)
    getImpl().OptionalNoneDecl =getOptionalDecl()->getUniqueElement(/*hasVal*/false);
  return getImpl().OptionalNoneDecl;
}

static VarDecl *getPointeeProperty(VarDecl *&cache,
                           NominalTypeDecl *(ASTContext::*getNominal)() const,
                                  const ASTContext &ctx) {
  if (cache) return cache;

  // There must be a generic type with one argument.
  NominalTypeDecl *nominal = (ctx.*getNominal)();
  if (!nominal) return nullptr;
  auto sig = nominal->getGenericSignature();
  if (!sig) return nullptr;
  if (sig->getGenericParams().size() != 1) return nullptr;

  // There must be a property named "pointee".
  auto identifier = ctx.getIdentifier("pointee");
  auto results = nominal->lookupDirect(identifier);
  if (results.size() != 1) return nullptr;

  // The property must have type T.
  auto *property = dyn_cast<VarDecl>(results[0]);
  if (!property) return nullptr;
  if (!property->getInterfaceType()->isEqual(sig->getGenericParams()[0]))
    return nullptr;

  cache = property;
  return property;
}

VarDecl *
ASTContext::getPointerPointeePropertyDecl(PointerTypeKind ptrKind) const {
  switch (ptrKind) {
  case PTK_UnsafeMutableRawPointer:
    return getPointeeProperty(getImpl().UnsafeMutableRawPointerMemoryDecl,
                             &ASTContext::getUnsafeMutableRawPointerDecl,
                             *this);
  case PTK_UnsafeRawPointer:
    return getPointeeProperty(getImpl().UnsafeRawPointerMemoryDecl,
                             &ASTContext::getUnsafeRawPointerDecl,
                             *this);
  case PTK_UnsafeMutablePointer:
    return getPointeeProperty(getImpl().UnsafeMutablePointerMemoryDecl,
                             &ASTContext::getUnsafeMutablePointerDecl,
                             *this);
  case PTK_UnsafePointer:
    return getPointeeProperty(getImpl().UnsafePointerMemoryDecl,
                             &ASTContext::getUnsafePointerDecl,
                             *this);
  case PTK_AutoreleasingUnsafeMutablePointer:
    return getPointeeProperty(getImpl().AutoreleasingUnsafeMutablePointerMemoryDecl,
                         &ASTContext::getAutoreleasingUnsafeMutablePointerDecl,
                             *this);
  }
  llvm_unreachable("bad pointer kind");
}

CanType ASTContext::getAnyObjectType() const {
  if (getImpl().AnyObjectType) {
    return getImpl().AnyObjectType;
  }

  getImpl().AnyObjectType = CanType(
    ProtocolCompositionType::get(
      *this, {}, /*HasExplicitAnyObject=*/true));
  return getImpl().AnyObjectType;
}

CanType ASTContext::getNeverType() const {
  auto neverDecl = getNeverDecl();
  if (!neverDecl)
    return CanType();
  return neverDecl->getDeclaredType()->getCanonicalType();
}

TypeAliasDecl *ASTContext::getVoidDecl() const {
  if (getImpl().VoidDecl) {
    return getImpl().VoidDecl;
  }

  // Go find 'Void' in the Swift module.
  SmallVector<ValueDecl *, 1> results;
  lookupInSwiftModule("Void", results);
  for (auto result : results) {
    if (auto typeAlias = dyn_cast<TypeAliasDecl>(result)) {
      getImpl().VoidDecl = typeAlias;
      return typeAlias;
    }
  }

  return getImpl().VoidDecl;
}

StructDecl *ASTContext::getObjCBoolDecl() const {
  if (!getImpl().ObjCBoolDecl) {
    SmallVector<ValueDecl *, 1> results;
    auto *Context = const_cast<ASTContext *>(this);
    if (ModuleDecl *M = Context->getModuleByName(Id_ObjectiveC.str())) {
      M->lookupValue({ }, getIdentifier("ObjCBool"), NLKind::UnqualifiedLookup,
                     results);
      for (auto result : results) {
        if (auto structDecl = dyn_cast<StructDecl>(result)) {
          if (structDecl->getGenericParams() == nullptr) {
            getImpl().ObjCBoolDecl = structDecl;
            break;
          }
        }
      }
    }
  }

  return getImpl().ObjCBoolDecl;
}

#define GET_FOUNDATION_DECL(NAME) \
ClassDecl *ASTContext::get##NAME##Decl() const { \
  if (!getImpl().NAME##Decl) { \
    if (ModuleDecl *M = getLoadedModule(Id_Foundation)) { \
      /* Note: use unqualified lookup so we find NSError regardless of */ \
      /* whether it's defined in the Foundation module or the Clang */ \
      /* Foundation module it imports. */ \
      UnqualifiedLookup lookup(getIdentifier(#NAME), M, nullptr); \
      if (auto type = lookup.getSingleTypeResult()) { \
        if (auto classDecl = dyn_cast<ClassDecl>(type)) { \
          if (classDecl->getGenericParams() == nullptr) { \
            getImpl().NAME##Decl = classDecl; \
          } \
        } \
      } \
    } \
  } \
  \
  return getImpl().NAME##Decl; \
}

FOR_KNOWN_FOUNDATION_TYPES(GET_FOUNDATION_DECL)
#undef GET_FOUNDATION_DECL
#undef FOR_KNOWN_FOUNDATION_TYPES

ProtocolDecl *ASTContext::getProtocol(KnownProtocolKind kind) const {
  // Check whether we've already looked for and cached this protocol.
  unsigned index = (unsigned)kind;
  assert(index < NumKnownProtocols && "Number of known protocols is wrong");
  if (getImpl().KnownProtocols[index])
    return getImpl().KnownProtocols[index];

  // Find all of the declarations with this name in the appropriate module.
  SmallVector<ValueDecl *, 1> results;

  const ModuleDecl *M;
  switch (kind) {
  case KnownProtocolKind::BridgedNSError:
  case KnownProtocolKind::BridgedStoredNSError:
  case KnownProtocolKind::ErrorCodeProtocol:
    M = getLoadedModule(Id_Foundation);
    break;
  case KnownProtocolKind::CFObject:
    M = getLoadedModule(Id_CoreFoundation);
    break;
  default:
    M = getStdlibModule();
    break;
  }

  if (!M)
    return nullptr;
  M->lookupValue({ }, getIdentifier(getProtocolName(kind)),
                 NLKind::UnqualifiedLookup, results);

  for (auto result : results) {
    if (auto protocol = dyn_cast<ProtocolDecl>(result)) {
      getImpl().KnownProtocols[index] = protocol;
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

/// Returns the type of an intrinsic function if it is not generic, otherwise
/// returns nullptr.
static FunctionType *
getIntrinsicCandidateType(FuncDecl *fn, bool allowTypeMembers) {
  auto type = fn->getInterfaceType();
  if (allowTypeMembers && fn->getDeclContext()->isTypeContext()) {
    auto fnType = type->getAs<FunctionType>();
    if (!fnType) return nullptr;

    type = fnType->getResult();
  }
  return type->getAs<FunctionType>();
}

/// Check whether the given type is Builtin.Int1.
static bool isBuiltinInt1Type(Type type) {
  if (auto intType = type->getAs<BuiltinIntegerType>())
    return intType->isFixedWidth() && intType->getFixedWidth() == 1;
  return false;
}

/// Check whether the given type is Builtin.Word.
static bool isBuiltinWordType(Type type) {
  if (auto intType = type->getAs<BuiltinIntegerType>())
    return intType->getWidth().isPointerWidth();
  return false;
}

/// Looks up all implementations of an operator (globally and declared in types)
/// and passes potential matches to the given callback. The search stops when
/// the predicate returns true (in which case the matching function declaration
/// is returned); otherwise, nullptr is returned if there are no matches.
/// \p C The AST context.
/// \p oper The name of the operator.
/// \p contextType If the operator is declared on a type, then only operators
///     defined on this type should be considered.
/// \p pred A callback predicate that takes as its argument the type of a
///     candidate function declaration and returns true if the function matches
///     the desired criteria.
/// \return The matching function declaration, or nullptr if there was no match.
static FuncDecl *
lookupOperatorFunc(const ASTContext &ctx, StringRef oper, Type contextType,
                   llvm::function_ref<bool(FunctionType *)> pred) {
  SmallVector<ValueDecl *, 32> candidates;
  ctx.lookupInSwiftModule(oper, candidates);

  for (auto candidate : candidates) {
    // All operator declarations should be functions, but make sure.
    auto *fnDecl = dyn_cast<FuncDecl>(candidate);
    if (!fnDecl)
      continue;

    if (fnDecl->getDeclContext()->isTypeContext()) {
      auto contextTy = fnDecl->getDeclContext()->getDeclaredInterfaceType();
      if (!contextTy->isEqual(contextType)) continue;
    }

    if (auto resolver = ctx.getLazyResolver())
      resolver->resolveDeclSignature(fnDecl);

    auto *funcTy = getIntrinsicCandidateType(fnDecl, /*allowTypeMembers=*/true);
    if (!funcTy)
      continue;

    if (pred(funcTy))
      return fnDecl;
  }

  return nullptr;
}

FuncDecl *ASTContext::getEqualIntDecl() const {
  if (getImpl().EqualIntDecl)
    return getImpl().EqualIntDecl;

  if (!getIntDecl() || !getBoolDecl())
    return nullptr;

  auto intType = getIntDecl()->getDeclaredType();
  auto boolType = getBoolDecl()->getDeclaredType();
  auto decl = lookupOperatorFunc(*this, "==",
                                 intType, [=](FunctionType *type) {
    // Check for the signature: (Int, Int) -> Bool
    if (type->getParams().size() != 2) return false;
    if (!type->getParams()[0].getOldType()->isEqual(intType) ||
        !type->getParams()[1].getOldType()->isEqual(intType)) return false;
    return type->getResult()->isEqual(boolType);
  });
  getImpl().EqualIntDecl = decl;
  return decl;
}

FuncDecl *ASTContext::getHashValueForDecl() const {
  if (getImpl().HashValueForDecl)
    return getImpl().HashValueForDecl;

  SmallVector<ValueDecl *, 1> results;
  lookupInSwiftModule("_hashValue", results);
  for (auto result : results) {
    auto *fd = dyn_cast<FuncDecl>(result);
    if (!fd)
      continue;
    auto paramList = fd->getParameters();
    if (paramList->size() != 1)
      continue;
    auto paramDecl = paramList->get(0);
    if (paramDecl->getArgumentName() != Id_for)
      continue;
    auto genericParams = fd->getGenericParams();
    if (!genericParams || genericParams->size() != 1)
      continue;
    getImpl().HashValueForDecl = fd;
    return fd;
  }
  return nullptr;
}

FuncDecl *ASTContext::getArrayAppendElementDecl() const {
  if (getImpl().ArrayAppendElementDecl)
    return getImpl().ArrayAppendElementDecl;

  auto AppendFunctions = getArrayDecl()->lookupDirect(getIdentifier("append"));

  for (auto CandidateFn : AppendFunctions) {
    auto FnDecl = dyn_cast<FuncDecl>(CandidateFn);
    auto Attrs = FnDecl->getAttrs();
    for (auto *A : Attrs.getAttributes<SemanticsAttr, false>()) {
      if (A->Value != "array.append_element")
        continue;

      auto SelfDecl = FnDecl->getImplicitSelfDecl();
      if (!SelfDecl->isInOut())
        return nullptr;

      auto SelfInOutTy = SelfDecl->getInterfaceType();
      BoundGenericStructType *SelfGenericStructTy =
        SelfInOutTy->getAs<BoundGenericStructType>();
      if (!SelfGenericStructTy)
        return nullptr;
      if (SelfGenericStructTy->getDecl() != getArrayDecl())
        return nullptr;

      auto ParamList = FnDecl->getParameters();
      if (ParamList->size() != 1)
        return nullptr;

      GenericTypeParamType *ElementType = ParamList->get(0)->
                             getInterfaceType()->getAs<GenericTypeParamType>();
      if (!ElementType)
        return nullptr;
      if (ElementType->getName() != getIdentifier("Element"))
        return nullptr;

      if (!FnDecl->getResultInterfaceType()->isVoid())
        return nullptr;

      getImpl().ArrayAppendElementDecl = FnDecl;
      return FnDecl;
    }
  }
  return nullptr;
}

FuncDecl *ASTContext::getArrayReserveCapacityDecl() const {
  if (getImpl().ArrayReserveCapacityDecl)
    return getImpl().ArrayReserveCapacityDecl;

  auto ReserveFunctions = getArrayDecl()->lookupDirect(
                                   getIdentifier("reserveCapacityForAppend"));

  for (auto CandidateFn : ReserveFunctions) {
    auto FnDecl = dyn_cast<FuncDecl>(CandidateFn);
    auto Attrs = FnDecl->getAttrs();
    for (auto *A : Attrs.getAttributes<SemanticsAttr, false>()) {
      if (A->Value != "array.reserve_capacity_for_append")
        continue;

      auto SelfDecl = FnDecl->getImplicitSelfDecl();
      if (!SelfDecl->isInOut())
        return nullptr;

      auto SelfInOutTy = SelfDecl->getInterfaceType();
      BoundGenericStructType *SelfGenericStructTy =
        SelfInOutTy->getAs<BoundGenericStructType>();
      if (!SelfGenericStructTy)
        return nullptr;
      if (SelfGenericStructTy->getDecl() != getArrayDecl())
        return nullptr;

      auto ParamList = FnDecl->getParameters();
      if (ParamList->size() != 1)
        return nullptr;
      StructType *IntType =
        ParamList->get(0)->getInterfaceType()->getAs<StructType>();
      if (!IntType)
        return nullptr;

      StructDecl *IntDecl = IntType->getDecl();
      auto StoredProperties = IntDecl->getStoredProperties();
      auto FieldIter = StoredProperties.begin();
      if (FieldIter == StoredProperties.end())
        return nullptr;
      VarDecl *field = *FieldIter;
      if (field->hasClangNode())
        return nullptr;
      if (!field->getInterfaceType()->is<BuiltinIntegerType>())
        return nullptr;
      if (std::next(FieldIter) != StoredProperties.end())
        return nullptr;

      if (!FnDecl->getResultInterfaceType()->isVoid())
        return nullptr;

      getImpl().ArrayReserveCapacityDecl = FnDecl;
      return FnDecl;
    }
  }
  return nullptr;
}

FuncDecl *
ASTContext::getUnimplementedInitializerDecl(LazyResolver *resolver) const {
  if (getImpl().UnimplementedInitializerDecl)
    return getImpl().UnimplementedInitializerDecl;

  // Look for the function.
  Type input, output;
  auto decl = findLibraryIntrinsic(*this, "_unimplementedInitializer",
                                   resolver);
  if (!decl)
    return nullptr;

  if (!getIntrinsicCandidateType(decl, /*allowTypeMembers=*/false))
    return nullptr;

  // FIXME: Check inputs and outputs.

  getImpl().UnimplementedInitializerDecl = decl;
  return decl;
}

FuncDecl *
ASTContext::getUndefinedDecl(LazyResolver *resolver) const {
  if (getImpl().UndefinedDecl)
    return getImpl().UndefinedDecl;

  // Look for the function.
  CanType input, output;
  auto decl = findLibraryIntrinsic(*this, "_undefined", resolver);
  if (!decl)
    return nullptr;

  getImpl().UndefinedDecl = decl;
  return decl;
}

FuncDecl *ASTContext::getIsOSVersionAtLeastDecl(LazyResolver *resolver) const {
  if (getImpl().IsOSVersionAtLeastDecl)
    return getImpl().IsOSVersionAtLeastDecl;

  // Look for the function.
  Type input, output;
  auto decl =
      findLibraryIntrinsic(*this, "_stdlib_isOSVersionAtLeast", resolver);
  if (!decl)
    return nullptr;

  auto *fnType = getIntrinsicCandidateType(decl, /*allowTypeMembers=*/false);
  if (!fnType)
    return nullptr;

  // Input must be (Builtin.Word, Builtin.Word, Builtin.Word)
  auto intrinsicsParams = fnType->getParams();
  if (intrinsicsParams.size() != 3)
    return nullptr;

  if (llvm::any_of(intrinsicsParams, [](const AnyFunctionType::Param &p) {
    return !isBuiltinWordType(p.getOldType());
  })) {
    return nullptr;
  }

  // Output must be Builtin.Int1
  if (!isBuiltinInt1Type(fnType->getResult()))
    return nullptr;

  getImpl().IsOSVersionAtLeastDecl = decl;
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
    return computeAssociativity(getImpl().AssociativityCache, left, right);
  }

  switch (computeAssociativity(getImpl().AssociativityCache, right, left)) {
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
  return findLibraryFunction(*this, getImpl().Get##Name, Id, resolver);  \
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

void ASTContext::addSynthesizedDecl(Decl *decl) {
  auto *mod = cast<FileUnit>(decl->getDeclContext()->getModuleScopeContext());
  if (mod->getKind() == FileUnitKind::ClangModule ||
      mod->getKind() == FileUnitKind::SerializedAST) {
    ExternalDefinitions.insert(decl);
    return;
  }

  cast<SourceFile>(mod)->SynthesizedDecls.push_back(decl);
}

void ASTContext::addCleanup(std::function<void(void)> cleanup) {
  getImpl().Cleanups.push_back(std::move(cleanup));
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

void ASTContext::addSearchPath(StringRef searchPath, bool isFramework,
                               bool isSystem) {
  OptionSet<SearchPathKind> &loaded = getImpl().SearchPathsSet[searchPath];
  auto kind = isFramework ? SearchPathKind::Framework : SearchPathKind::Import;
  if (loaded.contains(kind))
    return;
  loaded |= kind;

  if (isFramework)
    SearchPathOpts.FrameworkSearchPaths.push_back({searchPath, isSystem});
  else
    SearchPathOpts.ImportSearchPaths.push_back(searchPath);

  if (auto *clangLoader = getClangModuleLoader())
    clangLoader->addSearchPath(searchPath, isFramework, isSystem);
}

void ASTContext::addModuleLoader(std::unique_ptr<ModuleLoader> loader,
                                 bool IsClang) {
  if (IsClang) {
    assert(!getImpl().TheClangModuleLoader && "Already have a Clang module loader");
    getImpl().TheClangModuleLoader =
      static_cast<ClangModuleLoader *>(loader.get());
  }
  getImpl().ModuleLoaders.push_back(std::move(loader));
}

void ASTContext::loadExtensions(NominalTypeDecl *nominal,
                                unsigned previousGeneration) {
  PrettyStackTraceDecl stackTrace("loading extensions for", nominal);
  for (auto &loader : getImpl().ModuleLoaders) {
    loader->loadExtensions(nominal, previousGeneration);
  }
}

void ASTContext::loadObjCMethods(
       ClassDecl *classDecl,
       ObjCSelector selector,
       bool isInstanceMethod,
       unsigned previousGeneration,
       llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) {
  PrettyStackTraceSelector stackTraceSelector("looking for", selector);
  PrettyStackTraceDecl stackTraceDecl("...in", classDecl);
  for (auto &loader : getImpl().ModuleLoaders) {
    loader->loadObjCMethods(classDecl, selector, isInstanceMethod,
                            previousGeneration, methods);
  }
}

void ASTContext::verifyAllLoadedModules() const {
#ifndef NDEBUG
  FrontendStatsTracer tracer(Stats, "verify-all-loaded-modules");
  for (auto &loader : getImpl().ModuleLoaders)
    loader->verifyAllModules();

  for (auto &topLevelModulePair : LoadedModules) {
    ModuleDecl *M = topLevelModulePair.second;
    assert(!M->getFiles().empty() || M->failedToLoad());
  }
#endif
}

ClangModuleLoader *ASTContext::getClangModuleLoader() const {
  return getImpl().TheClangModuleLoader;
}

static void recordKnownProtocol(ModuleDecl *Stdlib, StringRef Name,
                                KnownProtocolKind Kind) {
  Identifier ID = Stdlib->getASTContext().getIdentifier(Name);
  UnqualifiedLookup Lookup(ID, Stdlib, nullptr, SourceLoc(),
                           UnqualifiedLookup::Flags::KnownPrivate |
                               UnqualifiedLookup::Flags::TypeLookup);
  if (auto Proto
        = dyn_cast_or_null<ProtocolDecl>(Lookup.getSingleTypeResult()))
    Proto->setKnownProtocolKind(Kind);
}

void ASTContext::recordKnownProtocols(ModuleDecl *Stdlib) {
#define PROTOCOL_WITH_NAME(Id, Name) \
  recordKnownProtocol(Stdlib, Name, KnownProtocolKind::Id);
#include "swift/AST/KnownProtocols.def"
}

ModuleDecl *ASTContext::getLoadedModule(
    ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath) const {
  assert(!ModulePath.empty());

  // TODO: Swift submodules.
  if (ModulePath.size() == 1) {
    return getLoadedModule(ModulePath[0].first);
  }
  return nullptr;
}

ModuleDecl *ASTContext::getLoadedModule(Identifier ModuleName) const {
  return LoadedModules.lookup(ModuleName);
}

void ASTContext::getVisibleTopLevelClangModules(
    SmallVectorImpl<clang::Module*> &Modules) const {
  getClangModuleLoader()->getClangPreprocessor().getHeaderSearchInfo().
    collectAllModules(Modules);
}

void ASTContext::registerGenericSignatureBuilder(
                                       GenericSignature *sig,
                                       GenericSignatureBuilder &&builder) {
  auto canSig = sig->getCanonicalSignature();
  auto known = getImpl().GenericSignatureBuilders.find(canSig);
  if (known != getImpl().GenericSignatureBuilders.end()) {
    ++NumRegisteredGenericSignatureBuildersAlready;
    return;
  }

  ++NumRegisteredGenericSignatureBuilders;
  getImpl().GenericSignatureBuilders[canSig] =
    llvm::make_unique<GenericSignatureBuilder>(std::move(builder));
}

GenericSignatureBuilder *ASTContext::getOrCreateGenericSignatureBuilder(
                                                      CanGenericSignature sig) {
  // Check whether we already have a generic signature builder for this
  // signature and module.
  auto known = getImpl().GenericSignatureBuilders.find(sig);
  if (known != getImpl().GenericSignatureBuilders.end())
    return known->second.get();

  // Create a new generic signature builder with the given signature.
  auto builder = new GenericSignatureBuilder(*this);

  // Store this generic signature builder (no generic environment yet).
  getImpl().GenericSignatureBuilders[sig] =
    std::unique_ptr<GenericSignatureBuilder>(builder);

  builder->addGenericSignature(sig);

#if SWIFT_GSB_EXPENSIVE_ASSERTIONS
  auto builderSig =
    builder->computeGenericSignature(SourceLoc(),
                                     /*allowConcreteGenericParams=*/true);
  if (builderSig->getCanonicalSignature() != sig) {
    llvm::errs() << "ERROR: generic signature builder is not idempotent.\n";
    llvm::errs() << "Original generic signature   : ";
    sig->print(llvm::errs());
    llvm::errs() << "\nReprocessed generic signature: ";
    auto reprocessedSig = builderSig->getCanonicalSignature();

    reprocessedSig->print(llvm::errs());
    llvm::errs() << "\n";

    if (sig->getGenericParams().size() ==
          reprocessedSig->getGenericParams().size() &&
        sig->getRequirements().size() ==
          reprocessedSig->getRequirements().size()) {
      for (unsigned i : indices(sig->getRequirements())) {
        auto sigReq = sig->getRequirements()[i];
        auto reprocessedReq = reprocessedSig->getRequirements()[i];
        if (sigReq.getKind() != reprocessedReq.getKind()) {
          llvm::errs() << "Requirement mismatch:\n";
          llvm::errs() << "  Original: ";
          sigReq.print(llvm::errs(), PrintOptions());
          llvm::errs() << "\n  Reprocessed: ";
          reprocessedReq.print(llvm::errs(), PrintOptions());
          llvm::errs() << "\n";
          break;
        }

        if (!sigReq.getFirstType()->isEqual(reprocessedReq.getFirstType())) {
          llvm::errs() << "First type mismatch, original is:\n";
          sigReq.getFirstType().dump(llvm::errs());
          llvm::errs() << "Reprocessed:\n";
          reprocessedReq.getFirstType().dump(llvm::errs());
          llvm::errs() << "\n";
          break;
        }

        if (sigReq.getKind() == RequirementKind::SameType &&
            !sigReq.getSecondType()->isEqual(reprocessedReq.getSecondType())) {
          llvm::errs() << "Second type mismatch, original is:\n";
          sigReq.getSecondType().dump(llvm::errs());
          llvm::errs() << "Reprocessed:\n";
          reprocessedReq.getSecondType().dump(llvm::errs());
          llvm::errs() << "\n";
          break;
        }
      }
    }

    llvm_unreachable("idempotency problem with a generic signature");
  }
#else
  // FIXME: This should be handled lazily in the future, and therefore not
  // required.
  builder->processDelayedRequirements();
#endif

  return builder;
}

GenericEnvironment *ASTContext::getOrCreateCanonicalGenericEnvironment(
                                              GenericSignatureBuilder *builder,
                                              GenericSignature *sig) {
  auto known = getImpl().CanonicalGenericEnvironments.find(builder);
  if (known != getImpl().CanonicalGenericEnvironments.end())
    return known->second;

  auto env = sig->createGenericEnvironment();
  getImpl().CanonicalGenericEnvironments[builder] = env;
  return env;
}

Optional<llvm::TinyPtrVector<ValueDecl *>>
OverriddenDeclsRequest::getCachedResult() const {
  auto decl = std::get<0>(getStorage());
  if (!decl->LazySemanticInfo.hasOverriddenComputed)
    return None;

  // If there are no overridden declarations (the common case), return.
  llvm::TinyPtrVector<ValueDecl *> overridden;
  if (!decl->LazySemanticInfo.hasOverridden) return overridden;

  // Retrieve the set of overrides from the ASTContext.
  ASTContext &ctx = decl->getASTContext();
  auto known = ctx.getImpl().Overrides.find(decl);
  assert(known != ctx.getImpl().Overrides.end());
  overridden.insert(overridden.end(),
                    known->second.begin(), known->second.end());
  return overridden;
}

void OverriddenDeclsRequest::cacheResult(
                                llvm::TinyPtrVector<ValueDecl *> value) const {
  auto decl = std::get<0>(getStorage());
  decl->LazySemanticInfo.hasOverriddenComputed = true;
  decl->LazySemanticInfo.hasOverridden = !value.empty();

  if (value.empty())
    return;

  // Sanity-check the declarations we were given.
  for (auto overriddenDecl : value) {
    assert(overriddenDecl->getKind() == decl->getKind() &&
           "Overridden decl kind mismatch");
    if (auto func = dyn_cast<AbstractFunctionDecl>(overriddenDecl))
      func->setIsOverridden();
  }

  // Record the overrides in the context.
  auto &ctx = decl->getASTContext();
  auto overriddenCopy =
    ctx.AllocateCopy(value.operator ArrayRef<ValueDecl *>());
  (void)ctx.getImpl().Overrides.insert({decl, overriddenCopy});
}

/// Returns the default witness for a requirement, or nullptr if there is
/// no default.
Witness ProtocolDecl::getDefaultWitness(ValueDecl *requirement) const {
  loadAllMembers();

  ASTContext &ctx = getASTContext();
  auto found = ctx.getImpl().DefaultWitnesses.find({this, requirement});
  if (found == ctx.getImpl().DefaultWitnesses.end())
    return Witness();
  return found->second;
}

/// Record the default witness for a requirement.
void ProtocolDecl::setDefaultWitness(ValueDecl *requirement, Witness witness) {
  assert(witness);
  ASTContext &ctx = getASTContext();
  auto pair = ctx.getImpl().DefaultWitnesses.insert(
                std::make_pair(std::make_pair(this, requirement), witness));
  assert(pair.second && "Already have a default witness!");
  (void) pair;
}

/// Returns the default type witness for an associated type, or a null
/// type if there is no default.
Type ProtocolDecl::getDefaultTypeWitness(AssociatedTypeDecl *assocType) const {
  auto &ctx = getASTContext();
  auto found = ctx.getImpl().DefaultTypeWitnesses.find({this, assocType});
  if (found == ctx.getImpl().DefaultTypeWitnesses.end())
    return Type();

  return found->second;
}

/// Set the default type witness for an associated type.
void ProtocolDecl::setDefaultTypeWitness(AssociatedTypeDecl *assocType,
                                         Type witness) {
  assert(witness);
  assert(!witness->hasArchetype() && "Only record interface types");
  ASTContext &ctx = getASTContext();
  auto pair = ctx.getImpl().DefaultTypeWitnesses.insert(
                std::make_pair(std::make_pair(this, assocType), witness));
  assert(pair.second && "Already have a default witness");
  (void)pair;
}

Optional<ProtocolConformanceRef>
ProtocolDecl::getDefaultAssociatedConformanceWitness(
                                             CanType association,
                                             ProtocolDecl *requirement) const {
  auto &ctx = getASTContext();
  auto found =
    ctx.getImpl().DefaultAssociatedConformanceWitnesses.find(
      std::make_tuple(this, association, requirement));
  if (found == ctx.getImpl().DefaultAssociatedConformanceWitnesses.end())
    return None;

  return found->second;
}

void ProtocolDecl::setDefaultAssociatedConformanceWitness(
                                          CanType association,
                                          ProtocolDecl *requirement,
                                          ProtocolConformanceRef conformance) {
  auto &ctx = getASTContext();
  auto pair = ctx.getImpl().DefaultAssociatedConformanceWitnesses.insert(
                std::make_pair(std::make_tuple(this, association, requirement),
                               conformance));
  assert(pair.second && "Already have a default associated conformance");
  (void)pair;
}

bool ASTContext::canImportModule(std::pair<Identifier, SourceLoc> ModulePath) {
  // If this module has already been successfully imported, it is importable.
  if (getLoadedModule(ModulePath) != nullptr)
    return true;

  // If we've failed loading this module before, don't look for it again.
  if (FailedModuleImportNames.count(ModulePath.first))
    return false;

  // Otherwise, ask the module loaders.
  for (auto &importer : getImpl().ModuleLoaders) {
    if (importer->canImportModule(ModulePath)) {
      return true;
    }
  }

  FailedModuleImportNames.insert(ModulePath.first);
  return false;
}

ModuleDecl *
ASTContext::getModule(ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath) {
  assert(!ModulePath.empty());

  if (auto *M = getLoadedModule(ModulePath))
    return M;

  auto moduleID = ModulePath[0];
  for (auto &importer : getImpl().ModuleLoaders) {
    if (ModuleDecl *M = importer->loadModule(moduleID.second, ModulePath)) {
      if (ModulePath.size() == 1 &&
          (ModulePath[0].first == StdlibModuleName ||
           ModulePath[0].first == Id_Foundation))
        recordKnownProtocols(M);
      return M;
    }
  }

  return nullptr;
}

ModuleDecl *ASTContext::getModuleByName(StringRef ModuleName) {
  SmallVector<std::pair<Identifier, SourceLoc>, 4>
  AccessPath;
  while (!ModuleName.empty()) {
    StringRef SubModuleName;
    std::tie(SubModuleName, ModuleName) = ModuleName.split('.');
    AccessPath.push_back({ getIdentifier(SubModuleName), SourceLoc() });
  }
  return getModule(AccessPath);
}

ModuleDecl *ASTContext::getStdlibModule(bool loadIfAbsent) {
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
  auto Known = getImpl().RawComments.find(D);
  if (Known == getImpl().RawComments.end())
    return None;

  return Known->second;
}

void ASTContext::setRawComment(const Decl *D, RawComment RC) {
  getImpl().RawComments[D] = RC;
}

Optional<StringRef> ASTContext::getBriefComment(const Decl *D) {
  auto Known = getImpl().BriefComments.find(D);
  if (Known == getImpl().BriefComments.end())
    return None;

  return Known->second;
}

void ASTContext::setBriefComment(const Decl *D, StringRef Comment) {
  getImpl().BriefComments[D] = Comment;
}

NormalProtocolConformance *
ASTContext::getBehaviorConformance(Type conformingType,
                                   ProtocolDecl *protocol,
                                   SourceLoc loc,
                                   AbstractStorageDecl *storage,
                                   ProtocolConformanceState state) {
  auto conformance = new (*this, AllocationArena::Permanent)
    NormalProtocolConformance(conformingType, protocol, loc, storage, state);

  if (auto nominal = conformingType->getAnyNominal()) {
    // Note: this is an egregious hack. The conformances need to be associated
    // with the actual storage declarations.
    SmallVector<ProtocolConformance *, 2> conformances;
    if (!nominal->lookupConformance(nominal->getModuleContext(), protocol,
                                    conformances))
      nominal->registerProtocolConformance(conformance);
  }
  return conformance;
}

NormalProtocolConformance *
ASTContext::getConformance(Type conformingType,
                           ProtocolDecl *protocol,
                           SourceLoc loc,
                           DeclContext *dc,
                           ProtocolConformanceState state) {
  assert(dc->isTypeContext());

  llvm::FoldingSetNodeID id;
  NormalProtocolConformance::Profile(id, protocol, dc);

  // Did we already record the normal conformance?
  void *insertPos;
  auto &normalConformances =
    getImpl().getArena(AllocationArena::Permanent).NormalConformances;
  if (auto result = normalConformances.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new normal protocol conformance.
  auto result
    = new (*this, AllocationArena::Permanent)
      NormalProtocolConformance(conformingType, protocol, loc, dc, state);
  normalConformances.InsertNode(result, insertPos);

  return result;
}

/// Produce a self-conformance for the given protocol.
SelfProtocolConformance *
ASTContext::getSelfConformance(ProtocolDecl *protocol) {
  auto &selfConformances =
    getImpl().getArena(AllocationArena::Permanent).SelfConformances;
  auto &entry = selfConformances[protocol];
  if (!entry) {
    entry = new (*this, AllocationArena::Permanent)
      SelfProtocolConformance(protocol->getDeclaredInterfaceType());
  }
  return entry;
}

/// If one of the ancestor conformances already has a matching type, use
/// that instead.
static ProtocolConformance *collapseSpecializedConformance(
                                             Type type,
                                             ProtocolConformance *conformance,
                                             SubstitutionMap substitutions) {
  while (true) {
    switch (conformance->getKind()) {
    case ProtocolConformanceKind::Specialized:
      conformance = cast<SpecializedProtocolConformance>(conformance)
                      ->getGenericConformance();
      break;

    case ProtocolConformanceKind::Normal:
    case ProtocolConformanceKind::Inherited:
    case ProtocolConformanceKind::Self:
      // If the conformance matches, return it.
      if (conformance->getType()->isEqual(type)) {
        for (auto subConformance : substitutions.getConformances())
          if (!subConformance.isAbstract())
            return nullptr;

        return conformance;
      }

      return nullptr;
    }
  }
}

ProtocolConformance *
ASTContext::getSpecializedConformance(Type type,
                                      ProtocolConformance *generic,
                                      SubstitutionMap substitutions) {
  // If we are performing a substitution that would get us back to the
  // a prior conformance (e.g., mapping into and then out of a conformance),
  // return the existing conformance.
  if (auto existing = collapseSpecializedConformance(type, generic,
                                                     substitutions)) {
    ++NumCollapsedSpecializedProtocolConformances;
    return existing;
  }

  llvm::FoldingSetNodeID id;
  SpecializedProtocolConformance::Profile(id, type, generic, substitutions);

  // Figure out which arena this conformance should go into.
  AllocationArena arena = getArena(type->getRecursiveProperties());

  // Did we already record the specialized conformance?
  void *insertPos;
  auto &specializedConformances = getImpl().getArena(arena).SpecializedConformances;
  if (auto result = specializedConformances.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new specialized conformance.
  auto result
    = new (*this, arena) SpecializedProtocolConformance(type, generic,
                                                        substitutions);
  auto node = specializedConformances.FindNodeOrInsertPos(id, insertPos);
  (void)node;
  assert(!node);
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
  auto &inheritedConformances = getImpl().getArena(arena).InheritedConformances;
  if (auto result
        = inheritedConformances.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new normal protocol conformance.
  auto result = new (*this, arena) InheritedProtocolConformance(type, inherited);
  inheritedConformances.InsertNode(result, insertPos);
  return result;
}

LazyContextData *ASTContext::getOrCreateLazyContextData(
                                                const DeclContext *dc,
                                                LazyMemberLoader *lazyLoader) {
  auto known = getImpl().LazyContexts.find(dc);
  if (known != getImpl().LazyContexts.end()) {
    // Make sure we didn't provide an incompatible lazy loader.
    assert(!lazyLoader || lazyLoader == known->second->loader);
    return known->second;
  }

  // Create new lazy iterable context data with the given loader.
  assert(lazyLoader && "Queried lazy data for non-lazy iterable context");
  if (isa<NominalTypeDecl>(dc) || isa<ExtensionDecl>(dc)) {
    auto *contextData = Allocate<LazyIterableDeclContextData>();
    contextData->loader = lazyLoader;
    getImpl().LazyContexts[dc] = contextData;
    return contextData;
  }

  // Create new lazy generic context data with the given loader.
  auto *contextData = Allocate<LazyGenericContextData>();
  contextData->loader = lazyLoader;
  getImpl().LazyContexts[dc] = contextData;
  return contextData;
}

bool ASTContext::hasUnparsedMembers(const IterableDeclContext *IDC) const {
  auto parsers = getImpl().lazyParsers;
  return std::any_of(parsers.begin(), parsers.end(),
    [IDC](LazyMemberParser *p) { return p->hasUnparsedMembers(IDC); });
}

void ASTContext::parseMembers(IterableDeclContext *IDC) {
  for (auto *p: getImpl().lazyParsers) {
    if (p->hasUnparsedMembers(IDC))
      p->parseMembers(IDC);
  }
}

LazyIterableDeclContextData *ASTContext::getOrCreateLazyIterableContextData(
                                            const IterableDeclContext *idc,
                                            LazyMemberLoader *lazyLoader) {
  if (auto ext = dyn_cast<ExtensionDecl>(idc)) {
    return (LazyIterableDeclContextData *)getOrCreateLazyContextData(
                                                              ext, lazyLoader);
  }

  auto nominal = cast<NominalTypeDecl>(idc);
  return (LazyIterableDeclContextData *)getOrCreateLazyContextData(nominal,
                                                                   lazyLoader);
}

LazyGenericContextData *ASTContext::getOrCreateLazyGenericContextData(
                                               const GenericContext *dc,
                                               LazyMemberLoader *lazyLoader) {
  return (LazyGenericContextData *)getOrCreateLazyContextData(dc,
                                                              lazyLoader);
}

void ASTContext::addDelayedConformanceDiag(
       NormalProtocolConformance *conformance,
       DelayedConformanceDiag fn) {
  getImpl().DelayedConformanceDiags[conformance].push_back(std::move(fn));
}

void ASTContext::
addDelayedMissingWitnesses(NormalProtocolConformance *conformance,
                           ArrayRef<ValueDecl*> witnesses) {
  auto &bucket = getImpl().DelayedMissingWitnesses[conformance];
  bucket.insert(bucket.end(), witnesses.begin(), witnesses.end());
}

std::vector<ValueDecl*> ASTContext::
takeDelayedMissingWitnesses(NormalProtocolConformance *conformance) {
  std::vector<ValueDecl*> result;
  auto known = getImpl().DelayedMissingWitnesses.find(conformance);
  if (known != getImpl().DelayedMissingWitnesses.end()) {
    result = std::move(known->second);
    getImpl().DelayedMissingWitnesses.erase(known);
  }
  return result;
}

std::vector<ASTContext::DelayedConformanceDiag>
ASTContext::takeDelayedConformanceDiags(NormalProtocolConformance *conformance){
  std::vector<ASTContext::DelayedConformanceDiag> result;
  auto known = getImpl().DelayedConformanceDiags.find(conformance);
  if (known != getImpl().DelayedConformanceDiags.end()) {
    result = std::move(known->second);
    getImpl().DelayedConformanceDiags.erase(known);
  }
  return result;
}

size_t ASTContext::getTotalMemory() const {
  size_t Size = sizeof(*this) +
    // LoadedModules ?
    // ExternalDefinitions ?
    llvm::capacity_in_bytes(CanonicalGenericTypeParamTypeNames) +
    // RemappedTypes ?
    sizeof(getImpl()) +
    getImpl().Allocator.getTotalMemory() +
    getImpl().Cleanups.capacity() +
    llvm::capacity_in_bytes(getImpl().ModuleLoaders) +
    llvm::capacity_in_bytes(getImpl().RawComments) +
    llvm::capacity_in_bytes(getImpl().BriefComments) +
    llvm::capacity_in_bytes(getImpl().ModuleTypes) +
    llvm::capacity_in_bytes(getImpl().GenericParamTypes) +
    // getImpl().GenericFunctionTypes ?
    // getImpl().SILFunctionTypes ?
    llvm::capacity_in_bytes(getImpl().SILBlockStorageTypes) +
    llvm::capacity_in_bytes(getImpl().IntegerTypes) +
    // getImpl().ProtocolCompositionTypes ?
    // getImpl().BuiltinVectorTypes ?
    // getImpl().GenericSignatures ?
    // getImpl().CompoundNames ?
    getImpl().OpenedExistentialArchetypes.getMemorySize() +
    getImpl().Permanent.getTotalMemory();

    Size += getSolverMemory();

    return Size;
}

size_t ASTContext::getSolverMemory() const {
  size_t Size = 0;
  
  if (getImpl().CurrentConstraintSolverArena) {
    Size += getImpl().CurrentConstraintSolverArena->getTotalMemory();
    Size += getImpl().CurrentConstraintSolverArena->Allocator.getBytesAllocated();
  }
  
  return Size;
}

size_t ASTContext::Implementation::Arena::getTotalMemory() const {
  return sizeof(*this) +
    // TupleTypes ?
    llvm::capacity_in_bytes(MetatypeTypes) +
    llvm::capacity_in_bytes(ExistentialMetatypeTypes) +
    llvm::capacity_in_bytes(ArraySliceTypes) +
    llvm::capacity_in_bytes(DictionaryTypes) +
    llvm::capacity_in_bytes(OptionalTypes) +
    llvm::capacity_in_bytes(ParenTypes) +
    llvm::capacity_in_bytes(ReferenceStorageTypes) +
    llvm::capacity_in_bytes(LValueTypes) +
    llvm::capacity_in_bytes(InOutTypes) +
    llvm::capacity_in_bytes(DependentMemberTypes) +
    llvm::capacity_in_bytes(DynamicSelfTypes);
    // FunctionTypes ?
    // EnumTypes ?
    // StructTypes ?
    // ClassTypes ?
    // UnboundGenericTypes ?
    // BoundGenericTypes ?
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
      ModuleDecl *lhsModule = lhs->getDeclContext()->getParentModule();
      ModuleDecl *rhsModule = rhs->getDeclContext()->getParentModule();
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
} // end anonymous namespace

/// Compute the information used to describe an Objective-C redeclaration.
std::pair<unsigned, DeclName> swift::getObjCMethodDiagInfo(
                                AbstractFunctionDecl *member) {
  if (isa<ConstructorDecl>(member))
    return { 0 + member->isImplicit(), member->getFullName() };

  if (isa<DestructorDecl>(member))
    return { 2 + member->isImplicit(), member->getFullName() };

  if (auto accessor = dyn_cast<AccessorDecl>(member)) {
    switch (accessor->getAccessorKind()) {
#define OBJC_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
    case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
      llvm_unreachable("Not an Objective-C entry point");

    case AccessorKind::Get:
      if (auto var = dyn_cast<VarDecl>(accessor->getStorage()))
        return { 5, var->getFullName() };

      return { 6, Identifier() };

    case AccessorKind::Set:
      if (auto var = dyn_cast<VarDecl>(accessor->getStorage()))
        return { 7, var->getFullName() };
      return { 8, Identifier() };
    }

    llvm_unreachable("Unhandled AccessorKind in switch.");
  }

  // Normal method.
  auto func = cast<FuncDecl>(member);
  return { 4, func->getFullName() };
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
    diag.fixItReplace(func->getLoc(), targetName.getBaseName().userFacingName());
  }

  // Fix the argument names that need fixing.
  assert(name.getArgumentNames().size()
          == targetName.getArgumentNames().size());
  auto params = func->getParameters();
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
  if (decl->isImplicit())
    return false;

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

  if (LangOpts.EnableObjCInterop) {
    if (!LangOpts.EnableObjCAttrRequiresFoundation)
      return;
    if (SF.Kind == SourceFileKind::SIL)
      return;
  }

  SF.forAllVisibleModules([&](ModuleDecl::ImportedModule import) {
    if (import.second->getName() == Id_Foundation)
      ImportsFoundationModule = true;
  });

  if (ImportsFoundationModule)
    return;

  for (auto Attr : SF.AttrsRequiringFoundation) {
    if (!LangOpts.EnableObjCInterop)
      Diags.diagnose(Attr->getLocation(), diag::objc_interop_disabled)
        .fixItRemove(Attr->getRangeWithAt());
    Diags.diagnose(Attr->getLocation(),
                   diag::attr_used_without_required_module,
                   Attr, Id_Foundation)
      .highlight(Attr->getRangeWithAt());
  }
}

void ASTContext::recordObjCMethod(AbstractFunctionDecl *func) {
  // If this method comes from Objective-C, ignore it.
  if (func->hasClangNode())
    return;

  getImpl().ObjCMethods.push_back(func);
}

/// Lookup for an Objective-C method with the given selector in the
/// given class or any of its superclasses.
static AbstractFunctionDecl *lookupObjCMethodInClass(
                               ClassDecl *classDecl,
                               ObjCSelector selector,
                               bool isInstanceMethod,
                               bool isInitializer,
                               SourceManager &srcMgr,
                               bool inheritingInits = true) {
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

  return lookupObjCMethodInClass(classDecl->getSuperclassDecl(), selector,
                                 isInstanceMethod, isInitializer, srcMgr,
                                 inheritingInits);
}

void AbstractFunctionDecl::setForeignErrorConvention(
                                         const ForeignErrorConvention &conv) {
  assert(hasThrows() && "setting error convention on non-throwing decl");
  auto &conventionsMap = getASTContext().getImpl().ForeignErrorConventions;
  assert(!conventionsMap.count(this) && "error convention already set");
  conventionsMap.insert({this, conv});
}

Optional<ForeignErrorConvention>
AbstractFunctionDecl::getForeignErrorConvention() const {
  if (!hasThrows())
    return None;
  auto &conventionsMap = getASTContext().getImpl().ForeignErrorConventions;
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
  getImpl().ObjCMethods.erase(std::remove_if(getImpl().ObjCMethods.begin(),
                                        getImpl().ObjCMethods.end(),
                                        captureMethodInSourceFile),
                         getImpl().ObjCMethods.end());

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

    auto classDecl = method->getDeclContext()->getSelfClassDecl();
    if (!classDecl)
      continue; // error-recovery path, only

    if (!classDecl->hasSuperclass())
      continue;

    // Look for a method that we have overridden in one of our
    // superclasses.
    // Note: This should be treated as a lookup for intra-module dependency
    // purposes, but a subclass already depends on its superclasses and any
    // extensions for many other reasons.
    auto selector = method->getObjCSelector();
    AbstractFunctionDecl *overriddenMethod
      = lookupObjCMethodInClass(classDecl->getSuperclassDecl(),
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
      if (auto accessor = dyn_cast<AccessorDecl>(overriddenMethod))
        overriddenDecl = accessor->getStorage();
    Diags.diagnose(overriddenDecl, diag::objc_declared_here,
                   overriddenDiagInfo.first, overriddenDiagInfo.second);

    diagnosedAny = true;
  }

  return diagnosedAny;
}

void ASTContext::recordObjCMethodConflict(ClassDecl *classDecl,
                                          ObjCSelector selector,
                                          bool isInstance) {
  getImpl().ObjCMethodConflicts.push_back(std::make_tuple(classDecl, selector,
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

                                 if (auto ad = dyn_cast<AccessorDecl>(method)) {
                                   return ad->getStorage()->isInvalid();
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
  if (getImpl().ObjCMethodConflicts.empty())
    return false;

  // Partition the set of conflicts to put the conflicts that involve
  // this source file at the end.
  auto firstLocalConflict
    = std::partition(getImpl().ObjCMethodConflicts.begin(),
                     getImpl().ObjCMethodConflicts.end(),
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
    = getImpl().ObjCMethodConflicts.end() - firstLocalConflict;
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
        if (auto accessor = dyn_cast<AccessorDecl>(originalMethod))
          originalDecl = accessor->getStorage();

      if (diagInfo == origDiagInfo) {
        Diags.diagnose(conflictingDecl, diag::objc_redecl_same,
                       diagInfo.first, diagInfo.second, selector);
        Diags.diagnose(originalDecl, diag::invalid_redecl_prev,
                       originalDecl->getBaseName());
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
  getImpl().ObjCMethodConflicts.erase(firstLocalConflict,
                                 getImpl().ObjCMethodConflicts.end());

  return anyConflicts;
}

void ASTContext::recordObjCUnsatisfiedOptReq(DeclContext *dc,
                                             AbstractFunctionDecl *req) {
  getImpl().ObjCUnsatisfiedOptReqs.push_back(ObjCUnsatisfiedOptReq(dc, req));
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
  if (getImpl().ObjCUnsatisfiedOptReqs.empty())
    return false;

  // Partition the set of unsatisfied requirements to put the
  // conflicts that involve this source file at the end.
  auto firstLocalReq
    = std::partition(getImpl().ObjCUnsatisfiedOptReqs.begin(),
                     getImpl().ObjCUnsatisfiedOptReqs.end(),
                     [&](const ObjCUnsatisfiedOptReq &unsatisfied) -> bool {
                       return &sf != unsatisfied.first->getParentSourceFile();
                     });

  // If there were no local unsatisfied requirements, we're done.
  unsigned numLocalReqs
    = getImpl().ObjCUnsatisfiedOptReqs.end() - firstLocalReq;
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
    ClassDecl *classDecl = unsatisfied.first->getSelfClassDecl();
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

    // Fix the name of the witness, if we can.
    if (req->getFullName() != conflicts[0]->getFullName() &&
        req->getKind() == conflicts[0]->getKind() &&
        isa<AccessorDecl>(req) == isa<AccessorDecl>(conflicts[0])) {
      // They're of the same kind: fix the name.
      unsigned kind;
      if (isa<ConstructorDecl>(req))
        kind = 1;
      else if (auto accessor = dyn_cast<AccessorDecl>(req))
        kind = isa<SubscriptDecl>(accessor->getStorage()) ? 3 : 2;
      else if (isa<FuncDecl>(req))
        kind = 0;
      else {
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
      Diags.diagnose(conflicts[0], diag::req_near_match_nonobjc, true)
        .fixItInsert(
          conflicts[0]->getAttributeInsertionLoc(/*forModifier=*/false),
          "@nonobjc ");

    Diags.diagnose(getDeclContextLoc(unsatisfied.first),
                   diag::protocol_conformance_here,
                   true,
                   classDecl->getFullName(),
                   protocolName);
    Diags.diagnose(req, diag::kind_declname_declared_here,
                   DescriptiveDeclKind::Requirement, reqDiagInfo.second);

    anyDiagnosed = true;
  }

  // Erase the local unsatisfied requirements from the list.
  getImpl().ObjCUnsatisfiedOptReqs.erase(firstLocalReq,
                                    getImpl().ObjCUnsatisfiedOptReqs.end());

  return anyDiagnosed;
}

Optional<KnownFoundationEntity> swift::getKnownFoundationEntity(StringRef name){
  return llvm::StringSwitch<Optional<KnownFoundationEntity>>(name)
#define FOUNDATION_ENTITY(Name) .Case(#Name, KnownFoundationEntity::Name)
#include "swift/AST/KnownFoundationEntities.def"
    .Default(None);
}

StringRef ASTContext::getSwiftName(KnownFoundationEntity kind) {
  StringRef objcName;
  switch (kind) {
#define FOUNDATION_ENTITY(Name) case KnownFoundationEntity::Name:  \
    objcName = #Name;                                             \
    break;
#include "swift/AST/KnownFoundationEntities.def"
  }

  return objcName;
}

//===----------------------------------------------------------------------===//
// Type manipulation routines.
//===----------------------------------------------------------------------===//

NameAliasType::NameAliasType(TypeAliasDecl *typealias, Type parent,
                             SubstitutionMap substitutions,
                             Type underlying,
                             RecursiveTypeProperties properties)
    : SugarType(TypeKind::NameAlias, underlying, properties),
      typealias(typealias) {
  // Record the parent (or absence of a parent).
  if (parent) {
    Bits.NameAliasType.HasParent = true;
    *getTrailingObjects<Type>() = parent;
  } else {
    Bits.NameAliasType.HasParent = false;
  }

  // Record the substitutions.
  if (substitutions) {
    Bits.NameAliasType.HasSubstitutionMap = true;
    *getTrailingObjects<SubstitutionMap>() = substitutions;
  } else {
    Bits.NameAliasType.HasSubstitutionMap = false;
  }
}

NameAliasType *NameAliasType::get(TypeAliasDecl *typealias, Type parent,
                                  SubstitutionMap substitutions,
                                  Type underlying) {
  // Compute the recursive properties.
  //
  auto properties = underlying->getRecursiveProperties();
  auto storedProperties = properties;
  if (parent) {
    properties |= parent->getRecursiveProperties();
    if (parent->hasTypeVariable())
      storedProperties |= RecursiveTypeProperties::HasTypeVariable;
  }
  auto genericSig = substitutions.getGenericSignature();
  if (genericSig) {
    for (Type gp : genericSig->getGenericParams()) {
      auto substGP = gp.subst(substitutions, SubstFlags::UseErrorType);
      properties |= substGP->getRecursiveProperties();
      if (substGP->hasTypeVariable())
        storedProperties |= RecursiveTypeProperties::HasTypeVariable;
    }
  }

  // Figure out which arena this type will go into.
  auto &ctx = underlying->getASTContext();
  auto arena = getArena(properties);

  // Profile the type.
  llvm::FoldingSetNodeID id;
  NameAliasType::Profile(id, typealias, parent, substitutions, underlying);

  // Did we already record this type?
  void *insertPos;
  auto &types = ctx.getImpl().getArena(arena).NameAliasTypes;
  if (auto result = types.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new type.
  auto size = totalSizeToAlloc<Type, SubstitutionMap>(parent ? 1 : 0,
                                                      genericSig ? 1 : 0);
  auto mem = ctx.Allocate(size, alignof(NameAliasType), arena);
  auto result = new (mem) NameAliasType(typealias, parent, substitutions,
                                        underlying, storedProperties);
  types.InsertNode(result, insertPos);
  return result;
}

void NameAliasType::Profile(llvm::FoldingSetNodeID &id) const {
  Profile(id, getDecl(), getParent(), getSubstitutionMap(),
          Type(getSinglyDesugaredType()));
}

void NameAliasType::Profile(
                           llvm::FoldingSetNodeID &id,
                           TypeAliasDecl *typealias,
                           Type parent, SubstitutionMap substitutions,
                           Type underlying) {
  id.AddPointer(typealias);
  id.AddPointer(parent.getPointer());
  substitutions.profile(id);
  id.AddPointer(underlying.getPointer());
}

// Simple accessors.
Type ErrorType::get(const ASTContext &C) { return C.TheErrorType; }

Type ErrorType::get(Type originalType) {
  assert(originalType);

  auto originalProperties = originalType->getRecursiveProperties();
  auto arena = getArena(originalProperties);

  auto &ctx = originalType->getASTContext();
  auto &entry = ctx.getImpl().getArena(arena).ErrorTypesWithOriginal[originalType];
  if (entry) return entry;

  void *mem = ctx.Allocate(sizeof(ErrorType) + sizeof(Type),
                           alignof(ErrorType), arena);
  RecursiveTypeProperties properties = RecursiveTypeProperties::HasError;
  if (originalProperties.hasTypeVariable())
    properties |= RecursiveTypeProperties::HasTypeVariable;
  return entry = new (mem) ErrorType(ctx, originalType, properties);
}

BuiltinIntegerType *BuiltinIntegerType::get(BuiltinIntegerWidth BitWidth,
                                            const ASTContext &C) {
  assert(!BitWidth.isArbitraryWidth());
  BuiltinIntegerType *&Result = C.getImpl().IntegerTypes[BitWidth];
  if (Result == nullptr)
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
        = context.getImpl().BuiltinVectorTypes.FindNodeOrInsertPos(id, insertPos))
    return vecType;

  assert(elementType->isCanonical() && "Non-canonical builtin vector?");
  BuiltinVectorType *vecTy
    = new (context, AllocationArena::Permanent)
       BuiltinVectorType(context, elementType, numElements);
  context.getImpl().BuiltinVectorTypes.InsertNode(vecTy, insertPos);
  return vecTy;
}

ParenType *ParenType::get(const ASTContext &C, Type underlying,
                          ParameterTypeFlags fl) {
  if (fl.isInOut())
    assert(!underlying->is<InOutType>() && "caller did not pass a base type");
  if (underlying->is<InOutType>())
    assert(fl.isInOut() && "caller did not set flags correctly");
  
  auto properties = underlying->getRecursiveProperties();
  auto arena = getArena(properties);
  ParenType *&Result =
      C.getImpl().getArena(arena).ParenTypes[{underlying, fl.toRaw()}];
  if (Result == nullptr) {
    Result = new (C, arena) ParenType(underlying,
                                      properties, fl);
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
    ID.AddPointer(Elt.Name.get());
    ID.AddPointer(Elt.getType().getPointer());
    ID.AddInteger(Elt.Flags.toRaw());
  }
}

/// getTupleType - Return the uniqued tuple type with the specified elements.
Type TupleType::get(ArrayRef<TupleTypeElt> Fields, const ASTContext &C) {
  if (Fields.size() == 1 && !Fields[0].isVararg() && !Fields[0].hasName())
    return ParenType::get(C, Fields[0].getRawType(),
                          Fields[0].getParameterFlags());

  RecursiveTypeProperties properties;
  bool hasElementWithOwnership = false;
  for (const TupleTypeElt &Elt : Fields) {
    auto eltTy = Elt.getType();
    if (!eltTy) continue;
    
    properties |= eltTy->getRecursiveProperties();
    // Recur into paren types and canonicalized paren types.  'inout' in nested
    // non-paren tuples are malformed and will be diagnosed later.
    if (auto *TTy = Elt.getType()->getAs<TupleType>()) {
      if (TTy->getNumElements() == 1)
        hasElementWithOwnership |= TTy->hasElementWithOwnership();
    } else if (auto *Pty = dyn_cast<ParenType>(Elt.getType().getPointer())) {
      hasElementWithOwnership |= (Pty->getParameterFlags().getValueOwnership() !=
                                  ValueOwnership::Default);
    } else {
      hasElementWithOwnership |= (Elt.getParameterFlags().getValueOwnership() !=
                                  ValueOwnership::Default);
    }
  }

  auto arena = getArena(properties);

  void *InsertPos = nullptr;
  // Check to see if we've already seen this tuple before.
  llvm::FoldingSetNodeID ID;
  TupleType::Profile(ID, Fields);

  if (TupleType *TT
        = C.getImpl().getArena(arena).TupleTypes.FindNodeOrInsertPos(ID,InsertPos))
    return TT;

  bool IsCanonical = true;   // All canonical elts means this is canonical.
  for (const TupleTypeElt &Elt : Fields) {
    if (Elt.getType().isNull() || !Elt.getType()->isCanonical()) {
      IsCanonical = false;
      break;
    }
  }

  // TupleType will copy the fields list into ASTContext owned memory.
  void *mem = C.Allocate(sizeof(TupleType) +
                         sizeof(TupleTypeElt) * Fields.size(),
                         alignof(TupleType), arena);
  auto New = new (mem) TupleType(Fields, IsCanonical ? &C : nullptr, properties,
                                 hasElementWithOwnership);
  C.getImpl().getArena(arena).TupleTypes.InsertNode(New, InsertPos);
  return New;
}

TupleTypeElt::TupleTypeElt(Type ty, Identifier name,
                           ParameterTypeFlags fl)
  : Name(name), ElementType(ty), Flags(fl) {
  if (fl.isInOut())
    assert(!ty->is<InOutType>() && "caller did not pass a base type");
  if (ty->is<InOutType>())
    assert(fl.isInOut() && "caller did not set flags correctly");
}

Type TupleTypeElt::getType() const {
  if (Flags.isInOut()) return InOutType::get(ElementType);
  return ElementType;
}

Type AnyFunctionType::Param::getOldType() const {
  if (Flags.isInOut()) return InOutType::get(Ty);
  return Ty;
}

AnyFunctionType::Param swift::computeSelfParam(AbstractFunctionDecl *AFD,
                                               bool isInitializingCtor,
                                               bool wantDynamicSelf) {
  auto *dc = AFD->getDeclContext();
  auto &Ctx = dc->getASTContext();
  
  // Determine the type of the container.
  auto containerTy = dc->getDeclaredInterfaceType();
  if (!containerTy || containerTy->hasError())
    return AnyFunctionType::Param(ErrorType::get(Ctx));

  // Determine the type of 'self' inside the container.
  auto selfTy = dc->getSelfInterfaceType();
  if (!selfTy || selfTy->hasError())
    return AnyFunctionType::Param(ErrorType::get(Ctx));

  bool isStatic = false;
  SelfAccessKind selfAccess = SelfAccessKind::NonMutating;
  bool isDynamicSelf = false;

  if (auto *FD = dyn_cast<FuncDecl>(AFD)) {
    isStatic = FD->isStatic();
    selfAccess = FD->getSelfAccessKind();

    // Methods returning 'Self' have a dynamic 'self'.
    //
    // FIXME: All methods of non-final classes should have this.
    if (wantDynamicSelf && FD->hasDynamicSelf())
      isDynamicSelf = true;
  } else if (auto *CD = dyn_cast<ConstructorDecl>(AFD)) {
    if (isInitializingCtor) {
      // initializing constructors of value types always have an implicitly
      // inout self.
      selfAccess = SelfAccessKind::Mutating;
    } else {
      // allocating constructors have metatype 'self'.
      isStatic = true;
    }

    // Convenience initializers have a dynamic 'self' in '-swift-version 5'.
    if (Ctx.isSwiftVersionAtLeast(5)) {
      if (wantDynamicSelf && CD->isConvenienceInit())
        if (auto *classDecl = selfTy->getClassOrBoundGenericClass())
          if (!classDecl->isFinal())
            isDynamicSelf = true;
    }
  } else if (isa<DestructorDecl>(AFD)) {
    // Destructors only correctly appear on classes today. (If move-only types
    // have destructors, they probably would want to consume self.)
    // Note that we can't assert(containerTy->hasReferenceSemantics()) here
    // since incorrect or incomplete code could have deinit decls in invalid
    // contexts, and we need to recover gracefully in those cases.
  }

  if (isDynamicSelf)
    selfTy = DynamicSelfType::get(selfTy, Ctx);

  // 'static' functions have 'self' of type metatype<T>.
  if (isStatic)
    return AnyFunctionType::Param(MetatypeType::get(selfTy, Ctx));

  // Reference types have 'self' of type T.
  if (containerTy->hasReferenceSemantics())
    return AnyFunctionType::Param(selfTy);

  auto flags = ParameterTypeFlags();
  switch (selfAccess) {
  case SelfAccessKind::__Consuming:
    flags = flags.withOwned(true);
    break;
  case SelfAccessKind::Mutating:
    flags = flags.withInOut(true);
    break;
  case SelfAccessKind::NonMutating:
    // The default flagless state.
    break;
  }

  return AnyFunctionType::Param(selfTy, Identifier(), flags);
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
  void *InsertPos = nullptr;
  RecursiveTypeProperties properties;
  if (Parent) properties |= Parent->getRecursiveProperties();
  auto arena = getArena(properties);

  if (auto unbound = C.getImpl().getArena(arena).UnboundGenericTypes
                        .FindNodeOrInsertPos(ID, InsertPos))
    return unbound;

  auto result = new (C, arena) UnboundGenericType(TheDecl, Parent, C,
                                                  properties);
  C.getImpl().getArena(arena).UnboundGenericTypes.InsertNode(result, InsertPos);
  return result;
}

void BoundGenericType::Profile(llvm::FoldingSetNodeID &ID,
                               NominalTypeDecl *TheDecl, Type Parent,
                               ArrayRef<Type> GenericArgs) {
  ID.AddPointer(TheDecl);
  ID.AddPointer(Parent.getPointer());
  ID.AddInteger(GenericArgs.size());
  for (Type Arg : GenericArgs) {
    ID.AddPointer(Arg.getPointer());
  }
}

BoundGenericType::BoundGenericType(TypeKind theKind,
                                   NominalTypeDecl *theDecl,
                                   Type parent,
                                   ArrayRef<Type> genericArgs,
                                   const ASTContext *context,
                                   RecursiveTypeProperties properties)
    : NominalOrBoundGenericNominalType(theDecl, parent, theKind, context,
                                       properties) {
  Bits.BoundGenericType.GenericArgCount = genericArgs.size();
  // Subtypes are required to provide storage for the generic arguments
  std::uninitialized_copy(genericArgs.begin(), genericArgs.end(),
                          getTrailingObjectsPointer());
}

BoundGenericType *BoundGenericType::get(NominalTypeDecl *TheDecl,
                                        Type Parent,
                                        ArrayRef<Type> GenericArgs) {
  assert(TheDecl->getGenericParams() && "must be a generic type decl");
  assert((!Parent || Parent->is<NominalType>() ||
          Parent->is<BoundGenericType>() ||
          Parent->is<UnboundGenericType>()) &&
         "parent must be a nominal type");

  ASTContext &C = TheDecl->getDeclContext()->getASTContext();
  llvm::FoldingSetNodeID ID;
  BoundGenericType::Profile(ID, TheDecl, Parent, GenericArgs);
  RecursiveTypeProperties properties;
  if (Parent) properties |= Parent->getRecursiveProperties();
  for (Type Arg : GenericArgs) {
    properties |= Arg->getRecursiveProperties();
  }

  auto arena = getArena(properties);

  void *InsertPos = nullptr;
  if (BoundGenericType *BGT =
        C.getImpl().getArena(arena).BoundGenericTypes.FindNodeOrInsertPos(ID,
                                                                     InsertPos))
    return BGT;

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
    auto sz = BoundGenericClassType::totalSizeToAlloc<Type>(GenericArgs.size());
    auto mem = C.Allocate(sz, alignof(BoundGenericClassType), arena);
    newType = new (mem) BoundGenericClassType(
        theClass, Parent, GenericArgs, IsCanonical ? &C : nullptr, properties);
  } else if (auto theStruct = dyn_cast<StructDecl>(TheDecl)) {
    auto sz =BoundGenericStructType::totalSizeToAlloc<Type>(GenericArgs.size());
    auto mem = C.Allocate(sz, alignof(BoundGenericStructType), arena);
    newType = new (mem) BoundGenericStructType(
        theStruct, Parent, GenericArgs, IsCanonical ? &C : nullptr, properties);
  } else if (auto theEnum = dyn_cast<EnumDecl>(TheDecl)) {
    auto sz = BoundGenericEnumType::totalSizeToAlloc<Type>(GenericArgs.size());
    auto mem = C.Allocate(sz, alignof(BoundGenericEnumType), arena);
    newType = new (mem) BoundGenericEnumType(
        theEnum, Parent, GenericArgs, IsCanonical ? &C : nullptr, properties);
  } else {
    llvm_unreachable("Unhandled NominalTypeDecl");
  }
  C.getImpl().getArena(arena).BoundGenericTypes.InsertNode(newType, InsertPos);

  return newType;
}

NominalType *NominalType::get(NominalTypeDecl *D, Type Parent, const ASTContext &C) {
  assert((isa<ProtocolDecl>(D) || !D->getGenericParams()) &&
         "must be a non-generic type decl");
  assert((!Parent || Parent->is<NominalType>() ||
          Parent->is<BoundGenericType>() ||
          Parent->is<UnboundGenericType>()) &&
         "parent must be a nominal type");

  switch (D->getKind()) {
  case DeclKind::Enum:
    return EnumType::get(cast<EnumDecl>(D), Parent, C);
  case DeclKind::Struct:
    return StructType::get(cast<StructDecl>(D), Parent, C);
  case DeclKind::Class:
    return ClassType::get(cast<ClassDecl>(D), Parent, C);
  case DeclKind::Protocol: {
    return ProtocolType::get(cast<ProtocolDecl>(D), Parent, C);
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

  void *insertPos = nullptr;
  if (auto enumTy
        = C.getImpl().getArena(arena).EnumTypes.FindNodeOrInsertPos(id, insertPos))
    return enumTy;

  auto enumTy = new (C, arena) EnumType(D, Parent, C, properties);
  C.getImpl().getArena(arena).EnumTypes.InsertNode(enumTy, insertPos);
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

  void *insertPos = nullptr;
  if (auto structTy
        = C.getImpl().getArena(arena).StructTypes.FindNodeOrInsertPos(id, insertPos))
    return structTy;

  auto structTy = new (C, arena) StructType(D, Parent, C, properties);
  C.getImpl().getArena(arena).StructTypes.InsertNode(structTy, insertPos);
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

  void *insertPos = nullptr;
  if (auto classTy
        = C.getImpl().getArena(arena).ClassTypes.FindNodeOrInsertPos(id, insertPos))
    return classTy;

  auto classTy = new (C, arena) ClassType(D, Parent, C, properties);
  C.getImpl().getArena(arena).ClassTypes.InsertNode(classTy, insertPos);
  return classTy;
}

void ClassType::Profile(llvm::FoldingSetNodeID &ID, ClassDecl *D, Type Parent) {
  ID.AddPointer(D);
  ID.AddPointer(Parent.getPointer());
}

ProtocolCompositionType *
ProtocolCompositionType::build(const ASTContext &C, ArrayRef<Type> Members,
                               bool HasExplicitAnyObject) {
  // Check to see if we've already seen this protocol composition before.
  void *InsertPos = nullptr;
  llvm::FoldingSetNodeID ID;
  ProtocolCompositionType::Profile(ID, Members, HasExplicitAnyObject);

  bool isCanonical = true;
  RecursiveTypeProperties properties;
  for (Type t : Members) {
    if (!t->isCanonical())
      isCanonical = false;
    properties |= t->getRecursiveProperties();
  }

  // Create a new protocol composition type.
  auto arena = getArena(properties);

  if (auto compTy
      = C.getImpl().getArena(arena).ProtocolCompositionTypes
          .FindNodeOrInsertPos(ID, InsertPos))
    return compTy;

  // Use trailing objects for member type storage
  auto size = totalSizeToAlloc<Type>(Members.size());
  auto mem = C.Allocate(size, alignof(ProtocolCompositionType), arena);
  auto compTy = new (mem) ProtocolCompositionType(isCanonical ? &C : nullptr,
                                                  Members,
                                                  HasExplicitAnyObject,
                                                  properties);
  C.getImpl().getArena(arena).ProtocolCompositionTypes.InsertNode(compTy, InsertPos);
  return compTy;
}

ReferenceStorageType *ReferenceStorageType::get(Type T,
                                                ReferenceOwnership ownership,
                                                const ASTContext &C) {
  assert(!T->hasTypeVariable()); // not meaningful in type-checker
  switch (optionalityOf(ownership)) {
  case ReferenceOwnershipOptionality::Disallowed:
    assert(!T->getOptionalObjectType() && "optional type is disallowed");
    break;
  case ReferenceOwnershipOptionality::Allowed:
    break;
  case ReferenceOwnershipOptionality::Required:
    assert(T->getOptionalObjectType() && "optional type is required");
    break;
  }

  auto properties = T->getRecursiveProperties();
  auto arena = getArena(properties);

  auto key = uintptr_t(T.getPointer()) | unsigned(ownership);
  auto &entry = C.getImpl().getArena(arena).ReferenceStorageTypes[key];
  if (entry) return entry;

  switch (ownership) {
  case ReferenceOwnership::Strong:
    llvm_unreachable("strong ownership does not use ReferenceStorageType");
#define REF_STORAGE(Name, ...) \
  case ReferenceOwnership::Name: \
    return entry = new (C, arena) \
      Name##StorageType(T, T->isCanonical() ? &C : nullptr, properties);
#include "swift/AST/ReferenceStorage.def"
  }
  llvm_unreachable("bad ownership");
}

AnyMetatypeType::AnyMetatypeType(TypeKind kind, const ASTContext *C,
                                 RecursiveTypeProperties properties,
                                 Type instanceType,
                                 Optional<MetatypeRepresentation> repr)
    : TypeBase(kind, C, properties), InstanceType(instanceType) {
  if (repr) {
    Bits.AnyMetatypeType.Representation = static_cast<char>(*repr) + 1;
  } else {
    Bits.AnyMetatypeType.Representation = 0;
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

  MetatypeType *&Entry = Ctx.getImpl().getArena(arena).MetatypeTypes[{T, reprKey}];
  if (Entry) return Entry;

  return Entry = new (Ctx, arena) MetatypeType(
             T, T->isCanonical() ? &Ctx : nullptr, properties, Repr);
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

  auto &entry = ctx.getImpl().getArena(arena).ExistentialMetatypeTypes[{T, reprKey}];
  if (entry) return entry;

  return entry = new (ctx, arena) ExistentialMetatypeType(
             T, T->isCanonical() ? &ctx : nullptr, properties, repr);
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

ModuleType *ModuleType::get(ModuleDecl *M) {
  ASTContext &C = M->getASTContext();

  ModuleType *&Entry = C.getImpl().ModuleTypes[M];
  if (Entry) return Entry;

  return Entry = new (C, AllocationArena::Permanent) ModuleType(M, C);
}

DynamicSelfType *DynamicSelfType::get(Type selfType, const ASTContext &ctx) {
  assert(selfType->isMaterializable()
         && "non-materializable dynamic self?");
  
  auto properties = selfType->getRecursiveProperties();
  auto arena = getArena(properties);

  auto &dynamicSelfTypes = ctx.getImpl().getArena(arena).DynamicSelfTypes;
  auto known = dynamicSelfTypes.find(selfType);
  if (known != dynamicSelfTypes.end())
    return known->second;

  auto result = new (ctx, arena) DynamicSelfType(selfType, ctx, properties);
  dynamicSelfTypes.insert({selfType, result});
  return result;
}

static RecursiveTypeProperties
getFunctionRecursiveProperties(ArrayRef<AnyFunctionType::Param> params,
                               Type result) {
  RecursiveTypeProperties properties;
  for (auto param : params)
    properties |= param.getPlainType()->getRecursiveProperties();
  properties |= result->getRecursiveProperties();
  properties &= ~RecursiveTypeProperties::IsLValue;
  return properties;
}

static bool
isFunctionTypeCanonical(ArrayRef<AnyFunctionType::Param> params,
                        Type result) {
  for (auto param : params) {
    if (!param.getPlainType()->isCanonical())
      return false;
  }

  return result->isCanonical();
}

// For now, generic function types cannot be dependent (in fact,
// they erase dependence) or contain type variables, and they're
// always materializable.
static RecursiveTypeProperties
getGenericFunctionRecursiveProperties(ArrayRef<AnyFunctionType::Param> params,
                                      Type result) {
  static_assert(RecursiveTypeProperties::BitWidth == 10,
                "revisit this if you add new recursive type properties");
  RecursiveTypeProperties properties;

  for (auto param : params) {
    if (param.getPlainType()->getRecursiveProperties().hasError())
      properties |= RecursiveTypeProperties::HasError;
  }

  if (result->getRecursiveProperties().hasDynamicSelf())
    properties |= RecursiveTypeProperties::HasDynamicSelf;
  if (result->getRecursiveProperties().hasError())
    properties |= RecursiveTypeProperties::HasError;

  return properties;
}

static bool
isGenericFunctionTypeCanonical(GenericSignature *sig,
                               ArrayRef<AnyFunctionType::Param> params,
                               Type result) {
  if (!sig->isCanonical())
    return false;

  for (auto param : params) {
    if (!sig->isCanonicalTypeInContext(param.getPlainType()))
      return false;
  }

  return sig->isCanonicalTypeInContext(result);
}

AnyFunctionType *AnyFunctionType::withExtInfo(ExtInfo info) const {
  if (isa<FunctionType>(this))
    return FunctionType::get(getParams(), getResult(), info);

  auto *genFnTy = cast<GenericFunctionType>(this);
  return GenericFunctionType::get(genFnTy->getGenericSignature(),
                                  getParams(), getResult(), info);
}

void AnyFunctionType::decomposeInput(
    Type type, SmallVectorImpl<AnyFunctionType::Param> &result) {
  switch (type->getKind()) {
  case TypeKind::Tuple: {
    auto tupleTy = cast<TupleType>(type.getPointer());
    for (auto &elt : tupleTy->getElements()) {
      result.emplace_back((elt.isVararg()
                           ? elt.getVarargBaseTy()
                           : elt.getRawType()),
                          elt.getName(),
                          elt.getParameterFlags());
    }
    return;
  }
      
  case TypeKind::Paren: {
    auto pty = cast<ParenType>(type.getPointer());
    result.emplace_back(pty->getUnderlyingType()->getInOutObjectType(),
                        Identifier(),
                        pty->getParameterFlags());
    return;
  }
      
  default:
    result.emplace_back(type->getInOutObjectType(), Identifier(),
                        ParameterTypeFlags::fromParameterType(
                          type, false, false, ValueOwnership::Default));
    return;
  }
}

Type AnyFunctionType::Param::getParameterType(bool forCanonical,
                                              ASTContext *ctx) const {
  Type type = getPlainType();
  if (isVariadic()) {
    if (!ctx) ctx = &type->getASTContext();
    auto arrayDecl = ctx->getArrayDecl();
    if (!arrayDecl)
      type = ErrorType::get(*ctx);
    else if (forCanonical)
      type = BoundGenericType::get(arrayDecl, Type(), {type});
    else
      type = ArraySliceType::get(type);
  }
  return type;
}

Type AnyFunctionType::composeInput(ASTContext &ctx, ArrayRef<Param> params,
                                   bool canonicalVararg) {
  SmallVector<TupleTypeElt, 4> elements;
  for (const auto &param : params) {
    Type eltType = param.getParameterType(canonicalVararg, &ctx);
    elements.emplace_back(eltType, param.getLabel(),
                          param.getParameterFlags());
  }
  return TupleType::get(elements, ctx);
}

bool AnyFunctionType::equalParams(ArrayRef<AnyFunctionType::Param> a,
                                  ArrayRef<AnyFunctionType::Param> b) {
  if (a.size() != b.size())
    return false;

  for (unsigned i = 0, n = a.size(); i != n; ++i) {
    if (a[i] != b[i])
      return false;
  }

  return true;
}

bool AnyFunctionType::equalParams(CanParamArrayRef a, CanParamArrayRef b) {
  if (a.size() != b.size())
    return false;

  for (unsigned i = 0, n = a.size(); i != n; ++i) {
    if (a[i] != b[i])
      return false;
  }

  return true;
}

void AnyFunctionType::relabelParams(MutableArrayRef<Param> params,
                                    ArrayRef<Identifier> labels) {
  assert(params.size() == labels.size());
  for (auto i : indices(params)) {
    auto &param = params[i];
    param = AnyFunctionType::Param(param.getPlainType(),
                                   labels[i],
                                   param.getParameterFlags());
  }
}

static void profileParams(llvm::FoldingSetNodeID &ID,
                          ArrayRef<AnyFunctionType::Param> params) {
  ID.AddInteger(params.size());
  for (auto param : params) {
    ID.AddPointer(param.getLabel().get());
    ID.AddPointer(param.getPlainType().getPointer());
    ID.AddInteger(param.getParameterFlags().toRaw());
  }
}

void FunctionType::Profile(llvm::FoldingSetNodeID &ID,
                           ArrayRef<AnyFunctionType::Param> params,
                           Type result,
                           ExtInfo info) {
  profileParams(ID, params);
  ID.AddPointer(result.getPointer());
  ID.AddInteger(info.getFuncAttrKey());
}

FunctionType *FunctionType::get(ArrayRef<AnyFunctionType::Param> params,
                                Type result, ExtInfo info) {
  auto properties = getFunctionRecursiveProperties(params, result);
  auto arena = getArena(properties);

  llvm::FoldingSetNodeID id;
  FunctionType::Profile(id, params, result, info);

  const ASTContext &ctx = result->getASTContext();

  // Do we already have this generic function type?
  void *insertPos;
  if (auto funcTy =
        ctx.getImpl().getArena(arena).FunctionTypes.FindNodeOrInsertPos(id, insertPos)) {
    return funcTy;
  }

  void *mem = ctx.Allocate(sizeof(FunctionType) +
                             sizeof(AnyFunctionType::Param) * params.size(),
                           alignof(FunctionType), arena);

  bool isCanonical = isFunctionTypeCanonical(params, result);
  auto funcTy = new (mem) FunctionType(params, result, info,
                                       isCanonical ? &ctx : nullptr,
                                       properties);
  ctx.getImpl().getArena(arena).FunctionTypes.InsertNode(funcTy, insertPos);
  return funcTy;
}

// If the input and result types are canonical, then so is the result.
FunctionType::FunctionType(ArrayRef<AnyFunctionType::Param> params,
                           Type output, ExtInfo info,
                           const ASTContext *ctx,
                           RecursiveTypeProperties properties)
    : AnyFunctionType(TypeKind::Function, ctx,
                      output, properties, params.size(), info) {
  std::uninitialized_copy(params.begin(), params.end(),
                          getTrailingObjects<AnyFunctionType::Param>());
}

void GenericFunctionType::Profile(llvm::FoldingSetNodeID &ID,
                                  GenericSignature *sig,
                                  ArrayRef<AnyFunctionType::Param> params,
                                  Type result,
                                  ExtInfo info) {
  ID.AddPointer(sig);
  profileParams(ID, params);
  ID.AddPointer(result.getPointer());
  ID.AddInteger(info.getFuncAttrKey());
}

GenericFunctionType *GenericFunctionType::get(GenericSignature *sig,
                                              ArrayRef<Param> params,
                                              Type result,
                                              ExtInfo info) {
  assert(sig && "no generic signature for generic function type?!");
  assert(!result->hasTypeVariable());

  llvm::FoldingSetNodeID id;
  GenericFunctionType::Profile(id, sig, params, result, info);

  const ASTContext &ctx = result->getASTContext();

  // Do we already have this generic function type?
  void *insertPos;
  if (auto result
        = ctx.getImpl().GenericFunctionTypes.FindNodeOrInsertPos(id, insertPos)) {
    return result;
  }

  // We have to construct this generic function type. Determine whether
  // it's canonical.  Unfortunately, isCanonicalTypeInContext can cause
  // new GenericFunctionTypes to be created and thus invalidate our insertion
  // point.
  bool isCanonical = isGenericFunctionTypeCanonical(sig, params, result);

  if (auto funcTy
        = ctx.getImpl().GenericFunctionTypes.FindNodeOrInsertPos(id, insertPos)) {
    return funcTy;
  }
  
  void *mem = ctx.Allocate(sizeof(GenericFunctionType) +
                             sizeof(AnyFunctionType::Param) * params.size(),
                           alignof(GenericFunctionType));

  auto properties = getGenericFunctionRecursiveProperties(params, result);
  auto funcTy = new (mem) GenericFunctionType(sig, params, result, info,
                                              isCanonical ? &ctx : nullptr,
                                              properties);

  ctx.getImpl().GenericFunctionTypes.InsertNode(funcTy, insertPos);
  return funcTy;
}

GenericFunctionType::GenericFunctionType(
                       GenericSignature *sig,
                       ArrayRef<AnyFunctionType::Param> params,
                       Type result,
                       ExtInfo info,
                       const ASTContext *ctx,
                       RecursiveTypeProperties properties)
  : AnyFunctionType(TypeKind::GenericFunction, ctx, result,
                    properties, params.size(), info), Signature(sig) {
  std::uninitialized_copy(params.begin(), params.end(),
                          getTrailingObjects<AnyFunctionType::Param>());
}

GenericTypeParamType *GenericTypeParamType::get(unsigned depth, unsigned index,
                                                const ASTContext &ctx) {
  auto known = ctx.getImpl().GenericParamTypes.find({ depth, index });
  if (known != ctx.getImpl().GenericParamTypes.end())
    return known->second;

  auto result = new (ctx, AllocationArena::Permanent)
                  GenericTypeParamType(depth, index, ctx);
  ctx.getImpl().GenericParamTypes[{depth, index}] = result;
  return result;
}

TypeArrayView<GenericTypeParamType>
GenericFunctionType::getGenericParams() const {
  return Signature->getGenericParams();
}

/// Retrieve the requirements of this polymorphic function type.
ArrayRef<Requirement> GenericFunctionType::getRequirements() const {
  return Signature->getRequirements();
}

void SILFunctionType::Profile(llvm::FoldingSetNodeID &id,
                              GenericSignature *genericParams,
                              ExtInfo info,
                              SILCoroutineKind coroutineKind,
                              ParameterConvention calleeConvention,
                              ArrayRef<SILParameterInfo> params,
                              ArrayRef<SILYieldInfo> yields,
                              ArrayRef<SILResultInfo> results,
                              Optional<SILResultInfo> errorResult,
                              Optional<ProtocolConformanceRef> conformance) {
  id.AddPointer(genericParams);
  id.AddInteger(info.getFuncAttrKey());
  id.AddInteger(unsigned(coroutineKind));
  id.AddInteger(unsigned(calleeConvention));
  id.AddInteger(params.size());
  for (auto param : params)
    param.profile(id);
  id.AddInteger(yields.size());
  for (auto yield : yields)
    yield.profile(id);
  id.AddInteger(results.size());
  for (auto result : results)
    result.profile(id);

  // Just allow the profile length to implicitly distinguish the
  // presence of an error result.
  if (errorResult) errorResult->profile(id);
  if (conformance)
    id.AddPointer(conformance->getRequirement());
}

SILFunctionType::SILFunctionType(GenericSignature *genericSig, ExtInfo ext,
                                 SILCoroutineKind coroutineKind,
                                 ParameterConvention calleeConvention,
                                 ArrayRef<SILParameterInfo> params,
                                 ArrayRef<SILYieldInfo> yields,
                                 ArrayRef<SILResultInfo> normalResults,
                                 Optional<SILResultInfo> errorResult,
                                 const ASTContext &ctx,
                                 RecursiveTypeProperties properties,
                      Optional<ProtocolConformanceRef> witnessMethodConformance)
    : TypeBase(TypeKind::SILFunction, &ctx, properties),
      GenericSig(genericSig),
      WitnessMethodConformance(witnessMethodConformance) {

  Bits.SILFunctionType.HasErrorResult = errorResult.hasValue();
  Bits.SILFunctionType.ExtInfo = ext.Bits;
  // The use of both assert() and static_assert() below is intentional.
  assert(Bits.SILFunctionType.ExtInfo == ext.Bits && "Bits were dropped!");
  static_assert(ExtInfo::NumMaskBits == NumSILExtInfoBits,
                "ExtInfo and SILFunctionTypeBitfields must agree on bit size");
  Bits.SILFunctionType.CoroutineKind = unsigned(coroutineKind);
  NumParameters = params.size();
  if (coroutineKind == SILCoroutineKind::None) {
    assert(yields.empty());
    NumAnyResults = normalResults.size();
    NumAnyIndirectFormalResults =
      std::count_if(normalResults.begin(), normalResults.end(),
                    [](const SILResultInfo &resultInfo) {
                      return resultInfo.isFormalIndirect();
                    });
    memcpy(getMutableResults().data(), normalResults.data(),
           normalResults.size() * sizeof(SILResultInfo));
  } else {
    assert(normalResults.empty());    
    NumAnyResults = yields.size();
    NumAnyIndirectFormalResults = 0; // unused
    memcpy(getMutableYields().data(), yields.data(),
           yields.size() * sizeof(SILYieldInfo));
  }

  assert(!isIndirectFormalParameter(calleeConvention));
  Bits.SILFunctionType.CalleeConvention = unsigned(calleeConvention);

  memcpy(getMutableParameters().data(), params.data(),
         params.size() * sizeof(SILParameterInfo));
  if (errorResult)
    getMutableErrorResult() = *errorResult;

  if (hasResultCache()) {
    getMutableFormalResultsCache() = CanType();
    getMutableAllResultsCache() = CanType();
  }
#ifndef NDEBUG
  if (ext.getRepresentation() == Representation::WitnessMethod)
    assert(WitnessMethodConformance &&
           "witness_method SIL function without a conformance");
  else
    assert(!WitnessMethodConformance &&
           "non-witness_method SIL function with a conformance");

  // Make sure the interface types are sane.
  if (genericSig) {
    for (auto gparam : genericSig->getGenericParams()) {
      (void)gparam;
      assert(gparam->isCanonical() && "generic signature is not canonicalized");
    }

    for (auto param : getParameters()) {
      (void)param;
      assert(!param.getType()->hasError()
             && "interface type of parameter should not contain error types");
      assert(!param.getType()->hasArchetype()
             && "interface type of parameter should not contain context archetypes");
    }
    for (auto result : getResults()) {
      (void)result;
      assert(!result.getType()->hasError()
             && "interface type of result should not contain error types");
      assert(!result.getType()->hasArchetype()
             && "interface type of result should not contain context archetypes");
    }
    for (auto yield : getYields()) {
      (void)yield;
      assert(!yield.getType()->hasError()
             && "interface type of yield should not contain error types");
      assert(!yield.getType()->hasArchetype()
             && "interface type of yield should not contain context archetypes");
    }
    if (hasErrorResult()) {
      assert(!getErrorResult().getType()->hasError()
             && "interface type of result should not contain error types");
      assert(!getErrorResult().getType()->hasArchetype()
             && "interface type of result should not contain context archetypes");
    }
  }
  for (auto result : getResults()) {
    (void)result;
    if (auto *FnType = result.getType()->getAs<SILFunctionType>()) {
      assert(!FnType->isNoEscape() &&
             "Cannot return an @noescape function type");
    }
  }
#endif
}

CanSILBlockStorageType SILBlockStorageType::get(CanType captureType) {
  ASTContext &ctx = captureType->getASTContext();
  auto found = ctx.getImpl().SILBlockStorageTypes.find(captureType);
  if (found != ctx.getImpl().SILBlockStorageTypes.end())
    return CanSILBlockStorageType(found->second);
  
  void *mem = ctx.Allocate(sizeof(SILBlockStorageType),
                           alignof(SILBlockStorageType));
  
  SILBlockStorageType *storageTy = new (mem) SILBlockStorageType(captureType);
  ctx.getImpl().SILBlockStorageTypes.insert({captureType, storageTy});
  return CanSILBlockStorageType(storageTy);
}

CanSILFunctionType SILFunctionType::get(GenericSignature *genericSig,
                                        ExtInfo ext,
                                        SILCoroutineKind coroutineKind,
                                        ParameterConvention callee,
                                        ArrayRef<SILParameterInfo> params,
                                        ArrayRef<SILYieldInfo> yields,
                                        ArrayRef<SILResultInfo> normalResults,
                                        Optional<SILResultInfo> errorResult,
                                        const ASTContext &ctx,
                    Optional<ProtocolConformanceRef> witnessMethodConformance) {
  assert(coroutineKind == SILCoroutineKind::None || normalResults.empty());
  assert(coroutineKind != SILCoroutineKind::None || yields.empty());
  assert(!ext.isPseudogeneric() || genericSig);

  llvm::FoldingSetNodeID id;
  SILFunctionType::Profile(id, genericSig, ext, coroutineKind, callee, params,
                           yields, normalResults, errorResult,
                           witnessMethodConformance);

  // Do we already have this generic function type?
  void *insertPos;
  if (auto result
        = ctx.getImpl().SILFunctionTypes.FindNodeOrInsertPos(id, insertPos))
    return CanSILFunctionType(result);

  // All SILFunctionTypes are canonical.

  // Allocate storage for the object.
  size_t bytes = sizeof(SILFunctionType)
                 + sizeof(SILParameterInfo) * params.size()
                 + sizeof(SILYieldInfo) * yields.size()
                 + sizeof(SILResultInfo) * normalResults.size()
                 + (errorResult ? sizeof(SILResultInfo) : 0)
                 + (normalResults.size() > 1 ? sizeof(CanType) * 2 : 0);
  void *mem = ctx.Allocate(bytes, alignof(SILFunctionType));

  RecursiveTypeProperties properties;
  static_assert(RecursiveTypeProperties::BitWidth == 10,
                "revisit this if you add new recursive type properties");
  for (auto &param : params)
    properties |= param.getType()->getRecursiveProperties();
  for (auto &yield : yields)
    properties |= yield.getType()->getRecursiveProperties();
  for (auto &result : normalResults)
    properties |= result.getType()->getRecursiveProperties();
  if (errorResult)
    properties |= errorResult->getType()->getRecursiveProperties();

  // FIXME: If we ever have first-class polymorphic values, we'll need to
  // revisit this.
  if (genericSig) {
    properties.removeHasTypeParameter();
    properties.removeHasDependentMember();
  }

  auto fnType =
      new (mem) SILFunctionType(genericSig, ext, coroutineKind, callee,
                                params, yields, normalResults, errorResult,
                                ctx, properties, witnessMethodConformance);
  ctx.getImpl().SILFunctionTypes.InsertNode(fnType, insertPos);
  return CanSILFunctionType(fnType);
}


ArraySliceType *ArraySliceType::get(Type base) {
  auto properties = base->getRecursiveProperties();
  auto arena = getArena(properties);

  const ASTContext &C = base->getASTContext();

  ArraySliceType *&entry = C.getImpl().getArena(arena).ArraySliceTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) ArraySliceType(C, base, properties);
}

DictionaryType *DictionaryType::get(Type keyType, Type valueType) {
  auto properties = keyType->getRecursiveProperties() 
                  | valueType->getRecursiveProperties();
  auto arena = getArena(properties);

  const ASTContext &C = keyType->getASTContext();

  DictionaryType *&entry
    = C.getImpl().getArena(arena).DictionaryTypes[{keyType, valueType}];
  if (entry) return entry;

  return entry = new (C, arena) DictionaryType(C, keyType, valueType, 
                                               properties);
}

OptionalType *OptionalType::get(Type base) {
  auto properties = base->getRecursiveProperties();
  auto arena = getArena(properties);

  const ASTContext &C = base->getASTContext();

  OptionalType *&entry = C.getImpl().getArena(arena).OptionalTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) OptionalType(C, base, properties);
}

ProtocolType *ProtocolType::get(ProtocolDecl *D, Type Parent,
                                const ASTContext &C) {
  llvm::FoldingSetNodeID id;
  ProtocolType::Profile(id, D, Parent);

  RecursiveTypeProperties properties;
  if (Parent) properties |= Parent->getRecursiveProperties();
  auto arena = getArena(properties);

  void *insertPos = nullptr;
  if (auto protoTy
        = C.getImpl().getArena(arena).ProtocolTypes.FindNodeOrInsertPos(id, insertPos))
    return protoTy;

  auto protoTy = new (C, arena) ProtocolType(D, Parent, C, properties);
  C.getImpl().getArena(arena).ProtocolTypes.InsertNode(protoTy, insertPos);

  return protoTy;
}

ProtocolType::ProtocolType(ProtocolDecl *TheDecl, Type Parent,
                           const ASTContext &Ctx,
                           RecursiveTypeProperties properties)
  : NominalType(TypeKind::Protocol, &Ctx, TheDecl, Parent, properties) { }

void ProtocolType::Profile(llvm::FoldingSetNodeID &ID, ProtocolDecl *D,
                           Type Parent) {
  ID.AddPointer(D);
  ID.AddPointer(Parent.getPointer());
}

LValueType *LValueType::get(Type objectTy) {
  assert(!objectTy->hasError() &&
         "cannot have ErrorType wrapped inside LValueType");
  assert(!objectTy->is<LValueType>() && !objectTy->is<InOutType>() &&
         "cannot have 'inout' or @lvalue wrapped inside an @lvalue");

  auto properties = objectTy->getRecursiveProperties()
                    | RecursiveTypeProperties::IsLValue;
  auto arena = getArena(properties);

  auto &C = objectTy->getASTContext();
  auto &entry = C.getImpl().getArena(arena).LValueTypes[objectTy];
  if (entry)
    return entry;

  const ASTContext *canonicalContext = objectTy->isCanonical() ? &C : nullptr;
  return entry = new (C, arena) LValueType(objectTy, canonicalContext,
                                           properties);
}

InOutType *InOutType::get(Type objectTy) {
  assert(!objectTy->is<LValueType>() && !objectTy->is<InOutType>() &&
         "cannot have 'inout' or @lvalue wrapped inside an 'inout'");

  auto properties = objectTy->getRecursiveProperties();

  properties &= ~RecursiveTypeProperties::IsLValue;
  auto arena = getArena(properties);

  auto &C = objectTy->getASTContext();
  auto &entry = C.getImpl().getArena(arena).InOutTypes[objectTy];
  if (entry)
    return entry;

  const ASTContext *canonicalContext = objectTy->isCanonical() ? &C : nullptr;
  return entry = new (C, arena) InOutType(objectTy, canonicalContext,
                                          properties);
}

DependentMemberType *DependentMemberType::get(Type base, Identifier name) {
  auto properties = base->getRecursiveProperties();
  properties |= RecursiveTypeProperties::HasDependentMember;
  auto arena = getArena(properties);

  llvm::PointerUnion<Identifier, AssociatedTypeDecl *> stored(name);
  const ASTContext &ctx = base->getASTContext();
  auto *&known = ctx.getImpl().getArena(arena).DependentMemberTypes[
                                            {base, stored.getOpaqueValue()}];
  if (!known) {
    const ASTContext *canonicalCtx = base->isCanonical() ? &ctx : nullptr;
    known = new (ctx, arena) DependentMemberType(base, name, canonicalCtx,
                                                 properties);
  }
  return known;
}

DependentMemberType *DependentMemberType::get(Type base,
                                              AssociatedTypeDecl *assocType) {
  assert(assocType && "Missing associated type");
  auto properties = base->getRecursiveProperties();
  properties |= RecursiveTypeProperties::HasDependentMember;
  auto arena = getArena(properties);

  llvm::PointerUnion<Identifier, AssociatedTypeDecl *> stored(assocType);
  const ASTContext &ctx = base->getASTContext();
  auto *&known = ctx.getImpl().getArena(arena).DependentMemberTypes[
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
  auto &openedExistentialArchetypes = ctx.getImpl().OpenedExistentialArchetypes;
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

  auto layout = existential->getExistentialLayout();

  SmallVector<ProtocolDecl *, 2> protos;
  for (auto proto : layout.getProtocols())
    protos.push_back(proto->getDecl());

  auto layoutConstraint = layout.getLayoutConstraint();
  auto layoutSuperclass = layout.getSuperclass();

  auto arena = AllocationArena::Permanent;
  void *mem = ctx.Allocate(
      totalSizeToAlloc<ProtocolDecl *, Type, LayoutConstraint, UUID>(
      protos.size(),
      layoutSuperclass ? 1 : 0,
      layoutConstraint ? 1 : 0, 1),
      alignof(ArchetypeType), arena);

  // FIXME: Pass in class layout constraint
  auto result =
      ::new (mem) ArchetypeType(ctx, existential,
                                protos, layoutSuperclass,
                                layoutConstraint, *knownID);
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

void TypeLoc::setInvalidType(ASTContext &C) {
  Ty = ErrorType::get(C);
}

namespace {
class raw_capturing_ostream : public raw_ostream {
  std::string Message;
  uint64_t Pos;
  CapturingTypeCheckerDebugConsumer &Listener;

public:
  raw_capturing_ostream(CapturingTypeCheckerDebugConsumer &Listener)
      : Listener(Listener) {}

  ~raw_capturing_ostream() override {
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

void SubstitutionMap::Storage::Profile(
                               llvm::FoldingSetNodeID &id,
                               GenericSignature *genericSig,
                               ArrayRef<Type> replacementTypes,
                               ArrayRef<ProtocolConformanceRef> conformances) {
  id.AddPointer(genericSig);
  if (!genericSig) return;

  // Profile those replacement types that corresponding to canonical generic
  // parameters within the generic signature.
  id.AddInteger(replacementTypes.size());

  unsigned i = 0;
  genericSig->forEachParam([&](GenericTypeParamType *gp, bool canonical) {
    if (canonical)
      id.AddPointer(replacementTypes[i].getPointer());
    else
      id.AddPointer(nullptr);
    i++;
  });

  // Conformances.
  id.AddInteger(conformances.size());
  for (auto conformance : conformances)
    id.AddPointer(conformance.getOpaqueValue());
}

SubstitutionMap::Storage *SubstitutionMap::Storage::get(
                            GenericSignature *genericSig,
                            ArrayRef<Type> replacementTypes,
                            ArrayRef<ProtocolConformanceRef> conformances) {
  // If there is no generic signature, we need no storage.
  if (!genericSig) {
    assert(replacementTypes.empty());
    assert(conformances.empty());
    return nullptr;
  }

  // Figure out which arena this should go in.
  RecursiveTypeProperties properties;
  for (auto type : replacementTypes) {
    if (type)
      properties |= type->getRecursiveProperties();
  }

  // Profile the substitution map.
  llvm::FoldingSetNodeID id;
  SubstitutionMap::Storage::Profile(id, genericSig, replacementTypes,
                                    conformances);

  auto arena = getArena(properties);

  // Did we already record this substitution map?
  auto &ctx = genericSig->getASTContext();
  void *insertPos;
  auto &substitutionMaps = ctx.getImpl().getArena(arena).SubstitutionMaps;
  if (auto result = substitutionMaps.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Allocate the appropriate amount of storage for the signature and its
  // replacement types and conformances.
  auto size = Storage::totalSizeToAlloc<Type, ProtocolConformanceRef>(
                                                      replacementTypes.size(),
                                                      conformances.size());
  auto mem = ctx.Allocate(size, alignof(Storage), arena);

  auto result = new (mem) Storage(genericSig, replacementTypes, conformances);
  substitutionMaps.InsertNode(result, insertPos);
  return result;
}

void GenericSignature::Profile(llvm::FoldingSetNodeID &ID,
                              TypeArrayView<GenericTypeParamType> genericParams,
                              ArrayRef<Requirement> requirements) {
  for (auto p : genericParams)
    ID.AddPointer(p);

  for (auto &reqt : requirements) {
    ID.AddPointer(reqt.getFirstType().getPointer());
    if (reqt.getKind() != RequirementKind::Layout)
      ID.AddPointer(reqt.getSecondType().getPointer());
    else
      ID.AddPointer(reqt.getLayoutConstraint().getPointer());
    ID.AddInteger(unsigned(reqt.getKind()));
  }
}

GenericSignature *
GenericSignature::get(ArrayRef<GenericTypeParamType *> params,
                      ArrayRef<Requirement> requirements,
                      bool isKnownCanonical) {
  SmallVector<Type, 4> paramTypes;
  for (auto param : params)
    paramTypes.push_back(param);
  auto paramsView = TypeArrayView<GenericTypeParamType>(paramTypes);
  return get(paramsView, requirements, isKnownCanonical);
}

GenericSignature *
GenericSignature::get(TypeArrayView<GenericTypeParamType> params,
                      ArrayRef<Requirement> requirements,
                      bool isKnownCanonical) {
  assert(!params.empty());

#ifndef NDEBUG
  for (auto req : requirements)
    assert(req.getFirstType()->isTypeParameter());
#endif

  // Check for an existing generic signature.
  llvm::FoldingSetNodeID ID;
  GenericSignature::Profile(ID, params, requirements);

  auto &ctx = getASTContext(params, requirements);
  void *insertPos;
  if (auto *sig = ctx.getImpl().GenericSignatures.FindNodeOrInsertPos(ID,
                                                                 insertPos)) {
    if (isKnownCanonical)
      sig->CanonicalSignatureOrASTContext = &ctx;

    return sig;
  }

  // Allocate and construct the new signature.
  size_t bytes = totalSizeToAlloc<Type, Requirement>(
      params.size(), requirements.size());
  void *mem = ctx.Allocate(bytes, alignof(GenericSignature));
  auto newSig = new (mem) GenericSignature(params, requirements,
                                           isKnownCanonical);
  ctx.getImpl().GenericSignatures.InsertNode(newSig, insertPos);
  return newSig;
}

GenericEnvironment *GenericEnvironment::getIncomplete(
                                                  GenericSignature *signature,
                                                  GenericSignatureBuilder *builder) {
  auto &ctx = signature->getASTContext();

  // Allocate and construct the new environment.
  unsigned numGenericParams = signature->getGenericParams().size();
  size_t bytes = totalSizeToAlloc<Type>(numGenericParams);
  void *mem = ctx.Allocate(bytes, alignof(GenericEnvironment));
  return new (mem) GenericEnvironment(signature, builder);
}

void DeclName::CompoundDeclName::Profile(llvm::FoldingSetNodeID &id,
                                         DeclBaseName baseName,
                                         ArrayRef<Identifier> argumentNames) {
  id.AddPointer(baseName.getAsOpaquePointer());
  id.AddInteger(argumentNames.size());
  for (auto arg : argumentNames)
    id.AddPointer(arg.get());
}

void DeclName::initialize(ASTContext &C, DeclBaseName baseName,
                          ArrayRef<Identifier> argumentNames) {
  if (argumentNames.empty()) {
    SimpleOrCompound = BaseNameAndCompound(baseName, true);
    return;
  }

  llvm::FoldingSetNodeID id;
  CompoundDeclName::Profile(id, baseName, argumentNames);

  void *insert = nullptr;
  if (CompoundDeclName *compoundName
        = C.getImpl().CompoundNames.FindNodeOrInsertPos(id, insert)) {
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
  C.getImpl().CompoundNames.InsertNode(compoundName, insert);
}

/// Build a compound value name given a base name and a set of argument names
/// extracted from a parameter list.
DeclName::DeclName(ASTContext &C, DeclBaseName baseName,
                   ParameterList *paramList) {
  SmallVector<Identifier, 4> names;
  
  for (auto P : *paramList)
    names.push_back(P->getArgumentName());
  initialize(C, baseName, names);
}

/// Find the implementation of the named type in the given module.
static NominalTypeDecl *findUnderlyingTypeInModule(ASTContext &ctx, 
                                                   Identifier name,
                                                   ModuleDecl *module) {
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
      return typealias->getDeclaredInterfaceType()->getAnyNominal();
    }
  }

  return nullptr;
}

bool ForeignRepresentationInfo::isRepresentableAsOptional() const {
  switch (getKind()) {
  case ForeignRepresentableKind::None:
    llvm_unreachable("this type is not representable");

  case ForeignRepresentableKind::Trivial:
    return Storage.getPointer() != 0;

  case ForeignRepresentableKind::Bridged: {
    auto KPK_ObjectiveCBridgeable = KnownProtocolKind::ObjectiveCBridgeable;
    ProtocolDecl *proto = getConformance()->getProtocol();
    assert(proto->isSpecificProtocol(KPK_ObjectiveCBridgeable) &&
           "unknown protocol; does it support optional?");
    (void)proto;
    (void)KPK_ObjectiveCBridgeable;

    return true;
  }

  case ForeignRepresentableKind::BridgedError:
    return true;

  case ForeignRepresentableKind::Object:
  case ForeignRepresentableKind::StaticBridged:
    llvm_unreachable("unexpected kind in ForeignRepresentableCacheEntry");
  }

  llvm_unreachable("Unhandled ForeignRepresentableKind in switch.");
}

ForeignRepresentationInfo
ASTContext::getForeignRepresentationInfo(NominalTypeDecl *nominal,
                                         ForeignLanguage language,
                                         const DeclContext *dc) {
  // Local function to add a type with the given name and module as
  // trivially-representable.
  auto addTrivial = [&](Identifier name, ModuleDecl *module,
                        bool allowOptional = false) {
    if (auto type = findUnderlyingTypeInModule(*this, name, module)) {
      auto info = ForeignRepresentationInfo::forTrivial();
      if (allowOptional)
        info = ForeignRepresentationInfo::forTrivialWithOptional();
      getImpl().ForeignRepresentableCache.insert({type, info});
    }
  };

  if (getImpl().ForeignRepresentableCache.empty()) {
    // Pre-populate the foreign-representable cache with known types.
    if (auto stdlib = getStdlibModule()) {
      addTrivial(getIdentifier("OpaquePointer"), stdlib, true);

      // Builtin types
      // FIXME: Layering violation to use the ClangImporter's define.
#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME) \
      addTrivial(getIdentifier(#SWIFT_TYPE_NAME), stdlib);
#include "swift/ClangImporter/BuiltinMappedTypes.def"

      // Even though we may never import types directly as Int or UInt
      // (e.g. on 64-bit Windows, where CLong maps to Int32 and
      // CLongLong to Int64), it's always possible to convert an Int
      // or UInt to a C type.
      addTrivial(getIdentifier("Int"), stdlib);
      addTrivial(getIdentifier("UInt"), stdlib);
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
  auto known = getImpl().ForeignRepresentableCache.find(nominal);
  bool wasNotFoundInCache = known == getImpl().ForeignRepresentableCache.end();

  // For the REPL. We might have initialized the cache above before CoreGraphics
  // was loaded.
  //   let s = "" // Here we initialize the ForeignRepresentableCache.
  //   import Foundation
  //   let pt = CGPoint(x: 1.0, y: 2.0) // Here we query for CGFloat.
  // Add CGFloat as trivial if we encounter it later.
  // If the type was not found check if it would be found after having recently
  // loaded the module.
  // Similar for types for other non stdlib modules.
  auto conditionallyAddTrivial = [&](NominalTypeDecl *nominalDecl,
                                     Identifier typeName, Identifier moduleName,
                                     bool allowOptional = false) {
    if (nominal->getName() == typeName && wasNotFoundInCache) {
      if (auto module = getLoadedModule(moduleName)) {
        addTrivial(typeName, module, allowOptional);
        known = getImpl().ForeignRepresentableCache.find(nominal);
        wasNotFoundInCache = known == getImpl().ForeignRepresentableCache.end();
      }
    }
  };
  conditionallyAddTrivial(nominal, getIdentifier("DarwinBoolean") , Id_Darwin);
  conditionallyAddTrivial(nominal, Id_Selector, Id_ObjectiveC, true);
  conditionallyAddTrivial(nominal, getIdentifier("ObjCBool"), Id_ObjectiveC);
  conditionallyAddTrivial(nominal, getSwiftId(KnownFoundationEntity::NSZone), Id_ObjectiveC, true);
  conditionallyAddTrivial(nominal, Id_CGFloat, getIdentifier("CoreGraphics"));
  const unsigned SWIFT_MAX_IMPORTED_SIMD_ELEMENTS = 4;
#define MAP_SIMD_TYPE(BASENAME, _, __)                                         \
  {                                                                            \
    char name[] = #BASENAME "0";                                               \
    for (unsigned i = 2; i <= SWIFT_MAX_IMPORTED_SIMD_ELEMENTS; ++i) {         \
      *(std::end(name) - 2) = '0' + i;                                         \
      conditionallyAddTrivial(nominal, getIdentifier(name), Id_simd);          \
    }                                                                          \
  }
#include "swift/ClangImporter/SIMDMappedTypes.def"

  if (wasNotFoundInCache ||
      (known->second.getKind() == ForeignRepresentableKind::None &&
       known->second.getGeneration() < CurrentGeneration)) {
    Optional<ForeignRepresentationInfo> result;

    // Look for a conformance to _ObjectiveCBridgeable (other than Optional's--
    // we don't want to allow exposing APIs with double-optional types like
    // NSObject??, even though Optional is bridged to its underlying type).
    //
    // FIXME: We're implicitly depending on the fact that lookupConformance
    // is global, ignoring the module we provide for it.
    if (nominal != dc->getASTContext().getOptionalDecl()) {
      if (auto objcBridgeable
            = getProtocol(KnownProtocolKind::ObjectiveCBridgeable)) {
        if (auto conformance
              = dc->getParentModule()->lookupConformance(
                  nominal->getDeclaredType(), objcBridgeable)) {
          result =
              ForeignRepresentationInfo::forBridged(conformance->getConcrete());
        }
      }
    }

    // Error is bridged to NSError, when it's available.
    if (nominal == getErrorDecl() && getNSErrorDecl())
      result = ForeignRepresentationInfo::forBridgedError();

    // If we didn't find anything, mark the result as "None".
    if (!result)
      result = ForeignRepresentationInfo::forNone(CurrentGeneration);
    
    // Cache the result.
    known = getImpl().ForeignRepresentableCache.insert({ nominal, *result }).first;
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

    LLVM_FALLTHROUGH;

  case ForeignLanguage::ObjectiveC:
    return entry;
  }

  llvm_unreachable("Unhandled ForeignLanguage in switch.");
}

bool ASTContext::isTypeBridgedInExternalModule(
     NominalTypeDecl *nominal) const {
  return (nominal == getBoolDecl() ||
          nominal == getIntDecl() ||
          nominal == getInt64Decl() ||
          nominal == getInt32Decl() ||
          nominal == getInt16Decl() ||
          nominal == getInt8Decl() ||
          nominal == getUIntDecl() ||
          nominal == getUInt64Decl() ||
          nominal == getUInt32Decl() ||
          nominal == getUInt16Decl() ||
          nominal == getUInt8Decl() ||
          nominal == getFloatDecl() ||
          nominal == getDoubleDecl() ||
          nominal == getArrayDecl() ||
          nominal == getDictionaryDecl() ||
          nominal == getSetDecl() ||
          nominal == getStringDecl() ||
          nominal == getSubstringDecl() ||
          nominal == getErrorDecl() ||
          nominal == getAnyHashableDecl() ||
          // Foundation's overlay depends on the CoreGraphics overlay, but
          // CoreGraphics value types bridge to Foundation objects such as
          // NSValue and NSNumber, so to avoid circular dependencies, the
          // bridging implementations of CG types appear in the Foundation
          // module.
          nominal->getParentModule()->getName() == Id_CoreGraphics ||
          // CoreMedia is a dependency of AVFoundation, but the bridged
          // NSValue implementations for CMTime, CMTimeRange, and
          // CMTimeMapping are provided by AVFoundation, and AVFoundation
          // gets upset if you don't use the NSValue subclasses its factory
          // methods instantiate.
          nominal->getParentModule()->getName() == Id_CoreMedia);
}

bool ASTContext::isObjCClassWithMultipleSwiftBridgedTypes(Type t) {
  auto clas = t->getClassOrBoundGenericClass();
  if (!clas)
    return false;
  
  if (clas == getNSErrorDecl())
    return true;
  if (clas == getNSNumberDecl())
    return true;
  if (clas == getNSValueDecl())
    return true;
  
  return false;
}

Type ASTContext::getBridgedToObjC(const DeclContext *dc, Type type,
                                  Type *bridgedValueType) const {
  if (type->isBridgeableObjectType()) {
    if (bridgedValueType) *bridgedValueType = type;

    return type;
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
      // Don't ascribe any behavior to Optional other than what we explicitly
      // give it. We don't want things like AnyObject?? to work.
      if (type->getAnyNominal() == getOptionalDecl())
        return None;
      
      // Find the protocol.
      auto proto = getProtocol(known);
      if (!proto) return None;

      return dc->getParentModule()->lookupConformance(type, proto);
    };

  // Do we conform to _ObjectiveCBridgeable?
  if (auto conformance
        = findConformance(KnownProtocolKind::ObjectiveCBridgeable)) {
    // The corresponding value type is... the type.
    if (bridgedValueType)
      *bridgedValueType = type;

    // Find the Objective-C class type we bridge to.
    if (conformance->isConcrete()) {
      return ProtocolConformanceRef::getTypeWitnessByName(
               type, *conformance, Id_ObjectiveCType,
               getLazyResolver());
    } else {
      return type->castTo<ArchetypeType>()->getNestedType(Id_ObjectiveCType);
    }
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

  // No special bridging to Objective-C, but this can become an 'Any'.
  return Type();
}

CanGenericSignature ASTContext::getSingleGenericParameterSignature() const {
  if (auto theSig = getImpl().SingleGenericParameterSignature)
    return theSig;
  
  auto param = GenericTypeParamType::get(0, 0, *this);
  auto sig = GenericSignature::get(param, { });
  auto canonicalSig = CanGenericSignature(sig);
  getImpl().SingleGenericParameterSignature = canonicalSig;
  return canonicalSig;
}

CanGenericSignature ASTContext::getExistentialSignature(CanType existential,
                                                        ModuleDecl *mod) {
  auto found = getImpl().ExistentialSignatures.find(existential);
  if (found != getImpl().ExistentialSignatures.end())
    return found->second;

  assert(existential.isExistentialType());

  GenericSignatureBuilder builder(*this);

  auto genericParam = GenericTypeParamType::get(0, 0, *this);
  builder.addGenericParameter(genericParam);

  Requirement requirement(RequirementKind::Conformance, genericParam,
                          existential);
  auto source =
    GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  builder.addRequirement(requirement, source, nullptr);

  CanGenericSignature genericSig(std::move(builder).computeGenericSignature(SourceLoc()));

  auto result = getImpl().ExistentialSignatures.insert(
    std::make_pair(existential, genericSig));
  assert(result.second);
  (void) result;

  return genericSig;
}

SILLayout *SILLayout::get(ASTContext &C,
                          CanGenericSignature Generics,
                          ArrayRef<SILField> Fields) {
  // Profile the layout parameters.
  llvm::FoldingSetNodeID id;
  Profile(id, Generics, Fields);
  
  // Return an existing layout if there is one.
  void *insertPos;
  auto &Layouts = C.getImpl().SILLayouts;
  
  if (auto existing = Layouts.FindNodeOrInsertPos(id, insertPos))
    return existing;
  
  // Allocate a new layout.
  void *memory = C.Allocate(totalSizeToAlloc<SILField>(Fields.size()),
                            alignof(SILLayout));
  
  auto newLayout = ::new (memory) SILLayout(Generics, Fields);
  Layouts.InsertNode(newLayout, insertPos);
  return newLayout;
}

CanSILBoxType SILBoxType::get(ASTContext &C,
                              SILLayout *Layout,
                              SubstitutionMap Substitutions) {
  // Canonicalize substitutions.
  Substitutions = Substitutions.getCanonical();

  // Return an existing layout if there is one.
  void *insertPos;
  auto &SILBoxTypes = C.getImpl().SILBoxTypes;
  llvm::FoldingSetNodeID id;
  Profile(id, Layout, Substitutions);
  if (auto existing = SILBoxTypes.FindNodeOrInsertPos(id, insertPos))
    return CanSILBoxType(existing);

  auto newBox = new (C, AllocationArena::Permanent) SILBoxType(C, Layout,
                                                               Substitutions);
  SILBoxTypes.InsertNode(newBox, insertPos);
  return CanSILBoxType(newBox);
}

/// TODO: Transitional factory to present the single-type SILBoxType::get
/// interface.
CanSILBoxType SILBoxType::get(CanType boxedType) {
  auto &ctx = boxedType->getASTContext();
  auto singleGenericParamSignature = ctx.getSingleGenericParameterSignature();
  auto genericParam = singleGenericParamSignature->getGenericParams()[0];
  auto layout = SILLayout::get(ctx, singleGenericParamSignature,
                               SILField(CanType(genericParam),
                                        /*mutable*/ true));

  auto subMap =
    SubstitutionMap::get(
      singleGenericParamSignature,
      [&](SubstitutableType *type) -> Type {
        if (type->isEqual(genericParam)) return boxedType;

        return nullptr;
      },
      MakeAbstractConformanceForGenericType());
  return get(boxedType->getASTContext(), layout, subMap);
}

LayoutConstraint
LayoutConstraint::getLayoutConstraint(LayoutConstraintKind Kind,
                                      ASTContext &C) {
  return getLayoutConstraint(Kind, 0, 0, C);
}

LayoutConstraint LayoutConstraint::getLayoutConstraint(LayoutConstraintKind Kind,
                                                      unsigned SizeInBits,
                                                      unsigned Alignment,
                                                      ASTContext &C) {
  if (!LayoutConstraintInfo::isKnownSizeTrivial(Kind)) {
    assert(SizeInBits == 0);
    assert(Alignment == 0);
    return getLayoutConstraint(Kind);
  }

  // Check to see if we've already seen this tuple before.
  llvm::FoldingSetNodeID ID;
  LayoutConstraintInfo::Profile(ID, Kind, SizeInBits, Alignment);

  void *InsertPos = nullptr;
  if (LayoutConstraintInfo *Layout =
          C.getImpl().getArena(AllocationArena::Permanent)
              .LayoutConstraints.FindNodeOrInsertPos(ID, InsertPos))
    return LayoutConstraint(Layout);

  LayoutConstraintInfo *New =
      LayoutConstraintInfo::isTrivial(Kind)
          ? new (C, AllocationArena::Permanent)
                LayoutConstraintInfo(Kind, SizeInBits, Alignment)
          : new (C, AllocationArena::Permanent) LayoutConstraintInfo(Kind);
  C.getImpl().getArena(AllocationArena::Permanent)
      .LayoutConstraints.InsertNode(New, InsertPos);
  return LayoutConstraint(New);
}


