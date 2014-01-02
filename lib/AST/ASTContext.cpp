//===--- ASTContext.cpp - ASTContext Implementation -----------------------===//
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
//  This file implements the ASTContext class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTMutationListener.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/TypeCheckerDebugConsumer.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/Allocator.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include <memory>

using namespace swift;

ASTMutationListener::~ASTMutationListener() = default;
LazyResolver::~LazyResolver() = default;
void ModuleLoader::anchor() {}

llvm::StringRef swift::getProtocolName(KnownProtocolKind kind) {
  switch (kind) {
#define PROTOCOL(Id) \
  case KnownProtocolKind::Id: \
    return #Id;
#include "swift/AST/KnownProtocols.def"
  }
}

struct ASTContext::Implementation {
  Implementation();
  ~Implementation();

  llvm::BumpPtrAllocator Allocator; // used in later initializations

  /// The set of cleanups to be called when the ASTContext is destroyed.
  std::vector<std::function<void(void)>> Cleanups;

  llvm::StringMap<char, llvm::BumpPtrAllocator&> IdentifierTable;

  /// The declaration of swift.Slice<T>.
  NominalTypeDecl *SliceDecl = nullptr;

  /// The declaration of swift.Optional<T>.
  EnumDecl *OptionalDecl = nullptr;

  /// The declaration of swift.Optional<T>.Some.
  EnumElementDecl *OptionalSomeDecl = nullptr;
  
  /// The declaration of swift.Optional<T>.None.
  EnumElementDecl *OptionalNoneDecl = nullptr;
  
  /// func _doesOptionalHaveValue<T>(v : [inout] Optional<T>) -> T
  FuncDecl *DoesOptionalHaveValueDecl = nullptr;

  /// func _getOptionalValue<T>(v : Optional<T>) -> T
  FuncDecl *GetOptionalValueDecl = nullptr;

  /// func _injectValueIntoOptional<T>(v : T) -> Optional<T>
  FuncDecl *InjectValueIntoOptionalDecl = nullptr;

  /// func _injectNothingIntoOptional<T>() -> Optional<T>
  FuncDecl *InjectNothingIntoOptionalDecl = nullptr;

  /// func _getBool(Builtin.Int1) -> Bool
  FuncDecl *GetBoolDecl = nullptr;

  /// \brief The set of known protocols, lazily populated as needed.
  ProtocolDecl *KnownProtocols[NumKnownProtocols] = { };

  /// \brief The various module loaders that import external modules into this
  /// ASTContext.
  SmallVector<llvm::IntrusiveRefCntPtr<swift::ModuleLoader>, 4> ModuleLoaders;

  /// \brief The module loader used to load Clang modules.
  // FIXME: We shouldn't be special-casing Clang.
  llvm::IntrusiveRefCntPtr<swift::ModuleLoader> ClangModuleLoader;

  /// \brief The set of AST mutation listeners.
  SmallVector<ASTMutationListener *, 4> MutationListeners;

  /// \brief Map from Swift declarations to the Clang nodes from which
  /// they were imported.
  llvm::DenseMap<const Decl *, ClangNode> ClangNodes;

  /// \brief Map from local declarations to their discriminators.
  /// Missing entries implicitly have value 0.
  llvm::DenseMap<const ValueDecl *, unsigned> LocalDiscriminators;

  /// \brief A cached unused pattern-binding initializer context.
  PatternBindingInitializer *UnusedPatternBindingContext = nullptr;
  
  /// \brief A cached unused default-argument initializer context.
  DefaultArgumentInitializer *UnusedDefaultArgumentContext = nullptr;

  /// \brief Structure that captures data that is segregated into different
  /// arenas.
  struct Arena {
    llvm::FoldingSet<TupleType> TupleTypes;
    llvm::DenseMap<std::pair<Type,char>, MetatypeType*> MetatypeTypes;
    llvm::DenseMap<std::pair<Type,std::pair<Type,char>>, FunctionType*>
      FunctionTypes;
    llvm::DenseMap<std::pair<Type, uint64_t>, ArrayType*> ArrayTypes;
    llvm::DenseMap<Type, ArraySliceType*> ArraySliceTypes;
    llvm::DenseMap<Type, OptionalType*> OptionalTypes;
    llvm::DenseMap<Type, ParenType*> ParenTypes;
    llvm::DenseMap<uintptr_t, ReferenceStorageType*> ReferenceStorageTypes;
    llvm::DenseMap<Type, LValueType*> LValueTypes;
    llvm::DenseMap<Type, InOutType*> InOutTypes;
    llvm::DenseMap<std::pair<Type, Type>, SubstitutedType *> SubstitutedTypes;
    llvm::DenseMap<std::pair<Type, void*>, DependentMemberType *>
      DependentMemberTypes;
    llvm::FoldingSet<EnumType> EnumTypes;
    llvm::FoldingSet<StructType> StructTypes;
    llvm::FoldingSet<ClassType> ClassTypes;
    llvm::FoldingSet<UnboundGenericType> UnboundGenericTypes;
    llvm::FoldingSet<BoundGenericType> BoundGenericTypes;
  };
  
  llvm::DenseMap<Module*, ModuleType*> ModuleTypes;
  llvm::DenseMap<std::pair<unsigned, unsigned>, GenericTypeParamType *>
    GenericParamTypes;
  llvm::FoldingSet<GenericFunctionType> GenericFunctionTypes;
  llvm::FoldingSet<SILFunctionType> SILFunctionTypes;
  llvm::DenseMap<BuiltinIntegerWidth, BuiltinIntegerType*> IntegerTypes;
  llvm::FoldingSet<ProtocolCompositionType> ProtocolCompositionTypes;
  llvm::FoldingSet<BuiltinVectorType> BuiltinVectorTypes;
  
  /// \brief The permanent arena.
  Arena Permanent;

    /// FIXME: Move into arena.
  llvm::DenseMap<BoundGenericType *, ArrayRef<Substitution>>
    BoundGenericSubstitutions;

  using ConformanceListPair = std::pair<unsigned, SmallVector<Decl *, 8>>;

  /// \brief The set of nominal types and extensions thereof known to conform
  /// to compiler-known protocols.
  ConformanceListPair KnownProtocolConformances[NumKnownProtocols];

  /// The list of normal protocol conformances.
  ///
  /// Since these conformances are tied explicitly to the source code, semantic
  /// analysis is responsible for handling the uniquing.
  SmallVector<NormalProtocolConformance *, 2> NormalConformances;

  /// The set of specialized protocol conformances.
  llvm::FoldingSet<SpecializedProtocolConformance> SpecializedConformances;

  /// The set of inherited protocol conformances.
  llvm::FoldingSet<InheritedProtocolConformance> InheritedConformances;

  /// \brief Temporary arena used for a constraint solver.
  struct ConstraintSolverArena : public Arena {
    /// \brief The allocator used for all allocations within this arena.
    llvm::BumpPtrAllocator &Allocator;

    ConstraintSolverArena(llvm::BumpPtrAllocator &Allocator)
      : Allocator(Allocator) { }

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
  }
};

ASTContext::Implementation::Implementation()
 : IdentifierTable(Allocator) {}
ASTContext::Implementation::~Implementation() {
  for (auto &cleanup : Cleanups)
    cleanup();
}

ConstraintCheckerArenaRAII::
ConstraintCheckerArenaRAII(ASTContext &self, llvm::BumpPtrAllocator &allocator)
  : Self(self), Data(self.Impl.CurrentConstraintSolverArena.release())
{
  Self.Impl.CurrentConstraintSolverArena.reset(
    new ASTContext::Implementation::ConstraintSolverArena(allocator));
}

ConstraintCheckerArenaRAII::~ConstraintCheckerArenaRAII() {
  Self.Impl.CurrentConstraintSolverArena.reset(
    (ASTContext::Implementation::ConstraintSolverArena *)Data);
}

static Module *createBuiltinModule(ASTContext &ctx) {
  auto M = new (ctx) Module(ctx.getIdentifier("Builtin"), ctx);
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
    StdlibModuleName(getIdentifier("swift")),
    SelfIdentifier(getIdentifier("self")),
    TypeCheckerDebug(new StderrTypeCheckerDebugConsumer()),
    TheErrorType(new (*this, AllocationArena::Permanent) ErrorType(*this)),
    TheEmptyTupleType(TupleType::get(ArrayRef<TupleTypeElt>(), *this)),
    TheObjectPointerType(new (*this, AllocationArena::Permanent)
                           BuiltinObjectPointerType(*this)),
    TheObjCPointerType(new (*this, AllocationArena::Permanent)
                         BuiltinObjCPointerType(*this)),
    TheRawPointerType(new (*this, AllocationArena::Permanent)
                        BuiltinRawPointerType(*this)),
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
                    BuiltinFloatType(BuiltinFloatType::PPC128,*this)) {
}

ASTContext::~ASTContext() {
  // Tear down protocol conformances.
  for (auto conformance : Impl.NormalConformances)
    conformance->~NormalProtocolConformance();
  for (auto &conformance : Impl.SpecializedConformances)
    conformance.~SpecializedProtocolConformance();

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
}

/// getIdentifier - Return the uniqued and AST-Context-owned version of the
/// specified string.
Identifier ASTContext::getIdentifier(StringRef Str) const {
  // Make sure null pointers stay null.
  if (Str.data() == nullptr) return Identifier(0);
  
  return Identifier(Impl.IdentifierTable.GetOrCreateValue(Str).getKeyData());
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
static NominalTypeDecl *findSyntaxSugarImpl(const ASTContext &ctx,
                                            StringRef name) {
  // Find all of the declarations with this name in the Swift module.
  SmallVector<ValueDecl *, 1> results;
  ctx.lookupInSwiftModule(name, results);
  for (auto result : results) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(result)) {
      if (auto params = nominal->getGenericParams()) {
        if (params->size() == 1) {
          // We found it.
          return nominal;
        }
      }
    }
  }

  return nullptr;
}

NominalTypeDecl *ASTContext::getSliceDecl() const {
  if (!Impl.SliceDecl)
    Impl.SliceDecl = findSyntaxSugarImpl(*this, "Array");

  return Impl.SliceDecl;
}

EnumDecl *ASTContext::getOptionalDecl() const {
  if (!Impl.OptionalDecl)
    Impl.OptionalDecl
      = dyn_cast_or_null<EnumDecl>(findSyntaxSugarImpl(*this, "Optional"));

  return Impl.OptionalDecl;
}

static EnumElementDecl *findEnumElement(EnumDecl *e, StringRef name) {
  if (!e) return nullptr;
  auto ident = e->getASTContext().getIdentifier(name);
  for (auto elt : e->getAllElements()) {
    if (elt->getName() == ident)
      return elt;
  }
  return nullptr;
}

EnumElementDecl *ASTContext::getOptionalSomeDecl() const {
  if (!Impl.OptionalSomeDecl)
    Impl.OptionalSomeDecl = findEnumElement(getOptionalDecl(), "Some");
  return Impl.OptionalSomeDecl;
}

EnumElementDecl *ASTContext::getOptionalNoneDecl() const {
  if (!Impl.OptionalNoneDecl)
    Impl.OptionalNoneDecl = findEnumElement(getOptionalDecl(), "None");
  return Impl.OptionalNoneDecl;
}

ProtocolDecl *ASTContext::getProtocol(KnownProtocolKind kind) const {
  // Check whether we've already looked for and cached this protocol.
  unsigned index = (unsigned)kind;
  assert(index < NumKnownProtocols && "Number of known protocols is wrong");
  if (Impl.KnownProtocols[index])
    return Impl.KnownProtocols[index];

  // Find all of the declarations with this name in the Swift module.
  SmallVector<ValueDecl *, 1> results;
  lookupInSwiftModule(getProtocolName(kind), results);
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
  auto fnType = dyn_cast<FunctionType>(fn->getType()->getCanonicalType());
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

/// Check whether the given function is generic over a single,
/// unconstrained archetype.
static bool isGenericIntrinsic(FuncDecl *fn, CanType &input, CanType &output,
                               CanType &param) {
  auto fnType =
    dyn_cast<PolymorphicFunctionType>(fn->getType()->getCanonicalType());
  if (!fnType || fnType->getAllArchetypes().size() != 1)
    return false;

  auto paramType = CanArchetypeType(fnType->getAllArchetypes()[0]);
  if (paramType->hasRequirements())
    return false;

  param = paramType;
  input = stripImmediateLabels(fnType.getInput());
  output = stripImmediateLabels(fnType.getResult());
  return true;
}

/// Check whether the given type is Optional applied to the given
/// type argument.
static bool isOptionalType(const ASTContext &ctx, CanType type,
                           CanType arg) {
  if (auto boundType = dyn_cast<BoundGenericType>(type)) {
    return (boundType->getDecl() == ctx.getOptionalDecl() &&
            boundType.getGenericArgs().size() == 1 &&
            boundType.getGenericArgs()[0] == arg);
  }
  return false;
}

FuncDecl *
ASTContext::getDoesOptionalHaveValueDecl(LazyResolver *resolver) const {
  if (Impl.DoesOptionalHaveValueDecl)
    return Impl.DoesOptionalHaveValueDecl;

  // Look for a generic function.
  CanType input, output, param;
  auto decl = findLibraryIntrinsic(*this, "_doesOptionalHaveValue", resolver);
  if (!decl || !isGenericIntrinsic(decl, input, output, param))
    return nullptr;

  // Input must be @inout Optional<T>.
  auto inputInOut = dyn_cast<InOutType>(input);
  if (!inputInOut || !isOptionalType(*this, inputInOut.getObjectType(), param))
    return nullptr;

  // Output must be Builtin.Int1.
  if (!isBuiltinInt1Type(output))
    return nullptr;

  Impl.DoesOptionalHaveValueDecl = decl;
  return decl;
}

FuncDecl *ASTContext::getGetOptionalValueDecl(LazyResolver *resolver) const {
  if (Impl.GetOptionalValueDecl)
    return Impl.GetOptionalValueDecl;

  // Look for the function.
  CanType input, output, param;
  auto decl = findLibraryIntrinsic(*this, "_getOptionalValue", resolver);
  if (!decl || !isGenericIntrinsic(decl, input, output, param))
    return nullptr;

  // Input must be Optional<T>.
  if (!isOptionalType(*this, input, param))
    return nullptr;

  // Output must be T.
  if (output != param)
    return nullptr;

  Impl.GetOptionalValueDecl = decl;
  return decl;
}

FuncDecl *
ASTContext::getInjectValueIntoOptionalDecl(LazyResolver *resolver) const {
  if (Impl.InjectValueIntoOptionalDecl)
    return Impl.InjectValueIntoOptionalDecl;

  // Look for the function.
  CanType input, output, param;
  auto decl = findLibraryIntrinsic(*this, "_injectValueIntoOptional",
                                   resolver);
  if (!decl || !isGenericIntrinsic(decl, input, output, param))
    return nullptr;

  // Input must be T.
  if (input != param)
    return nullptr;

  // Output must be Optional<T>.
  if (!isOptionalType(*this, output, param))
    return nullptr;

  Impl.InjectValueIntoOptionalDecl = decl;
  return decl;
}

FuncDecl *
ASTContext::getInjectNothingIntoOptionalDecl(LazyResolver *resolver) const {
  if (Impl.InjectNothingIntoOptionalDecl)
    return Impl.InjectNothingIntoOptionalDecl;

  // Look for the function.
  CanType input, output, param;
  auto decl = findLibraryIntrinsic(*this, "_injectNothingIntoOptional",
                                   resolver);
  if (!decl || !isGenericIntrinsic(decl, input, output, param))
    return nullptr;

  // Input must be ().
  auto inputTuple = dyn_cast<TupleType>(input);
  if (!inputTuple || inputTuple->getNumElements() != 0)
    return nullptr;

  // Output must be Optional<T>.
  if (!isOptionalType(*this, output, param))
    return nullptr;

  Impl.InjectNothingIntoOptionalDecl = decl;
  return decl;
}

bool ASTContext::hasOptionalIntrinsics(LazyResolver *resolver) const {
  return getOptionalDecl() &&
         getOptionalSomeDecl() &&
         getOptionalNoneDecl() &&
         getDoesOptionalHaveValueDecl(resolver) &&
         getGetOptionalValueDecl(resolver) &&
         getInjectValueIntoOptionalDecl(resolver) &&
         getInjectNothingIntoOptionalDecl(resolver);
}

void ASTContext::addMutationListener(ASTMutationListener &listener) {
  Impl.MutationListeners.push_back(&listener);
}

void ASTContext::removeMutationListener(ASTMutationListener &listener) {
  auto known = std::find(Impl.MutationListeners.rbegin(),
                         Impl.MutationListeners.rend(),
                         &listener);
  assert(known != Impl.MutationListeners.rend() && "listener not registered");
  Impl.MutationListeners.erase(known.base()-1);
}

void ASTContext::addedExternalDecl(Decl *decl) {
  for (auto listener : Impl.MutationListeners)
    listener->addedExternalDecl(decl);
}

void ASTContext::addCleanup(std::function<void(void)> cleanup) {
  Impl.Cleanups.push_back(std::move(cleanup));
}

bool ASTContext::hadError() const {
  return Diags.hadAnyError();
}

Optional<ArrayRef<Substitution>>
ASTContext::createTrivialSubstitutions(BoundGenericType *BGT) const {
  assert(BGT->isCanonical() && "Requesting non-canonical substitutions");
  ArrayRef<GenericParam> Params =
      BGT->getDecl()->getGenericParams()->getParams();
  assert(Params.size() == 1);
  auto Param = Params[0];
  assert(Param.getAsTypeParam()->getArchetype() && "Not type-checked yet");
  Substitution Subst;
  Subst.Archetype = Param.getAsTypeParam()->getArchetype();
  Subst.Replacement = BGT->getGenericArgs()[0];
  auto Substitutions = AllocateCopy(llvm::makeArrayRef(Subst));
  Impl.BoundGenericSubstitutions.insert(std::make_pair(BGT, Substitutions));
  return Substitutions;
}

Optional<ArrayRef<Substitution>>
ASTContext::getSubstitutions(BoundGenericType* bound) const {
  assert(bound->isCanonical() && "Requesting non-canonical substitutions");
  auto known = Impl.BoundGenericSubstitutions.find(bound);
  if (known != Impl.BoundGenericSubstitutions.end())
    return known->second;

  // We can trivially create substitutions for Slice and Optional.
  if (bound->getDecl() == getSliceDecl() ||
      bound->getDecl() == getOptionalDecl())
    return createTrivialSubstitutions(bound);

  return Nothing;
}

void ASTContext::setSubstitutions(BoundGenericType* Bound,
                                  ArrayRef<Substitution> Subs) const {
  assert(Bound->isCanonical() && "Requesting non-canonical substitutions");
  assert(Impl.BoundGenericSubstitutions.count(Bound) == 0 &&
         "Already have substitutions?");
  Impl.BoundGenericSubstitutions[Bound] = Subs;
}

void ASTContext::addModuleLoader(llvm::IntrusiveRefCntPtr<ModuleLoader> loader,
                                 bool IsClang) {
  Impl.ModuleLoaders.push_back(loader);
  if (IsClang) {
    assert(!Impl.ClangModuleLoader && "Already have a Clang module loader");
    Impl.ClangModuleLoader = std::move(loader);
  }
}

void ASTContext::loadExtensions(NominalTypeDecl *nominal,
                                unsigned previousGeneration) {
  for (auto loader : Impl.ModuleLoaders) {
    loader->loadExtensions(nominal, previousGeneration);
  }
}

llvm::IntrusiveRefCntPtr<ModuleLoader> ASTContext::getClangModuleLoader() const{
  return Impl.ClangModuleLoader;
}

static void recordKnownProtocol(Module *Stdlib, StringRef Name,
                                KnownProtocolKind Kind) {
  Identifier ID = Stdlib->Ctx.getIdentifier(Name);
  UnqualifiedLookup Lookup(ID, Stdlib, nullptr, SourceLoc(), /*IsType=*/true);
  if (auto Proto = dyn_cast_or_null<ProtocolDecl>(Lookup.getSingleTypeResult()))
    Proto->setKnownProtocolKind(Kind);
}

void ASTContext::recordKnownProtocols(Module *Stdlib) {
#define PROTOCOL(Name) \
  recordKnownProtocol(Stdlib, #Name, KnownProtocolKind::Name);
#include "swift/AST/KnownProtocols.def"
}

void ASTContext::recordConformingDecl(ValueDecl *ConformingD,
                                      ValueDecl *ConformanceD) {
  assert(ConformingD && ConformanceD);
  auto &Vec = ConformingDeclMap[ConformingD];
  // The vector should commonly have few elements.
  if (std::find(Vec.begin(), Vec.end(), ConformanceD) == Vec.end()) {
    Vec.push_back(ConformanceD);
    ConformingD->setConformsToProtocolRequirement();
  }
}

ArrayRef<ValueDecl *> ASTContext::getConformances(ValueDecl *D) {
  return ConformingDeclMap[D];
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
  return LoadedModules.lookup(ModuleName.str());
}

Module *
ASTContext::getModule(ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath) {
  assert(!ModulePath.empty());

  if (auto *M = getLoadedModule(ModulePath))
    return M;

  auto moduleID = ModulePath[0];
  for (auto importer : Impl.ModuleLoaders) {
    if (Module *M = importer->loadModule(moduleID.second, ModulePath)) {
      if (ModulePath.size() == 1 && ModulePath[0].first == StdlibModuleName)
        recordKnownProtocols(M);
      return M;
    }
  }

  return nullptr;
}

Module *ASTContext::getStdlibModule() const {
  if (TheStdlibModule)
    return TheStdlibModule;

  TheStdlibModule = getLoadedModule(StdlibModuleName);
  return TheStdlibModule;
}

ClangNode ASTContext::getClangNode(const Decl *decl) {
  auto known = Impl.ClangNodes.find(decl);
  assert(known != Impl.ClangNodes.end() && "No Clang node?");
  return known->second;
}

void ASTContext::setClangNode(const Decl *decl, ClangNode node) {
  Impl.ClangNodes[decl] = node;
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
ASTContext::createPatternBindingContext(PatternBindingDecl *binding) {
  // Check for an existing context we can re-use.
  if (auto existing = Impl.UnusedPatternBindingContext) {
    Impl.UnusedPatternBindingContext = nullptr;
    existing->reset(binding);
    return existing;
  }

  return new (*this) PatternBindingInitializer(binding);
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

void ASTContext::recordConformance(KnownProtocolKind protocolKind, Decl *decl) {
  assert(isa<NominalTypeDecl>(decl) || isa<ExtensionDecl>(decl));
  auto index = static_cast<unsigned>(protocolKind);
  assert(index < NumKnownProtocols);
  Impl.KnownProtocolConformances[index].second.push_back(decl);
}

/// \brief Retrieve the set of nominal types and extensions thereof that
/// conform to the given protocol.
ArrayRef<Decl *> ASTContext::getTypesThatConformTo(KnownProtocolKind kind) {
  auto index = static_cast<unsigned>(kind);
  assert(index < NumKnownProtocols);

  for (auto loader : Impl.ModuleLoaders) {
    loader->loadDeclsConformingTo(kind,
                                  Impl.KnownProtocolConformances[index].first);
  }
  Impl.KnownProtocolConformances[index].first = CurrentGeneration;

  return Impl.KnownProtocolConformances[index].second;
}

NormalProtocolConformance *
ASTContext::getConformance(Type conformingType,
                           ProtocolDecl *protocol,
                           SourceLoc loc,
                           DeclContext *dc,
                           ProtocolConformanceState state) {
  auto result
    = new (*this) NormalProtocolConformance(conformingType, protocol, loc,
                                            dc, state);
  Impl.NormalConformances.push_back(result);
  return result;
}

SpecializedProtocolConformance *
ASTContext::getSpecializedConformance(Type type,
                                      ProtocolConformance *generic,
                                      ArrayRef<Substitution> substitutions) {
  llvm::FoldingSetNodeID id;
  SpecializedProtocolConformance::Profile(id, type, generic);

  // Did we already record the specialized conformance?
  void *insertPos;
  if (auto result
        = Impl.SpecializedConformances.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new specialized conformance.
  auto result
    = new (*this) SpecializedProtocolConformance(type, generic, substitutions);
  Impl.SpecializedConformances.InsertNode(result, insertPos);
  return result;
}

InheritedProtocolConformance *
ASTContext::getInheritedConformance(Type type, ProtocolConformance *inherited) {
  llvm::FoldingSetNodeID id;
  InheritedProtocolConformance::Profile(id, type, inherited);

  // Did we already record the specialized conformance?
  void *insertPos;
  if (auto result
        = Impl.InheritedConformances.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new specialized conformance.
  auto result = new (*this) InheritedProtocolConformance(type, inherited);
  Impl.InheritedConformances.InsertNode(result, insertPos);
  return result;
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

/// \brief Retrieve the arena from which we should allocate storage for a type.
static AllocationArena getArena(bool hasTypeVariable) {
  return hasTypeVariable? AllocationArena::ConstraintSolver
                        : AllocationArena::Permanent;;
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
  bool hasTypeVariable = underlying->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);
  ParenType *&Result = C.Impl.getArena(arena).ParenTypes[underlying];
  if (Result == 0) {
    Result = new (C, arena) ParenType(underlying, hasTypeVariable);
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
    ID.AddPointer(Elt.getName().get());
    ID.AddPointer(Elt.TyAndDefaultOrVarArg.getOpaqueValue());
  }
}

/// getTupleType - Return the uniqued tuple type with the specified elements.
Type TupleType::get(ArrayRef<TupleTypeElt> Fields, const ASTContext &C) {
  if (Fields.size() == 1 && !Fields[0].isVararg() && !Fields[0].hasName())
    return ParenType::get(C, Fields[0].getType());

  bool HasTypeVariable = false;
  for (const TupleTypeElt &Elt : Fields) {
    if (Elt.getType() && Elt.getType()->hasTypeVariable()) {
      HasTypeVariable = true;
      break;
    }
  }

  auto arena = getArena(HasTypeVariable);


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
                                            HasTypeVariable);
  C.Impl.getArena(arena).TupleTypes.InsertNode(New, InsertPos);
  return New;
}

void UnboundGenericType::Profile(llvm::FoldingSetNodeID &ID,
                                 NominalTypeDecl *TheDecl, Type Parent) {
  ID.AddPointer(TheDecl);
  ID.AddPointer(Parent.getPointer());
}

UnboundGenericType* UnboundGenericType::get(NominalTypeDecl *TheDecl,
                                            Type Parent,
                                            const ASTContext &C) {
  llvm::FoldingSetNodeID ID;
  UnboundGenericType::Profile(ID, TheDecl, Parent);
  void *InsertPos = 0;
  bool hasTypeVariable = Parent && Parent->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  if (auto unbound = C.Impl.getArena(arena).UnboundGenericTypes
                        .FindNodeOrInsertPos(ID, InsertPos))
    return unbound;

  auto result = new (C, arena) UnboundGenericType(TheDecl, Parent, C,
                                                  hasTypeVariable);
  C.Impl.getArena(arena).UnboundGenericTypes.InsertNode(result, InsertPos);
  return result;
}

void BoundGenericType::Profile(llvm::FoldingSetNodeID &ID,
                               NominalTypeDecl *TheDecl, Type Parent,
                               ArrayRef<Type> GenericArgs,
                               bool &hasTypeVariable) {
  ID.AddPointer(TheDecl);
  ID.AddPointer(Parent.getPointer());
  if (Parent && Parent->hasTypeVariable())
    hasTypeVariable = true;
  ID.AddInteger(GenericArgs.size());
  for (Type Arg : GenericArgs) {
    ID.AddPointer(Arg.getPointer());
    if (Arg->hasTypeVariable())
      hasTypeVariable = true;
  }
}

BoundGenericType::BoundGenericType(TypeKind theKind,
                                   NominalTypeDecl *theDecl,
                                   Type parent,
                                   ArrayRef<Type> genericArgs,
                                   const ASTContext *context,
                                   bool hasTypeVariable)
  : TypeBase(theKind, context, hasTypeVariable),
    TheDecl(theDecl), Parent(parent), GenericArgs(genericArgs)
{
}

BoundGenericType *BoundGenericType::get(NominalTypeDecl *TheDecl,
                                        Type Parent,
                                        ArrayRef<Type> GenericArgs) {  
  ASTContext &C = TheDecl->getDeclContext()->getASTContext();
  llvm::FoldingSetNodeID ID;
  bool HasTypeVariable = false;
  BoundGenericType::Profile(ID, TheDecl, Parent, GenericArgs, HasTypeVariable);

  auto arena = getArena(HasTypeVariable);

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
                                                   HasTypeVariable);
  } else if (auto theStruct = dyn_cast<StructDecl>(TheDecl)) {
    newType = new (C, arena) BoundGenericStructType(theStruct, Parent, ArgsCopy,
                                                    IsCanonical ? &C : 0,
                                                    HasTypeVariable);
  } else {
    auto theEnum = cast<EnumDecl>(TheDecl);
    newType = new (C, arena) BoundGenericEnumType(theEnum, Parent, ArgsCopy,
                                                   IsCanonical ? &C : 0,
                                                   HasTypeVariable);
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
                     bool HasTypeVariable)
  : NominalType(TypeKind::Enum, &C, TheDecl, Parent, HasTypeVariable) { }

EnumType *EnumType::get(EnumDecl *D, Type Parent, const ASTContext &C) {
  llvm::FoldingSetNodeID id;
  EnumType::Profile(id, D, Parent);

  bool hasTypeVariable = Parent && Parent->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  void *insertPos = 0;
  if (auto enumTy
        = C.Impl.getArena(arena).EnumTypes.FindNodeOrInsertPos(id, insertPos))
    return enumTy;

  auto enumTy = new (C, arena) EnumType(D, Parent, C, hasTypeVariable);
  C.Impl.getArena(arena).EnumTypes.InsertNode(enumTy, insertPos);
  return enumTy;
}

void EnumType::Profile(llvm::FoldingSetNodeID &ID, EnumDecl *D, Type Parent) {
  ID.AddPointer(D);
  ID.AddPointer(Parent.getPointer());
}

StructType::StructType(StructDecl *TheDecl, Type Parent, const ASTContext &C,
                       bool HasTypeVariable)
  : NominalType(TypeKind::Struct, &C, TheDecl, Parent, HasTypeVariable) { }

StructType *StructType::get(StructDecl *D, Type Parent, const ASTContext &C) {
  llvm::FoldingSetNodeID id;
  StructType::Profile(id, D, Parent);

  bool hasTypeVariable = Parent && Parent->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  void *insertPos = 0;
  if (auto structTy
        = C.Impl.getArena(arena).StructTypes.FindNodeOrInsertPos(id, insertPos))
    return structTy;

  auto structTy = new (C, arena) StructType(D, Parent, C, hasTypeVariable);
  C.Impl.getArena(arena).StructTypes.InsertNode(structTy, insertPos);
  return structTy;
}

void StructType::Profile(llvm::FoldingSetNodeID &ID, StructDecl *D, Type Parent) {
  ID.AddPointer(D);
  ID.AddPointer(Parent.getPointer());
}

ClassType::ClassType(ClassDecl *TheDecl, Type Parent, const ASTContext &C,
                     bool HasTypeVariable)
  : NominalType(TypeKind::Class, &C, TheDecl, Parent, HasTypeVariable) { }

ClassType *ClassType::get(ClassDecl *D, Type Parent, const ASTContext &C) {
  llvm::FoldingSetNodeID id;
  ClassType::Profile(id, D, Parent);

  bool hasTypeVariable = Parent && Parent->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  void *insertPos = 0;
  if (auto classTy
        = C.Impl.getArena(arena).ClassTypes.FindNodeOrInsertPos(id, insertPos))
    return classTy;

  auto classTy = new (C, arena) ClassType(D, Parent, C, hasTypeVariable);
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
  auto arena = AllocationArena::Permanent;

  auto key = uintptr_t(T.getPointer()) | unsigned(ownership);
  auto &entry = C.Impl.getArena(arena).ReferenceStorageTypes[key];
  if (entry) return entry;

  switch (ownership) {
  case Ownership::Strong: llvm_unreachable("not possible");
  case Ownership::Unowned:
    return entry =
      new (C, arena) UnownedStorageType(T, T->isCanonical() ? &C : 0);
  case Ownership::Weak:
    return entry =
      new (C, arena) WeakStorageType(T, T->isCanonical() ? &C : 0);
  }
  llvm_unreachable("bad ownership");
}

MetatypeType *MetatypeType::get(Type T, Optional<bool> IsThin,
                                const ASTContext &C) {
  bool hasTypeVariable = T->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  char thinKey;
  if (IsThin.hasValue())
    thinKey = *IsThin ? 1 : 0;
  else
    thinKey = 2;
  
  MetatypeType *&Entry = C.Impl.getArena(arena)
    .MetatypeTypes[{T, thinKey}];
  if (Entry) return Entry;

  return Entry = new (C, arena) MetatypeType(T, T->isCanonical() ? &C : 0,
                                             hasTypeVariable,
                                             IsThin);
}

MetatypeType::MetatypeType(Type T, const ASTContext *C, bool HasTypeVariable,
                           Optional<bool> IsThin)
  : TypeBase(TypeKind::Metatype, C, HasTypeVariable),
    InstanceType(T) {
  MetatypeTypeBits.HasThin = IsThin.hasValue();
  if (IsThin.hasValue())
    MetatypeTypeBits.Thin = *IsThin;
}

ModuleType *ModuleType::get(Module *M) {
  ASTContext &C = M->getASTContext();
  
  ModuleType *&Entry = C.Impl.ModuleTypes[M];
  if (Entry) return Entry;
  
  return Entry = new (C, AllocationArena::Permanent) ModuleType(M, C);
}

/// FunctionType::get - Return a uniqued function type with the specified
/// input and result.
FunctionType *FunctionType::get(Type Input, Type Result,
                                const ExtInfo &Info) {
  bool hasTypeVariable = Input->hasTypeVariable() || Result->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);
  char attrKey = Info.getFuncAttrKey();

  const ASTContext &C = Input->getASTContext();
  
  FunctionType *&Entry
    = C.Impl.getArena(arena).FunctionTypes[{Input, {Result, attrKey} }];
  if (Entry) return Entry;

  return Entry = new (C, arena) FunctionType(Input, Result,
                                             hasTypeVariable,
                                             Info);
}

// If the input and result types are canonical, then so is the result.
FunctionType::FunctionType(Type input, Type output,
                           bool hasTypeVariable,
                           const ExtInfo &Info)
: AnyFunctionType(TypeKind::Function,
                  (input->isCanonical() && output->isCanonical()) ?
                  &input->getASTContext() : 0,
                  input, output,
                  hasTypeVariable,
                  Info)
{ }


/// FunctionType::get - Return a uniqued function type with the specified
/// input and result.
PolymorphicFunctionType *PolymorphicFunctionType::get(Type input, Type output,
                                                      GenericParamList *params,
                                                      const ExtInfo &Info) {
  // FIXME: one day we should do canonicalization properly.
  bool hasTypeVariable = input->hasTypeVariable() || output->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  const ASTContext &C = input->getASTContext();

  return new (C, arena) PolymorphicFunctionType(input, output, params,
                                                Info, C);
}

PolymorphicFunctionType::PolymorphicFunctionType(Type input, Type output,
                                                 GenericParamList *params,
                                                 const ExtInfo &Info,
                                                 const ASTContext &C)
  : AnyFunctionType(TypeKind::PolymorphicFunction,
                    (input->isCanonical() && output->isCanonical()) ?&C : 0,
                    input, output,
                    /*HasTypeVariable=*/false,
                    Info),
    Params(params)
{
  assert(!input->hasTypeVariable() && !output->hasTypeVariable());
}

void GenericFunctionType::Profile(llvm::FoldingSetNodeID &ID,
                                  ArrayRef<GenericTypeParamType *> params,
                                  ArrayRef<Requirement> requirements,
                                  Type input,
                                  Type result,
                                  const ExtInfo &info) {
  ID.AddInteger(params.size());
  for (auto param : params)
    ID.AddPointer(param);
  ID.AddInteger(requirements.size());
  for (const auto &req : requirements) {
    ID.AddInteger(static_cast<unsigned>(req.getKind()));
    ID.AddPointer(req.getFirstType().getPointer());
    ID.AddPointer(req.getSecondType().getPointer());
  }
  ID.AddPointer(input.getPointer());
  ID.AddPointer(result.getPointer());
  ID.AddInteger(info.getFuncAttrKey());
}

GenericFunctionType *
GenericFunctionType::get(ArrayRef<GenericTypeParamType *> params,
                         ArrayRef<Requirement> requirements,
                         Type input,
                         Type output,
                         const ExtInfo &info) {
  assert(!input->hasTypeVariable() && !output->hasTypeVariable());

  llvm::FoldingSetNodeID id;
  GenericFunctionType::Profile(id, params, requirements, input, output, info);

  const ASTContext &ctx = input->getASTContext();

  // Do we already have this generic function type?
  void *insertPos;
  if (auto result
        = ctx.Impl.GenericFunctionTypes.FindNodeOrInsertPos(id, insertPos))
    return result;

  // We have to construct this generic function type. Determine whether
  // it's canonical.
  bool isCanonical = input->isCanonical() && output->isCanonical();
  if (isCanonical) {
    for (auto param : params) {
      if (!param->isCanonical()) {
        isCanonical = false;
        break;
      }
    }
  }
  if (isCanonical ) {
    for (const auto &req : requirements) {
      if (!req.getFirstType()->isCanonical() ||
          (req.getSecondType() && !req.getSecondType()->isCanonical())) {
        isCanonical = false;
        break;
      }
    }
  }

  // Allocate storage for the object.
  size_t bytes = sizeof(GenericFunctionType)
               + sizeof(GenericTypeParamType *) * params.size()
               + sizeof(Requirement) * requirements.size();
  void *mem = ctx.Allocate(bytes, alignof(GenericFunctionType));

  auto result = new (mem) GenericFunctionType(params, requirements, input,
                                              output, info,
                                              isCanonical? &ctx : nullptr);
  ctx.Impl.GenericFunctionTypes.InsertNode(result, insertPos);
  return result;
}

GenericFunctionType::GenericFunctionType(
                       ArrayRef<GenericTypeParamType *> genericParams,
                       ArrayRef<Requirement> requirements,
                       Type input,
                       Type result,
                       const ExtInfo &info,
                       const ASTContext *ctx)
  : AnyFunctionType(TypeKind::GenericFunction, ctx, input, result,
                    /*hasTypeVariable=*/false, info),
    NumGenericParams(genericParams.size()),
    NumRequirements(requirements.size())
{
  std::copy(genericParams.begin(), genericParams.end(),
            getGenericParamsBuffer().data());
  std::copy(requirements.begin(), requirements.end(),
            getRequirementsBuffer().data());
}

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

void SILFunctionType::Profile(llvm::FoldingSetNodeID &id,
                              GenericParamList *genericParams,
                              ExtInfo info,
                              ParameterConvention calleeConvention,
                              ArrayRef<SILParameterInfo> params,
                              SILResultInfo result) {
  id.AddPointer(genericParams);
  id.AddInteger(info.getFuncAttrKey());
  id.AddInteger(unsigned(calleeConvention));
  id.AddInteger(params.size());
  for (auto param : params)
    param.profile(id);
  result.profile(id);
}

SILFunctionType::SILFunctionType(GenericParamList *genericParams, ExtInfo ext,
                                 ParameterConvention calleeConvention,
                                 ArrayRef<SILParameterInfo> params,
                                 SILResultInfo result,
                                 const ASTContext &ctx)
  : TypeBase(TypeKind::SILFunction, &ctx, /*HasTypeVariable*/ false),
    GenericParams(genericParams),
    // TODO: GenericSig should be given as a constructor parameter.
    Result(result) {
  SILFunctionTypeBits.ExtInfo = ext.Bits;
  SILFunctionTypeBits.NumParameters = params.size();
  assert(!isIndirectParameter(calleeConvention));
  SILFunctionTypeBits.CalleeConvention = unsigned(calleeConvention);
  memcpy(getMutableParameters().data(), params.data(),
         params.size() * sizeof(SILParameterInfo));
  
  // Derive interface types for the result and parameters.
  if (GenericParams) {
    llvm::DenseMap<ArchetypeType *, Type> archetypeMap;
    // TODO: GenericSig and the interface types should be the constructor
    // parameters.
    GenericSig = GenericParams->getAsCanonicalGenericSignature(archetypeMap,
                                                               getASTContext());
    
    /* FIXME: We place the wrong generic parameter list on some SILFunctionTypes
       causing things to explode when we try to map out the archetypes.
       Disable until those problems are fixed.
     
    auto getArchetypesAsDependentTypes = [&](Type t) -> Type {
      if (!t) return t;
      if (auto arch = t->getAs<ArchetypeType>()) {
        return arch->getAsDependentType(archetypeMap);
      }
      return t;
    };
    
    for (unsigned i = 0; i < params.size(); ++i) {
      auto &interfaceParam = getMutableInterfaceParameters()[i];
      const auto &param = getParameters()[i];
      
      auto interfaceParamTy = param.getType()
        .transform(getArchetypesAsDependentTypes)
        ->getCanonicalType();
      interfaceParam = SILParameterInfo(interfaceParamTy,
                                        param.getConvention());
    }
    
    auto interfaceResultTy = Result.getType()
      .transform(getArchetypesAsDependentTypes)
      ->getCanonicalType();
    InterfaceResult = SILResultInfo(interfaceResultTy,
                                    Result.getConvention());
     */
    InterfaceResult = Result;
    memcpy(getMutableInterfaceParameters().data(),
           getMutableParameters().data(),
           params.size() * sizeof(SILParameterInfo));
  } else {
    // If not generic, the interface types are equivalent.
    GenericSig = nullptr;
    InterfaceResult = Result;
    memcpy(getMutableInterfaceParameters().data(),
           getMutableParameters().data(),
           params.size() * sizeof(SILParameterInfo));
  }
}


CanSILFunctionType SILFunctionType::get(GenericParamList *genericParams,
                                        ExtInfo ext, ParameterConvention callee,
                                        ArrayRef<SILParameterInfo> params,
                                        SILResultInfo result,
                                        const ASTContext &ctx) {
  llvm::FoldingSetNodeID id;
  SILFunctionType::Profile(id, genericParams, ext, callee, params, result);

  // Do we already have this generic function type?
  void *insertPos;
  if (auto result
        = ctx.Impl.SILFunctionTypes.FindNodeOrInsertPos(id, insertPos))
    return CanSILFunctionType(result);

  // All SILFunctionTypes are canonical.

  // Allocate storage for the object.
  // FIXME: 2*params.size() so we can stash interface types.
  size_t bytes = sizeof(SILFunctionType)
               + sizeof(SILParameterInfo) * 2 * params.size();
  void *mem = ctx.Allocate(bytes, alignof(SILFunctionType));

  auto fnType =
    new (mem) SILFunctionType(genericParams, ext, callee, params, result, ctx);
  ctx.Impl.SILFunctionTypes.InsertNode(fnType, insertPos);
  return CanSILFunctionType(fnType);
}

/// Return a uniqued array type with the specified base type and the
/// specified size.
ArrayType *ArrayType::get(Type BaseType, uint64_t Size) {
  assert(Size != 0);

  bool hasTypeVariable = BaseType->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  const ASTContext &C = BaseType->getASTContext();
  
  ArrayType *&Entry
    = C.Impl.getArena(arena).ArrayTypes[std::make_pair(BaseType, Size)];
  if (Entry) return Entry;

  return Entry = new (C, arena) ArrayType(BaseType, Size, hasTypeVariable);
}

ArrayType::ArrayType(Type base, uint64_t size, bool hasTypeVariable)
  : TypeBase(TypeKind::Array, 
             base->isCanonical() ? &base->getASTContext() : 0,
             hasTypeVariable),
    Base(base), Size(size) {}


ArraySliceType *ArraySliceType::get(Type base) {
  bool hasTypeVariable = base->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  const ASTContext &C = base->getASTContext();

  ArraySliceType *&entry = C.Impl.getArena(arena).ArraySliceTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) ArraySliceType(C, base, hasTypeVariable);
}

OptionalType *OptionalType::get(Type base) {
  bool hasTypeVariable = base->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  const ASTContext &C = base->getASTContext();

  OptionalType *&entry = C.Impl.getArena(arena).OptionalTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) OptionalType(C, base, hasTypeVariable);
}

ProtocolType *ProtocolType::get(ProtocolDecl *D, const ASTContext &C) {
  if (auto declaredTy = D->getDeclaredType())
    return declaredTy->castTo<ProtocolType>();

  auto protoTy = new (C, AllocationArena::Permanent) ProtocolType(D, C);
  D->setDeclaredType(protoTy);
  return protoTy;
}

ProtocolType::ProtocolType(ProtocolDecl *TheDecl, const ASTContext &Ctx)
  : NominalType(TypeKind::Protocol, &Ctx, TheDecl, /*Parent=*/Type(),
                /*HasTypeVariable=*/false) { }

LValueType *LValueType::get(Type objectTy) {
  assert(!objectTy->is<ErrorType>() &&
         "can not have ErrorType wrapped inside LValueType");
//  assert(!objectTy->is<LValueType>() && !objectTy->is<InOutType>() &&
//         "can not have @inout or @lvalue wrapped inside an @lvalue");

  bool hasTypeVariable = objectTy->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  auto &C = objectTy->getASTContext();
  auto &entry = C.Impl.getArena(arena).LValueTypes[objectTy];
  if (entry)
    return entry;

  const ASTContext *canonicalContext = objectTy->isCanonical() ? &C : nullptr;
  return entry = new (C, arena) LValueType(objectTy, canonicalContext,
                                           hasTypeVariable);
}

InOutType *InOutType::get(Type objectTy) {
  assert(!objectTy->is<ErrorType>() &&
         "can not have ErrorType wrapped inside InOutType");
//  assert(!objectTy->is<LValueType>() && !objectTy->is<InOutType>() &&
//         "can not have @inout or @lvalue wrapped inside an @inout");
  
  bool hasTypeVariable = objectTy->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);
  
  auto &C = objectTy->getASTContext();
  auto &entry = C.Impl.getArena(arena).InOutTypes[objectTy];
  if (entry)
    return entry;
  
  const ASTContext *canonicalContext = objectTy->isCanonical() ? &C : nullptr;
  return entry = new (C, arena) InOutType(objectTy, canonicalContext,
                                           hasTypeVariable);
}

/// Return a uniqued substituted type.
SubstitutedType *SubstitutedType::get(Type Original, Type Replacement,
                                      const ASTContext &C) {
  bool hasTypeVariable = Replacement->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  SubstitutedType *&Known
    = C.Impl.getArena(arena).SubstitutedTypes[{Original, Replacement}];
  if (!Known) {
    Known = new (C, arena) SubstitutedType(Original, Replacement,
                                           hasTypeVariable);
  }
  return Known;
}

DependentMemberType *DependentMemberType::get(Type base, Identifier name,
                                              const ASTContext &ctx) {
  bool hasTypeVariable = base->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  llvm::PointerUnion<Identifier, AssociatedTypeDecl *> stored(name);
  auto *&known = ctx.Impl.getArena(arena).DependentMemberTypes[
                                            {base, stored.getOpaqueValue()}];
  if (!known) {
    const ASTContext *canonicalCtx = base->isCanonical() ? &ctx : nullptr;
    known = new (ctx, arena) DependentMemberType(base, name, canonicalCtx,
                                                 hasTypeVariable);
  }
  return known;
}

DependentMemberType *DependentMemberType::get(Type base,
                                              AssociatedTypeDecl *assocType,
                                              const ASTContext &ctx) {
  bool hasTypeVariable = base->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  llvm::PointerUnion<Identifier, AssociatedTypeDecl *> stored(assocType);
  auto *&known = ctx.Impl.getArena(arena).DependentMemberTypes[
                                            {base, stored.getOpaqueValue()}];
  if (!known) {
    const ASTContext *canonicalCtx = base->isCanonical() ? &ctx : nullptr;
    known = new (ctx, arena) DependentMemberType(base, assocType, canonicalCtx,
                                                 hasTypeVariable);
  }
  return known;
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

