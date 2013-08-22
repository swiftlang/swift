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
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/ModuleLoadListener.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/Allocator.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include <memory>

using namespace swift;

ASTMutationListener::~ASTMutationListener() { }

ModuleLoadListener::~ModuleLoadListener() {}

struct ASTContext::Implementation {
  Implementation();
  ~Implementation();

  llvm::BumpPtrAllocator Allocator; // used in later initializations

  /// The set of cleanups to be called when the ASTContext is destroyed.
  std::vector<std::function<void(void)>> Cleanups;

  llvm::StringMap<char, llvm::BumpPtrAllocator&> IdentifierTable;

  /// \brief The various module loaders that import external modules into this
  /// ASTContext.
  SmallVector<llvm::IntrusiveRefCntPtr<swift::ModuleLoader>, 4> ModuleLoaders;

  /// \brief The module loader used to load Clang modules.
  // FIXME: We shouldn't be special-casing Clang.
  llvm::IntrusiveRefCntPtr<swift::ModuleLoader> ClangModuleLoader;

  /// \brief The set of AST mutation listeners.
  SmallVector<ASTMutationListener *, 4> MutationListeners;

  /// \brief The set of 'module load' event listeners.
  SmallVector<ModuleLoadListener *, 2> ModuleLoadListeners;

  /// \brief Map from Swift declarations to the Clang nodes from which
  /// they were imported.
  llvm::DenseMap<swift::Decl *, ClangNode> ClangNodes;

  /// \brief Structure that captures data that is segregated into different
  /// arenas.
  struct Arena {
    llvm::FoldingSet<TupleType> TupleTypes;
    llvm::DenseMap<Type, MetaTypeType*> MetaTypeTypes;
    llvm::DenseMap<std::pair<Type,std::pair<Type,char>>, FunctionType*>
      FunctionTypes;
    llvm::DenseMap<std::pair<Type, uint64_t>, ArrayType*> ArrayTypes;
    llvm::DenseMap<Type, ArraySliceType*> ArraySliceTypes;
    llvm::DenseMap<Type, OptionalType*> OptionalTypes;
    llvm::DenseMap<Type, ParenType*> ParenTypes;
    llvm::DenseMap<uintptr_t, ReferenceStorageType*> ReferenceStorageTypes;
    llvm::DenseMap<std::pair<Type, LValueType::Qual::opaque_type>, LValueType*>
      LValueTypes;
    llvm::DenseMap<std::pair<Type, Type>, SubstitutedType *> SubstitutedTypes;
    llvm::FoldingSet<UnionType> UnionTypes;
    llvm::FoldingSet<StructType> StructTypes;
    llvm::FoldingSet<ClassType> ClassTypes;
    llvm::FoldingSet<UnboundGenericType> UnboundGenericTypes;
    llvm::FoldingSet<BoundGenericType> BoundGenericTypes;
  };
  
  llvm::DenseMap<Module*, ModuleType*> ModuleTypes;
  llvm::DenseMap<unsigned, BuiltinIntegerType*> IntegerTypes;
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

ASTContext::ASTContext(LangOptions &langOpts, SourceManager &SourceMgr,
                       DiagnosticEngine &Diags)
  : Impl(*new Implementation()),
    LangOpts(langOpts),
    SourceMgr(SourceMgr),
    Diags(Diags),
    TheBuiltinModule(new (*this) BuiltinModule(getIdentifier("Builtin"), *this)),
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
                    BuiltinFloatType(BuiltinFloatType::PPC128,*this)){
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
Identifier ASTContext::getIdentifier(StringRef Str) {
  // Make sure null pointers stay null.
  if (Str.data() == nullptr) return Identifier(0);
  
  return Identifier(Impl.IdentifierTable.GetOrCreateValue(Str).getKeyData());
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

void ASTContext::addedExternalType(Type type) {
  for (auto listener : Impl.MutationListeners)
    listener->addedExternalType(type);
}

void ASTContext::addCleanup(std::function<void(void)> cleanup) {
  Impl.Cleanups.push_back(std::move(cleanup));
}

bool ASTContext::hadError() const {
  return Diags.hadAnyError();
}

Optional<ArrayRef<Substitution>>
ASTContext::getSubstitutions(BoundGenericType* Bound) const {
  assert(Bound->isCanonical() && "Requesting non-canonical substitutions");
  auto Known = Impl.BoundGenericSubstitutions.find(Bound);
  if (Known == Impl.BoundGenericSubstitutions.end())
    return Nothing;

  return Known->second;
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

Module *
ASTContext::getModule(ArrayRef<std::pair<Identifier, SourceLoc>> modulePath) {
  assert(!modulePath.empty());
  auto moduleID = modulePath[0];

  // TODO: Swift submodules.
  if (modulePath.size() == 1) {
    if (Module *M = LoadedModules.lookup(moduleID.first.str()))
      return M;
  }

  for (auto importer : Impl.ModuleLoaders) {
    if (Module *M = importer->loadModule(moduleID.second, modulePath))
      return M;
  }

  return nullptr;
}

void ASTContext::addModuleLoadListener(ModuleLoadListener &Listener) {
  Impl.ModuleLoadListeners.push_back(&Listener);
}

void ASTContext::removeModuleLoadListener(ModuleLoadListener &Listener) {
  auto Known = std::find(Impl.ModuleLoadListeners.rbegin(),
                         Impl.ModuleLoadListeners.rend(),
                         &Listener);
  assert(Known != Impl.ModuleLoadListeners.rend() && "listener not registered");
  Impl.ModuleLoadListeners.erase(Known.base() - 1);
}

ClangNode ASTContext::getClangNode(Decl *decl) {
  auto known = Impl.ClangNodes.find(decl);
  assert(known != Impl.ClangNodes.end() && "No Clang node?");
  return known->second;
}

void ASTContext::setClangNode(Decl *decl, ClangNode node) {
  Impl.ClangNodes[decl] = node;
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
                           Module *containingModule,
                           WitnessMap &&witnesses,
                           TypeWitnessMap &&typeWitnesses,
                           InheritedConformanceMap &&inheritedConformances,
                           ArrayRef<ValueDecl *> defaultedDefinitions) {
  auto result
    = new (*this) NormalProtocolConformance(conformingType, protocol,
                                            containingModule,
                                            std::move(witnesses),
                                            std::move(typeWitnesses),
                                            std::move(inheritedConformances),
                                            defaultedDefinitions);
  Impl.NormalConformances.push_back(result);
  return result;
}

SpecializedProtocolConformance *
ASTContext::getSpecializedConformance(Type type,
                                      ProtocolConformance *generic,
                                      ArrayRef<Substitution> substitutions,
                                      TypeWitnessMap &&typeWitnesses) {
  llvm::FoldingSetNodeID id;
  SpecializedProtocolConformance::Profile(id, type, generic);

  // Did we already record the specialized conformance?
  void *insertPos;
  if (auto result
        = Impl.SpecializedConformances.FindNodeOrInsertPos(id, insertPos))
    return result;

  // Build a new specialized conformance.
  auto result
    = new (*this) SpecializedProtocolConformance(type, generic, substitutions,
                                                 std::move(typeWitnesses));
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

BuiltinIntegerType *BuiltinIntegerType::get(unsigned BitWidth,
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

Type TupleType::getEmpty(const ASTContext &C) { return C.TheEmptyTupleType; }

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
    auto theUnion = cast<UnionDecl>(TheDecl);
    newType = new (C, arena) BoundGenericUnionType(theUnion, Parent, ArgsCopy,
                                                   IsCanonical ? &C : 0,
                                                   HasTypeVariable);
  }
  C.Impl.getArena(arena).BoundGenericTypes.InsertNode(newType, InsertPos);

  return newType;
}

NominalType *NominalType::get(NominalTypeDecl *D, Type Parent, const ASTContext &C) {
  switch (D->getKind()) {
  case DeclKind::Union:
    return UnionType::get(cast<UnionDecl>(D), Parent, C);
  case DeclKind::Struct:
    return StructType::get(cast<StructDecl>(D), Parent, C);
  case DeclKind::Class:
    return ClassType::get(cast<ClassDecl>(D), Parent, C);
  case DeclKind::Protocol:
    assert(!Parent && "Protocols cannot have parents");
    return ProtocolType::get(cast<ProtocolDecl>(D), C);

  default:
    llvm_unreachable("Not a nominal declaration!");
  }
}

UnionType::UnionType(UnionDecl *TheDecl, Type Parent, const ASTContext &C,
                     bool HasTypeVariable)
  : NominalType(TypeKind::Union, &C, TheDecl, Parent, HasTypeVariable) { }

UnionType *UnionType::get(UnionDecl *D, Type Parent, const ASTContext &C) {
  llvm::FoldingSetNodeID id;
  UnionType::Profile(id, D, Parent);

  bool hasTypeVariable = Parent && Parent->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  void *insertPos = 0;
  if (auto unionTy
        = C.Impl.getArena(arena).UnionTypes.FindNodeOrInsertPos(id, insertPos))
    return unionTy;

  auto unionTy = new (C, arena) UnionType(D, Parent, C, hasTypeVariable);
  C.Impl.getArena(arena).UnionTypes.InsertNode(unionTy, insertPos);
  return unionTy;
}

void UnionType::Profile(llvm::FoldingSetNodeID &ID, UnionDecl *D, Type Parent) {
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

  return entry = new (C, arena) ReferenceStorageType(T, ownership,
                                                     T->isCanonical() ? &C : 0);
}

MetaTypeType *MetaTypeType::get(Type T, const ASTContext &C) {
  bool hasTypeVariable = T->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  MetaTypeType *&Entry = C.Impl.getArena(arena).MetaTypeTypes[T];
  if (Entry) return Entry;

  return Entry = new (C, arena) MetaTypeType(T, T->isCanonical() ? &C : 0,
                                             hasTypeVariable);
}

MetaTypeType::MetaTypeType(Type T, const ASTContext *C, bool HasTypeVariable)
  : TypeBase(TypeKind::MetaType, C, HasTypeVariable),
    InstanceType(T) {
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
                                const ExtInfo &Info,
                                const ASTContext &C) {
  bool hasTypeVariable = Input->hasTypeVariable() || Result->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);
  char attrKey = Info.getFuncAttrKey();

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
                                                      const ExtInfo &Info,
                                                      const ASTContext &C) {
  // FIXME: one day we should do canonicalization properly.
  bool hasTypeVariable = input->hasTypeVariable() || output->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

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

/// Return a uniqued array type with the specified base type and the
/// specified size.
ArrayType *ArrayType::get(Type BaseType, uint64_t Size, const ASTContext &C) {
  assert(Size != 0);

  bool hasTypeVariable = BaseType->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

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


ArraySliceType *ArraySliceType::get(Type base, const ASTContext &C) {
  bool hasTypeVariable = base->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  ArraySliceType *&entry = C.Impl.getArena(arena).ArraySliceTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) ArraySliceType(base, hasTypeVariable);
}

OptionalType *OptionalType::get(Type base, const ASTContext &C) {
  bool hasTypeVariable = base->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  OptionalType *&entry = C.Impl.getArena(arena).OptionalTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) OptionalType(base, hasTypeVariable);
}

ProtocolType *ProtocolType::get(ProtocolDecl *D, const ASTContext &C) {
  // If the declaration already has a type, return that.
  if (D->hasType())
    return D->getDeclaredType()->castTo<ProtocolType>();

  return new (C, AllocationArena::Permanent) ProtocolType(D, C);
}

ProtocolType::ProtocolType(ProtocolDecl *TheDecl, const ASTContext &Ctx)
  : NominalType(TypeKind::Protocol, &Ctx, TheDecl, /*Parent=*/Type(),
                /*HasTypeVariable=*/false) { }

LValueType *LValueType::get(Type objectTy, Qual quals, const ASTContext &C) {
  bool hasTypeVariable = objectTy->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  auto key = std::make_pair(objectTy, quals.getOpaqueData());
  auto &entry = C.Impl.getArena(arena).LValueTypes[key];
  if (entry)
    return entry;

  const ASTContext *canonicalContext = objectTy->isCanonical() ? &C : nullptr;
  return entry = new (C, arena) LValueType(objectTy, quals, canonicalContext,
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
