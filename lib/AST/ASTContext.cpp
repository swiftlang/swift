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
#include "swift/AST/ModuleLoader.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include <memory>

using namespace swift;

ASTMutationListener::~ASTMutationListener() { }

struct ASTContext::Implementation {
  Implementation();
  ~Implementation();

  llvm::BumpPtrAllocator Allocator; // used in later initializations
  llvm::StringMap<char, llvm::BumpPtrAllocator&> IdentifierTable;

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
    llvm::DenseMap<Type, ParenType*> ParenTypes;
    llvm::DenseMap<std::pair<Type, LValueType::Qual::opaque_type>, LValueType*>
      LValueTypes;
    llvm::DenseMap<std::pair<Type, Type>, SubstitutedType *> SubstitutedTypes;
    llvm::FoldingSet<OneOfType> OneOfTypes;
    llvm::FoldingSet<StructType> StructTypes;
    llvm::FoldingSet<ClassType> ClassTypes;
    llvm::FoldingSet<UnboundGenericType> UnboundGenericTypes;
    llvm::FoldingSet<BoundGenericType> BoundGenericTypes;
  };
  
  llvm::DenseMap<Module*, ModuleType*> ModuleTypes;
  llvm::DenseMap<unsigned, BuiltinIntegerType*> IntegerTypes;
  llvm::FoldingSet<ProtocolCompositionType> ProtocolCompositionTypes;

  /// \brief The permanent arena.
  Arena Permanent;

    /// FIXME: Move into arena.
  llvm::DenseMap<BoundGenericType *, ArrayRef<Substitution>>
    BoundGenericSubstitutions;

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
ASTContext::Implementation::~Implementation() {}


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

ASTContext::ASTContext(LangOptions &langOpts, llvm::SourceMgr &sourcemgr,
                       DiagnosticEngine &Diags)
  : Impl(*new Implementation()),
    LangOpts(langOpts),
    SourceMgr(sourcemgr),
    Diags(Diags),
    TheBuiltinModule(new (*this) BuiltinModule(getIdentifier("Builtin"),*this)),
    TheErrorType(new (*this, AllocationArena::Permanent) ErrorType(*this)),
    TheEmptyTupleType(TupleType::get(ArrayRef<TupleTypeElt>(), *this)),
    TheObjectPointerType(new (*this, AllocationArena::Permanent)
                           BuiltinObjectPointerType(*this)),
    TheObjCPointerType(new (*this, AllocationArena::Permanent)
                         BuiltinObjCPointerType(*this)),
    TheRawPointerType(new (*this, AllocationArena::Permanent)
                        BuiltinRawPointerType(*this)),
    TheOpaquePointerType(new (*this, AllocationArena::Permanent)
                         BuiltinOpaquePointerType(*this)),
    TheUnstructuredUnresolvedType(new (*this, AllocationArena::Permanent)
                                    UnstructuredUnresolvedType(*this)),
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
  delete &Impl;

  for (auto &entry : ConformsTo)
    delete const_cast<ProtocolConformance*>(entry.second);
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
  if (Str.empty()) return Identifier(0);
  
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

bool ASTContext::hadError() const {
  return Diags.hadAnyError();
}

Optional<ArrayRef<Substitution>>
ASTContext::getSubstitutions(BoundGenericType* Bound) {
  assert(Bound->isCanonical() && "Requesting non-canonical substitutions");
  auto Known = Impl.BoundGenericSubstitutions.find(Bound);
  if (Known == Impl.BoundGenericSubstitutions.end())
    return Nothing;

  return Known->second;
}

void ASTContext::setSubstitutions(BoundGenericType* Bound,
                                  ArrayRef<Substitution> Subs) {
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

ClangNode ASTContext::getClangNode(Decl *decl) {
  auto known = Impl.ClangNodes.find(decl);
  assert(known != Impl.ClangNodes.end() && "No Clang node?");
  return known->second;
}

void ASTContext::setClangNode(Decl *decl, ClangNode node) {
  Impl.ClangNodes[decl] = node;
}

//===----------------------------------------------------------------------===//
// Type manipulation routines.
//===----------------------------------------------------------------------===//

// Simple accessors.
Type ErrorType::get(ASTContext &C) { return C.TheErrorType; }
Type UnstructuredUnresolvedType::get(ASTContext &C) { 
  return C.TheUnstructuredUnresolvedType; 
}


BuiltinIntegerType *BuiltinIntegerType::get(unsigned BitWidth, ASTContext &C) {
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

ParenType *ParenType::get(ASTContext &C, Type underlying) {
  bool hasTypeVariable = underlying->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);
  ParenType *&Result = C.Impl.getArena(arena).ParenTypes[underlying];
  if (Result == 0) {
    Result = new (C, arena) ParenType(underlying, hasTypeVariable);
  }
  return Result;
}

Type TupleType::getEmpty(ASTContext &C) { return C.TheEmptyTupleType; }

void TupleType::Profile(llvm::FoldingSetNodeID &ID,
                        ArrayRef<TupleTypeElt> Fields) {
  ID.AddInteger(Fields.size());
  for (const TupleTypeElt &Elt : Fields) {
    ID.AddPointer(Elt.getType().getPointer());
    ID.AddPointer(Elt.getName().get());
    ID.AddPointer(Elt.getInit());
    ID.AddPointer(Elt.getVarargBaseTy().getPointer());
  }
}

/// getTupleType - Return the uniqued tuple type with the specified elements.
Type TupleType::get(ArrayRef<TupleTypeElt> Fields, ASTContext &C) {
  if (Fields.size() == 1 && !Fields[0].isVararg() && !Fields[0].hasName())
    return ParenType::get(C, Fields[0].getType());

  bool HasAnyDefaultValues = false;
  bool HasTypeVariable = false;

  for (const TupleTypeElt &Elt : Fields) {
    if (Elt.hasInit()) {
      HasAnyDefaultValues = true;
      if (HasTypeVariable)
        break;
    }
    if (Elt.getType() && Elt.getType()->hasTypeVariable()) {
      HasTypeVariable = true;
      if (HasAnyDefaultValues)
        break;
    }
  }

  auto arena = getArena(HasTypeVariable);


  void *InsertPos = 0;
  if (!HasAnyDefaultValues) {
    // Check to see if we've already seen this tuple before.
    llvm::FoldingSetNodeID ID;
    TupleType::Profile(ID, Fields);

    if (TupleType *TT
          = C.Impl.getArena(arena).TupleTypes.FindNodeOrInsertPos(ID,InsertPos))
      return TT;
  }

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
  if (!HasAnyDefaultValues)
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
                                            ASTContext &C) {
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
                                   ASTContext *context,
                                   bool hasTypeVariable)
  : TypeBase(theKind, context, /*Unresolved=*/false,
             hasTypeVariable),
    TheDecl(theDecl), Parent(parent), GenericArgs(genericArgs)
{
  // Determine whether this type is unresolved.
  if (parent && parent->isUnresolvedType())
    setUnresolved();
  else for (Type arg : genericArgs) {
    if (arg->isUnresolvedType()) {
      setUnresolved();
      break;
    }
  }
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
    auto theOneOf = cast<OneOfDecl>(TheDecl);
    newType = new (C, arena) BoundGenericOneOfType(theOneOf, Parent, ArgsCopy,
                                                   IsCanonical ? &C : 0,
                                                   HasTypeVariable);
  }
  C.Impl.getArena(arena).BoundGenericTypes.InsertNode(newType, InsertPos);

  return newType;
}

NominalType *NominalType::get(NominalTypeDecl *D, Type Parent, ASTContext &C) {
  switch (D->getKind()) {
  case DeclKind::OneOf:
    return OneOfType::get(cast<OneOfDecl>(D), Parent, C);
  case DeclKind::Struct:
    return StructType::get(cast<StructDecl>(D), Parent, C);
  case DeclKind::Class:
    return ClassType::get(cast<ClassDecl>(D), Parent, C);
  case DeclKind::Protocol:
    return D->getDeclaredType()->castTo<ProtocolType>();

  default:
    llvm_unreachable("Not a nominal declaration!");
  }
}

OneOfType::OneOfType(OneOfDecl *TheDecl, Type Parent, ASTContext &C,
                     bool HasTypeVariable)
  : NominalType(TypeKind::OneOf, &C, TheDecl, Parent, HasTypeVariable) { }

OneOfType *OneOfType::get(OneOfDecl *D, Type Parent, ASTContext &C) {
  llvm::FoldingSetNodeID id;
  OneOfType::Profile(id, D, Parent);

  bool hasTypeVariable = Parent && Parent->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  void *insertPos = 0;
  if (auto oneOfTy
        = C.Impl.getArena(arena).OneOfTypes.FindNodeOrInsertPos(id, insertPos))
    return oneOfTy;

  auto oneOfTy = new (C, arena) OneOfType(D, Parent, C, hasTypeVariable);
  C.Impl.getArena(arena).OneOfTypes.InsertNode(oneOfTy, insertPos);
  return oneOfTy;
}

void OneOfType::Profile(llvm::FoldingSetNodeID &ID, OneOfDecl *D, Type Parent) {
  ID.AddPointer(D);
  ID.AddPointer(Parent.getPointer());
}

StructType::StructType(StructDecl *TheDecl, Type Parent, ASTContext &C,
                       bool HasTypeVariable)
  : NominalType(TypeKind::Struct, &C, TheDecl, Parent, HasTypeVariable) { }

StructType *StructType::get(StructDecl *D, Type Parent, ASTContext &C) {
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

ClassType::ClassType(ClassDecl *TheDecl, Type Parent, ASTContext &C,
                     bool HasTypeVariable)
  : NominalType(TypeKind::Class, &C, TheDecl, Parent, HasTypeVariable) { }

ClassType *ClassType::get(ClassDecl *D, Type Parent, ASTContext &C) {
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

IdentifierType *IdentifierType::getNew(ASTContext &C,
                                       MutableArrayRef<Component> Components) {
  Components = C.AllocateCopy(Components);
  return new (C, AllocationArena::Permanent) IdentifierType(Components);
}

ProtocolCompositionType *
ProtocolCompositionType::build(ASTContext &C, ArrayRef<Type> Protocols) {
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


MetaTypeType *MetaTypeType::get(Type T, ASTContext &C) {
  bool hasTypeVariable = T->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  MetaTypeType *&Entry = C.Impl.getArena(arena).MetaTypeTypes[T];
  if (Entry) return Entry;

  return Entry = new (C, arena) MetaTypeType(T, T->isCanonical() ? &C : 0,
                                             hasTypeVariable);
}

MetaTypeType::MetaTypeType(Type T, ASTContext *C, bool HasTypeVariable)
  : TypeBase(TypeKind::MetaType, C, T->isUnresolvedType(), HasTypeVariable),
    InstanceType(T) {
}

ModuleType *ModuleType::get(Module *M) {
  ASTContext &C = M->getASTContext();
  
  ModuleType *&Entry = C.Impl.ModuleTypes[M];
  if (Entry) return Entry;
  
  return Entry = new (C, AllocationArena::Permanent) ModuleType(M, C);
}

static char getFuncAttrKey(bool isAutoClosure, bool isBlock, bool isThin,
                           AbstractCC cc) {
  return isAutoClosure | (isBlock << 1) | (isThin << 2)
       | (unsigned(cc) << 3);
}

/// FunctionType::get - Return a uniqued function type with the specified
/// input and result.
FunctionType *FunctionType::get(Type Input, Type Result,
                                bool isAutoClosure, bool isBlock, bool isThin,
                                AbstractCC cc,
                                ASTContext &C) {
  bool hasTypeVariable = Input->hasTypeVariable() || Result->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);
  char attrKey = getFuncAttrKey(isAutoClosure, isBlock, isThin, cc);

  FunctionType *&Entry
    = C.Impl.getArena(arena).FunctionTypes[{Input, {Result, attrKey} }];
  if (Entry) return Entry;

  return Entry = new (C, arena) FunctionType(Input, Result,
                                             isAutoClosure,
                                             isBlock,
                                             hasTypeVariable,
                                             isThin,
                                             cc);
}

// If the input and result types are canonical, then so is the result.
FunctionType::FunctionType(Type input, Type output,
                           bool isAutoClosure, bool isBlock,
                           bool hasTypeVariable, bool isThin,
                           AbstractCC cc)
  : AnyFunctionType(TypeKind::Function,
             (input->isCanonical() && output->isCanonical()) ?
               &input->getASTContext() : 0,
             input, output,
             (input->isUnresolvedType() || output->isUnresolvedType()),
             hasTypeVariable,
             isThin,
             cc),
    AutoClosure(isAutoClosure),
    Block(isBlock)
{ }


/// FunctionType::get - Return a uniqued function type with the specified
/// input and result.
PolymorphicFunctionType *PolymorphicFunctionType::get(Type input, Type output,
                                                      GenericParamList *params,
                                                      bool isThin,
                                                      AbstractCC cc,
                                                      ASTContext &C) {
  // FIXME: one day we should do canonicalization properly.
  bool hasTypeVariable = input->hasTypeVariable() || output->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  return new (C, arena) PolymorphicFunctionType(input, output, params,
                                                isThin, cc, C);
}

PolymorphicFunctionType::PolymorphicFunctionType(Type input, Type output,
                                                 GenericParamList *params,
                                                 bool isThin,
                                                 AbstractCC cc,
                                                 ASTContext &C)
  : AnyFunctionType(TypeKind::PolymorphicFunction,
                    (input->isCanonical() && output->isCanonical()) ?&C : 0,
                    input, output,
                    (input->isUnresolvedType() || output->isUnresolvedType()),
                    /*HasTypeVariable=*/false,
                    isThin, cc),
    Params(params)
{
  assert(!input->hasTypeVariable() && !output->hasTypeVariable());
}

/// Return a uniqued array type with the specified base type and the
/// specified size.
ArrayType *ArrayType::get(Type BaseType, uint64_t Size, ASTContext &C) {
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
             base->isUnresolvedType(), hasTypeVariable),
    Base(base), Size(size) {}


/// Return a uniqued array slice type with the specified base type.
ArraySliceType *ArraySliceType::get(Type base, ASTContext &C) {
  bool hasTypeVariable = base->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  ArraySliceType *&entry = C.Impl.getArena(arena).ArraySliceTypes[base];
  if (entry) return entry;

  return entry = new (C, arena) ArraySliceType(base, hasTypeVariable);
}

ProtocolType::ProtocolType(ProtocolDecl *TheDecl, ASTContext &Ctx)
  : NominalType(TypeKind::Protocol, &Ctx, TheDecl, /*Parent=*/Type(),
                /*HasTypeVariable=*/false) { }

LValueType *LValueType::get(Type objectTy, Qual quals, ASTContext &C) {
  bool hasTypeVariable = objectTy->hasTypeVariable();
  auto arena = getArena(hasTypeVariable);

  auto key = std::make_pair(objectTy, quals.getOpaqueData());
  auto &entry = C.Impl.getArena(arena).LValueTypes[key];
  if (entry)
    return entry;

  ASTContext *canonicalContext = (objectTy->isCanonical() ? &C : nullptr);
  return entry = new (C, arena) LValueType(objectTy, quals, canonicalContext,
                                           hasTypeVariable);
}

/// Return a uniqued substituted type.
SubstitutedType *SubstitutedType::get(Type Original, Type Replacement,
                                      ASTContext &C) {
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
  T = ErrorType::get(C);
}
