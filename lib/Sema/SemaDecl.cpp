//===--- SemaDecl.cpp - Swift Semantic Analysis for Declarations ----------===//
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
//  This file implements semantic analysis for Swift declarations.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/SemaDecl.h"
#include "swift/Sema/Sema.h"
#include "swift/Sema/Scope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;

typedef std::pair<unsigned, ValueDecl*> ValueScopeEntry;
typedef llvm::ScopedHashTable<Identifier, ValueScopeEntry> ValueScopeHTType;

typedef std::pair<unsigned, TypeAliasDecl*> TypeScopeEntry;
typedef llvm::ScopedHashTable<Identifier, TypeScopeEntry> TypeScopeHTType;

static ValueScopeHTType &getValueHT(void *P) {
  return *(ValueScopeHTType*)P;
}
static TypeScopeHTType &getTypeHT(void *P) {
  return *(TypeScopeHTType*)P;
}

typedef llvm::DenseMap<Identifier,TypeAliasDecl*> UnresolvedTypesMapTy;
static UnresolvedTypesMapTy &getUnresolvedTypesHT(void *P){
  return *(UnresolvedTypesMapTy*)P;
}

SemaDecl::SemaDecl(Sema &S)
  : SemaBase(S),
    ValueScopeHT(new ValueScopeHTType()),
    TypeScopeHT(new TypeScopeHTType()),
    CurScope(0),
    UnresolvedTypes(new UnresolvedTypesMapTy()){
}

SemaDecl::~SemaDecl() {
  delete &getValueHT(ValueScopeHT);
  delete &getTypeHT(TypeScopeHT);
  delete &getUnresolvedTypesHT(UnresolvedTypes);
}


/// handleEndOfTranslationUnit - This is invoked at the end of the translation
/// unit.
void SemaDecl::handleEndOfTranslationUnit() {
  // Verify that any forward declared types were ultimately defined.
  UnresolvedTypesMapTy &UT = getUnresolvedTypesHT(UnresolvedTypes);
  // FIXME: Nondeterminstic iteration.
  for (UnresolvedTypesMapTy::iterator I = UT.begin(), E = UT.end(); I != E; ++I)
    Error(I->second->getLocStart(), "use of undeclared type '" + I->first.str()
          + "'");
}

//===----------------------------------------------------------------------===//
// Name lookup.
//===----------------------------------------------------------------------===//

/// LookupValueName - Perform a lexical scope lookup for the specified name,
/// returning the active decl if found or null if not.
ValueDecl *SemaDecl::LookupValueName(Identifier Name) {
  return getValueHT(ValueScopeHT).lookup(Name).second;
}

/// LookupTypeName - Perform a lexical scope lookup for the specified name in
/// a type context, returning the decl if found or installing and returning a
/// new Unresolved one if not.
TypeAliasDecl *SemaDecl::LookupTypeName(Identifier Name, llvm::SMLoc Loc) {
  TypeAliasDecl *TAD = getTypeHT(TypeScopeHT).lookup(Name).second;
  if (TAD) return TAD;
  
  // If we don't have a definition for this type, introduce a new TypeAliasDecl
  // with an unresolved underlying type.
  TAD = new (S.Context) TypeAliasDecl(Loc, Name, S.Context.TheUnresolvedType);
  getUnresolvedTypesHT(UnresolvedTypes)[Name] = TAD;
  
  // Inject this into the outermost scope so that subsequent name lookups of the
  // same type will find it.
  llvm::ScopedHashTableScope<Identifier, TypeScopeEntry> *S =
    getTypeHT(TypeScopeHT).getCurScope();
  while (S->getParentScope())
    S = S->getParentScope();
  
  getTypeHT(TypeScopeHT).insertIntoScope(S, Name, std::make_pair(0, TAD));
  return TAD;
}

/// AddToScope - Register the specified decl as being in the current lexical
/// scope.
void SemaDecl::AddToScope(ValueDecl *D) {
  // If we have a shadowed variable definition, check to see if we have a
  // redefinition: two definitions in the same scope with the same name.
  std::pair<unsigned, ValueDecl*> Entry =
    getValueHT(ValueScopeHT).lookup(D->Name);
  if (Entry.second && Entry.first == CurScope->getDepth()) {
    Error(D->getLocStart(),
          "variable declaration conflicts with previous declaration");
    Note(LookupValueName(D->Name)->getLocStart(), "previous declaration here");
    return;
  }
  
  getValueHT(ValueScopeHT).insert(D->Name,
                                  std::make_pair(CurScope->getDepth(), D));
}

/// GetAnonDecl - Get the anondecl for the specified anonymous closure
/// argument reference.  This occurs for use of $0 .. $9.
AnonDecl *SemaDecl::GetAnonDecl(llvm::StringRef Text, llvm::SMLoc RefLoc) {
  assert(Text.size() >= 2 && Text[0] == '$' && 
         Text[1] >= '0' && Text[1] <= '9' && "Not a valid anon decl");
  unsigned ArgNo = 0;
  if (Text.substr(1).getAsInteger(10, ArgNo)) {
    Error(RefLoc, "invalid name in $ expression");
    return 0;
  }
           
  // If this is the first reference to the anonymous symbol decl, create it.
  if (AnonClosureArgs.size() <= ArgNo || AnonClosureArgs[ArgNo].isNull()) {
    // Otherwise, this is the first reference to the anonymous decl,
    // synthesize it now.
    if (ArgNo >= AnonClosureArgs.size())
      AnonClosureArgs.resize(ArgNo+1);
    
    AnonClosureArgs[ArgNo] =
      new (S.Context) AnonDecl(RefLoc, S.Context.getIdentifier(Text),
                               S.Context.TheDependentType);
  }
  return AnonClosureArgs[ArgNo].get();
}

//===----------------------------------------------------------------------===//
// Name Processing.
//===----------------------------------------------------------------------===//

/// GetTypeForPath - This returns the type of an element of the specified type.
/// This is always a structural, syntactic, query as it is used for name
/// processing, which is in the space of "how people wrote it".
static Type *GetTypeForPath(Type *Ty, llvm::ArrayRef<unsigned> Path) {
  if (Path.empty())
    return Ty;
  
  // Right now, you can only dive into syntactic tuples.
  TupleType *TT = llvm::dyn_cast<TupleType>(Ty);
  if (TT == 0) return 0;
  
  // Reject invalid indices.
  if (Path[0] >= TT->Fields.size())
    return 0;
  
  return GetTypeForPath(TT->getElementType(Path[0]), Path.slice(1));
}

/// ActOnElementName - Assign a name to an element of D specified by Path.
ElementRefDecl *SemaDecl::
ActOnElementName(Identifier Name, llvm::SMLoc NameLoc, VarDecl *D,
                 llvm::ArrayRef<unsigned> Path) {
  Type *Ty = GetTypeForPath(D->Ty, Path);
  assert(Ty && "Access path validity should already have been checked by"
         " CheckAccessPathArity");
  
  // Create the decl for this name and add it to the current scope.
  return new (S.Context) ElementRefDecl(D, NameLoc, Name, Ty);
}

/// CheckAccessPathArity - Check that the type specified by the access path has
/// the right arity and return false if so.  Otherwise emit an error and emit
/// true.
bool SemaDecl::CheckAccessPathArity(unsigned NumChildren, llvm::SMLoc LPLoc,
                                    VarDecl *D, llvm::ArrayRef<unsigned> Path) {
  TupleType *Ty =llvm::dyn_cast_or_null<TupleType>(GetTypeForPath(D->Ty, Path));
  if (Ty && Ty->Fields.size() == NumChildren)
    return false;
  
  Error(LPLoc, "tuple specifier has wrong number of elements for actual type");
  return true;
}


//===----------------------------------------------------------------------===//
// Declaration handling.
//===----------------------------------------------------------------------===//

/// DiagnoseUnresolvedTypes - This function is invoked on all nodes in an
/// expression tree checking to make sure they don't contain any DependentTypes.
static Expr *DiagnoseUnresolvedTypes(Expr *E, Expr::WalkOrder Order,
                                     void *Data) {
  // Ignore the preorder walk.  We'd rather diagnose use of unresolved types
  // during the postorder walk so that the inner most expressions are diagnosed
  // before the outermost ones.
  if (Order == Expr::Walk_PreOrder)
    return E;
  
  if (E->Ty->getAs<DependentType>() == 0)
    return E;
  
  SemaDecl &SD = *(SemaDecl*)Data;
  E->dump();  // FIXME: This is a gross hack because our diagnostics suck.
  SD.Error(E->getLocStart(),
           "ambiguous expression could not resolve a concrete type");
  return 0;
}


/// ActOnTopLevelDecl - This is called after parsing a new top-level decl.
void SemaDecl::ActOnTopLevelDecl(ValueDecl *D) {
  // Check for and diagnose any uses of anonymous arguments that were unbound.
  for (unsigned i = 0, e = AnonClosureArgs.size(); i != e; ++i) {
    if (AnonClosureArgs[i].isNull()) continue;
    AnonDecl *AD = AnonClosureArgs[i].get();

    Error(AD->UseLoc,
          "use of anonymous closure argument in non-closure context");
  }
  AnonClosureArgs.clear();
  
  if (Expr *E = D->Init)
    E->WalkExpr(DiagnoseUnresolvedTypes, this);
}

/// ActOnTopLevelDeclError - This is called after an error parsing a top-level
/// decl.
void SemaDecl::ActOnTopLevelDeclError() {
  // Clear out any referenced anonymous closure arguments without diagnosing
  // them.  The error was already reported with the malformed decl.
  AnonClosureArgs.clear();
}


/// ValidateAttributes - Check that the func/var declaration attributes are ok.
static void ValidateAttributes(DeclAttributes &Attrs, Type *Ty, SemaDecl &SD) {
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.InfixPrecedence != -1) {
    bool IsError = true;
    if (FunctionType *FT = llvm::dyn_cast<FunctionType>(Ty))
      if (TupleType *TT = llvm::dyn_cast<TupleType>(FT->Input))
        IsError = TT->Fields.size() != 2;
    if (IsError) {
      SD.Error(Attrs.LSquareLoc, "function with 'infix' specified must take "
               "a two element tuple as input");
      Attrs.InfixPrecedence = -1;
    }
  }
}

VarDecl *SemaDecl::ActOnVarDecl(llvm::SMLoc VarLoc, Identifier Name,
                                Type *Ty, Expr *Init, DeclAttributes &Attrs) {
  assert((Ty != 0 || Init != 0) && "Must have a type or an expr already");
  
  if (Ty == 0)
    Ty = Init->Ty;
  else if (Init) {
    // If both a type and an initializer are specified, make sure the
    // initializer's type agrees with the (redundant) type.
    Expr *InitE = S.expr.ConvertToType(Init, Ty, false, SemaExpr::CR_VarInit);
    if (InitE)
      Init = InitE;
    else
      Ty = Init->Ty;
  }
  
  // Validate attributes.
  ValidateAttributes(Attrs, Ty, *this);
  
  return new (S.Context) VarDecl(VarLoc, Name, Ty, Init, Attrs);
}

FuncDecl *SemaDecl::
ActOnFuncDecl(llvm::SMLoc FuncLoc, Identifier Name,
              Type *Ty, DeclAttributes &Attrs) {
  assert(Ty && "Type not specified?");

  // Validate attributes.
  ValidateAttributes(Attrs, Ty, *this);

  return new (S.Context) FuncDecl(FuncLoc, Name, Ty, 0, Attrs);
}

/// FuncTypePiece - This little enum is used by AddFuncArgumentsToScope to keep
/// track of where in a function type it is currently looking.  This affects how
/// the decls are processed and created.
enum FuncTypePiece {
  FTP_Function,  // Looking at the initial functiontype itself.
  FTP_Input,     // Looking at the input to the function type
  FTP_Output     // Looking at the output to the function type.
};

/// AddFuncArgumentsToScope - Walk the type specified for a Func object (which
/// is known to be a FunctionType on the outer level) creating and adding named
/// arguments to the current scope.  This causes redefinition errors to be
/// emitted.
///
/// Note that we really *do* want dyn_cast here, not getAs, because we do not
/// want to look through type aliases or other sugar, we want to see what the
/// user wrote in the func declaration.
static void AddFuncArgumentsToScope(Type *Ty,
                                    llvm::SmallVectorImpl<unsigned> &AccessPath,
                                    FuncTypePiece Mode,
                                    llvm::SMLoc FuncLoc, SemaDecl &SD) {
  // Handle the function case first.
  if (Mode == FTP_Function) {
    FunctionType *FT = llvm::cast<FunctionType>(Ty);
    AccessPath.push_back(0);
    AddFuncArgumentsToScope(FT->Input, AccessPath, FTP_Input, FuncLoc, SD);
    
    AccessPath.back() = 1;
    
    // If this is a->b->c then we treat b as an input, not (b->c) as an output.
    if (llvm::isa<FunctionType>(FT->Result))
      AddFuncArgumentsToScope(FT->Result, AccessPath, FTP_Function, FuncLoc,SD);
    else    
      AddFuncArgumentsToScope(FT->Result, AccessPath, FTP_Output, FuncLoc, SD);
    AccessPath.pop_back();
    return;
  }

  // Otherwise, we're looking at an input or output to the func.  The only type
  // we currently dive into is the humble tuple, which can be recursive.
  TupleType *TT = llvm::dyn_cast<TupleType>(Ty);
  if (TT == 0) return;

  
  AccessPath.push_back(0);

  // For tuples, recursively processes their elements (to handle cases like:
  //    (x : (.a : int, .b : int), y: int) -> ...
  // and create decls for any named elements.
  for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
    AccessPath.back() = 1;
    AddFuncArgumentsToScope(TT->Fields[i].Ty, AccessPath, Mode, FuncLoc, SD);

    // If this field is named, create the argument decl for it.
    Identifier Name = TT->Fields[i].Name;
    // Ignore unnamed fields.
    if (Name.get() == 0) continue;
    
    
    // Create the argument decl for this named argument.
    ArgDecl *AD = new (SD.S.Context) ArgDecl(FuncLoc, Name, TT->Fields[i].Ty);
    
    // Eventually we should mark the input/outputs as readonly vs writeonly.
    //bool isInput = Mode == FTP_Input;

    SD.AddToScope(AD);
  }
  
  AccessPath.pop_back();
}


void SemaDecl::CreateArgumentDeclsForFunc(FuncDecl *FD) {
  llvm::SmallVector<unsigned, 8> AccessPath;
  AddFuncArgumentsToScope(FD->Ty, AccessPath, FTP_Function, FD->FuncLoc, *this);
}


FuncDecl *SemaDecl::ActOnFuncBody(FuncDecl *FD, Expr *Body) {
  assert(FD && Body && "Elements of func body not specified?");
         
  // Validate that the body's type matches the function's type if this isn't a
  // external function.
  Body = S.expr.ConvertToType(Body, FD->Ty, false, SemaExpr::CR_FuncBody);
  if (Body == 0) return 0;
  
  FD->Init = Body;
  return FD;
}

Decl *SemaDecl::ActOnStructDecl(llvm::SMLoc StructLoc, DeclAttributes &Attrs,
                                Identifier Name, Type *Ty) {
  // The 'struct' is syntactically fine, invoke the semantic actions for the
  // syntactically expanded oneof type.  Struct declarations are just sugar for
  // other existing constructs.
  SemaType::OneOfElementInfo ElementInfo;
  ElementInfo.Name = Name.str();
  ElementInfo.NameLoc = StructLoc;
  ElementInfo.EltType = Ty;
  OneOfType *OneOfTy = S.type.ActOnOneOfType(StructLoc, Attrs, ElementInfo);
  
  // Given the type, we create a TypeAlias and inject it into the current scope.
  TypeAliasDecl *TAD = S.decl.ActOnTypeAlias(StructLoc, Name, OneOfTy);
  
  // In addition to defining the oneof declaration, structs also inject their
  // constructor into the global scope.
  assert(OneOfTy->Elements.size() == 1 && "Struct has exactly one element");
  S.decl.AddToScope(OneOfTy->getElement(0));
  
  return TAD;
}


TypeAliasDecl *SemaDecl::ActOnTypeAlias(llvm::SMLoc TypeAliasLoc,
                                        Identifier Name, Type *Ty) {
  std::pair<unsigned,TypeAliasDecl*> Entry =getTypeHT(TypeScopeHT).lookup(Name);

  // If we have no existing entry, or if the existing entry is at a different
  // scope level then this is a valid insertion.
  if (Entry.second == 0 || Entry.first != CurScope->getDepth()) {
    TypeAliasDecl *New = new (S.Context) TypeAliasDecl(TypeAliasLoc, Name, Ty);
    getTypeHT(TypeScopeHT).insert(Name,
                                  std::make_pair(CurScope->getDepth(), New));
    return New;
  }
  
  TypeAliasDecl *ExistingDecl = Entry.second;
  
  // If the previous definition was just a use of an undeclared type, complete
  // the type now.
  if (llvm::isa<UnresolvedType>(ExistingDecl->UnderlyingTy)) {
    // Remove the entry for this type from the UnresolvedTypes map.
    getUnresolvedTypesHT(UnresolvedTypes).erase(Name);
    
    // Update the decl we already have to be the correct type.
    ExistingDecl->TypeAliasLoc = TypeAliasLoc;
    ExistingDecl->UnderlyingTy = Ty;
    return ExistingDecl;
  }
  
  // Otherwise, we have a redefinition: two definitions in the same scope with
  // the same name.
  Error(TypeAliasLoc,
        "redefinition of type named '" +llvm::StringRef(Name.get()) + "'");
  Note(ExistingDecl->getLocStart(), "previous declaration here");
  return ExistingDecl;
}
