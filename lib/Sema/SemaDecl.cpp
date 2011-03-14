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
    UnresolvedTypes(new UnresolvedTypesMapTy()) {
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
    error(I->second->getLocStart(), "use of undeclared type '" + I->first.str()
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
    error(D->getLocStart(),
          "variable declaration conflicts with previous declaration");
    warning(LookupValueName(D->Name)->getLocStart(), "previous declaration here");
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
    error(RefLoc, "invalid name in $ expression");
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

/// ActOnElementName - Assign a name to an element of D specified by Path.
ElementRefDecl *SemaDecl::
ActOnElementName(Identifier Name, llvm::SMLoc NameLoc, VarDecl *D,
                 llvm::ArrayRef<unsigned> Path) {
  Type *Ty = ElementRefDecl::getTypeForPath(D->Ty, Path);

  // If the type of the path is obviously invalid, diagnose it now and refuse to
  // create the decl.  The most common result here is DependentType, which
  // allows type checking to resolve this later.
  if (Ty == 0) {
    error(NameLoc, "'" + Name.str() + "' is an invalid index for '" +
          D->Ty->getString() + "'");
    return 0;
  }
  
  // Copy the access path into ASTContext's memory.
  Path = S.Context.AllocateCopy(Path);
  
  // Create the decl for this name and add it to the current scope.
  return new (S.Context) ElementRefDecl(D, NameLoc, Name, Path, Ty);
}

//===----------------------------------------------------------------------===//
// Declaration handling.
//===----------------------------------------------------------------------===//

/// ActOnTopLevelDecl - This is called after parsing a new top-level decl.
void SemaDecl::ActOnTopLevelDecl(ValueDecl *D) {
#if 0
  // Check for and diagnose any uses of anonymous arguments that were unbound.
  for (unsigned i = 0, e = AnonClosureArgs.size(); i != e; ++i) {
    if (AnonClosureArgs[i].isNull()) continue;
    AnonDecl *AD = AnonClosureArgs[i].get();

    error(AD->UseLoc,
          "use of anonymous closure argument in non-closure context");
  }
#endif
  
  AnonClosureArgs.clear();
}

/// ActOnTopLevelDeclError - This is called after an error parsing a top-level
/// decl.
void SemaDecl::ActOnTopLevelDeclError() {
  // Clear out any referenced anonymous closure arguments without diagnosing
  // them.  The error was already reported with the malformed decl.
  AnonClosureArgs.clear();
}

/// Note that DeclVarName is sitting on the stack, not copied into the
/// ASTContext.
VarDecl *SemaDecl::ActOnVarDecl(llvm::SMLoc VarLoc, DeclVarName &Name,
                                Type *Ty, Expr *Init, DeclAttributes &Attrs) {
  assert((Ty != 0 || Init != 0) && "Must have a type or an expr already");
  if (Ty == 0)
    Ty = S.Context.TheDependentType;
  
  if (Name.isSimple())
    return new (S.Context) VarDecl(VarLoc, Name.Name, Ty, Init, Attrs);
  
  // Copy the name into the ASTContext heap.
  DeclVarName *TmpName = new (S.Context) DeclVarName(Name);
  return new (S.Context) VarDecl(VarLoc, TmpName, Ty, Init, Attrs);
}

FuncDecl *SemaDecl::
ActOnFuncDecl(llvm::SMLoc FuncLoc, Identifier Name,
              Type *Ty, DeclAttributes &Attrs) {
  assert(Ty && "Type not specified?");

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
  error(TypeAliasLoc,
        "redefinition of type named '" +llvm::StringRef(Name.get()) + "'");
  warning(ExistingDecl->getLocStart(), "previous declaration here");
  return ExistingDecl;
}
