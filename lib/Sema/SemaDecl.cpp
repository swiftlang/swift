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
#include "swift/AST/Types.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
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
void SemaDecl::handleEndOfTranslationUnit(TranslationUnitDecl *TUD,
                                          SMLoc FileStart,
                                          ArrayRef<ExprOrDecl> Items,
                                          SMLoc FileEnd) {
  // First thing, we transform the body into a brace expression.
  ExprOrDecl *NewElements = 
    S.Context.AllocateCopy<ExprOrDecl>(Items.begin(), Items.end());
  TUD->Body = new (S.Context) BraceExpr(FileStart, NewElements, Items.size(),
                                        false, FileEnd);
  
  // Do a prepass over the declarations to make sure they have basic sanity and
  // to find the list of top-level value declarations.
  for (unsigned i = 0, e = TUD->Body->NumElements; i != e; ++i) {
    if (!TUD->Body->Elements[i].is<Decl*>()) continue;
    
    Decl *D = TUD->Body->Elements[i].get<Decl*>();
       
    // If any top-level value decl has an unresolved type, then it is erroneous.
    // It is not valid to have something like "var x = 4" at the top level, all
    // types must be explicit here.
    ValueDecl *VD = dyn_cast<ValueDecl>(D);
    if (VD == 0) continue;

    // FIXME: This can be better handled in the various ActOnDecl methods when
    // they get passed in a parent context decl.

    // Verify that values have a type specified.
    if (VD->Ty->is<DependentType>()) {
      error(VD->getLocStart(),
            "top level declarations require a type specifier");
      // FIXME: Should mark the decl as invalid.
      VD->Ty = TupleType::getEmpty(S.Context);
    }
  }
  
  // Verify that any forward declared types were ultimately defined.
  // TODO: Move this to name binding!
  unsigned Next = 0;
  for (TypeAliasDecl *Decl : UnresolvedTypeList) {
    
    // If a type got defined, remove it from the vector.
    if (!isa<UnresolvedType>(Decl->UnderlyingTy.getPointer()))
      continue;
    
    UnresolvedTypeList[Next++] = Decl;
  }
  // Strip out stuff that got replaced.
  UnresolvedTypeList.resize(Next);
    
  TUD->UnresolvedTypesForParser = S.Context.AllocateCopy(UnresolvedTypeList);
}

//===----------------------------------------------------------------------===//
// Name lookup.
//===----------------------------------------------------------------------===//

/// LookupValueName - Perform a lexical scope lookup for the specified name,
/// returning the active decl if found or null if not.
ValueDecl *SemaDecl::LookupValueName(Identifier Name) {
  std::pair<unsigned, ValueDecl*> Res = getValueHT(ValueScopeHT).lookup(Name);
  // If we found nothing, or we found a decl at the top-level, return nothing.
  // We ignore results at the top-level because we may have overloading that
  // will be resolved properly by name binding.
  if (Res.first == 0) return 0;
  return Res.second;
}

/// LookupTypeName - Perform a lexical scope lookup for the specified name in
/// a type context, returning the decl if found or installing and returning a
/// new Unresolved one if not.
TypeAliasDecl *SemaDecl::LookupTypeName(Identifier Name, SMLoc Loc) {
  TypeAliasDecl *TAD = getTypeHT(TypeScopeHT).lookup(Name).second;
  if (TAD) return TAD;
  
  // If we don't have a definition for this type, introduce a new TypeAliasDecl
  // with an unresolved underlying type.
  TAD = new (S.Context) TypeAliasDecl(Loc, Name,UnresolvedType::get(S.Context));
  getUnresolvedTypesHT(UnresolvedTypes)[Name] = TAD;
  UnresolvedTypeList.push_back(TAD);
  
  // Inject this into the outermost scope so that subsequent name lookups of the
  // same type will find it.
  llvm::ScopedHashTableScope<Identifier, TypeScopeEntry> *S =
    getTypeHT(TypeScopeHT).getCurScope();
  while (S->getParentScope())
    S = S->getParentScope();
  
  getTypeHT(TypeScopeHT).insertIntoScope(S, Name, std::make_pair(0, TAD));
  return TAD;
}

static void DiagnoseRedefinition(ValueDecl *Prev, ValueDecl *New, SemaDecl &SD){
  assert(New != Prev && "Cannot conflict with self");
  if (New->Init)
    SD.error(New->getLocStart(), "definition conflicts with previous value");
  else
    SD.error(New->getLocStart(), "declaration conflicts with previous value");
  
  if (Prev->Init)
    SD.note(Prev->getLocStart(), "previous definition here");
  else
    SD.note(Prev->getLocStart(), "previous declaration here");
}

/// CheckValidOverload - Check whether it is ok for D1 and D2 to be declared at
/// the same scope.  This check is a transitive relationship, so if "D1 is a
/// valid overload of D2" and "D2 is a valid overload of D3" then we know that
/// D1/D3 are valid overloads and we don't have to check all permutations.
static bool CheckValidOverload(const ValueDecl *D1, const ValueDecl *D2,
                               SemaDecl &SD) {
  if (D1->Attrs.InfixPrecedence != D2->Attrs.InfixPrecedence) {
    SD.error(D1->getLocStart(),
             "infix precedence of functions in an overload set must match");
    SD.note(D2->getLocStart(), "previous declaration here");
    return true;
  }
  
  // Otherwise, everything is fine.
  return false;
}

/// AddToScope - Register the specified decl as being in the current lexical
/// scope.
void SemaDecl::AddToScope(ValueDecl *D) {
  // If we have a shadowed variable definition, check to see if we have a
  // redefinition: two definitions in the same scope with the same name.
  ValueScopeHTType &ValueHT = getValueHT(ValueScopeHT);
  ValueScopeHTType::iterator EntryI = ValueHT.begin(D->Name);
  
  // A redefinition is a hit in the scoped table at the same depth.
  if (EntryI != ValueHT.end() && EntryI->first == CurScope->getDepth()) {
    ValueDecl *PrevDecl = EntryI->second;
    
    // If this is at top-level scope, we allow overloading.  If not, we don't.
    // FIXME: This should be tied to whether the scope corresponds to a
    // DeclContext like a TranslationUnit or a Namespace.  Add a bit to Scope
    // to track this?
    if (CurScope->getDepth() != 0)
      return DiagnoseRedefinition(PrevDecl, D, *this);
    
    // If this is at top-level scope, validate that the members of the overload
    // set all agree.
    
    // Check to see if D and PrevDecl are valid in the same overload set.
    if (CheckValidOverload(D, PrevDecl, *this))
      return;
    
    // Note: we don't check whether all of the elements of the overload set have
    // different argument types.  This is checked later.
  }
  
  getValueHT(ValueScopeHT).insert(D->Name,
                                  std::make_pair(CurScope->getDepth(), D));
}

//===----------------------------------------------------------------------===//
// Name Processing.
//===----------------------------------------------------------------------===//

/// ActOnElementName - Assign a name to an element of D specified by Path.
ElementRefDecl *SemaDecl::
ActOnElementName(Identifier Name, SMLoc NameLoc, VarDecl *D,
                 ArrayRef<unsigned> Path) {
  Type Ty = ElementRefDecl::getTypeForPath(D->Ty, Path);

  // If the type of the path is obviously invalid, diagnose it now and refuse to
  // create the decl.  The most common result here is DependentType, which
  // allows type checking to resolve this later.
  if (Ty.isNull()) {
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

Decl *SemaDecl::ActOnImportDecl(SMLoc ImportLoc,
                        ArrayRef<std::pair<Identifier, SMLoc>> Path,
                                DeclAttributes &Attrs) {
  if (!Attrs.empty())
    error(Attrs.LSquareLoc, "invalid attributes specified for import");
  
  Path = S.Context.AllocateCopy(Path);
  return new (S.Context) ImportDecl(ImportLoc, Path);
}


/// Note that DeclVarName is sitting on the stack, not copied into the
/// ASTContext.
VarDecl *SemaDecl::ActOnVarDecl(SMLoc VarLoc, DeclVarName &Name,
                                Type Ty, Expr *Init, DeclAttributes &Attrs) {
  assert((!Ty.isNull() || Init != 0) && "Must have a type or an expr already");
  if (Ty.isNull())
    Ty = DependentType::get(S.Context);
  
  if (Name.isSimple())
    return new (S.Context) VarDecl(VarLoc, Name.Name, Ty, Init, Attrs);
  
  // Copy the name into the ASTContext heap.
  DeclVarName *TmpName = new (S.Context) DeclVarName(Name);
  return new (S.Context) VarDecl(VarLoc, TmpName, Ty, Init, Attrs);
}

FuncDecl *SemaDecl::
ActOnFuncDecl(SMLoc FuncLoc, Type ReceiverType, Identifier Name, Type Ty,
              DeclAttributes &Attrs) {
  assert(!Ty.isNull() && "Type not specified?");

  // If the parsed type is not spelled as a function type (i.e., has no '->' in
  // it), then it is implicitly a function that returns ().
  if (!isa<FunctionType>(Ty.getPointer()))
    Ty = S.type.ActOnFunctionType(Ty, SMLoc(), TupleType::getEmpty(S.Context));

  // If a receiver type was specified, install the first type as the receiver,
  // as a tuple with element named 'this'.  This turns "int->int" on FooTy into
  // "(this : FooTy)->(int->int)".
  if (!ReceiverType.isNull()) {
    TupleTypeElt ReceiverElt(ReceiverType, S.Context.getIdentifier("this"));
    ReceiverType = TupleType::get(ReceiverElt, S.Context);
    Ty = S.type.ActOnFunctionType(ReceiverType, SMLoc(), Ty);
  }
  
  return new (S.Context) FuncDecl(FuncLoc, Name, Ty, 0, Attrs);
}

/// FuncTypePiece - This little enum is used by AddFuncArgumentsToScope to keep
/// track of where in a function type it is currently looking.  This affects how
/// the decls are processed and created.
enum class FuncTypePiece {
  Function,  // Looking at the initial functiontype itself.
  Input,     // Looking at the input to the function type
  Output     // Looking at the output to the function type.
};

/// AddFuncArgumentsToScope - Walk the type specified for a Func object (which
/// is known to be a FunctionType on the outer level) creating and adding named
/// arguments to the current scope.  This causes redefinition errors to be
/// emitted.
///
/// Note that we really *do* want dyn_cast here, not getAs, because we do not
/// want to look through type aliases or other sugar, we want to see what the
/// user wrote in the func declaration.
static void AddFuncArgumentsToScope(Type Ty,
                                    SmallVectorImpl<unsigned> &AccessPath,
                                    FuncTypePiece Mode,
                                    SMLoc FuncLoc, SemaDecl &SD) {
  // Handle the function case first.
  if (Mode == FuncTypePiece::Function) {
    FunctionType *FT = cast<FunctionType>(Ty.getPointer());
    AccessPath.push_back(0);
    AddFuncArgumentsToScope(FT->Input, AccessPath, FuncTypePiece::Input,
                            FuncLoc, SD);
    
    AccessPath.back() = 1;
    
    // If this is a->b->c then we treat b as an input, not (b->c) as an output.
    if (isa<FunctionType>(FT->Result.getPointer()))
      AddFuncArgumentsToScope(FT->Result, AccessPath,
                              FuncTypePiece::Function, FuncLoc,SD);
    else    
      AddFuncArgumentsToScope(FT->Result, AccessPath,
                              FuncTypePiece::Output, FuncLoc, SD);
    AccessPath.pop_back();
    return;
  }

  // Otherwise, we're looking at an input or output to the func.  The only type
  // we currently dive into is the humble tuple, which can be recursive.  This
  // should dive in syntactically.
  TupleType *TT = dyn_cast<TupleType>(Ty.getPointer());
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
    //bool isInput = Mode == FuncTypePiece::Input;

    SD.AddToScope(AD);
  }
  
  AccessPath.pop_back();
}


void SemaDecl::CreateArgumentDeclsForFunc(ValueDecl *D) {
  SmallVector<unsigned, 8> AccessPath;
  AddFuncArgumentsToScope(D->Ty, AccessPath, FuncTypePiece::Function,
                          D->getLocStart(), *this);
}


void SemaDecl::ActOnFuncBody(ValueDecl *FD, Expr *Body) {
  assert(FD && Body && "Elements of func body not specified?");
  FD->Init = Body;
}

void SemaDecl::ActOnStructDecl(SMLoc StructLoc, DeclAttributes &Attrs,
                               Identifier Name, Type BodyTy,
                               SmallVectorImpl<ExprOrDecl> &Decls) {
  // Get the TypeAlias for the name that we'll eventually have.  This ensures
  // that the constructors generated have the pretty name for the type instead
  // of the raw oneof.
  TypeAliasDecl *TAD = S.decl.ActOnTypeAlias(StructLoc, Name,
                                             UnresolvedType::get(S.Context));
  Decls.push_back(TAD);
  // The 'struct' is syntactically fine, invoke the semantic actions for the
  // syntactically expanded oneof type.  Struct declarations are just sugar for
  // other existing constructs.
  SemaType::OneOfElementInfo ElementInfo;
  ElementInfo.Name = Name.str();
  ElementInfo.NameLoc = StructLoc;
  ElementInfo.EltType = BodyTy;
  OneOfType *OneOfTy = S.type.ActOnOneOfType(StructLoc, Attrs, ElementInfo,TAD);
  assert(OneOfTy->hasSingleElement() && "Somehow isn't a struct?");
  
  // In addition to defining the oneof declaration, structs also inject their
  // constructor into the global scope.
  assert(OneOfTy->Elements.size() == 1 && "Struct has exactly one element");
  S.decl.AddToScope(OneOfTy->getElement(0));
  Decls.push_back(OneOfTy->getElement(0));
}


TypeAliasDecl *SemaDecl::ActOnTypeAlias(SMLoc TypeAliasLoc,
                                        Identifier Name, Type Ty) {
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
  if (ExistingDecl->UnderlyingTy->is<UnresolvedType>()) {
    // Remove the entry for this type from the UnresolvedTypes map.
    getUnresolvedTypesHT(UnresolvedTypes).erase(Name);
    
    // This will get removed from UnresolvedTypeList at the end of the TU.
    
    // Update the decl we already have to be the correct type.
    ExistingDecl->TypeAliasLoc = TypeAliasLoc;
    ExistingDecl->UnderlyingTy = Ty;
    return ExistingDecl;
  }
  
  // Otherwise, we have a redefinition: two definitions in the same scope with
  // the same name.
  error(TypeAliasLoc,
        "redefinition of type named '" +StringRef(Name.get()) + "'");
  warning(ExistingDecl->getLocStart(), "previous declaration here");
  return ExistingDecl;
}
