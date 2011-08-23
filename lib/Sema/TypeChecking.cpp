//===--- TypeChecking.cpp - Type Checking ---------------------------------===//
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
// This file implements semantic analysis for expressions, and other pieces
// that require final type checking.  If this passes a translation unit with no
// errors, then it is good to go.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "TypeChecking.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

void TypeChecker::note(SMLoc Loc, const Twine &Message) {
  Context.SourceMgr.PrintMessage(Loc, Message, "note");
}
void TypeChecker::warning(SMLoc Loc, const Twine &Message) {
  Context.SourceMgr.PrintMessage(Loc, Message, "warning");
}
void TypeChecker::error(SMLoc Loc, const Twine &Message) {
  Context.setHadError();
  Context.SourceMgr.PrintMessage(Loc, Message, "error");
}


//===----------------------------------------------------------------------===//
// BindAndValidateClosureArgs - When a closure is formed, this walks an AST to
// update AnonClosureArgExpr to be of the right type.
//===----------------------------------------------------------------------===//

namespace {
struct RewriteAnonArgExpr {
  Type FuncInputTy;
  TypeChecker &TC;
  
  RewriteAnonArgExpr(Type funcInputTy, TypeChecker &tc)
    : FuncInputTy(funcInputTy), TC(tc) {}
  
  static Expr *WalkFn(Expr *E, Expr::WalkOrder Order, void *rewriter) {
    RewriteAnonArgExpr &Rewriter = *static_cast<RewriteAnonArgExpr*>(rewriter);
    Type FuncInputTy = Rewriter.FuncInputTy;
  
    if (Order == Expr::WalkOrder::PreOrder) {
      // If this is a ClosureExpr, don't walk into it.  This would find *its*
      // anonymous closure arguments, not ours.
      if (isa<ClosureExpr>(E)) return 0; // Don't recurse into it.
      
      // Otherwise, do recurse into it.  We handle anon args in the postorder
      // visitation.
      return E;
    }
  
    // If we found a closure argument, process it.
    AnonClosureArgExpr *A = dyn_cast<AnonClosureArgExpr>(E);
    if (A == 0) return E;  
    
    // If the input to the function is a non-tuple, only $0 is valid, if it is a
    // tuple, then $0..$N are valid depending on the number of inputs to the
    // tuple.
    unsigned NumInputArgs = 1;
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy.getPointer()))
      NumInputArgs = TT->Fields.size();
    
    assert(A->Ty->is<DependentType>() && "Anon arg already has a type?");
    
    // Verify that the argument number isn't too large, e.g. using $4 when the
    // bound function only has 2 inputs.
    if (A->ArgNo >= NumInputArgs) {
      Rewriter.TC.error(A->Loc,
               "use of invalid anonymous argument, with number higher than"
               " # arguments to bound function");
      return 0;
    }
    
    // Assign the AnonDecls their actual concrete types now that we know the
    // context they are being used in.
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy.getPointer())) {
      A->Ty = TT->getElementType(A->ArgNo);
    } else {
      assert(NumInputArgs == 1 && "Must have unary case");
      A->Ty = FuncInputTy;
    }
    return A;
  }
};
} // end anonymous namespace

/// bindAndValidateClosureArgs - The specified list of anonymous closure
/// arguments was bound to a closure function with the specified input
/// arguments.  Validate the argument list and, if valid, allocate and return
/// a pointer to the argument to be used for the ClosureExpr.
bool TypeChecker::bindAndValidateClosureArgs(Expr *Body, Type FuncInput) {  
  RewriteAnonArgExpr Rewriter(FuncInput, *this);
  
  // Walk the body and rewrite any anonymous arguments.  Note that this
  // isn't a particularly efficient way to handle this, because we walk subtrees
  // even if they have no anonymous arguments.
  return Body->WalkExpr(RewriteAnonArgExpr::WalkFn, 0, &Rewriter) == 0;
}



//===----------------------------------------------------------------------===//
// Type Validation
//===----------------------------------------------------------------------===//

/// validateType - Recursively check to see if the type of a decl is valid.  If
/// not, diagnose the problem and collapse it to an ErrorType.
bool TypeChecker::validateType(ValueDecl *VD) {
  if (!validateType(VD->Ty)) return false;
  
  VD->Ty = ErrorType::get(Context);
  return true;
}

/// validateType - Types can contain expressions (in the default values for
/// tuple elements), and thus need semantic analysis to ensure that these
/// expressions are valid and that they have the appropriate conversions etc.
///
/// This returns true if the type is invalid.
bool TypeChecker::validateType(Type InTy) {
  assert(InTy && "Cannot validate null types!");

  TypeBase *T = InTy.getPointer();
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If a type has a canonical type, then it is known safe.
  if (T->hasCanonicalTypeComputed()) return false;

  bool IsInvalid = false;
  
  switch (T->Kind) {
  case TypeKind::Error:
    // Error already diagnosed.
    return true;
  case TypeKind::BuiltinInt1:
  case TypeKind::BuiltinInt8:
  case TypeKind::BuiltinInt16:
  case TypeKind::BuiltinInt32:
  case TypeKind::BuiltinInt64:
  case TypeKind::Dependent:
    return false;
  case TypeKind::OneOf:
    for (OneOfElementDecl *Elt : cast<OneOfType>(T)->Elements) {
      // Ignore element decls that have no associated type.
      if (Elt->ArgumentType.isNull())
        continue;
      
      IsInvalid = validateType(Elt);
      if (IsInvalid) break;
    }
    break;
  case TypeKind::NameAlias:
    IsInvalid = validateType(cast<NameAliasType>(T)->TheDecl->UnderlyingTy);
    if (IsInvalid)
      cast<NameAliasType>(T)->TheDecl->UnderlyingTy = ErrorType::get(Context);
    break;
  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(T);
    
    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type EltTy = TT->Fields[i].Ty;
      if (EltTy && validateType(EltTy)) {
        IsInvalid = true;
        break;
      }

      Expr *EltInit = TT->Fields[i].Init;
      if (EltInit == 0) continue;
      
      SMLoc InitLoc = EltInit->getLocStart();
      checkBody(EltInit, EltTy);
      if (EltInit == 0) {
        note(InitLoc, "while converting default tuple value to element type");
        IsInvalid = true;
        break;
      }
        
      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      assert(EltTy.isNull() || EltTy->isEqual(EltInit->Ty, Context));
      EltTy = EltInit->Ty;

      TT->updateInitializedElementType(i, EltTy, EltInit);
    }
    break;
  }
      
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(T);
    IsInvalid = validateType(FT->Input) || validateType(FT->Result);
    break;
  }
  case TypeKind::Array:
    ArrayType *AT = cast<ArrayType>(T);
    IsInvalid = validateType(AT->Base);
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }

  // If we determined that this type is invalid, erase it in the caller.
  if (IsInvalid) {
    InTy = ErrorType::get(Context);
    return true;
  }

  // Now that we decided that this type is ok, get the canonical type for it so
  // that we never reanalyze it again.
  // If it is ever a performance win to avoid computing canonical types, we can
  // just keep a SmallPtrSet of analyzed Types in TypeChecker.
  InTy->getCanonicalType(Context);
  
  // FIXME: This isn't good enough: top-level stuff can have these as well and
  // their types need to be resolved at the end of name binding.  Perhaps we
  // should require them to have explicit types even if they have values and 
  // let the value mismatch be detected at typechecking time? 
  return false;
}

//===----------------------------------------------------------------------===//
// Type Checking Entrypoint
//===----------------------------------------------------------------------===//

/// validateAttributes - Check that the func/var declaration attributes are ok.
void TypeChecker::validateAttributes(DeclAttributes &Attrs, Type Ty) {
  // If the decl is a unary operator, then it must be a function whose input is
  // a single element tuple.
  if (Attrs.isUnary) {
    bool IsError = true;
    if (FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer()))
      if (TupleType *TT = dyn_cast<TupleType>(FT->Input.getPointer()))
        IsError = TT->Fields.size() != 1;
    if (IsError) {
      error(Attrs.LSquareLoc, "function with 'unary' specified must take "
            "a single element tuple as input");
      Attrs.isUnary = false;
      // FIXME: Set the 'isError' bit on the decl.
    }
  }
  
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.InfixPrecedence != -1) {
    bool IsError = true;
    if (FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer()))
      if (TupleType *TT = dyn_cast<TupleType>(FT->Input.getPointer()))
        IsError = TT->Fields.size() != 2;
    if (IsError) {
      error(Attrs.LSquareLoc, "function with 'infix' specified must take "
            "a two element tuple as input");
      Attrs.InfixPrecedence = -1;
      // FIXME: Set the 'isError' bit on the decl.
    }
  }
}

/// DiagnoseUnresolvedTypes - This function is invoked on all nodes in an
/// expression tree checking to make sure they don't contain any DependentTypes.
static Expr *DiagnoseUnresolvedTypes(Expr *E, Expr::WalkOrder Order,
                                     void *Data) {
  TypeChecker &TC = *(TypeChecker*)Data;
  
  // Ignore the preorder walk.  We'd rather diagnose use of unresolved types
  // during the postorder walk so that the inner most expressions are diagnosed
  // before the outermost ones.
  if (Order == Expr::WalkOrder::PreOrder)
    return E;
  
  assert(!isa<SequenceExpr>(E) && "Should have resolved this");
  
  // Use is to strip off sugar.
  if (!E->Ty->is<DependentType>())
    return E;
  
  TC.error(E->getLocStart(),
           "ambiguous expression was not resolved to a concrete type");
  return 0;
}


/// checkBody - Type check an expression that is used in a top-level
/// context like a var/func body, or tuple default value.  If DestTy is
/// specified, the expression is coerced to the requested type.
///
/// If the body turns out to be a sequence, this returns the single element
/// with the excess in the provided smallvector.
void TypeChecker::checkBody(Expr *&E, Type DestTy) {
  assert(E != 0 && "Can't check a null body!");
  E = typeCheckExpression(E);
  if (E == 0) return;
  
  if (DestTy)
    E = convertToType(E, DestTy);
  
  // Check the initializer/body to make sure that we succeeded in resolving
  // all of the types contained within it.  We should not have any
  // DependentType's left for subexpressions.
  if (E)
    E = E->WalkExpr(DiagnoseUnresolvedTypes, 0, this);
}

void TypeChecker::typeCheck(TypeAliasDecl *TAD) {
  validateType(TAD->getAliasType(Context));
}

void TypeChecker::typeCheckERD(ElementRefDecl *ERD) {
  // If the type is already resolved we're done.  ElementRefDecls are simple.
  if (!ERD->Ty->is<DependentType>()) return;
  
  if (Type T = ElementRefDecl::getTypeForPath(ERD->VD->Ty, ERD->AccessPath))
    ERD->Ty = T;
  else {
    error(ERD->getLocStart(), "'" + ERD->Name.str() +
          "' is an invalid index for '" + ERD->VD->Ty->getString() +
          "'");
    // FIXME: This should be "invalid"
    ERD->Ty = TupleType::getEmpty(Context);
  }
}

bool TypeChecker::validateVarName(Type Ty, DeclVarName *Name) {
  // Check for a type specifier mismatch on this level.
  assert(Ty && "This lookup should never fail");

  // If this is a simple varname, then it matches any type, and we're done.
  if (Name->isSimple())
    return false;

  // If we're peering into an unresolved type, we can't analyze it yet.
  if (Ty->is<DependentType>()) return false;

  // If we have a single-element oneof (like a struct) then we allow matching
  // the struct elements with the tuple syntax.
  if (OneOfType *OOT = Ty->getAs<OneOfType>())
    if (OOT->hasSingleElement())
      Ty = OOT->getElement(0)->ArgumentType;
  
  // If we have a complex case, Ty must be a tuple and the name specifier must
  // have the correct number of elements.
  TupleType *AccessedTuple = Ty->getAs<TupleType>();
  if (AccessedTuple == 0) {
    error(Name->LPLoc, "name specifier matches '" + Ty->getString() +
          "' which is not a tuple");
    return true;
  }

  // Verify the # elements line up.
  if (Name->Elements.size() != AccessedTuple->Fields.size()) {
    error(Name->LPLoc, "name specifier matches '" + Ty->getString() +
          "' which requires " + Twine(AccessedTuple->Fields.size()) +
          " names, but has " + Twine(Name->Elements.size()));
    return true;
  }
  
  // Okay, everything looks good at this level, recurse.
  for (unsigned i = 0, e = Name->Elements.size(); i != e; ++i) {
    if (validateVarName(AccessedTuple->Fields[i].Ty, Name->Elements[i]))
      return true;
  }

  return false;
}

void TypeChecker::typeCheckVarDecl(VarDecl *VD) {
  // Type check the ValueDecl part of a VarDecl.
  if (typeCheckValueDecl(VD))
    return;
  
  // If the VarDecl had a name specifier, verify that it lines up with the
  // actual type of the VarDecl.
  if (VD->NestedName && validateVarName(VD->Ty, VD->NestedName))
    VD->NestedName = 0;
}


bool TypeChecker::typeCheckValueDecl(ValueDecl *VD) {
  if (validateType(VD)) {
    VD->Init = 0;
    return true;
  }

  // Validate that the initializers type matches the expected type.
  if (VD->Init == 0) {
    // If we have no initializer and the type is dependent, then the initializer
    // was invalid and removed.
    if (VD->Ty->is<DependentType>()) return true;
  } else if (VD->Ty->is<DependentType>()) {
    checkBody(VD->Init, 0);
    if (VD->Init == 0)
      note(VD->getLocStart(),
           "while converting 'var' initializer to declared type");
    else
      VD->Ty = VD->Init->Ty;
  } else {
    // If both a type and an initializer are specified, make sure the
    // initializer's type agrees (or converts) to the redundant type.
    checkBody(VD->Init, VD->Ty);
    
    if (VD->Init == 0) {
      if (isa<VarDecl>(VD))
        note(VD->getLocStart(),
             "while converting 'var' initializer to declared type");
    }
  }
  
  validateAttributes(VD->Attrs, VD->Ty);
  return false;
}

/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnitDecl *TUD, ASTContext &Ctx) {
  TypeChecker TC(Ctx);
  
  // Type check the top-level BraceExpr.  This sorts out any top-level
  // expressions and recursively processes the rest of the translation unit.
  TC.typeCheckTranslationUnit(TUD);
}
