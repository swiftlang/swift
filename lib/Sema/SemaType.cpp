//===--- SemaType.cpp - Swift Semantic Analysis for Types -----------------===//
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
//  This file implements semantic analysis for Swift types.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/SemaType.h"
#include "swift/Sema/Sema.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;

Type *SemaType::ActOnInt32Type(llvm::SMLoc Loc) {
  return S.Context.TheInt32Type;
}

Type *SemaType::ActOnTypeName(llvm::SMLoc Loc, Identifier Name) {
  return S.decl.LookupTypeName(Name, Loc)->getAliasType(S.Context);
}


Type *SemaType::ActOnTupleType(llvm::SMLoc LPLoc, TupleTypeElt *Elements,
                               unsigned NumElements, llvm::SMLoc RPLoc) {
  // Validate the elements.
  for (unsigned i = 0; i != NumElements; ++i) {
    Type *&Ty = Elements[i].Ty;
    Expr *&Init = Elements[i].Init;
    assert((Ty != 0 || Init != 0) && "Must have a type or an expr already");
    
    // If a type isn't specified, get the element type from the initializer.
    if (Ty == 0)
      Ty = Init->Ty;
    else if (Init) {
      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      Expr *InitE = S.expr.ConvertToType(Init, Ty, false,
                                         SemaExpr::CR_TupleInit);
      if (InitE)
        Init = InitE;
      else
        Ty = Init->Ty;
    }
  }
  
  return S.Context.getTupleType(llvm::ArrayRef<TupleTypeElt>(Elements,
                                                             NumElements));
}

OneOfType *SemaType::ActOnOneOfType(llvm::SMLoc OneOfLoc, 
                                    const DeclAttributes &Attrs,
                                    llvm::ArrayRef<OneOfElementInfo> Elts) {
  // No attributes are valid on oneof types at this time.
  if (!Attrs.empty())
    Error(Attrs.LSquareLoc, "oneof types are not allowed to have attributes");
  
  llvm::SmallPtrSet<const char *, 16> SeenSoFar;
  llvm::SmallVector<OneOfElementDecl *, 16> EltDecls;
    
  Type *TmpTy = S.Context.TheEmptyTupleType;
  for (unsigned i = 0, e = Elts.size(); i != e; ++i) {
    Identifier NameI = S.Context.getIdentifier(Elts[i].Name);
    
    // If this was multiply defined, reject it.
    if (!SeenSoFar.insert(NameI.get())) {
      Error(Elts[i].NameLoc, "element named '" + Elts[i].Name +
            "' defined multiple times");
      // Don't copy this element into NewElements.
      // TODO: QoI: add note for previous definition.
      continue;
    }

    // Create a decl for each element, giving each a temporary type.
    EltDecls.push_back(
      new (S.Context) OneOfElementDecl(Elts[i].NameLoc, NameI, TmpTy,
                                       Elts[i].EltType));
  }
  
  OneOfType *Result = S.Context.getNewOneOfType(OneOfLoc, EltDecls);
  
  // Now that the oneof type is created, we can go back and give proper types to
  // each element decl.
  for (unsigned i = 0, e = EltDecls.size(); i != e; ++i) {
    Type *EltTy = Result;
    // If the OneOf Element takes a type argument, then it is actually a
    // function that takes the type argument and returns the OneOfType.
    if (Type *ArgTy = EltDecls[i]->ArgumentType)
      EltTy = S.Context.getFunctionType(ArgTy, EltTy);
    EltDecls[i]->Ty = EltTy;
  }
  
  return Result;
}


Type *SemaType::ActOnFunctionType(Type *Input, llvm::SMLoc ArrowLoc,
                                  Type *Output) {
  return S.Context.getFunctionType(Input, Output);
}

Type *SemaType::ActOnArrayType(Type *BaseTy, llvm::SMLoc LSquareLoc, Expr *Size,
                               llvm::SMLoc RSquareLoc) {
  // Unsized arrays.
  if (Size == 0)
    return S.Context.getArrayType(BaseTy, 0);
  
  // FIXME: Add real support for evaluating constant expressions for array
  // sizes.
  uint64_t SizeVal;
  if (IntegerLiteral *IL = llvm::dyn_cast<IntegerLiteral>(Size))
    SizeVal = IL->getValue();
  else {
    Error(Size->getLocStart(), "invalid type size, not a constant");
    SizeVal = 1;
  }
  
  if (SizeVal == 0) {
    Error(Size->getLocStart(), "array types must be larger than zero elements");
    SizeVal = 1;
  }
  
  return S.Context.getArrayType(BaseTy, SizeVal);
}
