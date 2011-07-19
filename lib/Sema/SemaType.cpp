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
#include "swift/AST/Types.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;

Type SemaType::ActOnTypeName(SMLoc Loc, Identifier Name) {
  return S.decl.LookupTypeName(Name, Loc)->getAliasType(S.Context);
}


Type SemaType::ActOnTupleType(SMLoc LPLoc, 
                               llvm::ArrayRef<TupleTypeElt> Elements,
                               SMLoc RPLoc) {
  return S.Context.getTupleType(Elements);
}

OneOfType *SemaType::ActOnOneOfType(SMLoc OneOfLoc, 
                                    const DeclAttributes &Attrs,
                                    llvm::ArrayRef<OneOfElementInfo> Elts,
                                    TypeAliasDecl *PrettyTypeName) {
  // No attributes are valid on oneof types at this time.
  if (!Attrs.empty())
    error(Attrs.LSquareLoc, "oneof types are not allowed to have attributes");
  
  llvm::SmallPtrSet<const char *, 16> SeenSoFar;
  llvm::SmallVector<OneOfElementDecl *, 16> EltDecls;
    
  // If we have a PrettyTypeName to use, use it.  Otherwise, just assign the
  // constructors a temporary dummy type.
  Type TmpTy = S.Context.TheEmptyTupleType;
  if (PrettyTypeName)
    TmpTy = PrettyTypeName->getAliasType(S.Context);
  
  for (unsigned i = 0, e = Elts.size(); i != e; ++i) {
    Identifier NameI = S.Context.getIdentifier(Elts[i].Name);
    
    // If this was multiply defined, reject it.
    if (!SeenSoFar.insert(NameI.get())) {
      error(Elts[i].NameLoc, "element named '" + Elts[i].Name +
            "' defined multiple times");
      // Don't copy this element into NewElements.
      // TODO: QoI: add note for previous definition.
      continue;
    }
    
    Type EltTy = TmpTy;
    if (Type ArgTy = Elts[i].EltType)
      if (PrettyTypeName)
        EltTy = S.Context.getFunctionType(ArgTy, EltTy);

    // Create a decl for each element, giving each a temporary type.
    EltDecls.push_back(
      new (S.Context) OneOfElementDecl(Elts[i].NameLoc, NameI, EltTy,
                                       Elts[i].EltType));
  }
  
  OneOfType *Result = S.Context.getNewOneOfType(OneOfLoc, EltDecls);

  if (PrettyTypeName) {
    // If we have a pretty name for this, complete it to its actual type.
    assert(llvm::isa<UnresolvedType>(PrettyTypeName->UnderlyingTy
                                                .getPointer()) &&
           "Not an incomplete decl to complete!");
    PrettyTypeName->UnderlyingTy = Result;
  } else {
    // Now that the oneof type is created, we can go back and give proper types
    // to each element decl.
    for (unsigned i = 0, e = EltDecls.size(); i != e; ++i) {
      Type EltTy = Result;
      // If the OneOf Element takes a type argument, then it is actually a
      // function that takes the type argument and returns the OneOfType.
      if (Type ArgTy = EltDecls[i]->ArgumentType)
        EltTy = S.Context.getFunctionType(ArgTy, EltTy);
      EltDecls[i]->Ty = EltTy;
    }
  }
  
  return Result;
}


Type SemaType::ActOnFunctionType(Type Input, SMLoc ArrowLoc, Type Output){
  return S.Context.getFunctionType(Input, Output);
}

Type SemaType::ActOnArrayType(Type BaseTy, SMLoc LSquareLoc, Expr *Size,
                              SMLoc RSquareLoc) {
  // Unsized arrays.
  if (Size == 0)
    return S.Context.getArrayType(BaseTy, 0);
  
  // FIXME: Add real support for evaluating constant expressions for array
  // sizes.
  uint64_t SizeVal;
  if (IntegerLiteral *IL = llvm::dyn_cast<IntegerLiteral>(Size))
    SizeVal = IL->getValue();
  else {
    error(Size->getLocStart(), "invalid type size, not a constant");
    SizeVal = 1;
  }
  
  if (SizeVal == 0) {
    error(Size->getLocStart(), "array types must be larger than zero elements");
    SizeVal = 1;
  }
  
  return S.Context.getArrayType(BaseTy, SizeVal);
}
