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
#include "llvm/Support/SMLoc.h"
using namespace swift;

Type *SemaType::ActOnIntType(llvm::SMLoc Loc) {
  return S.Context.IntType;
}
Type *SemaType::ActOnVoidType(llvm::SMLoc Loc) {
  return S.Context.VoidType;
}

Type *SemaType::ActOnTupleType(llvm::SMLoc LPLoc,
                          llvm::PointerUnion<Type*, NamedDecl*> const *Elements,
                               unsigned NumElements, llvm::SMLoc RPLoc) {
  // Verify that tuple elements don't have initializers.  In the future, we can
  // consider adding these back if we so desire.
  for (unsigned i = 0, e = NumElements; i != e; ++i) {
    if (NamedDecl *D = Elements[i].dyn_cast<NamedDecl*>()) {
      if (D->Init != 0) {
        Error(D->Init->getLocStart(),
              "tuple element should not have an initializer");
        D->Init = 0;
      }
    }
  }
  
  return S.Context.getTupleType(Elements, NumElements);
}

Type *SemaType::ActOnFunctionType(Type *Input, llvm::SMLoc ArrowLoc,
                                  Type *Output) {
  return S.Context.getFunctionType(Input, Output);
}
