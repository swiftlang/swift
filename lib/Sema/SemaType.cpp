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

Type *SemaType::ActOnInt32Type(llvm::SMLoc Loc) {
  return S.Context.TheInt32Type;
}

Type *SemaType::ActOnTypeName(llvm::SMLoc Loc, llvm::StringRef Name) {
  return S.Context.getNamedType(S.Context.getIdentifier(Name));
}


Type *SemaType::ActOnTupleType(llvm::SMLoc LPLoc, TupleTypeElt const *Elements,
                               unsigned NumElements, llvm::SMLoc RPLoc) {
  return S.Context.getTupleType(Elements, NumElements);
}

Type *SemaType::ActOnFunctionType(Type *Input, llvm::SMLoc ArrowLoc,
                                  Type *Output) {
  return S.Context.getFunctionType(Input, Output);
}

Type *SemaType::ActOnTypeAlias(llvm::SMLoc TypeAliasLoc, llvm::StringRef Name,
                               Type *Ty) {
  return S.Context.getAliasType(S.Context.getIdentifier(Name), Ty);
}
