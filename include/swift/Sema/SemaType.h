//===--- SemaType.h - Swift Semantic Analysis for Types ---------*- C++ -*-===//
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
// This file defines the Sema interface which implement hooks invoked by the 
// parser to build the AST for types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPE_H
#define SWIFT_SEMA_TYPE_H

#include "swift/Sema/SemaBase.h"

namespace llvm {
  class StringRef;
}

namespace swift {
  class Sema;
  class Decl;
  class NamedDecl;
  class Type;
  class TupleType;
  class TupleTypeElt;
  
/// SemaType - Semantic analysis support for Swift types.
class SemaType : public SemaBase {
public:
  explicit SemaType(Sema &S) : SemaBase(S) {}

  Type *ActOnInt32Type(llvm::SMLoc Loc);
  Type *ActOnTypeName(llvm::SMLoc Loc, llvm::StringRef Name);
  Type *ActOnTupleType(llvm::SMLoc LPLoc, const TupleTypeElt *Elements,
                       unsigned NumElements, llvm::SMLoc RPLoc);
  Type *ActOnFunctionType(Type *Input, llvm::SMLoc ArrowLoc, Type *Output);
  
  
  Type *ActOnTypeAlias(llvm::SMLoc TypeAliasLoc, llvm::StringRef Name,Type *Ty);
};
  
} // end namespace swift

#endif
