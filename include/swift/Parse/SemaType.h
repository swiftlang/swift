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

#include "swift/AST/Type.h"
#include "swift/Parse/SemaBase.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace swift {
  class Sema;
  class Decl;
  class Type;
  class Expr;
  class TupleType;
  class TupleTypeElt;
  class DeclAttributes;
  class OneOfType;
  class Identifier;
  class TypeAliasDecl;
  
/// SemaType - Semantic analysis support for Swift types.
class SemaType : public SemaBase {
public:
  explicit SemaType(Sema &S) : SemaBase(S) {}

  Type ActOnTypeName(SMLoc Loc, Identifier Name);
  Type ActOnTupleType(SMLoc LPLoc, ArrayRef<TupleTypeElt> Elements,
                      SMLoc RPLoc);
  Type ActOnFunctionType(Type Input, SMLoc ArrowLoc, Type Output);
  Type ActOnArrayType(Type BaseTy, SMLoc LSquareLoc, Expr *Size,
                       SMLoc RSquareLoc);
  
  struct OneOfElementInfo {
    SMLoc NameLoc;
    StringRef Name;
    Type EltType;
  };
  
  OneOfType *ActOnOneOfType(SMLoc OneOfLoc, const DeclAttributes &Attrs,
                            ArrayRef<OneOfElementInfo> Elts,
                            TypeAliasDecl *PrettyTypeName = 0);
};
  
} // end namespace swift

#endif
