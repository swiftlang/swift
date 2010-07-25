//===--- Type.cpp - Swift Language Type ASTs ------------------------------===//
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
//  This file implements the Type class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Type.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;
using llvm::cast;

// Only allow allocation of Stmts using the allocator in ASTContext.
void *Type::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}

//===----------------------------------------------------------------------===//
// Various Type Methods.
//===----------------------------------------------------------------------===//


/// getElementType - Return the type of the specified field, looking through
/// NamedDecls automatically.
Type *TupleType::getElementType(unsigned FieldNo) const {
  assert(FieldNo < NumFields && "Invalid field number");
  
  if (Type *T = Fields[FieldNo].dyn_cast<Type*>())
    return T;
  
  return Fields[FieldNo].get<NamedDecl*>()->Ty;
}


//===----------------------------------------------------------------------===//
//  Type printing.
//===----------------------------------------------------------------------===//


void Type::dump() const {
  llvm::errs() << *this << '\n';
}

void Type::print(llvm::raw_ostream &OS) const {
  switch (Kind) {
  case BuiltinIntKind:   return cast<BuiltinType>(this)->print(OS);
  case TupleTypeKind:    return cast<TupleType>(this)->print(OS);
  case FunctionTypeKind: return cast<FunctionType>(this)->print(OS);
  }
}

void BuiltinType::print(llvm::raw_ostream &OS) const {
  assert(Kind == BuiltinIntKind && "Only one builtin type!");
  OS << "int";
}

void TupleType::print(llvm::raw_ostream &OS) const {
  OS << "(";
  
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    if (i) OS << ", ";
    const TypeOrDecl &TD = Fields[i];
    
    if (Type *Ty = TD.dyn_cast<Type*>()) {
      OS << *Ty;
      continue;
    }
    
    NamedDecl *VD = TD.get<NamedDecl*>();
    assert(llvm::isa<VarDecl>(VD));
    OS << "var " << VD->Name << " : ";
    VD->Ty->print(OS);
    assert(VD->Init == 0 && "Don't handle inits");
  }
  OS << ")";
}

void FunctionType::print(llvm::raw_ostream &OS) const {
  OS << *Input << " -> " << *Result;
}

