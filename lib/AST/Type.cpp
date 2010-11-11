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

Type *Type::getDesugaredType() {
  switch (Kind) {
  case DependentTypeKind:
  case BuiltinInt32Kind:
  case OneOfTypeKind:
  case TupleTypeKind:
  case FunctionTypeKind:
  case ArrayTypeKind:
    // None of these types have sugar at the outer level.
    return this;
  case AliasTypeKind:
    return cast<AliasType>(this)->UnderlyingType->getDesugaredType();
  }

  assert(0 && "Unknown type kind");
  return 0;
}

/// getNamedElementId - If this tuple has a field with the specified name,
/// return the field index, otherwise return -1.
int TupleType::getNamedElementId(Identifier I) const {
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    if (Fields[i].Name == I)
      return i;
  }

  // Otherwise, name not found.
  return -1;
}


//===----------------------------------------------------------------------===//
//  Type printing.
//===----------------------------------------------------------------------===//


void Type::dump() const {
  llvm::errs() << *this << '\n';
}

void Type::print(llvm::raw_ostream &OS) const {
  switch (Kind) {
  case DependentTypeKind:     return cast<DependentType>(this)->print(OS);
  case BuiltinInt32Kind:      return cast<BuiltinType>(this)->print(OS);
  case AliasTypeKind:         return cast<AliasType>(this)->print(OS);
  case OneOfTypeKind:          return cast<OneOfType>(this)->print(OS);
  case TupleTypeKind:         return cast<TupleType>(this)->print(OS);
  case FunctionTypeKind:      return cast<FunctionType>(this)->print(OS);
  case ArrayTypeKind:         return cast<ArrayType>(this)->print(OS);
  }
}

void BuiltinType::print(llvm::raw_ostream &OS) const {
  switch (Kind) {
  default: assert(0 && "Unknown builtin type");
  case BuiltinInt32Kind: OS << "__builtin_int32_type"; break;
  }
}

void DependentType::print(llvm::raw_ostream &OS) const {
  OS << "<<dependent type>>";
}

void AliasType::print(llvm::raw_ostream &OS) const {
  OS << Name.get();
}

void OneOfType::print(llvm::raw_ostream &OS) const {
  OS << TheDecl->Name.get();
}

void TupleType::print(llvm::raw_ostream &OS) const {
  OS << "(";
  
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    if (i) OS << ", ";
    const TupleTypeElt &TD = Fields[i];
    
    if (TD.Name.get() == 0) {
      OS << *TD.Ty;
      continue;
    }
    
    OS << "var " << TD.Name << " : " << *TD.Ty;
  }
  OS << ")";
}

void FunctionType::print(llvm::raw_ostream &OS) const {
  OS << *Input << " -> " << *Result;
}

void ArrayType::print(llvm::raw_ostream &OS) const {
  OS << *Base << '[';
  if (Size)
    OS << Size;
  OS << ']';
}
