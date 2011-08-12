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

#include "swift/AST/Types.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

// Only allow allocation of Stmts using the allocator in ASTContext.
void *TypeBase::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}

//===----------------------------------------------------------------------===//
// Various Type Methods.
//===----------------------------------------------------------------------===//

/// isEqual - Return true if these two types are equal, ignoring sugar.
bool TypeBase::isEqual(Type Other, ASTContext &Ctx) {
  return getCanonicalType(Ctx) == Other.getPointer()->getCanonicalType(Ctx);
}



/// getCanonicalType - Return the canonical version of this type, which has
/// sugar from all levels stripped off.
TypeBase *TypeBase::getCanonicalType(ASTContext &Ctx) {
  assert(this != 0 &&
         "Cannot call getCanonicalType before name binding is complete");

  // If the type is itself canonical or if the canonical type was already
  // computed, just return what we have.
  if (CanonicalType)
    return CanonicalType;
  
  TypeBase *Result = 0;
  switch (Kind) {
  case TypeKind::BuiltinInt1:
  case TypeKind::BuiltinInt8:
  case TypeKind::BuiltinInt16:
  case TypeKind::BuiltinInt32:
  case TypeKind::BuiltinInt64:
  case TypeKind::Dependent:
  case TypeKind::OneOf:
    assert(0 && "These are always canonical");
  case TypeKind::NameAlias:
    Result = cast<NameAliasType>(this)->
                  getDesugaredType()->getCanonicalType(Ctx);
    break;
  case TypeKind::Tuple: {
    SmallVector<TupleTypeElt, 8> CanElts;
    TupleType *TT = cast<TupleType>(this);
    CanElts.resize(TT->Fields.size());
    for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
      CanElts[i].Name = TT->Fields[i].Name;
      assert(!TT->Fields[i].Ty.isNull() &&
             "Cannot get canonical type of TypeChecked TupleType!");
      CanElts[i].Ty = TT->Fields[i].Ty->getCanonicalType(Ctx);
    }
    
    Result = TupleType::get(CanElts, Ctx);
    break;
  }
    
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(this);
    Type In = FT->Input->getCanonicalType(Ctx);
    Type Out = FT->Result->getCanonicalType(Ctx);
    Result = FunctionType::get(In, Out, Ctx);
    break;
  }
  case TypeKind::Array:
    ArrayType *AT = cast<ArrayType>(this);
    Type EltTy = AT->Base->getCanonicalType(Ctx);
    Result = ArrayType::get(EltTy, AT->Size, Ctx);
    break;
  }
  assert(Result && "Case not implemented!");
  return Result;
}


TypeBase *TypeBase::getDesugaredType() {
  switch (Kind) {
  case TypeKind::Dependent:
  case TypeKind::BuiltinInt1:
  case TypeKind::BuiltinInt8:
  case TypeKind::BuiltinInt16:
  case TypeKind::BuiltinInt32:
  case TypeKind::BuiltinInt64:
  case TypeKind::OneOf:
  case TypeKind::Tuple:
  case TypeKind::Function:
  case TypeKind::Array:
    // None of these types have sugar at the outer level.
    return this;
  case TypeKind::NameAlias:
    return cast<NameAliasType>(this)->TheDecl->UnderlyingTy->getDesugaredType();
  }

  assert(0 && "Unknown type kind");
  return 0;
}

/// hasAnyDefaultValues - Return true if any of our elements has a default
/// value.
bool TupleType::hasAnyDefaultValues() const {
  for (const TupleTypeElt &Elt : Fields)
    if (Elt.Init)
      return true;
  return false;
}

/// getNamedElementId - If this tuple has a field with the specified name,
/// return the field index, otherwise return -1.
int TupleType::getNamedElementId(Identifier I) const {
  for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
    if (Fields[i].Name == I)
      return i;
  }

  // Otherwise, name not found.
  return -1;
}

/// getFieldForScalarInit - If a tuple of this type can be initialized with a
/// scalar, return the field number that the scalar is assigned to.  If not,
/// return -1.
int TupleType::getFieldForScalarInit() const {
  if (Fields.empty()) return -1;
  
  int FieldWithoutDefault = -1;
  for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
    // Ignore fields with a default value.
    if (Fields[i].Init) continue;
    
    // If we already saw a field missing a default value, then we cannot assign
    // a scalar to this tuple.
    if (FieldWithoutDefault != -1)
      return -1;
    
    // Otherwise, remember this field number.
    FieldWithoutDefault = i;    
  }
  
  // If all the elements have default values, the scalar initializes the first
  // value in the tuple.
  return FieldWithoutDefault == -1 ? 0 : FieldWithoutDefault;
}


/// updateInitializedElementType - This methods updates the element type and
/// initializer for a non-canonical TupleType that has an initializer for the
/// specified element.  This should only be used by TypeChecker.
void TupleType::updateInitializedElementType(unsigned EltNo, Type NewTy,
                                             Expr *NewInit) {
  assert(!hasCanonicalTypeComputed() &&
         "Cannot munge an already canonicalized type!");
  TupleTypeElt &Elt = const_cast<TupleTypeElt&>(Fields[EltNo]);
  assert(Elt.Init && "Can only update elements with default values");
  Elt.Ty = NewTy;
  Elt.Init = NewInit;
}

OneOfElementDecl *OneOfType::getElement(Identifier Name) const {
  // FIXME: Linear search is not great for large oneof decls.
  for (OneOfElementDecl *Elt : Elements)
    if (Elt->Name == Name)
      return Elt;
  return 0;
}

/// hasSingleElement - Return true if this is a single element oneof that has
/// an argument type.  These are typically (but not necessarily) made with
/// 'struct'.  Since it is unambiguous which slice is being referenced,
/// various syntactic forms are allowed for these, like direct "foo.x" syntax.
bool OneOfType::hasSingleElement() const {
  return Elements.size() == 1 && !Elements[0]->ArgumentType.isNull();
}

//===----------------------------------------------------------------------===//
//  Type Printing
//===----------------------------------------------------------------------===//

void Type::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void Type::print(raw_ostream &OS) const {
  if (isNull())
    OS << "<null>";
  else
    Ptr->print(OS);
}

/// getString - Return the name of the type as a string, for use in
/// diagnostics only.
std::string Type::getString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS);
  return OS.str();
}


/// getString - Return the name of the type as a string, for use in
/// diagnostics only.
std::string TypeBase::getString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS);
  return OS.str();
}


void TypeBase::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void TypeBase::print(raw_ostream &OS) const {
  switch (Kind) {
  case TypeKind::Dependent:     return cast<DependentType>(this)->print(OS);
  case TypeKind::BuiltinInt1:
  case TypeKind::BuiltinInt8:
  case TypeKind::BuiltinInt16:
  case TypeKind::BuiltinInt32:
  case TypeKind::BuiltinInt64:  return cast<BuiltinType>(this)->print(OS);
  case TypeKind::NameAlias:     return cast<NameAliasType>(this)->print(OS);
  case TypeKind::OneOf:         return cast<OneOfType>(this)->print(OS);
  case TypeKind::Tuple:         return cast<TupleType>(this)->print(OS);
  case TypeKind::Function:      return cast<FunctionType>(this)->print(OS);
  case TypeKind::Array:         return cast<ArrayType>(this)->print(OS);
  }
}

void BuiltinType::print(raw_ostream &OS) const {
  switch (Kind) {
  default: assert(0 && "Unknown builtin type");
  case TypeKind::BuiltinInt1:  OS << "__builtin_int1_type"; break;
  case TypeKind::BuiltinInt8:  OS << "__builtin_int8_type"; break;
  case TypeKind::BuiltinInt16: OS << "__builtin_int16_type"; break;
  case TypeKind::BuiltinInt32: OS << "__builtin_int32_type"; break;
  case TypeKind::BuiltinInt64: OS << "__builtin_int64_type"; break;
  }
}

void DependentType::print(raw_ostream &OS) const {
  OS << "<<dependent type>>";
}

void NameAliasType::print(raw_ostream &OS) const {
  OS << TheDecl->Name.get();
}

void OneOfType::print(raw_ostream &OS) const {
  OS << "oneof { ";
    
  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    if (i) OS << ", ";
    OS << Elements[i]->Name;
    if (!Elements[i]->ArgumentType.isNull())
      OS << " : " << Elements[i]->ArgumentType;
  }
  
  OS << '}';
}

void TupleType::print(raw_ostream &OS) const {
  OS << "(";
  
  for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
    if (i) OS << ", ";
    const TupleTypeElt &TD = Fields[i];
    
    if (!TD.Name.empty())
      OS << TD.Name << ' ';
    
    OS << ": " << TD.Ty;
  }
  OS << ')';
}

void FunctionType::print(raw_ostream &OS) const {
  OS << Input << " -> " << Result;
}

void ArrayType::print(raw_ostream &OS) const {
  OS << Base << '[';
  if (Size)
    OS << Size;
  OS << ']';
}
