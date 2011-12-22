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
#include "llvm/ADT/APFloat.h"
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
bool TypeBase::isEqual(Type Other) {
  return getCanonicalType() == Other.getPointer()->getCanonicalType();
}

/// getCanonicalType - Return the canonical version of this type, which has
/// sugar from all levels stripped off.
TypeBase *TypeBase::getCanonicalType() {
  assert(this != 0 &&
         "Cannot call getCanonicalType before name binding is complete");

  // If the type is itself canonical, return it.
  if (CanonicalType.is<ASTContext*>())
    return this;
  // If the canonical type was already computed, just return what we have.
  if (TypeBase *CT = CanonicalType.get<TypeBase*>())
    return CT;
  
  // Otherwise, compute and cache it.
  TypeBase *Result = 0;
  switch (Kind) {
  case TypeKind::Error:
  case TypeKind::BuiltinFloatingPoint:
  case TypeKind::BuiltinInteger:
  case TypeKind::Dependent:
  case TypeKind::OneOf:
  case TypeKind::Protocol:
    assert(0 && "These are always canonical");
  case TypeKind::NameAlias:
    Result = cast<NameAliasType>(this)->
                  getDesugaredType()->getCanonicalType();
    break;
  case TypeKind::Tuple: {
    SmallVector<TupleTypeElt, 8> CanElts;
    TupleType *TT = cast<TupleType>(this);
    CanElts.resize(TT->Fields.size());
    for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
      CanElts[i].Name = TT->Fields[i].Name;
      assert(!TT->Fields[i].Ty.isNull() &&
             "Cannot get canonical type of TypeChecked TupleType!");
      CanElts[i].Ty = TT->Fields[i].Ty->getCanonicalType();
    }
    
    assert(!TT->Fields.empty() && "Empty tuples are always canonical");
    Result = TupleType::get(CanElts, CanElts[0].Ty->getASTContext());
    break;
  }
    
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(this);
    Type In = FT->Input->getCanonicalType();
    Type Out = FT->Result->getCanonicalType();
    Result = FunctionType::get(In, Out, In->getASTContext());
    break;
  }
  case TypeKind::Array:
    ArrayType *AT = cast<ArrayType>(this);
    Type EltTy = AT->Base->getCanonicalType();
    Result = ArrayType::get(EltTy, AT->Size, EltTy->getASTContext());
    break;
  }
  
  // Cache the canonical type for future queries.
  assert(Result && "Case not implemented!");
  CanonicalType = Result;
  return Result;
}


TypeBase *TypeBase::getDesugaredType() {
  switch (Kind) {
  case TypeKind::Error:
  case TypeKind::Dependent:
  case TypeKind::BuiltinFloatingPoint:
  case TypeKind::BuiltinInteger:
  case TypeKind::OneOf:
  case TypeKind::Tuple:
  case TypeKind::Function:
  case TypeKind::Array:
  case TypeKind::Protocol:
    // None of these types have sugar at the outer level.
    return this;
  case TypeKind::NameAlias:
    return cast<NameAliasType>(this)->TheDecl->getUnderlyingType()
             ->getDesugaredType();
  }

  assert(0 && "Unknown type kind");
  return 0;
}

const llvm::fltSemantics &BuiltinFloatingPointType::getAPFloatSemantics() const{
  switch (getFPKind()) {
    case BuiltinFloatingPointType::IEEE16:  return APFloat::IEEEhalf;
    case BuiltinFloatingPointType::IEEE32:  return APFloat::IEEEsingle;
    case BuiltinFloatingPointType::IEEE64:  return APFloat::IEEEdouble;
    case BuiltinFloatingPointType::IEEE80:  return APFloat::x87DoubleExtended;
    case BuiltinFloatingPointType::IEEE128: return APFloat::IEEEquad;
    case BuiltinFloatingPointType::PPC128:  return APFloat::PPCDoubleDouble;
  }
  assert(0 && "Unknown FP semantics");
  return APFloat::IEEEhalf;
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
    if (Elt->getName() == Name)
      return Elt;
  return 0;
}

bool OneOfType::isTransparentType() const {
  return Elements.size() == 1 && !Elements[0]->getArgumentType().isNull();
}

Type OneOfType::getTransparentType() const {
  assert(Elements.size() == 1);
  assert(!Elements[0]->getArgumentType().isNull());
  return Elements[0]->getArgumentType();
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
  case TypeKind::Error:         return cast<ErrorType>(this)->print(OS);
  case TypeKind::Dependent:     return cast<DependentType>(this)->print(OS);
  case TypeKind::BuiltinFloatingPoint:
    return cast<BuiltinFloatingPointType>(this)->print(OS);
  case TypeKind::BuiltinInteger:
    return cast<BuiltinIntegerType>(this)->print(OS);
  case TypeKind::NameAlias:     return cast<NameAliasType>(this)->print(OS);
  case TypeKind::OneOf:         return cast<OneOfType>(this)->print(OS);
  case TypeKind::Tuple:         return cast<TupleType>(this)->print(OS);
  case TypeKind::Function:      return cast<FunctionType>(this)->print(OS);
  case TypeKind::Array:         return cast<ArrayType>(this)->print(OS);
  case TypeKind::Protocol:      return cast<ProtocolType>(this)->print(OS);
  }
}

void BuiltinIntegerType::print(raw_ostream &OS) const {
  OS << "Builtin::int" << cast<BuiltinIntegerType>(this)->getBitWidth();
}

void BuiltinFloatingPointType::print(raw_ostream &OS) const {
  switch (Kind) {
  case IEEE16:  OS << "Builtin::FP_IEEE16"; return;
  case IEEE32:  OS << "Builtin::FP_IEEE32"; return;
  case IEEE64:  OS << "Builtin::FP_IEEE64"; return;
  case IEEE80:  OS << "Builtin::FP_IEEE80"; return;
  case IEEE128: OS << "Builtin::FP_IEEE128"; return;
  case PPC128:  OS << "Builtin::FP_PPC128"; return;
  }
}

void ErrorType::print(raw_ostream &OS) const {
  OS << "<<error type>>";
}

void DependentType::print(raw_ostream &OS) const {
  OS << "<<dependent type>>";
}

void NameAliasType::print(raw_ostream &OS) const {
  OS << TheDecl->getName().get();
}

void OneOfType::print(raw_ostream &OS) const {
  OS << "oneof { ";
    
  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    if (i) OS << ", ";
    OS << Elements[i]->getName();
    if (!Elements[i]->getArgumentType().isNull())
      OS << " : " << Elements[i]->getArgumentType();
  }
  
  OS << '}';
}

void TupleType::print(raw_ostream &OS) const {
  OS << "(";
  
  for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
    if (i) OS << ", ";
    const TupleTypeElt &TD = Fields[i];
    
    if (!TD.Name.empty())
      OS << TD.Name << " : ";
    
    OS << TD.Ty;
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

void ProtocolType::print(raw_ostream &OS) const {
  OS << "protocol {";
  for (Decl *D : Elements)
    D->print(OS);
  OS << '}';
}

