//===--- Builtins.cpp - Swift Language Builtin ASTs -----------------------===//
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
//  This file implements the interface to the Builtin APIs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Builtins.h"
#include "swift/AST/AST.h"
#include "llvm/ADT/StringSwitch.h"
using namespace swift;

Type swift::getBuiltinType(ASTContext &Context, StringRef Name) {
  if (Name == "float32")
    return Context.TheFloat32Type;
  if (Name == "float64")
    return Context.TheFloat64Type;

  // Handle 'int8' and friends.
  if (Name.substr(0, 3) == "int") {
    unsigned BitWidth;
    if (!Name.substr(3).getAsInteger(10, BitWidth) &&
        BitWidth <= 1024)  // Cap to prevent insane things.
      return BuiltinIntegerType::get(BitWidth, Context);
  }
  
  return Type();
}

BuiltinValueKind swift::isBuiltinValue(ASTContext &C, StringRef Name, Type &Ty){
  // builtin-id ::= operation-id '_' type-id
  // This will almost certainly get more sophisticated.
  StringRef::size_type Underscore = Name.find_last_of('_');
  if (Underscore == StringRef::npos) return BuiltinValueKind::None;

  // Check that the type parameter is well-formed and set it up for returning.
  Ty = getBuiltinType(C, Name.substr(Underscore + 1));
  if (Ty.isNull())
    return BuiltinValueKind::None;

  // Check that the operation name is well-formed.
  StringRef OperationName = Name.substr(0, Underscore);

  return llvm::StringSwitch<BuiltinValueKind>(OperationName)
#define BUILTIN(id, name) \
    .Case(name, BuiltinValueKind::id)
#include "swift/AST/Builtins.def"
    .Default(BuiltinValueKind::None);
}

/// Build a builtin function declarations.
static FuncDecl *getBuiltinFunction(ASTContext &Context, Identifier Id, Type T){
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), Id, T,
                                /*init*/ nullptr, Context.TheBuiltinModule);
}

/// Build a unary operation declaration.
static ValueDecl *getUnaryOperation(ASTContext &Context, Identifier Id,
                                    Type ArgType) {
  Type FnTy = FunctionType::get(ArgType, ArgType, Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

/// Build a binary operation declaration.
static ValueDecl *getBinaryOperation(ASTContext &Context, Identifier Id,
                                     Type ArgType) {
  TupleTypeElt ArgElts[] = { TupleTypeElt(ArgType), TupleTypeElt(ArgType) };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = FunctionType::get(Arg, ArgType, Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

/// Build a binary predicate declaration.
static ValueDecl *getBinaryPredicate(ASTContext &Context, Identifier Id,
                                     Type ArgType) {
  TupleTypeElt ArgElts[] = { TupleTypeElt(ArgType), TupleTypeElt(ArgType) };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = FunctionType::get(Arg, BuiltinIntegerType::get(1, Context),
                                Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

/// An array of the overloaded builtin kinds.
static const OverloadedBuiltinKind OverloadedBuiltinKinds[] = {
  OverloadedBuiltinKind::None,

// There's deliberately no BUILTIN clause here so that we'll blow up
// if new builtin categories are added there and not here.
#define BUILTIN_UNARY_OPERATION(id, name, overload) overload,
#define BUILTIN_BINARY_OPERATION(id, name, overload) overload,
#define BUILTIN_BINARY_PREDICATE(id, name, overload) overload,
#include "swift/AST/Builtins.def"
};

/// Determines if a builtin type falls within the given category.
/// The category cannot be OverloadedBuiltinKind::None.
inline bool isBuiltinTypeOverloaded(Type T, OverloadedBuiltinKind OK) {
  switch (OK) {
  case OverloadedBuiltinKind::None:
    break; // invalid 
  case OverloadedBuiltinKind::Integer:
    return T->is<BuiltinIntegerType>();
  case OverloadedBuiltinKind::Float:
    return T->Kind == TypeKind::BuiltinFloat32 ||
           T->Kind == TypeKind::BuiltinFloat64;
  case OverloadedBuiltinKind::Arithmetic:
    return true;
  }
  llvm_unreachable("bad overloaded builtin kind");
}


ValueDecl *swift::getBuiltinValue(ASTContext &Context, Identifier Id) {
  Type ArgType;
  BuiltinValueKind BV = isBuiltinValue(Context, Id.str(), ArgType);

  // Filter out inappropriate overloads.
  OverloadedBuiltinKind OBK = OverloadedBuiltinKinds[unsigned(BV)];
  if (OBK != OverloadedBuiltinKind::None &&
      !isBuiltinTypeOverloaded(ArgType, OBK))
    return nullptr;

  switch (BV) {
  case BuiltinValueKind::None: return nullptr;

#define BUILTIN(id, name)
#define BUILTIN_UNARY_OPERATION(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return getUnaryOperation(Context, Id, ArgType);

#define BUILTIN(id, name)
#define BUILTIN_BINARY_OPERATION(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return getBinaryOperation(Context, Id, ArgType);

#define BUILTIN(id, name)
#define BUILTIN_BINARY_PREDICATE(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return getBinaryPredicate(Context, Id, ArgType);
  }
  llvm_unreachable("bad builtin value!");
}
