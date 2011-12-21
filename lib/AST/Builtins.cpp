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

BuiltinTypeKind swift::isBuiltinType(StringRef Name) {
  return llvm::StringSwitch<BuiltinTypeKind>(Name)
    .Case("int1",    BuiltinTypeKind::int1)
    .Case("int8",    BuiltinTypeKind::int8)
    .Case("int16",   BuiltinTypeKind::int16)
    .Case("int32",   BuiltinTypeKind::int32)
    .Case("int64",   BuiltinTypeKind::int64)
    .Case("float32", BuiltinTypeKind::float32)
    .Case("float64", BuiltinTypeKind::float64)
    .Default(BuiltinTypeKind::None);
}

Type swift::getBuiltinType(ASTContext &Context, BuiltinTypeKind T) {
  switch (T) {
  case BuiltinTypeKind::None:    return Type();
  case BuiltinTypeKind::int1:    return Context.TheInt1Type;
  case BuiltinTypeKind::int8:    return Context.TheInt8Type;
  case BuiltinTypeKind::int16:   return Context.TheInt16Type;
  case BuiltinTypeKind::int32:   return Context.TheInt32Type;
  case BuiltinTypeKind::int64:   return Context.TheInt64Type;
  case BuiltinTypeKind::float32: return Context.TheFloat32Type;
  case BuiltinTypeKind::float64: return Context.TheFloat64Type;
  }
}

Type swift::getBuiltinType(ASTContext &Context, Identifier Id) {
  return getBuiltinType(Context, isBuiltinType(Id.str()));
}

BuiltinValueKind swift::isBuiltinValue(StringRef Name, BuiltinTypeKind &Parm) {
  // builtin-id ::= operation-id '_' type-id
  // This will almost certainly get more sophisticated.
  StringRef::size_type Underscore = Name.find_last_of('_');
  if (Underscore == StringRef::npos) return BuiltinValueKind::None;

  // Check that the type parameter is well-formed.
  Parm = isBuiltinType(Name.substr(Underscore + 1));
  if (Parm == BuiltinTypeKind::None) return BuiltinValueKind::None;

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
                                    BuiltinValueKind BV, BuiltinTypeKind BT) {
  Type T = getBuiltinType(Context, BT);
  Type FnTy = FunctionType::get(T, T, Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

/// Build a binary operation declaration.
static ValueDecl *getBinaryOperation(ASTContext &Context, Identifier Id,
                                     BuiltinValueKind BV, BuiltinTypeKind BT) {
  Type T = getBuiltinType(Context, BT);

  TupleTypeElt ArgElts[] = { TupleTypeElt(T), TupleTypeElt(T) };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = FunctionType::get(Arg, T, Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

/// Build a binary predicate declaration.
static ValueDecl *getBinaryPredicate(ASTContext &Context, Identifier Id,
                                     BuiltinValueKind BV, BuiltinTypeKind BT) {
  Type T = getBuiltinType(Context, BT);

  TupleTypeElt ArgElts[] = { TupleTypeElt(T), TupleTypeElt(T) };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = FunctionType::get(Arg, Context.TheInt1Type, Context);
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

ValueDecl *swift::getBuiltinValue(ASTContext &Context, Identifier Id) {
  BuiltinTypeKind BT;
  BuiltinValueKind BV = isBuiltinValue(Id.str(), BT);

  // Filter out inappropriate overloads.
  OverloadedBuiltinKind OBK = OverloadedBuiltinKinds[unsigned(BV)];
  if (OBK != OverloadedBuiltinKind::None &&
      !isBuiltinTypeOverloaded(BT, OBK))
    return nullptr;

  switch (BV) {
  case BuiltinValueKind::None: return nullptr;

#define BUILTIN(id, name)
#define BUILTIN_UNARY_OPERATION(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return getUnaryOperation(Context, Id, BV, BT);

#define BUILTIN(id, name)
#define BUILTIN_BINARY_OPERATION(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return getBinaryOperation(Context, Id, BV, BT);

#define BUILTIN(id, name)
#define BUILTIN_BINARY_PREDICATE(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return getBinaryPredicate(Context, Id, BV, BT);
  }
  llvm_unreachable("bad builtin value!");
}
