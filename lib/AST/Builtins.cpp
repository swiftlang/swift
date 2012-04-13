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
  if (Name == "RawPointer")
    return Context.TheRawPointerType;
  if (Name == "ObjectPointer")
    return Context.TheObjectPointerType;
  
  if (Name == "FPIEEE32")
    return Context.TheIEEE32Type;
  if (Name == "FPIEEE64")
    return Context.TheIEEE64Type;

  // Handle 'int8' and friends.
  if (Name.substr(0, 3) == "Int") {
    unsigned BitWidth;
    if (!Name.substr(3).getAsInteger(10, BitWidth) &&
        BitWidth <= 1024 && BitWidth != 0)  // Cap to prevent insane things.
      return BuiltinIntegerType::get(BitWidth, Context);
  }
  
  // Target specific FP types.
  if (Name == "FPIEEE16")
    return Context.TheIEEE16Type;
  if (Name == "FPIEEE80")
    return Context.TheIEEE80Type;
  if (Name == "FPIEEE128")
    return Context.TheIEEE128Type;
  if (Name == "FPPPC128")
    return Context.ThePPC128Type;

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
                                /*init*/ nullptr, Context.TheBuiltinModule,
                                /*IsModuleScope*/true);
}

/// Build a getelementptr operation declaration.
static ValueDecl *getGepOperation(ASTContext &Context, Identifier Id,
                                  Type ArgType) {
  // This is always "(i8*, IntTy) -> i8*"
  TupleTypeElt ArgElts[] = {
    TupleTypeElt(Context.TheRawPointerType, Identifier()),
    TupleTypeElt(ArgType, Identifier())
  };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = FunctionType::get(Arg, Context.TheRawPointerType, Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

/// Build a binary operation declaration.
static ValueDecl *getBinaryOperation(ASTContext &Context, Identifier Id,
                                     Type ArgType) {
  TupleTypeElt ArgElts[] = {
    TupleTypeElt(ArgType, Identifier()),
    TupleTypeElt(ArgType, Identifier())
  };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = FunctionType::get(Arg, ArgType, Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

/// Build a binary predicate declaration.
static ValueDecl *getBinaryPredicate(ASTContext &Context, Identifier Id,
                                     Type ArgType) {
  TupleTypeElt ArgElts[] = {
    TupleTypeElt(ArgType, Identifier()),
    TupleTypeElt(ArgType, Identifier())
  };
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
#define BUILTIN_GEP_OPERATION(id, name, overload) overload,
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
    return T->is<BuiltinFloatType>();
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
#define BUILTIN_GEP_OPERATION(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return getGepOperation(Context, Id, ArgType);

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
