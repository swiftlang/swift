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

/// getBuiltinBaseName - Decode the type list of a builtin (e.g. mul_Int32) and
/// return the base name (e.g. "mul").
StringRef swift::getBuiltinBaseName(ASTContext &C, StringRef Name,
                                    SmallVectorImpl<Type> &Types) {
  // builtin-id ::= operation-id ('_' type-id)*
  for (StringRef::size_type Underscore = Name.find_last_of('_');
       Underscore != StringRef::npos; Underscore = Name.find_last_of('_')) {
    
    // Check that the type parameter is well-formed and set it up for returning.
    // This allows operations with underscores in them, like "icmp_eq".
    Type Ty = getBuiltinType(C, Name.substr(Underscore + 1));
    if (Ty.isNull()) break;
    
    Types.push_back(Ty);
    
    Name = Name.substr(0, Underscore);
  }
  
  std::reverse(Types.begin(), Types.end());
  return Name;
}

namespace {
  /// BuiltinValueKind - The set of (possibly overloaded) builtin functions.
  enum class BuiltinValueKind {
    None,
    
#define BUILTIN(Id, Name) Id,
#include "swift/AST/Builtins.def"
  };
}


/// Build a builtin function declarations.
static FuncDecl *getBuiltinFunction(ASTContext &Context, Identifier Id, Type T){
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), Id, T,
                                /*init*/ nullptr, Context.TheBuiltinModule);
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

/// Build a cast.  There is some custom type checking here.
static ValueDecl *getCastOperation(ASTContext &Context, Identifier Id,
                                   BuiltinValueKind VK,
                                   ArrayRef<Type> Types) {
  if (Types.size() == 0 || Types.size() > 2) return nullptr;
  Type Input = Types[0];
  Type Output = Types.size() == 2 ? Types[1] : Type();
  
  // Custom type checking.  We know the one or two types have been subjected to
  // the "isBuiltinTypeOverloaded" predicate successfully.
  switch (VK) {
  default: assert(0 && "Not a cast operation");
  case BuiltinValueKind::Trunc:
    if (Output.isNull() ||
        !Input->is<BuiltinIntegerType>() || !Output->is<BuiltinIntegerType>() ||
        Input->castTo<BuiltinIntegerType>()->getBitWidth() <=
        Output->castTo<BuiltinIntegerType>()->getBitWidth())
      return nullptr;
    break;
      
  case BuiltinValueKind::ZExt:
  case BuiltinValueKind::SExt:
    if (Output.isNull() ||
        !Input->is<BuiltinIntegerType>() || !Output->is<BuiltinIntegerType>() ||
        Input->castTo<BuiltinIntegerType>()->getBitWidth() >=
        Output->castTo<BuiltinIntegerType>()->getBitWidth())
      return nullptr;
    break;
  case BuiltinValueKind::FPToUI:
  case BuiltinValueKind::FPToSI:
    if (Output.isNull() || !Input->is<BuiltinFloatType>() ||
        !Output->is<BuiltinIntegerType>())
      return nullptr;
    break;
      
  case BuiltinValueKind::UIToFP:
  case BuiltinValueKind::SIToFP:
    if (Output.isNull() || !Input->is<BuiltinIntegerType>() ||
        !Output->is<BuiltinFloatType>())
      return nullptr;
    break;

  case BuiltinValueKind::FPTrunc:
    if (Output.isNull() ||
        !Input->is<BuiltinFloatType>() || !Output->is<BuiltinFloatType>() ||
        Input->castTo<BuiltinFloatType>()->getFPKind() <=
        Output->castTo<BuiltinFloatType>()->getFPKind())
      return nullptr;
    break;
  case BuiltinValueKind::FPExt:
    if (Output.isNull() ||
        !Input->is<BuiltinFloatType>() || !Output->is<BuiltinFloatType>() ||
        Input->castTo<BuiltinFloatType>()->getFPKind() >=
        Output->castTo<BuiltinFloatType>()->getFPKind())
      return nullptr;
    break;

  case BuiltinValueKind::PtrToInt:
    if (!Output.isNull() || !Input->is<BuiltinIntegerType>()) return nullptr;
    Output = Input;
    Input = Context.TheRawPointerType;
    break;
  case BuiltinValueKind::IntToPtr:
    if (!Output.isNull() || !Input->is<BuiltinIntegerType>()) return nullptr;
    Output = Context.TheRawPointerType;
    break;
  case BuiltinValueKind::BitCast:
    if (Output.isNull()) return nullptr;
    // FIXME: Implement bitcast typechecking.
    assert(0 && "Bitcast not supported yet!");
    return nullptr;
  }
  
  Type FnTy = FunctionType::get(Input, Output, Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

static ValueDecl *getLoadOperation(ASTContext &Context, Identifier Id,
                                   Type ResultTy) {
  TupleTypeElt ArgElts[] = {
    TupleTypeElt(Context.TheRawPointerType, Identifier())
  };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = FunctionType::get(Arg, ResultTy, Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

static ValueDecl *getStoreOperation(ASTContext &Context, Identifier Id,
                                    Type ValueTy) {
  TupleTypeElt ArgElts[] = {
    TupleTypeElt(ValueTy, Identifier()),
    TupleTypeElt(Context.TheRawPointerType, Identifier())
  };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = FunctionType::get(Arg, TupleType::getEmpty(Context), Context);
  return getBuiltinFunction(Context, Id, FnTy);
}

/// An array of the overloaded builtin kinds.
static const OverloadedBuiltinKind OverloadedBuiltinKinds[] = {
  OverloadedBuiltinKind::None,

// There's deliberately no BUILTIN clause here so that we'll blow up
// if new builtin categories are added there and not here.
#define BUILTIN_CAST_OPERATION(id, name) OverloadedBuiltinKind::Special,
#define BUILTIN_GEP_OPERATION(id, name, overload) overload,
#define BUILTIN_BINARY_OPERATION(id, name, overload) overload,
#define BUILTIN_BINARY_PREDICATE(id, name, overload) overload,
#define BUILTIN_LOAD(id, name) OverloadedBuiltinKind::Special,
#define BUILTIN_ASSIGN(id, name) OverloadedBuiltinKind::Special,
#define BUILTIN_INIT(id, name) OverloadedBuiltinKind::Special,
#include "swift/AST/Builtins.def"
};

/// Determines if a builtin type falls within the given category.
inline bool isBuiltinTypeOverloaded(Type T, OverloadedBuiltinKind OK) {
  switch (OK) {
  case OverloadedBuiltinKind::None:
    return false;  // always fail. 
  case OverloadedBuiltinKind::Integer:
    return T->is<BuiltinIntegerType>();
  case OverloadedBuiltinKind::IntegerOrRawPointer:
    return T->is<BuiltinIntegerType>() || T->is<BuiltinRawPointerType>();
  case OverloadedBuiltinKind::Float:
    return T->is<BuiltinFloatType>();
  case OverloadedBuiltinKind::Special:
    return true;
  }
  llvm_unreachable("bad overloaded builtin kind");
}


ValueDecl *swift::getBuiltinValue(ASTContext &Context, Identifier Id) {
  SmallVector<Type, 4> Types;
  StringRef OperationName = getBuiltinBaseName(Context, Id.str(), Types);
  
  if (OperationName == "trap" && Types.empty()) {
    Type Empty = TupleType::getEmpty(Context);
    Type FnTy = FunctionType::get(Empty, Empty, Context);
    return getBuiltinFunction(Context, Id, FnTy);
  }
  
  
  BuiltinValueKind BV = llvm::StringSwitch<BuiltinValueKind>(OperationName)
#define BUILTIN(id, name) \
       .Case(name, BuiltinValueKind::id)
#include "swift/AST/Builtins.def"
       .Default(BuiltinValueKind::None);

  // Filter out inappropriate overloads.
  OverloadedBuiltinKind OBK = OverloadedBuiltinKinds[unsigned(BV)];

  // Verify that all types match the overload filter.
  for (Type T : Types)
    if (!isBuiltinTypeOverloaded(T, OBK))
      return nullptr;

  switch (BV) {
  case BuiltinValueKind::None: return nullptr;

#define BUILTIN(id, name)
#define BUILTIN_GEP_OPERATION(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    if (Types.size() != 1) return nullptr;
    return getGepOperation(Context, Id, Types[0]);

#define BUILTIN(id, name)
#define BUILTIN_BINARY_OPERATION(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    if (Types.size() != 1) return nullptr;
    return getBinaryOperation(Context, Id, Types[0]);

#define BUILTIN(id, name)
#define BUILTIN_BINARY_PREDICATE(id, name, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    if (Types.size() != 1) return nullptr;
    return getBinaryPredicate(Context, Id, Types[0]);
      
#define BUILTIN(id, name)
#define BUILTIN_CAST_OPERATION(id, name)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return getCastOperation(Context, Id, BV, Types);
      
  case BuiltinValueKind::Load:
    if (Types.size() != 1) return nullptr;
    return getLoadOperation(Context, Id, Types[0]);

  case BuiltinValueKind::Assign:
  case BuiltinValueKind::Init:
    if (Types.size() != 1) return nullptr;
    return getStoreOperation(Context, Id, Types[0]);
  }
  llvm_unreachable("bad builtin value!");
}
