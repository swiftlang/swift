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
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Intrinsics.h"
#include <tuple>
using namespace swift;

Type swift::getBuiltinType(ASTContext &Context, StringRef Name) {
  if (Name == "RawPointer")
    return Context.TheRawPointerType;
  if (Name == "ObjectPointer")
    return Context.TheObjectPointerType;
  if (Name == "ObjCPointer")
    return Context.TheObjCPointerType;
  
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
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), Id, SourceLoc(),
                                /*generic=*/nullptr, T, /*init*/ nullptr,
                                Context.TheBuiltinModule);
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

static std::tuple<Type, GenericParamList*>
getGenericParam(ASTContext &Context) {
  Identifier GenericName = Context.getIdentifier("T");
  ArchetypeType *Archetype
    = ArchetypeType::getNew(Context, nullptr, GenericName, ArrayRef<Type>(),
                            Type(), 0);
  auto GenericTyDecl =
      new (Context) TypeAliasDecl(SourceLoc(), GenericName,
                                  SourceLoc(), TypeLoc::withoutLoc(Archetype),
                                  Context.TheBuiltinModule,
                                  MutableArrayRef<TypeLoc>());
  GenericTyDecl->setGenericParameter();
  Type GenericTy = GenericTyDecl->getAliasType();
  GenericParam Param = GenericTyDecl;
  auto ParamList = GenericParamList::create(Context, SourceLoc(), Param,
                                            SourceLoc());
  ParamList->setAllArchetypes(
    Context.AllocateCopy(ArrayRef<ArchetypeType *>(&Archetype, 1)));
  return std::make_tuple(GenericTy, ParamList);
}

static ValueDecl *getLoadOperation(ASTContext &Context, Identifier Id) {
  Type GenericTy;
  GenericParamList *ParamList;
  std::tie(GenericTy, ParamList) = getGenericParam(Context);

  TupleTypeElt ArgElts[] = {
    TupleTypeElt(Context.TheRawPointerType, Identifier())
  };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = PolymorphicFunctionType::get(Arg, GenericTy, ParamList, Context);
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), Id, SourceLoc(),
                                ParamList, FnTy, /*init*/ nullptr,
                                Context.TheBuiltinModule);
}

static ValueDecl *getStoreOperation(ASTContext &Context, Identifier Id) {
  Type GenericTy;
  GenericParamList *ParamList;
  std::tie(GenericTy, ParamList) = getGenericParam(Context);

  TupleTypeElt ArgElts[] = {
    TupleTypeElt(GenericTy, Identifier()),
    TupleTypeElt(Context.TheRawPointerType, Identifier())
  };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = PolymorphicFunctionType::get(Arg, TupleType::getEmpty(Context),
                                           ParamList, Context);
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), Id, SourceLoc(),
                                ParamList, FnTy, /*init*/ nullptr,
                                Context.TheBuiltinModule);
}

static ValueDecl *getDestroyOperation(ASTContext &Context, Identifier id) {
  Type genericTy;
  GenericParamList *paramList;
  std::tie(genericTy, paramList) = getGenericParam(Context);

  TupleTypeElt argElts[] = {
    TupleTypeElt(MetaTypeType::get(genericTy, Context), Identifier()),
    TupleTypeElt(Context.TheRawPointerType, Identifier())
  };
  Type argTy = TupleType::get(argElts, Context);
  Type fnTy = PolymorphicFunctionType::get(argTy, TupleType::getEmpty(Context),
                                           paramList, Context);
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), id, SourceLoc(),
                                paramList, fnTy, /*init*/ nullptr,
                                Context.TheBuiltinModule);
}

static Type getPointerSizeType(ASTContext &Context) {
  // FIXME: Size of a pointer here?
  return BuiltinIntegerType::get(64, Context);
}

static ValueDecl *getSizeOrAlignOfOperation(ASTContext &Context, Identifier Id){
  Type GenericTy;
  GenericParamList *ParamList;
  std::tie(GenericTy, ParamList) = getGenericParam(Context);

  TupleTypeElt ArgElts[] = {
    TupleTypeElt(MetaTypeType::get(GenericTy, Context), Identifier()),
  };
  Type Arg = TupleType::get(ArgElts, Context);
  Type FnTy = PolymorphicFunctionType::get(Arg,
                                           getPointerSizeType(Context),
                                           ParamList, Context);
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), Id, SourceLoc(),
                                ParamList, FnTy, /*init*/ nullptr,
                                Context.TheBuiltinModule);
}

static ValueDecl *getAllocOperation(ASTContext &Context, Identifier id) {
  Type ptrSizeTy = getPointerSizeType(Context);
  TupleTypeElt argElts[] = {
    TupleTypeElt(ptrSizeTy, Identifier()),
    TupleTypeElt(ptrSizeTy, Identifier())
  };
  Type argTy = TupleType::get(argElts, Context);
  Type fnTy = FunctionType::get(argTy, Context.TheRawPointerType, Context);

  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), id, SourceLoc(),
                                nullptr, fnTy, /*init*/ nullptr,
                                Context.TheBuiltinModule);
}

static ValueDecl *getDeallocOperation(ASTContext &Context, Identifier id) {
  TupleTypeElt argElts[] = {
    TupleTypeElt(Context.TheRawPointerType, Identifier()),
    TupleTypeElt(getPointerSizeType(Context), Identifier())
  };
  Type argTy = TupleType::get(argElts, Context);
  Type fnTy = FunctionType::get(argTy, TupleType::getEmpty(Context), Context);

  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), id, SourceLoc(),
                                nullptr, fnTy, /*init*/ nullptr,
                                Context.TheBuiltinModule);
}

static ValueDecl *getFenceOperation(ASTContext &Context, Identifier id) {
  Type tt = TupleType::getEmpty(Context);
  Type fnTy = FunctionType::get(tt, tt, Context);
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), id, SourceLoc(),
                                nullptr, fnTy, /*init*/ nullptr,
                                Context.TheBuiltinModule);
}

static ValueDecl *getCmpXChgOperation(ASTContext &Context, Identifier id,
                                      Type type) {
  TupleTypeElt argElts[] = {
    Context.TheRawPointerType, type, type
  };
  Type argTy = TupleType::get(argElts, Context);
  Type fnTy = FunctionType::get(argTy, type, Context);
  
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), id, SourceLoc(),
                                nullptr, fnTy, /*init*/ nullptr,
                                Context.TheBuiltinModule);
}

static ValueDecl *getObjectPointerCast(ASTContext &Context, Identifier Id,
                                       BuiltinValueKind BV) {
  Type GenericTy;
  GenericParamList *ParamList;
  std::tie(GenericTy, ParamList) = getGenericParam(Context);

  Type BuiltinTy;
  if (BV == BuiltinValueKind::BridgeToRawPointer ||
      BV == BuiltinValueKind::BridgeFromRawPointer)
    BuiltinTy = Context.TheRawPointerType;
  else
    BuiltinTy = Context.TheObjectPointerType;

  Type Arg, Ret;
  if (BV == BuiltinValueKind::CastToObjectPointer ||
      BV == BuiltinValueKind::BridgeToRawPointer) {
    Arg = GenericTy;
    Ret = BuiltinTy;
  } else {
    Arg = BuiltinTy;
    Ret = GenericTy;
  }

  TupleTypeElt ArgElts[] = { TupleTypeElt(Arg, Identifier()) };
  Arg = TupleType::get(ArgElts, Context);

  Type FnTy = PolymorphicFunctionType::get(Arg, Ret, ParamList, Context);
  return new (Context) FuncDecl(SourceLoc(), SourceLoc(), Id, SourceLoc(),
                                ParamList, FnTy, /*init*/ nullptr,
                                Context.TheBuiltinModule);
}

/// An array of the overloaded builtin kinds.
static const OverloadedBuiltinKind OverloadedBuiltinKinds[] = {
  OverloadedBuiltinKind::None,

// There's deliberately no BUILTIN clause here so that we'll blow up
// if new builtin categories are added there and not here.
#define BUILTIN_CAST_OPERATION(id, name) OverloadedBuiltinKind::Special,
#define BUILTIN_BINARY_OPERATION(id, name, overload) \
   OverloadedBuiltinKind::overload,
#define BUILTIN_BINARY_PREDICATE(id, name, overload) \
   OverloadedBuiltinKind::overload,
#define BUILTIN_MISC_OPERATION(id, name, overload) \
   OverloadedBuiltinKind::overload,
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

/// getLLVMIntrinsicID - Given an LLVM IR intrinsic name with argument types
/// remove (e.g. like "bswap") return the LLVM IR IntrinsicID for the intrinsic
/// or 0 if the intrinsic name doesn't match anything.
unsigned swift::getLLVMIntrinsicID(StringRef InName, bool hasArgTypes) {
  using namespace llvm;

  // FIXME: Hack so we don't treat "trunc" as "llvm.trunc"; we should consider
  // renaming the swift intrinsic so it doesn't conflict.
  if (InName == "trunc")
    return llvm::Intrinsic::not_intrinsic;

  // Prepend "llvm." and change _ to . in name.
  SmallString<128> NameS;
  NameS.append("llvm.");
  for (char C : InName)
    NameS.push_back(C == '_' ? '.' : C);
  if (hasArgTypes)
    NameS.push_back('.');
  
  const char *Name = NameS.c_str();
  unsigned Len = NameS.size();
#define GET_FUNCTION_RECOGNIZER
#include "llvm/Intrinsics.gen"
#undef GET_FUNCTION_RECOGNIZER
  return llvm::Intrinsic::not_intrinsic;
}

static Type DecodeIntrinsicType(ArrayRef<llvm::Intrinsic::IITDescriptor> &Table,
                                ArrayRef<Type> Tys, ASTContext &Context) {
  typedef llvm::Intrinsic::IITDescriptor IITDescriptor;
  IITDescriptor D = Table.front();
  Table = Table.slice(1);
  switch (D.Kind) {
  case IITDescriptor::MMX:
  case IITDescriptor::Metadata:
  case IITDescriptor::Vector:
  case IITDescriptor::ExtendVecArgument:
  case IITDescriptor::TruncVecArgument:
    // These types cannot be expressed in swift yet.
    return Type();

  case IITDescriptor::Void: return TupleType::getEmpty(Context);
  case IITDescriptor::Float: return Context.TheIEEE32Type;
  case IITDescriptor::Double: return Context.TheIEEE64Type;

  case IITDescriptor::Integer:
    return BuiltinIntegerType::get(D.Integer_Width, Context);
  case IITDescriptor::Pointer:
    if (D.Pointer_AddressSpace)
      return Type();  // Reject non-default address space pointers.
      
    // Decode but ignore the pointee.  Just decode all IR pointers to unsafe
    // pointer type.
    (void)DecodeIntrinsicType(Table, Tys, Context);
    return Context.TheRawPointerType;
  case IITDescriptor::Argument:
    if (D.getArgumentNumber() >= Tys.size())
      return Type();
    return Tys[D.getArgumentNumber()];
  case IITDescriptor::Struct: {
    SmallVector<TupleTypeElt, 5> Elts;
    for (unsigned i = 0; i != D.Struct_NumElements; ++i) {
      Type T = DecodeIntrinsicType(Table, Tys, Context);
      if (!T) return Type();
      
      Elts.push_back(TupleTypeElt(T, Identifier()));
    }
    return TupleType::get(Elts, Context);
  }
  }
  llvm_unreachable("unhandled");
}


static Type getSwiftFunctionTypeForIntrinsic(unsigned iid,
                                             ArrayRef<Type> TypeArgs,
                                             ASTContext &Context) {
  typedef llvm::Intrinsic::IITDescriptor IITDescriptor;
  SmallVector<IITDescriptor, 8> Table;
  getIntrinsicInfoTableEntries((llvm::Intrinsic::ID)iid, Table);

  ArrayRef<IITDescriptor> TableRef = Table;

  // Decode the intrinsic's LLVM IR type, and map it to swift builtin types.
  Type ResultTy = DecodeIntrinsicType(TableRef, TypeArgs, Context);
  if (!ResultTy) return Type();
  
  SmallVector<TupleTypeElt, 8> ArgTys;
  while (!TableRef.empty()) {
    Type ArgTy = DecodeIntrinsicType(TableRef, TypeArgs, Context);
    if (!ArgTy) return Type();
    ArgTys.push_back(TupleTypeElt(ArgTy, Identifier()));
  }
  
  Type Arg = TupleType::get(ArgTys, Context);
  return FunctionType::get(Arg, ResultTy, Context);
}

static bool isValidFenceOrdering(StringRef Ordering) {
  return Ordering == "acquire" || Ordering == "release" ||
         Ordering == "acqrel" || Ordering == "seqcst";
}

static bool isValidAtomicOrdering(StringRef Ordering) {
  return Ordering == "unordered" || Ordering == "monotonic" ||
         Ordering == "acquire" || Ordering == "release" ||
         Ordering == "acqrel" || Ordering == "seqcst";
}

ValueDecl *swift::getBuiltinValue(ASTContext &Context, Identifier Id) {
  SmallVector<Type, 4> Types;
  StringRef OperationName = getBuiltinBaseName(Context, Id.str(), Types);

  // If this is the name of an LLVM intrinsic, cons up a swift function with a
  // type that matches the IR types.
  if (unsigned ID = getLLVMIntrinsicID(OperationName, !Types.empty())) {
    if (Type FnTy = getSwiftFunctionTypeForIntrinsic(ID, Types, Context))
      return getBuiltinFunction(Context, Id, FnTy);
  }
  
  
  // If this starts with fence, we have special suffixes to handle.
  if (OperationName.startswith("fence_")) {
    OperationName = OperationName.drop_front(strlen("fence_"));
    
    // Verify we have a single integer, floating point, or pointer type.
    if (!Types.empty()) return nullptr;
    
    // Get and validate the ordering argument, which is required.
    auto Underscore = OperationName.find('_');
    if (!isValidFenceOrdering(OperationName.substr(0, Underscore)))
      return nullptr;
    OperationName = OperationName.substr(Underscore);
    
    // Accept singlethread if present.
    if (OperationName.startswith("_singlethread"))
      OperationName = OperationName.drop_front(strlen("_singlethread"));
    // Nothing else is allowed in the name.
    if (!OperationName.empty())
      return nullptr;
    return getFenceOperation(Context, Id);
  }
  
  // If this starts with cmpxchg, we have special suffixes to handle.
  if (OperationName.startswith("cmpxchg_")) {
    OperationName = OperationName.drop_front(strlen("cmpxchg_"));
    
    // Verify we have a single integer, floating point, or pointer type.
    if (Types.size() != 1) return nullptr;
    Type T = Types[0];
    if (!T->is<BuiltinIntegerType>() && !T->is<BuiltinRawPointerType>() &&
        !T->is<BuiltinFloatType>())
      return nullptr;

    // Get and validate the ordering argument, which is required.
    auto Underscore = OperationName.find('_');
    if (!isValidAtomicOrdering(OperationName.substr(0, Underscore)))
      return nullptr;
    OperationName = OperationName.substr(Underscore);
    
    // Accept volatile and singlethread if present.
    if (OperationName.startswith("_volatile"))
      OperationName = OperationName.drop_front(strlen("_volatile"));
    if (OperationName.startswith("_singlethread"))
      OperationName = OperationName.drop_front(strlen("_singlethread"));
    // Nothing else is allowed in the name.
    if (!OperationName.empty())
      return nullptr;
    return getCmpXChgOperation(Context, Id, T);
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
  case BuiltinValueKind::Fence:
  case BuiltinValueKind::CmpXChg:
    assert(0 && "Handled above");
  case BuiltinValueKind::None: return nullptr;

  case BuiltinValueKind::Gep:
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
  case BuiltinValueKind::Move:
    if (!Types.empty()) return nullptr;
    return getLoadOperation(Context, Id);

  case BuiltinValueKind::Destroy:
    if (!Types.empty()) return nullptr;
    return getDestroyOperation(Context, Id);

  case BuiltinValueKind::Assign:
  case BuiltinValueKind::Init:
    if (!Types.empty()) return nullptr;
    return getStoreOperation(Context, Id);

  case BuiltinValueKind::Sizeof:
  case BuiltinValueKind::Strideof:
  case BuiltinValueKind::Alignof:
    return getSizeOrAlignOfOperation(Context, Id);

  case BuiltinValueKind::AllocRaw:
    return getAllocOperation(Context, Id);

  case BuiltinValueKind::DeallocRaw:
    return getDeallocOperation(Context, Id);

  case BuiltinValueKind::CastToObjectPointer:
  case BuiltinValueKind::CastFromObjectPointer:
  case BuiltinValueKind::BridgeToRawPointer:
  case BuiltinValueKind::BridgeFromRawPointer:
    if (!Types.empty()) return nullptr;
    return getObjectPointerCast(Context, Id, BV);
  }
  llvm_unreachable("bad builtin value!");
}
