//===--- Builtins.cpp - Swift Language Builtin ASTs -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the interface to the Builtin APIs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/Basic/LLVMContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/GenericEnvironment.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include <tuple>
using namespace swift;

struct BuiltinExtraInfoTy {
  const char *Attributes;
};

static const BuiltinExtraInfoTy BuiltinExtraInfo[] = {
    {nullptr},
#define BUILTIN(Id, Name, Attrs) {Attrs},
#include "swift/AST/Builtins.def"
};

bool BuiltinInfo::isReadNone() const {
  return strchr(BuiltinExtraInfo[(unsigned)ID].Attributes, 'n') != nullptr;
}

bool IntrinsicInfo::hasAttribute(llvm::Attribute::AttrKind Kind) const {
  // FIXME: We should not be relying on the global LLVM context.
  llvm::AttributeList attrs =
      llvm::Intrinsic::getAttributes(getGlobalLLVMContext(), ID);
  return (attrs.hasAttribute(llvm::AttributeList::FunctionIndex, Kind));
}

Type swift::getBuiltinType(ASTContext &Context, StringRef Name) {
  // Vectors are VecNxT, where "N" is the number of elements and
  // T is the element type.
  if (Name.startswith("Vec")) {
    Name = Name.substr(3);
    StringRef::size_type xPos = Name.find('x');
    if (xPos == StringRef::npos)
      return Type();

    unsigned numElements;
    if (Name.substr(0, xPos).getAsInteger(10, numElements) ||
        numElements == 0 || numElements > 1024)
      return Type();

    Type elementType = getBuiltinType(Context, Name.substr(xPos + 1));
    if (!elementType)
      return Type();

    return BuiltinVectorType::get(Context, elementType, numElements);
  }

  if (Name == "RawPointer")
    return Context.TheRawPointerType;
  if (Name == "NativeObject")
    return Context.TheNativeObjectType;
  if (Name == "UnknownObject")
    return Context.TheUnknownObjectType;
  if (Name == "BridgeObject")
    return Context.TheBridgeObjectType;
  if (Name == "SILToken")
    return Context.TheSILTokenType;
  if (Name == "UnsafeValueBuffer")
    return Context.TheUnsafeValueBufferType;
  
  if (Name == "FPIEEE32")
    return Context.TheIEEE32Type;
  if (Name == "FPIEEE64")
    return Context.TheIEEE64Type;

  if (Name == "Word")
    return BuiltinIntegerType::getWordType(Context);

  // Handle 'int8' and friends.
  if (Name.substr(0, 3) == "Int") {
    unsigned BitWidth;
    if (!Name.substr(3).getAsInteger(10, BitWidth) &&
        BitWidth <= 2048 && BitWidth != 0)  // Cap to prevent insane things.
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

  // AnyObject is the empty class-constrained existential.
  if (Name == "AnyObject")
    return CanType(
      ProtocolCompositionType::get(Context, {},
                                   /*HasExplicitAnyObject=*/true));

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

/// Build a builtin function declaration.
static FuncDecl *
getBuiltinFunction(Identifier Id, ArrayRef<Type> argTypes, Type ResType,
                   FunctionType::ExtInfo Info = FunctionType::ExtInfo()) {
  auto &Context = ResType->getASTContext();
  
  ModuleDecl *M = Context.TheBuiltinModule;
  DeclContext *DC = &M->getMainFile(FileUnitKind::Builtin);

  SmallVector<ParamDecl*, 4> params;
  for (Type argType : argTypes) {
    auto PD = new (Context)
        ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                  Identifier(), SourceLoc(), Identifier(), DC);
    PD->setInterfaceType(argType);
    PD->setValidationToChecked();
    PD->setImplicit();
    params.push_back(PD);
  }

  auto *paramList = ParameterList::create(Context, params);
  
  DeclName Name(Context, Id, paramList);
  auto FD = FuncDecl::create(Context, /*StaticLoc=*/SourceLoc(),
                             StaticSpellingKind::None,
                             /*FuncLoc=*/SourceLoc(),
                             Name, /*NameLoc=*/SourceLoc(),
                             /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                             /*GenericParams=*/nullptr,
                             paramList,
                             TypeLoc::withoutLoc(ResType), DC);
  FD->computeType(Info);
  FD->setValidationToChecked();
  FD->setImplicit();
  FD->setAccess(AccessLevel::Public);
  return FD;
}

/// Build a builtin function declaration.
static FuncDecl *
getBuiltinGenericFunction(Identifier Id,
                          ArrayRef<AnyFunctionType::Param> ArgParamTypes,
                          Type ResType,
                          GenericParamList *GenericParams,
                          GenericEnvironment *Env) {
  assert(GenericParams && "Missing generic parameters");
  auto &Context = ResType->getASTContext();

  ModuleDecl *M = Context.TheBuiltinModule;
  DeclContext *DC = &M->getMainFile(FileUnitKind::Builtin);

  SmallVector<ParamDecl*, 4> params;
  for (unsigned i = 0, e = ArgParamTypes.size(); i < e; i++) {
    auto paramIfaceType = ArgParamTypes[i].getPlainType();
    auto specifier =
      VarDecl::getParameterSpecifierForValueOwnership(
        ArgParamTypes[i].getParameterFlags().getValueOwnership());
    auto PD = new (Context) ParamDecl(specifier,
                                      SourceLoc(), SourceLoc(),
                                      Identifier(), SourceLoc(),
                                      Identifier(), DC);
    PD->setInterfaceType(paramIfaceType);
    PD->setValidationToChecked();
    PD->setImplicit();
    params.push_back(PD);
  }

  auto *paramList = ParameterList::create(Context, params);

  DeclName Name(Context, Id, paramList);
  auto func = FuncDecl::create(Context, /*StaticLoc=*/SourceLoc(),
                               StaticSpellingKind::None,
                               /*FuncLoc=*/SourceLoc(),
                               Name, /*NameLoc=*/SourceLoc(),
                               /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                               GenericParams,
                               paramList,
                               TypeLoc::withoutLoc(ResType), DC);

  func->setGenericEnvironment(Env);
  func->computeType();
  func->setValidationToChecked();
  func->setImplicit();
  func->setAccess(AccessLevel::Public);

  return func;
}

/// Build a getelementptr operation declaration.
static ValueDecl *getGepRawOperation(Identifier Id, Type ArgType) {
  auto &Context = ArgType->getASTContext();
  
  // This is always "(i8*, IntTy) -> i8*"
  Type ArgElts[] = { Context.TheRawPointerType, ArgType };
  Type ResultTy = Context.TheRawPointerType;
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getStringObjectOrOperation(Identifier Id, Type ArgType) {
  return getBuiltinFunction(Id, {ArgType, ArgType}, ArgType);
}

/// Build a binary operation declaration.
static ValueDecl *getBinaryOperation(Identifier Id, Type ArgType) {
  return getBuiltinFunction(Id, { ArgType, ArgType }, ArgType);
}

/// Build a declaration for a binary operation with overflow.
static ValueDecl *getBinaryOperationWithOverflow(Identifier Id,
                                                 Type ArgType) {
  auto &Context = ArgType->getASTContext();
  Type ShouldCheckForOverflowTy = BuiltinIntegerType::get(1, Context);
  Type ArgElts[] = { ArgType, ArgType, ShouldCheckForOverflowTy };
  Type OverflowBitTy = BuiltinIntegerType::get(1, Context);
  TupleTypeElt ResultElts[] = { ArgType, OverflowBitTy };
  Type ResultTy = TupleType::get(ResultElts, Context);
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getUnaryOperation(Identifier Id, Type ArgType) {
  return getBuiltinFunction(Id, { ArgType }, ArgType);
}

/// Build a binary predicate declaration.
static ValueDecl *getBinaryPredicate(Identifier Id, Type ArgType) {
  auto &Context = ArgType->getASTContext();

  Type ArgElts[] = { ArgType, ArgType };
  Type ResultTy = BuiltinIntegerType::get(1, Context);
  if (auto VecTy = ArgType->getAs<BuiltinVectorType>()) {
    ResultTy = BuiltinVectorType::get(Context, ResultTy,
                                      VecTy->getNumElements());
  }
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

/// Build a cast.  There is some custom type checking here.
static ValueDecl *getCastOperation(ASTContext &Context, Identifier Id,
                                   BuiltinValueKind VK,
                                   ArrayRef<Type> Types) {
  if (Types.empty() || Types.size() > 2) return nullptr;
  Type Input = Types[0];
  Type Output = Types.size() == 2 ? Types[1] : Type();

  // If both types are vectors, look through the vectors.
  Type CheckInput = Input;
  Type CheckOutput = Output;
  bool UnwrappedVector = false;
  auto InputVec = Input->getAs<BuiltinVectorType>();
  auto OutputVec = Output.isNull()? nullptr :Output->getAs<BuiltinVectorType>();
  if (InputVec && OutputVec &&
      InputVec->getNumElements() == OutputVec->getNumElements()) {
    UnwrappedVector = true;
    CheckInput = InputVec->getElementType();
    CheckOutput = OutputVec->getElementType();
  }

  // Custom type checking.  We know the one or two types have been subjected to
  // the "isBuiltinTypeOverloaded" predicate successfully.
  switch (VK) {
  default: llvm_unreachable("Not a cast operation");

  case BuiltinValueKind::Trunc:
    if (CheckOutput.isNull() ||
        !CheckInput->is<BuiltinIntegerType>() ||
        !CheckOutput->is<BuiltinIntegerType>() ||
        CheckInput->castTo<BuiltinIntegerType>()->getLeastWidth() <=
          CheckOutput->castTo<BuiltinIntegerType>()->getGreatestWidth())
      return nullptr;
    break;
  case BuiltinValueKind::TruncOrBitCast:
    if (CheckOutput.isNull() ||
        !CheckInput->is<BuiltinIntegerType>() ||
        !CheckOutput->is<BuiltinIntegerType>() ||
        CheckInput->castTo<BuiltinIntegerType>()->getLeastWidth() <
          CheckOutput->castTo<BuiltinIntegerType>()->getGreatestWidth())
      return nullptr;
    break;
      
  case BuiltinValueKind::ZExt:
  case BuiltinValueKind::SExt: {
    if (CheckOutput.isNull() ||
        !CheckInput->is<BuiltinIntegerType>() ||
        !CheckOutput->is<BuiltinIntegerType>() ||
        CheckInput->castTo<BuiltinIntegerType>()->getGreatestWidth() >=
          CheckOutput->castTo<BuiltinIntegerType>()->getLeastWidth())
      return nullptr;
    break;
  }
  case BuiltinValueKind::ZExtOrBitCast:
  case BuiltinValueKind::SExtOrBitCast: {
    if (CheckOutput.isNull() ||
        !CheckInput->is<BuiltinIntegerType>() ||
        !CheckOutput->is<BuiltinIntegerType>() ||
        CheckInput->castTo<BuiltinIntegerType>()->getGreatestWidth() >
          CheckOutput->castTo<BuiltinIntegerType>()->getLeastWidth())
      return nullptr;
    break;
  }

  case BuiltinValueKind::FPToUI:
  case BuiltinValueKind::FPToSI:
    if (CheckOutput.isNull() || !CheckInput->is<BuiltinFloatType>() ||
        !CheckOutput->is<BuiltinIntegerType>())
      return nullptr;
    break;
      
  case BuiltinValueKind::UIToFP:
  case BuiltinValueKind::SIToFP:
    if (CheckOutput.isNull() || !CheckInput->is<BuiltinIntegerType>() ||
        !CheckOutput->is<BuiltinFloatType>())
      return nullptr;
    break;

  case BuiltinValueKind::FPTrunc:
    if (CheckOutput.isNull() ||
        !CheckInput->is<BuiltinFloatType>() ||
        !CheckOutput->is<BuiltinFloatType>() ||
        CheckInput->castTo<BuiltinFloatType>()->getFPKind() <=
        CheckOutput->castTo<BuiltinFloatType>()->getFPKind())
      return nullptr;
    break;
  case BuiltinValueKind::FPExt:
    if (CheckOutput.isNull() ||
        !CheckInput->is<BuiltinFloatType>() ||
        !CheckOutput->is<BuiltinFloatType>() ||
        CheckInput->castTo<BuiltinFloatType>()->getFPKind() >=
        CheckOutput->castTo<BuiltinFloatType>()->getFPKind())
      return nullptr;
    break;

  case BuiltinValueKind::PtrToInt:
    // FIXME: Do we care about vectors of pointers?
    if (!CheckOutput.isNull() || !CheckInput->is<BuiltinIntegerType>() ||
        UnwrappedVector)
      return nullptr;
    Output = Input;
    Input = Context.TheRawPointerType;
    break;
  case BuiltinValueKind::IntToPtr:
    // FIXME: Do we care about vectors of pointers?
    if (!CheckOutput.isNull() || !CheckInput->is<BuiltinIntegerType>() ||
        UnwrappedVector)
      return nullptr;
    Output = Context.TheRawPointerType;
    break;
  case BuiltinValueKind::BitCast:
    if (CheckOutput.isNull()) return nullptr;

    // Support float <-> int bitcast where the types are the same widths.
    if (auto *BIT = CheckInput->getAs<BuiltinIntegerType>())
      if (auto *BFT = CheckOutput->getAs<BuiltinFloatType>())
        if (BIT->isFixedWidth() && BIT->getFixedWidth() == BFT->getBitWidth())
            break;
    if (auto *BFT = CheckInput->getAs<BuiltinFloatType>())
      if (auto *BIT = CheckOutput->getAs<BuiltinIntegerType>())
        if (BIT->isFixedWidth() && BIT->getFixedWidth() == BFT->getBitWidth())
          break;

    // FIXME: Implement bitcast typechecking.
    llvm_unreachable("Bitcast not supported yet!");
    return nullptr;
  }

  return getBuiltinFunction(Id, { Input }, Output);
}

static const char * const GenericParamNames[] = {
  "T",
  "U",
  "V",
  "W",
  "X",
  "Y",
  "Z"
};

static GenericTypeParamDecl*
createGenericParam(ASTContext &ctx, const char *name, unsigned index) {
  ModuleDecl *M = ctx.TheBuiltinModule;
  Identifier ident = ctx.getIdentifier(name);
  SmallVector<ProtocolDecl *, 1> protos;
  auto genericParam =
    new (ctx) GenericTypeParamDecl(&M->getMainFile(FileUnitKind::Builtin),
                                   ident, SourceLoc(), 0, index);
  return genericParam;
}

/// Create a generic parameter list with multiple generic parameters.
static GenericParamList *getGenericParams(ASTContext &ctx,
                                          unsigned numParameters,
                       SmallVectorImpl<GenericTypeParamDecl*> &genericParams) {
  assert(numParameters <= llvm::array_lengthof(GenericParamNames));
  assert(genericParams.empty());

  for (unsigned i = 0; i != numParameters; ++i)
    genericParams.push_back(createGenericParam(ctx, GenericParamNames[i], i));

  auto paramList = GenericParamList::create(ctx, SourceLoc(), genericParams,
                                            SourceLoc());
  return paramList;
}

namespace {
  class BuiltinGenericSignatureBuilder {
  public:
    ASTContext &Context;

  private:
    GenericParamList *TheGenericParamList;
    SmallVector<GenericTypeParamDecl*, 2> GenericTypeParams;
    GenericEnvironment *GenericEnv = nullptr;
    SmallVector<AnyFunctionType::Param, 4> InterfaceParams;
    Type InterfaceResult;

  public:
    BuiltinGenericSignatureBuilder(ASTContext &ctx, unsigned numGenericParams = 1)
        : Context(ctx) {
      TheGenericParamList = getGenericParams(ctx, numGenericParams,
                                             GenericTypeParams);

      GenericSignatureBuilder Builder(ctx);
      for (auto gp : GenericTypeParams) {
        Builder.addGenericParameter(gp);
      }

      auto GenericSig =
        std::move(Builder).computeGenericSignature(SourceLoc());
      GenericEnv = GenericSig->createGenericEnvironment();
    }

    template <class G>
    void addParameter(const G &generator,
                      ValueOwnership ownership = ValueOwnership::Default) {
      Type gTyIface = generator.build(*this);
      auto flags = ParameterTypeFlags().withValueOwnership(ownership);
      InterfaceParams.emplace_back(gTyIface, Identifier(), flags);
    }

    template <class G>
    void setResult(const G &generator) {
      InterfaceResult = generator.build(*this);
    }

    ValueDecl *build(Identifier name) {
      return getBuiltinGenericFunction(name, InterfaceParams,
                                       InterfaceResult,
                                       TheGenericParamList,
                                       GenericEnv);
    }

    // Don't use these generator classes directly; call the make{...}
    // functions which follow this class.

    struct ConcreteGenerator {
      Type TheType;
      Type build(BuiltinGenericSignatureBuilder &builder) const {
        return TheType;
      }
    };
    struct ParameterGenerator {
      unsigned Index;
      Type build(BuiltinGenericSignatureBuilder &builder) const {
        return builder.GenericTypeParams[Index]->getDeclaredInterfaceType();
      }
    };
    struct LambdaGenerator {
      std::function<Type(BuiltinGenericSignatureBuilder &)> TheFunction;
      Type build(BuiltinGenericSignatureBuilder &builder) const {
        return TheFunction(builder);
      }
    };
    template <class T>
    struct MetatypeGenerator {
      T Object;
      Optional<MetatypeRepresentation> Repr;
      Type build(BuiltinGenericSignatureBuilder &builder) const {
        return MetatypeType::get(Object.build(builder), Repr);
      }
    };
  };
} // end anonymous namespace

static BuiltinGenericSignatureBuilder::ConcreteGenerator
makeConcrete(Type type) {
  return { type };
}

// SWIFT_ENABLE_TENSORFLOW
template <class P, class... Gs>
static BuiltinGenericSignatureBuilder::LambdaGenerator
makeBoundGeneric(NominalTypeDecl *decl, const P &parentGenerator,
                 const Gs & ...genericParamGenerators) {
  return {
    [=](BuiltinGenericSignatureBuilder &builder) -> Type {
      Type parent = parentGenerator.build(builder);
      Type genParams[] = {
        genericParamGenerators.build(builder)...
      };
      return BoundGenericType::get(decl, parent, genParams);
    }
  };
}

static BuiltinGenericSignatureBuilder::ParameterGenerator
makeGenericParam(unsigned index = 0) {
  return { index };
}

template <class... Gs>
static BuiltinGenericSignatureBuilder::LambdaGenerator
makeTuple(const Gs & ...elementGenerators) {
  return {
    [=](BuiltinGenericSignatureBuilder &builder) -> Type {
      TupleTypeElt elts[] = {
        elementGenerators.build(builder)...
      };
      return TupleType::get(elts, builder.Context);
    }
  };
}

template <class... Gs>
static BuiltinGenericSignatureBuilder::LambdaGenerator
makeBoundGenericType(NominalTypeDecl *decl,
                     const Gs & ...argumentGenerators) {
  return {
    [=](BuiltinGenericSignatureBuilder &builder) -> Type {
      Type args[] = {
        argumentGenerators.build(builder)...
      };
      return BoundGenericType::get(decl, Type(), args);
    }
  };
}

template <class T>
static BuiltinGenericSignatureBuilder::MetatypeGenerator<T>
makeMetatype(const T &object, Optional<MetatypeRepresentation> repr = None) {
  return { object, repr };
}

/// Create a function with type <T> T -> ().
static ValueDecl *getRefCountingOperation(ASTContext &Context, Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeGenericParam());
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getLoadOperation(ASTContext &Context, Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.setResult(makeGenericParam());
  return builder.build(Id);
}

static ValueDecl *getStoreOperation(ASTContext &Context, Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeGenericParam(), ValueOwnership::Owned);
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getDestroyOperation(ASTContext &Context, Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getDestroyArrayOperation(ASTContext &Context, Identifier Id) {
  auto wordType = BuiltinIntegerType::get(BuiltinIntegerWidth::pointer(),
                                          Context);
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeConcrete(wordType));
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getTransferArrayOperation(ASTContext &Context, Identifier Id){
  auto wordType = BuiltinIntegerType::get(BuiltinIntegerWidth::pointer(),
                                          Context);
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeConcrete(wordType));
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getIsUniqueOperation(ASTContext &Context, Identifier Id) {
  // <T> (@inout T) -> Int1
  Type Int1Ty = BuiltinIntegerType::get(1, Context);

  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeGenericParam(), ValueOwnership::InOut);
  builder.setResult(makeConcrete(Int1Ty));
  return builder.build(Id);
}

static ValueDecl *getBindMemoryOperation(ASTContext &Context, Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeConcrete(BuiltinIntegerType::getWordType(Context)));
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getAllocWithTailElemsOperation(ASTContext &Context,
                                                 Identifier Id,
                                                 int NumTailTypes) {
  if (NumTailTypes < 1 ||
      1 + NumTailTypes > (int)llvm::array_lengthof(GenericParamNames))
    return nullptr;
  BuiltinGenericSignatureBuilder builder(Context, 1 + NumTailTypes);
  builder.addParameter(makeMetatype(makeGenericParam(0)));
  for (int Idx = 0; Idx < NumTailTypes; ++Idx) {
    builder.addParameter(makeConcrete(BuiltinIntegerType::getWordType(Context)));
    builder.addParameter(makeMetatype(makeGenericParam(Idx + 1)));
  }
  builder.setResult(makeGenericParam(0));
  return builder.build(Id);
}

static ValueDecl *getProjectTailElemsOperation(ASTContext &Context,
                                               Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context, 2);
  builder.addParameter(makeGenericParam(0));
  builder.addParameter(makeMetatype(makeGenericParam(1)));
  builder.setResult(makeConcrete(Context.TheRawPointerType));
  return builder.build(Id);
}

/// Build a getelementptr operation declaration.
static ValueDecl *getGepOperation(ASTContext &Context, Identifier Id,
                                  Type ArgType) {
  BuiltinGenericSignatureBuilder builder(Context, 1);
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeConcrete(ArgType));
  builder.addParameter(makeMetatype(makeGenericParam(0)));
  builder.setResult(makeConcrete(Context.TheRawPointerType));
  return builder.build(Id);
}

static ValueDecl *getGetTailAddrOperation(ASTContext &Context, Identifier Id,
                                          Type ArgType) {
  BuiltinGenericSignatureBuilder builder(Context, 2);
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeConcrete(ArgType));
  builder.addParameter(makeMetatype(makeGenericParam(0)));
  builder.addParameter(makeMetatype(makeGenericParam(1)));
  builder.setResult(makeConcrete(Context.TheRawPointerType));
  return builder.build(Id);
}

static ValueDecl *getBeginUnpairedAccessOperation(ASTContext &Context,
                                                  Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeMetatype(makeGenericParam(0)));
  builder.setResult(makeConcrete(Context.TheEmptyTupleType));
  return builder.build(Id);
}

static ValueDecl *
getPerformInstantaneousReadAccessOperation(ASTContext &Context,
                                           Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeMetatype(makeGenericParam(0)));
  builder.setResult(makeConcrete(Context.TheEmptyTupleType));
  return builder.build(Id);
}

static ValueDecl *getEndUnpairedAccessOperation(ASTContext &Context,
                                                Identifier Id) {
  return getBuiltinFunction(Id, { Context.TheRawPointerType },
                                Context.TheEmptyTupleType);
}
static ValueDecl *getSizeOrAlignOfOperation(ASTContext &Context,
                                            Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(BuiltinIntegerType::getWordType(Context)));
  return builder.build(Id);
}

static ValueDecl *getIsPODOperation(ASTContext &Context, Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(BuiltinIntegerType::get(1,Context)));
  return builder.build(Id);
}

static ValueDecl *getIsBitwiseTakable(ASTContext &Context, Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(BuiltinIntegerType::get(1,Context)));
  return builder.build(Id);
}

static ValueDecl *getIsOptionalOperation(ASTContext &Context, Identifier Id) {
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(BuiltinIntegerType::get(1,Context)));
  return builder.build(Id);
}

static ValueDecl *getIsSameMetatypeOperation(ASTContext &Context, Identifier Id) {
  CanType anyMetatype = CanExistentialMetatypeType::get(Context.TheAnyType);
  auto ResultTy = BuiltinIntegerType::get(1,Context);
  return getBuiltinFunction(Id, {anyMetatype, anyMetatype}, ResultTy);
}

static ValueDecl *getAllocOperation(ASTContext &Context, Identifier Id) {
  Type PtrSizeTy = BuiltinIntegerType::getWordType(Context);
  Type ResultTy = Context.TheRawPointerType;
  return getBuiltinFunction(Id, { PtrSizeTy, PtrSizeTy }, ResultTy);
}

static ValueDecl *getDeallocOperation(ASTContext &Context, Identifier Id) {
  auto PtrSizeTy = BuiltinIntegerType::getWordType(Context);
  Type ArgElts[] = { Context.TheRawPointerType, PtrSizeTy, PtrSizeTy };
  Type ResultTy = TupleType::getEmpty(Context);
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getFenceOperation(ASTContext &Context, Identifier Id) {
  return getBuiltinFunction(Id, {}, TupleType::getEmpty(Context));
}

static ValueDecl *getVoidErrorOperation(ASTContext &Context, Identifier Id) {
  return getBuiltinFunction(Id, {Context.getExceptionType()},
                            TupleType::getEmpty(Context));
}

static ValueDecl *getUnexpectedErrorOperation(ASTContext &Context,
                                              Identifier Id) {
  return getBuiltinFunction(Id, {Context.getExceptionType()},
                            Context.getNeverType());
}

static ValueDecl *getCmpXChgOperation(ASTContext &Context, Identifier Id,
                                      Type T) {
  Type ArgElts[] = { Context.TheRawPointerType, T, T };
  Type BoolTy = BuiltinIntegerType::get(1, Context);
  Type ResultTy = TupleType::get({ T, BoolTy }, Context);
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getAtomicRMWOperation(ASTContext &Context, Identifier Id,
                                        Type T) {
  return getBuiltinFunction(Id, { Context.TheRawPointerType, T }, T);
}

static ValueDecl *getAtomicLoadOperation(ASTContext &Context, Identifier Id,
                                         Type T) {
  return getBuiltinFunction(Id, { Type(Context.TheRawPointerType) }, T);
}

static ValueDecl *getAtomicStoreOperation(ASTContext &Context, Identifier Id,
                                          Type T) {
  return getBuiltinFunction(Id, { Context.TheRawPointerType, T },
                            Context.TheEmptyTupleType);
}

static ValueDecl *getNativeObjectCast(ASTContext &Context, Identifier Id,
                                      BuiltinValueKind BV) {

  ValueOwnership ownership;
  Type builtinTy;
  switch (BV) {
  case BuiltinValueKind::CastToNativeObject:
  case BuiltinValueKind::UnsafeCastToNativeObject:
  case BuiltinValueKind::CastFromNativeObject:
    builtinTy = Context.TheNativeObjectType;
    ownership = ValueOwnership::Owned;
    break;

  case BuiltinValueKind::BridgeToRawPointer:
  case BuiltinValueKind::BridgeFromRawPointer:
    builtinTy = Context.TheRawPointerType;
    ownership = ValueOwnership::Default;
    break;

  default:
    llvm_unreachable("unexpected kind");
  }

  BuiltinGenericSignatureBuilder builder(Context);
  if (BV == BuiltinValueKind::CastToNativeObject ||
      BV == BuiltinValueKind::UnsafeCastToNativeObject ||
      BV == BuiltinValueKind::BridgeToRawPointer) {
    builder.addParameter(makeGenericParam(), ownership);
    builder.setResult(makeConcrete(builtinTy));
  } else {
    builder.addParameter(makeConcrete(builtinTy), ownership);
    builder.setResult(makeGenericParam());
  }
  return builder.build(Id);
}

static ValueDecl *getCastToBridgeObjectOperation(ASTContext &C,
                                                 Identifier Id) {
  auto wordType = BuiltinIntegerType::get(BuiltinIntegerWidth::pointer(),
                                          C);
  BuiltinGenericSignatureBuilder builder(C);
  builder.addParameter(makeGenericParam(), ValueOwnership::Owned);
  builder.addParameter(makeConcrete(wordType));
  builder.setResult(makeConcrete(C.TheBridgeObjectType));
  return builder.build(Id);
}

static ValueDecl *getCastFromBridgeObjectOperation(ASTContext &C,
                                                   Identifier Id,
                                                   BuiltinValueKind BV) {
  Type BridgeTy = C.TheBridgeObjectType;
  switch (BV) {
  case BuiltinValueKind::CastReferenceFromBridgeObject: {
    BuiltinGenericSignatureBuilder builder(C);
    builder.addParameter(makeConcrete(BridgeTy), ValueOwnership::Owned);
    builder.setResult(makeGenericParam());
    return builder.build(Id);
  }

  case BuiltinValueKind::CastBitPatternFromBridgeObject: {
    Type WordTy = BuiltinIntegerType::get(BuiltinIntegerWidth::pointer(), C);
    return getBuiltinFunction(Id, { BridgeTy }, WordTy);
  }

  default:
    llvm_unreachable("not a cast from bridge object op");
  }
}

/// ClassifyBridgeObject has type:
///      (Builtin.BridgeObject) -> (Builtin.Int1, Builtin.Int1).
static ValueDecl *getClassifyBridgeObject(ASTContext &C, Identifier Id) {
  Type int1Ty = BuiltinIntegerType::get(1, C);
  Type resultTy = TupleType::get({
    TupleTypeElt(int1Ty, C.getIdentifier("isObjCObject")),
    TupleTypeElt(int1Ty, C.getIdentifier("isObjCTaggedPointer"))
  }, C);

  return getBuiltinFunction(Id, { C.TheBridgeObjectType }, resultTy);
}

static ValueDecl *getValueToBridgeObject(ASTContext &C, Identifier Id) {
  BuiltinGenericSignatureBuilder builder(C);
  builder.addParameter(makeGenericParam(0));
  builder.setResult(makeConcrete(C.TheBridgeObjectType));
  return builder.build(Id);
}

static ValueDecl *getUnsafeGuaranteed(ASTContext &C, Identifier Id) {
  // <T : AnyObject> T -> (T, Int8Ty)
  //
  BuiltinGenericSignatureBuilder builder(C);
  auto T = makeGenericParam();
  builder.addParameter(T);
  Type Int8Ty = BuiltinIntegerType::get(8, C);
  builder.setResult(makeTuple(T, makeConcrete(Int8Ty)));
  return builder.build(Id);
}

static ValueDecl *getUnsafeGuaranteedEnd(ASTContext &C, Identifier Id) {
  // Int8Ty -> ()
  Type Int8Ty = BuiltinIntegerType::get(8, C);
  return getBuiltinFunction(Id, { Int8Ty }, TupleType::getEmpty(C));
}

static ValueDecl *getOnFastPath(ASTContext &Context, Identifier Id) {
  return getBuiltinFunction(Id, {}, TupleType::getEmpty(Context));
}

static ValueDecl *getCastReferenceOperation(ASTContext &ctx,
                                            Identifier name) {
  // <T, U> T -> U
  // SILGen and IRGen check additional constraints during lowering.
  BuiltinGenericSignatureBuilder builder(ctx, 2);
  builder.addParameter(makeGenericParam(0), ValueOwnership::Owned);
  builder.setResult(makeGenericParam(1));
  return builder.build(name);
}

static ValueDecl *getReinterpretCastOperation(ASTContext &ctx,
                                              Identifier name) {
  // <T, U> T -> U
  // SILGen and IRGen check additional constraints during lowering.
  BuiltinGenericSignatureBuilder builder(ctx, 2);
  builder.addParameter(makeGenericParam(0), ValueOwnership::Owned);
  builder.setResult(makeGenericParam(1));
  return builder.build(name);
}

static ValueDecl *getZeroInitializerOperation(ASTContext &Context,
                                             Identifier Id) {
  // <T> () -> T
  BuiltinGenericSignatureBuilder builder(Context);
  builder.setResult(makeGenericParam());
  return builder.build(Id);
}

static ValueDecl *getGetObjCTypeEncodingOperation(ASTContext &Context,
                                                  Identifier Id) {
  // <T> T.Type -> RawPointer
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(Context.TheRawPointerType));
  return builder.build(Id);
}

// SWIFT_ENABLE_TENSORFLOW
static ValueDecl *getTensorFlowSend(ASTContext &Context, Identifier Id) {
  // <T> (T) -> ()
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeGenericParam());
  builder.setResult(makeConcrete(Context.TheEmptyTupleType));
  return builder.build(Id);
}

static ValueDecl *getTensorFlowReceive(ASTContext &Context, Identifier Id) {
  // <T> () -> (T)
  BuiltinGenericSignatureBuilder builder(Context);
  builder.setResult(makeGenericParam());
  return builder.build(Id);
}

static ValueDecl *getAutoDiffCreateTape(ASTContext &Context, Identifier Id) {
  // <T> () -> (Swift._AutoDiffTape<T>)
  BuiltinGenericSignatureBuilder builder(Context, 1);
  auto *tapeDecl = Context.get_AutoDiffTapeDecl();
  builder.setResult(
    makeBoundGeneric(tapeDecl, makeConcrete(Type()), makeGenericParam()));
  return builder.build(Id);
}

static ValueDecl *getAutoDiffPushToTape(ASTContext &Context, Identifier Id) {
  // <T> (Swift._AutoDiffTape<T>, T, Builtin.Word) -> ()
  BuiltinGenericSignatureBuilder builder(Context, 1);
  auto *tapeDecl = Context.get_AutoDiffTapeDecl();
  auto T = makeGenericParam();
  builder.addParameter(makeBoundGeneric(tapeDecl, makeConcrete(Type()), T));
  builder.addParameter(T);
  builder.addParameter(makeConcrete(BuiltinIntegerType::getWordType(Context)));
  builder.setResult(makeConcrete(Context.TheEmptyTupleType));
  return builder.build(Id);
}

static ValueDecl *getAutoDiffPopFromTape(ASTContext &Context, Identifier Id) {
  // <T> (Swift._AutoDiffTape<T>, Builtin.Word) -> (T)
  BuiltinGenericSignatureBuilder builder(Context, 1);
  auto *tapeDecl = Context.get_AutoDiffTapeDecl();
  auto T = makeGenericParam();
  builder.addParameter(makeBoundGeneric(tapeDecl, makeConcrete(Type()), T));
  builder.addParameter(makeConcrete(BuiltinIntegerType::getWordType(Context)));
  builder.setResult(T);
  return builder.build(Id);
}

static ValueDecl *getAutoDiffDestroyTape(ASTContext &Context, Identifier Id) {
  // <T> (Swift._AutoDiffTape<T>) -> ()
  BuiltinGenericSignatureBuilder builder(Context, 1);
  auto *tapeDecl = Context.get_AutoDiffTapeDecl();
  builder.addParameter(
    makeBoundGeneric(tapeDecl, makeConcrete(Type()), makeGenericParam()));
  builder.setResult(makeConcrete(Context.TheEmptyTupleType));
  return builder.build(Id);
}

static ValueDecl *getPoundAssert(ASTContext &Context, Identifier Id) {
  auto int1Type = BuiltinIntegerType::get(1, Context);
  auto optionalRawPointerType = BoundGenericEnumType::get(
      Context.getOptionalDecl(), Type(), {Context.TheRawPointerType});
  return getBuiltinFunction(Id, {int1Type, optionalRawPointerType},
                            Context.TheEmptyTupleType);
}

static ValueDecl *getTSanInoutAccess(ASTContext &Context, Identifier Id) {
  // <T> T -> ()
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeGenericParam());
  builder.setResult(makeConcrete(Context.TheEmptyTupleType));
  return builder.build(Id);
}

static ValueDecl *getAddressOfOperation(ASTContext &Context, Identifier Id) {
  // <T> (@inout T) -> RawPointer
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeGenericParam(), ValueOwnership::InOut);
  builder.setResult(makeConcrete(Context.TheRawPointerType));
  return builder.build(Id);
}

static ValueDecl *getAddressOfBorrowOperation(ASTContext &Context,
                                              Identifier Id) {
  // <T> (T) -> RawPointer
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeGenericParam());
  builder.setResult(makeConcrete(Context.TheRawPointerType));
  return builder.build(Id);
}

static ValueDecl *getTypeJoinOperation(ASTContext &Context, Identifier Id) {
  // <T,U,V> (T.Type, U.Type) -> V.Type
  BuiltinGenericSignatureBuilder builder(Context, 3);
  builder.addParameter(makeMetatype(makeGenericParam(0)));
  builder.addParameter(makeMetatype(makeGenericParam(1)));
  builder.setResult(makeMetatype(makeGenericParam(2)));
  return builder.build(Id);
}

static ValueDecl *getTypeJoinInoutOperation(ASTContext &Context,
                                            Identifier Id) {
  // <T,U,V> (inout T, U.Type) -> V.Type
  BuiltinGenericSignatureBuilder builder(Context, 3);
  builder.addParameter(makeGenericParam(0), ValueOwnership::InOut);
  builder.addParameter(makeMetatype(makeGenericParam(1)));
  builder.setResult(makeMetatype(makeGenericParam(2)));
  return builder.build(Id);
}

static ValueDecl *getTypeJoinMetaOperation(ASTContext &Context, Identifier Id) {
  // <T,U,V> (T.Type, U.Type) -> V.Type
  BuiltinGenericSignatureBuilder builder(Context, 3);
  builder.addParameter(makeMetatype(makeGenericParam(0)));
  builder.addParameter(makeMetatype(makeGenericParam(1)));
  builder.setResult(makeMetatype(makeGenericParam(2)));
  return builder.build(Id);
}

static ValueDecl *getCanBeObjCClassOperation(ASTContext &Context,
                                             Identifier Id) {
  // <T> T.Type -> Builtin.Int8
  BuiltinGenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(BuiltinIntegerType::get(8, Context)));
  return builder.build(Id);
}

static ValueDecl *getCondFailOperation(ASTContext &C, Identifier Id) {
  // Int1 -> ()
  auto CondTy = BuiltinIntegerType::get(1, C);
  auto VoidTy = TupleType::getEmpty(C);
  return getBuiltinFunction(Id, {CondTy}, VoidTy);
}

static ValueDecl *getAssertConfOperation(ASTContext &C, Identifier Id) {
  // () -> Int32
  auto Int32Ty = BuiltinIntegerType::get(32, C);
  return getBuiltinFunction(Id, {}, Int32Ty);
}

static ValueDecl *getFixLifetimeOperation(ASTContext &C, Identifier Id) {
  // <T> T -> ()
  BuiltinGenericSignatureBuilder builder(C);
  builder.addParameter(makeGenericParam());
  builder.setResult(makeConcrete(TupleType::getEmpty(C)));
  return builder.build(Id);
}

static ValueDecl *getExtractElementOperation(ASTContext &Context, Identifier Id,
                                             Type FirstTy, Type SecondTy) {
  // (Vector<N, T>, Int32) -> T
  auto VecTy = FirstTy->getAs<BuiltinVectorType>();
  if (!VecTy)
    return nullptr;

  auto IndexTy = SecondTy->getAs<BuiltinIntegerType>();
  if (!IndexTy || !IndexTy->isFixedWidth() || IndexTy->getFixedWidth() != 32)
    return nullptr;

  Type ResultTy = VecTy->getElementType();
  return getBuiltinFunction(Id, { VecTy, IndexTy }, ResultTy);
}

static ValueDecl *getInsertElementOperation(ASTContext &Context, Identifier Id,
                                            Type FirstTy, Type SecondTy,
                                            Type ThirdTy) {
  // (Vector<N, T>, T, Int32) -> Vector<N, T>
  auto VecTy = FirstTy->getAs<BuiltinVectorType>();
  if (!VecTy)
    return nullptr;
  auto ElementTy = VecTy->getElementType();

  if (!SecondTy->isEqual(ElementTy))
    return nullptr;

  auto IndexTy = ThirdTy->getAs<BuiltinIntegerType>();
  if (!IndexTy || !IndexTy->isFixedWidth() || IndexTy->getFixedWidth() != 32)
    return nullptr;

  Type ArgElts[] = { VecTy, ElementTy, IndexTy };
  return getBuiltinFunction(Id, ArgElts, VecTy);
}

static ValueDecl *getStaticReportOperation(ASTContext &Context, Identifier Id) {
  auto BoolTy = BuiltinIntegerType::get(1, Context);
  auto MessageTy = Context.TheRawPointerType;

  Type ArgElts[] = { BoolTy, BoolTy, MessageTy };
  Type ResultTy = TupleType::getEmpty(Context);
  
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getCheckedTruncOperation(ASTContext &Context,
                                                Identifier Id,
                                                Type InputTy,
                                                Type OutputTy) {
  auto InTy = InputTy->getAs<BuiltinIntegerType>();
  auto OutTy = OutputTy->getAs<BuiltinIntegerType>();
  if (!InTy || !OutTy)
    return nullptr;
  if (InTy->getLeastWidth() < OutTy->getGreatestWidth())
    return nullptr;

  Type OverflowBitTy = BuiltinIntegerType::get(1, Context);
  TupleTypeElt ResultElts[] = { Type(OutTy), OverflowBitTy };
  Type ResultTy = TupleType::get(ResultElts, Context);
  return getBuiltinFunction(Id, { InTy }, ResultTy);
}

static ValueDecl *getCheckedConversionOperation(ASTContext &Context,
                                                Identifier Id,
                                                Type Ty) {
  Type BuiltinTy = Ty->getAs<BuiltinIntegerType>();
  if (!BuiltinTy)
    return nullptr;

  Type SignErrorBitTy = BuiltinIntegerType::get(1, Context);
  TupleTypeElt ResultElts[] = { BuiltinTy, SignErrorBitTy };
  Type ResultTy = TupleType::get(ResultElts, Context);
  return getBuiltinFunction(Id, { BuiltinTy }, ResultTy);
}

static ValueDecl *getIntToFPWithOverflowOperation(ASTContext &Context,
                                                  Identifier Id, Type InputTy,
                                                  Type OutputTy) {
  auto InTy = InputTy->getAs<BuiltinIntegerType>();
  auto OutTy = OutputTy->getAs<BuiltinFloatType>();
  if (!InTy || !OutTy)
    return nullptr;

  return getBuiltinFunction(Id, { InTy }, OutTy);
}

static ValueDecl *getUnreachableOperation(ASTContext &Context,
                                          Identifier Id) {
  auto NeverTy = Context.getNeverType();
  if (!NeverTy)
    return nullptr;

  // () -> Never
  return getBuiltinFunction(Id, {}, NeverTy);
}

static ValueDecl *getOnceOperation(ASTContext &Context,
                                   Identifier Id,
                                   bool withContext) {
  // (RawPointer, @convention(c) ([Context]) -> ()[, Context]) -> ()
  
  auto HandleTy = Context.TheRawPointerType;
  auto VoidTy = Context.TheEmptyTupleType;
  auto Thin = FunctionType::ExtInfo(FunctionTypeRepresentation::CFunctionPointer,
                                    /*throws*/ false);
  if (withContext) {
    auto ContextTy = Context.TheRawPointerType;
    auto ContextArg = FunctionType::Param(ContextTy);
    auto BlockTy = FunctionType::get({ContextArg}, VoidTy, Thin);
    return getBuiltinFunction(Id, {HandleTy, BlockTy, ContextTy}, VoidTy);
  } else {
    auto BlockTy = FunctionType::get({}, VoidTy, Thin);
    return getBuiltinFunction(Id, {HandleTy, BlockTy}, VoidTy);
  }
}

/// An array of the overloaded builtin kinds.
static const OverloadedBuiltinKind OverloadedBuiltinKinds[] = {
  OverloadedBuiltinKind::None,

// There's deliberately no BUILTIN clause here so that we'll blow up
// if new builtin categories are added there and not here.
#define BUILTIN_CAST_OPERATION(id, attrs, name) \
   OverloadedBuiltinKind::Special,
#define BUILTIN_CAST_OR_BITCAST_OPERATION(id, attrs, name) \
   OverloadedBuiltinKind::Special,
#define BUILTIN_BINARY_OPERATION(id, name, attrs, overload) \
   OverloadedBuiltinKind::overload,
#define BUILTIN_BINARY_OPERATION_WITH_OVERFLOW(id, name, _, attrs, overload) \
   OverloadedBuiltinKind::overload,
#define BUILTIN_BINARY_PREDICATE(id, name, attrs, overload) \
   OverloadedBuiltinKind::overload,
#define BUILTIN_UNARY_OPERATION(id, name, attrs, overload) \
   OverloadedBuiltinKind::overload,
#define BUILTIN_SIL_OPERATION(id, name, overload) \
   OverloadedBuiltinKind::overload,
#define BUILTIN_MISC_OPERATION(id, name, attrs, overload) \
   OverloadedBuiltinKind::overload,
#define BUILTIN_SANITIZER_OPERATION(id, name, attrs) \
  OverloadedBuiltinKind::None,
#define BUILTIN_TYPE_CHECKER_OPERATION(id, name) OverloadedBuiltinKind::Special,
#define BUILTIN_TYPE_TRAIT_OPERATION(id, name) \
   OverloadedBuiltinKind::Special,
#define BUILTIN_RUNTIME_CALL(id, attrs, name) \
  OverloadedBuiltinKind::Special,
#include "swift/AST/Builtins.def"
};

/// Determines if a builtin type falls within the given category.
inline bool isBuiltinTypeOverloaded(Type T, OverloadedBuiltinKind OK) {
  switch (OK) {
  case OverloadedBuiltinKind::None:
    return false;  // always fail. 
  case OverloadedBuiltinKind::Integer:
    return T->is<BuiltinIntegerType>();
  case OverloadedBuiltinKind::IntegerOrVector:
    return T->is<BuiltinIntegerType>() ||
           (T->is<BuiltinVectorType>() &&
            T->castTo<BuiltinVectorType>()->getElementType()
              ->is<BuiltinIntegerType>());
  case OverloadedBuiltinKind::IntegerOrRawPointer:
    return T->is<BuiltinIntegerType>() || T->is<BuiltinRawPointerType>();
  case OverloadedBuiltinKind::IntegerOrRawPointerOrVector:
    return T->is<BuiltinIntegerType>() || T->is<BuiltinRawPointerType>() ||
           (T->is<BuiltinVectorType>() &&
            T->castTo<BuiltinVectorType>()->getElementType()
              ->is<BuiltinIntegerType>());
  case OverloadedBuiltinKind::Float:
    return T->is<BuiltinFloatType>();
  case OverloadedBuiltinKind::FloatOrVector:
    return T->is<BuiltinFloatType>() ||
           (T->is<BuiltinVectorType>() &&
            T->castTo<BuiltinVectorType>()->getElementType()
              ->is<BuiltinFloatType>());
  case OverloadedBuiltinKind::Special:
    return true;
  }
  llvm_unreachable("bad overloaded builtin kind");
}

/// Table of string intrinsic names indexed by enum value.
static const char *const IntrinsicNameTable[] = {
    "not_intrinsic",
#define GET_INTRINSIC_NAME_TABLE
#include "llvm/IR/IntrinsicImpl.inc"
#undef GET_INTRINSIC_NAME_TABLE
};

#define GET_INTRINSIC_TARGET_DATA
#include "llvm/IR/IntrinsicImpl.inc"
#undef GET_INTRINSIC_TARGET_DATA

/// getLLVMIntrinsicID - Given an LLVM IR intrinsic name with argument types
/// removed (e.g. like "bswap") return the LLVM IR IntrinsicID for the intrinsic
/// or 0 if the intrinsic name doesn't match anything.
unsigned swift::getLLVMIntrinsicID(StringRef InName) {
  using namespace llvm;

  // Swift intrinsic names start with int_.
  if (!InName.startswith("int_"))
    return llvm::Intrinsic::not_intrinsic;
  InName = InName.drop_front(strlen("int_"));
  
  // Prepend "llvm." and change _ to . in name.
  SmallString<128> NameS;
  NameS.append("llvm.");
  for (char C : InName)
    NameS.push_back(C == '_' ? '.' : C);

  const char *Name = NameS.c_str();
  ArrayRef<const char *> NameTable(&IntrinsicNameTable[1],
                                   TargetInfos[1].Offset);
  int Idx = Intrinsic::lookupLLVMIntrinsicByName(NameTable, Name);
  return static_cast<Intrinsic::ID>(Idx + 1);
}

llvm::Intrinsic::ID
swift::getLLVMIntrinsicIDForBuiltinWithOverflow(BuiltinValueKind ID) {
  switch (ID) {
    default: break;
    case BuiltinValueKind::SAddOver:
      return llvm::Intrinsic::sadd_with_overflow;
    case BuiltinValueKind::UAddOver:
      return llvm::Intrinsic::uadd_with_overflow;
    case BuiltinValueKind::SSubOver:
      return llvm::Intrinsic::ssub_with_overflow;
    case BuiltinValueKind::USubOver:
      return llvm::Intrinsic::usub_with_overflow;
    case BuiltinValueKind::SMulOver:
      return llvm::Intrinsic::smul_with_overflow;
    case BuiltinValueKind::UMulOver:
      return llvm::Intrinsic::umul_with_overflow;
  }
  llvm_unreachable("Cannot convert the overflow builtin to llvm intrinsic.");
}

static Type DecodeIntrinsicType(ArrayRef<llvm::Intrinsic::IITDescriptor> &Table,
                                ArrayRef<Type> Tys, ASTContext &Context) {
  typedef llvm::Intrinsic::IITDescriptor IITDescriptor;
  IITDescriptor D = Table.front();
  Table = Table.slice(1);
  switch (D.Kind) {
  default:
    llvm_unreachable("Unhandled case");
  case IITDescriptor::Half:
  case IITDescriptor::MMX:
  case IITDescriptor::Metadata:
  case IITDescriptor::Vector:
  case IITDescriptor::ExtendArgument:
  case IITDescriptor::TruncArgument:
  case IITDescriptor::VarArg:
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
      
      Elts.push_back(T);
    }
    return TupleType::get(Elts, Context);
  }
  }
  llvm_unreachable("unhandled");
}

/// \returns true on success, false on failure.
static bool
getSwiftFunctionTypeForIntrinsic(unsigned iid, ArrayRef<Type> TypeArgs,
                                 ASTContext &Context,
                                 SmallVectorImpl<Type> &ArgElts,
                                 Type &ResultTy, FunctionType::ExtInfo &Info) {
  llvm::Intrinsic::ID ID = (llvm::Intrinsic::ID)iid;
  
  typedef llvm::Intrinsic::IITDescriptor IITDescriptor;
  SmallVector<IITDescriptor, 8> Table;
  getIntrinsicInfoTableEntries(ID, Table);

  ArrayRef<IITDescriptor> TableRef = Table;

  // Decode the intrinsic's LLVM IR type, and map it to swift builtin types.
  ResultTy = DecodeIntrinsicType(TableRef, TypeArgs, Context);
  if (!ResultTy)
    return false;

  while (!TableRef.empty()) {
    Type ArgTy = DecodeIntrinsicType(TableRef, TypeArgs, Context);
    if (!ArgTy)
      return false;
    ArgElts.push_back(ArgTy);
  }
  
  // Translate LLVM function attributes to Swift function attributes.
  llvm::AttributeList attrs =
      llvm::Intrinsic::getAttributes(getGlobalLLVMContext(), ID);
  Info = FunctionType::ExtInfo();
  if (attrs.hasAttribute(llvm::AttributeList::FunctionIndex,
                         llvm::Attribute::NoReturn)) {
    ResultTy = Context.getNeverType();
    if (!ResultTy)
      return false;
  }
  
  return true;
}

static bool isValidFenceOrdering(StringRef Ordering) {
  return Ordering == "acquire" || Ordering == "release" ||
         Ordering == "acqrel" || Ordering == "seqcst";
}

static bool isValidRMWOrdering(StringRef Ordering) {
  return Ordering == "unordered" || Ordering == "monotonic" ||
         Ordering == "acquire" || Ordering == "release" ||
         Ordering == "acqrel" || Ordering == "seqcst";
}

static bool isValidLoadOrdering(StringRef Ordering) {
  return Ordering == "unordered" || Ordering == "monotonic" ||
         Ordering == "acquire" ||
         Ordering == "seqcst";
}

static bool isValidStoreOrdering(StringRef Ordering) {
  return Ordering == "unordered" || Ordering == "monotonic" ||
         Ordering == "release" ||
         Ordering == "seqcst";
}

llvm::AtomicOrdering swift::decodeLLVMAtomicOrdering(StringRef O) {
  using namespace llvm;
  return StringSwitch<AtomicOrdering>(O)
    .Case("unordered", AtomicOrdering::Unordered)
    .Case("monotonic", AtomicOrdering::Monotonic)
    .Case("acquire", AtomicOrdering::Acquire)
    .Case("release", AtomicOrdering::Release)
    .Case("acqrel", AtomicOrdering::AcquireRelease)
    .Case("seqcst", AtomicOrdering::SequentiallyConsistent)
    .Default(AtomicOrdering::NotAtomic);
}

static bool isUnknownOrUnordered(llvm::AtomicOrdering ordering) {
  using namespace llvm;
  switch (ordering) {
  case AtomicOrdering::NotAtomic:
  case AtomicOrdering::Unordered:
    return true;

  case AtomicOrdering::Monotonic:
  case AtomicOrdering::Acquire:
  case AtomicOrdering::Release:
  case AtomicOrdering::AcquireRelease:
  case AtomicOrdering::SequentiallyConsistent:
    return false;
  }

  llvm_unreachable("Unhandled AtomicOrdering in switch.");
}

static bool isValidCmpXChgOrdering(StringRef SuccessString, 
                                   StringRef FailureString) {
  using namespace llvm;
  AtomicOrdering SuccessOrdering = decodeLLVMAtomicOrdering(SuccessString);
  AtomicOrdering FailureOrdering = decodeLLVMAtomicOrdering(FailureString);

  // Unordered and unknown values are not allowed.
  if (isUnknownOrUnordered(SuccessOrdering) ||
      isUnknownOrUnordered(FailureOrdering))
    return false;
  // Success must be at least as strong as failure.
  if (!isAtLeastOrStrongerThan(SuccessOrdering, FailureOrdering))
    return false;
  // Failure may not release because no store occurred.
  if (FailureOrdering == AtomicOrdering::Release ||
      FailureOrdering == AtomicOrdering::AcquireRelease)
    return false;

  return true;
}

ValueDecl *swift::getBuiltinValueDecl(ASTContext &Context, Identifier Id) {
  SmallVector<Type, 4> Types;
  StringRef OperationName = getBuiltinBaseName(Context, Id.str(), Types);

  // If this is the name of an LLVM intrinsic, cons up a swift function with a
  // type that matches the IR types.
  if (unsigned ID = getLLVMIntrinsicID(OperationName)) {
    SmallVector<Type, 8> ArgElts;
    Type ResultTy;
    FunctionType::ExtInfo Info;
    if (getSwiftFunctionTypeForIntrinsic(ID, Types, Context, ArgElts, ResultTy,
                                         Info))
      return getBuiltinFunction(Id, ArgElts, ResultTy, Info);
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

    // Get and validate the ordering arguments, which are both required.
    SmallVector<StringRef, 4> Parts;
    OperationName.split(Parts, "_");
    if (Parts.size() < 2)
      return nullptr;
    if (!isValidCmpXChgOrdering(Parts[0], Parts[1]))
      return nullptr;
    auto NextPart = Parts.begin() + 2;

    // Accept weak, volatile, and singlethread if present.
    if (NextPart != Parts.end() && *NextPart == "weak")
      NextPart++;
    if (NextPart != Parts.end() && *NextPart == "volatile")
      NextPart++;
    if (NextPart != Parts.end() && *NextPart == "singlethread")
      NextPart++;
    // Nothing else is allowed in the name.
    if (NextPart != Parts.end())
      return nullptr;
    return getCmpXChgOperation(Context, Id, T);
  }

  // If this starts with atomicrmw, we have special suffixes to handle.
  if (OperationName.startswith("atomicrmw_")) {
    OperationName = OperationName.drop_front(strlen("atomicrmw_"));
    
    // Verify we have a single integer or pointer type.
    if (Types.size() != 1) return nullptr;
    Type Ty = Types[0];
    if (!Ty->is<BuiltinIntegerType>() && !Ty->is<BuiltinRawPointerType>())
      return nullptr;
    
    // Get and validate the suboperation name, which is required.
    auto Underscore = OperationName.find('_');
    if (Underscore == StringRef::npos) return nullptr;
    StringRef SubOp = OperationName.substr(0, Underscore);
    if (SubOp != "xchg" && SubOp != "add" && SubOp != "sub" && SubOp != "and" &&
        SubOp != "nand" && SubOp != "or" && SubOp != "xor" && SubOp != "max" &&
        SubOp != "min" && SubOp != "umax" && SubOp != "umin")
      return nullptr;
    OperationName = OperationName.drop_front(Underscore+1);
    
    // Get and validate the ordering argument, which is required.
    Underscore = OperationName.find('_');
    if (!isValidRMWOrdering(OperationName.substr(0, Underscore)))
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
    return getAtomicRMWOperation(Context, Id, Ty);
  }

  // If this starts with atomicload or atomicstore, we have special suffixes to
  // handle.
  if (OperationName.startswith("atomicload_")) {
    OperationName = OperationName.drop_front(strlen("atomicload_"));

    // Verify we have a single integer, floating point, or pointer type.
    if (Types.size() != 1) return nullptr;
    Type T = Types[0];
    if (!T->is<BuiltinIntegerType>() && !T->is<BuiltinRawPointerType>() &&
        !T->is<BuiltinFloatType>())
      return nullptr;

    // Get and validate the ordering argument, which is required.
    auto Underscore = OperationName.find('_');
    if (!isValidLoadOrdering(OperationName.substr(0, Underscore)))
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
    return getAtomicLoadOperation(Context, Id, T);
  }
  if (OperationName.startswith("atomicstore_")) {
    OperationName = OperationName.drop_front(strlen("atomicstore_"));

    // Verify we have a single integer, floating point, or pointer type.
    if (Types.size() != 1) return nullptr;
    Type T = Types[0];
    if (!T->is<BuiltinIntegerType>() && !T->is<BuiltinRawPointerType>() &&
        !T->is<BuiltinFloatType>())
      return nullptr;

    // Get and validate the ordering argument, which is required.
    auto Underscore = OperationName.find('_');
    if (!isValidStoreOrdering(OperationName.substr(0, Underscore)))
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
    return getAtomicStoreOperation(Context, Id, T);
  }
  if (OperationName.startswith("allocWithTailElems_")) {
    OperationName = OperationName.drop_front(strlen("allocWithTailElems_"));
    int NumTailTypes = 0;
    if (OperationName.getAsInteger(10, NumTailTypes))
      return nullptr;

    return getAllocWithTailElemsOperation(Context, Id, NumTailTypes);
  }

  auto BV = llvm::StringSwitch<BuiltinValueKind>(OperationName)
#define BUILTIN(id, name, Attrs) .Case(name, BuiltinValueKind::id)
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
  case BuiltinValueKind::AtomicRMW:
  case BuiltinValueKind::AtomicLoad:
  case BuiltinValueKind::AtomicStore:
  case BuiltinValueKind::AllocWithTailElems:
    llvm_unreachable("Handled above");
  case BuiltinValueKind::None: return nullptr;

  case BuiltinValueKind::GepRaw:
    if (Types.size() != 1) return nullptr;
    return getGepRawOperation(Id, Types[0]);

  case BuiltinValueKind::StringObjectOr:
    if (Types.size() != 1)
      return nullptr;
    return getStringObjectOrOperation(Id, Types[0]);

  case BuiltinValueKind::Gep:
    if (Types.size() != 1) return nullptr;
    return getGepOperation(Context, Id, Types[0]);

  case BuiltinValueKind::GetTailAddr:
    if (Types.size() != 1) return nullptr;
    return getGetTailAddrOperation(Context, Id, Types[0]);

  case BuiltinValueKind::PerformInstantaneousReadAccess:
    if (!Types.empty()) return nullptr;
      return getPerformInstantaneousReadAccessOperation(Context, Id);

  case BuiltinValueKind::BeginUnpairedModifyAccess:
    if (!Types.empty()) return nullptr;
    return getBeginUnpairedAccessOperation(Context, Id);

  case BuiltinValueKind::EndUnpairedAccess:
    if (!Types.empty()) return nullptr;
    return getEndUnpairedAccessOperation(Context, Id);

#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_OPERATION(id, name, attrs, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    if (Types.size() != 1) return nullptr;
    return getBinaryOperation(Id, Types[0]);

#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_OPERATION_WITH_OVERFLOW(id, name, _, attrs, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
      if (Types.size() != 1) return nullptr;
      return getBinaryOperationWithOverflow(Id, Types[0]);

#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_PREDICATE(id, name, attrs, overload)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    if (Types.size() != 1) return nullptr;
    return getBinaryPredicate(Id, Types[0]);

#define BUILTIN(id, name, Attrs)
#define BUILTIN_UNARY_OPERATION(id, name, attrs, overload)   case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    if (Types.size() != 1) return nullptr;
    return getUnaryOperation(Id, Types[0]);
      
#define BUILTIN(id, name, Attrs)
#define BUILTIN_CAST_OPERATION(id, name, attrs)  case BuiltinValueKind::id:
#define BUILTIN_CAST_OR_BITCAST_OPERATION(id, name, attrs)  case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    return getCastOperation(Context, Id, BV, Types);

  case BuiltinValueKind::Retain:
  case BuiltinValueKind::Release:
  case BuiltinValueKind::Autorelease:
    if (!Types.empty()) return nullptr;
    return getRefCountingOperation(Context, Id);
      
  case BuiltinValueKind::Load:
  case BuiltinValueKind::LoadRaw:
  case BuiltinValueKind::LoadInvariant:
  case BuiltinValueKind::Take:
    if (!Types.empty()) return nullptr;
    return getLoadOperation(Context, Id);
      
  case BuiltinValueKind::Destroy:
    if (!Types.empty()) return nullptr;
    return getDestroyOperation(Context, Id);

  case BuiltinValueKind::Assign:
  case BuiltinValueKind::Init:
    if (!Types.empty()) return nullptr;
    return getStoreOperation(Context, Id);

  case BuiltinValueKind::DestroyArray:
    if (!Types.empty()) return nullptr;
    return getDestroyArrayOperation(Context, Id);
      
  case BuiltinValueKind::CopyArray:
  case BuiltinValueKind::TakeArrayNoAlias:
  case BuiltinValueKind::TakeArrayFrontToBack:
  case BuiltinValueKind::TakeArrayBackToFront:
  case BuiltinValueKind::AssignCopyArrayNoAlias:
  case BuiltinValueKind::AssignCopyArrayFrontToBack:
  case BuiltinValueKind::AssignCopyArrayBackToFront:
  case BuiltinValueKind::AssignTakeArray:
    if (!Types.empty()) return nullptr;
    return getTransferArrayOperation(Context, Id);

  case BuiltinValueKind::IsUnique:
  case BuiltinValueKind::IsUnique_native:
    if (!Types.empty()) return nullptr;
    return getIsUniqueOperation(Context, Id);

  case BuiltinValueKind::BindMemory:
    if (!Types.empty()) return nullptr;
    return getBindMemoryOperation(Context, Id);

  case BuiltinValueKind::ProjectTailElems:
    if (!Types.empty()) return nullptr;
    return getProjectTailElemsOperation(Context, Id);

  case BuiltinValueKind::Sizeof:
  case BuiltinValueKind::Strideof:
  case BuiltinValueKind::Alignof:
    return getSizeOrAlignOfOperation(Context, Id);

  case BuiltinValueKind::IsPOD:
    return getIsPODOperation(Context, Id);

  case BuiltinValueKind::IsBitwiseTakable:
    return getIsBitwiseTakable(Context, Id);

  case BuiltinValueKind::IsOptionalType:
    return getIsOptionalOperation(Context, Id);

  case BuiltinValueKind::IsSameMetatype:
    return getIsSameMetatypeOperation(Context, Id);

  case BuiltinValueKind::AllocRaw:
    return getAllocOperation(Context, Id);

  case BuiltinValueKind::DeallocRaw:
    return getDeallocOperation(Context, Id);

  case BuiltinValueKind::CastToNativeObject:
  case BuiltinValueKind::UnsafeCastToNativeObject:
  case BuiltinValueKind::CastFromNativeObject:
  case BuiltinValueKind::BridgeToRawPointer:
  case BuiltinValueKind::BridgeFromRawPointer:
    if (!Types.empty()) return nullptr;
    return getNativeObjectCast(Context, Id, BV);

  case BuiltinValueKind::CastToBridgeObject:
    if (!Types.empty()) return nullptr;
    return getCastToBridgeObjectOperation(Context, Id);
  case BuiltinValueKind::CastReferenceFromBridgeObject:
  case BuiltinValueKind::CastBitPatternFromBridgeObject:
    if (!Types.empty()) return nullptr;
    return getCastFromBridgeObjectOperation(Context, Id, BV);
      
  case BuiltinValueKind::CastReference:
    if (!Types.empty()) return nullptr;
    return getCastReferenceOperation(Context, Id);

  case BuiltinValueKind::ReinterpretCast:
    if (!Types.empty()) return nullptr;
    return getReinterpretCastOperation(Context, Id);
      
  case BuiltinValueKind::AddressOf:
    if (!Types.empty()) return nullptr;
    return getAddressOfOperation(Context, Id);

  case BuiltinValueKind::AddressOfBorrow:
    if (!Types.empty()) return nullptr;
    return getAddressOfBorrowOperation(Context, Id);

  case BuiltinValueKind::CondFail:
    return getCondFailOperation(Context, Id);

  case BuiltinValueKind::AssertConf:
    return getAssertConfOperation(Context, Id);
      
  case BuiltinValueKind::FixLifetime:
    return getFixLifetimeOperation(Context, Id);
      
  case BuiltinValueKind::CanBeObjCClass:
    return getCanBeObjCClassOperation(Context, Id);
      
  case BuiltinValueKind::CondUnreachable:
  case BuiltinValueKind::Unreachable:
    return getUnreachableOperation(Context, Id);
      
  case BuiltinValueKind::ZeroInitializer:
    return getZeroInitializerOperation(Context, Id);
      
  case BuiltinValueKind::Once:
  case BuiltinValueKind::OnceWithContext:
    return getOnceOperation(Context, Id, BV == BuiltinValueKind::OnceWithContext);

  case BuiltinValueKind::WillThrow:
  case BuiltinValueKind::ErrorInMain:
    return getVoidErrorOperation(Context, Id);

  case BuiltinValueKind::UnexpectedError:
    return getUnexpectedErrorOperation(Context, Id);

  case BuiltinValueKind::ExtractElement:
    if (Types.size() != 2) return nullptr;
    return getExtractElementOperation(Context, Id, Types[0], Types[1]);

  case BuiltinValueKind::InsertElement:
    if (Types.size() != 3) return nullptr;
    return getInsertElementOperation(Context, Id, Types[0], Types[1], Types[2]);

  case BuiltinValueKind::StaticReport:
    if (!Types.empty()) return nullptr;
    return getStaticReportOperation(Context, Id);

  case BuiltinValueKind::UToSCheckedTrunc:
  case BuiltinValueKind::SToSCheckedTrunc:
  case BuiltinValueKind::SToUCheckedTrunc:
  case BuiltinValueKind::UToUCheckedTrunc:
    if (Types.size() != 2) return nullptr;
    return getCheckedTruncOperation(Context, Id, Types[0], Types[1]);

  case BuiltinValueKind::SUCheckedConversion:
  case BuiltinValueKind::USCheckedConversion:
    if (Types.size() != 1) return nullptr;
    return getCheckedConversionOperation(Context, Id, Types[0]);

  case BuiltinValueKind::ClassifyBridgeObject:
    if (!Types.empty()) return nullptr;
    return getClassifyBridgeObject(Context, Id);
  case BuiltinValueKind::ValueToBridgeObject:
    if (!Types.empty())
      return nullptr;
    return getValueToBridgeObject(Context, Id);
  case BuiltinValueKind::UnsafeGuaranteed:
    return getUnsafeGuaranteed(Context, Id);

  case BuiltinValueKind::UnsafeGuaranteedEnd:
    return getUnsafeGuaranteedEnd(Context, Id);

  // SWIFT_ENABLE_TENSORFLOW
  case BuiltinValueKind::TensorFlowSend:
    return getTensorFlowSend(Context, Id);
  case BuiltinValueKind::TensorFlowReceive:
    return getTensorFlowReceive(Context, Id);
  case BuiltinValueKind::AutoDiffCreateTape:
    return getAutoDiffCreateTape(Context, Id);
  case BuiltinValueKind::AutoDiffPushToTape:
    return getAutoDiffPushToTape(Context, Id);
  case BuiltinValueKind::AutoDiffPopFromTape:
    return getAutoDiffPopFromTape(Context, Id);
  case BuiltinValueKind::AutoDiffDestroyTape:
    return getAutoDiffDestroyTape(Context, Id);
  case BuiltinValueKind::PoundAssert:
    return getPoundAssert(Context, Id);

  case BuiltinValueKind::OnFastPath:
    return getOnFastPath(Context, Id);

  case BuiltinValueKind::IntToFPWithOverflow:
    if (Types.size() != 2) return nullptr;
    return getIntToFPWithOverflowOperation(Context, Id, Types[0], Types[1]);

  case BuiltinValueKind::GetObjCTypeEncoding:
    return getGetObjCTypeEncodingOperation(Context, Id);

  case BuiltinValueKind::TSanInoutAccess:
    return getTSanInoutAccess(Context, Id);

  case BuiltinValueKind::Swift3ImplicitObjCEntrypoint:
    return getBuiltinFunction(Id,
                              {},
                              TupleType::getEmpty(Context));

  case BuiltinValueKind::TypeJoin:
    return getTypeJoinOperation(Context, Id);

  case BuiltinValueKind::TypeJoinInout:
    return getTypeJoinInoutOperation(Context, Id);

  case BuiltinValueKind::TypeJoinMeta:
    return getTypeJoinMetaOperation(Context, Id);      
  }

  llvm_unreachable("bad builtin value!");
}

StringRef swift::getBuiltinName(BuiltinValueKind ID) {
  switch (ID) {
  case BuiltinValueKind::None:
    llvm_unreachable("no builtin kind");
#define BUILTIN(Id, Name, Attrs) \
  case BuiltinValueKind::Id: \
    return Name;
#include "swift/AST/Builtins.def"
  }
  llvm_unreachable("bad BuiltinValueKind");
}
