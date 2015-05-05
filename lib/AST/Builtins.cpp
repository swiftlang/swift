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
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include <cstring>
#include <tuple>
using namespace swift;

struct BuiltinExtraInfoTy {
  const char *Attributes;
};

static const BuiltinExtraInfoTy BuiltinExtraInfo[] = {
  {0},
#define BUILTIN(Id, Name, Attrs) {Attrs},
#include "swift/AST/Builtins.def"
};

bool BuiltinInfo::isReadNone() const {
  return strchr(BuiltinExtraInfo[(unsigned)ID].Attributes, 'n') != 0;
}

bool IntrinsicInfo::hasAttribute(llvm::Attribute::AttrKind Kind) const {
  // FIXME: We should not be relying on the global LLVM context.
  llvm::AttributeSet attrs
    = llvm::Intrinsic::getAttributes(llvm::getGlobalContext(), ID);
  return (attrs.hasAttribute(llvm::AttributeSet::FunctionIndex, Kind));
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
getBuiltinFunction(Identifier Id,
                   ArrayRef<TupleTypeElt> ArgTypes,
                   Type ResType,
                   FunctionType::ExtInfo Info = FunctionType::ExtInfo()) {
  auto &Context = ResType->getASTContext();
  Type ArgType = TupleType::get(ArgTypes, Context);
  Type FnType;
  FnType = FunctionType::get(ArgType, ResType, Info);

  Module *M = Context.TheBuiltinModule;
  DeclContext *DC = &M->getMainFile(FileUnitKind::Builtin);

  SmallVector<TuplePatternElt, 4> ParamPatternElts;
  for (auto &ArgTupleElt : ArgTypes) {
    auto PD = new (Context) ParamDecl(/*IsLet*/true, SourceLoc(),
                                      Identifier(), SourceLoc(),
                                      Identifier(), ArgTupleElt.getType(),
                                      DC);
    PD->setImplicit();
    Pattern *Pat = new (Context) NamedPattern(PD, /*implicit=*/true);
    Pat = new (Context) TypedPattern(Pat,
            TypeLoc::withoutLoc(ArgTupleElt.getType()), /*implicit=*/true);
    PD->setParamParentPattern(Pat);

    ParamPatternElts.push_back(TuplePatternElt(Pat));
  }

  Pattern *ParamPattern = TuplePattern::createSimple(
      Context, SourceLoc(), ParamPatternElts, SourceLoc());

  llvm::SmallVector<Identifier, 2> ArgNames(ParamPattern->numTopLevelVariables(),
                                            Identifier());
  DeclName Name(Context, Id, ArgNames);
  auto FD = FuncDecl::create(Context, SourceLoc(), StaticSpellingKind::None,
                          SourceLoc(), Name, SourceLoc(), SourceLoc(),
                          /*GenericParams=*/nullptr, FnType, ParamPattern,
                          TypeLoc::withoutLoc(ResType),
                          DC);
  FD->setImplicit();
  FD->setAccessibility(Accessibility::Public);
  return FD;
}

/// Build a builtin function declaration.
static FuncDecl *
getBuiltinGenericFunction(Identifier Id,
                          ArrayRef<TupleTypeElt> ArgParamTypes,
                          ArrayRef<TupleTypeElt> ArgBodyTypes,
                          Type ResType,
                          Type ResBodyType,
                          GenericParamList *GenericParams,
                          FunctionType::ExtInfo Info = FunctionType::ExtInfo()) {
  assert(GenericParams && "Missing generic parameters");
  auto &Context = ResType->getASTContext();

  Type ArgParamType = TupleType::get(ArgParamTypes, Context);
  Type ArgBodyType = TupleType::get(ArgBodyTypes, Context);

  // Compute the function type.
  Type FnType = PolymorphicFunctionType::get(ArgBodyType, ResBodyType,
                                             GenericParams, Info);

  // Compute the interface type.
  SmallVector<GenericTypeParamType *, 1> GenericParamTypes;
  for (auto gp : *GenericParams) {
    GenericParamTypes.push_back(gp->getDeclaredType()
                                  ->castTo<GenericTypeParamType>());
  }
  // Create witness markers for all of the generic param types.
  SmallVector<Requirement, 2> requirements;
  for (auto param : GenericParamTypes) {
    requirements.push_back(Requirement(RequirementKind::WitnessMarker,
                                       param, Type()));
  }
  
  GenericSignature *Sig = GenericSignature::get(GenericParamTypes,requirements);
  
  Type InterfaceType = GenericFunctionType::get(Sig,
                                                ArgParamType, ResType,
                                                Info);

  Module *M = Context.TheBuiltinModule;
  DeclContext *DC = &M->getMainFile(FileUnitKind::Builtin);

  SmallVector<TuplePatternElt, 4> ParamPatternElts;
  for (auto &ArgTupleElt : ArgBodyTypes) {
    auto PD = new (Context) ParamDecl(/*IsLet*/true, SourceLoc(),
                                      Identifier(), SourceLoc(),
                                      Identifier(), ArgTupleElt.getType(),
                                      DC);
    PD->setImplicit();
    Pattern *Pat = new (Context) NamedPattern(PD, /*implicit=*/true);
    Pat = new (Context) TypedPattern(Pat,
            TypeLoc::withoutLoc(ArgTupleElt.getType()), /*implicit=*/true);
    PD->setParamParentPattern(Pat);

    ParamPatternElts.push_back(TuplePatternElt(Pat));
  }

  Pattern *ParamPattern = TuplePattern::createSimple(
                          Context, SourceLoc(), ParamPatternElts, SourceLoc());
  llvm::SmallVector<Identifier, 2> ArgNames(
                                     ParamPattern->numTopLevelVariables(),
                                     Identifier());
  DeclName Name(Context, Id, ArgNames);
  auto func = FuncDecl::create(Context, SourceLoc(), StaticSpellingKind::None,
                               SourceLoc(), Name,
                               SourceLoc(), SourceLoc(), GenericParams, FnType,
                               ParamPattern, TypeLoc::withoutLoc(ResBodyType),
                               DC);
    
  func->setInterfaceType(InterfaceType);
  func->setImplicit();
  func->setAccessibility(Accessibility::Public);

  return func;
}

/// Build a getelementptr operation declaration.
static ValueDecl *getGepOperation(Identifier Id, Type ArgType) {
  auto &Context = ArgType->getASTContext();
  
  // This is always "(i8*, IntTy) -> i8*"
  TupleTypeElt ArgElts[] = { Context.TheRawPointerType, ArgType };
  Type ResultTy = Context.TheRawPointerType;
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

/// Build a binary operation declaration.
static ValueDecl *getBinaryOperation(Identifier Id, Type ArgType) {
  TupleTypeElt ArgElts[] = { ArgType, ArgType };
  Type ResultTy = ArgType;
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

/// Build a declaration for a binary operation with overflow.
static ValueDecl *getBinaryOperationWithOverflow(Identifier Id,
                                                 Type ArgType) {
  auto &Context = ArgType->getASTContext();
  Type ShouldCheckForOverflowTy = BuiltinIntegerType::get(1, Context);
  TupleTypeElt ArgElts[] = { ArgType, ArgType, ShouldCheckForOverflowTy };
  Type OverflowBitTy = BuiltinIntegerType::get(1, Context);
  TupleTypeElt ResultElts[] = { ArgType, OverflowBitTy };
  Type ResultTy = TupleType::get(ResultElts, Context);
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getUnaryOperation(Identifier Id, Type ArgType) {
  TupleTypeElt ArgElts[] = { ArgType };
  Type ResultTy = ArgType;
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

/// Build a binary predicate declaration.
static ValueDecl *getBinaryPredicate(Identifier Id, Type ArgType) {
  auto &Context = ArgType->getASTContext();

  TupleTypeElt ArgElts[] = { ArgType, ArgType };
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
  if (Types.size() == 0 || Types.size() > 2) return nullptr;
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

  TupleTypeElt ArgElts[] = { Input };
  return getBuiltinFunction(Id, ArgElts, Output);
}

static const char * const GenericParamNames[] = {
  "T",
  "U",
};

static std::pair<ArchetypeType*, GenericTypeParamDecl*>
createGenericParam(ASTContext &ctx, const char *name, unsigned index) {
  Module *M = ctx.TheBuiltinModule;
  Identifier ident = ctx.getIdentifier(name);
  ArchetypeType *archetype
    = ArchetypeType::getNew(ctx, nullptr,
                            static_cast<AssociatedTypeDecl *>(nullptr),
                            ident, ArrayRef<Type>(), Type(), false);
  auto genericParam =
    new (ctx) GenericTypeParamDecl(&M->getMainFile(FileUnitKind::Builtin),
                                   ident, SourceLoc(), 0, index);
  genericParam->setArchetype(archetype);
  return { archetype, genericParam };
}

/// Create a generic parameter list with multiple generic parameters.
static GenericParamList *getGenericParams(ASTContext &ctx,
                                          unsigned numParameters,
                       SmallVectorImpl<ArchetypeType*> &archetypes,
                       SmallVectorImpl<GenericTypeParamDecl*> &genericParams) {
  assert(numParameters <= llvm::array_lengthof(GenericParamNames));
  assert(archetypes.empty() && genericParams.empty());

  for (unsigned i = 0; i != numParameters; ++i) {
    auto archetypeAndParam = createGenericParam(ctx, GenericParamNames[i], i);
    archetypes.push_back(archetypeAndParam.first);
    genericParams.push_back(archetypeAndParam.second);
  }

  auto paramList = GenericParamList::create(ctx, SourceLoc(), genericParams,
                                            SourceLoc());
  paramList->setAllArchetypes(ctx.AllocateCopy(archetypes));
  return paramList;
}

namespace {
  class GenericSignatureBuilder {
  public:
    ASTContext &Context;

  private:
    GenericParamList *TheGenericParamList;

    SmallVector<GenericTypeParamDecl*, 2> GenericTypeParams;
    SmallVector<ArchetypeType*, 2> Archetypes;

    SmallVector<TupleTypeElt, 4> InterfaceParams;
    SmallVector<TupleTypeElt, 4> BodyParams;

    Type InterfaceResult;
    Type BodyResult;

  public:
    GenericSignatureBuilder(ASTContext &ctx, unsigned numGenericParams = 1)
        : Context(ctx) {
      TheGenericParamList = getGenericParams(ctx, numGenericParams,
                                             Archetypes, GenericTypeParams);
    }

    template <class G>
    void addParameter(const G &generator) {
      InterfaceParams.push_back(generator.build(*this, false));
      BodyParams.push_back(generator.build(*this, true));
    }

    template <class G>
    void setResult(const G &generator) {
      InterfaceResult = generator.build(*this, false);
      BodyResult = generator.build(*this, true);
    }

    ValueDecl *build(Identifier name) {
      return getBuiltinGenericFunction(name, InterfaceParams, BodyParams,
                                       InterfaceResult, BodyResult,
                                       TheGenericParamList);
    }

    // Don't use these generator classes directly; call the make{...}
    // functions which follow this class.

    struct ConcreteGenerator {
      Type TheType;
      Type build(GenericSignatureBuilder &builder, bool forBody) const {
        return TheType;
      }
    };
    struct ParameterGenerator {
      unsigned Index;
      Type build(GenericSignatureBuilder &builder, bool forBody) const {
        return (forBody ? builder.Archetypes[Index]
                        : builder.GenericTypeParams[Index]->getDeclaredType());
      }
    };
    struct LambdaGenerator {
      std::function<Type(GenericSignatureBuilder &,bool)> TheFunction;
      Type build(GenericSignatureBuilder &builder, bool forBody) const {
        return TheFunction(builder, forBody);
      }
    };
    template <class T, class U>
    struct FunctionGenerator {
      T Arg;
      U Result;
      FunctionType::ExtInfo ExtInfo;
      Type build(GenericSignatureBuilder &builder, bool forBody) const {
        return FunctionType::get(Arg.build(builder, forBody),
                                 Result.build(builder, forBody),
                                 ExtInfo);
      }
    };
    template <class T>
    struct InOutGenerator {
      T Object;
      Type build(GenericSignatureBuilder &builder, bool forBody) const {
        return InOutType::get(Object.build(builder, forBody));
      }
    };
    template <class T>
    struct MetatypeGenerator {
      T Object;
      Optional<MetatypeRepresentation> Repr;
      Type build(GenericSignatureBuilder &builder, bool forBody) const {
        return MetatypeType::get(Object.build(builder, forBody), Repr);
      }
    };
  };
}

static GenericSignatureBuilder::ConcreteGenerator
makeConcrete(Type type) {
  return { type };
}

static GenericSignatureBuilder::ParameterGenerator
makeGenericParam(unsigned index = 0) {
  return { index };
}

template <class... Gs>
static GenericSignatureBuilder::LambdaGenerator
makeTuple(const Gs & ...elementGenerators) {
  return {
    [=](GenericSignatureBuilder &builder, bool forBody) -> Type {
      TupleTypeElt elts[] = {
        elementGenerators.build(builder, forBody)...
      };
      return TupleType::get(elts, builder.Context);
    }
  };
}

template <class T, class U>
static GenericSignatureBuilder::FunctionGenerator<T,U>
makeFunction(const T &arg, const U &result,
             FunctionType::ExtInfo extInfo = FunctionType::ExtInfo()) {
  return { arg, result, extInfo };
}

template <class T>
static GenericSignatureBuilder::InOutGenerator<T>
makeInOut(const T &object) {
  return { object };
}

template <class T>
static GenericSignatureBuilder::MetatypeGenerator<T>
makeMetatype(const T &object, Optional<MetatypeRepresentation> repr = None) {
  return { object, repr };
}

/// Create a function with type <T> T -> ().
static ValueDecl *getRefCountingOperation(ASTContext &Context, Identifier Id) {
  GenericSignatureBuilder builder(Context);
  builder.addParameter(makeGenericParam());
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getLoadOperation(ASTContext &Context, Identifier Id) {
  GenericSignatureBuilder builder(Context);
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.setResult(makeGenericParam());
  return builder.build(Id);
}

static ValueDecl *getStoreOperation(ASTContext &Context, Identifier Id) {
  GenericSignatureBuilder builder(Context);
  builder.addParameter(makeGenericParam());
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getDestroyOperation(ASTContext &Context, Identifier Id) {
  GenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getDestroyArrayOperation(ASTContext &Context, Identifier Id) {
  auto wordType = BuiltinIntegerType::get(BuiltinIntegerWidth::pointer(),
                                          Context);
  GenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.addParameter(makeConcrete(Context.TheRawPointerType));
  builder.addParameter(makeConcrete(wordType));
  builder.setResult(makeConcrete(TupleType::getEmpty(Context)));
  return builder.build(Id);
}

static ValueDecl *getTransferArrayOperation(ASTContext &Context, Identifier Id){
  auto wordType = BuiltinIntegerType::get(BuiltinIntegerWidth::pointer(),
                                          Context);
  GenericSignatureBuilder builder(Context);
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

  GenericSignatureBuilder builder(Context);
  builder.addParameter(makeInOut(makeGenericParam()));
  builder.setResult(makeConcrete(Int1Ty));
  return builder.build(Id);
}

static ValueDecl *getSizeOrAlignOfOperation(ASTContext &Context,
                                            Identifier Id) {
  GenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(BuiltinIntegerType::getWordType(Context)));
  return builder.build(Id);
}

static ValueDecl *getAllocOperation(ASTContext &Context, Identifier Id) {
  Type PtrSizeTy = BuiltinIntegerType::getWordType(Context);
  TupleTypeElt ArgElts[] = { PtrSizeTy, PtrSizeTy };
  Type ResultTy = Context.TheRawPointerType;
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getDeallocOperation(ASTContext &Context, Identifier Id) {
  auto PtrSizeTy = BuiltinIntegerType::getWordType(Context);
  TupleTypeElt ArgElts[] = { Context.TheRawPointerType, PtrSizeTy, PtrSizeTy };
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
                            TupleType::getEmpty(Context),
                            AnyFunctionType::ExtInfo().withIsNoReturn());
}

static ValueDecl *getCmpXChgOperation(ASTContext &Context, Identifier Id,
                                      Type T) {
  TupleTypeElt ArgElts[] = { Context.TheRawPointerType, T, T };
  Type BoolTy = BuiltinIntegerType::get(1, Context);
  TupleTypeElt ResultElts[] = { T, BoolTy };
  Type ResultTy = TupleType::get(ResultElts, Context);
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getAtomicRMWOperation(ASTContext &Context, Identifier Id,
                                        Type T) {
  TupleTypeElt ArgElts[] = { Context.TheRawPointerType, T };
  Type ResultTy = T;
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getNativeObjectCast(ASTContext &Context, Identifier Id,
                                      BuiltinValueKind BV) {
  Type BuiltinTy;
  if (BV == BuiltinValueKind::BridgeToRawPointer ||
      BV == BuiltinValueKind::BridgeFromRawPointer)
    BuiltinTy = Context.TheRawPointerType;
  else if (BV == BuiltinValueKind::CastToUnknownObject ||
           BV == BuiltinValueKind::CastFromUnknownObject)
    BuiltinTy = Context.TheUnknownObjectType;
  else
    BuiltinTy = Context.TheNativeObjectType;

  GenericSignatureBuilder builder(Context);
  if (BV == BuiltinValueKind::CastToNativeObject ||
      BV == BuiltinValueKind::CastToUnknownObject ||
      BV == BuiltinValueKind::BridgeToRawPointer) {
    builder.addParameter(makeGenericParam());
    builder.setResult(makeConcrete(BuiltinTy));
  } else {
    builder.addParameter(makeConcrete(BuiltinTy));
    builder.setResult(makeGenericParam());
  }
  return builder.build(Id);
}

static ValueDecl *getCastToBridgeObjectOperation(ASTContext &C,
                                                 Identifier Id) {
  auto wordType = BuiltinIntegerType::get(BuiltinIntegerWidth::pointer(),
                                          C);
  GenericSignatureBuilder builder(C);
  builder.addParameter(makeGenericParam());
  builder.addParameter(makeConcrete(wordType));
  builder.setResult(makeConcrete(C.TheBridgeObjectType));
  return builder.build(Id);
}

static ValueDecl *getCastFromBridgeObjectOperation(ASTContext &C,
                                                   Identifier Id,
                                                   BuiltinValueKind BV) {
  Type BridgeTy = C.TheBridgeObjectType;
  TupleTypeElt ArgElts[] = { BridgeTy };
  
  switch (BV) {
  case BuiltinValueKind::CastReferenceFromBridgeObject: {
    GenericSignatureBuilder builder(C);
    builder.addParameter(makeConcrete(BridgeTy));
    builder.setResult(makeGenericParam());
    return builder.build(Id);
  }

  case BuiltinValueKind::CastBitPatternFromBridgeObject: {
    Type WordTy = BuiltinIntegerType::get(BuiltinIntegerWidth::pointer(), C);
    return getBuiltinFunction(Id, ArgElts, WordTy);
  }
      
  default:
    llvm_unreachable("not a cast from bridge object op");
  }
}

static ValueDecl *getReinterpretCastOperation(ASTContext &ctx,
                                              Identifier name) {
  // <T, U> T -> U
  // SILGen and IRGen check additional constraints during lowering.
  GenericSignatureBuilder builder(ctx, 2);
  builder.addParameter(makeGenericParam(0));
  builder.setResult(makeGenericParam(1));
  return builder.build(name);
}

static ValueDecl *getMarkDependenceOperation(ASTContext &ctx, Identifier name) {
  // <T,U> (T,U) -> T
  GenericSignatureBuilder builder(ctx, 2);
  builder.addParameter(makeGenericParam(0));
  builder.addParameter(makeGenericParam(1));
  builder.setResult(makeGenericParam(0));
  return builder.build(name);
}

static ValueDecl *getZeroInitializerOperation(ASTContext &Context,
                                             Identifier Id) {
  // <T> () -> T
  GenericSignatureBuilder builder(Context);
  builder.setResult(makeGenericParam());
  return builder.build(Id);
}

static ValueDecl *getAddressOfOperation(ASTContext &Context, Identifier Id) {
  // <T> (@inout T) -> RawPointer
  GenericSignatureBuilder builder(Context);
  builder.addParameter(makeInOut(makeGenericParam()));
  builder.setResult(makeConcrete(Context.TheRawPointerType));
  return builder.build(Id);
}

static ValueDecl *getCanBeObjCClassOperation(ASTContext &Context,
                                             Identifier Id) {
  // <T> T.Type -> Builtin.Int8
  GenericSignatureBuilder builder(Context);
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(BuiltinIntegerType::get(8, Context)));
  return builder.build(Id);
}

static ValueDecl *getCondFailOperation(ASTContext &C, Identifier Id) {
  // Int1 -> ()
  auto CondTy = BuiltinIntegerType::get(1, C);
  auto VoidTy = TupleType::getEmpty(C);
  TupleTypeElt CondElt(CondTy);
  return getBuiltinFunction(Id, CondElt, VoidTy);
}

static ValueDecl *getAssertConfOperation(ASTContext &C, Identifier Id) {
  // () -> Int32
  auto Int32Ty = BuiltinIntegerType::get(32, C);
  auto VoidTy = TupleType::getEmpty(C);
  TupleTypeElt EmptyElt(VoidTy);
  return getBuiltinFunction(Id, EmptyElt, Int32Ty);
}

static ValueDecl *getFixLifetimeOperation(ASTContext &C, Identifier Id) {
  // <T> T -> ()
  GenericSignatureBuilder builder(C);
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

  TupleTypeElt ArgElts[] = { VecTy, IndexTy };
  Type ResultTy = VecTy->getElementType();
  return getBuiltinFunction(Id, ArgElts, ResultTy);
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

  TupleTypeElt ArgElts[] = { VecTy, ElementTy, IndexTy };
  Type ResultTy = VecTy;
  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getStaticReportOperation(ASTContext &Context, Identifier Id) {
  auto BoolTy = BuiltinIntegerType::get(1, Context);
  auto MessageTy = Context.TheRawPointerType;

  TupleTypeElt ArgElts[] = { BoolTy, BoolTy, MessageTy };
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

  TupleTypeElt ArgElts[] = { InTy };
  Type OverflowBitTy = BuiltinIntegerType::get(1, Context);
  TupleTypeElt ResultElts[] = { OutTy, OverflowBitTy };
  Type ResultTy = TupleType::get(ResultElts, Context);

  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getCheckedConversionOperation(ASTContext &Context,
                                                Identifier Id,
                                                Type Ty) {
  auto BuiltinTy = Ty->getAs<BuiltinIntegerType>();
  if (!BuiltinTy)
    return nullptr;

  TupleTypeElt ArgElts[] = { BuiltinTy };
  Type SignErrorBitTy = BuiltinIntegerType::get(1, Context);
  TupleTypeElt ResultElts[] = { BuiltinTy, SignErrorBitTy };
  Type ResultTy = TupleType::get(ResultElts, Context);

  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getIntToFPWithOverflowOperation(ASTContext &Context,
                                                  Identifier Id, Type InputTy,
                                                  Type OutputTy) {
  auto InTy = InputTy->getAs<BuiltinIntegerType>();
  auto OutTy = OutputTy->getAs<BuiltinFloatType>();
  if (!InTy || !OutTy)
    return nullptr;

  TupleTypeElt ArgElts[] = { InTy };
  Type ResultTy = OutTy;

  return getBuiltinFunction(Id, ArgElts, ResultTy);
}

static ValueDecl *getUnreachableOperation(ASTContext &Context,
                                          Identifier Id) {
  // @noreturn () -> ()
  auto VoidTy = Context.TheEmptyTupleType;
  return getBuiltinFunction(Id, {}, VoidTy,
                            AnyFunctionType::ExtInfo().withIsNoReturn(true));
}

static ValueDecl *getOnceOperation(ASTContext &Context,
                                   Identifier Id) {
  // (RawPointer, @convention(thin) () -> ()) -> ()
  
  auto HandleTy = Context.TheRawPointerType;
  auto VoidTy = Context.TheEmptyTupleType;
  auto Thin = FunctionType::ExtInfo(FunctionTypeRepresentation::Thin,
                                    /*noreturn*/ false, /*throws*/ false);
  
  auto BlockTy = FunctionType::get(VoidTy, VoidTy, Thin);
  
  TupleTypeElt InFields[] = {HandleTy, BlockTy};
  auto OutTy = VoidTy;
  
  return getBuiltinFunction(Id, InFields, OutTy);
}

static ValueDecl *getTryPinOperation(ASTContext &ctx, Identifier name) {
  // <T> NativeObject -> T
  // (T must actually be NativeObject?)
  GenericSignatureBuilder builder(ctx);
  builder.addParameter(makeConcrete(ctx.TheNativeObjectType));
  builder.setResult(makeGenericParam());
  return builder.build(name);
}

static ValueDecl *getProjectValueBufferOperation(ASTContext &ctx,
                                                 Identifier name) {
  // <T> (inout Builtin.UnsafeValueBuffer, T.Type) -> Builtin.RawPointer
  GenericSignatureBuilder builder(ctx);
  builder.addParameter(makeConcrete(
                              InOutType::get(ctx.TheUnsafeValueBufferType)));
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(ctx.TheRawPointerType));
  return builder.build(name);
}

static ValueDecl *getDeallocValueBufferOperation(ASTContext &ctx,
                                                 Identifier name) {
  // <T> (inout Builtin.UnsafeValueBuffer, T.Type) -> ()
  GenericSignatureBuilder builder(ctx);
  builder.addParameter(makeConcrete(
                              InOutType::get(ctx.TheUnsafeValueBufferType)));
  builder.addParameter(makeMetatype(makeGenericParam()));
  builder.setResult(makeConcrete(ctx.TheRawPointerType));
  return builder.build(name);
}

static ValueDecl *
getMakeMaterializeForSetCallbackOperation(ASTContext &ctx, Identifier name) {
  // <T>    ((Builtin.RawPointer,
  //          inout Builtin.UnsafeValueBuffer,
  //          inout T,
  //          T.Type) -> ())
  //      ->
  //  @convention(thin) ((Builtin.RawPointer,
  //          inout Builtin.UnsafeValueBuffer,
  //          inout T,
  //          @thick T.Type) -> ())
  GenericSignatureBuilder builder(ctx);
  builder.addParameter(
    makeFunction(
      makeTuple(makeConcrete(ctx.TheRawPointerType),
                makeConcrete(InOutType::get(ctx.TheUnsafeValueBufferType)),
                makeInOut(makeGenericParam()),
                makeMetatype(makeGenericParam())),
      makeConcrete(TupleType::getEmpty(ctx))));
  builder.setResult(
    makeFunction(
      makeTuple(makeConcrete(ctx.TheRawPointerType),
                makeConcrete(InOutType::get(ctx.TheUnsafeValueBufferType)),
                makeInOut(makeGenericParam()),
                makeMetatype(makeGenericParam(), MetatypeRepresentation::Thick)),
      makeConcrete(TupleType::getEmpty(ctx)),
      FunctionType::ExtInfo()
        .withRepresentation(FunctionType::Representation::Thin)));
  return builder.build(name);
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

/// getLLVMIntrinsicID - Given an LLVM IR intrinsic name with argument types
/// removed (e.g. like "bswap") return the LLVM IR IntrinsicID for the intrinsic
/// or 0 if the intrinsic name doesn't match anything.
unsigned swift::getLLVMIntrinsicID(StringRef InName, bool hasArgTypes) {
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
  if (hasArgTypes)
    NameS.push_back('.');
  
  const char *Name = NameS.c_str();
  unsigned Len = NameS.size();
#define GET_FUNCTION_RECOGNIZER
#include "llvm/IR/Intrinsics.gen"
#undef GET_FUNCTION_RECOGNIZER
  return llvm::Intrinsic::not_intrinsic;
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
                                 SmallVectorImpl<TupleTypeElt> &ArgElts,
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
  llvm::AttributeSet attrs
    = llvm::Intrinsic::getAttributes(llvm::getGlobalContext(), ID);
  Info = FunctionType::ExtInfo();
  if (attrs.hasAttribute(llvm::AttributeSet::FunctionIndex,
                         llvm::Attribute::NoReturn))
    Info = Info.withIsNoReturn(true);
  
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

/// decodeLLVMAtomicOrdering - turn a string like "release" into the LLVM enum.
static llvm::AtomicOrdering decodeLLVMAtomicOrdering(StringRef O) {
  using namespace llvm;
  return StringSwitch<AtomicOrdering>(O)
    .Case("unordered", Unordered)
    .Case("monotonic", Monotonic)
    .Case("acquire", Acquire)
    .Case("release", Release)
    .Case("acqrel", AcquireRelease)
    .Case("seqcst", SequentiallyConsistent)
    .Default(NotAtomic);
}

static bool isValidCmpXChgOrdering(StringRef SuccessString, 
                                   StringRef FailureString) {
  using namespace llvm;
  AtomicOrdering SuccessOrdering = decodeLLVMAtomicOrdering(SuccessString);
  AtomicOrdering FailureOrdering = decodeLLVMAtomicOrdering(FailureString);

  // Unordered and unknown values are not allowed.
  if (SuccessOrdering <= Unordered  ||  FailureOrdering <= Unordered)
    return false;
  // Success must be at least as strong as failure.
  if (SuccessOrdering < FailureOrdering)
    return false;
  // Failure may not release because no store occurred.
  if (FailureOrdering == Release  ||  FailureOrdering == AcquireRelease)
    return false;

  return true;
}

ValueDecl *swift::getBuiltinValueDecl(ASTContext &Context, Identifier Id) {
  SmallVector<Type, 4> Types;
  StringRef OperationName = getBuiltinBaseName(Context, Id.str(), Types);

  // If this is the name of an LLVM intrinsic, cons up a swift function with a
  // type that matches the IR types.
  if (unsigned ID = getLLVMIntrinsicID(OperationName, !Types.empty())) {
    SmallVector<TupleTypeElt, 8> ArgElts;
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
    
    // Verify we have a single integer, floating point, or pointer type.
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
  
  BuiltinValueKind BV = llvm::StringSwitch<BuiltinValueKind>(OperationName)
#define BUILTIN(id, name, Attrs) \
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
  case BuiltinValueKind::AtomicRMW:
    llvm_unreachable("Handled above");
  case BuiltinValueKind::None: return nullptr;

  case BuiltinValueKind::Gep:
    if (Types.size() != 1) return nullptr;
    return getGepOperation(Id, Types[0]);

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

  case BuiltinValueKind::TryPin:
    return getTryPinOperation(Context, Id);

  case BuiltinValueKind::Retain:
  case BuiltinValueKind::Release:
  case BuiltinValueKind::Autorelease:
  case BuiltinValueKind::Unpin:
    if (!Types.empty()) return nullptr;
    return getRefCountingOperation(Context, Id);
      
  case BuiltinValueKind::Load:
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
  case BuiltinValueKind::TakeArrayFrontToBack:
  case BuiltinValueKind::TakeArrayBackToFront:
    if (!Types.empty()) return nullptr;
    return getTransferArrayOperation(Context, Id);

  case BuiltinValueKind::IsUnique:
  case BuiltinValueKind::IsUniqueOrPinned:
  case BuiltinValueKind::IsUnique_native:
  case BuiltinValueKind::IsUniqueOrPinned_native:
    if (!Types.empty()) return nullptr;
    return getIsUniqueOperation(Context, Id);

  case BuiltinValueKind::Sizeof:
  case BuiltinValueKind::Strideof:
  case BuiltinValueKind::Alignof:
  case BuiltinValueKind::StrideofNonZero:
    return getSizeOrAlignOfOperation(Context, Id);

  case BuiltinValueKind::AllocRaw:
    return getAllocOperation(Context, Id);

  case BuiltinValueKind::DeallocRaw:
    return getDeallocOperation(Context, Id);

  case BuiltinValueKind::MarkDependence:
    return getMarkDependenceOperation(Context, Id);

  case BuiltinValueKind::CastToNativeObject:
  case BuiltinValueKind::CastFromNativeObject:
  case BuiltinValueKind::CastToUnknownObject:
  case BuiltinValueKind::CastFromUnknownObject:
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
      
  case BuiltinValueKind::ReinterpretCast:
    if (!Types.empty()) return nullptr;
    return getReinterpretCastOperation(Context, Id);

  case BuiltinValueKind::AllocValueBuffer:
  case BuiltinValueKind::ProjectValueBuffer:
    if (!Types.empty()) return nullptr;
    return getProjectValueBufferOperation(Context, Id);

  case BuiltinValueKind::DeallocValueBuffer:
    if (!Types.empty()) return nullptr;
    return getDeallocValueBufferOperation(Context, Id);

  case BuiltinValueKind::MakeMaterializeForSetCallback:
    if (!Types.empty()) return nullptr;
    return getMakeMaterializeForSetCallbackOperation(Context, Id);
      
  case BuiltinValueKind::AddressOf:
    if (!Types.empty()) return nullptr;
    return getAddressOfOperation(Context, Id);
      
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
    return getOnceOperation(Context, Id);

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

  case BuiltinValueKind::IntToFPWithOverflow:
    if (Types.size() != 2) return nullptr;
    return getIntToFPWithOverflowOperation(Context, Id, Types[0], Types[1]);
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

