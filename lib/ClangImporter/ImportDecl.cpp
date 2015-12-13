//===--- ImportDecl.cpp - Import Clang Declarations -----------------------===//
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
// This file implements support for importing Clang declarations into Swift.
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "swift/Strings.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/Lexer.h"
#include "swift/Config.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Lookup.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Path.h"

#include <algorithm>

#define DEBUG_TYPE "Clang module importer"

STATISTIC(NumTotalImportedEntities, "# of imported clang entities");
STATISTIC(NumFactoryMethodsAsInitializers,
          "# of factory methods mapped to initializers");

using namespace swift;

namespace swift {
namespace inferred_attributes {
  enum {
    requires_stored_property_inits = 0x01
  };
}
}

/// \brief Retrieve the type of 'self' for the given context.
static Type getSelfTypeForContext(DeclContext *dc) {
  // For a protocol or extension thereof, the type is 'Self'.
  // FIXME: Weird that we're producing an archetype for protocol Self,
  // but the declared type of the context in non-protocol cases.
  if (dc->isProtocolOrProtocolExtensionContext())
    return dc->getProtocolSelf()->getArchetype();
  return dc->getDeclaredTypeOfContext();
}

static bool isInSystemModule(DeclContext *D) {
  if (cast<ClangModuleUnit>(D->getModuleScopeContext())->isSystemModule())
    return true;
  return false;
}

/// Create an implicit 'self' decl for a method in the specified type.  If
/// 'static' is true, then this is self for a static method in the type.
///
/// Note that this decl is created, but it is returned with an incorrect
/// DeclContext that needs to be reset once the method exists.
///
static VarDecl *createSelfDecl(DeclContext *DC, bool isStaticMethod,
                               bool isInOut = false) {
  auto selfType = getSelfTypeForContext(DC);

  ASTContext &C = DC->getASTContext();

  if (isStaticMethod)
    selfType = MetatypeType::get(selfType);

  bool isLet = true;
  if (auto *ND = selfType->getAnyNominal())
    isLet = !isInOut && !isa<StructDecl>(ND) && !isa<EnumDecl>(ND);

  if (isInOut)
    selfType = InOutType::get(selfType);

  VarDecl *selfDecl = new (C) ParamDecl(/*IsLet*/isLet, SourceLoc(), 
                                        Identifier(), SourceLoc(), C.Id_self, 
                                        selfType, DC);
  selfDecl->setImplicit();
  return selfDecl;
}

/// Create a typedpattern(namedpattern(decl))
static Pattern *createTypedNamedPattern(VarDecl *decl) {
  ASTContext &Ctx = decl->getASTContext();
  Type ty = decl->getType();

  Pattern *P = new (Ctx) NamedPattern(decl);
  P->setType(ty);
  P->setImplicit();
  P = new (Ctx) TypedPattern(P, TypeLoc::withoutLoc(ty));
  P->setType(ty);
  P->setImplicit();
  return P;
}

template <size_t A, size_t B>
static bool verifyNameMapping(MappedTypeNameKind NameMappping,
                              const char (&left)[A], const char (&right)[B]) {
  return NameMappping == MappedTypeNameKind::DoNothing ||
         strcmp(left, right) != 0;
}

/// \brief Map a well-known C type to a swift type from the standard library.
///
/// \param IsError set to true when we know the corresponding swift type name,
/// but we could not find it.  (For example, the type was not defined in the
/// standard library or the required standard library module was not imported.)
/// This should be a hard error, we don't want to map the type only sometimes.
///
/// \returns A pair of a swift type and its name that corresponds to a given
/// C type.
static std::pair<Type, StringRef>
getSwiftStdlibType(const clang::TypedefNameDecl *D,
                   Identifier Name,
                   ClangImporter::Implementation &Impl,
                   bool *IsError, MappedTypeNameKind &NameMapping) {
  *IsError = false;

  MappedCTypeKind CTypeKind;
  unsigned Bitwidth;
  StringRef SwiftModuleName;
  bool IsSwiftModule; // True if SwiftModuleName == STDLIB_NAME.
  StringRef SwiftTypeName;
  bool CanBeMissing;

  do {
#define MAP_TYPE(C_TYPE_NAME, C_TYPE_KIND, C_TYPE_BITWIDTH,        \
                 SWIFT_MODULE_NAME, SWIFT_TYPE_NAME,               \
                 CAN_BE_MISSING, C_NAME_MAPPING)                   \
    if (Name.str() == C_TYPE_NAME) {                               \
      CTypeKind = MappedCTypeKind::C_TYPE_KIND;                    \
      Bitwidth = C_TYPE_BITWIDTH;                                  \
      if (StringRef(SWIFT_MODULE_NAME) == StringRef(STDLIB_NAME))  \
        IsSwiftModule = true;                                      \
      else {                                                       \
        IsSwiftModule = false;                                     \
        SwiftModuleName = SWIFT_MODULE_NAME;                       \
      }                                                            \
      SwiftTypeName = SWIFT_TYPE_NAME;                             \
      CanBeMissing = CAN_BE_MISSING;                               \
      NameMapping = MappedTypeNameKind::C_NAME_MAPPING;            \
      assert(verifyNameMapping(MappedTypeNameKind::C_NAME_MAPPING, \
                               C_TYPE_NAME, SWIFT_TYPE_NAME) &&    \
             "MappedTypes.def: Identical names must use DoNothing"); \
      break;                                                       \
    }
#include "MappedTypes.def"

    // We did not find this type, thus it is not mapped.
    return std::make_pair(Type(), "");
  } while(0);

  clang::ASTContext &ClangCtx = Impl.getClangASTContext();

  auto ClangType = D->getUnderlyingType();

  // If the C type does not have the expected size, don't import it as a stdlib
  // type.
  unsigned ClangTypeSize = ClangCtx.getTypeSize(ClangType);
  if (Bitwidth != 0 && Bitwidth != ClangTypeSize)
    return std::make_pair(Type(), "");

  // Check other expected properties of the C type.
  switch(CTypeKind) {
  case MappedCTypeKind::UnsignedInt:
    if (!ClangType->isUnsignedIntegerType())
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::SignedInt:
    if (!ClangType->isSignedIntegerType())
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::UnsignedWord:
    if (ClangTypeSize != 64 && ClangTypeSize != 32)
      return std::make_pair(Type(), "");
    if (!ClangType->isUnsignedIntegerType())
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::SignedWord:
    if (ClangTypeSize != 64 && ClangTypeSize != 32)
      return std::make_pair(Type(), "");
    if (!ClangType->isSignedIntegerType())
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::FloatIEEEsingle:
  case MappedCTypeKind::FloatIEEEdouble:
  case MappedCTypeKind::FloatX87DoubleExtended: {
    if (!ClangType->isFloatingType())
      return std::make_pair(Type(), "");

    const llvm::fltSemantics &Sem = ClangCtx.getFloatTypeSemantics(ClangType);
    switch(CTypeKind) {
    case MappedCTypeKind::FloatIEEEsingle:
      assert(Bitwidth == 32 && "FloatIEEEsingle should be 32 bits wide");
      if (&Sem != &APFloat::IEEEsingle)
        return std::make_pair(Type(), "");
      break;

    case MappedCTypeKind::FloatIEEEdouble:
      assert(Bitwidth == 64 && "FloatIEEEdouble should be 64 bits wide");
      if (&Sem != &APFloat::IEEEdouble)
        return std::make_pair(Type(), "");
      break;

    case MappedCTypeKind::FloatX87DoubleExtended:
      assert(Bitwidth == 80 && "FloatX87DoubleExtended should be 80 bits wide");
      if (&Sem != &APFloat::x87DoubleExtended)
        return std::make_pair(Type(), "");
      break;

    default:
      llvm_unreachable("should see only floating point types here");
    }
    }
    break;

  case MappedCTypeKind::VaList:
    if (ClangTypeSize != ClangCtx.getTypeSize(ClangCtx.VoidPtrTy))
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::ObjCBool:
    if (!ClangCtx.hasSameType(ClangType, ClangCtx.ObjCBuiltinBoolTy) &&
        !(ClangCtx.getBOOLDecl() &&
          ClangCtx.hasSameType(ClangType, ClangCtx.getBOOLType())))
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::ObjCSel:
    if (!ClangCtx.hasSameType(ClangType, ClangCtx.getObjCSelType()) &&
        !ClangCtx.hasSameType(ClangType,
                              ClangCtx.getObjCSelRedefinitionType()))
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::ObjCId:
    if (!ClangCtx.hasSameType(ClangType, ClangCtx.getObjCIdType()) &&
        !ClangCtx.hasSameType(ClangType,
                              ClangCtx.getObjCIdRedefinitionType()))
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::ObjCClass:
    if (!ClangCtx.hasSameType(ClangType, ClangCtx.getObjCClassType()) &&
        !ClangCtx.hasSameType(ClangType,
                              ClangCtx.getObjCClassRedefinitionType()))
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::CGFloat:
    if (!ClangType->isFloatingType())
      return std::make_pair(Type(), "");
    break;

  case MappedCTypeKind::Block:
    if (!ClangType->isBlockPointerType())
      return std::make_pair(Type(), "");
    break;
  }

  Module *M;
  if (IsSwiftModule)
    M = Impl.getStdlibModule();
  else
    M = Impl.getNamedModule(SwiftModuleName);
  if (!M) {
    // User did not import the library module that contains the type we want to
    // substitute.
    *IsError = true;
    return std::make_pair(Type(), "");
  }

  Type SwiftType = Impl.getNamedSwiftType(M, SwiftTypeName);
  if (!SwiftType && !CanBeMissing) {
    // The required type is not defined in the standard library.
    *IsError = true;
    return std::make_pair(Type(), "");
  }
  return std::make_pair(SwiftType, SwiftTypeName);
}

static bool isNSDictionaryMethod(const clang::ObjCMethodDecl *MD,
                                 clang::Selector cmd) {
  if (MD->getSelector() != cmd)
    return false;
  if (isa<clang::ObjCProtocolDecl>(MD->getDeclContext()))
    return false;
  if (MD->getClassInterface()->getName() != "NSDictionary")
    return false;
  return true;
}

/// Build the \c rawValue property trivial getter for an option set or
/// unknown enum.
///
/// \code
/// struct NSSomeOptionSet : OptionSetType {
///   let rawValue: Raw
/// }
/// \endcode
static FuncDecl *makeRawValueTrivialGetter(ClangImporter::Implementation &Impl,
                                           StructDecl *optionSetDecl,
                                           ValueDecl *rawDecl) {
  ASTContext &C = Impl.SwiftContext;
  auto optionSetType = optionSetDecl->getDeclaredTypeInContext();
  auto rawType = rawDecl->getType();

  VarDecl *selfDecl = createSelfDecl(optionSetDecl, false);
  Pattern *selfParam = createTypedNamedPattern(selfDecl);

  Pattern *methodParam = TuplePattern::create(C, SourceLoc(),{},SourceLoc());
  methodParam->setType(TupleType::getEmpty(C));
  Pattern *params[] = {selfParam, methodParam};

  FuncDecl *getterDecl = FuncDecl::create(
      C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
      DeclName(), SourceLoc(), SourceLoc(), SourceLoc(), nullptr, Type(), params,
      TypeLoc::withoutLoc(rawType), optionSetDecl);
  getterDecl->setImplicit();
  
  auto toRawArgType = TupleType::getEmpty(C);
  Type toRawType = FunctionType::get(toRawArgType, rawType);
  toRawType = FunctionType::get(optionSetType, toRawType);
  getterDecl->setType(toRawType);
  getterDecl->setBodyResultType(rawType);
  getterDecl->setAccessibility(Accessibility::Public);

  // Don't bother synthesizing the body if we've already finished type-checking.
  if (Impl.hasFinishedTypeChecking())
    return getterDecl;

  auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/ true);
  auto valueRef = new (C) MemberRefExpr(selfRef, SourceLoc(),
                                        rawDecl, SourceLoc(),
                                        /*implicit*/ true);
  auto valueRet = new (C) ReturnStmt(SourceLoc(), valueRef);
  
  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(valueRet),
                                SourceLoc(),
                                /*implicit*/ true);
  getterDecl->setBody(body);
  
  // Add as an external definition.
  C.addedExternalDecl(getterDecl);

  return getterDecl;
}

/// Build the \c rawValue property trivial setter for an unknown enum.
///
/// \code
/// struct SomeRandomCEnum {
///   var rawValue: Raw
/// }
/// \endcode
static FuncDecl *makeRawValueTrivialSetter(ClangImporter::Implementation &Impl,
                                           StructDecl *importedDecl,
                                           ValueDecl *rawDecl) {
  // FIXME: Largely duplicated from the type checker.
  ASTContext &C = Impl.SwiftContext;
  auto selfType = importedDecl->getDeclaredTypeInContext();
  auto rawType = rawDecl->getType();

  VarDecl *selfDecl = new (C) ParamDecl(/*IsLet*/false, SourceLoc(),
                                        Identifier(), SourceLoc(),
                                        C.Id_self, selfType,
                                        importedDecl);
  selfDecl->setImplicit();
  Pattern *selfParam = createTypedNamedPattern(selfDecl);

  VarDecl *newValueDecl = new (C) ParamDecl(/*IsLet*/true, SourceLoc(),
                                            Identifier(), SourceLoc(),
                                            C.Id_value, rawType, importedDecl);
  newValueDecl->setImplicit();
  Pattern *newValueParam = createTypedNamedPattern(newValueDecl);
  newValueParam = new (C) ParenPattern(SourceLoc(), newValueParam, SourceLoc());
  newValueParam->setType(ParenType::get(C, rawType));

  Pattern *params[] = {selfParam, newValueParam};
  Type voidTy = TupleType::getEmpty(C);

  FuncDecl *setterDecl = FuncDecl::create(
      C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
      DeclName(), SourceLoc(), SourceLoc(), SourceLoc(), nullptr, Type(), params,
      TypeLoc::withoutLoc(voidTy), importedDecl);
  setterDecl->setImplicit();
  setterDecl->setMutating();
  
  Type fnTy = FunctionType::get(newValueParam->getType(), voidTy);
  fnTy = FunctionType::get(selfType, fnTy);
  setterDecl->setType(fnTy);
  setterDecl->setBodyResultType(voidTy);
  setterDecl->setAccessibility(Accessibility::Public);

  // Don't bother synthesizing the body if we've already finished type-checking.
  if (Impl.hasFinishedTypeChecking())
    return setterDecl;

  auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/ true);
  auto dest = new (C) MemberRefExpr(selfRef, SourceLoc(), rawDecl, SourceLoc(),
                                    /*implicit*/ true);

  auto paramRef = new (C) DeclRefExpr(newValueDecl, SourceLoc(),
                                      /*implicit*/true);

  auto assign = new (C) AssignExpr(dest, SourceLoc(), paramRef,
                                   /*implicit*/true);

  auto body = BraceStmt::create(C, SourceLoc(), { assign }, SourceLoc(),
                                /*implicit*/ true);
  setterDecl->setBody(body);

  // Add as an external definition.
  C.addedExternalDecl(setterDecl);

  return setterDecl;
}

// Build the init(rawValue:) initializer for an imported NS_ENUM.
//   enum NSSomeEnum: RawType {
//     init?(rawValue: RawType) {
//       self = Builtin.reinterpretCast(rawValue)
//     }
//   }
// Unlike a standard init(rawValue:) enum initializer, this does a reinterpret
// cast in order to preserve unknown or future cases from C.
static ConstructorDecl *
makeEnumRawValueConstructor(ClangImporter::Implementation &Impl,
                            EnumDecl *enumDecl) {
  ASTContext &C = Impl.SwiftContext;
  auto enumTy = enumDecl->getDeclaredTypeInContext();
  auto metaTy = MetatypeType::get(enumTy);
  
  VarDecl *selfDecl = createSelfDecl(enumDecl, false);
  Pattern *selfPattern = createTypedNamedPattern(selfDecl);

  auto param = new (C) ParamDecl(/*let*/ true,
                                 SourceLoc(), C.Id_rawValue,
                                 SourceLoc(), C.Id_rawValue,
                                 enumDecl->getRawType(),
                                 enumDecl);
  Pattern *paramPattern = new (C) NamedPattern(param);
  paramPattern->setType(enumDecl->getRawType());
  paramPattern->setImplicit();
  paramPattern = new (C)
    TypedPattern(paramPattern, TypeLoc::withoutLoc(enumDecl->getRawType()));
  paramPattern->setType(enumDecl->getRawType());
  paramPattern->setImplicit();
  
  auto patternElt = TuplePatternElt(paramPattern);
  patternElt.setLabel(C.Id_rawValue, SourceLoc());
  paramPattern = TuplePattern::create(C, SourceLoc(), patternElt, SourceLoc());
  paramPattern->setImplicit();
  auto typeElt = TupleTypeElt(enumDecl->getRawType(), C.Id_rawValue);
  auto paramTy = TupleType::get(typeElt, C);
  paramPattern->setType(paramTy);

  DeclName name(C, C.Id_init, C.Id_rawValue);
  auto *ctorDecl = new (C) ConstructorDecl(name, enumDecl->getLoc(),
                                           OTK_Optional, SourceLoc(),
                                           selfPattern, paramPattern,
                                           nullptr, SourceLoc(), enumDecl);
  ctorDecl->setImplicit();
  ctorDecl->setAccessibility(Accessibility::Public);

  auto optEnumTy = OptionalType::get(enumTy);

  auto fnTy = FunctionType::get(paramTy, optEnumTy);
  auto allocFnTy = FunctionType::get(metaTy, fnTy);
  auto initFnTy = FunctionType::get(enumTy, fnTy);
  ctorDecl->setType(allocFnTy);
  ctorDecl->setInitializerType(initFnTy);

  // Don't bother synthesizing the body if we've already finished type-checking.
  if (Impl.hasFinishedTypeChecking())
    return ctorDecl;
  
  auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/true);
  auto paramRef = new (C) DeclRefExpr(param, SourceLoc(),
                                      /*implicit*/ true);
  auto reinterpretCast
    = cast<FuncDecl>(getBuiltinValueDecl(C, C.getIdentifier("reinterpretCast")));
  auto reinterpretCastRef
    = new (C) DeclRefExpr(reinterpretCast, SourceLoc(), /*implicit*/ true);
  auto reinterpreted = new (C) CallExpr(reinterpretCastRef, paramRef,
                                        /*implicit*/ true);
  auto assign = new (C) AssignExpr(selfRef, SourceLoc(), reinterpreted,
                                   /*implicit*/ true);
  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(assign), SourceLoc(),
                                /*implicit*/ true);
  
  ctorDecl->setBody(body);
  
  C.addedExternalDecl(ctorDecl);
  
  return ctorDecl;
}

// Build the rawValue getter for an imported NS_ENUM.
//   enum NSSomeEnum: RawType {
//     var rawValue: RawType {
//       return Builtin.reinterpretCast(self)
//     }
//   }
// Unlike a standard init(rawValue:) enum initializer, this does a reinterpret
// cast in order to preserve unknown or future cases from C.
static FuncDecl *makeEnumRawValueGetter(ClangImporter::Implementation &Impl,
                                        EnumDecl *enumDecl,
                                        VarDecl *rawValueDecl) {
  ASTContext &C = Impl.SwiftContext;
  
  VarDecl *selfDecl = createSelfDecl(enumDecl, false);
  Pattern *selfPattern = createTypedNamedPattern(selfDecl);
  
  Pattern *methodParam = TuplePattern::create(C, SourceLoc(),{},SourceLoc());
  auto unitTy = TupleType::getEmpty(C);
  methodParam->setType(unitTy);

  Pattern *params[] = {selfPattern, methodParam};

  auto fnTy = FunctionType::get(unitTy, enumDecl->getRawType());
  fnTy = FunctionType::get(selfDecl->getType(), fnTy);

  auto getterDecl =
    FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
                     DeclName(), SourceLoc(), SourceLoc(), SourceLoc(), nullptr,
                     Type(), params,
                     TypeLoc::withoutLoc(enumDecl->getRawType()), enumDecl);
  getterDecl->setImplicit();
  getterDecl->setType(fnTy);
  getterDecl->setBodyResultType(enumDecl->getRawType());
  getterDecl->setAccessibility(Accessibility::Public);

  rawValueDecl->makeComputed(SourceLoc(), getterDecl, nullptr, nullptr,
                             SourceLoc());

  // Don't bother synthesizing the body if we've already finished type-checking.
  if (Impl.hasFinishedTypeChecking())
    return getterDecl;
  
  auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/true);
  auto reinterpretCast
    = cast<FuncDecl>(getBuiltinValueDecl(C, C.getIdentifier("reinterpretCast")));
  auto reinterpretCastRef
    = new (C) DeclRefExpr(reinterpretCast, SourceLoc(), /*implicit*/ true);
  auto reinterpreted = new (C) CallExpr(reinterpretCastRef, selfRef,
                                        /*implicit*/ true);
  auto ret = new (C) ReturnStmt(SourceLoc(), reinterpreted);
  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(ret), SourceLoc(),
                                /*implicit*/ true);
  
  getterDecl->setBody(body);
  C.addedExternalDecl(getterDecl);
  return getterDecl;
}

static FuncDecl *makeFieldGetterDecl(ClangImporter::Implementation &Impl,
                                     StructDecl *importedDecl,
                                     VarDecl *importedFieldDecl,
                                     ClangNode clangNode = ClangNode()) {
  auto &C = Impl.SwiftContext;
  auto selfDecl = createSelfDecl(importedDecl, /*is static*/ false);
  auto selfPattern = createTypedNamedPattern(selfDecl);

  auto getterFnRetTypeLoc = TypeLoc::withoutLoc(importedFieldDecl->getType());
  auto getterMethodParam = TuplePattern::create(C, SourceLoc(), {}, SourceLoc());
  Pattern *getterParams[] = { selfPattern, getterMethodParam };

  auto getterDecl = FuncDecl::create(C, importedFieldDecl->getLoc(),
                                     StaticSpellingKind::None,
                                     SourceLoc(), DeclName(), SourceLoc(),
                                     SourceLoc(), SourceLoc(), nullptr, Type(),
                                     getterParams, getterFnRetTypeLoc,
                                     importedDecl, clangNode);
  getterDecl->setAccessibility(Accessibility::Public);
  
  auto voidTy = TupleType::getEmpty(C);
  
  // Create the field getter
  auto getterFnTy = FunctionType::get(voidTy, importedFieldDecl->getType());
  
  // Getter methods need to take self as the first argument. Wrap the
  // function type to take the self type.
  getterFnTy = FunctionType::get(selfDecl->getType(), getterFnTy);

  getterDecl->setType(getterFnTy);
  getterDecl->setBodyResultType(importedFieldDecl->getType());

  return getterDecl;
}

static FuncDecl *makeFieldSetterDecl(ClangImporter::Implementation &Impl,
                                     StructDecl *importedDecl,
                                     VarDecl *importedFieldDecl,
                                     ClangNode clangNode = ClangNode()) {
  auto &C = Impl.SwiftContext;
  auto inoutSelfDecl = createSelfDecl(importedDecl,
                                      /*isStaticMethod*/ false,
                                      /*isInOut*/ true);
  auto inoutSelfPattern = createTypedNamedPattern(inoutSelfDecl);

  auto voidTy = TupleType::getEmpty(C);

  auto setterFnRetTypeLoc = TypeLoc::withoutLoc(voidTy);

  auto newValueDecl = new (C) ParamDecl(/*isLet */ true, SourceLoc(),
                                        Identifier(), SourceLoc(), C.Id_value,
                                        importedFieldDecl->getType(),
                                        importedDecl);
  Pattern *setterNewValueParam = createTypedNamedPattern(newValueDecl);

  // FIXME: Remove ParenPattern?
  setterNewValueParam = new (C) ParenPattern(SourceLoc(), setterNewValueParam,
                                             SourceLoc());
  setterNewValueParam->setType(ParenType::get(C, importedFieldDecl->getType()));

  Pattern *setterParams[] = { inoutSelfPattern, setterNewValueParam };

  auto setterDecl = FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None,
                                     SourceLoc(), DeclName(), SourceLoc(),
                                     SourceLoc(), SourceLoc(), nullptr, Type(),
                                     setterParams, setterFnRetTypeLoc,
                                     importedDecl, clangNode);
  
  // Create the field setter
  auto setterFnTy = FunctionType::get(importedFieldDecl->getType(), voidTy);
  
  // Setter methods need to take self as the first argument. Wrap the
  // function type to take the self type.
  setterFnTy = FunctionType::get(inoutSelfDecl->getType(),
                                 setterFnTy);

  setterDecl->setType(setterFnTy);
  setterDecl->setBodyResultType(voidTy);
  setterDecl->setAccessibility(Accessibility::Public);
  setterDecl->setMutating();

  return setterDecl;
}

/// Build the union field getter and setter.
///
/// \code
/// struct SomeImportedUnion {
///   var myField: Int {
///     get {
///       return Builtin.reinterpretCast(self)
///     }
///     set(newValue) {
///       Builtin.initialize(Builtin.addressof(self), newValue))
///     }
///   }
/// }
/// \endcode
///
/// \returns a pair of the getter and setter function decls.
static std::pair<FuncDecl *, FuncDecl *>
makeUnionFieldAccessors(ClangImporter::Implementation &Impl,
                        StructDecl *importedUnionDecl,
                        VarDecl *importedFieldDecl) {
  auto &C = Impl.SwiftContext;

  auto getterDecl = makeFieldGetterDecl(Impl,
                                        importedUnionDecl,
                                        importedFieldDecl);

  auto setterDecl = makeFieldSetterDecl(Impl,
                                        importedUnionDecl,
                                        importedFieldDecl);

  importedFieldDecl->makeComputed(SourceLoc(), getterDecl, setterDecl, nullptr,
                                  SourceLoc());

  // Don't bother synthesizing the body if we've already finished type-checking.
  if (Impl.hasFinishedTypeChecking())
    return { getterDecl, setterDecl };

  // Synthesize the getter body
  {
    auto selfDecl = getterDecl->getImplicitSelfDecl();

    auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/ true);
    auto reinterpretCast = cast<FuncDecl>(getBuiltinValueDecl(
        C, C.getIdentifier("reinterpretCast")));
    auto reinterpretCastRef
      = new (C) DeclRefExpr(reinterpretCast, SourceLoc(), /*implicit*/ true);
    auto reinterpreted = new (C) CallExpr(reinterpretCastRef, selfRef,
                                          /*implicit*/ true);
    auto ret = new (C) ReturnStmt(SourceLoc(), reinterpreted);
    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(ret), SourceLoc(),
                                  /*implicit*/ true);
    getterDecl->setBody(body);
    C.addedExternalDecl(getterDecl);
  }

  // Synthesize the setter body
  {
    auto inoutSelfDecl = setterDecl->getImplicitSelfDecl();

    auto inoutSelfRef = new (C) DeclRefExpr(inoutSelfDecl, SourceLoc(),
                                            /*implicit*/ true);
    auto inoutSelf = new (C) InOutExpr(SourceLoc(), inoutSelfRef,
      InOutType::get(importedUnionDecl->getType()), /*implicit*/ true);

    auto newValueDecl = setterDecl->getBodyParamPatterns()[1]->getSingleVar();

    auto newValueRef = new (C) DeclRefExpr(newValueDecl, SourceLoc(),
                                           /*implicit*/ true);
    auto addressofFn = cast<FuncDecl>(getBuiltinValueDecl(
      C, C.getIdentifier("addressof")));
    auto addressofFnRef
      = new (C) DeclRefExpr(addressofFn, SourceLoc(), /*implicit*/ true);
    auto selfPointer = new (C) CallExpr(addressofFnRef, inoutSelf,
                                          /*implicit*/ true);
    auto initializeFn = cast<FuncDecl>(getBuiltinValueDecl(
      C, C.getIdentifier("initialize")));
    auto initializeFnRef
      = new (C) DeclRefExpr(initializeFn, SourceLoc(), /*implicit*/ true);
    auto initalizeArgs = TupleExpr::createImplicit(C,
                                                   { newValueRef, selfPointer },
                                                   {});
    auto initialize = new (C) CallExpr(initializeFnRef, initalizeArgs,
                                       /*implicit*/ true);
    auto body = BraceStmt::create(C, SourceLoc(), { initialize }, SourceLoc(),
                                  /*implicit*/ true);
    setterDecl->setBody(body);
    C.addedExternalDecl(setterDecl);
  }

  return { getterDecl, setterDecl };
}

static clang::DeclarationName
getAccessorDeclarationName(clang::ASTContext &Ctx,
                           StructDecl *structDecl,
                           VarDecl *fieldDecl,
                           const char *suffix) {
  std::string id;
  llvm::raw_string_ostream IdStream(id);
  IdStream << "$" << structDecl->getName()
           << "$" << fieldDecl->getName()
           << "$" << suffix;

  return clang::DeclarationName(&Ctx.Idents.get(IdStream.str()));
}

/// Build the bitfield getter and setter using Clang.
///
/// \code
/// static inline int get(RecordType self) {
///   return self.field;
/// }
/// static inline void set(int newValue, RecordType *self) {
///   self->field = newValue;
/// }
/// \endcode
///
/// \returns a pair of the getter and setter function decls.
static std::pair<FuncDecl *, FuncDecl *>
makeBitFieldAccessors(ClangImporter::Implementation &Impl,
                      clang::RecordDecl *structDecl,
                      StructDecl *importedStructDecl,
                      clang::FieldDecl *fieldDecl,
                      VarDecl *importedFieldDecl) {
  clang::ASTContext &Ctx = Impl.getClangASTContext();

  // Getter: static inline FieldType get(RecordType self);
  auto recordType = Ctx.getRecordType(structDecl);
  auto recordPointerType = Ctx.getPointerType(recordType);
  auto fieldType = fieldDecl->getType();
  auto fieldNameInfo = clang::DeclarationNameInfo(fieldDecl->getDeclName(),
                                                  clang::SourceLocation());

  auto cGetterName = getAccessorDeclarationName(Ctx, importedStructDecl,
                                                importedFieldDecl, "getter");
  auto cGetterType = Ctx.getFunctionType(fieldDecl->getType(),
                                         recordType,
                                         clang::FunctionProtoType::ExtProtoInfo());
  auto cGetterTypeInfo = Ctx.getTrivialTypeSourceInfo(cGetterType);
  auto cGetterDecl = clang::FunctionDecl::Create(Ctx,
                                                 structDecl->getDeclContext(),
                                                 clang::SourceLocation(),
                                                 clang::SourceLocation(),
                                                 cGetterName,
                                                 cGetterType,
                                                 cGetterTypeInfo,
                                                 clang::SC_Static);
  cGetterDecl->setImplicitlyInline();
  assert(!cGetterDecl->isExternallyVisible());

  auto getterDecl = makeFieldGetterDecl(Impl,
                                        importedStructDecl,
                                        importedFieldDecl,
                                        cGetterDecl);

  // Setter: static inline void set(FieldType newValue, RecordType *self);
  SmallVector<clang::QualType, 8> cSetterParamTypes;
  cSetterParamTypes.push_back(fieldType);
  cSetterParamTypes.push_back(recordPointerType);

  auto cSetterName = getAccessorDeclarationName(Ctx, importedStructDecl,
                                                importedFieldDecl, "setter");
  auto cSetterType = Ctx.getFunctionType(Ctx.VoidTy,
                                         cSetterParamTypes,
                                         clang::FunctionProtoType::ExtProtoInfo());
  auto cSetterTypeInfo = Ctx.getTrivialTypeSourceInfo(cSetterType);
  
  auto cSetterDecl = clang::FunctionDecl::Create(Ctx,
                                                 structDecl->getDeclContext(),
                                                 clang::SourceLocation(),
                                                 clang::SourceLocation(),
                                                 cSetterName,
                                                 cSetterType,
                                                 cSetterTypeInfo,
                                                 clang::SC_Static);
  cSetterDecl->setImplicitlyInline();
  assert(!cSetterDecl->isExternallyVisible());

  auto setterDecl = makeFieldSetterDecl(Impl,
                                        importedStructDecl,
                                        importedFieldDecl,
                                        cSetterDecl);

  importedFieldDecl->makeComputed(SourceLoc(),
                                  getterDecl,
                                  setterDecl,
                                  nullptr,
                                  SourceLoc());

  // Don't bother synthesizing the body if we've already finished type-checking.
  if (Impl.hasFinishedTypeChecking())
    return { getterDecl, setterDecl };
  
  // Synthesize the getter body
  {
    auto cGetterSelfId = nullptr;
    auto recordTypeInfo = Ctx.getTrivialTypeSourceInfo(recordType);
    auto cGetterSelf = clang::ParmVarDecl::Create(Ctx, cGetterDecl,
                                                  clang::SourceLocation(),
                                                  clang::SourceLocation(),
                                                  cGetterSelfId,
                                                  recordType,
                                                  recordTypeInfo,
                                                  clang::SC_None,
                                                  nullptr);
    cGetterDecl->setParams(cGetterSelf);
    
    auto cGetterSelfExpr = new (Ctx) clang::DeclRefExpr(cGetterSelf, false,
                                                        recordType,
                                                        clang::VK_RValue,
                                                        clang::SourceLocation());
    auto cGetterExpr = new (Ctx) clang::MemberExpr(cGetterSelfExpr,
                                                   /*isarrow=*/ false,
                                                   clang::SourceLocation(),
                                                   fieldDecl,
                                                   fieldNameInfo,
                                                   fieldType,
                                                   clang::VK_RValue,
                                                   clang::OK_BitField);
    
    auto cGetterBody = new (Ctx) clang::ReturnStmt(clang::SourceLocation(),
                                                   cGetterExpr,
                                                   nullptr);
    cGetterDecl->setBody(cGetterBody);

    Impl.registerExternalDecl(getterDecl);
  }

  // Synthesize the setter body
  {
    SmallVector<clang::ParmVarDecl *, 2> cSetterParams;
    auto fieldTypeInfo = Ctx.getTrivialTypeSourceInfo(fieldType);
    auto cSetterValue = clang::ParmVarDecl::Create(Ctx, cSetterDecl,
                                                   clang::SourceLocation(),
                                                   clang::SourceLocation(),
                                                   /* nameID? */ nullptr,
                                                   fieldType,
                                                   fieldTypeInfo,
                                                   clang::SC_None,
                                                   nullptr);
    cSetterParams.push_back(cSetterValue);
    auto recordPointerTypeInfo = Ctx.getTrivialTypeSourceInfo(recordPointerType);
    auto cSetterSelf = clang::ParmVarDecl::Create(Ctx, cSetterDecl,
                                                  clang::SourceLocation(),
                                                  clang::SourceLocation(),
                                                  /* nameID? */ nullptr,
                                                  recordPointerType,
                                                  recordPointerTypeInfo,
                                                  clang::SC_None,
                                                  nullptr);
    cSetterParams.push_back(cSetterSelf);
    cSetterDecl->setParams(cSetterParams);
    
    auto cSetterSelfExpr = new (Ctx) clang::DeclRefExpr(cSetterSelf, false,
                                                        recordPointerType,
                                                        clang::VK_RValue,
                                                        clang::SourceLocation());
    
    auto cSetterMemberExpr = new (Ctx) clang::MemberExpr(cSetterSelfExpr,
                                                         /*isarrow=*/ true,
                                                         clang::SourceLocation(),
                                                         fieldDecl,
                                                         fieldNameInfo,
                                                         fieldType,
                                                         clang::VK_LValue,
                                                         clang::OK_BitField);
    
    auto cSetterValueExpr = new (Ctx) clang::DeclRefExpr(cSetterValue, false,
                                                         fieldType,
                                                         clang::VK_RValue,
                                                         clang::SourceLocation());
    
    auto cSetterExpr = new (Ctx) clang::BinaryOperator(cSetterMemberExpr,
                                                       cSetterValueExpr,
                                                       clang::BO_Assign,
                                                       fieldType,
                                                       clang::VK_RValue,
                                                       clang::OK_Ordinary,
                                                       clang::SourceLocation(),
                                                       /*fpContractable=*/ false);
    
    cSetterDecl->setBody(cSetterExpr);

    Impl.registerExternalDecl(setterDecl);
  }

  return { getterDecl, setterDecl };
}

/// \brief Create a declaration name for anonymous enums, unions and structs.
///
/// Since Swift does not natively support these features, we fake them by
/// importing them as declarations with generated names. The generated name
/// is derived from the name of the field in the outer type. Since the
/// anonymous type is imported as a nested type of the outer type, this
/// generated name will most likely be unique.
static Identifier getClangDeclName(ClangImporter::Implementation &Impl,
                                   const clang::TagDecl *decl) {
  // Import the name of this declaration.
  Identifier name = Impl.importFullName(decl).Imported.getBaseName();
  if (!name.empty()) return name;

  // If that didn't succeed, check whether this is an anonymous tag declaration
  // with a corresponding typedef-name declaration.
  if (decl->getDeclName().isEmpty()) {
    if (auto *typedefForAnon = decl->getTypedefNameForAnonDecl())
      return Impl.importFullName(typedefForAnon).Imported.getBaseName();
  }

  if (!decl->isRecord())
    return name;

  // If the type has no name and no structure name, but is not anonymous,
  // generate a name for it. Specifically this is for cases like:
  //   struct a {
  //     struct {} z;
  //   }
  // Where the member z is an unnamed struct, but does have a member-name
  // and is accessible as a member of struct a.
  if (auto recordDecl = dyn_cast<clang::RecordDecl>(decl->getLexicalDeclContext())) {
    for (auto field : recordDecl->fields()) {
      if (field->getType()->getAsTagDecl() == decl) {
        // We found the field. The field should not be anonymous, since we are
        // using its name to derive the generated declaration name.
        assert(!field->isAnonymousStructOrUnion());

        // Create a name for the declaration from the field name.
        std::string Id;
        llvm::raw_string_ostream IdStream(Id);

        const char *kind;
        if (decl->isStruct())
          kind = "struct";
        else if (decl->isUnion())
          kind = "union";
        else
          llvm_unreachable("unknown decl kind");

        IdStream << "__Unnamed_" << kind
                 << "_" << field->getName();
        return Impl.SwiftContext.getIdentifier(IdStream.str());
      }
    }
  }

  return name;
}

namespace {
  class CFPointeeInfo {
    bool IsValid;
    bool IsConst;
    PointerUnion<const clang::RecordDecl*, const clang::TypedefNameDecl*> Decl;
    CFPointeeInfo() = default;

    static CFPointeeInfo forRecord(bool isConst,
                                   const clang::RecordDecl *decl) {
      assert(decl);
      CFPointeeInfo info;
      info.IsValid = true;
      info.IsConst = isConst;
      info.Decl = decl;
      return info;
    }

    static CFPointeeInfo forTypedef(const clang::TypedefNameDecl *decl) {
      assert(decl);
      CFPointeeInfo info;
      info.IsValid = true;
      info.IsConst = false;
      info.Decl = decl;
      return info;
    }

    static CFPointeeInfo forConstVoid() {
      CFPointeeInfo info;
      info.IsValid = true;
      info.IsConst = true;
      info.Decl = nullptr;
      return info;
    }

    static CFPointeeInfo forInvalid() {
      CFPointeeInfo info;
      info.IsValid = false;
      return info;
    }

  public:
    static CFPointeeInfo classifyTypedef(const clang::TypedefNameDecl *decl);

    bool isValid() const { return IsValid; }
    explicit operator bool() const { return isValid(); }

    bool isConst() const { return IsConst; }

    bool isConstVoid() const {
      assert(isValid());
      return Decl.isNull();
    }

    bool isRecord() const {
      assert(isValid());
      return !Decl.isNull() && Decl.is<const clang::RecordDecl *>();
    }
    const clang::RecordDecl *getRecord() const {
      assert(isRecord());
      return Decl.get<const clang::RecordDecl *>();
    }

    bool isTypedef() const {
      assert(isValid());
      return !Decl.isNull() && Decl.is<const clang::TypedefNameDecl *>();
    }
    const clang::TypedefNameDecl *getTypedef() const {
      assert(isTypedef());
      return Decl.get<const clang::TypedefNameDecl *>();
    }
  };
}

/// The maximum length of any particular string in the whitelist.
const size_t MaxCFWhitelistStringLength = 38;
namespace {
  struct CFWhitelistEntry {
    unsigned char Length;
    char Data[MaxCFWhitelistStringLength + 1];

    operator StringRef() const { return StringRef(Data, Length); }
  };

  // Quasi-lexicographic order: string length first, then string data.
  // Since we don't care about the actual length, we can use this, which
  // lets us ignore the string data a larger proportion of the time.
  struct CFWhitelistComparator {
    bool operator()(StringRef lhs, StringRef rhs) const {
      return (lhs.size() < rhs.size() ||
              (lhs.size() == rhs.size() && lhs < rhs));
    }
  };
}

template <size_t Len>
static constexpr size_t string_lengthof(const char (&data)[Len]) {
  return Len - 1;
}

/// The CF whitelist.  We use 'constexpr' to verify that this is
/// emitted as a constant.  Note that this is expected to be sorted in
/// quasi-lexicographic order.
static constexpr const CFWhitelistEntry CFWhitelist[] = {
#define CF_TYPE(NAME) { string_lengthof(#NAME), #NAME },
#define NON_CF_TYPE(NAME)
#include "SortedCFDatabase.def"
};
const size_t NumCFWhitelistEntries = sizeof(CFWhitelist) / sizeof(*CFWhitelist);

/// Maintain a set of whitelisted CF types.
static bool isWhitelistedCFTypeName(StringRef name) {
  return std::binary_search(CFWhitelist, CFWhitelist + NumCFWhitelistEntries,
                            name, CFWhitelistComparator());
}

/// Classify a potential CF typedef.
CFPointeeInfo
CFPointeeInfo::classifyTypedef(const clang::TypedefNameDecl *typedefDecl) {
  clang::QualType type = typedefDecl->getUnderlyingType();

  if (auto subTypedef = type->getAs<clang::TypedefType>()) {
    if (classifyTypedef(subTypedef->getDecl()))
      return forTypedef(subTypedef->getDecl());
    return forInvalid();
  }

  if (auto ptr = type->getAs<clang::PointerType>()) {
    auto pointee = ptr->getPointeeType();

    // Must be 'const' or nothing.
    clang::Qualifiers quals = pointee.getQualifiers();
    bool isConst = quals.hasConst();
    quals.removeConst();
    if (quals.empty()) {
      if (auto record = pointee->getAs<clang::RecordType>()) {
        auto recordDecl = record->getDecl();
        if (recordDecl->hasAttr<clang::ObjCBridgeAttr>() ||
            recordDecl->hasAttr<clang::ObjCBridgeMutableAttr>() ||
            recordDecl->hasAttr<clang::ObjCBridgeRelatedAttr>() ||
            isWhitelistedCFTypeName(typedefDecl->getName())) {
          return forRecord(isConst, record->getDecl());
        }
      } else if (isConst && pointee->isVoidType()) {
        if (typedefDecl->hasAttr<clang::ObjCBridgeAttr>() ||
            isWhitelistedCFTypeName(typedefDecl->getName())) {
          return forConstVoid();
        }
      }
    }
  }

  return forInvalid();
}

/// Return the name to import a CF typedef as.
static StringRef getImportedCFTypeName(StringRef name) {
  // If the name ends in the CF typedef suffix ("Ref"), drop that.
  if (name.endswith(SWIFT_CFTYPE_SUFFIX))
    return name.drop_back(strlen(SWIFT_CFTYPE_SUFFIX));
  return name;
}

bool ClangImporter::Implementation::isCFTypeDecl(
       const clang::TypedefNameDecl *Decl) {
  if (CFPointeeInfo::classifyTypedef(Decl))
    return true;
  return false;
}

StringRef ClangImporter::Implementation::getCFTypeName(
            const clang::TypedefNameDecl *decl,
            StringRef *secondaryName) {
  if (secondaryName) *secondaryName = "";

  if (auto pointee = CFPointeeInfo::classifyTypedef(decl)) {
    auto name = decl->getName();
    if (pointee.isRecord()) {
      auto resultName = getImportedCFTypeName(name);
      if (secondaryName && name != resultName)
        *secondaryName = name;

      return resultName;
    }

    if (pointee.isTypedef() && secondaryName) {
      StringRef otherName = getImportedCFTypeName(name);
      if (otherName != name) 
        *secondaryName = otherName;
    }

    return name;
  }

  return "";
}

/// Add an AvailableAttr to the declaration for the given
/// version range.
static void applyAvailableAttribute(Decl *decl, VersionRange &range,
                                    ASTContext &C) {
  // If the range is "all", this is the same as not having an available
  // attribute.
  if (!range.hasLowerEndpoint())
    return;

  clang::VersionTuple noVersion;
  auto AvAttr = new (C) AvailableAttr(SourceLoc(), SourceRange(),
                                      targetPlatform(C.LangOpts),
                                      /*message=*/StringRef(),
                                      /*rename=*/StringRef(),
                                      range.getLowerEndpoint(),
                                      /*deprecated=*/noVersion,
                                      /*obsoleted=*/noVersion,
                                      UnconditionalAvailabilityKind::None,
                                      /*implicit=*/false);

  decl->getAttrs().add(AvAttr);
}

/// Synthesize availability attributes for protocol requirements
/// based on availability of the types mentioned in the requirements.
static void inferProtocolMemberAvailability(ClangImporter::Implementation &impl,
                                            DeclContext *dc, Decl *member) {
  // Don't synthesize attributes if there is already an
  // availability annotation.
  if (member->getAttrs().hasAttribute<AvailableAttr>())
    return;

  auto *valueDecl = dyn_cast<ValueDecl>(member);
  if (!valueDecl)
    return;

  VersionRange requiredRange =
      AvailabilityInference::inferForType(valueDecl->getType());

  ASTContext &C = impl.SwiftContext;

  VersionRange containingDeclRange = AvailabilityInference::availableRange(
      dc->getInnermostDeclarationDeclContext(), C);

  requiredRange.constrainWith(containingDeclRange);

  applyAvailableAttribute(valueDecl, requiredRange, C);
}

namespace {
  typedef ClangImporter::Implementation::EnumKind EnumKind;

  /// \brief Convert Clang declarations into the corresponding Swift
  /// declarations.
  class SwiftDeclConverter
    : public clang::ConstDeclVisitor<SwiftDeclConverter, Decl *>
  {
    ClangImporter::Implementation &Impl;
    bool forwardDeclaration = false;

  public:
    explicit SwiftDeclConverter(ClangImporter::Implementation &impl)
      : Impl(impl) { }

    bool hadForwardDeclaration() const {
      return forwardDeclaration;
    }

    Decl *VisitDecl(const clang::Decl *decl) {
      return nullptr;
    }

    Decl *VisitTranslationUnitDecl(const clang::TranslationUnitDecl *decl) {
      // Note: translation units are handled specially by importDeclContext.
      return nullptr;
    }

    Decl *VisitNamespaceDecl(const clang::NamespaceDecl *decl) {
      // FIXME: Implement once Swift has namespaces.
      return nullptr;
    }

    Decl *VisitUsingDirectiveDecl(const clang::UsingDirectiveDecl *decl) {
      // Never imported.
      return nullptr;
    }

    Decl *VisitNamespaceAliasDecl(const clang::NamespaceAliasDecl *decl) {
      // FIXME: Implement once Swift has namespaces.
      return nullptr;
    }

    Decl *VisitLabelDecl(const clang::LabelDecl *decl) {
      // Labels are function-local, and therefore never imported.
      return nullptr;
    }

    /// Try to strip "Mutable" out of a type name.
    clang::IdentifierInfo *
    getImmutableCFSuperclassName(const clang::TypedefNameDecl *decl) {
      StringRef name = decl->getName();

      // Split at the first occurrence of "Mutable".
      StringRef _mutable = "Mutable";
      auto mutableIndex = camel_case::findWord(name, _mutable);
      if (mutableIndex == StringRef::npos)
        return nullptr;

      StringRef namePrefix = name.substr(0, mutableIndex);
      StringRef nameSuffix = name.substr(mutableIndex + _mutable.size());

      // Abort if "Mutable" appears twice.
      if (camel_case::findWord(nameSuffix, _mutable) != StringRef::npos)
        return nullptr;

      llvm::SmallString<128> buffer;
      buffer += namePrefix;
      buffer += nameSuffix;
      return &Impl.getClangASTContext().Idents.get(buffer.str());
    }

    /// Check whether this CF typedef is a Mutable type, and if so,
    /// look for a non-Mutable typedef.
    ///
    /// If the "subclass" is:
    ///   typedef struct __foo *XXXMutableYYY;
    /// then we look for a "superclass" that matches:
    ///   typedef const struct __foo *XXXYYY;
    Type findImmutableCFSuperclass(const clang::TypedefNameDecl *decl,
                                   CFPointeeInfo subclassInfo) {
      // If this type is already immutable, it has no immutable
      // superclass.
      if (subclassInfo.isConst()) return Type();

      // If this typedef name does not contain "Mutable", it has no
      // immutable superclass.
      auto superclassName = getImmutableCFSuperclassName(decl);
      if (!superclassName) return Type();

      // Look for a typedef that successfully classifies as a CF
      // typedef with the same underlying record.
      auto superclassTypedef = Impl.lookupTypedef(superclassName);
      if (!superclassTypedef) return Type();
      auto superclassInfo = CFPointeeInfo::classifyTypedef(superclassTypedef);
      if (!superclassInfo || !superclassInfo.isRecord() ||
          !declaresSameEntity(superclassInfo.getRecord(),
                              subclassInfo.getRecord()))
        return Type();

      // Try to import the superclass.
      Decl *importedSuperclassDecl = Impl.importDeclReal(superclassTypedef);
      if (!importedSuperclassDecl) return Type();

      auto importedSuperclass =
        cast<TypeAliasDecl>(importedSuperclassDecl)->getDeclaredType();
      assert(importedSuperclass->is<ClassType>());
      return importedSuperclass;
    }

    /// Attempt to find a superclass for the given CF typedef.
    Type findCFSuperclass(const clang::TypedefNameDecl *decl,
                          CFPointeeInfo info) {
      if (Type immutable = findImmutableCFSuperclass(decl, info))
        return immutable;

      // TODO: use NSObject if it exists?
      return Type();
    }

    Type importCFClassType(const clang::TypedefNameDecl *decl,
                           Identifier className, CFPointeeInfo info) {
      auto dc = Impl.importDeclContextOf(decl);
      if (!dc) return Type();

      Type superclass = findCFSuperclass(decl, info);

      // TODO: maybe use NSObject as the superclass if we can find it?
      // TODO: try to find a non-mutable type to use as the superclass.

      auto theClass =
        Impl.createDeclWithClangNode<ClassDecl>(decl, SourceLoc(), className,
                                                SourceLoc(), None,
                                                nullptr, dc);
      theClass->computeType();
      theClass->setCircularityCheck(CircularityCheck::Checked);
      theClass->setSuperclass(superclass);
      theClass->setCheckedInheritanceClause();
      theClass->setAddedImplicitInitializers(); // suppress all initializers
      theClass->setForeign(true);
      addObjCAttribute(theClass, None);
      Impl.registerExternalDecl(theClass);

      SmallVector<ProtocolDecl *, 4> protocols;
      theClass->getImplicitProtocols(protocols);
      addObjCProtocolConformances(theClass, protocols);

      // Look for bridging attributes on the clang record.  We can
      // just check the most recent redeclaration, which will inherit
      // any attributes from earlier declarations.
      auto record = info.getRecord()->getMostRecentDecl();
      if (info.isConst()) {
        if (auto attr = record->getAttr<clang::ObjCBridgeAttr>()) {
          // Record the Objective-C class to which this CF type is toll-free
          // bridged.
          if (ClassDecl *objcClass = dyn_cast_or_null<ClassDecl>(
                                       Impl.importDeclByName(
                                         attr->getBridgedType()->getName()))) {
            theClass->getAttrs().add(
              new (Impl.SwiftContext) ObjCBridgedAttr(objcClass));
          }
        }
      } else {
        if (auto attr = record->getAttr<clang::ObjCBridgeMutableAttr>()) {
          // Record the Objective-C class to which this CF type is toll-free
          // bridged.
          if (ClassDecl *objcClass = dyn_cast_or_null<ClassDecl>(
                                       Impl.importDeclByName(
                                         attr->getBridgedType()->getName()))) {
            theClass->getAttrs().add(
              new (Impl.SwiftContext) ObjCBridgedAttr(objcClass));
          }
        }
      }

      return theClass->getDeclaredType();
    }

    Decl *VisitTypedefNameDecl(const clang::TypedefNameDecl *Decl) {
      auto importedName = Impl.importFullName(Decl);
      auto Name = importedName.Imported.getBaseName();
      if (Name.empty())
        return nullptr;

      Type SwiftType;
      if (Decl->getDeclContext()->getRedeclContext()->isTranslationUnit()) {
        bool IsError;
        StringRef StdlibTypeName;
        MappedTypeNameKind NameMapping;
        std::tie(SwiftType, StdlibTypeName) =
            getSwiftStdlibType(Decl, Name, Impl, &IsError, NameMapping);

        if (IsError)
          return nullptr;

        // Import 'typedef struct __Blah *BlahRef;' and
        // 'typedef const void *FooRef;' as CF types if they have the
        // right attributes or match our name whitelist.
        if (!SwiftType) {
          if (auto pointee = CFPointeeInfo::classifyTypedef(Decl)) {
            // If the pointee is a record, consider creating a class type.
            if (pointee.isRecord()) {
              SwiftType = importCFClassType(Decl, Name, pointee);
              if (!SwiftType) return nullptr;
              NameMapping = MappedTypeNameKind::DefineOnly;

              // If there is an alias (i.e., that doesn't have "Ref"),
              // use that as the name of the typedef later.
              if (importedName.Alias)
                Name = importedName.Alias.getBaseName();

            // If the pointee is another CF typedef, create an extra typealias
            // for the name without "Ref", but not a separate type.
            } else if (pointee.isTypedef()) {
              auto underlying =
                cast_or_null<TypeDecl>(Impl.importDecl(pointee.getTypedef()));
              if (!underlying)
                return nullptr;

              if (auto typealias = dyn_cast<TypeAliasDecl>(underlying)) {
                Type doublyUnderlyingTy = typealias->getUnderlyingType();
                if (isa<NameAliasType>(doublyUnderlyingTy.getPointer()))
                  SwiftType = doublyUnderlyingTy;
              }
              if (!SwiftType)
                SwiftType = underlying->getDeclaredType();

              auto DC = Impl.importDeclContextOf(Decl);
              if (!DC)
                return nullptr;

              // If there is an alias (i.e., that doesn't have "Ref"),
              // create that separate typedef.
              if (importedName.Alias) {
                auto aliasWithoutRef =
                  Impl.createDeclWithClangNode<TypeAliasDecl>(
                    Decl,
                    Impl.importSourceLoc(Decl->getLocStart()),
                    importedName.Alias.getBaseName(),
                    Impl.importSourceLoc(Decl->getLocation()),
                    TypeLoc::withoutLoc(SwiftType),
                    DC);

                aliasWithoutRef->computeType();
                SwiftType = aliasWithoutRef->getDeclaredType();
                NameMapping = MappedTypeNameKind::DefineOnly;
              } else {
                NameMapping = MappedTypeNameKind::DefineAndUse;
              }

            // If the pointee is 'const void', 
            // 'CFTypeRef', bring it in specifically as AnyObject.
            } else if (pointee.isConstVoid()) {
              auto proto = Impl.SwiftContext.getProtocol(
                                               KnownProtocolKind::AnyObject);
              if (!proto)
                return nullptr;
              SwiftType = proto->getDeclaredType();
              NameMapping = MappedTypeNameKind::DefineOnly;
            }
          }
        }

        if (SwiftType) {
          // Note that this typedef-name is special.
          Impl.SpecialTypedefNames[Decl->getCanonicalDecl()] = NameMapping;

          if (NameMapping == MappedTypeNameKind::DoNothing) {
            // Record the remapping using the name of the Clang declaration.
            // This will be useful for type checker diagnostics when
            // a user tries to use the Objective-C/C type instead of the
            // Swift type.
            Impl.SwiftContext.RemappedTypes[Decl->getNameAsString()]
              = SwiftType;

            // Don't create an extra typealias in the imported module because
            // doing so will cause confusion (or even lookup ambiguity) between
            // the name in the imported module and the same name in the
            // standard library.
            if (auto *NAT = dyn_cast<NameAliasType>(SwiftType.getPointer()))
              return NAT->getDecl();

            auto *NTD = SwiftType->getAnyNominal();
            assert(NTD);
            return NTD;
          }
        }
      }

      auto DC = Impl.importDeclContextOf(Decl);
      if (!DC)
        return nullptr;

      if (!SwiftType) {
        // Import typedefs of blocks as their fully-bridged equivalent Swift
        // type. That matches how we want to use them in most cases. All other
        // types should be imported in a non-bridged way.
        clang::QualType ClangType = Decl->getUnderlyingType();
        SwiftType = Impl.importType(ClangType,
                                    ImportTypeKind::Typedef,
                                    isInSystemModule(DC),
                                    ClangType->isBlockPointerType());
      }

      if (!SwiftType)
        return nullptr;

      auto Loc = Impl.importSourceLoc(Decl->getLocation());
      auto Result = Impl.createDeclWithClangNode<TypeAliasDecl>(Decl,
                                      Impl.importSourceLoc(Decl->getLocStart()),
                                      Name,
                                      Loc,
                                      TypeLoc::withoutLoc(SwiftType),
                                      DC);
      Result->computeType();
      return Result;
    }

    Decl *
    VisitUnresolvedUsingTypenameDecl(const
                                     clang::UnresolvedUsingTypenameDecl *decl) {
      // Note: only occurs in templates.
      return nullptr;
    }
    
    /// \brief Create a default constructor that initializes a struct to zero.
    ConstructorDecl *createDefaultConstructor(StructDecl *structDecl) {
      auto &context = Impl.SwiftContext;
      
      // Create the 'self' declaration.
      auto selfType = structDecl->getDeclaredTypeInContext();
      auto selfMetatype = MetatypeType::get(selfType);
      auto selfDecl = createSelfDecl(structDecl, false);
      Pattern *selfPattern = createTypedNamedPattern(selfDecl);

      // The default initializer takes no arguments.
      auto paramPattern = TuplePattern::create(context, SourceLoc(), {},
                                               SourceLoc());
      auto emptyTy = TupleType::getEmpty(context);

      // Create the constructor.
      DeclName name(context, context.Id_init, {});
      auto constructor =
        new (context) ConstructorDecl(name, structDecl->getLoc(),
                                      OTK_None, SourceLoc(),
                                      selfPattern, paramPattern,
                                      nullptr, SourceLoc(), structDecl);
      
      // Set the constructor's type.
      auto fnTy = FunctionType::get(emptyTy, selfType);
      auto allocFnTy = FunctionType::get(selfMetatype, fnTy);
      auto initFnTy = FunctionType::get(selfType, fnTy);
      constructor->setType(allocFnTy);
      constructor->setInitializerType(initFnTy);
      constructor->setAccessibility(Accessibility::Public);

      // Mark the constructor transparent so that we inline it away completely.
      constructor->getAttrs().add(
                              new (context) TransparentAttr(/*implicit*/ true));

      // Use a builtin to produce a zero initializer, and assign it to self.
      constructor->setBodySynthesizer([](AbstractFunctionDecl *constructor) {
        ASTContext &context = constructor->getASTContext();

        // Construct the left-hand reference to self.
        Expr *lhs =
            new (context) DeclRefExpr(constructor->getImplicitSelfDecl(),
                                      SourceLoc(), /*implicit=*/true);

        // Construct the right-hand call to Builtin.zeroInitializer.
        Identifier zeroInitID = context.getIdentifier("zeroInitializer");
        auto zeroInitializerFunc =
            cast<FuncDecl>(getBuiltinValueDecl(context, zeroInitID));
        auto zeroInitializerRef = new (context) DeclRefExpr(zeroInitializerFunc,
                                                            SourceLoc(),
                                                            /*implicit*/ true);
        auto emptyTuple = TupleExpr::createEmpty(context, SourceLoc(),
                                                 SourceLoc(),
                                                 /*implicit*/ true);
        auto call = new (context) CallExpr(zeroInitializerRef, emptyTuple,
                                           /*implicit*/ true);

        auto assign = new (context) AssignExpr(lhs, SourceLoc(), call,
                                               /*implicit*/ true);

        // Create the function body.
        auto body = BraceStmt::create(context, SourceLoc(), { assign },
                                      SourceLoc());
        constructor->setBody(body);
      });

      // Add this as an external definition.
      Impl.registerExternalDecl(constructor);

      // We're done.
      return constructor;
    }

    /// \brief Create a constructor that initializes a struct from its members.
    ConstructorDecl *createValueConstructor(StructDecl *structDecl,
                                            ArrayRef<VarDecl *> members,
                                            bool wantCtorParamNames,
                                            bool wantBody) {
      auto &context = Impl.SwiftContext;

      // Create the 'self' declaration.
      auto selfType = structDecl->getDeclaredTypeInContext();
      auto selfMetatype = MetatypeType::get(selfType);
      auto selfDecl = createSelfDecl(structDecl, false);

      Pattern *selfPattern = createTypedNamedPattern(selfDecl);

      // Construct the set of parameters from the list of members.
      SmallVector<Pattern *, 4> paramPatterns;
      SmallVector<TuplePatternElt, 8> patternElts;
      SmallVector<TupleTypeElt, 8> tupleElts;
      SmallVector<VarDecl *, 8> params;
      SmallVector<Identifier, 4> argNames;
      for (auto var : members) {
        Identifier argName = wantCtorParamNames ? var->getName()
                                                : Identifier();
        auto param = new (context) ParamDecl(/*IsLet*/ true,
                                             SourceLoc(), argName,
                                             SourceLoc(), var->getName(),
                                             var->getType(), structDecl);
        argNames.push_back(argName);
        params.push_back(param);
        Pattern *pattern = createTypedNamedPattern(param);
        paramPatterns.push_back(pattern);
        patternElts.push_back(TuplePatternElt(pattern));
        patternElts.back().setLabel(argName, SourceLoc());
        tupleElts.push_back(TupleTypeElt(var->getType(), var->getName()));
      }
      auto paramPattern = TuplePattern::create(context, SourceLoc(), patternElts,
                                               SourceLoc());
      auto paramTy = TupleType::get(tupleElts, context);
      paramPattern->setType(paramTy);
      paramTy = paramTy->getRelabeledType(context, argNames);

      // Create the constructor
      DeclName name(context, context.Id_init, argNames);
      auto constructor =
        new (context) ConstructorDecl(name, structDecl->getLoc(),
                                      OTK_None, SourceLoc(),
                                      selfPattern, paramPattern,
                                      nullptr, SourceLoc(), structDecl);

      // Set the constructor's type.
      auto fnTy = FunctionType::get(paramTy, selfType);
      auto allocFnTy = FunctionType::get(selfMetatype, fnTy);
      auto initFnTy = FunctionType::get(selfType, fnTy);
      constructor->setType(allocFnTy);
      constructor->setInitializerType(initFnTy);
      constructor->setAccessibility(Accessibility::Public);

      // Make the constructor transparent so we inline it away completely.
      constructor->getAttrs().add(
                              new (context) TransparentAttr(/*implicit*/ true));

      if (wantBody) {
        // Assign all of the member variables appropriately.
        SmallVector<ASTNode, 4> stmts;

        // To keep DI happy, initialize stored properties before computed.
        for (unsigned pass = 0; pass < 2; pass++) {
          for (unsigned i = 0, e = members.size(); i < e; i++) {
            auto var = members[i];
            if (var->hasStorage() == (pass != 0))
              continue;

            // Construct left-hand side.
            Expr *lhs = new (context) DeclRefExpr(selfDecl, SourceLoc(),
                                                  /*Implicit=*/true);
            lhs = new (context) MemberRefExpr(lhs, SourceLoc(), var, SourceLoc(),
                                              /*Implicit=*/true);

            // Construct right-hand side.
            auto rhs = new (context) DeclRefExpr(params[i], SourceLoc(),
                                                 /*Implicit=*/true);

            // Add assignment.
            stmts.push_back(new (context) AssignExpr(lhs, SourceLoc(), rhs,
                                                     /*Implicit=*/true));
          }
        }

        // Create the function body.
        auto body = BraceStmt::create(context, SourceLoc(), stmts, SourceLoc());
        constructor->setBody(body);
      }

      // Add this as an external definition.
      Impl.registerExternalDecl(constructor);

      // We're done.
      return constructor;
    }
    
    /// Import an NS_ENUM constant as a case of a Swift enum.
    Decl *importEnumCase(const clang::EnumConstantDecl *decl,
                         const clang::EnumDecl *clangEnum,
                         EnumDecl *theEnum) {
      auto &context = Impl.SwiftContext;
      auto name = Impl.importFullName(decl).Imported.getBaseName();
      if (name.empty())
        return nullptr;
      
      // Use the constant's underlying value as its raw value in Swift.
      bool negative = false;
      llvm::APSInt rawValue = decl->getInitVal();
      
      // Did we already import an enum constant for this enum with the
      // same value? If so, import it as a standalone constant.

      auto insertResult =
          Impl.EnumConstantValues.insert({{clangEnum, rawValue}, nullptr});
      if (!insertResult.second)
        return importEnumCaseAlias(decl, insertResult.first->second,
                                   clangEnum, theEnum);

      if (clangEnum->getIntegerType()->isSignedIntegerOrEnumerationType()
          && rawValue.slt(0)) {
        rawValue = -rawValue;
        negative = true;
      }
      llvm::SmallString<12> rawValueText;
      rawValue.toString(rawValueText, 10, /*signed*/ false);
      StringRef rawValueTextC
        = context.AllocateCopy(StringRef(rawValueText));
      auto rawValueExpr = new (context) IntegerLiteralExpr(rawValueTextC,
                                                       SourceLoc(),
                                                       /*implicit*/ false);
      if (negative)
        rawValueExpr->setNegative(SourceLoc());
      
      auto element
        = Impl.createDeclWithClangNode<EnumElementDecl>(decl, SourceLoc(),
                                        name, TypeLoc(),
                                        SourceLoc(), rawValueExpr,
                                        theEnum);
      insertResult.first->second = element;

      // Give the enum element the appropriate type.
      element->computeType();

      Impl.importAttributes(decl, element);

      return element;
    }
    
    /// Import an NS_OPTIONS constant as a static property of a Swift struct.
    ///
    /// This is also used to import enum case aliases.
    Decl *importOptionConstant(const clang::EnumConstantDecl *decl,
                               const clang::EnumDecl *clangEnum,
                               NominalTypeDecl *theStruct) {
      auto name = Impl.importFullName(decl).Imported.getBaseName();
      if (name.empty())
        return nullptr;
      
      // Create the constant.
      auto convertKind = ConstantConvertKind::Construction;
      if (isa<EnumDecl>(theStruct))
        convertKind = ConstantConvertKind::ConstructionWithUnwrap;
      Decl *CD = Impl.createConstant(name, theStruct,
                                     theStruct->getDeclaredTypeInContext(),
                                     clang::APValue(decl->getInitVal()),
                                     convertKind, /*isStatic*/ true, decl);
      Impl.importAttributes(decl, CD);
      return CD;
    }

    /// Import \p alias as an alias for the imported constant \p original.
    ///
    /// This builds the getter in a way that's compatible with switch
    /// statements. Changing the body here may require changing
    /// TypeCheckPattern.cpp as well.
    Decl *importEnumCaseAlias(const clang::EnumConstantDecl *alias,
                              EnumElementDecl *original,
                              const clang::EnumDecl *clangEnum,
                              NominalTypeDecl *importedEnum) {
      auto name = Impl.importFullName(alias).Imported.getBaseName();
      if (name.empty())
        return nullptr;
      
      // Construct the original constant. Enum constants without payloads look
      // like simple values, but actually have type 'MyEnum.Type -> MyEnum'.
      auto constantRef = new (Impl.SwiftContext) DeclRefExpr(original,
                                                             SourceLoc(),
                                                             /*implicit*/true);
      Type importedEnumTy = importedEnum->getDeclaredTypeInContext();
      auto typeRef = TypeExpr::createImplicit(importedEnumTy,
                                              Impl.SwiftContext);
      auto instantiate = new (Impl.SwiftContext) DotSyntaxCallExpr(constantRef,
                                                                   SourceLoc(),
                                                                   typeRef);
      instantiate->setType(importedEnumTy);

      Decl *CD = Impl.createConstant(name, importedEnum, importedEnumTy,
                                     instantiate, ConstantConvertKind::None,
                                     /*isStatic*/ true, alias);
      Impl.importAttributes(alias, CD);
      return CD;
    }

    template<unsigned N>
    void populateInheritedTypes(NominalTypeDecl *nominal,
                                ProtocolDecl * const (&protocols)[N]) {
      TypeLoc inheritedTypes[N];
      for_each(MutableArrayRef<TypeLoc>(inheritedTypes),
               ArrayRef<ProtocolDecl *>(protocols),
               [](TypeLoc &tl, ProtocolDecl *proto) {
                 tl = TypeLoc::withoutLoc(proto->getDeclaredType());
               });
      nominal->setInherited(Impl.SwiftContext.AllocateCopy(inheritedTypes));
      nominal->setCheckedInheritanceClause();
    }

    NominalTypeDecl *importAsOptionSetType(DeclContext *dc,
                                           Identifier name,
                                           const clang::EnumDecl *decl) {
      ASTContext &cxt = Impl.SwiftContext;
      
      // Compute the underlying type.
      auto underlyingType = Impl.importType(decl->getIntegerType(),
                                            ImportTypeKind::Enum,
                                            isInSystemModule(dc),
                                            /*isFullyBridgeable*/false);
      if (!underlyingType)
        return nullptr;

      auto Loc = Impl.importSourceLoc(decl->getLocation());

      // Create a struct with the underlying type as a field.
      auto structDecl = Impl.createDeclWithClangNode<StructDecl>(decl,
        Loc, name, Loc, None, nullptr, dc);
      structDecl->computeType();

      // Note that this is a raw option set type.
      structDecl->getAttrs().add(
        new (Impl.SwiftContext) SynthesizedProtocolAttr(
                                  KnownProtocolKind::OptionSetType));

      
      // Create a field to store the underlying value.
      auto varName = Impl.SwiftContext.Id_rawValue;
      auto var = new (Impl.SwiftContext) VarDecl(/*static*/ false,
                                                 /*IsLet*/ true,
                                                 SourceLoc(), varName,
                                                 underlyingType,
                                                 structDecl);
      var->setImplicit();
      var->setAccessibility(Accessibility::Public);
      var->setSetterAccessibility(Accessibility::Private);

      // Create a pattern binding to describe the variable.
      Pattern *varPattern = createTypedNamedPattern(var);

      auto patternBinding =
          PatternBindingDecl::create(Impl.SwiftContext, SourceLoc(),
                                     StaticSpellingKind::None, SourceLoc(),
                                     varPattern, nullptr, structDecl);
      
      // Create the init(rawValue:) constructor.
      auto labeledValueConstructor = createValueConstructor(
                                structDecl, var,
                                /*wantCtorParamNames=*/true,
                                /*wantBody=*/!Impl.hasFinishedTypeChecking());

      // Build an OptionSetType conformance for the type.
      ProtocolDecl *protocols[]
        = {cxt.getProtocol(KnownProtocolKind::OptionSetType)};
      populateInheritedTypes(structDecl, protocols);

      structDecl->addMember(labeledValueConstructor);
      structDecl->addMember(patternBinding);
      structDecl->addMember(var);
      return structDecl;
    }
    

    Decl *VisitEnumDecl(const clang::EnumDecl *decl) {
      decl = decl->getDefinition();
      if (!decl) {
        forwardDeclaration = true;
        return nullptr;
      }
      
      auto name = getClangDeclName(Impl, decl);
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;
      
      ASTContext &cxt = Impl.SwiftContext;
      
      // Create the enum declaration and record it.
      NominalTypeDecl *result;
      auto enumKind = Impl.classifyEnum(Impl.getClangPreprocessor(), decl);
      switch (enumKind) {
      case EnumKind::Constants: {
        // There is no declaration. Rather, the type is mapped to the
        // underlying type.
        return nullptr;
      }

      case EnumKind::Unknown: {
        // Compute the underlying type of the enumeration.
        auto underlyingType = Impl.importType(decl->getIntegerType(),
                                              ImportTypeKind::Enum,
                                              isInSystemModule(dc),
                                              /*isFullyBridgeable*/false);
        if (!underlyingType)
          return nullptr;

        auto Loc = Impl.importSourceLoc(decl->getLocation());
        auto structDecl = Impl.createDeclWithClangNode<StructDecl>(decl,
          Loc, name, Loc, None, nullptr, dc);
        structDecl->computeType();

        ProtocolDecl *protocols[]
          = {cxt.getProtocol(KnownProtocolKind::RawRepresentable),
             cxt.getProtocol(KnownProtocolKind::Equatable)};
        populateInheritedTypes(structDecl, protocols);

        // Note that this is a raw representable type.
        structDecl->getAttrs().add(
          new (Impl.SwiftContext) SynthesizedProtocolAttr(
                                    KnownProtocolKind::RawRepresentable));

        // Create a variable to store the underlying value.
        auto varName = Impl.SwiftContext.Id_rawValue;
        auto var = new (Impl.SwiftContext) VarDecl(/*static*/ false,
                                                   /*IsLet*/ false,
                                                   SourceLoc(), varName,
                                                   underlyingType,
                                                   structDecl);
        var->setAccessibility(Accessibility::Public);
        var->setSetterAccessibility(Accessibility::Public);

        // Create a pattern binding to describe the variable.
        Pattern *varPattern = createTypedNamedPattern(var);

        auto patternBinding =
            PatternBindingDecl::create(Impl.SwiftContext, SourceLoc(),
                                       StaticSpellingKind::None, SourceLoc(),
                                       varPattern, nullptr, structDecl);

        // Create a constructor to initialize that value from a value of the
        // underlying type.
        auto valueConstructor =
            createValueConstructor(structDecl, var,
                                   /*wantCtorParamNames=*/false,
                                   /*wantBody=*/!Impl.hasFinishedTypeChecking());
        auto labeledValueConstructor =
            createValueConstructor(structDecl, var,
                                   /*wantCtorParamNames=*/true,
                                   /*wantBody=*/!Impl.hasFinishedTypeChecking());

        // Add delayed implicit members to the type.
        auto &Impl = this->Impl;
        structDecl->setDelayedMemberDecls(
            [=, &Impl](SmallVectorImpl<Decl *> &NewDecls) {
              auto rawGetter = makeRawValueTrivialGetter(Impl, structDecl, var);
              NewDecls.push_back(rawGetter);
              auto rawSetter = makeRawValueTrivialSetter(Impl, structDecl, var);
              NewDecls.push_back(rawSetter);
              // FIXME: MaterializeForSet?
              var->addTrivialAccessors(rawGetter, rawSetter, nullptr);
            });

        // Set the members of the struct.
        structDecl->addMember(valueConstructor);
        structDecl->addMember(labeledValueConstructor);
        structDecl->addMember(patternBinding);
        structDecl->addMember(var);

        result = structDecl;
        break;
      }

      case EnumKind::Enum: {
        EnumDecl *nativeDecl;
        bool declaredNative = hasNativeSwiftDecl(decl, name, dc, nativeDecl);
        if (declaredNative && nativeDecl)
          return nativeDecl;

        // Compute the underlying type.
        auto underlyingType = Impl.importType(decl->getIntegerType(),
                                              ImportTypeKind::Enum,
                                              isInSystemModule(dc),
                                              /*isFullyBridgeable*/false);
        if (!underlyingType)
          return nullptr;
        
        auto enumDecl = Impl.createDeclWithClangNode<EnumDecl>(decl,
                   Impl.importSourceLoc(decl->getLocStart()),
                   name, Impl.importSourceLoc(decl->getLocation()),
                   None, nullptr, dc);
        enumDecl->computeType();
        
        // Set up the C underlying type as its Swift raw type.
        enumDecl->setRawType(underlyingType);
        
        // Add protocol declarations to the enum declaration.
        enumDecl->setInherited(
          Impl.SwiftContext.AllocateCopy(
            llvm::makeArrayRef(TypeLoc::withoutLoc(underlyingType))));
        enumDecl->setCheckedInheritanceClause();

        // Provide custom implementations of the init(rawValue:) and rawValue
        // conversions that just do a bitcast. We can't reliably filter a
        // C enum without additional knowledge that the type has no
        // undeclared values, and won't ever add cases.
        auto rawValueConstructor = makeEnumRawValueConstructor(Impl, enumDecl);

        auto varName = Impl.SwiftContext.Id_rawValue;
        auto rawValue = new (Impl.SwiftContext) VarDecl(/*static*/ false,
                                                   /*IsLet*/ false,
                                                   SourceLoc(), varName,
                                                   underlyingType,
                                                   enumDecl);
        rawValue->setImplicit();
        rawValue->setAccessibility(Accessibility::Public);
        rawValue->setSetterAccessibility(Accessibility::Private);
        
        // Create a pattern binding to describe the variable.
        Pattern *varPattern = createTypedNamedPattern(rawValue);
        
        auto rawValueBinding =
          PatternBindingDecl::create(Impl.SwiftContext, SourceLoc(),
                                     StaticSpellingKind::None, SourceLoc(),
                                     varPattern, nullptr, enumDecl);

        auto rawValueGetter = makeEnumRawValueGetter(Impl, enumDecl, rawValue);

        enumDecl->addMember(rawValueConstructor);
        enumDecl->addMember(rawValueGetter);
        enumDecl->addMember(rawValue);
        enumDecl->addMember(rawValueBinding);
        
        result = enumDecl;
        break;
      }
          
      case EnumKind::Options: {
        result = importAsOptionSetType(dc, name, decl);
        if (!result)
          return nullptr;
        
        break;
      }
      }
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

      // Import each of the enumerators.
      
      bool addEnumeratorsAsMembers;
      switch (enumKind) {
      case EnumKind::Constants:
      case EnumKind::Unknown:
        addEnumeratorsAsMembers = false;
        break;
      case EnumKind::Options:
      case EnumKind::Enum:
        addEnumeratorsAsMembers = true;
        break;
      }
      
      for (auto ec = decl->enumerator_begin(), ecEnd = decl->enumerator_end();
           ec != ecEnd; ++ec) {
        Decl *enumeratorDecl;
        switch (enumKind) {
        case EnumKind::Constants:
        case EnumKind::Unknown:
          enumeratorDecl = Impl.importDecl(*ec);
          break;
        case EnumKind::Options:
          enumeratorDecl = importOptionConstant(*ec, decl, result);
          break;
        case EnumKind::Enum:
          enumeratorDecl = importEnumCase(*ec, decl, cast<EnumDecl>(result));
          break;
        }
        if (!enumeratorDecl)
          continue;

        if (addEnumeratorsAsMembers) {
          result->addMember(enumeratorDecl);
          if (auto *var = dyn_cast<VarDecl>(enumeratorDecl))
            result->addMember(var->getGetter());
        }
      }

      // Add the type decl to ExternalDefinitions so that we can type-check
      // raw values and IRGen can emit metadata for it.
      // FIXME: There might be better ways to do this.
      Impl.registerExternalDecl(result);
      return result;
    }

    Decl *VisitRecordDecl(const clang::RecordDecl *decl) {
      // Track whether this record contains fields we can't reference in Swift
      // as stored properties.
      bool hasUnreferenceableStorage = false;
      
      // Track whether this record contains fields that can't be zero-
      // initialized.
      bool hasZeroInitializableStorage = true;

      // Track whether all fields in this record can be referenced in Swift,
      // either as stored or computed properties, in which case the record type
      // gets a memberwise initializer.
      bool hasMemberwiseInitializer = true;

      if (decl->isUnion()) {
        hasUnreferenceableStorage = true;

        // We generate initializers specially for unions below.
        hasMemberwiseInitializer = false;
      }

      // FIXME: Skip Microsoft __interfaces.
      if (decl->isInterface())
        return nullptr;

      // The types of anonymous structs or unions are never imported; their
      // fields are dumped directly into the enclosing class.
      if (decl->isAnonymousStructOrUnion())
        return nullptr;

      // FIXME: Figure out how to deal with incomplete types, since that
      // notion doesn't exist in Swift.
      decl = decl->getDefinition();
      if (!decl) {
        forwardDeclaration = true;
        return nullptr;
      }

      auto name = getClangDeclName(Impl, decl);
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      // Create the struct declaration and record it.
      auto result = Impl.createDeclWithClangNode<StructDecl>(decl,
                                 Impl.importSourceLoc(decl->getLocStart()),
                                 name,
                                 Impl.importSourceLoc(decl->getLocation()),
                                 None, nullptr, dc);
      result->computeType();
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

      // FIXME: Figure out what to do with superclasses in C++. One possible
      // solution would be to turn them into members and add conversion
      // functions.

      // Import each of the members.
      SmallVector<VarDecl *, 4> members;
      SmallVector<ConstructorDecl *, 4> ctors;

      // FIXME: Import anonymous union fields and support field access when
      // it is nested in a struct.

      for (auto m : decl->decls()) {
        auto nd = dyn_cast<clang::NamedDecl>(m);
        if (!nd) {
          // We couldn't import the member, so we can't reference it in Swift.
          hasUnreferenceableStorage = true;
          hasMemberwiseInitializer = false;
          continue;
        }

        if (auto field = dyn_cast<clang::FieldDecl>(nd)) {
          // Skip anonymous structs or unions; they'll be dealt with via the
          // IndirectFieldDecls.
          if (field->isAnonymousStructOrUnion())
            continue;

          // Non-nullable pointers can't be zero-initialized.
          if (auto nullability = field->getType()
                ->getNullability(Impl.getClangASTContext())) {
            if (*nullability == clang::NullabilityKind::NonNull)
              hasZeroInitializableStorage = false;
          }
          // TODO: If we had the notion of a closed enum with no private
          // cases or resilience concerns, then complete NS_ENUMs with
          // no case corresponding to zero would also not be zero-
          // initializable.

          // Unnamed bitfields are just for padding and should not
          // inhibit creation of a memberwise initializer.
          if (field->isUnnamedBitfield()) {
            hasUnreferenceableStorage = true;
            continue;
          }
        }

        auto member = Impl.importDecl(nd);
        if (!member) {
          // We don't know what this field is. Assume it may be important in C.
          hasUnreferenceableStorage = true;
          hasMemberwiseInitializer = false;
          continue;
        }

        if (isa<TypeDecl>(member)) {
          // A struct nested inside another struct will either be logically
          // a sibling of the outer struct, or contained inside of it, depending
          // on if it has a declaration name or not.
          //
          // struct foo { struct bar { ... } baz; } // sibling
          // struct foo { struct { ... } baz; } // child
          //
          // In the latter case, we add the imported type as a nested type
          // of the parent.
          //
          // TODO: C++ types have different rules.
          if (auto nominalDecl = dyn_cast<NominalTypeDecl>(member->getDeclContext())) {
            assert(nominalDecl == result && "interesting nesting of C types?");
            nominalDecl->addMember(member);
          }
          continue;
        }

        auto VD = cast<VarDecl>(member);

        // Bitfields are imported as computed properties with Clang-generated
        // accessors.
        if (auto field = dyn_cast<clang::FieldDecl>(nd)) {
          if (field->isBitField()) {
            // We can't represent this struct completely in SIL anymore,
            // but we're still able to define a memberwise initializer.
            hasUnreferenceableStorage = true;

            makeBitFieldAccessors(Impl,
                                  const_cast<clang::RecordDecl *>(decl),
                                  result,
                                  const_cast<clang::FieldDecl *>(field),
                                  VD);
          }
        }

        if (decl->isUnion()) {
          // Union fields should only be available indirectly via a computed
          // property. Since the union is made of all of the fields at once,
          // this is a trivial accessor that casts self to the correct
          // field type.

          // FIXME: Allow indirect field access of anonymous structs.
          if (isa<clang::IndirectFieldDecl>(nd))
            continue;

          Decl *getter, *setter;
          std::tie(getter, setter) = makeUnionFieldAccessors(Impl, result, VD);
          members.push_back(VD);

          // Create labeled inititializers for unions that take one of the
          // fields, which only initializes the data for that field.
          auto valueCtor =
              createValueConstructor(result, VD,
                                     /*want param names*/true,
                                     /*wantBody=*/!Impl.hasFinishedTypeChecking());
          ctors.push_back(valueCtor);
        } else {
          members.push_back(VD);
        }
      }

      bool hasReferenceableFields = !members.empty();

      if (hasZeroInitializableStorage) {
        // Add constructors for the struct.
        ctors.push_back(createDefaultConstructor(result));
        if (hasReferenceableFields && hasMemberwiseInitializer) {
          // The default zero initializer suppresses the implicit value
          // constructor that would normally be formed, so we have to add that
          // explicitly as well.
          //
          // If we can completely represent the struct in SIL, leave the body
          // implicit, otherwise synthesize one to call property setters.
          bool wantBody = (hasUnreferenceableStorage &&
                           !Impl.hasFinishedTypeChecking());
          auto valueCtor = createValueConstructor(result, members,
                                                  /*want param names*/true,
                                                  /*want body*/wantBody);
          if (!hasUnreferenceableStorage)
            valueCtor->setIsMemberwiseInitializer();

          ctors.push_back(valueCtor);
        }
      }

      for (auto member : members) {
        result->addMember(member);
      }
      
      for (auto ctor : ctors) {
        result->addMember(ctor);
      }

      result->setHasUnreferenceableStorage(hasUnreferenceableStorage);

      // Add the struct decl to ExternalDefinitions so that IRGen can emit
      // metadata for it.
      // FIXME: There might be better ways to do this.
      Impl.registerExternalDecl(result);

      return result;
    }

    Decl *VisitClassTemplateSpecializationDecl(
                 const clang::ClassTemplateSpecializationDecl *decl) {
      // FIXME: We could import specializations, but perhaps only as unnamed
      // structural types.
      return nullptr;
    }

    Decl *VisitClassTemplatePartialSpecializationDecl(
                 const clang::ClassTemplatePartialSpecializationDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitTemplateTypeParmDecl(const clang::TemplateTypeParmDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitEnumConstantDecl(const clang::EnumConstantDecl *decl) {
      auto clangEnum = cast<clang::EnumDecl>(decl->getDeclContext());
      
      auto name = Impl.importFullName(decl).Imported.getBaseName();
      if (name.empty())
        return nullptr;

      switch (Impl.classifyEnum(Impl.getClangPreprocessor(), clangEnum)) {
      case EnumKind::Constants: {
        // The enumeration was simply mapped to an integral type. Create a
        // constant with that integral type.

        // The context where the constant will be introduced.
        auto dc = Impl.importDeclContextOf(clangEnum);
        if (!dc)
          return nullptr;

        // Enumeration type.
        auto &clangContext = Impl.getClangASTContext();
        auto type = Impl.importType(clangContext.getTagDeclType(clangEnum),
                                    ImportTypeKind::Value,
                                    isInSystemModule(dc),
                                    /*isFullyBridgeable*/false);
        if (!type)
          return nullptr;
        // FIXME: Importing the type will recursively revisit this same
        // EnumConstantDecl. Short-circuit out if we already emitted the import
        // for this decl.
        if (auto Known = Impl.importDeclCached(decl))
          return Known;

        // Create the global constant.
        auto result = Impl.createConstant(name, dc, type,
                                          clang::APValue(decl->getInitVal()),
                                          ConstantConvertKind::Coerce,
                                          /*static*/ false, decl);
        Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
        return result;
      }

      case EnumKind::Unknown: {
        // The enumeration was mapped to a struct containing the integral
        // type. Create a constant with that struct type.

        auto dc = Impl.importDeclContextOf(clangEnum);
        if (!dc)
          return nullptr;

        // Import the enumeration type.
        auto enumType = Impl.importType(
                          Impl.getClangASTContext().getTagDeclType(clangEnum),
                          ImportTypeKind::Value,
                          isInSystemModule(dc),
                          /*isFullyBridgeable*/false);
        if (!enumType)
          return nullptr;
        // FIXME: Importing the type will can recursively revisit this same
        // EnumConstantDecl. Short-circuit out if we already emitted the import
        // for this decl.
        if (auto Known = Impl.importDeclCached(decl))
          return Known;

        // Create the global constant.
        auto result = Impl.createConstant(name, dc, enumType,
                                          clang::APValue(decl->getInitVal()),
                                          ConstantConvertKind::Construction,
                                          /*static*/ false, decl);
        Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
        return result;
      }

      case EnumKind::Enum:
      case EnumKind::Options: {
        // The enumeration was mapped to a high-level Swift type, and its
        // elements were created as children of that enum. They aren't available
        // independently.
        return nullptr;
      }
      }
    }


    Decl *
    VisitUnresolvedUsingValueDecl(const clang::UnresolvedUsingValueDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitIndirectFieldDecl(const clang::IndirectFieldDecl *decl) {
      // Check whether the context of any of the fields in the chain is a
      // union. If so, don't import this field.
      for (auto f = decl->chain_begin(), fEnd = decl->chain_end(); f != fEnd;
           ++f) {
        if (auto record = dyn_cast<clang::RecordDecl>((*f)->getDeclContext())) {
          if (record->isUnion())
            return nullptr;
        }
      }
      auto name = Impl.importFullName(decl).Imported.getBaseName();
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      auto type = Impl.importType(decl->getType(),
                                  ImportTypeKind::Variable,
                                  isInSystemModule(dc),
                                  /*isFullyBridgeable*/false);
      if (!type)
        return nullptr;

      // Map this indirect field to a Swift variable.
      auto result = Impl.createDeclWithClangNode<VarDecl>(decl,
                       /*static*/ false, /*IsLet*/ false,
                       Impl.importSourceLoc(decl->getLocStart()),
                       name, type, dc);
      return result;
    }

    Decl *VisitFunctionDecl(const clang::FunctionDecl *decl) {
      decl = decl->getMostRecentDecl();

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      // Determine the name of the function.
      auto importedName = Impl.importFullName(decl);
      if (!importedName)
        return nullptr;

      DeclName name = importedName.Imported;
      bool hasCustomName = importedName.HasCustomName;

      // Import the function type. If we have parameters, make sure their names
      // get into the resulting function type.
      SmallVector<Pattern *, 4> bodyPatterns;
      Type type = Impl.importFunctionType(decl,
                                          decl->getReturnType(),
                                          { decl->param_begin(),
                                            decl->param_size() },
                                          decl->isVariadic(),
                                          decl->isNoReturn(),
                                          isInSystemModule(dc),
                                          hasCustomName,
                                          bodyPatterns,
                                          name);
      if (!type)
        return nullptr;

      auto resultTy = type->castTo<FunctionType>()->getResult();
      auto loc = Impl.importSourceLoc(decl->getLocation());

      // If we had no argument labels to start with, add empty labels now.
      if (name.isSimpleName()) {
        llvm::SmallVector<Identifier, 2>
          argNames(bodyPatterns[0]->numTopLevelVariables(), Identifier());
        name = DeclName(Impl.SwiftContext, name.getBaseName(), argNames);
      }

      // FIXME: Poor location info.
      auto nameLoc = Impl.importSourceLoc(decl->getLocation());
      auto result = FuncDecl::create(
          Impl.SwiftContext, SourceLoc(), StaticSpellingKind::None, loc,
          name, nameLoc, SourceLoc(), SourceLoc(),
          /*GenericParams=*/nullptr, type, bodyPatterns,
          TypeLoc::withoutLoc(resultTy), dc, decl);

      result->setBodyResultType(resultTy);

      result->setAccessibility(Accessibility::Public);

      if (decl->isNoReturn())
        result->getAttrs().add(
            new (Impl.SwiftContext) NoReturnAttr(/*IsImplicit=*/false));

      // Keep track of inline function bodies so that we can generate
      // IR from them using Clang's IR generator.
      if ((decl->isInlined() || decl->hasAttr<clang::AlwaysInlineAttr>() ||
           !decl->isExternallyVisible())
          && decl->hasBody()) {
        Impl.registerExternalDecl(result);
      }

      // Set availability.
      auto knownFnInfo = Impl.getKnownGlobalFunction(decl);
      if (knownFnInfo && knownFnInfo->Unavailable) {
        Impl.markUnavailable(result, knownFnInfo->UnavailableMsg);
      }

      if (decl->isVariadic()) {
        Impl.markUnavailable(result, "Variadic function is unavailable");
      }

      return result;
    }

    Decl *VisitCXXMethodDecl(const clang::CXXMethodDecl *decl) {
      // FIXME: Import C++ member functions as methods.
      return nullptr;
    }

    Decl *VisitFieldDecl(const clang::FieldDecl *decl) {
      // Fields are imported as variables.
      auto name = Impl.importFullName(decl).Imported.getBaseName();
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      auto type = Impl.importType(decl->getType(),
                                  ImportTypeKind::RecordField,
                                  isInSystemModule(dc),
                                  /*isFullyBridgeable*/false);
      if (!type)
        return nullptr;

      auto result =
        Impl.createDeclWithClangNode<VarDecl>(decl,
                              /*static*/ false, /*IsLet*/ false,
                              Impl.importSourceLoc(decl->getLocation()),
                              name, type, dc);

      // Handle attributes.
      if (decl->hasAttr<clang::IBOutletAttr>())
        result->getAttrs().add(
            new (Impl.SwiftContext) IBOutletAttr(/*IsImplicit=*/false));
      // FIXME: Handle IBOutletCollection.

      return result;
    }

    Decl *VisitObjCIvarDecl(const clang::ObjCIvarDecl *decl) {
      // Disallow direct ivar access (and avoid conflicts with property names).
      return nullptr;
    }

    Decl *VisitObjCAtDefsFieldDecl(const clang::ObjCAtDefsFieldDecl *decl) {
      // @defs is an anachronism; ignore it.
      return nullptr;
    }

    Decl *VisitVarDecl(const clang::VarDecl *decl) {
      // FIXME: Swift does not have static variables in structs/classes yet.
      if (decl->getDeclContext()->isRecord())
        return nullptr;

      // Variables are imported as... variables.
      auto name = Impl.importFullName(decl).Imported.getBaseName();
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      auto knownVarInfo = Impl.getKnownGlobalVariable(decl);

      // Lookup nullability info.
      OptionalTypeKind optionality = OTK_ImplicitlyUnwrappedOptional;
      if (knownVarInfo) {
        if (auto nullability = knownVarInfo->getNullability())
          optionality = Impl.translateNullability(*nullability);
      }

      // If the declaration is const, consider it audited.
      // We can assume that loading a const global variable doesn't
      // involve an ownership transfer.
      bool isAudited = decl->getType().isConstQualified();

      Type type = Impl.importType(decl->getType(),
                                  (isAudited ? ImportTypeKind::AuditedVariable
                                   : ImportTypeKind::Variable),
                                  isInSystemModule(dc),
                                  /*isFullyBridgeable*/false);

      if (!type)
        return nullptr;

      auto result = Impl.createDeclWithClangNode<VarDecl>(decl,
                       /*static*/ false,
                       Impl.shouldImportGlobalAsLet(decl->getType()),
                       Impl.importSourceLoc(decl->getLocation()),
                       name, type, dc);

      // Check availability.
      if (knownVarInfo && knownVarInfo->Unavailable) {
        Impl.markUnavailable(result, knownVarInfo->UnavailableMsg);
      }

      if (!decl->hasExternalStorage())
        Impl.registerExternalDecl(result);

      return result;
    }

    Decl *VisitImplicitParamDecl(const clang::ImplicitParamDecl *decl) {
      // Parameters are never directly imported.
      return nullptr;
    }

    Decl *VisitParmVarDecl(const clang::ParmVarDecl *decl) {
      // Parameters are never directly imported.
      return nullptr;
    }

    Decl *
    VisitNonTypeTemplateParmDecl(const clang::NonTypeTemplateParmDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitTemplateDecl(const clang::TemplateDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitUsingDecl(const clang::UsingDecl *decl) {
      // Using declarations are not imported.
      return nullptr;
    }

    Decl *VisitUsingShadowDecl(const clang::UsingShadowDecl *decl) {
      // Using shadow declarations are not imported; rather, name lookup just
      // looks through them.
      return nullptr;
    }

    /// Add an @objc(name) attribute with the given, optional name expressed as
    /// selector.
    ///
    /// The importer should use this rather than adding the attribute directly.
    void addObjCAttribute(ValueDecl *decl, Optional<ObjCSelector> name) {
      auto &ctx = Impl.SwiftContext;
      decl->getAttrs().add(ObjCAttr::create(ctx, name, /*implicit=*/true));

      // If the declaration we attached the 'objc' attribute to is within a
      // class, record it in the class.
      if (auto contextTy = decl->getDeclContext()->getDeclaredInterfaceType()) {
        if (auto classDecl = contextTy->getClassOrBoundGenericClass()) {
          if (auto method = dyn_cast<AbstractFunctionDecl>(decl)) {
            classDecl->recordObjCMethod(method);
          }
        }
      }
    }

    /// Add an @objc(name) attribute with the given, optional name expressed as
    /// selector.
    ///
    /// The importer should use this rather than adding the attribute directly.
    void addObjCAttribute(ValueDecl *decl, Identifier name) {
      addObjCAttribute(decl, ObjCSelector(Impl.SwiftContext, 0, name));
    }

    Decl *VisitObjCMethodDecl(const clang::ObjCMethodDecl *decl) {
      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      // While importing the DeclContext, we might have imported the decl
      // itself.
      if (auto Known = Impl.importDeclCached(decl))
        return Known;

      return VisitObjCMethodDecl(decl, dc);
    }

    /// Check whether we have already imported a method with the given
    /// selector in the given context.
    bool methodAlreadyImported(ObjCSelector selector, bool isInstance,
                               DeclContext *dc) {
      // We only need to perform this check for classes.
      auto classDecl
        = dc->getDeclaredInterfaceType()->getClassOrBoundGenericClass();
      if (!classDecl)
        return false;

      // Make sure we don't search in Clang modules for this method.
      ++Impl.ActiveSelectors[{selector, isInstance}];

      // Look for a matching imported or deserialized member.
      bool result = false;
      for (auto decl : classDecl->lookupDirect(selector, isInstance)) {
        if (decl->getClangDecl()
            || !decl->getDeclContext()->getParentSourceFile()) {
          result = true;
          break;
        }
      }

      // Restore the previous active count in the active-selector mapping.
      auto activeCount = Impl.ActiveSelectors.find({selector, isInstance});
      --activeCount->second;
      if (activeCount->second == 0)
        Impl.ActiveSelectors.erase(activeCount);

      return result;
    }

    /// If the given method is a factory method, import it as a constructor
    Optional<ConstructorDecl *>
    importFactoryMethodAsConstructor(Decl *member,
                                     const clang::ObjCMethodDecl *decl,
                                     ObjCSelector selector,
                                     DeclContext *dc) {
      // Import the full name of the method.
      auto importedName = Impl.importFullName(decl);

      // Check that we imported an initializer name.
      DeclName initName = importedName;
      if (initName.getBaseName() != Impl.SwiftContext.Id_init) return None;

      // ... that came from a factory method.
      if (importedName.InitKind != CtorInitializerKind::Factory &&
          importedName.InitKind != CtorInitializerKind::ConvenienceFactory)
        return None;

      bool redundant = false;
      auto result = importConstructor(decl, dc, false, importedName.InitKind,
                                      /*required=*/false, selector,
                                      importedName,
                                      {decl->param_begin(), decl->param_size()},
                                      decl->isVariadic(), redundant);
      if (result)
        Impl.importAttributes(decl, result);

      if ((result || redundant) && member) {
        ++NumFactoryMethodsAsInitializers;

        // Mark the imported class method "unavailable", with a useful error
        // message.
        // TODO: Could add a replacement string?
        llvm::SmallString<64> message;
        llvm::raw_svector_ostream os(message);
        os << "use object construction '"
           << decl->getClassInterface()->getName() << "(";
        for (auto arg : initName.getArgumentNames()) {
          os << arg << ":";
        }
        os << ")'";
        member->getAttrs().add(
          AvailableAttr::createUnconditional(
            Impl.SwiftContext, 
            Impl.SwiftContext.AllocateCopy(os.str())));
      }

      return result;
    }

    Decl *VisitObjCMethodDecl(const clang::ObjCMethodDecl *decl,
                              DeclContext *dc,
                              bool forceClassMethod = false) {
      // If we have an init method, import it as an initializer.
      if (Impl.isInitMethod(decl)) {
        // Cannot force initializers into class methods.
        if (forceClassMethod)
          return nullptr;

        return importConstructor(decl, dc, /*isImplicit=*/false, None,
                                 /*required=*/false);
      }

      // Check whether we already imported this method.
      if (!forceClassMethod && dc == Impl.importDeclContextOf(decl)) {
        // FIXME: Should also be able to do this for forced class
        // methods.
        auto known = Impl.ImportedDecls.find(decl->getCanonicalDecl());
        if (known != Impl.ImportedDecls.end())
          return known->second;
      }

      // Check whether another method with the same selector has already been
      // imported into this context.
      ObjCSelector selector = Impl.importSelector(decl->getSelector());
      bool isInstance = decl->isInstanceMethod() && !forceClassMethod;
      if (methodAlreadyImported(selector, isInstance, dc))
        return nullptr;

      auto importedName
        = Impl.importFullName(decl,
                              ClangImporter::Implementation::ImportNameFlags
                                ::SuppressFactoryMethodAsInit);
      if (!importedName)
        return nullptr;

      assert(dc->getDeclaredTypeOfContext() && "Method in non-type context?");
      assert(isa<ClangModuleUnit>(dc->getModuleScopeContext()) &&
             "Clang method in Swift context?");

      // FIXME: We should support returning "Self.Type" for a root class
      // instance method mirrored as a class method, but it currently causes
      // problems for the type checker.
      if (forceClassMethod && decl->hasRelatedResultType())
        return nullptr;

      // Add the implicit 'self' parameter patterns.
      SmallVector<Pattern *, 4> bodyPatterns;
      auto selfVar =
        createSelfDecl(dc, decl->isClassMethod() || forceClassMethod);
      Pattern *selfPat = createTypedNamedPattern(selfVar);
      bodyPatterns.push_back(selfPat);

      SpecialMethodKind kind = SpecialMethodKind::Regular;
      // FIXME: This doesn't handle implicit properties.
      if (decl->isPropertyAccessor())
        kind = SpecialMethodKind::PropertyAccessor;
      else if (isNSDictionaryMethod(decl, Impl.objectForKeyedSubscript))
        kind = SpecialMethodKind::NSDictionarySubscriptGetter;

      // Import the type that this method will have.
      DeclName name = importedName.Imported;
      Optional<ForeignErrorConvention> errorConvention;
      auto type = Impl.importMethodType(decl,
                                        decl->getReturnType(),
                                        { decl->param_begin(),
                                          decl->param_size() },
                                        decl->isVariadic(),
                                        decl->hasAttr<clang::NoReturnAttr>(),
                                        isInSystemModule(dc),
                                        bodyPatterns,
                                        importedName,
                                        name,
                                        errorConvention,
                                        kind);
      if (!type)
        return nullptr;

      // Check whether we recursively imported this method
      if (!forceClassMethod && dc == Impl.importDeclContextOf(decl)) {
        // FIXME: Should also be able to do this for forced class
        // methods.
        auto known = Impl.ImportedDecls.find(decl->getCanonicalDecl());
        if (known != Impl.ImportedDecls.end())
          return known->second;
      }

      auto result = FuncDecl::create(
          Impl.SwiftContext, SourceLoc(), StaticSpellingKind::None,
          SourceLoc(), name, SourceLoc(), SourceLoc(), SourceLoc(),
          /*GenericParams=*/nullptr, Type(),
          bodyPatterns, TypeLoc(), dc, decl);

      result->setAccessibility(Accessibility::Public);

      auto resultTy = type->castTo<FunctionType>()->getResult();
      Type interfaceType;

      // If the method has a related result type that is representable
      // in Swift as DynamicSelf, do so.
      if (decl->hasRelatedResultType()) {
        result->setDynamicSelf(true);
        resultTy = result->getDynamicSelf();
        assert(resultTy && "failed to get dynamic self");

        Type interfaceSelfTy = result->getDynamicSelfInterface();
        OptionalTypeKind nullability = OTK_ImplicitlyUnwrappedOptional;
        if (auto typeNullability = decl->getReturnType()->getNullability(
                                     Impl.getClangASTContext())) {
          // If the return type has nullability, use it.
          nullability = Impl.translateNullability(*typeNullability);
        } else if (auto known = Impl.getKnownObjCMethod(decl)) {
          // If the method is known to have nullability information for
          // its return type, use that.
          if (known->NullabilityAudited) {
            nullability = Impl.translateNullability(known->getReturnTypeInfo());
          }
        }
        if (nullability != OTK_None && !errorConvention.hasValue()) {
          resultTy = OptionalType::get(nullability, resultTy);
          interfaceSelfTy = OptionalType::get(nullability, interfaceSelfTy);
        }

        // Update the method type with the new result type.
        auto methodTy = type->castTo<FunctionType>();
        type = FunctionType::get(methodTy->getInput(), resultTy, 
                                 methodTy->getExtInfo());

        // Create the interface type of the method.
        interfaceType = FunctionType::get(methodTy->getInput(), interfaceSelfTy,
                                          methodTy->getExtInfo());
        interfaceType = FunctionType::get(selfVar->getType(), interfaceType);
      }

      // Add the 'self' parameter to the function type.
      type = FunctionType::get(selfVar->getType(), type);

      if (auto proto = dyn_cast<ProtocolDecl>(dc)) {
        std::tie(type, interfaceType)
          = getProtocolMethodType(proto, type->castTo<AnyFunctionType>());
      }

      result->setBodyResultType(resultTy);
      result->setType(type);
      result->setInterfaceType(interfaceType);

      // Optional methods in protocols.
      if (decl->getImplementationControl() == clang::ObjCMethodDecl::Optional &&
          isa<ProtocolDecl>(dc))
        result->getAttrs().add(new (Impl.SwiftContext)
                                      OptionalAttr(/*implicit*/false));

      // Mark class methods as static.
      if (decl->isClassMethod() || forceClassMethod)
        result->setStatic();
      if (forceClassMethod)
        result->setImplicit();

      // Mark this method @objc.
      addObjCAttribute(result, selector);

      // If this method overrides another method, mark it as such.
      recordObjCOverride(result);

      // Record the error convention.
      if (errorConvention) {
        result->setForeignErrorConvention(*errorConvention);
      }

      // Handle attributes.
      if (decl->hasAttr<clang::IBActionAttr>())
        result->getAttrs().add(
            new (Impl.SwiftContext) IBActionAttr(/*IsImplicit=*/false));

      // Check whether there's some special method to import.
      if (!forceClassMethod) {
        if (dc == Impl.importDeclContextOf(decl) &&
            !Impl.ImportedDecls[decl->getCanonicalDecl()])
          Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

        importSpecialMethod(result, dc);
      }
      return result;
    }

  public:
    /// \brief Given an imported method, try to import it as some kind of
    /// special declaration, e.g., a constructor or subscript.
    Decl *importSpecialMethod(Decl *decl, DeclContext *dc) {
      // Check whether there's a method associated with this declaration.
      auto objcMethod
        = dyn_cast_or_null<clang::ObjCMethodDecl>(decl->getClangDecl());
      if (!objcMethod)
        return nullptr;

      // Only consider Objective-C methods...
      switch (objcMethod->getMethodFamily()) {
      case clang::OMF_None:
        // Check for one of the subscripting selectors.
        if (objcMethod->isInstanceMethod() &&
            (objcMethod->getSelector() == Impl.objectAtIndexedSubscript ||
             objcMethod->getSelector() == Impl.setObjectAtIndexedSubscript ||
             objcMethod->getSelector() == Impl.objectForKeyedSubscript ||
             objcMethod->getSelector() == Impl.setObjectForKeyedSubscript))
          return importSubscript(decl, objcMethod, dc);
          
        return nullptr;

      case clang::OMF_init:
      case clang::OMF_initialize:
      case clang::OMF_new:
      case clang::OMF_alloc:
      case clang::OMF_autorelease:
      case clang::OMF_copy:
      case clang::OMF_dealloc:
      case clang::OMF_finalize:
      case clang::OMF_mutableCopy:
      case clang::OMF_performSelector:
      case clang::OMF_release:
      case clang::OMF_retain:
      case clang::OMF_retainCount:
      case clang::OMF_self:
        // None of these methods have special consideration.
        return nullptr;
      }
    }

  private:
    /// Record the function or initializer overridden by the given Swift method.
    void recordObjCOverride(AbstractFunctionDecl *decl) {
      // Figure out the class in which this method occurs.
      auto classTy = decl->getExtensionType()->getAs<ClassType>();
      if (!classTy)
        return;

      auto superTy = classTy->getSuperclass(nullptr);
      if (!superTy)
        return;

      // Dig out the Objective-C superclass.
      auto superDecl = superTy->getAnyNominal();
      SmallVector<ValueDecl *, 4> results;
      superDecl->lookupQualified(superTy, decl->getFullName(),
                                 NL_QualifiedDefault | NL_KnownNoDependency,
                                 Impl.getTypeResolver(),
                                 results);

      for (auto member : results) {
        if (member->getKind() != decl->getKind() ||
            member->isInstanceMember() != decl->isInstanceMember())
          continue;

        // Set function override.
        // FIXME: Proper type checking here!
        if (auto func = dyn_cast<FuncDecl>(decl)) {
          func->setOverriddenDecl(cast<FuncDecl>(member));
          return;
        }

        // Set constructor override.
        auto ctor = cast<ConstructorDecl>(decl);
        auto memberCtor = cast<ConstructorDecl>(member);
        ctor->setOverriddenDecl(memberCtor);

        // Propagate 'required' to subclass initializers.
        if (memberCtor->isRequired() &&
            !ctor->getAttrs().hasAttribute<RequiredAttr>()) {
          ctor->getAttrs().add(
            new (Impl.SwiftContext) RequiredAttr(/*implicit=*/true));
        }
      }
    }
    
    /// \brief Given an imported method, try to import it as a constructor.
    ///
    /// Objective-C methods in the 'init' family are imported as
    /// constructors in Swift, enabling object construction syntax, e.g.,
    ///
    /// \code
    /// // in objc: [[NSArray alloc] initWithCapacity:1024]
    /// NSArray(capacity: 1024)
    /// \endcode
    ConstructorDecl *importConstructor(const clang::ObjCMethodDecl *objcMethod,
                                       DeclContext *dc,
                                       bool implicit,
                                       Optional<CtorInitializerKind> kind,
                                       bool required) {
      // Only methods in the 'init' family can become constructors.
      assert(Impl.isInitMethod(objcMethod) && "Not a real init method");

      // Check whether we've already created the constructor.
      auto known = Impl.Constructors.find({objcMethod, dc});
      if (known != Impl.Constructors.end())
        return known->second;
      
      // Check whether there is already a method with this selector.
      auto selector = Impl.importSelector(objcMethod->getSelector());
      if (methodAlreadyImported(selector, /*isInstance=*/true, dc))
        return nullptr;

      // Map the name and complete the import.
      ArrayRef<const clang::ParmVarDecl *> params{
        objcMethod->param_begin(),
        objcMethod->param_end()
      };

      bool variadic = objcMethod->isVariadic();
      auto importedName = Impl.importFullName(objcMethod);
      if (!importedName) return nullptr;

      // If we dropped the variadic, handle it now.
      if (importedName.DroppedVariadic) {
        selector = ObjCSelector(Impl.SwiftContext, selector.getNumArgs()-1,
                                selector.getSelectorPieces().drop_back());
        params = params.drop_back(1);
        variadic = false;
      }

      bool redundant;
      return importConstructor(objcMethod, dc, implicit, kind, required,
                               selector, importedName, params,
                               variadic, redundant);
    }

    /// Returns the latest "introduced" version on the current platform for
    /// \p D.
    clang::VersionTuple findLatestIntroduction(const clang::Decl *D) {
      clang::VersionTuple result;

      for (auto *attr : D->specific_attrs<clang::AvailabilityAttr>()) {
        if (attr->getPlatform()->getName() == "swift") {
          clang::VersionTuple maxVersion{~0U, ~0U, ~0U};
          return maxVersion;
        }

        // Does this availability attribute map to the platform we are
        // currently targeting?
        if (!Impl.PlatformAvailabilityFilter ||
            !Impl.PlatformAvailabilityFilter(attr->getPlatform()->getName()))
          continue;

        // Take advantage of the empty version being 0.0.0.0.
        result = std::max(result, attr->getIntroduced());
      }

      return result;
    }

    /// Returns true if importing \p objcMethod will produce a "better"
    /// initializer than \p existingCtor.
    bool
    existingConstructorIsWorse(const ConstructorDecl *existingCtor,
                               const clang::ObjCMethodDecl *objcMethod,
                               CtorInitializerKind kind) {
      CtorInitializerKind existingKind = existingCtor->getInitKind();

      // If the new kind is the same as the existing kind, stick with
      // the existing constructor.
      if (existingKind == kind)
        return false;

      // Check for cases that are obviously better or obviously worse.
      if (kind == CtorInitializerKind::Designated ||
          existingKind == CtorInitializerKind::Factory)
        return true;

      if (kind == CtorInitializerKind::Factory ||
          existingKind == CtorInitializerKind::Designated)
        return false;

      assert(kind == CtorInitializerKind::Convenience ||
             kind == CtorInitializerKind::ConvenienceFactory);
      assert(existingKind == CtorInitializerKind::Convenience ||
             existingKind == CtorInitializerKind::ConvenienceFactory);

      // Between different kinds of convenience initializers, keep the one that
      // was introduced first.
      // FIXME: But if one of them is now deprecated, should we prefer the
      // other?
      clang::VersionTuple introduced = findLatestIntroduction(objcMethod);
      VersionRange existingIntroduced =
          AvailabilityInference::availableRange(existingCtor,
                                                Impl.SwiftContext);
      assert(!existingIntroduced.isEmpty());

      if (existingIntroduced.isAll()) {
        if (!introduced.empty())
          return false;
      } else if (introduced != existingIntroduced.getLowerEndpoint()) {
        return introduced < existingIntroduced.getLowerEndpoint();
      }

      // The "introduced" versions are the same. Prefer Convenience over
      // ConvenienceFactory, but otherwise prefer leaving things as they are.
      if (kind == CtorInitializerKind::Convenience &&
          existingKind == CtorInitializerKind::ConvenienceFactory)
        return true;

      return false;
    }

    using ImportedName = ClangImporter::Implementation::ImportedName;

    /// \brief Given an imported method, try to import it as a constructor.
    ///
    /// Objective-C methods in the 'init' family are imported as
    /// constructors in Swift, enabling object construction syntax, e.g.,
    ///
    /// \code
    /// // in objc: [[NSArray alloc] initWithCapacity:1024]
    /// NSArray(capacity: 1024)
    /// \endcode
    ///
    /// This variant of the function is responsible for actually binding the
    /// constructor declaration appropriately.
    ConstructorDecl *importConstructor(const clang::ObjCMethodDecl *objcMethod,
                                       DeclContext *dc,
                                       bool implicit,
                                       Optional<CtorInitializerKind> kindIn,
                                       bool required,
                                       ObjCSelector selector,
                                       ImportedName importedName,
                                       ArrayRef<const clang::ParmVarDecl*> args,
                                       bool variadic,
                                       bool &redundant) {
      redundant = false;

      // Figure out the type of the container.
      auto containerTy = dc->getDeclaredTypeOfContext();
      assert(containerTy && "Method in non-type context?");
      auto nominalOwner = containerTy->getAnyNominal();

      // Find the interface, if we can.
      const clang::ObjCInterfaceDecl *interface = nullptr;
      if (auto classDecl = containerTy->getClassOrBoundGenericClass()) {
        interface = dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                      classDecl->getClangDecl());
      }

      // If we weren't told what kind of initializer this should be,
      // figure it out now.
      CtorInitializerKind kind;

      if (kindIn) {
        kind = *kindIn;

        // If we know this is a designated initializer, mark it as such.
        if (interface && Impl.hasDesignatedInitializers(interface) &&
            Impl.isDesignatedInitializer(interface, objcMethod))
          kind = CtorInitializerKind::Designated;
      } else {
        // If the owning Objective-C class has designated initializers and this
        // is not one of them, treat it as a convenience initializer.
        if (interface && Impl.hasDesignatedInitializers(interface) &&
            !Impl.isDesignatedInitializer(interface, objcMethod)) {
          kind = CtorInitializerKind::Convenience;
        } else {
          kind = CtorInitializerKind::Designated;
        }
      }

      // Add the implicit 'self' parameter patterns.
      SmallVector<Pattern *, 4> bodyPatterns;
      auto selfTy = getSelfTypeForContext(dc);
      auto selfMetaVar = createSelfDecl(dc, true);
      Pattern *selfPat = createTypedNamedPattern(selfMetaVar);
      bodyPatterns.push_back(selfPat);

      // Import the type that this method will have.
      Optional<ForeignErrorConvention> errorConvention;
      DeclName name = importedName.Imported;
      auto type = Impl.importMethodType(objcMethod,
                                        objcMethod->getReturnType(),
                                        args,
                                        variadic,
                                        objcMethod->hasAttr<clang::NoReturnAttr>(),
                                        isInSystemModule(dc),
                                        bodyPatterns,
                                        importedName,
                                        name,
                                        errorConvention,
                                        SpecialMethodKind::Constructor);
      if (!type)
        return nullptr;

      // Determine the failability of this initializer.
      auto oldFnType = type->castTo<AnyFunctionType>();
      OptionalTypeKind failability;
      (void)oldFnType->getResult()->getAnyOptionalObjectType(failability);

      // Rebuild the function type with the appropriate result type;
      Type resultTy = selfTy;
      if (failability)
        resultTy = OptionalType::get(failability, resultTy);

      type = FunctionType::get(oldFnType->getInput(), resultTy,
                               oldFnType->getExtInfo());

      // Add the 'self' parameter to the function types.
      Type allocType = FunctionType::get(selfMetaVar->getType(), type);
      Type initType = FunctionType::get(selfTy, type);

      // Look for other imported constructors that occur in this context with
      // the same name.
      Type allocParamType = allocType->castTo<AnyFunctionType>()->getResult()
                              ->castTo<AnyFunctionType>()->getInput();
      for (auto other : nominalOwner->lookupDirect(name)) {
        auto ctor = dyn_cast<ConstructorDecl>(other);
        if (!ctor || ctor->isInvalid() ||
            ctor->getAttrs().isUnavailable(Impl.SwiftContext) ||
            !ctor->getClangDecl())
          continue;

        // Resolve the type of the constructor.
        if (!ctor->hasType())
          Impl.getTypeResolver()->resolveDeclSignature(ctor);

        // If the types don't match, this is a different constructor with
        // the same selector. This can happen when an overlay overloads an
        // existing selector with a Swift-only signature.
        Type ctorParamType = ctor->getType()->castTo<AnyFunctionType>()
                               ->getResult()->castTo<AnyFunctionType>()
                               ->getInput();
        if (!ctorParamType->isEqual(allocParamType)) {
          continue;
        }

        // If the existing constructor has a less-desirable kind, mark
        // the existing constructor unavailable.
        if (existingConstructorIsWorse(ctor, objcMethod, kind)) {
          // Show exactly where this constructor came from.
          llvm::SmallString<32> errorStr;
          errorStr += "superseded by import of ";
          if (objcMethod->isClassMethod())
            errorStr += "+[";
          else
            errorStr += "-[";

          auto objcDC = objcMethod->getDeclContext();
          if (auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(objcDC)) {
            errorStr += objcClass->getName();
            errorStr += ' ';
          } else if (auto objcCat = dyn_cast<clang::ObjCCategoryDecl>(objcDC)) {
            errorStr += objcCat->getClassInterface()->getName();
            auto catName = objcCat->getName();
            if (!catName.empty()) {
              errorStr += '(';
              errorStr += catName;
              errorStr += ')';
            }
            errorStr += ' ';
          } else if (auto objcProto=dyn_cast<clang::ObjCProtocolDecl>(objcDC)) {
            errorStr += objcProto->getName();
            errorStr += ' ';
          }

          errorStr += objcMethod->getSelector().getAsString();
          errorStr += ']';

          auto attr
            = AvailableAttr::createUnconditional(
                Impl.SwiftContext,
                Impl.SwiftContext.AllocateCopy(errorStr.str()));
          ctor->getAttrs().add(attr);
          continue;
        }

        // Otherwise, we shouldn't create a new constructor, because
        // it will be no better than the existing one.
        redundant = true;
        return nullptr;
      }

      // Check whether we've already created the constructor.
      auto known = Impl.Constructors.find({objcMethod, dc});
      if (known != Impl.Constructors.end())
        return known->second;

      VarDecl *selfVar = createSelfDecl(dc, false);
      selfPat = createTypedNamedPattern(selfVar);

      // Create the actual constructor.
      auto result = Impl.createDeclWithClangNode<ConstructorDecl>(objcMethod,
                      name, SourceLoc(), failability, SourceLoc(), selfPat, 
                      bodyPatterns.back(), /*GenericParams=*/nullptr,
                      SourceLoc(), dc);

      // Make the constructor declaration immediately visible in its
      // class or protocol type.
      nominalOwner->makeMemberVisible(result);

      addObjCAttribute(result, selector);

      // Fix the types when we've imported into a protocol.
      if (auto proto = dyn_cast<ProtocolDecl>(dc)) {
        Type interfaceAllocType;
        Type interfaceInitType;
        std::tie(allocType, interfaceAllocType)
          = getProtocolMethodType(proto, allocType->castTo<AnyFunctionType>());
        std::tie(initType, interfaceInitType)
          = getProtocolMethodType(proto, initType->castTo<AnyFunctionType>());

        result->setInitializerInterfaceType(interfaceInitType);
        result->setInterfaceType(interfaceAllocType);
      }

      result->setType(allocType);
      result->setInitializerType(initType);

      if (implicit)
        result->setImplicit();

      // Set the kind of initializer.
      result->setInitKind(kind);

      // Consult API notes to determine whether this initializer is required.
      if (!required && Impl.isRequiredInitializer(objcMethod))
        required = true;

      // Check whether this initializer satisfies a requirement in a protocol.
      if (!required && !isa<ProtocolDecl>(dc) &&
          objcMethod->isInstanceMethod()) {
        auto objcParent = cast<clang::ObjCContainerDecl>(
                            objcMethod->getDeclContext());

        if (isa<clang::ObjCProtocolDecl>(objcParent)) {
          // An initializer declared in a protocol is required.
          required = true;
        } else {
          // If the class in which this initializer was declared conforms to a
          // protocol that requires this initializer, then this initializer is
          // required.
          SmallPtrSet<clang::ObjCProtocolDecl *, 8> objcProtocols;
          objcParent->getASTContext().CollectInheritedProtocols(objcParent,
                                                                objcProtocols);
          for (auto objcProto : objcProtocols) {
            for (auto decl : objcProto->lookup(objcMethod->getSelector())) {
              if (cast<clang::ObjCMethodDecl>(decl)->isInstanceMethod()) {
                required = true;
                break;
              }
            }

            if (required)
              break;
          }
        }
      }

      // If this initializer is required, add the appropriate attribute.
      if (required) {
        result->getAttrs().add(
          new (Impl.SwiftContext) RequiredAttr(/*implicit=*/true));
      }

      // Record the error convention.
      if (errorConvention) {
        result->setForeignErrorConvention(*errorConvention);
      }

      // Record the constructor for future re-use.
      Impl.Constructors[{objcMethod, dc}] = result;

      // If this constructor overrides another constructor, mark it as such.
      recordObjCOverride(result);

      // Inform the context that we have external definitions.
      Impl.registerExternalDecl(result);

      return result;
    }

    /// \brief Retrieve the single variable described in the given pattern.
    ///
    /// This routine assumes that the pattern is something very simple
    /// like (x : type) or (x).
    VarDecl *getSingleVar(Pattern *pattern) {
      pattern = pattern->getSemanticsProvidingPattern();
      if (auto tuple = dyn_cast<TuplePattern>(pattern)) {
        pattern = tuple->getElement(0).getPattern()
                    ->getSemanticsProvidingPattern();
      }

      return cast<NamedPattern>(pattern)->getDecl();
    }

    /// Retrieves the type and interface type for a protocol or
    /// protocol extension method given the computed type of that
    /// method.
    std::pair<Type, Type> getProtocolMethodType(DeclContext *dc,
                                                AnyFunctionType *fnType) {
      Type type = PolymorphicFunctionType::get(fnType->getInput(),
                                               fnType->getResult(),
                                               dc->getGenericParamsOfContext());

      // Figure out the curried 'self' type for the interface type. It's always
      // either the generic parameter type 'Self' or a metatype thereof.
      auto selfDecl = dc->getProtocolSelf();
      auto selfTy = selfDecl->getDeclaredType();
      auto interfaceInputTy = selfTy;
      auto inputTy = fnType->getInput();
      if (auto tupleTy = inputTy->getAs<TupleType>()) {
        if (tupleTy->getNumElements() == 1)
          inputTy = tupleTy->getElementType(0);
      }
      if (inputTy->is<MetatypeType>())
        interfaceInputTy = MetatypeType::get(interfaceInputTy);

      auto selfArchetype = selfDecl->getArchetype();
      auto interfaceResultTy = fnType->getResult().transform(
        [&](Type type) -> Type {
          if (type->is<DynamicSelfType>() || type->isEqual(selfArchetype)) {
            return DynamicSelfType::get(selfTy, Impl.SwiftContext);
          }

          return type;
        });

      Type interfaceType = GenericFunctionType::get(
                             dc->getGenericSignatureOfContext(),
                             interfaceInputTy,
                             interfaceResultTy,
                             AnyFunctionType::ExtInfo());
      return { type, interfaceType };
    }

    /// Build a declaration for an Objective-C subscript getter.
    FuncDecl *buildSubscriptGetterDecl(const FuncDecl *getter, Type elementTy,
                               DeclContext *dc, Pattern *indices) {
      auto &context = Impl.SwiftContext;
      auto loc = getter->getLoc();

      // Form the argument patterns.
      SmallVector<Pattern *, 3> getterArgs;

      // 'self'
      getterArgs.push_back(createTypedNamedPattern(createSelfDecl(dc, false)));

      // index, for subscript operations.
      assert(indices);
      indices = indices->clone(context);
      auto pat = TuplePattern::create(context, loc, TuplePatternElt(indices),
                                      loc);
      pat->setType(TupleType::get(TupleTypeElt(indices->getType()),
                                  context));
      getterArgs.push_back(pat);

      // Form the type of the getter.
      auto getterType = elementTy;
      for (auto it = getterArgs.rbegin(), itEnd = getterArgs.rend();
           it != itEnd; ++it) {
        getterType = FunctionType::get(
                       (*it)->getType()->getUnlabeledType(context),
                       getterType);
      }

      // If we're in a protocol, the getter thunk will be polymorphic.
      Type interfaceType;
      if (dc->isProtocolOrProtocolExtensionContext()) {
        std::tie(getterType, interfaceType)
          = getProtocolMethodType(dc, getterType->castTo<AnyFunctionType>());
      }

      // Create the getter thunk.
      FuncDecl *thunk = FuncDecl::create(
          context, SourceLoc(), StaticSpellingKind::None, loc,
          Identifier(), SourceLoc(), SourceLoc(), SourceLoc(), nullptr, getterType,
          getterArgs, TypeLoc::withoutLoc(elementTy), dc,
          getter->getClangNode());
      thunk->setBodyResultType(elementTy);
      thunk->setInterfaceType(interfaceType);
      thunk->setAccessibility(Accessibility::Public);

      auto objcAttr = getter->getAttrs().getAttribute<ObjCAttr>();
      assert(objcAttr);
      thunk->getAttrs().add(objcAttr->clone(context));
      // FIXME: Should we record thunks?

      return thunk;
    }

      /// Build a declaration for an Objective-C subscript setter.
    FuncDecl *buildSubscriptSetterDecl(const FuncDecl *setter, Type elementTy,
                               DeclContext *dc, Pattern *indices) {
      auto &context = Impl.SwiftContext;
      auto loc = setter->getLoc();
      auto tuple = cast<TuplePattern>(setter->getBodyParamPatterns()[1]);

      // Objective-C subscript setters are imported with a function type
      // such as:
      //
      //   (self) -> (value, index) -> ()
      //
      // Build a setter thunk with the latter signature that maps to the
      // former.

      // Form the argument patterns.
      SmallVector<Pattern *, 2> setterArgs;

      // 'self'
      setterArgs.push_back(createTypedNamedPattern(createSelfDecl(dc, false)));

      
      SmallVector<TuplePatternElt, 2> ValueElts;
      SmallVector<TupleTypeElt, 2> ValueEltTys;

      auto paramVarDecl = new (context) ParamDecl(
          /*isLet=*/false, SourceLoc(), Identifier(), loc,
          tuple->getElement(0).getPattern()->getSingleVar()->getName(),
          elementTy, dc);
      auto valuePattern = createTypedNamedPattern(paramVarDecl);
      ValueElts.push_back(TuplePatternElt(valuePattern));
      ValueEltTys.push_back(TupleTypeElt(valuePattern->getType()));
      
      // Clone the indices for the thunk.
      assert(indices);
      indices = indices->clone(context);
      ValueElts.push_back(TuplePatternElt(indices));
      ValueEltTys.push_back(TupleTypeElt(indices->getType()));

      // value
      setterArgs.push_back(TuplePattern::create(context, loc, ValueElts, loc));
      setterArgs.back()->setType(TupleType::get(ValueEltTys, context));

      // Form the type of the setter.
      Type setterType = TupleType::getEmpty(context);
      for (auto it = setterArgs.rbegin(), itEnd = setterArgs.rend();
           it != itEnd; ++it) {
        setterType = FunctionType::get(
                       (*it)->getType()->getUnlabeledType(context),
                       setterType);
      }

      // If we're in a protocol or extension thereof, the setter thunk
      // will be polymorphic.
      Type interfaceType;
      if (dc->isProtocolOrProtocolExtensionContext()) {
        std::tie(setterType, interfaceType)
          = getProtocolMethodType(dc, setterType->castTo<AnyFunctionType>());
      }

      // Create the setter thunk.
      FuncDecl *thunk = FuncDecl::create(
          context, SourceLoc(), StaticSpellingKind::None, setter->getLoc(),
          Identifier(), SourceLoc(), SourceLoc(), SourceLoc(), nullptr, setterType,
          setterArgs, TypeLoc::withoutLoc(TupleType::getEmpty(context)), dc,
          setter->getClangNode());
      thunk->setBodyResultType(TupleType::getEmpty(context));
      thunk->setInterfaceType(interfaceType);
      thunk->setAccessibility(Accessibility::Public);

      auto objcAttr = setter->getAttrs().getAttribute<ObjCAttr>();
      assert(objcAttr);
      thunk->getAttrs().add(objcAttr->clone(context));

      return thunk;
    }
    
    /// Hack: Handle the case where a subscript is read-only in the
    /// main class interface (either explicitly or because of an adopted
    /// protocol) and then the setter is added in a category/extension.
    ///
    /// \see importSubscript
    // FIXME: This is basically the same as handlePropertyRedeclaration below.
    void handleSubscriptRedeclaration(SubscriptDecl *original,
                                      const SubscriptDecl *redecl) {
      // If the subscript isn't from Clang, we can't safely update it.
      if (!original->hasClangNode())
        return;

      // If the original declaration was implicit, we may want to change that.
      if (original->isImplicit() && !redecl->isImplicit() &&
          !redecl->getDeclContext()->isProtocolOrProtocolExtensionContext())
        original->setImplicit(false);

      // The only other transformation we know how to do safely is add a
      // setter. If the subscript is already settable, we're done.
      if (original->isSettable())
        return;

      auto setter = redecl->getSetter();
      if (!setter)
        return;

      original->setComputedSetter(setter);
    }

    /// \brief Given either the getter or setter for a subscript operation,
    /// create the Swift subscript declaration.
    SubscriptDecl *importSubscript(Decl *decl,
                                   const clang::ObjCMethodDecl *objcMethod,
                                   DeclContext *dc) {
      assert(objcMethod->isInstanceMethod() && "Caller must filter");

      if (objcMethod->hasAttr<clang::SwiftPrivateAttr>())
        return nullptr;

      const clang::ObjCInterfaceDecl *interface = nullptr;
      const clang::ObjCProtocolDecl *protocol =
          dyn_cast<clang::ObjCProtocolDecl>(objcMethod->getDeclContext());
      if (!protocol)
        interface = objcMethod->getClassInterface();
      auto lookupInstanceMethod = [&](clang::Selector Sel) ->
          const clang::ObjCMethodDecl * {
        if (interface)
          return interface->lookupInstanceMethod(Sel);
        else
          return protocol->lookupInstanceMethod(Sel);
      };

      FuncDecl *getter = nullptr, *setter = nullptr;
      clang::Selector counterpartSelector;

      if (objcMethod->getSelector() == Impl.objectAtIndexedSubscript) {
        getter = cast<FuncDecl>(decl);
        counterpartSelector = Impl.setObjectAtIndexedSubscript;
      } else if (objcMethod->getSelector() == Impl.setObjectAtIndexedSubscript){
        setter = cast<FuncDecl>(decl);
        counterpartSelector = Impl.objectAtIndexedSubscript;
      } else if (objcMethod->getSelector() == Impl.objectForKeyedSubscript) {
        getter = cast<FuncDecl>(decl);
        counterpartSelector = Impl.setObjectForKeyedSubscript;
      } else if (objcMethod->getSelector() == Impl.setObjectForKeyedSubscript) {
        setter = cast<FuncDecl>(decl);
        counterpartSelector = Impl.objectForKeyedSubscript;
      } else {
        llvm_unreachable("Unknown getter/setter selector");
      }

      bool optionalMethods = (objcMethod->getImplementationControl() ==
                              clang::ObjCMethodDecl::Optional);

      auto *counterpart = lookupInstanceMethod(counterpartSelector);
      if (counterpart) {
        auto *importedCounterpart =
            cast_or_null<FuncDecl>(Impl.importDecl(counterpart));

        assert(!importedCounterpart || !importedCounterpart->isStatic());

        if (getter)
          setter = importedCounterpart;
        else
          getter = importedCounterpart;

        if (optionalMethods)
          optionalMethods = (counterpart->getImplementationControl() ==
                             clang::ObjCMethodDecl::Optional);

        if (counterpart->hasAttr<clang::SwiftPrivateAttr>())
          return nullptr;
      }

      // Swift doesn't have write-only subscripting.
      if (!getter)
        return nullptr;

      // Check whether we've already created a subscript operation for
      // this getter/setter pair.
      if (auto subscript = Impl.Subscripts[{getter, setter}])
        return subscript->getDeclContext() == dc? subscript : nullptr;

      // Compute the element type, looking through the implicit 'self'
      // parameter and the normal function parameters.
      auto elementTy
        = getter->getType()->castTo<AnyFunctionType>()->getResult()
            ->castTo<AnyFunctionType>()->getResult();

      // Check the form of the getter.
      FuncDecl *getterThunk = nullptr;
      Pattern *getterIndices = nullptr;
      auto &context = Impl.SwiftContext;

      // Find the getter indices and make sure they match.
      {
        auto tuple = dyn_cast<TuplePattern>(getter->getBodyParamPatterns()[1]);
        if (tuple && tuple->getNumElements() != 1)
          return nullptr;

        getterIndices = tuple->getElement(0).getPattern();
      }

      // Check the form of the setter.
      FuncDecl *setterThunk = nullptr;
      Pattern *setterIndices = nullptr;
      if (setter) {
        auto tuple = dyn_cast<TuplePattern>(setter->getBodyParamPatterns()[1]);
        if (!tuple)
          return nullptr;

        if (tuple->getNumElements() != 2)
          return nullptr;

        // The setter must accept elements of the same type as the getter
        // returns.
        // FIXME: Adjust C++ references?
        auto setterElementTy = tuple->getElement(0).getPattern()->getType();
        if (!elementTy->isEqual(setterElementTy)) {
          auto nonOptionalElementTy = elementTy->getAnyOptionalObjectType();
          if (nonOptionalElementTy.isNull())
            nonOptionalElementTy = elementTy;

          auto nonOptionalSetterElementTy =
              setterElementTy->getAnyOptionalObjectType();
          if (nonOptionalSetterElementTy.isNull())
            nonOptionalSetterElementTy = setterElementTy;

          if (!nonOptionalElementTy->isEqual(nonOptionalSetterElementTy))
            return nullptr;

          elementTy =
              ImplicitlyUnwrappedOptionalType::get(nonOptionalElementTy);
        }

        setterIndices = tuple->getElement(1).getPattern();

        // The setter must use the same indices as the getter.
        // FIXME: Adjust C++ references?
        if (!setterIndices->getType()->isEqual(getterIndices->getType())) {
          setter = nullptr;
          setterIndices = nullptr;

          // Check whether we've already created a subscript operation for
          // this getter.
          if (auto subscript = Impl.Subscripts[{getter, nullptr}])
            return subscript->getDeclContext() == dc? subscript : nullptr;
        }
      }

      getterThunk = buildSubscriptGetterDecl(getter, elementTy, dc,
                                             getterIndices);
      if (setter)
        setterThunk = buildSubscriptSetterDecl(setter, elementTy, dc,
                                               setterIndices);

      // Build the subscript declaration.
      auto bodyPatterns =
          getterThunk->getBodyParamPatterns()[1]->clone(context);
      DeclName name(context, context.Id_subscript, { Identifier() });
      auto subscript
        = Impl.createDeclWithClangNode<SubscriptDecl>(getter->getClangNode(),
                                      name, decl->getLoc(), bodyPatterns,
                                      decl->getLoc(),
                                      TypeLoc::withoutLoc(elementTy), dc);
      subscript->makeComputed(SourceLoc(), getterThunk, setterThunk, nullptr,
                              SourceLoc());
      auto indicesType = bodyPatterns->getType();
      indicesType = indicesType->getRelabeledType(context,
                                                  name.getArgumentNames());

      subscript->setType(FunctionType::get(indicesType,
                                           subscript->getElementType()));
      addObjCAttribute(subscript, None);

      // Optional subscripts in protocols.
      if (optionalMethods && isa<ProtocolDecl>(dc))
        subscript->getAttrs().add(new (Impl.SwiftContext)
                                         OptionalAttr(true));

      // Note that we've created this subscript.
      Impl.Subscripts[{getter, setter}] = subscript;
      Impl.Subscripts[{getterThunk, nullptr}] = subscript;

      // Make the getter/setter methods unavailable.
      if (!getter->getAttrs().isUnavailable(Impl.SwiftContext))
        Impl.markUnavailable(getter, "use subscripting");
      if (setter && !setter->getAttrs().isUnavailable(Impl.SwiftContext))
        Impl.markUnavailable(setter, "use subscripting");

      // Determine whether this subscript operation overrides another subscript
      // operation.
      // FIXME: This ends up looking in the superclass for entirely bogus
      // reasons. Fix it.
      auto containerTy = dc->getDeclaredTypeInContext();
      SmallVector<ValueDecl *, 2> lookup;
      dc->lookupQualified(containerTy, name,
                          NL_QualifiedDefault | NL_KnownNoDependency,
                          Impl.getTypeResolver(), lookup);
      Type unlabeledIndices;
      for (auto result : lookup) {
        auto parentSub = dyn_cast<SubscriptDecl>(result);
        if (!parentSub)
          continue;

        // Compute the type of indices for our own subscript operation, lazily.
        if (!unlabeledIndices) {
          unlabeledIndices = subscript->getIndices()->getType()
                               ->getUnlabeledType(Impl.SwiftContext);
        }

        // Compute the type of indices for the subscript we found.
        auto parentUnlabeledIndices = parentSub->getIndices()->getType()
                                       ->getUnlabeledType(Impl.SwiftContext);
        if (!unlabeledIndices->isEqual(parentUnlabeledIndices))
          continue;

        if (parentSub == subscript)
          continue;

        const DeclContext *overrideContext = parentSub->getDeclContext();
        assert(dc != overrideContext && "subscript already exists");

        if (overrideContext->getDeclaredTypeInContext()->isEqual(containerTy)) {
          // We've encountered a redeclaration of the subscript.
          // HACK: Just update the original declaration instead of importing a
          // second subscript.
          handleSubscriptRedeclaration(parentSub, subscript);
          Impl.Subscripts[{getter, setter}] = parentSub;
          return nullptr;
        }

        // The index types match. This is an override, so mark it as such.
        subscript->setOverriddenDecl(parentSub);
        getterThunk->setOverriddenDecl(parentSub->getGetter());
        if (auto parentSetter = parentSub->getSetter()) {
          if (setterThunk)
            setterThunk->setOverriddenDecl(parentSetter);
        }

        // FIXME: Eventually, deal with multiple overrides.
        break;
      }

      return subscript;
    }

    /// Import the accessor and its attributes.
    FuncDecl *importAccessor(clang::ObjCMethodDecl *clangAccessor,
                             DeclContext *dc) {
      auto *accessor =
          cast_or_null<FuncDecl>(VisitObjCMethodDecl(clangAccessor, dc));
      if (!accessor) {
        return nullptr;
      }

      Impl.importAttributes(clangAccessor, accessor);

      return accessor;
    }

  public:

    /// Recursively add the given protocol and its inherited protocols to the
    /// given vector, guarded by the known set of protocols.
    void addProtocols(ProtocolDecl *protocol,
                      SmallVectorImpl<ProtocolDecl *> &protocols,
                      llvm::SmallPtrSet<ProtocolDecl *, 4> &known) {
      if (!known.insert(protocol).second)
        return;

      protocols.push_back(protocol);
      for (auto inherited : protocol->getInheritedProtocols(
                              Impl.getTypeResolver()))
        addProtocols(inherited, protocols, known);
    }

    // Import the given Objective-C protocol list, along with any
    // implicitly-provided protocols, and attach them to the given
    // declaration.
    void importObjCProtocols(Decl *decl,
                             const clang::ObjCProtocolList &clangProtocols,
                             SmallVectorImpl<TypeLoc> &inheritedTypes) {
      SmallVector<ProtocolDecl *, 4> protocols;
      llvm::SmallPtrSet<ProtocolDecl *, 4> knownProtocols;
      if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
        nominal->getImplicitProtocols(protocols);
        knownProtocols.insert(protocols.begin(), protocols.end());
      }

      for (auto cp = clangProtocols.begin(), cpEnd = clangProtocols.end();
           cp != cpEnd; ++cp) {
        if (auto proto = cast_or_null<ProtocolDecl>(Impl.importDecl(*cp))) {
          addProtocols(proto, protocols, knownProtocols);
          inheritedTypes.push_back(
            TypeLoc::withoutLoc(proto->getDeclaredType()));
        }
      }

      addObjCProtocolConformances(decl, protocols);
    }

    /// Add conformances to the given Objective-C protocols to the
    /// given declaration.
    void addObjCProtocolConformances(Decl *decl,
                                     ArrayRef<ProtocolDecl*> protocols) {
      // Set the inherited protocols of a protocol.
      if (auto proto = dyn_cast<ProtocolDecl>(decl)) {
        // Copy the list of protocols.
        MutableArrayRef<ProtocolDecl *> allProtocols
          = Impl.SwiftContext.AllocateCopy(protocols);
        proto->setInheritedProtocols(allProtocols);

        return;
      }

      Impl.recordImportedProtocols(decl, protocols);

      // Synthesize trivial conformances for each of the protocols.
      SmallVector<ProtocolConformance *, 4> conformances;
;
      auto dc = decl->getInnermostDeclContext();
      auto &ctx = Impl.SwiftContext;
      for (unsigned i = 0, n = protocols.size(); i != n; ++i) {
        // FIXME: Build a superclass conformance if the superclass
        // conforms.
        auto conformance
          = ctx.getConformance(dc->getDeclaredTypeOfContext(),
                               protocols[i], SourceLoc(),
                               dc,
                               ProtocolConformanceState::Incomplete);
        Impl.scheduleFinishProtocolConformance(conformance);
        conformances.push_back(conformance);
      }

      // Set the conformances.
      // FIXME: This could be lazier.
      unsigned id = Impl.allocateDelayedConformance(std::move(conformances));
      if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
        nominal->setConformanceLoader(&Impl, id);
      } else {
        auto ext = cast<ExtensionDecl>(decl);
        ext->setConformanceLoader(&Impl, id);
      }
    }

    /// Finds the counterpart accessor method for \p MD, if one exists, in the
    /// same lexical context.
    const clang::ObjCMethodDecl *
    findImplicitPropertyAccessor(const clang::ObjCMethodDecl *MD) {
      // FIXME: Do we want to infer class properties?
      if (!MD->isInstanceMethod())
        return nullptr;

      // First, collect information about the method we have.
      clang::Selector sel = MD->getSelector();
      llvm::SmallString<64> counterpartName;
      auto numArgs = sel.getNumArgs();
      clang::QualType propTy;

      if (numArgs > 1)
        return nullptr;

      if (numArgs == 0) {
        clang::IdentifierInfo *getterID = sel.getIdentifierInfoForSlot(0);
        if (!getterID)
          return nullptr;
        counterpartName =
          clang::SelectorTable::constructSetterName(getterID->getName());
        propTy = MD->getReturnType();

      } else {
        if (!MD->getReturnType()->isVoidType())
          return nullptr;

        clang::IdentifierInfo *setterID = sel.getIdentifierInfoForSlot(0);
        if (!setterID || !setterID->getName().startswith("set"))
          return nullptr;
        counterpartName = setterID->getName().substr(3);
        counterpartName[0] = tolower(counterpartName[0]);
        propTy = MD->parameters().front()->getType();
      }

      // Next, look for its counterpart.
      const clang::ASTContext &clangCtx = Impl.getClangASTContext();
      auto container = cast<clang::ObjCContainerDecl>(MD->getDeclContext());
      for (auto method : make_range(container->instmeth_begin(),
                                    container->instmeth_end())) {
        // Condition 1: it must be a getter if we have a setter, and vice versa.
        clang::Selector nextSel = method->getSelector();
        if (nextSel.getNumArgs() != (1 - numArgs))
          continue;

        // Condition 2: it must have the name we expect.
        clang::IdentifierInfo *nextID = nextSel.getIdentifierInfoForSlot(0);
        if (!nextID)
          continue;
        if (nextID->getName() != counterpartName)
          continue;

        // Condition 3: it must have the right type signature.
        if (numArgs == 0) {
          if (!method->getReturnType()->isVoidType())
            continue;
          clang::QualType paramTy = method->parameters().front()->getType();
          if (!clangCtx.hasSameUnqualifiedType(propTy, paramTy))
            continue;
        } else {
          clang::QualType returnTy = method->getReturnType();
          if (!clangCtx.hasSameUnqualifiedType(propTy, returnTy))
            continue;
        }

        return method;
      }

      return nullptr;
    }

    /// Creates a computed property VarDecl from the given getter and
    /// optional setter.
    Decl *makeImplicitPropertyDecl(Decl *opaqueGetter,
                                   Decl *opaqueSetter,
                                   DeclContext *dc) {
      auto getter = cast<FuncDecl>(opaqueGetter);
      auto setter = cast_or_null<FuncDecl>(opaqueSetter);
      assert(!setter || setter->getResultType()->isVoid());

      auto name = getter->getName();

      // Check whether there is a function with the same name as this
      // property. If so, suppress the property; the user will have to use
      // the methods directly, to avoid ambiguities.
      auto containerTy = dc->getDeclaredTypeInContext();
      VarDecl *overridden = nullptr;
      SmallVector<ValueDecl *, 2> lookup;
      dc->lookupQualified(containerTy, name,
                          NL_QualifiedDefault | NL_KnownNoDependency,
                          nullptr, lookup);
      for (auto result : lookup) {
        if (isa<FuncDecl>(result))
          return nullptr;

        if (auto var = dyn_cast<VarDecl>(result))
          overridden = var;
      }

      // Re-import the type as a property type.
      auto clangGetter = cast<clang::ObjCMethodDecl>(getter->getClangDecl());
      auto type = Impl.importType(clangGetter->getReturnType(),
                                  ImportTypeKind::Property,
                                  isInSystemModule(dc),
                                  /*isFullyBridgeable*/true);
      if (!type)
        return nullptr;
      
      auto result = Impl.createDeclWithClangNode<VarDecl>(clangGetter,
          /*static*/ false, /*IsLet*/ false,
          Impl.importSourceLoc(clangGetter->getLocation()),
          name, type, dc);
      
      // Turn this into a computed property.
      // FIXME: Fake locations for '{' and '}'?
      result->makeComputed(SourceLoc(), getter, setter, nullptr, SourceLoc());
      addObjCAttribute(result, None);

      if (overridden)
        result->setOverriddenDecl(overridden);

      return result;
    }

    static bool
    isPotentiallyConflictingSetter(const clang::ObjCProtocolDecl *proto,
                                   const clang::ObjCMethodDecl *method) {
      auto sel = method->getSelector();
      if (sel.getNumArgs() != 1)
        return false;

      clang::IdentifierInfo *setterID = sel.getIdentifierInfoForSlot(0);
      if (!setterID || !setterID->getName().startswith("set"))
        return false;

      for (auto *prop : proto->properties()) {
        if (prop->getSetterName() == sel)
          return true;
      }

      return false;
    }

    /// Import members of the given Objective-C container and add them to the
    /// list of corresponding Swift members.
    void importObjCMembers(const clang::ObjCContainerDecl *decl,
                           DeclContext *swiftContext,
                           SmallVectorImpl<Decl *> &members,
                           bool &hasMissingRequiredMember) {
      llvm::SmallPtrSet<Decl *, 4> knownMembers;
      for (auto m = decl->decls_begin(), mEnd = decl->decls_end();
           m != mEnd; ++m) {
        auto nd = dyn_cast<clang::NamedDecl>(*m);
        if (!nd || nd != nd->getCanonicalDecl())
          continue;

        auto member = Impl.importDecl(nd);
        if (!member) {
          if (auto method = dyn_cast<clang::ObjCMethodDecl>(nd)) {
            if (method->getImplementationControl() ==
                clang::ObjCMethodDecl::Required)
              hasMissingRequiredMember = true;
          } else if (auto prop = dyn_cast<clang::ObjCPropertyDecl>(nd)) {
            if (prop->getPropertyImplementation() ==
                clang::ObjCPropertyDecl::Required)
              hasMissingRequiredMember = true;
          }
          continue;
        }

        if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(nd)) {
          // If there is a special declaration associated with this member,
          // add it now.
          if (auto special = importSpecialMethod(member, swiftContext)) {
            if (knownMembers.insert(special).second)
              members.push_back(special);
          }

          // If this is a factory method, try to import it as a constructor.
          if (auto factory = importFactoryMethodAsConstructor(
                               member,
                               objcMethod, 
                               Impl.importSelector(objcMethod->getSelector()),
                               swiftContext)) {
            if (*factory)
              members.push_back(*factory);
          }

          // Objective-C root class instance methods are reflected on the
          // metatype as well.
          if (objcMethod->isInstanceMethod()) {
            Type swiftTy = swiftContext->getDeclaredTypeInContext();
            auto swiftClass = swiftTy->getClassOrBoundGenericClass();
            if (swiftClass && !swiftClass->getSuperclass() &&
                !decl->getClassMethod(objcMethod->getSelector(),
                                      /*AllowHidden=*/true)) {
              auto classMember = VisitObjCMethodDecl(objcMethod, swiftContext,
                                                     true);
              if (classMember)
                members.push_back(classMember);
            }
          }

          // Import explicit properties as instance properties, not as separate
          // getter and setter methods.
          if (!Impl.isAccessibilityDecl(objcMethod)) {
            // If this member is a method that is a getter or setter for a
            // propertythat was imported, don't add it to the list of members
            // so it won't be found by name lookup. This eliminates the
            // ambiguity between property names and getter names (by choosing
            // to only have a variable).
            if (objcMethod->isPropertyAccessor()) {
              auto prop = objcMethod->findPropertyDecl(/*checkOverrides=*/false);
              assert(prop);
              (void)Impl.importDecl(const_cast<clang::ObjCPropertyDecl *>(prop));
              // We may have attached this member to an existing property even
              // if we've failed to import a new property.
              if (cast<FuncDecl>(member)->isAccessor())
                continue;
            } else if (Impl.InferImplicitProperties) {
              // Try to infer properties for matched getter/setter pairs.
              // Be careful to only do this once per matched pair.
              if (auto counterpart = findImplicitPropertyAccessor(objcMethod)) {
                if (auto counterpartImported = Impl.importDecl(counterpart)) {
                  if (objcMethod->getReturnType()->isVoidType()) {
                    if (auto prop = makeImplicitPropertyDecl(counterpartImported,
                                                             member,
                                                             swiftContext)) {
                      members.push_back(prop);
                    } else {
                      // If we fail to import the implicit property, fall back to
                      // adding the accessors as members. We have to add BOTH
                      // accessors here because we already skipped over the other
                      // one.
                      members.push_back(member);
                      members.push_back(counterpartImported);
                    }
                  }
                  continue;
                }
              }
            } else if (auto *proto = dyn_cast<clang::ObjCProtocolDecl>(decl)) {
              if (isPotentiallyConflictingSetter(proto, objcMethod))
                continue;
            }
          }
        }

        members.push_back(member);
      }

      // Hack to deal with unannotated Objective-C protocols. If the protocol
      // comes from clang and is not annotated and the protocol requirement
      // itself is not annotated, then infer availability of the requirement
      // based on its types. This makes it possible for a type to conform to an
      // Objective-C protocol that is missing annotations but whose requirements
      // use types that are less available than the conforming type.
      auto *proto = dyn_cast<ProtocolDecl>(swiftContext);
      if (!proto || proto->getAttrs().hasAttribute<AvailableAttr>())
        return;

      for (Decl *member : members) {
        inferProtocolMemberAvailability(Impl, swiftContext, member);
      }

    }

    static bool
    classImplementsProtocol(const clang::ObjCInterfaceDecl *constInterface,
                            const clang::ObjCProtocolDecl *constProto,
                            bool checkCategories) {
      auto interface = const_cast<clang::ObjCInterfaceDecl *>(constInterface);
      auto proto = const_cast<clang::ObjCProtocolDecl *>(constProto);
      return interface->ClassImplementsProtocol(proto, checkCategories);
    }

    /// \brief Import the members of all of the protocols to which the given
    /// Objective-C class, category, or extension explicitly conforms into
    /// the given list of members, so long as the method was not already
    /// declared in the class.
    ///
    /// FIXME: This whole thing is a hack, because name lookup should really
    /// just find these members when it looks in the protocol. Unfortunately,
    /// that's not something the name lookup code can handle right now, and
    /// it may still be necessary when the protocol's instance methods become
    /// class methods on a root class (e.g. NSObject-the-protocol's instance
    /// methods become class methods on NSObject).
    void importMirroredProtocolMembers(const clang::ObjCContainerDecl *decl,
                                       DeclContext *dc,
                                       ArrayRef<ProtocolDecl *> protocols,
                                       SmallVectorImpl<Decl *> &members,
                                       ASTContext &Ctx) {
      Type swiftTy = dc->getDeclaredTypeInContext();
      auto swiftClass = swiftTy->getClassOrBoundGenericClass();
      bool isRoot = swiftClass && !swiftClass->getSuperclass();

      const clang::ObjCInterfaceDecl *interfaceDecl = nullptr;
      const ClangModuleUnit *declModule;
      const ClangModuleUnit *interfaceModule;

      for (auto proto : protocols) {
        auto clangProto =
          cast_or_null<clang::ObjCProtocolDecl>(proto->getClangDecl());
        if (!clangProto)
          continue;

        if (!interfaceDecl) {
          declModule = Impl.getClangModuleForDecl(decl);
          if ((interfaceDecl = dyn_cast<clang::ObjCInterfaceDecl>(decl))) {
            interfaceModule = declModule;
          } else {
            auto category = cast<clang::ObjCCategoryDecl>(decl);
            interfaceDecl = category->getClassInterface();
            interfaceModule = Impl.getClangModuleForDecl(interfaceDecl);
          }
        }

        // Don't import a protocol's members if the superclass already adopts
        // the protocol, or (for categories) if the class itself adopts it
        // in its main @interface.
        if (decl != interfaceDecl)
          if (classImplementsProtocol(interfaceDecl, clangProto, false))
            continue;
        if (auto superInterface = interfaceDecl->getSuperClass())
          if (classImplementsProtocol(superInterface, clangProto, true))
            continue;

        for (auto member : proto->getMembers()) {
          if (auto prop = dyn_cast<VarDecl>(member)) {
            auto objcProp =
              dyn_cast_or_null<clang::ObjCPropertyDecl>(prop->getClangDecl());
            if (!objcProp)
              continue;

            // We can't import a property if there's already a method with this
            // name. (This also covers other properties with that same name.)
            // FIXME: We should still mirror the setter as a method if it's
            // not already there.
            clang::Selector sel = objcProp->getGetterName();
            if (interfaceDecl->getInstanceMethod(sel))
              continue;

            bool inNearbyCategory =
                std::any_of(interfaceDecl->visible_categories_begin(),
                            interfaceDecl->visible_categories_end(),
                            [=](const clang::ObjCCategoryDecl *category)->bool {
              if (category != decl) {
                auto *categoryModule = Impl.getClangModuleForDecl(category);
                if (categoryModule != declModule &&
                    categoryModule != interfaceModule) {
                  return false;
                }
              }
              return category->getInstanceMethod(sel);
            });
            if (inNearbyCategory)
              continue;

            if (auto imported = Impl.importMirroredDecl(objcProp, dc, proto)) {
              members.push_back(imported);
              // FIXME: We should mirror properties of the root class onto the
              // metatype.
            }

            continue;
          }

          auto afd = dyn_cast<AbstractFunctionDecl>(member);
          if (!afd)
            continue;

          if (auto func = dyn_cast<FuncDecl>(afd))
            if (func->isAccessor())
              continue;

          auto objcMethod =
            dyn_cast_or_null<clang::ObjCMethodDecl>(member->getClangDecl());
          if (!objcMethod)
            continue;

          // When mirroring an initializer, make it designated and required.
          if (Impl.isInitMethod(objcMethod)) {
            // Import the constructor.
            if (auto imported = importConstructor(
                                  objcMethod, dc, /*implicit=*/true,
                                  CtorInitializerKind::Designated,
                                  /*required=*/true)){
              members.push_back(imported);
            }

            continue;
          }

          // Import the method.
          if (auto imported = Impl.importMirroredDecl(objcMethod, dc, proto)) {
            members.push_back(imported);
          }

          // Import instance methods of a root class also as class methods.
          if (isRoot && objcMethod->isInstanceMethod()) {
            if (auto classImport = Impl.importMirroredDecl(objcMethod,
                                                           dc, proto, true))
              members.push_back(classImport);
          }
        }
      }
    }

    /// \brief Import constructors from our superclasses (and their
    /// categories/extensions), effectively "inheriting" constructors.
    void importInheritedConstructors(ClassDecl *classDecl,
                                     SmallVectorImpl<Decl *> &newMembers) {
      if (!classDecl->hasSuperclass())
        return;

      auto curObjCClass
        = cast<clang::ObjCInterfaceDecl>(classDecl->getClangDecl());

      auto inheritConstructors = [&](DeclRange members,
                                     Optional<CtorInitializerKind> kind) {
        for (auto member : members) {
          auto ctor = dyn_cast<ConstructorDecl>(member);
          if (!ctor)
            continue;

          // Don't inherit (non-convenience) factory initializers.
          // Note that convenience factories return instancetype and can be
          // inherited.
          switch (ctor->getInitKind()) {
          case CtorInitializerKind::Factory:
            continue;
          case CtorInitializerKind::ConvenienceFactory:
          case CtorInitializerKind::Convenience:
          case CtorInitializerKind::Designated:
            break;
          }

          auto objcMethod
            = dyn_cast_or_null<clang::ObjCMethodDecl>(ctor->getClangDecl());
          if (!objcMethod)
            continue;

          auto &clangSourceMgr = Impl.getClangASTContext().getSourceManager();
          clang::PrettyStackTraceDecl trace(objcMethod, clang::SourceLocation(),
                                            clangSourceMgr,
                                            "importing (inherited)");

          // If this initializer came from a factory method, inherit
          // it as an initializer.
          if (objcMethod->isClassMethod()) {
            assert(ctor->getInitKind() ==
                     CtorInitializerKind::ConvenienceFactory);

            ImportedName importedName = Impl.importFullName(objcMethod);
            importedName.HasCustomName = true;
            bool redundant;
            if (auto newCtor = importConstructor(objcMethod, classDecl,
                                                 /*implicit=*/true,
                                                 ctor->getInitKind(),
                                                 /*required=*/false, 
                                                 ctor->getObjCSelector(),
                                                 importedName,
                                                 objcMethod->parameters(),
                                                 objcMethod->isVariadic(),
                                                 redundant)) {
              Impl.importAttributes(objcMethod, newCtor, curObjCClass);
              newMembers.push_back(newCtor);
            }
            continue;
          }

          // Figure out what kind of constructor this will be.
          CtorInitializerKind myKind;
          bool isRequired = false;
          if (ctor->isRequired()) {
            // Required initializers are always considered designated.
            isRequired = true;
            myKind = CtorInitializerKind::Designated;
          } else if (kind) {
            myKind = *kind;
          } else {
            myKind = ctor->getInitKind();
          }

          // Import the constructor into this context.
          if (auto newCtor = importConstructor(objcMethod, classDecl,
                                               /*implicit=*/true,
                                               myKind,
                                               isRequired)) {
            Impl.importAttributes(objcMethod, newCtor, curObjCClass);
            newMembers.push_back(newCtor);
          }
        }
      };

      // The kind of initializer to import. If this class has designated
      // initializers, everything it imports is a convenience initializer.
      Optional<CtorInitializerKind> kind;
      if (Impl.hasDesignatedInitializers(curObjCClass))
        kind = CtorInitializerKind::Convenience;

      auto superclass
        = cast<ClassDecl>(classDecl->getSuperclass()->getAnyNominal());

      // If we we have a superclass, import from it.
      if (auto superclassClangDecl = superclass->getClangDecl()) {
        if (isa<clang::ObjCInterfaceDecl>(superclassClangDecl)) {
          inheritConstructors(superclass->getMembers(), kind);

          for (auto ext : superclass->getExtensions())
            inheritConstructors(ext->getMembers(), kind);
        }
      }
    }

    Decl *VisitObjCCategoryDecl(const clang::ObjCCategoryDecl *decl) {
      // Objective-C categories and extensions map to Swift extensions.
      clang::SourceLocation categoryNameLoc = decl->getCategoryNameLoc();
      if (categoryNameLoc.isMacroID()) {
        // Climb up to the top-most macro invocation.
        clang::Preprocessor &PP = Impl.getClangPreprocessor();
        clang::SourceManager &SM = PP.getSourceManager();
        clang::SourceLocation macroCaller =
          SM.getImmediateMacroCallerLoc(categoryNameLoc);
        while (macroCaller.isMacroID()) {
          categoryNameLoc = macroCaller;
          macroCaller = SM.getImmediateMacroCallerLoc(categoryNameLoc);
        }
        if (PP.getImmediateMacroName(categoryNameLoc) == "SWIFT_EXTENSION")
          return nullptr;
      }

      // Find the Swift class being extended.
      auto objcClass
        = cast_or_null<ClassDecl>(Impl.importDecl(decl->getClassInterface()));
      if (!objcClass)
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      // Create the extension declaration and record it.
      auto loc = Impl.importSourceLoc(decl->getLocStart());
      auto result = ExtensionDecl::create(
                      Impl.SwiftContext, loc,
                      TypeLoc::withoutLoc(objcClass->getDeclaredType()),
                      { }, dc, nullptr, decl);
      objcClass->addExtension(result);
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
      SmallVector<TypeLoc, 4> inheritedTypes;
      importObjCProtocols(result, decl->getReferencedProtocols(),
                          inheritedTypes);
      result->setValidated();
      result->setInherited(Impl.SwiftContext.AllocateCopy(inheritedTypes));
      result->setCheckedInheritanceClause();
      result->setMemberLoader(&Impl, 0);

      return result;
    }

    template <typename T, typename U>
    T *resolveSwiftDeclImpl(const U *decl, Identifier name, Module *adapter) {
      SmallVector<ValueDecl *, 4> results;
      adapter->lookupValue({}, name, NLKind::QualifiedLookup, results);
      if (results.size() == 1) {
        if (auto singleResult = dyn_cast<T>(results.front())) {
          if (auto typeResolver = Impl.getTypeResolver())
            typeResolver->resolveDeclSignature(singleResult);
          Impl.ImportedDecls[decl->getCanonicalDecl()] = singleResult;
          return singleResult;
        }
      }

      return nullptr;
    }

    template <typename T, typename U>
    T *resolveSwiftDecl(const U *decl, Identifier name,
                        ClangModuleUnit *clangModule) {
      if (auto adapter = clangModule->getAdapterModule())
        return resolveSwiftDeclImpl<T>(decl, name, adapter);
      if (clangModule == Impl.ImportedHeaderUnit) {
        // Use an index-based loop because new owners can come in as we're
        // iterating.
        for (size_t i = 0; i < Impl.ImportedHeaderOwners.size(); ++i) {
          Module *owner = Impl.ImportedHeaderOwners[i];
          if (T *result = resolveSwiftDeclImpl<T>(decl, name, owner))
            return result;
        }
      }
      return nullptr;
    }

    template <typename U>
    bool hasNativeSwiftDecl(const U *decl) {
      using clang::AnnotateAttr;
      for (auto annotation : decl->template specific_attrs<AnnotateAttr>()) {
        if (annotation->getAnnotation() == SWIFT_NATIVE_ANNOTATION_STRING) {
          return true;
        }
      }

      return false;
    }

    template <typename T, typename U>
    bool hasNativeSwiftDecl(const U *decl, Identifier name,
                            const DeclContext *dc, T *&swiftDecl) {
      if (!hasNativeSwiftDecl(decl))
        return false;
      if (auto *nameAttr = decl->template getAttr<clang::SwiftNameAttr>()) {
        StringRef customName = nameAttr->getName();
        if (Lexer::isIdentifier(customName))
          name = Impl.SwiftContext.getIdentifier(customName);
      }
      auto wrapperUnit = cast<ClangModuleUnit>(dc->getModuleScopeContext());
      swiftDecl = resolveSwiftDecl<T>(decl, name, wrapperUnit);
      return true;
    }

    void markMissingSwiftDecl(ValueDecl *VD) {
      const char *message;
      if (isa<ClassDecl>(VD))
        message = "cannot find Swift declaration for this class";
      else if (isa<ProtocolDecl>(VD))
        message = "cannot find Swift declaration for this protocol";
      else
        llvm_unreachable("unknown bridged decl kind");
      auto attr = AvailableAttr::createUnconditional(Impl.SwiftContext,
                                                        message);
      VD->getAttrs().add(attr);
    }
    
    Decl *VisitObjCProtocolDecl(const clang::ObjCProtocolDecl *decl) {
      Identifier name = Impl.importFullName(decl).Imported.getBaseName();
      if (name.empty())
        return nullptr;

      // FIXME: Figure out how to deal with incomplete protocols, since that
      // notion doesn't exist in Swift.
      if (!decl->hasDefinition()) {
        // Check if this protocol is implemented in its adapter.
        if (auto clangModule = Impl.getClangModuleForDecl(decl, true))
          if (auto native = resolveSwiftDecl<ProtocolDecl>(decl, name,
                                                           clangModule))
            return native;

        forwardDeclaration = true;
        return nullptr;
      }

      decl = decl->getDefinition();

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      ProtocolDecl *nativeDecl;
      bool declaredNative = hasNativeSwiftDecl(decl, name, dc, nativeDecl);
      if (declaredNative && nativeDecl)
        return nativeDecl;

      // Create the protocol declaration and record it.
      auto result = Impl.createDeclWithClangNode<ProtocolDecl>(decl,
                                   dc,
                                   Impl.importSourceLoc(decl->getLocStart()),
                                   Impl.importSourceLoc(decl->getLocation()),
                                   name,
                                   None);
      result->computeType();
      addObjCAttribute(result, Impl.importIdentifier(decl->getIdentifier()));

      if (declaredNative)
        markMissingSwiftDecl(result);

      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

      // Create the archetype for the implicit 'Self'.
      auto selfId = Impl.SwiftContext.Id_Self;
      auto selfDecl = result->getProtocolSelf();
      ArchetypeType *selfArchetype =
                           ArchetypeType::getNew(Impl.SwiftContext, nullptr,
                                                 result, selfId,
                                                 Type(result->getDeclaredType()),
                                                 Type(), false);
      selfDecl->setArchetype(selfArchetype);

      // Set AllArchetypes of the protocol. ObjC protocols don't have associated
      // types so only the Self archetype is present.
      
      result->getGenericParams()->setAllArchetypes(
             Impl.SwiftContext.AllocateCopy(llvm::makeArrayRef(selfArchetype)));
      
      // Set the generic parameters and requirements.
      auto genericParam = selfDecl->getDeclaredType()
                            ->castTo<GenericTypeParamType>();
      Requirement genericRequirements[2] = {
        Requirement(RequirementKind::WitnessMarker, genericParam, Type()),
        Requirement(RequirementKind::Conformance, genericParam,
                    result->getDeclaredType())
      };
      auto sig = GenericSignature::get(genericParam, genericRequirements);
      result->setGenericSignature(sig);

      result->setCircularityCheck(CircularityCheck::Checked);

      // Import protocols this protocol conforms to.
      SmallVector<TypeLoc, 4> inheritedTypes;
      importObjCProtocols(result, decl->getReferencedProtocols(),
                          inheritedTypes);
      result->setInherited(Impl.SwiftContext.AllocateCopy(inheritedTypes));
      result->setCheckedInheritanceClause();

      result->setMemberLoader(&Impl, 0);

      // Add the protocol decl to ExternalDefinitions so that IRGen can emit
      // metadata for it.
      // FIXME: There might be better ways to do this.
      Impl.registerExternalDecl(result);

      return result;
    }

    // Add inferred attributes.
    void addInferredAttributes(Decl *decl, unsigned attributes) {
      using namespace inferred_attributes;
      if (attributes & requires_stored_property_inits) {
        auto a = new (Impl.SwiftContext)
          RequiresStoredPropertyInitsAttr(/*IsImplicit=*/true);
        decl->getAttrs().add(a);
        cast<ClassDecl>(decl)->setRequiresStoredPropertyInits(true);
      }
    }

    Decl *VisitObjCInterfaceDecl(const clang::ObjCInterfaceDecl *decl) {
      auto name = Impl.importFullName(decl).Imported.getBaseName();
      if (name.empty())
        return nullptr;

      auto createRootClass = [=](DeclContext *dc = nullptr) -> ClassDecl * {
        if (!dc) {
          dc = Impl.getClangModuleForDecl(decl->getCanonicalDecl(),
                                          /*forwardDeclaration=*/true);
        }

        auto result = Impl.createDeclWithClangNode<ClassDecl>(decl,
                                                        SourceLoc(), name,
                                                        SourceLoc(), None,
                                                        nullptr, dc);
        result->computeType();
        Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
        result->setCircularityCheck(CircularityCheck::Checked);
        result->setSuperclass(Type());
        result->setCheckedInheritanceClause();
        result->setAddedImplicitInitializers(); // suppress all initializers
        addObjCAttribute(result, Impl.importIdentifier(decl->getIdentifier()));
        Impl.registerExternalDecl(result);
        return result;
      };

      // Special case for Protocol, which gets forward-declared as an ObjC
      // class which is hidden in modern Objective-C runtimes.
      // We treat it as a foreign class (like a CF type) because it doesn't
      // have a real public class object.
      clang::ASTContext &clangCtx = Impl.getClangASTContext();
      if (decl->getCanonicalDecl() ==
          clangCtx.getObjCProtocolDecl()->getCanonicalDecl()) {
        Type nsObjectTy = Impl.getNSObjectType();
        if (!nsObjectTy)
          return nullptr;
        const ClassDecl *nsObjectDecl =
          nsObjectTy->getClassOrBoundGenericClass();

        auto result = createRootClass(nsObjectDecl->getDeclContext());
        result->setForeign(true);
        return result;
      }

      if (!decl->hasDefinition()) {
        // Check if this class is implemented in its adapter.
        if (auto clangModule = Impl.getClangModuleForDecl(decl, true)) {
          if (auto native = resolveSwiftDecl<ClassDecl>(decl, name,
                                                        clangModule)) {
            return native;
          }
        }

        if (Impl.ImportForwardDeclarations) {
          // Fake it by making an unavailable opaque @objc root class.
          auto result = createRootClass();
          result->setImplicit();
          auto attr = AvailableAttr::createUnconditional(Impl.SwiftContext,
              "This Objective-C class has only been forward-declared; "
              "import its owning module to use it");
          result->getAttrs().add(attr);
          return result;
        }

        forwardDeclaration = true;
        return nullptr;
      }

      decl = decl->getDefinition();
      assert(decl);

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      ClassDecl *nativeDecl;
      bool declaredNative = hasNativeSwiftDecl(decl, name, dc, nativeDecl);
      if (declaredNative && nativeDecl)
        return nativeDecl;

      // Create the class declaration and record it.
      auto result = Impl.createDeclWithClangNode<ClassDecl>(decl,
                                Impl.importSourceLoc(decl->getLocStart()),
                                name,
                                Impl.importSourceLoc(decl->getLocation()),
                                None, nullptr, dc);
      result->computeType();
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
      result->setCircularityCheck(CircularityCheck::Checked);
      result->setAddedImplicitInitializers();
      addObjCAttribute(result, Impl.importIdentifier(decl->getIdentifier()));

      if (declaredNative)
        markMissingSwiftDecl(result);

      // If this Objective-C class has a supertype, import it.
      SmallVector<TypeLoc, 4> inheritedTypes;
      Type superclassType;
      if (auto objcSuper = decl->getSuperClass()) {
        auto super = cast_or_null<ClassDecl>(Impl.importDecl(objcSuper));
        if (!super)
          return nullptr;

        superclassType = super->getDeclaredType();
        inheritedTypes.push_back(TypeLoc::withoutLoc(superclassType));
      }
      result->setSuperclass(superclassType);

      // Import protocols this class conforms to.
      importObjCProtocols(result, decl->getReferencedProtocols(),
                          inheritedTypes);
      result->setInherited(Impl.SwiftContext.AllocateCopy(inheritedTypes));
      result->setCheckedInheritanceClause();

      // Add inferred attributes.
#define INFERRED_ATTRIBUTES(ModuleName, ClassName, AttributeSet)        \
      if (name.str().equals(#ClassName) &&                              \
          result->getParentModule()->getName().str().equals(#ModuleName)) {  \
        using namespace inferred_attributes;                            \
        addInferredAttributes(result, AttributeSet);                    \
      }
#include "InferredAttributes.def"

      result->setMemberLoader(&Impl, 0);

      // Pass the class to the type checker to create an implicit destructor.
      Impl.registerExternalDecl(result);

      return result;
    }

    Decl *VisitObjCImplDecl(const clang::ObjCImplDecl *decl) {
      // Implementations of Objective-C classes and categories are not
      // reflected into Swift.
      return nullptr;
    }

    Decl *VisitObjCPropertyDecl(const clang::ObjCPropertyDecl *decl) {
      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      // While importing the DeclContext, we might have imported the decl
      // itself.
      if (auto Known = Impl.importDeclCached(decl))
        return Known;

      return VisitObjCPropertyDecl(decl, dc);
    }

    void applyPropertyOwnership(
        VarDecl *prop, clang::ObjCPropertyDecl::PropertyAttributeKind attrs) {
      Type ty = prop->getType();
      if (auto innerTy = ty->getAnyOptionalObjectType())
        ty = innerTy;
      if (!ty->isAnyClassReferenceType())
        return;

      ASTContext &ctx = prop->getASTContext();
      if (attrs & clang::ObjCPropertyDecl::OBJC_PR_copy) {
        prop->getAttrs().add(new (ctx) NSCopyingAttr(false));
        return;
      }
      if (attrs & clang::ObjCPropertyDecl::OBJC_PR_weak) {
        prop->getAttrs().add(new (ctx) OwnershipAttr(Ownership::Weak));
        prop->overwriteType(WeakStorageType::get(prop->getType(), ctx));
        return;
      }
      if ((attrs & clang::ObjCPropertyDecl::OBJC_PR_assign) ||
          (attrs & clang::ObjCPropertyDecl::OBJC_PR_unsafe_unretained)) {
        prop->getAttrs().add(new (ctx) OwnershipAttr(Ownership::Unmanaged));
        prop->overwriteType(UnmanagedStorageType::get(prop->getType(), ctx));
        return;
      }
    }

    /// Hack: Handle the case where a property is declared \c readonly in the
    /// main class interface (either explicitly or because of an adopted
    /// protocol) and then \c readwrite in a category/extension.
    ///
    /// \see VisitObjCPropertyDecl
    void handlePropertyRedeclaration(VarDecl *original,
                                     const clang::ObjCPropertyDecl *redecl) {
      // If the property isn't from Clang, we can't safely update it.
      if (!original->hasClangNode())
        return;

      // If the original declaration was implicit, we may want to change that.
      if (original->isImplicit() && !redecl->isImplicit() &&
          !isa<clang::ObjCProtocolDecl>(redecl->getDeclContext()))
        original->setImplicit(false);

      if (!original->getAttrs().hasAttribute<OwnershipAttr>() &&
          !original->getAttrs().hasAttribute<NSCopyingAttr>()) {
        applyPropertyOwnership(original,
                               redecl->getPropertyAttributesAsWritten());
      }

      auto clangSetter = redecl->getSetterMethodDecl();
      if (!clangSetter)
        return;

      // The only other transformation we know how to do safely is add a
      // setter. If the property is already settable, we're done.
      if (original->isSettable(nullptr))
        return;

      FuncDecl *setter = importAccessor(clangSetter,
                                        original->getDeclContext());
      if (!setter)
        return;

      original->setComputedSetter(setter);
    }

    Decl *VisitObjCPropertyDecl(const clang::ObjCPropertyDecl *decl,
                                DeclContext *dc) {
      auto name = Impl.importFullName(decl).Imported.getBaseName();
      if (name.empty())
        return nullptr;

      if (Impl.isAccessibilityDecl(decl))
        return nullptr;

      // Check whether there is a function with the same name as this
      // property. If so, suppress the property; the user will have to use
      // the methods directly, to avoid ambiguities.
      auto containerTy = dc->getDeclaredTypeInContext();
      VarDecl *overridden = nullptr;
      SmallVector<ValueDecl *, 2> lookup;
      dc->lookupQualified(containerTy, name,
                          NL_QualifiedDefault | NL_KnownNoDependency,
                          Impl.getTypeResolver(), lookup);
      for (auto result : lookup) {
        if (isa<FuncDecl>(result) && result->isInstanceMember() &&
            result->getFullName().getArgumentNames().empty())
          return nullptr;

        if (auto var = dyn_cast<VarDecl>(result))
          overridden = var;
      }

      if (overridden) {
        const DeclContext *overrideContext = overridden->getDeclContext();
        if (overrideContext != dc &&
            overrideContext->getDeclaredTypeInContext()->isEqual(containerTy)) {
          // We've encountered a redeclaration of the property.
          // HACK: Just update the original declaration instead of importing a
          // second property.
          handlePropertyRedeclaration(overridden, decl);
          return nullptr;
        }
      }

      Type type = Impl.importPropertyType(decl, isInSystemModule(dc));
      if (!type)
        return nullptr;

      // Import the getter.
      FuncDecl *getter = nullptr;
      if (auto clangGetter = decl->getGetterMethodDecl()) {
        getter = importAccessor(clangGetter, dc);
        if (!getter)
          return nullptr;
      }

      // Import the setter, if there is one.
      FuncDecl *setter = nullptr;
      if (auto clangSetter = decl->getSetterMethodDecl()) {
        setter = importAccessor(clangSetter, dc);
        if (!setter)
          return nullptr;
      }

      // Check whether the property already got imported.
      if (dc == Impl.importDeclContextOf(decl)) {
        auto known = Impl.ImportedDecls.find(decl->getCanonicalDecl());
        if (known != Impl.ImportedDecls.end())
          return known->second;
      }

      auto result = Impl.createDeclWithClangNode<VarDecl>(decl,
          /*static*/ false, /*IsLet*/ false,
          Impl.importSourceLoc(decl->getLocation()),
          name, type, dc);
      
      // Turn this into a computed property.
      // FIXME: Fake locations for '{' and '}'?
      result->makeComputed(SourceLoc(), getter, setter, nullptr, SourceLoc());
      addObjCAttribute(result, None);
      applyPropertyOwnership(result, decl->getPropertyAttributesAsWritten());

      // Handle attributes.
      if (decl->hasAttr<clang::IBOutletAttr>())
        result->getAttrs().add(
            new (Impl.SwiftContext) IBOutletAttr(/*IsImplicit=*/false));
      if (decl->getPropertyImplementation() == clang::ObjCPropertyDecl::Optional
          && isa<ProtocolDecl>(dc) &&
          !result->getAttrs().hasAttribute<OptionalAttr>())
        result->getAttrs().add(new (Impl.SwiftContext)
                                      OptionalAttr(/*implicit*/false));
      // FIXME: Handle IBOutletCollection.

      if (overridden)
        result->setOverriddenDecl(overridden);

      return result;
    }

    Decl *
    VisitObjCCompatibleAliasDecl(const clang::ObjCCompatibleAliasDecl *decl) {
      // Like C++ using declarations, name lookup simply looks through
      // Objective-C compatibility aliases. They are not imported directly.
      return nullptr;
    }

    Decl *VisitLinkageSpecDecl(const clang::LinkageSpecDecl *decl) {
      // Linkage specifications are not imported.
      return nullptr;
    }

    Decl *VisitObjCPropertyImplDecl(const clang::ObjCPropertyImplDecl *decl) {
      // @synthesize and @dynamic are not imported, since they are not part
      // of the interface to a class.
      return nullptr;
    }

    Decl *VisitFileScopeAsmDecl(const clang::FileScopeAsmDecl *decl) {
      return nullptr;
    }

    Decl *VisitAccessSpecDecl(const clang::AccessSpecDecl *decl) {
      return nullptr;
    }

    Decl *VisitFriendDecl(const clang::FriendDecl *decl) {
      // Friends are not imported; Swift has a different access control
      // mechanism.
      return nullptr;
    }

    Decl *VisitFriendTemplateDecl(const clang::FriendTemplateDecl *decl) {
      // Friends are not imported; Swift has a different access control
      // mechanism.
      return nullptr;
    }

    Decl *VisitStaticAssertDecl(const clang::StaticAssertDecl *decl) {
      // Static assertions are an implementation detail.
      return nullptr;
    }

    Decl *VisitBlockDecl(const clang::BlockDecl *decl) {
      // Blocks are not imported (although block types can be imported).
      return nullptr;
    }

    Decl *VisitClassScopeFunctionSpecializationDecl(
                 const clang::ClassScopeFunctionSpecializationDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitImportDecl(const clang::ImportDecl *decl) {
      // Transitive module imports are not handled at the declaration level.
      // Rather, they are understood from the module itself.
      return nullptr;
    }
  };
}

/// \brief Classify the given Clang enumeration to describe how to import it.
EnumKind ClangImporter::Implementation::
classifyEnum(clang::Preprocessor &pp, const clang::EnumDecl *decl) {
  // Anonymous enumerations simply get mapped to constants of the
  // underlying type of the enum, because there is no way to conjure up a
  // name for the Swift type.
  if (!decl->hasNameForLinkage())
    return EnumKind::Constants;

  // Was the enum declared using *_ENUM or *_OPTIONS?
  // FIXME: Use Clang attributes instead of grovelling the macro expansion loc.
  auto loc = decl->getLocStart();
  if (loc.isMacroID()) {
    StringRef MacroName = pp.getImmediateMacroName(loc);
    if (MacroName == "CF_ENUM" || MacroName == "OBJC_ENUM" ||
        MacroName == "SWIFT_ENUM" || MacroName == "__CF_NAMED_ENUM")
      return EnumKind::Enum;
    if (MacroName == "CF_OPTIONS" || MacroName == "OBJC_OPTIONS"
        || MacroName == "SWIFT_OPTIONS")
      return EnumKind::Options;
  }

  // Hardcode a particular annoying case in the OS X headers.
  if (decl->getName() == "DYLD_BOOL")
    return EnumKind::Enum;

  // Fall back to the 'Unknown' path.
  return EnumKind::Unknown;
}

Decl *ClangImporter::Implementation::importDeclCached(
    const clang::NamedDecl *ClangDecl) {
  auto Known = ImportedDecls.find(ClangDecl->getCanonicalDecl());
  if (Known != ImportedDecls.end())
    return Known->second;

  return nullptr;
}

/// Checks if we don't need to import the typedef itself.  If the typedef
/// should be skipped, returns the underlying declaration that the typedef
/// refers to -- this declaration should be imported instead.
static const clang::TagDecl *
canSkipOverTypedef(ClangImporter::Implementation &Impl,
                   const clang::NamedDecl *D,
                   bool &TypedefIsSuperfluous) {
  // If we have a typedef that refers to a tag type of the same name,
  // skip the typedef and import the tag type directly.

  TypedefIsSuperfluous = false;

  auto *ClangTypedef = dyn_cast<clang::TypedefNameDecl>(D);
  if (!ClangTypedef)
    return nullptr;

  const clang::DeclContext *RedeclContext =
      ClangTypedef->getDeclContext()->getRedeclContext();
  if (!RedeclContext->isTranslationUnit())
    return nullptr;

  clang::QualType UnderlyingType = ClangTypedef->getUnderlyingType();

  // A typedef to a typedef should get imported as a typealias.
  auto *TypedefT = UnderlyingType->getAs<clang::TypedefType>();
  if (TypedefT)
    return nullptr;

  auto *TT = UnderlyingType->getAs<clang::TagType>();
  if (!TT)
    return nullptr;

  clang::TagDecl *UnderlyingDecl = TT->getDecl();
  if (UnderlyingDecl->getDeclContext()->getRedeclContext() != RedeclContext)
    return nullptr;

  if (UnderlyingDecl->getDeclName().isEmpty())
    return UnderlyingDecl;

  auto TypedefName = ClangTypedef->getDeclName();
  auto TagDeclName = UnderlyingDecl->getDeclName();
  if (TypedefName != TagDeclName)
    return nullptr;

  TypedefIsSuperfluous = true;
  return UnderlyingDecl;
}

/// Import Clang attributes as Swift attributes.
void ClangImporter::Implementation::importAttributes(
    const clang::NamedDecl *ClangDecl,
    Decl *MappedDecl,
    const clang::ObjCContainerDecl *NewContext)
{
  ASTContext &C = SwiftContext;

  if (auto maybeDefinition = getDefinitionForClangTypeDecl(ClangDecl))
    if (maybeDefinition.getValue())
      ClangDecl = cast<clang::NamedDecl>(maybeDefinition.getValue());

  // Scan through Clang attributes and map them onto Swift
  // equivalents.
  bool AnyUnavailable = false;
  for (clang::NamedDecl::attr_iterator AI = ClangDecl->attr_begin(),
       AE = ClangDecl->attr_end(); AI != AE; ++AI) {
    //
    // __attribute__((unavailable)
    //
    // Mapping: @available(*,unavailable)
    //
    if (auto unavailable = dyn_cast<clang::UnavailableAttr>(*AI)) {
      auto Message = unavailable->getMessage();
      auto attr = AvailableAttr::createUnconditional(C, Message);
      MappedDecl->getAttrs().add(attr);
      AnyUnavailable = true;
      continue;
    }

    //
    // __attribute__((annotate(swift1_unavailable)))
    //
    // Mapping: @available(*, unavailable)
    //
    if (auto unavailable_annot = dyn_cast<clang::AnnotateAttr>(*AI))
      if (unavailable_annot->getAnnotation() == "swift1_unavailable") {
        auto attr = AvailableAttr::createUnconditional(
            C, "", "", UnconditionalAvailabilityKind::UnavailableInSwift);
        MappedDecl->getAttrs().add(attr);
        AnyUnavailable = true;
        continue;
      }

    //
    // __attribute__((deprecated))
    //
    // Mapping: @available(*,deprecated)
    //
    if (auto deprecated = dyn_cast<clang::DeprecatedAttr>(*AI)) {
      auto Message = deprecated->getMessage();
      auto attr = AvailableAttr::createUnconditional(C, Message, "",
                    UnconditionalAvailabilityKind::Deprecated);
      MappedDecl->getAttrs().add(attr);
      continue;
    }

    // __attribute__((availability))
    //
    if (auto avail = dyn_cast<clang::AvailabilityAttr>(*AI)) {
      StringRef Platform = avail->getPlatform()->getName();

      // Is this our special "availability(swift, unavailable)" attribute?
      if (Platform == "swift") {
        auto attr = AvailableAttr::createUnconditional(
            C, avail->getMessage(), /*renamed*/"",
            UnconditionalAvailabilityKind::UnavailableInSwift);
        MappedDecl->getAttrs().add(attr);
        AnyUnavailable = true;
        continue;
      }

      // Does this availability attribute map to the platform we are
      // currently targeting?
      if (!PlatformAvailabilityFilter ||
          !PlatformAvailabilityFilter(Platform))
        continue;

      auto platformK =
        llvm::StringSwitch<Optional<PlatformKind>>(Platform)
          .Case("ios", PlatformKind::iOS)
          .Case("macosx", PlatformKind::OSX)
          .Case("tvos", PlatformKind::tvOS)
          .Case("watchos", PlatformKind::watchOS)
          .Case("ios_app_extension", PlatformKind::iOSApplicationExtension)
          .Case("macosx_app_extension",
                PlatformKind::OSXApplicationExtension)
          .Case("tvos_app_extension",
                PlatformKind::tvOSApplicationExtension)
          .Case("watchos_app_extension",
                PlatformKind::watchOSApplicationExtension)
          .Default(None);
      if (!platformK)
        continue;

      // Is this declaration marked unconditionally unavailable?
      auto Unconditional = UnconditionalAvailabilityKind::None;
      if (avail->getUnavailable()) {
        Unconditional = UnconditionalAvailabilityKind::Unavailable;
        AnyUnavailable = true;
      }

      StringRef message = avail->getMessage();

      const auto &deprecated = avail->getDeprecated();
      if (!deprecated.empty()) {
        if (DeprecatedAsUnavailableFilter &&
            DeprecatedAsUnavailableFilter(deprecated.getMajor(),
                                          deprecated.getMinor())) {
          AnyUnavailable = true;
          Unconditional = UnconditionalAvailabilityKind::Unavailable;
          if (message.empty())
            message = DeprecatedAsUnavailableMessage;
        }
      }

      const auto &obsoleted = avail->getObsoleted();
      const auto &introduced = avail->getIntroduced();

      auto AvAttr = new (C) AvailableAttr(SourceLoc(), SourceRange(),
                                             platformK.getValue(),
                                             message, /*rename*/StringRef(),
                                             introduced, deprecated, obsoleted,
                                             Unconditional, /*implicit=*/false);

      MappedDecl->getAttrs().add(AvAttr);
    }
  }

  // If the declaration is unavailable, we're done.
  if (AnyUnavailable)
    return;

  // Add implicit attributes.
  if (auto MD = dyn_cast<clang::ObjCMethodDecl>(ClangDecl)) {
    Optional<api_notes::ObjCMethodInfo> knownMethod;
    if (NewContext)
      knownMethod = getKnownObjCMethod(MD, NewContext);
    if (!knownMethod)
      knownMethod = getKnownObjCMethod(MD);

    // Any knowledge of methods known due to our whitelists.
    if (knownMethod) {
      // Availability.
      if (knownMethod->Unavailable) {
        auto attr = AvailableAttr::createUnconditional(
                      C,
                      SwiftContext.AllocateCopy(knownMethod->UnavailableMsg));
        MappedDecl->getAttrs().add(attr);

        // If we made a protocol requirement unavailable, mark it optional:
        // nobody should have to satisfy it.
        if (isa<ProtocolDecl>(MappedDecl->getDeclContext())) {
          if (!MappedDecl->getAttrs().hasAttribute<OptionalAttr>())
            MappedDecl->getAttrs().add(new (C) OptionalAttr(/*implicit*/false));
        }
      }
    }
  } else if (auto PD = dyn_cast<clang::ObjCPropertyDecl>(ClangDecl)) {
    if (auto knownProperty = getKnownObjCProperty(PD)) {
      if (knownProperty->Unavailable) {
        auto attr = AvailableAttr::createUnconditional(
                      C,
                      SwiftContext.AllocateCopy(knownProperty->UnavailableMsg));
        MappedDecl->getAttrs().add(attr);
      }
    }
  } else if (auto CD = dyn_cast<clang::ObjCContainerDecl>(ClangDecl)) {
    if (isa<clang::ObjCInterfaceDecl>(CD) || isa<clang::ObjCProtocolDecl>(CD)) {
      if (auto knownContext = getKnownObjCContext(CD)) {
        if (knownContext->Unavailable) {
          auto attr = AvailableAttr::createUnconditional(
                        C,
                        SwiftContext.AllocateCopy(
                          knownContext->UnavailableMsg));
          MappedDecl->getAttrs().add(attr);
        }
      }
    }
  }

  // Ban NSInvocation.
  if (auto ID = dyn_cast<clang::ObjCInterfaceDecl>(ClangDecl)) {
    if (ID->getName() == "NSInvocation") {
      auto attr = AvailableAttr::createUnconditional(C, "");
      MappedDecl->getAttrs().add(attr);
      return;
    }
  }

  // Ban CFRelease|CFRetain|CFAutorelease(CFTypeRef) as well as custom ones
  // such as CGColorRelease(CGColorRef).
  if (auto FD = dyn_cast<clang::FunctionDecl>(ClangDecl)) {
    if (FD->getNumParams() == 1 &&
         (FD->getName().endswith("Release") ||
          FD->getName().endswith("Retain") ||
          FD->getName().endswith("Autorelease")))
      if (auto t = FD->getParamDecl(0)->getType()->getAs<clang::TypedefType>())
        if (isCFTypeDecl(t->getDecl())) {
          auto attr = AvailableAttr::createUnconditional(C,
            "Core Foundation objects are automatically memory managed");
          MappedDecl->getAttrs().add(attr);
          return;
        }
  }

  // Hack: mark any method named "print" with less than two parameters as
  // warn_unqualified_access.
  if (auto MD = dyn_cast<FuncDecl>(MappedDecl)) {
    if (MD->getName().str() == "print" &&
        MD->getDeclContext()->isTypeContext()) {
      auto *formalParams = MD->getBodyParamPatterns()[1];
      if (formalParams->numTopLevelVariables() <= 1) {
        // Use a non-implicit attribute so it shows up in the generated
        // interface.
        MD->getAttrs().add(
            new (C) WarnUnqualifiedAccessAttr(/*implicit*/false));
      }
    }
  }

  // Map __attribute__((warn_unused_result)).
  if (ClangDecl->hasAttr<clang::WarnUnusedResultAttr>()) {
    MappedDecl->getAttrs().add(new (C) WarnUnusedResultAttr(SourceLoc(),
                                                            SourceLoc(),
                                                            false));
  }
  // Map __attribute__((const)).
  if (ClangDecl->hasAttr<clang::ConstAttr>()) {
    MappedDecl->getAttrs().add(new (C) EffectsAttr(EffectsKind::ReadNone));
  }
  // Map __attribute__((pure)).
  if (ClangDecl->hasAttr<clang::PureAttr>()) {
    MappedDecl->getAttrs().add(new (C) EffectsAttr(EffectsKind::ReadOnly));
  }
}

Decl *
ClangImporter::Implementation::importDeclImpl(const clang::NamedDecl *ClangDecl,
                                              bool &TypedefIsSuperfluous,
                                              bool &HadForwardDeclaration) {
  assert(ClangDecl);

  bool SkippedOverTypedef = false;
  Decl *Result = nullptr;
  if (auto *UnderlyingDecl = canSkipOverTypedef(*this, ClangDecl,
                                                TypedefIsSuperfluous)) {
    Result = importDecl(UnderlyingDecl);
    SkippedOverTypedef = true;
  }

  if (!Result) {
    SwiftDeclConverter converter(*this);
    Result = converter.Visit(ClangDecl);
    HadForwardDeclaration = converter.hadForwardDeclaration();
  }
  if (!Result)
    return nullptr;

  if (Result)
    importAttributes(ClangDecl, Result);

#ifndef NDEBUG
  auto Canon = cast<clang::NamedDecl>(ClangDecl->getCanonicalDecl());

  // Note that the decl was imported from Clang.  Don't mark Swift decls as
  // imported.
  if (!Result->getDeclContext()->isModuleScopeContext() ||
      isa<ClangModuleUnit>(Result->getDeclContext())) {
    // Either the Swift declaration was from stdlib,
    // or we imported the underlying decl of the typedef,
    // or we imported the decl itself.
    bool ImportedCorrectly =
        !Result->getClangDecl() || SkippedOverTypedef ||
        Result->getClangDecl()->getCanonicalDecl() == Canon;

    // Or the other type is a typedef,
    if (!ImportedCorrectly &&
        isa<clang::TypedefNameDecl>(Result->getClangDecl())) {
      // both types are ValueDecls:
      if (isa<clang::ValueDecl>(Result->getClangDecl())) {
        ImportedCorrectly =
            getClangASTContext().hasSameType(
                cast<clang::ValueDecl>(Result->getClangDecl())->getType(),
                cast<clang::ValueDecl>(Canon)->getType());
      } else if (isa<clang::TypeDecl>(Result->getClangDecl())) {
        // both types are TypeDecls:
        ImportedCorrectly =
            getClangASTContext().hasSameUnqualifiedType(
                getClangASTContext().getTypeDeclType(
                    cast<clang::TypeDecl>(Result->getClangDecl())),
                getClangASTContext().getTypeDeclType(
                    cast<clang::TypeDecl>(Canon)));
      }
      assert(ImportedCorrectly);
    }
    assert(Result->hasClangNode());
  }
#else
  (void)SkippedOverTypedef;
#endif

  return Result;
}

void ClangImporter::Implementation::startedImportingEntity() {
  ++NumCurrentImportingEntities;
  ++NumTotalImportedEntities;
}

void ClangImporter::Implementation::finishedImportingEntity() {
  assert(NumCurrentImportingEntities &&
         "finishedImportingEntity not paired with startedImportingEntity");
  if (NumCurrentImportingEntities == 1) {
    // We decrease NumCurrentImportingEntities only after pending actions
    // are finished, to avoid recursively re-calling finishPendingActions().
    finishPendingActions();
  }
  --NumCurrentImportingEntities;
}

void ClangImporter::Implementation::finishPendingActions() {
  while (true) {
    if (!RegisteredExternalDecls.empty()) {
      if (hasFinishedTypeChecking()) {
        RegisteredExternalDecls.clear();
      } else {
        Decl *D = RegisteredExternalDecls.pop_back_val();
        SwiftContext.addedExternalDecl(D);
        if (auto typeResolver = getTypeResolver())
          if (auto *nominal = dyn_cast<NominalTypeDecl>(D))
            if (!nominal->hasDelayedMembers())
              typeResolver->resolveExternalDeclImplicitMembers(nominal);
      }
    } else if (!DelayedProtocolConformances.empty()) {
      NormalProtocolConformance *conformance =
          DelayedProtocolConformances.pop_back_val();
      finishProtocolConformance(conformance);
    } else {
      break;
    }
  }
}

/// Finish the given protocol conformance (for an imported type)
/// by filling in any missing witnesses.
void ClangImporter::Implementation::finishProtocolConformance(
    NormalProtocolConformance *conformance) {
  // Create witnesses for requirements not already met.
  for (auto req : conformance->getProtocol()->getMembers()) {
    auto valueReq = dyn_cast<ValueDecl>(req);
    if (!valueReq)
      continue;

    if (!conformance->hasWitness(valueReq)) {
      if (auto func = dyn_cast<AbstractFunctionDecl>(valueReq)){
        // For an optional requirement, record an empty witness:
        // we'll end up querying this at runtime.
        auto Attrs = func->getAttrs();
        if (Attrs.hasAttribute<OptionalAttr>()) {
          conformance->setWitness(valueReq, ConcreteDeclRef());
          continue;
        }
      }

      conformance->setWitness(valueReq, valueReq);
    } else {
      // An initializer that conforms to a requirement is required.
      auto witness = conformance->getWitness(valueReq, nullptr).getDecl();
      if (auto ctor = dyn_cast_or_null<ConstructorDecl>(witness)) {
        if (!ctor->getAttrs().hasAttribute<RequiredAttr>()) {
          ctor->getAttrs().add(
            new (SwiftContext) RequiredAttr(/*implicit=*/true));
        }
      }
    }
  }

  conformance->setState(ProtocolConformanceState::Complete);
}

Decl *ClangImporter::Implementation::importDeclAndCacheImpl(
    const clang::NamedDecl *ClangDecl,
    bool SuperfluousTypedefsAreTransparent) {
  if (!ClangDecl)
    return nullptr;

  clang::PrettyStackTraceDecl trace(ClangDecl, clang::SourceLocation(),
                                    Instance->getSourceManager(), "importing");

  auto Canon = cast<clang::NamedDecl>(ClangDecl->getCanonicalDecl());

  if (auto Known = importDeclCached(Canon)) {
    if (!SuperfluousTypedefsAreTransparent &&
        SuperfluousTypedefs.count(Canon))
      return nullptr;
    return Known;
  }

  bool TypedefIsSuperfluous = false;
  bool HadForwardDeclaration = false;

  ImportingEntityRAII ImportingEntity(*this);
  Decl *Result = importDeclImpl(ClangDecl, TypedefIsSuperfluous,
                                HadForwardDeclaration);
  if (!Result)
    return nullptr;

  if (TypedefIsSuperfluous) {
    SuperfluousTypedefs.insert(Canon);
    if (auto tagDecl = dyn_cast_or_null<clang::TagDecl>(Result->getClangDecl()))
      DeclsWithSuperfluousTypedefs.insert(tagDecl);
  }

  if (!HadForwardDeclaration)
    ImportedDecls[Canon] = Result;

  if (!SuperfluousTypedefsAreTransparent && TypedefIsSuperfluous)
    return nullptr;

  return Result;
}

Decl *
ClangImporter::Implementation::importMirroredDecl(const clang::NamedDecl *decl,
                                                  DeclContext *dc,
                                                  ProtocolDecl *proto,
                                                  bool forceClassMethod) {
  if (!decl)
    return nullptr;

  clang::PrettyStackTraceDecl trace(decl, clang::SourceLocation(),
                                    Instance->getSourceManager(),
                                    "importing (mirrored)");

  auto canon = decl->getCanonicalDecl();
  auto known = ImportedProtocolDecls.find({{canon, forceClassMethod}, dc });
  if (known != ImportedProtocolDecls.end())
    return known->second;

  SwiftDeclConverter converter(*this);
  Decl *result;
  if (auto method = dyn_cast<clang::ObjCMethodDecl>(decl)) {
    result = converter.VisitObjCMethodDecl(method, dc, forceClassMethod);
  } else if (auto prop = dyn_cast<clang::ObjCPropertyDecl>(decl)) {
    assert(!forceClassMethod && "can't mirror properties yet");
    result = converter.VisitObjCPropertyDecl(prop, dc);
  } else {
    llvm_unreachable("unexpected mirrored decl");
  }

  if (result) {
    if (!forceClassMethod) {
      if (auto special = converter.importSpecialMethod(result, dc))
        result = special;
    }

    assert(result->getClangDecl() && result->getClangDecl() == canon);
    result->setImplicit();

    // Map the Clang attributes onto Swift attributes.
    importAttributes(decl, result);

    if (proto->getAttrs().hasAttribute<AvailableAttr>()) {
      if (!result->getAttrs().hasAttribute<AvailableAttr>()) {
        VersionRange protoRange =
            AvailabilityInference::availableRange(proto, SwiftContext);
        applyAvailableAttribute(result, protoRange, SwiftContext);
      }
    } else {
      // Infer the same availability for the mirrored declaration as we would for
      // the protocol member it is mirroring.
      inferProtocolMemberAvailability(*this, dc, result);
    }
  }
  if (result || !converter.hadForwardDeclaration())
    ImportedProtocolDecls[{{canon, forceClassMethod}, dc}] = result;
  return result;
}

DeclContext *ClangImporter::Implementation::importDeclContextImpl(
    const clang::DeclContext *dc) {
  // Every declaration should come from a module, so we should not see the
  // TranslationUnit DeclContext here.
  assert(!dc->isTranslationUnit());

  auto decl = dyn_cast<clang::NamedDecl>(dc);
  if (!decl)
    return nullptr;

  auto swiftDecl = importDecl(decl);
  if (!swiftDecl)
    return nullptr;

  if (auto nominal = dyn_cast<NominalTypeDecl>(swiftDecl))
    return nominal;
  if (auto extension = dyn_cast<ExtensionDecl>(swiftDecl))
    return extension;
  if (auto constructor = dyn_cast<ConstructorDecl>(swiftDecl))
    return constructor;
  if (auto destructor = dyn_cast<DestructorDecl>(swiftDecl))
    return destructor;
  return nullptr;
}

DeclContext *
ClangImporter::Implementation::importDeclContextOf(const clang::Decl *D) {
  const clang::DeclContext *DC = D->getDeclContext();

  if (DC->isTranslationUnit()) {
    if (auto *M = getClangModuleForDecl(D))
      return M;
    else
      return nullptr;
  }

  return importDeclContextImpl(DC);
}

ValueDecl *
ClangImporter::Implementation::createConstant(Identifier name, DeclContext *dc,
                                              Type type,
                                              const clang::APValue &value,
                                              ConstantConvertKind convertKind,
                                              bool isStatic,
                                              ClangNode ClangN) {
  auto &context = SwiftContext;

  // Create the integer literal value.
  Expr *expr = nullptr;
  switch (value.getKind()) {
  case clang::APValue::AddrLabelDiff:
  case clang::APValue::Array:
  case clang::APValue::ComplexFloat:
  case clang::APValue::ComplexInt:
  case clang::APValue::LValue:
  case clang::APValue::MemberPointer:
  case clang::APValue::Struct:
  case clang::APValue::Uninitialized:
  case clang::APValue::Union:
  case clang::APValue::Vector:
    llvm_unreachable("Unhandled APValue kind");

  case clang::APValue::Float:
  case clang::APValue::Int: {
    // Print the value.
    llvm::SmallString<16> printedValueBuf;
    if (value.getKind() == clang::APValue::Int) {
      value.getInt().toString(printedValueBuf);
    } else {
      assert(value.getFloat().isFinite() && "can't handle infinities or NaNs");
      value.getFloat().toString(printedValueBuf);
    }
    StringRef printedValue = printedValueBuf.str();

    // If this was a negative number, record that and strip off the '-'.
    bool isNegative = printedValue.front() == '-';
    if (isNegative)
      printedValue = printedValue.drop_front();

    // Create the expression node.
    StringRef printedValueCopy(context.AllocateCopy(printedValue));
    if (value.getKind() == clang::APValue::Int) {
      expr = new (context) IntegerLiteralExpr(printedValueCopy, SourceLoc(),
                                              /*Implicit=*/true);
    } else {
      expr = new (context) FloatLiteralExpr(printedValueCopy, SourceLoc(),
                                            /*Implicit=*/true);
    }

    if (isNegative)
      cast<NumberLiteralExpr>(expr)->setNegative(SourceLoc());

    break;
  }
  }

  assert(expr);
  return createConstant(name, dc, type, expr, convertKind, isStatic, ClangN);
}


ValueDecl *
ClangImporter::Implementation::createConstant(Identifier name, DeclContext *dc,
                                              Type type, StringRef value,
                                              ConstantConvertKind convertKind,
                                              bool isStatic,
                                              ClangNode ClangN) {
  auto expr = new (SwiftContext) StringLiteralExpr(value, SourceRange());
  return createConstant(name, dc, type, expr, convertKind, isStatic, ClangN);
}


ValueDecl *
ClangImporter::Implementation::createConstant(Identifier name, DeclContext *dc,
                                              Type type, Expr *valueExpr,
                                              ConstantConvertKind convertKind,
                                              bool isStatic,
                                              ClangNode ClangN) {
  auto &context = SwiftContext;

  auto var = createDeclWithClangNode<VarDecl>(ClangN,
                                   isStatic, /*IsLet*/ false,
                                   SourceLoc(), name, type, dc);

  // Form the argument patterns.
  SmallVector<Pattern *, 3> getterArgs;

  // 'self'
  if (dc->isTypeContext()) {
    auto selfTy = dc->getDeclaredTypeInContext();
    if (isStatic)
      selfTy = MetatypeType::get(selfTy);

    getterArgs.push_back(
      Pattern::buildImplicitSelfParameter(SourceLoc(), 
                                          TypeLoc::withoutLoc(selfTy),
                                          dc));
  }
  
  // empty tuple
  getterArgs.push_back(TuplePattern::create(context, SourceLoc(), { },
                                            SourceLoc()));
  getterArgs.back()->setType(TupleType::getEmpty(context));

  // Form the type of the getter.
  auto getterType = type;
  for (auto it = getterArgs.rbegin(), itEnd = getterArgs.rend();
       it != itEnd; ++it) {
    getterType = FunctionType::get((*it)->getType()->getUnlabeledType(context),
                                   getterType);
  }

  // Create the getter function declaration.
  auto func = FuncDecl::create(context, SourceLoc(), StaticSpellingKind::None,
                               SourceLoc(), Identifier(),
                               SourceLoc(), SourceLoc(), SourceLoc(),
                               nullptr, getterType,
                               getterArgs, TypeLoc::withoutLoc(type), dc);
  func->setStatic(isStatic);
  func->setBodyResultType(type);
  func->setAccessibility(Accessibility::Public);

  // If we're not done type checking, build the getter body.
  if (!hasFinishedTypeChecking()) {
    auto expr = valueExpr;

    // If we need a conversion, add one now.
    switch (convertKind) {
    case ConstantConvertKind::None:
      break;

    case ConstantConvertKind::Construction:
    case ConstantConvertKind::ConstructionWithUnwrap: {
      auto typeRef = TypeExpr::createImplicit(type, context);
      
      // make a "(rawValue: <subexpr>)" tuple.
      expr = TupleExpr::create(context, SourceLoc(), expr,
                               context.Id_rawValue, SourceLoc(),
                               SourceLoc(), /*trailingClosure*/false,
                               /*implicit*/true);
      expr = new (context) CallExpr(typeRef, expr, /*Implicit=*/true);
      if (convertKind == ConstantConvertKind::ConstructionWithUnwrap)
        expr = new (context) ForceValueExpr(expr, SourceLoc());
      break;
    }

    case ConstantConvertKind::Coerce:
      break;

    case ConstantConvertKind::Downcast: {
      expr = new (context) ForcedCheckedCastExpr(expr, SourceLoc(), SourceLoc(),
                                                 TypeLoc::withoutLoc(type));
      expr->setImplicit();
      break;
    }
    }

    // Create the return statement.
    auto ret = new (context) ReturnStmt(SourceLoc(), expr);

    // Finally, set the body.
    func->setBody(BraceStmt::create(context, SourceLoc(),
                                    ASTNode(ret),
                                    SourceLoc()));
  }

  // Mark the function transparent so that we inline it away completely.
  func->getAttrs().add(new (context) TransparentAttr(/*implicit*/ true));
  
  // Set the function up as the getter.
  var->makeComputed(SourceLoc(), func, nullptr, nullptr, SourceLoc());

  // Register this thunk as an external definition.
  registerExternalDecl(func);

  return var;
}

/// \brief Create a decl with error type and an "unavailable" attribute on it
/// with the specified message.
void ClangImporter::Implementation::
markUnavailable(ValueDecl *decl, StringRef unavailabilityMsgRef) {

  unavailabilityMsgRef = SwiftContext.AllocateCopy(unavailabilityMsgRef);
  auto ua = AvailableAttr::createUnconditional(SwiftContext,
                                                  unavailabilityMsgRef);
  decl->getAttrs().add(ua);
}

/// \brief Create a decl with error type and an "unavailable" attribute on it
/// with the specified message.
ValueDecl *ClangImporter::Implementation::
createUnavailableDecl(Identifier name, DeclContext *dc, Type type,
                      StringRef UnavailableMessage, bool isStatic,
                      ClangNode ClangN) {

  // Create a new VarDecl with dummy type.
  auto var = createDeclWithClangNode<VarDecl>(ClangN,
                                              isStatic, /*IsLet*/ false,
                                              SourceLoc(), name, type, dc);
  markUnavailable(var, UnavailableMessage);

  return var;
}


void
ClangImporter::Implementation::loadAllMembers(Decl *D, uint64_t unused,
                                              bool *hasMissingRequiredMembers) {
  assert(D->hasClangNode());
  auto clangDecl = cast<clang::ObjCContainerDecl>(D->getClangDecl());

  clang::PrettyStackTraceDecl trace(clangDecl, clang::SourceLocation(),
                                    Instance->getSourceManager(),
                                    "loading members for");

  SwiftDeclConverter converter(*this);

  DeclContext *DC;
  IterableDeclContext *IDC;
  SmallVector<ProtocolDecl *, 4> protos;

  // Figure out the declaration context we're importing into.
  if (auto nominal = dyn_cast<NominalTypeDecl>(D)) {
    DC = nominal;
    IDC = nominal;
  } else {
    auto ext = cast<ExtensionDecl>(D);
    DC = ext;
    IDC = ext;
  }

  ImportingEntityRAII Importing(*this);

  SmallVector<Decl *, 16> members;
  bool scratch;
  if (!hasMissingRequiredMembers)
    hasMissingRequiredMembers = &scratch;
  *hasMissingRequiredMembers = false;
  converter.importObjCMembers(clangDecl, DC,
                              members, *hasMissingRequiredMembers);

  protos = takeImportedProtocols(D);
  if (auto clangClass = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl)) {
    auto swiftClass = cast<ClassDecl>(D);
    clangDecl = clangClass = clangClass->getDefinition();

    // Imported inherited initializers.
    if (clangClass->getName() != "Protocol") {
      converter.importInheritedConstructors(const_cast<ClassDecl *>(swiftClass),
                                            members);
    }

  } else if (auto clangProto = dyn_cast<clang::ObjCProtocolDecl>(clangDecl)) {
    clangDecl = clangProto->getDefinition();
  }

  // Import mirrored declarations for protocols to which this category
  // or extension conforms.
  // FIXME: This is supposed to be a short-term hack.
  converter.importMirroredProtocolMembers(clangDecl, DC,
                                          protos, members, SwiftContext);

  // Add the members now, before ~ImportingEntityRAII does work that might
  // involve them.
  for (auto member : members) {
    IDC->addMember(member);
  }

}

void ClangImporter::Implementation::loadAllConformances(
       const Decl *D, uint64_t contextData,
       SmallVectorImpl<ProtocolConformance *> &Conformances) {
  Conformances = takeDelayedConformance(contextData);
}

Optional<MappedTypeNameKind>
ClangImporter::Implementation::getSpecialTypedefKind(clang::TypedefNameDecl *decl) {
  auto iter = SpecialTypedefNames.find(decl->getCanonicalDecl());
  if (iter == SpecialTypedefNames.end())
    return None;
  return iter->second;
}

Identifier
ClangImporter::getEnumConstantName(const clang::EnumConstantDecl *enumConstant){
  return Impl.importFullName(enumConstant).Imported.getBaseName();
}

