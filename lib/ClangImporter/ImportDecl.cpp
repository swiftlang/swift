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
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/Lex/Preprocessor.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"

#define DEBUG_TYPE "Clang Importer"

STATISTIC(NumTotalImportedEntities, "# of imported clang entities");

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
  // For a protocol, the type is 'Self'.
  if (auto proto = dyn_cast<ProtocolDecl>(dc))
    return proto->getSelf()->getArchetype();

  return dc->getDeclaredTypeOfContext();
}

/// Create an implicit 'self' decl for a method in the specified type.  If
/// 'static' is true, then this is self for a static method in the type.
///
/// Note that this decl is created, but it is returned with an incorrect
/// DeclContext that needs to be reset once the method exists.
///
static VarDecl *createSelfDecl(DeclContext *DC, bool isStaticMethod) {
  auto selfType = getSelfTypeForContext(DC);

  ASTContext &C = DC->getASTContext();

  if (isStaticMethod)
    selfType = MetatypeType::get(selfType, C);

  bool isLet = true;
  if (auto *ND = selfType->getAnyNominal())
    isLet = !isa<StructDecl>(ND) && !isa<EnumDecl>(ND);

  VarDecl *selfDecl = new (C) VarDecl(/*static*/ false, /*IsLet*/isLet,
                                      SourceLoc(), C.Id_self, selfType, DC);
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
  MappedLanguages Languages;
  bool CanBeMissing;

  do {
#define MAP_TYPE(C_TYPE_NAME, C_TYPE_KIND, C_TYPE_BITWIDTH,     \
                 SWIFT_MODULE_NAME, SWIFT_TYPE_NAME, LANGUAGES, \
                 CAN_BE_MISSING, C_NAME_MAPPING)                \
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
      Languages = MappedLanguages::LANGUAGES;                      \
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

  if (Languages != MappedLanguages::All) {
    if ((unsigned(Languages) & unsigned(MappedLanguages::ObjC1)) != 0 &&
        !ClangCtx.getLangOpts().ObjC1)
      return std::make_pair(Type(), "");
  }

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
    // FIXME: why is va_list not a pointer type on 32-bit arm
    if (ClangTypeSize != 64 && ClangTypeSize != 32)
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

/// \brief Returns the common prefix of two strings at camel-case word
/// granularity.
///
/// For example, given "NSFooBar" and "NSFooBas", returns "NSFoo"
/// (not "NSFooBa"). The returned StringRef is a slice of the "a" argument.
///
/// This is used to derive the common prefix of enum constants so we can elide
/// it from the Swift interface.
static StringRef getCommonWordPrefix(StringRef a, StringRef b) {
  unsigned prefixLength = 0;
  unsigned commonSize = std::min(a.size(), b.size());
  for (size_t i = 0; i < commonSize; ++i) {
    // If this is a camel-case word boundary, advance the prefix length.
    if (isupper(a[i]) && isupper(b[i]))
      prefixLength = i;

    if (a[i] != b[i])
      return a.slice(0, prefixLength);
  }
  return a.slice(0, commonSize);
}

namespace {
  enum class OptionSetFactoryMethod {
    FromRaw,
    FromMask,
  };
}

/// Build the 'fromMask' or 'fromRaw' method for an option set.
/// struct NSSomeOptionSet : RawOptionSet {
///   var value : RawType
///   static func fromMask(value: RawType) -> NSSomeOptionSet {
///     return NSSomeOptionSet(value)
///   }
///   static func fromRaw(value: RawType) -> NSSomeOptionSet? {
///     return NSSomeOptionSet(value)
///   }
/// }
static FuncDecl *makeOptionSetFactoryMethod(StructDecl *optionSetDecl,
                                      VarDecl *valueDecl,
                                      OptionSetFactoryMethod factoryMethod) {
  auto &C = optionSetDecl->getASTContext();
  auto optionSetType = optionSetDecl->getDeclaredTypeInContext();
  auto rawType = valueDecl->getType();
  
  VarDecl *selfDecl = createSelfDecl(optionSetDecl, true);
  Pattern *selfParam = createTypedNamedPattern(selfDecl);
  VarDecl *rawDecl = new (C) VarDecl(/*static*/ false, /*IsLet*/true,
                                     SourceLoc(), C.getIdentifier("raw"),
                                     Type(), optionSetDecl);
  rawDecl->setImplicit();
  rawDecl->setType(rawType);
  Pattern *rawParam = createTypedNamedPattern(rawDecl);
  auto rawArgType = TupleType::get(TupleTypeElt(rawType,
                                                C.getIdentifier("raw")), C);
  rawParam = TuplePattern::create(C, SourceLoc(),
                                  TuplePatternElt(rawParam), SourceLoc());
  rawParam->setImplicit();
  rawParam->setType(rawArgType);

  Pattern *argParams[] = {selfParam->clone(C, Pattern::Implicit),
                          rawParam->clone(C, Pattern::Implicit)};
  Pattern *bodyParams[] = {selfParam, rawParam};
  
  Type retType;
  switch (factoryMethod) {
  case OptionSetFactoryMethod::FromMask:
    retType = optionSetType;
    break;
  case OptionSetFactoryMethod::FromRaw:
    retType = OptionalType::get(optionSetType);
    break;
  }
  
  Identifier name;
  switch (factoryMethod) {
  case OptionSetFactoryMethod::FromMask:
    name = C.getIdentifier("fromMask");
    break;
  case OptionSetFactoryMethod::FromRaw:
    name = C.getIdentifier("fromRaw");
    break;
  }
  
  auto factoryDecl = FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None,
                                      SourceLoc(),
                                      name,
                                      SourceLoc(), nullptr, Type(),
                                      argParams,
                                      bodyParams,
                                      TypeLoc::withoutLoc(retType),
                                      optionSetDecl);
  
  factoryDecl->setStatic();
  factoryDecl->setImplicit();
  selfDecl->setDeclContext(factoryDecl);
  rawDecl->setDeclContext(factoryDecl);
  
  Type factoryType = FunctionType::get(rawArgType, retType);
  factoryType = FunctionType::get(selfDecl->getType(), factoryType);
  factoryDecl->setType(factoryType);
  factoryDecl->setBodyResultType(retType);

  auto *ctorRef = new (C) DeclRefExpr(ConcreteDeclRef(optionSetDecl),
                                      SourceLoc(), /*implicit*/ true);
  auto *rawRef = new (C) DeclRefExpr(ConcreteDeclRef(rawDecl),
                                     SourceLoc(), /*implicit*/ true);
  auto *ctorCall = new (C) CallExpr(ctorRef, rawRef,
                                    /*implicit*/ true);
  auto *ctorRet = new (C) ReturnStmt(SourceLoc(), ctorCall,
                                     /*implicit*/ true);
  
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(ctorRet),
                                SourceLoc(),
                                /*implicit*/ true);
  
  factoryDecl->setBody(body);
  
  // Add as an external definition.
  C.addedExternalDecl(factoryDecl);
  
  return factoryDecl;
}

// Build the 'toRaw' method for an option set.
// struct NSSomeOptionSet : RawOptionSet {
//   var value: RawType
//   func toRaw() -> RawType {
//     return self.value
//   }
// }
static FuncDecl *makeOptionSetToRawMethod(StructDecl *optionSetDecl,
                                          ValueDecl *valueDecl) {
  ASTContext &C = optionSetDecl->getASTContext();
  auto optionSetType = optionSetDecl->getDeclaredTypeInContext();
  auto rawType = valueDecl->getType();

  VarDecl *selfDecl = createSelfDecl(optionSetDecl, false);
  Pattern *selfParam = createTypedNamedPattern(selfDecl);

  Pattern *methodParam = TuplePattern::create(C, SourceLoc(),{},SourceLoc());
  methodParam->setType(TupleType::getEmpty(C));
  Pattern *params[] = {selfParam, methodParam};

  FuncDecl *toRawDecl = FuncDecl::create(
      C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
      C.getIdentifier("toRaw"), SourceLoc(), nullptr, Type(), params, params,
      TypeLoc::withoutLoc(rawType), optionSetDecl);
  toRawDecl->setImplicit();
  
  auto toRawArgType = TupleType::getEmpty(C);
  Type toRawType = FunctionType::get(toRawArgType, rawType);
  toRawType = FunctionType::get(optionSetType, toRawType);
  toRawDecl->setType(toRawType);
  toRawDecl->setBodyResultType(rawType);
  
  selfDecl->setDeclContext(toRawDecl);

  auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/ true);
  auto valueRef = new (C) MemberRefExpr(selfRef, SourceLoc(),
                                        valueDecl, SourceLoc(),
                                        /*implicit*/ true);
  auto valueRet = new (C) ReturnStmt(SourceLoc(), valueRef);
  
  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(valueRet),
                                SourceLoc(),
                                /*implicit*/ true);
  toRawDecl->setBody(body);
  
  // Add as an external definition.
  C.addedExternalDecl(toRawDecl);

  return toRawDecl;
}

static Expr *
getOperatorRef(ASTContext &C, Identifier name) {
  // FIXME: This is hideous!
  UnqualifiedLookup lookup(name, C.getStdlibModule(), nullptr);
  if (!lookup.isSuccess())
    return nullptr;
  
  SmallVector<ValueDecl *, 4> found;
  for (auto &result : lookup.Results) {
    if (!result.hasValueDecl())
      continue;
    
    if (!isa<FuncDecl>(result.getValueDecl()))
      continue;
    
    found.push_back(result.getValueDecl());
  }
  
  if (found.empty())
    return nullptr;
  
  if (found.size() == 1) {
    return new (C) DeclRefExpr(found[0], SourceLoc(),
                               /*Implicit=*/true);
  } else {
    auto foundCopy = C.AllocateCopy(found);
    return new (C) OverloadedDeclRefExpr(
                                     foundCopy, SourceLoc(), /*Implicit=*/true);
  }
}


// Build the 'getLogicValue' method for an option set.
// struct NSSomeOptionSet : RawOptionSet {
//   var value: RawType
//   func getLogicValue() -> Bool {
//     return self.value != 0
//   }
// }
static FuncDecl *makeOptionSetGetLogicValueMethod(StructDecl *optionSetDecl,
                                                  ValueDecl *valueDecl) {
  ASTContext &C = optionSetDecl->getASTContext();
  auto boolType = C.getGetBoolDecl(nullptr)->getType()
               ->castTo<AnyFunctionType>()->getResult();

  VarDecl *selfDecl = createSelfDecl(optionSetDecl, /*NotStaticMethod*/false);
  Pattern *selfParam = createTypedNamedPattern(selfDecl);
  Pattern *methodParam = TuplePattern::create(C, SourceLoc(),{},SourceLoc());
  methodParam->setType(TupleType::getEmpty(C));
  Pattern *params[] = {selfParam, methodParam};
  
  FuncDecl *getLVDecl = FuncDecl::create(
      C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
      C.getIdentifier("getLogicValue"), SourceLoc(), nullptr, Type(), params,
      params, TypeLoc::withoutLoc(boolType), optionSetDecl);
  getLVDecl->setImplicit();
  
  auto toRawArgType = TupleType::getEmpty(C);
  Type toRawType = FunctionType::get(toRawArgType, boolType);
  toRawType = FunctionType::get(optionSetDecl->getDeclaredTypeInContext(),
                                toRawType);
  getLVDecl->setType(toRawType);
  getLVDecl->setBodyResultType(boolType);
  
  selfDecl->setDeclContext(getLVDecl);
  
  auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/ true);
  auto valueRef = new (C) MemberRefExpr(selfRef, SourceLoc(),
                                        valueDecl, SourceLoc(),
                                        /*implicit*/ true);

  auto zero = new (C) IntegerLiteralExpr("0", SourceLoc(), /*implicit*/ true);

  auto neRef = getOperatorRef(C, C.Id_NotEqualsOperator);
  
  Expr *args[] = {valueRef, zero};
  auto argsTuple = new (C) TupleExpr(SourceLoc(),
                                     C.AllocateCopy(args),
                                     nullptr,
                                     SourceLoc(),
                                     /*trailingClosure*/ false,
                                     /*implicit*/ true);
  auto apply = new (C) BinaryExpr(neRef, argsTuple, /*implicit*/ true);
  auto ret = new (C) ReturnStmt(SourceLoc(), apply);

  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(ret),
                                SourceLoc(),
                                /*implicit*/ true);
  getLVDecl->setBody(body);
  
  // Add as an external definition.
  C.addedExternalDecl(getLVDecl);
  
  return getLVDecl;
}

// Build the default initializer for an option set.
// struct NSSomeOptionSet : RawOptionSet {
//   var value: RawType
//   init() {
//     return 0
//   }
// }
static ConstructorDecl *makeOptionSetDefaultConstructor(StructDecl *optionSetDecl,
                                                        ValueDecl *valueDecl) {
  ASTContext &C = optionSetDecl->getASTContext();
  auto optionSetType = optionSetDecl->getDeclaredTypeInContext();
  auto metaTy = MetatypeType::get(optionSetType, C);

  VarDecl *selfDecl = createSelfDecl(optionSetDecl, false);
  Pattern *selfPattern = createTypedNamedPattern(selfDecl);

  Pattern *methodParam = TuplePattern::create(C, SourceLoc(),{},SourceLoc());
  methodParam->setType(TupleType::getEmpty(C));

  auto *ctorDecl = new (C) ConstructorDecl(C.Id_init, optionSetDecl->getLoc(),
                                           selfPattern, methodParam,
                                           selfPattern, methodParam,
                                           nullptr, optionSetDecl);
  ctorDecl->setImplicit();
  
  auto fnTy = FunctionType::get(TupleType::getEmpty(C), optionSetType);
  auto allocFnTy = FunctionType::get(metaTy, fnTy);
  auto initFnTy = FunctionType::get(optionSetType, fnTy);
  ctorDecl->setType(allocFnTy);
  ctorDecl->setInitializerType(initFnTy);
  
  selfDecl->setDeclContext(ctorDecl);
  
  auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/true);
  auto valueRef = new (C) MemberRefExpr(selfRef, SourceLoc(),
                                        valueDecl, SourceLoc(),
                                        /*implicit*/ true);
  auto zero = new (C) IntegerLiteralExpr("0", SourceLoc(),
                                         /*implicit*/ true);
  auto assign = new (C) AssignExpr(valueRef, SourceLoc(), zero,
                                   /*implicit*/ true);
  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(assign), SourceLoc(),
                                /*implicit*/ true);
  
  ctorDecl->setBody(body);
  
  C.addedExternalDecl(ctorDecl);
  
  return ctorDecl;
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

    Decl *VisitTypedefNameDecl(const clang::TypedefNameDecl *Decl) {
      auto Name = Impl.importName(Decl->getDeclName());
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

        if (SwiftType) {
          // Note that this typedef-name is special.
          Impl.SpecialTypedefNames[Decl] = NameMapping;

          if (NameMapping == MappedTypeNameKind::DoNothing) {
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

      if (!SwiftType)
        SwiftType = Impl.importType(Decl->getUnderlyingType(),
                                    ImportTypeKind::Normal);

      if (!SwiftType)
        return nullptr;

      auto Loc = Impl.importSourceLoc(Decl->getLocation());
      return new (Impl.SwiftContext) TypeAliasDecl(
                                      Impl.importSourceLoc(Decl->getLocStart()),
                                      Name,
                                      Loc,
                                      TypeLoc::withoutLoc(SwiftType),
                                      DC);
    }

    Decl *
    VisitUnresolvedUsingTypenameDecl(const
                                     clang::UnresolvedUsingTypenameDecl *decl) {
      // Note: only occurs in templates.
      return nullptr;
    }

    /// \brief Create a constructor that initializes a struct from its members.
    ConstructorDecl *createValueConstructor(StructDecl *structDecl,
                                            ArrayRef<Decl *> members) {
      auto &context = Impl.SwiftContext;
      auto name = context.Id_init;

      // Create the 'self' declaration.
      auto selfType = structDecl->getDeclaredTypeInContext();
      auto selfMetatype = MetatypeType::get(selfType, context);
      auto selfDecl = createSelfDecl(structDecl, false);

      Pattern *selfPattern = createTypedNamedPattern(selfDecl);

      // Construct the set of parameters from the list of members.
      SmallVector<Pattern *, 4> paramPatterns;
      SmallVector<TuplePatternElt, 8> patternElts;
      SmallVector<TupleTypeElt, 8> tupleElts;
      SmallVector<VarDecl *, 8> params;
      for (auto member : members) {
        if (auto var = dyn_cast<VarDecl>(member)) {
          if (!var->hasStorage())
            continue;
          
          auto param = new (context) VarDecl(/*static*/ false, /*IsLet*/ true,
                                             SourceLoc(), var->getName(),
                                             var->getType(), structDecl);
          params.push_back(param);
          Pattern *pattern = createTypedNamedPattern(param);
          paramPatterns.push_back(pattern);
          patternElts.push_back(TuplePatternElt(pattern));
          tupleElts.push_back(TupleTypeElt(var->getType(), var->getName()));
        }
      }
      auto paramPattern = TuplePattern::create(context, SourceLoc(), patternElts,
                                               SourceLoc());
      auto paramTy = TupleType::get(tupleElts, context);
      paramPattern->setType(paramTy);

      // Create the constructor
      auto constructor =
        new (context) ConstructorDecl(name, structDecl->getLoc(),
                                      selfPattern, paramPattern,
                                      selfPattern, paramPattern,
                                      nullptr, structDecl);

      // Set the constructor's type.
      auto fnTy = FunctionType::get(paramTy, selfType);
      auto allocFnTy = FunctionType::get(selfMetatype, fnTy);
      auto initFnTy = FunctionType::get(selfType, fnTy);
      constructor->setType(allocFnTy);
      constructor->setInitializerType(initFnTy);

      // Assign all of the member variables appropriately.
      SmallVector<ASTNode, 4> stmts;
      unsigned paramIdx = 0;
      for (auto member : members) {
        auto var = dyn_cast<VarDecl>(member);
        if (!var || !var->hasStorage())
          continue;

        // Construct left-hand side.
        Expr *lhs = new (context) DeclRefExpr(selfDecl, SourceLoc(),
                                              /*Implicit=*/true);
        lhs = new (context) MemberRefExpr(lhs, SourceLoc(), var, SourceLoc(),
                                          /*Implicit=*/true);

        // Construct right-hand side.
        auto param = params[paramIdx++];
        auto rhs = new (context) DeclRefExpr(param, SourceLoc(),
                                             /*Implicit=*/true);

        // Add assignment.
        stmts.push_back(new (context) AssignExpr(lhs, SourceLoc(), rhs,
                                                 /*Implicit=*/true));
      }

      // Create the function body.
      auto body = BraceStmt::create(context, SourceLoc(), stmts, SourceLoc());
      constructor->setBody(body);

      // Add this as an external definition.
      Impl.registerExternalDecl(constructor);

      // We're done.
      return constructor;
    }
    
    /// Get the Swift name for an enum constant.
    Identifier getEnumConstantName(const clang::EnumConstantDecl *decl,
                                   const clang::EnumDecl *clangEnum) {
      // Look up the common name prefix for this enum's constants.
      StringRef enumPrefix = "";
      auto foundPrefix = Impl.EnumConstantNamePrefixes.find(clangEnum);
      if (foundPrefix != Impl.EnumConstantNamePrefixes.end()) {
        enumPrefix = foundPrefix->second;
      }
      
      return Impl.importName(decl->getDeclName(), /*suffix*/ "", enumPrefix);
    }
    
    /// Determine the common prefix to remove from the element names of an
    /// enum. We'll elide this prefix from then names in
    /// the Swift interface because Swift enum cases are naturally namespaced
    /// by the enum type.
    void computeEnumCommonWordPrefix(const clang::EnumDecl *decl,
                                     Identifier enumName) {
      auto enumerators = decl->enumerators();
      if (enumerators.begin() == enumerators.end())
        return;
      StringRef commonPrefix = enumName.str();
      for (auto nextValue : enumerators)
        commonPrefix = getCommonWordPrefix(commonPrefix, nextValue->getName());
      Impl.EnumConstantNamePrefixes.insert({decl, commonPrefix});
    }
    
    /// Import an NS_ENUM constant as a case of a Swift enum.
    Decl *importEnumCase(const clang::EnumConstantDecl *decl,
                         const clang::EnumDecl *clangEnum,
                         EnumDecl *theEnum) {
      auto &context = Impl.SwiftContext;
      auto name = getEnumConstantName(decl, clangEnum);
      if (name.empty())
        return nullptr;
      
      // Use the constant's underlying value as its raw value in Swift.
      bool negative = false;
      llvm::APSInt rawValue = decl->getInitVal();
      
      // Check that we didn't already import an enum constant for this enum
      // with the same value. Swift enums don't currently support aliases.
      if (Impl.EnumConstantValues.count({clangEnum, rawValue}))
        return nullptr;
      
      Impl.EnumConstantValues.insert({clangEnum, rawValue});
      
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
        = new (context) EnumElementDecl(SourceLoc(),
                                        name, TypeLoc(),
                                        SourceLoc(), rawValueExpr,
                                        theEnum);

      // Give the enum element the appropriate type.
      auto argTy = MetatypeType::get(theEnum->getDeclaredType(), context);
      element->overwriteType(FunctionType::get(argTy,
                                               theEnum->getDeclaredType()));
      element->setClangNode(decl);
      return element;
    }
    
    /// Import an NS_OPTIONS constant as a static property of a Swift struct.
    Decl *importOptionConstant(const clang::EnumConstantDecl *decl,
                               const clang::EnumDecl *clangEnum,
                               StructDecl *theStruct) {
      auto name = getEnumConstantName(decl, clangEnum);
      if (name.empty())
        return nullptr;
      
      // Create the constant.
      auto element = Impl.createConstant(name, theStruct,
                                 theStruct->getDeclaredTypeInContext(),
                                 clang::APValue(decl->getInitVal()),
                                 ConstantConvertKind::Construction,
                                 /*isStatic*/ true);
      element->setClangNode(decl);
      return element;
    }

    Decl *VisitEnumDecl(const clang::EnumDecl *decl) {
      decl = decl->getDefinition();
      if (!decl) {
        forwardDeclaration = true;
        return nullptr;
      }
      
      Identifier name;
      if (decl->getDeclName())
        name = Impl.importName(decl->getDeclName());
      else if (decl->getTypedefNameForAnonDecl())
        name =Impl.importName(decl->getTypedefNameForAnonDecl()->getDeclName());

      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;
      
      ASTContext &cxt = Impl.SwiftContext;
      
      // Create the enum declaration and record it.
      Decl *result;
      auto enumKind = Impl.classifyEnum(decl);
      switch (enumKind) {
      case EnumKind::Constants: {
        // There is no declaration. Rather, the type is mapped to the
        // underlying type.
        return nullptr;
      }

      case EnumKind::Unknown: {
        auto Loc = Impl.importSourceLoc(decl->getLocation());
        auto structDecl = new (Impl.SwiftContext)
          StructDecl(Loc, name, Loc, { }, nullptr, dc);
        structDecl->computeType();

        // Compute the underlying type of the enumeration.
        auto underlyingType = Impl.importType(decl->getIntegerType(),
                                              ImportTypeKind::Enum);
        if (!underlyingType)
          return nullptr;

        // Create a variable to store the underlying value.
        auto varName = Impl.SwiftContext.getIdentifier("value");
        auto var = new (Impl.SwiftContext) VarDecl(/*static*/ false,
                                                   /*IsLet*/ false,
                                                   SourceLoc(), varName,
                                                   underlyingType,
                                                   structDecl);

        // Create a pattern binding to describe the variable.
        Pattern *varPattern = createTypedNamedPattern(var);

        auto patternBinding = new (Impl.SwiftContext)
            PatternBindingDecl(SourceLoc(), StaticSpellingKind::None,
                               SourceLoc(), varPattern, nullptr,
                               /*conditional*/ false, structDecl);

        // Create a constructor to initialize that value from a value of the
        // underlying type.
        Decl *varDecl = var;
        auto constructor = createValueConstructor(structDecl, varDecl);

        // Set the members of the struct.
        Decl *members[3] = { constructor, patternBinding, var };
        structDecl->setMembers(
          Impl.SwiftContext.AllocateCopy(ArrayRef<Decl *>(members, 3)),
          SourceRange());

        result = structDecl;
        break;
      }

      case EnumKind::Enum: {
        // Compute the underlying type.
        auto underlyingType = Impl.importType(decl->getIntegerType(),
                                              ImportTypeKind::Enum);
        if (!underlyingType)
          return nullptr;
        
        auto enumDecl = new (Impl.SwiftContext)
          EnumDecl(Impl.importSourceLoc(decl->getLocStart()),
                   name, Impl.importSourceLoc(decl->getLocation()),
                   {}, nullptr, dc);
        enumDecl->computeType();
        
        // Set up the C underlying type as its Swift raw type.
        enumDecl->setRawType(underlyingType);
        
        // Add delayed protocol declarations to the enum declaration.
        DelayedProtocolDecl delayedProtocols[] = {
          [&]() {return cxt.getProtocol(KnownProtocolKind::RawRepresentable);},
          [&]() {return cxt.getProtocol(KnownProtocolKind::Equatable);},
          [&]() {return cxt.getProtocol(KnownProtocolKind::Hashable);}
        };
        auto delayedProtoList = Impl.SwiftContext.AllocateCopy(
                                                      delayedProtocols);
        enumDecl->setDelayedProtocolDecls(delayedProtoList);
        
        result = enumDecl;
        computeEnumCommonWordPrefix(decl, name);
        
        break;
      }
          
      case EnumKind::Options: {
        // Compute the underlying type.
        auto underlyingType = Impl.importType(decl->getIntegerType(),
                                              ImportTypeKind::Enum);
        if (!underlyingType)
          return nullptr;

        auto Loc = Impl.importSourceLoc(decl->getLocation());

        // Create a struct with the underlying type as a field.
        auto structDecl = new (Impl.SwiftContext)
          StructDecl(Loc, name, Loc, { }, nullptr, dc);
        structDecl->computeType();
        
        // Create a field to store the underlying value.
        auto varName = Impl.SwiftContext.getIdentifier("value");
        auto var = new (Impl.SwiftContext) VarDecl(/*static*/ false,
                                                   /*IsLet*/ false,
                                                   SourceLoc(), varName,
                                                   underlyingType,
                                                   structDecl);

        // Create a pattern binding to describe the variable.
        Pattern *varPattern = createTypedNamedPattern(var);

        auto patternBinding = new (Impl.SwiftContext)
            PatternBindingDecl(SourceLoc(), StaticSpellingKind::None,
                               SourceLoc(), varPattern, nullptr,
                               /*conditional*/ false, structDecl);

        // Create a default initializer to get the value with no options set.
        auto defaultConstructor = makeOptionSetDefaultConstructor(structDecl,
                                                                  var);
        
        // Create a constructor to initialize that value from a value of the
        // underlying type.
        Decl *varDecl = var;
        auto valueConstructor = createValueConstructor(structDecl, varDecl);

        // Build a delayed RawOptionSet conformance for the type.
        DelayedProtocolDecl delayedProtocols[] = {
          [&]() {return cxt.getProtocol(KnownProtocolKind::RawOptionSet);}
        };
        structDecl->setDelayedProtocolDecls(
            Impl.SwiftContext.AllocateCopy(delayedProtocols));

        // Add delayed implicit members to the type.
        DelayedDecl delayedMembers[] = {
          [=](){return makeOptionSetFactoryMethod(structDecl, var,
                                         OptionSetFactoryMethod::FromMask);},
          [=](){return makeOptionSetFactoryMethod(structDecl, var,
                                         OptionSetFactoryMethod::FromRaw);},
          [=](){return makeOptionSetToRawMethod(structDecl, var);},
          [=](){return makeOptionSetGetLogicValueMethod(structDecl, var);}
        };
        
        structDecl->setDelayedMemberDecls(Impl.SwiftContext.AllocateCopy(
                                                              delayedMembers));
        
        // Set the members of the struct.
        Decl *members[] = {
          defaultConstructor,
          valueConstructor,
          patternBinding,
          var
        };
        structDecl->setMembers(
          Impl.SwiftContext.AllocateCopy(members),
          SourceRange());

        result = structDecl;
        computeEnumCommonWordPrefix(decl, name);

        break;
      }
      }
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
      result->setClangNode(decl);

      // Import each of the enumerators.
      
      SmallVector<Decl *, 4> enumeratorDecls;
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
          enumeratorDecl = importOptionConstant(*ec, decl,
                                                cast<StructDecl>(result));
          break;
        case EnumKind::Enum:
          enumeratorDecl = importEnumCase(*ec, decl, cast<EnumDecl>(result));
          break;
        }
        if (!enumeratorDecl)
          continue;

        enumeratorDecls.push_back(enumeratorDecl);
      }

      // FIXME: Source range isn't totally accurate because Clang lacks the
      // location of the '{'.
      if (addEnumeratorsAsMembers) {
        auto nomResult = cast<NominalTypeDecl>(result);
        enumeratorDecls.append(nomResult->getMembers().begin(),
                               nomResult->getMembers().end());
        nomResult->setMembers(Impl.SwiftContext.AllocateCopy(enumeratorDecls),
                                Impl.importSourceRange(clang::SourceRange(
                                                       decl->getLocation(),
                                                       decl->getRBraceLoc())));
      }
      
      // Add the type decl to ExternalDefinitions so that we can type-check
      // raw values and IRGen can emit metadata for it.
      // FIXME: There might be better ways to do this.
      Impl.registerExternalDecl(result);
      return result;
    }

    Decl *VisitRecordDecl(const clang::RecordDecl *decl) {
      // FIXME: Skip unions for now. We can't properly map them to Swift unions,
      // because they aren't discriminated in any way. We could map them to
      // structs, but that would make them very, very unsafe to use.
      if (decl->isUnion())
        return nullptr;

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

      Identifier name;
      if (decl->getDeclName())
        name = Impl.importName(decl->getDeclName());
      else if (decl->getTypedefNameForAnonDecl())
        name =Impl.importName(decl->getTypedefNameForAnonDecl()->getDeclName());

      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      // We don't import structs with bitfields because we can not layout them
      // correctly in IRGen.
      for (auto m = decl->decls_begin(), mEnd = decl->decls_end();
           m != mEnd; ++m) {
        if (auto FD = dyn_cast<clang::FieldDecl>(*m))
          if (FD->isBitField())
            return nullptr;
      }

      // Create the struct declaration and record it.
      auto result = new (Impl.SwiftContext)
                      StructDecl(Impl.importSourceLoc(decl->getLocStart()),
                                 name,
                                 Impl.importSourceLoc(decl->getLocation()),
                                 { }, nullptr, dc);
      result->computeType();
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
      result->setClangNode(decl);

      // FIXME: Figure out what to do with superclasses in C++. One possible
      // solution would be to turn them into members and add conversion
      // functions.

      // Import each of the members.
      SmallVector<Decl *, 4> members;
      for (auto m = decl->decls_begin(), mEnd = decl->decls_end();
           m != mEnd; ++m) {
        auto nd = dyn_cast<clang::NamedDecl>(*m);
        if (!nd)
          continue;

        // Skip anonymous structs or unions; they'll be dealt with via the
        // IndirectFieldDecls.
        if (auto field = dyn_cast<clang::FieldDecl>(nd))
          if (field->isAnonymousStructOrUnion())
            continue;

        auto member = Impl.importDecl(nd);
        if (!member)
          continue;

        members.push_back(member);
      }

      // FIXME: Source range isn't totally accurate because Clang lacks the
      // location of the '{'.
      result->setMembers(Impl.SwiftContext.AllocateCopy(members),
                         Impl.importSourceRange(clang::SourceRange(
                                                  decl->getLocation(),
                                                  decl->getRBraceLoc())));
      
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
      
      auto name = getEnumConstantName(decl, clangEnum);
      if (name.empty())
        return nullptr;

      switch (Impl.classifyEnum(clangEnum)) {
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
                                    ImportTypeKind::Normal);
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
                                          /*static*/ false);
        Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
        return result;
      }

      case EnumKind::Unknown: {
        // The enumeration was mapped to a struct containining the integral
        // type. Create a constant with that struct type.

        auto dc = Impl.importDeclContextOf(clangEnum);
        if (!dc)
          return nullptr;

        // Import the enumeration type.
        auto enumType = Impl.importType(
                          Impl.getClangASTContext().getTagDeclType(clangEnum),
                          ImportTypeKind::Normal);
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
                                          /*static*/ false);
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

      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto type = Impl.importType(decl->getType(), ImportTypeKind::Normal);
      if (!type)
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      // Map this indirect field to a Swift variable.
      return new (Impl.SwiftContext)
               VarDecl(/*static*/ false, /*IsLet*/ false,
                       Impl.importSourceLoc(decl->getLocStart()),
                       name, type, dc);
    }

    Decl *VisitFunctionDecl(const clang::FunctionDecl *decl) {
      decl = decl->getMostRecentDecl();

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      // Import the function type. If we have parameters, make sure their names
      // get into the resulting function type.
      SmallVector<Pattern *, 4> argPatterns;
      SmallVector<Pattern *, 4> bodyPatterns;
      Type type = Impl.importFunctionType(decl->getReturnType(),
                                          { decl->param_begin(),
                                            decl->param_size() },
                                          decl->isVariadic(),
                                          decl->isNoReturn(),
                                          argPatterns, bodyPatterns);
      if (!type)
        return nullptr;

      auto resultTy = type->castTo<FunctionType>()->getResult();
      auto loc = Impl.importSourceLoc(decl->getLocation());

      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      // FIXME: Poor location info.
      auto nameLoc = Impl.importSourceLoc(decl->getLocation());
      auto result = FuncDecl::create(
          Impl.SwiftContext, SourceLoc(), StaticSpellingKind::None, loc,
          name, nameLoc,
          /*GenericParams=*/nullptr, type, argPatterns, bodyPatterns,
          TypeLoc::withoutLoc(resultTy), dc);

      // Keep track of inline function bodies so that we can generate
      // IR from them using Clang's IR generator.
      if ((decl->isInlined() || decl->hasAttr<clang::AlwaysInlineAttr>())
          && decl->getBody()) {
        // FIXME: Total hack to force instantiation of inline
        //        functions into the module rather than going through
        //        Clang's CodeGenModule::Release(), which will emit
        //        deferred decls that have been referenced, since
        //        Release() does many things including emitting stuff
        //        that along with what Swift emits results in broken
        //        modules.
        auto *attr = clang::UsedAttr::CreateImplicit(decl->getASTContext());
        const_cast<clang::FunctionDecl *>(decl)->addAttr(attr);

        result->setClangNode(decl);
        Impl.registerExternalDecl(result);
      }

      result->setBodyResultType(resultTy);
      return result;
    }

    Decl *VisitCXXMethodDecl(const clang::CXXMethodDecl *decl) {
      // FIXME: Import C++ member functions as methods.
      return nullptr;
    }

    Decl *VisitFieldDecl(const clang::FieldDecl *decl) {
      // We don't import bitfields because we can not layout them correctly in
      // IRGen.
      if (decl->isBitField())
        return nullptr;

      // Fields are imported as variables.
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto type = Impl.importType(decl->getType(), ImportTypeKind::Normal);
      if (!type)
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      auto result =
        new (Impl.SwiftContext) VarDecl(/*static*/ false, /*IsLet*/ false,
                              Impl.importSourceLoc(decl->getLocation()),
                              name, type, dc);

      // Handle attributes.
      if (decl->hasAttr<clang::IBOutletAttr>())
        result->getMutableAttrs().setAttr(AK_IBOutlet, SourceLoc());
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
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto type = Impl.importType(decl->getType(), ImportTypeKind::Normal);
      if (!type)
        return nullptr;

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      // FIXME: Should 'const' vardecl's be imported as 'let' decls?
      return new (Impl.SwiftContext)
               VarDecl(/*static*/ false,
                       /*IsLet*/ false,
                       Impl.importSourceLoc(decl->getLocation()),
                       name, type, dc);
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

    Decl *VisitObjCMethodDecl(const clang::ObjCMethodDecl *decl,
                              DeclContext *dc, bool forceClassMethod = false) {
      bool firstParamIncludedInName;
      DeclName name = Impl.importName(decl->getSelector(),
                                      &firstParamIncludedInName);
      if (!name)
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
      SmallVector<Pattern *, 4> argPatterns;
      SmallVector<Pattern *, 4> bodyPatterns;
      auto selfVar =
        createSelfDecl(dc, decl->isClassMethod() || forceClassMethod);
      Pattern *selfPat = createTypedNamedPattern(selfVar);
      argPatterns.push_back(selfPat);
      bodyPatterns.push_back(selfPat);
      bool hasSelectorStyleSignature;

      SpecialMethodKind kind = SpecialMethodKind::Regular;
      if (isNSDictionaryMethod(decl, Impl.objectForKeyedSubscript))
        kind = SpecialMethodKind::NSDictionarySubscriptGetter;

      // Import the type that this method will have.
      auto type = Impl.importFunctionType(decl->getReturnType(),
                                          { decl->param_begin(),
                                            decl->param_size() },
                                          decl->isVariadic(),
                                          decl->hasAttr<clang::NoReturnAttr>(),
                                          argPatterns,
                                          bodyPatterns,
                                          &hasSelectorStyleSignature,
                                          decl->getSelector(),
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
          SourceLoc(), name, SourceLoc(), /*GenericParams=*/nullptr, Type(),
          argPatterns, bodyPatterns, TypeLoc(), dc);
      result->setFirstParamIncludedInName(firstParamIncludedInName);

      auto resultTy = type->castTo<FunctionType>()->getResult();
      Type interfaceType;

      // If the method has a related result type that is representable
      // in Swift as DynamicSelf, do so.
      if (decl->hasRelatedResultType()) {
        result->setDynamicSelf(true);
        resultTy = result->getDynamicSelf();
        assert(resultTy && "failed to get dynamic self");
        if (Impl.EnableOptional)
          resultTy = UncheckedOptionalType::get(resultTy);
        
        // Update the method type with the new result type.
        auto methodTy = type->castTo<FunctionType>();
        type = FunctionType::get(methodTy->getInput(), resultTy, 
                                 methodTy->getExtInfo());

        // Create the interface type of the method.
        interfaceType = FunctionType::get(methodTy->getInput(),
                                          result->getDynamicSelfInterface(),
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

      if (hasSelectorStyleSignature)
        result->setHasSelectorStyleSignature();

      // Optional methods in protocols.
      if (decl->getImplementationControl() == clang::ObjCMethodDecl::Optional &&
          isa<ProtocolDecl>(dc))
        result->getMutableAttrs().setAttr(AK_optional, SourceLoc());

      // Mark this as an Objective-C method.
      result->setIsObjC(true);

      // Mark class methods as static.
      if (decl->isClassMethod() || forceClassMethod)
        result->setStatic();

      // If this method overrides another method, mark it as such.
      recordObjCMethodOverride(result, decl);

      // Handle attributes.
      if (decl->hasAttr<clang::IBActionAttr>())
        result->getMutableAttrs().setAttr(AK_IBAction, SourceLoc());

      // Check whether there's some special method to import.
      result->setClangNode(decl);
      if (!forceClassMethod) {
        if (dc == Impl.importDeclContextOf(decl) &&
            !Impl.ImportedDecls[decl->getCanonicalDecl()])
          Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

        if (decl->getMethodFamily() != clang::OMF_init ||
            !isReallyInitMethod(decl)) {
          importSpecialMethod(result, dc);
        }
      }
      return result;
    }

  private:
    /// Check whether the given name starts with the given word.
    static bool startsWithWord(StringRef name, StringRef word) {
      if (name.size() < word.size()) return false;
      return ((name.size() == word.size() || !islower(name[word.size()])) &&
              name.startswith(word));
    }

    /// Determine whether the given Objective-C method, which Clang classifies
    /// as an init method, is considered an init method in Swift.
    static bool isReallyInitMethod(const clang::ObjCMethodDecl *method) {
      if (!method->isInstanceMethod())
        return false;

      auto selector = method->getSelector();
      auto first = selector.getIdentifierInfoForSlot(0);
      if (!first) return false;

      return startsWithWord(first->getName(), "init");
    }

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
        // An init instance method can be a constructor.
        if (isReallyInitMethod(objcMethod))
          return importConstructor(decl, objcMethod, dc, /*implicit=*/false,
                                   /*isConvenienceInit=*/false);
        return nullptr;

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

    /// Record the function override by the given Swift method (along with
    /// it's Objective-C counterpart).
    void recordObjCMethodOverride(AbstractFunctionDecl *swiftMethod,
                                  const clang::ObjCMethodDecl *objcMethod) {
      // FIXME: Rework this using Swift lookup and semantics, to
      // properly cope with mirrored members.

      // If this function overrides another function, mark it as such.
      auto classTy = swiftMethod->getExtensionType()->getAs<ClassType>();
      if (!classTy)
        return;

      auto superTy = classTy->getSuperclass(nullptr);
      if (!superTy)
        return;

      // Dig out the Objective-C superclass.
      auto superDecl = superTy->getAnyNominal();
      auto superObjCClass = dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                              superDecl->getClangDecl());
      if (!superObjCClass)
        return;

      // Look for an overridden method.
      auto superObjCMethod = superObjCClass->lookupMethod(
                               objcMethod->getSelector(),
                               objcMethod->isInstanceMethod());
      if (!superObjCMethod)
        return;

      // We found a method that we've overridden. Import it.
      AbstractFunctionDecl *superMethod = nullptr;
      if (isa<clang::ObjCProtocolDecl>(superObjCMethod->getDeclContext())) {
        superMethod = cast_or_null<AbstractFunctionDecl>(
                        Impl.importMirroredDecl(superObjCMethod, superDecl));
      } else {
        superMethod = cast_or_null<AbstractFunctionDecl>(
                        Impl.importDecl(superObjCMethod));
      }
      if (!superMethod)
        return;

      assert(swiftMethod->getDeclContext() != superMethod->getDeclContext() &&
             "can not override method in the same DeclContext");

      // Set function override.
      // FIXME: Proper type checking here!
      if (auto swiftFunc = dyn_cast<FuncDecl>(swiftMethod)) {
        swiftFunc->setOverriddenDecl(cast<FuncDecl>(superMethod));
        return;
      }

      // Set constructor override.
      auto swiftCtor = cast<ConstructorDecl>(swiftMethod);

      // If the superclass lookup found a method, not a constructor, try to
      // map to the constructor.
      auto superCtor = dyn_cast<ConstructorDecl>(superMethod);
      if (!superCtor) {
        superCtor = dyn_cast_or_null<ConstructorDecl>(
                      importSpecialMethod(superMethod,
                                          superMethod->getDeclContext()));
        if (!superCtor)
          return;
      }
      swiftCtor->setOverriddenDecl(superCtor);
    }

    /// \brief Given an imported method, try to import it as a constructor.
    ///
    /// Objective-C methods in the 'init' family are imported as
    /// constructors in Swift, enabling object construction syntax, e.g.,
    ///
    /// \code
    /// // in objc: [[NSArray alloc] initWithCapacity:1024]
    /// NSArray(withCapacity: 1024)
    /// \endcode
    ConstructorDecl *importConstructor(Decl *decl,
                                       const clang::ObjCMethodDecl *objcMethod,
                                       DeclContext *dc,
                                       bool implicit,
                                       bool isConvenienceInit) {
      // Figure out the type of the container.
      auto containerTy = dc->getDeclaredTypeOfContext();
      assert(containerTy && "Method in non-type context?");

      // Only methods in the 'init' family can become constructors.
      FuncDecl *alloc = nullptr;

      assert(objcMethod->getMethodFamily() == clang::OMF_init &&
             "Not an init method");
      assert(isReallyInitMethod(objcMethod) && "Not a real init method");

      // Check whether we've already created the constructor.
      FuncDecl *init = cast<FuncDecl>(decl);
      auto known = Impl.Constructors.find({init, dc});
      if (known != Impl.Constructors.end())
        return known->second;

      // Make sure we have a usable 'alloc' method. Otherwise, we can't
      // build this constructor anyway.
      const clang::ObjCInterfaceDecl *interface;
      if (isa<clang::ObjCProtocolDecl>(objcMethod->getDeclContext())) {
        // For a protocol method, look into the context in which we'll be
        // mirroring the method to find 'alloc'.
        // FIXME: Part of the mirroring hack.
        auto classDecl = containerTy->getClassOrBoundGenericClass();
        if (!classDecl)
          return nullptr;

        interface = dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                      classDecl->getClangDecl());
      } else {
        // For non-protocol methods, just look for the interface.
        interface = objcMethod->getClassInterface();
      }

      // If we couldn't find a class, we're done.
      if (!interface)
        return nullptr;

      // Form the Objective-C selector for alloc.
      auto &clangContext = Impl.getClangASTContext();
      auto allocId = &clangContext.Idents.get("alloc");
      auto allocSel = clangContext.Selectors.getNullarySelector(allocId);

      // Find the 'alloc' class method.
      auto allocMethod = interface->lookupClassMethod(allocSel);
      if (!allocMethod)
        return nullptr;

      // Import the 'alloc' class method.
      alloc = cast_or_null<FuncDecl>(Impl.importDecl(allocMethod));
      if (!alloc)
        return nullptr;

      auto loc = decl->getLoc();
      auto name = Impl.SwiftContext.Id_init;

      // Add the implicit 'self' parameter patterns.
      SmallVector<Pattern *, 4> argPatterns;
      SmallVector<Pattern *, 4> bodyPatterns;
      auto selfTy = getSelfTypeForContext(dc);
      auto selfMetaVar = createSelfDecl(dc, true);
      Pattern *selfPat = createTypedNamedPattern(selfMetaVar);
      argPatterns.push_back(selfPat);
      bodyPatterns.push_back(selfPat);
      bool hasSelectorStyleSignature;

      // Import the type that this method will have.
      auto type = Impl.importFunctionType(objcMethod->getReturnType(),
                                          { objcMethod->param_begin(),
                                            objcMethod->param_size() },
                                          objcMethod->isVariadic(),
                                  objcMethod->hasAttr<clang::NoReturnAttr>(),
                                          argPatterns,
                                          bodyPatterns,
                                          &hasSelectorStyleSignature,
                                          objcMethod->getSelector(),
                                          SpecialMethodKind::Constructor);
      assert(type && "Type has already been successfully converted?");

      // Check whether we've already created the constructor.
      known = Impl.Constructors.find({init, dc});
      if (known != Impl.Constructors.end())
        return known->second;

      // A constructor returns an object of the type, not 'id'.
      // This is effectively implementing related-result-type semantics.
      // FIXME: Perhaps actually check whether the routine has a related result
      // type?
      type = FunctionType::get(type->castTo<FunctionType>()->getInput(),
                               selfTy);

      // Add the 'self' parameter to the function types.
      Type allocType = FunctionType::get(selfMetaVar->getType(), type);
      Type initType = FunctionType::get(selfTy, type);

      VarDecl *selfVar = createSelfDecl(dc, false);
      selfPat = createTypedNamedPattern(selfVar);

      // Create the actual constructor.
      auto result = new (Impl.SwiftContext)
         ConstructorDecl(name, loc, selfPat, argPatterns.back(),
                         selfPat, bodyPatterns.back(), /*GenericParams=*/0, dc);
      result->setType(allocType);
      result->setInitializerType(initType);
      result->setIsObjC(true);
      result->setClangNode(objcMethod);
      
      if (hasSelectorStyleSignature)
        result->setHasSelectorStyleSignature();
      if (implicit)
        result->setImplicit();

      // If the owning Objective-C class has designated initializers and this
      // is not one of them, treat it as a convenience initializer.
      if (isConvenienceInit ||
          (interface && interface->hasDesignatedInitializers() &&
           !objcMethod->hasAttr<clang::ObjCDesignatedInitializerAttr>())) {
        result->setCompleteObjectInit(true);
      }

      // Record the constructor for future re-use.
      Impl.Constructors[{init, dc}] = result;

      // If this constructor overrides another constructor, mark it as such.
      recordObjCMethodOverride(result, objcMethod);

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
        pattern = tuple->getFields()[0].getPattern()
                    ->getSemanticsProvidingPattern();
      }

      return cast<NamedPattern>(pattern)->getDecl();
    }

    /// Retrieves the type and interface type for a protocol method given
    /// the computed type of that method.
    std::pair<Type, Type> getProtocolMethodType(ProtocolDecl *proto,
                                                AnyFunctionType *fnType) {
      Type type = PolymorphicFunctionType::get(fnType->getInput(),
                                               fnType->getResult(),
                                               proto->getGenericParams());

      // Figure out the curried 'self' type for the interface type. It's always
      // either the generic parameter type 'Self' or a metatype thereof.
      auto interfaceInputTy = proto->getSelf()->getDeclaredType();
      auto inputTy = fnType->getInput();
      if (auto tupleTy = inputTy->getAs<TupleType>()) {
        if (tupleTy->getNumElements() == 1)
          inputTy = tupleTy->getElementType(0);
      }
      if (inputTy->is<MetatypeType>())
        interfaceInputTy = MetatypeType::get(interfaceInputTy,
                                             Impl.SwiftContext);

      auto interfaceResultTy = fnType->getResult().transform(
        [&](Type type) -> Type {
          if (type->is<DynamicSelfType>()) {
            return DynamicSelfType::get(proto->getSelf()->getDeclaredType(),
                                        Impl.SwiftContext);
          }

          return type;
        });

      Type interfaceType = GenericFunctionType::get(
                             proto->getGenericSignature(),
                             interfaceInputTy,
                             interfaceResultTy,
                             AnyFunctionType::ExtInfo());
      return { type, interfaceType };
    }

    /// \brief Build a thunk for an Objective-C getter.
    ///
    /// \param getter The Objective-C getter method.
    ///
    /// \param dc The declaration context into which the thunk will be added.
    ///
    /// \param indices If non-null, the indices for a subscript getter. Null
    /// indicates that we're generating a getter thunk for a property getter.
    ///
    /// \returns The getter thunk.
    FuncDecl *buildGetterThunk(FuncDecl *getter, DeclContext *dc,
                               Pattern *indices) {
      auto &context = Impl.SwiftContext;
      auto loc = getter->getLoc();

      // Figure out the element type, by looking through 'self' and the normal
      // parameters.
      auto elementTy
        = getter->getType()->castTo<AnyFunctionType>()->getResult()
            ->castTo<AnyFunctionType>()->getResult();

      // Form the argument patterns.
      SmallVector<Pattern *, 3> getterArgs;

      // 'self'
      getterArgs.push_back(createTypedNamedPattern(createSelfDecl(dc, false)));

      // index, for subscript operations.
      if (indices) {
        // Clone the indices for the thunk.
        indices = indices->clone(context);
        auto pat = TuplePattern::create(context, loc, TuplePatternElt(indices),
                                        loc);
        pat->setType(TupleType::get(TupleTypeElt(indices->getType(),
                                                 indices->getBoundName()),
                                    context));
        getterArgs.push_back(pat);
      } else {
        // Otherwise, an empty tuple
        getterArgs.push_back(TuplePattern::create(context, loc, { }, loc));
        getterArgs.back()->setType(TupleType::getEmpty(context));
      }

      // Form the type of the getter.
      auto getterType = elementTy;
      for (auto it = getterArgs.rbegin(), itEnd = getterArgs.rend();
           it != itEnd; ++it) {
        getterType = FunctionType::get((*it)->getType(), getterType);
      }

      // If we're in a protocol, the getter thunk will be polymorphic.
      Type interfaceType;
      if (auto proto = dyn_cast<ProtocolDecl>(dc)) {
        std::tie(getterType, interfaceType)
          = getProtocolMethodType(proto, getterType->castTo<AnyFunctionType>());
      }

      // Create the getter thunk.
      auto thunk = FuncDecl::create(
          context, SourceLoc(), StaticSpellingKind::None, getter->getLoc(),
          Identifier(), SourceLoc(), nullptr, getterType, getterArgs,
          getterArgs, TypeLoc::withoutLoc(elementTy), dc);
      thunk->setBodyResultType(elementTy);
      thunk->setInterfaceType(interfaceType);

      thunk->setIsObjC(true);
      return thunk;
    }

    /// \brief Build a thunk for an Objective-C setter.
    ///
    /// \param setter The Objective-C setter method.
    ///
    /// \param dc The declaration context into which the thunk will be added.
    ///
    /// \param indices If non-null, the indices for a subscript setter. Null
    /// indicates that we're generating a setter thunk for a property setter.
    ///
    /// \returns The getter thunk.
    FuncDecl *buildSetterThunk(FuncDecl *setter, DeclContext *dc,
                               Pattern *indices) {
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
      //
      // Property setters are similar, but don't have indices.

      // Form the argument patterns.
      SmallVector<Pattern *, 2> setterArgs;

      // 'self'
      setterArgs.push_back(createTypedNamedPattern(createSelfDecl(dc, false)));

      
      SmallVector<TuplePatternElt, 2> ValueElts;
      SmallVector<TupleTypeElt, 2> ValueEltTys;
      
      auto valuePattern = tuple->getFields()[0].getPattern()->clone(context);
      ValueElts.push_back(TuplePatternElt(valuePattern));
      ValueEltTys.push_back(TupleTypeElt(valuePattern->getType(),
                                         valuePattern->getBoundName()));
      
      // index, for subscript operations.
      if (indices) {
        // Clone the indices for the thunk.
        indices = indices->clone(context);
        ValueElts.push_back(TuplePatternElt(indices));
        ValueEltTys.push_back(TupleTypeElt(indices->getType(),
                                           indices->getBoundName()));
      }
      
      // value
      setterArgs.push_back(TuplePattern::create(context, loc, ValueElts, loc));
      setterArgs.back()->setType(TupleType::get(ValueEltTys, context));

      // Form the type of the setter.
      Type setterType = TupleType::getEmpty(context);
      for (auto it = setterArgs.rbegin(), itEnd = setterArgs.rend();
           it != itEnd; ++it) {
        setterType = FunctionType::get((*it)->getType(), setterType);
      }

      // If we're in a protocol, the setter thunk will be polymorphic.
      Type interfaceType;
      if (auto proto = dyn_cast<ProtocolDecl>(dc)) {
        std::tie(setterType, interfaceType)
          = getProtocolMethodType(proto, setterType->castTo<AnyFunctionType>());
      }

      // Create the setter thunk.
      auto thunk = FuncDecl::create(
          context, SourceLoc(), StaticSpellingKind::None, setter->getLoc(),
          Identifier(), SourceLoc(),
          nullptr, setterType, setterArgs, setterArgs,
          TypeLoc::withoutLoc(TupleType::getEmpty(context)), dc);
      thunk->setBodyResultType(TupleType::getEmpty(context));
      thunk->setInterfaceType(interfaceType);

      thunk->setIsObjC(true);
      return thunk;
    }
    
    /// \brief Given either the getter or setter for a subscript operation,
    /// create the Swift subscript declaration.
    SubscriptDecl *importSubscript(Decl *decl,
                                   const clang::ObjCMethodDecl *objcMethod,
                                   DeclContext *dc) {
      assert(objcMethod->isInstanceMethod() && "Caller must filter");

      const clang::ObjCInterfaceDecl *interface = nullptr;
      const clang::ObjCProtocolDecl *protocol =
          dyn_cast<clang::ObjCProtocolDecl>(objcMethod->getDeclContext());
      if (!protocol)
        interface = objcMethod->getClassInterface();
      auto lookupInstanceMethod = [&](clang::Selector Sel) ->
          clang::ObjCMethodDecl * {
        if (interface)
          return interface->lookupInstanceMethod(Sel);
        else
          return protocol->lookupInstanceMethod(Sel);
      };

      bool optionalMethods = true;
      FuncDecl *getter = nullptr, *setter = nullptr;
      if (objcMethod->getSelector() == Impl.objectAtIndexedSubscript) {
        getter = cast<FuncDecl>(decl);

        // Find the setter
        if (auto objcSetter = lookupInstanceMethod(
                                Impl.setObjectAtIndexedSubscript)) {
          setter = cast_or_null<FuncDecl>(Impl.importDecl(objcSetter));

          // Don't allow static setters.
          if (setter && setter->isStatic())
            setter = nullptr;

          if (setter) {
            optionalMethods = optionalMethods &&
              objcSetter->getImplementationControl()
                == clang::ObjCMethodDecl::Optional;
          }
        }
      } else if (objcMethod->getSelector() == Impl.setObjectAtIndexedSubscript){
        setter = cast<FuncDecl>(decl);

        // Find the getter.
        if (auto objcGetter = lookupInstanceMethod(
                                Impl.objectAtIndexedSubscript)) {
          getter = cast_or_null<FuncDecl>(Impl.importDecl(objcGetter));

          // Don't allow static getters.
          if (getter && getter->isStatic())
            return nullptr;

          if (getter) {
            optionalMethods = optionalMethods &&
              objcGetter->getImplementationControl()
                == clang::ObjCMethodDecl::Optional;
          }
        }

        // FIXME: Swift doesn't have write-only subscripting.
        if (!getter)
          return nullptr;
      } else if (objcMethod->getSelector() == Impl.objectForKeyedSubscript) {
        getter = cast<FuncDecl>(decl);

        // Find the setter
        if (auto objcSetter = lookupInstanceMethod(
                                Impl.setObjectForKeyedSubscript)) {
          setter = cast_or_null<FuncDecl>(Impl.importDecl(objcSetter));

          // Don't allow static setters.
          if (setter && setter->isStatic())
            setter = nullptr;

          if (setter) {
            optionalMethods = optionalMethods &&
              objcSetter->getImplementationControl()
                == clang::ObjCMethodDecl::Optional;
          }

        }
      } else if (objcMethod->getSelector() == Impl.setObjectForKeyedSubscript) {
        setter = cast<FuncDecl>(decl);

        // Find the getter.
        if (auto objcGetter = lookupInstanceMethod(
                                Impl.objectForKeyedSubscript)) {
          getter = cast_or_null<FuncDecl>(Impl.importDecl(objcGetter));

          // Don't allow static getters.
          if (getter && getter->isStatic())
            return nullptr;

          if (getter) {
            optionalMethods = optionalMethods &&
              objcGetter->getImplementationControl()
                == clang::ObjCMethodDecl::Optional;
          }
        }

        // FIXME: Swift doesn't have write-only subscripting.
        if (!getter)
          return nullptr;
      } else {
        llvm_unreachable("Unknown getter/setter selector");
      }

      // Check whether we've already created a subscript operation for
      // this getter/setter pair.
      if (auto subscript = Impl.Subscripts[{getter, setter}])
        return subscript;

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
        auto tuple = dyn_cast<TuplePattern>(getter->getArgParamPatterns()[1]);
        if (tuple && tuple->getFields().size() != 1)
          return nullptr;

        getterIndices = tuple->getFields()[0].getPattern();
      }

      // Check the form of the setter.
      FuncDecl *setterThunk = nullptr;
      Pattern *setterIndices = nullptr;
      if (setter) {
        auto tuple = dyn_cast<TuplePattern>(setter->getBodyParamPatterns()[1]);
        if (!tuple)
          return nullptr;

        if (tuple->getFields().size() != 2)
          return nullptr;

        // The setter must accept elements of the same type as the getter
        // returns.
        // FIXME: Adjust C++ references?
        auto setterElementTy = tuple->getFields()[0].getPattern()->getType();
        if (!elementTy->isEqual(setterElementTy))
          return nullptr;

        setterIndices = tuple->getFields()[1].getPattern();

        // The setter must use the same indices as the getter.
        // FIXME: Adjust C++ references?
        // FIXME: Special case for NSDictionary, which uses 'id' for the getter
        // but 'id <NSCopying>' for the setter.
        if (!setterIndices->getType()->isEqual(getterIndices->getType())) {
          setter = nullptr;
          setterIndices = nullptr;

          // Check whether we've already created a subscript operation for
          // this getter.
          if (auto subscript = Impl.Subscripts[{getter, nullptr}])
            return subscript;
        }
      }

      getterThunk = buildGetterThunk(getter, dc, getterIndices);
      if (setter)
        setterThunk = buildSetterThunk(setter, dc, setterIndices);

      // Build the subscript declaration.
      auto argPatterns =
          getterThunk->getArgParamPatterns()[1]->clone(context);
      auto name = context.Id_subscript;
      auto subscript
        = new (context) SubscriptDecl(name, decl->getLoc(), argPatterns,
                                      decl->getLoc(),
                                      TypeLoc::withoutLoc(elementTy), dc);
      subscript->setAccessors(SourceRange(), getterThunk, setterThunk);
      subscript->setType(FunctionType::get(subscript->getIndices()->getType(),
                                           subscript->getElementType()));
      subscript->setIsObjC(true);

      // Optional subscripts in protocols.
      if (optionalMethods && isa<ProtocolDecl>(dc))
        subscript->getMutableAttrs().setAttr(AK_optional, SourceLoc());

      // Note that we've created this subscript.
      Impl.Subscripts[{getter, setter}] = subscript;
      Impl.Subscripts[{getterThunk, nullptr}] = subscript;

      // Determine whether this subscript operation overrides another subscript
      // operation.
      // FIXME: This ends up looking in the superclass for entirely bogus
      // reasons. Fix it.
      auto containerTy = dc->getDeclaredTypeInContext();
      SmallVector<ValueDecl *, 2> lookup;
      dc->lookupQualified(containerTy, name, NL_QualifiedDefault,
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

        assert(subscript->getDeclContext() != parentSub->getDeclContext() &&
               "can not override method in the same DeclContext");

        // The index types match. This is an override, so mark it as such.
        subscript->setOverriddenDecl(parentSub);
        if (auto parentGetter = parentSub->getGetter()) {
          if (getterThunk)
            getterThunk->setOverriddenDecl(parentGetter);
        }
        if (auto parentSetter = parentSub->getSetter()) {
          if (setterThunk)
            setterThunk->setOverriddenDecl(parentSetter);
        }

        // FIXME: Eventually, deal with multiple overrides.
        break;
      }

      return subscript;
    }

  public:

    /// Recursively add the given protocol and its inherited protocols to the
    /// given vector, guarded by the known set of protocols.
    static void addProtocols(ProtocolDecl *protocol,
                             SmallVectorImpl<ProtocolDecl *> &protocols,
                             llvm::SmallPtrSet<ProtocolDecl *, 4> &known) {
      if (!known.insert(protocol))
        return;

      protocols.push_back(protocol);
      for (auto inherited : protocol->getProtocols())
        addProtocols(inherited, protocols, known);
    }

    /// Finish the given protocol conformance (for an imported type)
    /// by filling in any missing witnesses.
    void finishProtocolConformance(NormalProtocolConformance *conformance) {
      // Create witnesses for requirements not already met.
      for (auto req : conformance->getProtocol()->getMembers()) {
        auto valueReq = dyn_cast<ValueDecl>(req);
        if (!valueReq)
          continue;

        if (!conformance->hasWitness(valueReq)) {
          if (auto func = dyn_cast<AbstractFunctionDecl>(valueReq)){
            // For an optional requirement, record an empty witness:
            // we'll end up querying this at runtime.
            if (func->getAttrs().isOptional()) {
              conformance->setWitness(valueReq, ConcreteDeclRef());
              continue;
            }
          }

          conformance->setWitness(valueReq, valueReq);
        }
      }

      conformance->setState(ProtocolConformanceState::Complete);
    }

    // Import the given Objective-C protocol list, along with any
    // implicitly-provided protocols, and attach them to the given
    // declaration.
    void importObjCProtocols(Decl *decl,
                             const clang::ObjCProtocolList &clangProtocols) {
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
        }
      }

      // Copy the list of protocols.
      MutableArrayRef<ProtocolDecl *> allProtocols 
        = Impl.SwiftContext.AllocateCopy(protocols);

      // Set the protocols.
      if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
        nominal->setProtocols(allProtocols);
      } else {
        auto ext = cast<ExtensionDecl>(decl);
        ext->setProtocols(allProtocols);
      }

      // Protocols don't require conformances.
      if (isa<ProtocolDecl>(decl))
        return;

      // Synthesize trivial conformances for each of the protocols.
      MutableArrayRef<ProtocolConformance *> allConformances
        = Impl.SwiftContext.Allocate<ProtocolConformance *>(allProtocols.size());
      auto dc = decl->getInnermostDeclContext();
      auto &ctx = Impl.SwiftContext;
      for (unsigned i = 0, n = allProtocols.size(); i != n; ++i) {
        // FIXME: Build a superclass conformance if the superclass
        // conforms.
        auto conformance
          = ctx.getConformance(dc->getDeclaredTypeOfContext(),
                               allProtocols[i], SourceLoc(),
                               dc->getModuleScopeContext(),
                               ProtocolConformanceState::Incomplete);
        finishProtocolConformance(conformance);
        allConformances[i] = conformance;
      }

      // Set the conformances.
      if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
        nominal->setConformances(allConformances);
      } else {
        auto ext = cast<ExtensionDecl>(decl);
        ext->setConformances(allConformances);        
      }
    }

    /// Import members of the given Objective-C container and add them to the
    /// list of corresponding Swift members.
    void importObjCMembers(const clang::ObjCContainerDecl *decl,
                           DeclContext *swiftContext,
                           SmallVectorImpl<Decl *> &members) {
      llvm::SmallPtrSet<Decl *, 4> knownMembers;
      for (auto m = decl->decls_begin(), mEnd = decl->decls_end();
           m != mEnd; ++m) {
        auto nd = dyn_cast<clang::NamedDecl>(*m);
        if (!nd)
          continue;

        auto member = Impl.importDecl(nd);
        if (!member)
          continue;

        // If this member is a method that is a getter or setter for a property
        // that was imported, don't add it to the list of members so it won't
        // be found by name lookup. This eliminates the ambiguity between
        // property names and getter names (by choosing to only have a
        // variable).
        if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(nd)) {
          if (auto property = objcMethod->findPropertyDecl())
            if (Impl.importDecl(
                  const_cast<clang::ObjCPropertyDecl *>(property)))
              continue;

          // If there is a special declaration associated with this member,
          // add it now.
          if (auto special = importSpecialMethod(member, swiftContext)) {
            if (knownMembers.insert(special))
              members.push_back(special);

            // If we imported a constructor, the underlying init method is not
            // visible.
            if (isa<ConstructorDecl>(special))
              continue;
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
        }
        
        members.push_back(member);
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
    /// the given list of members, so long as the the method was not already
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

      for (auto proto : protocols) {
        auto clangProto =
          cast_or_null<clang::ObjCProtocolDecl>(proto->getClangDecl());
        if (!clangProto)
          continue;

        // Don't import a protocol's members if the superclass already adopts
        // the protocol, or (for categories) if the class itself adopts it
        // in its main @interface.
        auto interfaceDecl = dyn_cast<clang::ObjCInterfaceDecl>(decl);
        if (!interfaceDecl) {
          auto category = cast<clang::ObjCCategoryDecl>(decl);
          interfaceDecl = category->getClassInterface();
          if (classImplementsProtocol(interfaceDecl, clangProto, false))
            continue;
        }
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
            clang::Selector sel = objcProp->getGetterName();
            if (decl->getMethod(sel, /*instance=*/true))
              continue;

            if (auto imported = Impl.importMirroredDecl(objcProp, dc)) {
              members.push_back(imported);
              // FIXME: We should mirror properties of the root class onto the
              // metatype.
            }

            continue;
          }

          auto func = dyn_cast<FuncDecl>(member);
          if (!func || func->isAccessor())
            continue;

          auto objcMethod =
            dyn_cast_or_null<clang::ObjCMethodDecl>(func->getClangDecl());
          if (!objcMethod)
            continue;

          clang::Selector sel = objcMethod->getSelector();
          if (decl->getMethod(sel, objcMethod->isInstanceMethod()))
            continue;

          if (auto imported = Impl.importMirroredDecl(objcMethod, dc)) {
            members.push_back(imported);

            // Import any special methods based on this member.
            if (auto special = importSpecialMethod(imported, dc))
              members.push_back(special);

            if (isRoot && objcMethod->isInstanceMethod() &&
                !decl->getClassMethod(sel, /*AllowHidden=*/true)) {
              if (auto classImport = Impl.importMirroredDecl(objcMethod,
                                                             dc, true))
                members.push_back(classImport);
            }
          }
        }
      }
    }

    /// \brief Determine whether the given Objective-C class has an instance or
    /// class method with the given selector directly declared (i.e., not in
    /// a superclass or protocol).
    static bool hasMethodShallow(const clang::Selector sel, bool isInstance,
                                 const clang::ObjCInterfaceDecl *objcClass) {
      if (objcClass->getMethod(sel, isInstance))
        return true;

      for (auto cat = objcClass->visible_categories_begin(),
                catEnd = objcClass->visible_categories_end();
           cat != catEnd;
           ++cat) {
        if ((*cat)->getMethod(sel, isInstance))
          return true;
      }

      return false;
    }

    /// \brief Import constructors from our superclasses (and their
    /// categories/extensions), effectively "inheriting" constructors.
    void importInheritedConstructors(const clang::ObjCInterfaceDecl *objcClass,
                                     DeclContext *dc,
                                     SmallVectorImpl<Decl *> &members) {
      // FIXME: Would like a more robust way to ensure that we aren't creating
      // duplicates.
      llvm::SmallSet<clang::Selector, 16> knownSelectors;
      auto inheritConstructors = [&](const clang::ObjCContainerDecl *container,
                                     bool isConvenienceInit){
        for (auto meth = container->meth_begin(),
                  methEnd = container->meth_end();
             meth != methEnd; ++meth) {
          if ((*meth)->getMethodFamily() == clang::OMF_init &&
              isReallyInitMethod(*meth) &&
              !hasMethodShallow((*meth)->getSelector(),
                                (*meth)->isInstanceMethod(),
                                objcClass) &&
              knownSelectors.insert((*meth)->getSelector())) {
                if (auto imported = Impl.importDecl(*meth)) {
                  if (auto special = importConstructor(imported, *meth, dc,
                                                       /*implicit=*/true,
                                                       isConvenienceInit)) {
                    members.push_back(special);
                  }
                }
              }
        }
      };

      bool isConvenienceInit = false;
      for (auto curObjCClass = objcClass->getSuperClass(); curObjCClass;
           curObjCClass = curObjCClass->getSuperClass()) {
        inheritConstructors(curObjCClass, isConvenienceInit);
        for (auto cat = curObjCClass->visible_categories_begin(),
                  catEnd = curObjCClass->visible_categories_end();
             cat != catEnd;
             ++cat) {
          inheritConstructors(*cat, isConvenienceInit);
        }
        
        // When we hit a class that does declare it's designated
        // initializers, any initializers above it are convenience
        // initializers.
        if (curObjCClass->hasDesignatedInitializers())
          isConvenienceInit = true;
      }
    }

    Decl *VisitObjCCategoryDecl(const clang::ObjCCategoryDecl *decl) {
      // Objective-C categories and extensions map to Swift extensions.

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
      auto result
        = new (Impl.SwiftContext)
            ExtensionDecl(loc,
                          TypeLoc::withoutLoc(objcClass->getDeclaredType()),
                          { },
                          dc);
      objcClass->addExtension(result);
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
      result->setClangNode(decl);
      importObjCProtocols(result, decl->getReferencedProtocols());
      result->setCheckedInheritanceClause();
      result->setMemberLoader(&Impl, 0);

      return result;
    }

    template <typename T, typename U>
    T *resolveSwiftDecl(const U *decl, Identifier name, Module *adapter) {
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
    T *resolveSwiftDeclIfAnnotated(const U *decl, Identifier name,
                                   const DeclContext *dc) {
      using clang::AnnotateAttr;
      for (auto annotation : decl->template specific_attrs<AnnotateAttr>()) {
        if (annotation->getAnnotation() == SWIFT_NATIVE_ANNOTATION_STRING) {
          auto wrapperUnit = cast<ClangModuleUnit>(dc->getModuleScopeContext());
          return resolveSwiftDecl<T>(decl, name,
                                     wrapperUnit->getAdapterModule());
        }
      }

      return nullptr;
    }

    Decl *VisitObjCProtocolDecl(const clang::ObjCProtocolDecl *decl) {
      // Form the protocol name, using the renaming table when necessary.
      Identifier name;
      if (false) { }
#define RENAMED_PROTOCOL(ObjCName, SwiftName)                  \
      else if (decl->getName().equals(#ObjCName)) {            \
        name = Impl.SwiftContext.getIdentifier(#SwiftName);    \
      }
#include "RenamedProtocols.def"
      else {
        name = Impl.importName(decl->getDeclName());
      }

      if (name.empty())
        return nullptr;

      // FIXME: Figure out how to deal with incomplete protocols, since that
      // notion doesn't exist in Swift.
      if (!decl->hasDefinition()) {
        // Check if this protocol is implemented in its adapter.
        // FIXME: This only matters for the module currently being built.
        if (auto clangModule = Impl.getClangModuleForDecl(decl, true))
          if (auto adapter = clangModule->getAdapterModule())
            if (auto native = resolveSwiftDecl<ProtocolDecl>(decl, name,
                                                             adapter))
              return native;

        forwardDeclaration = true;
        return nullptr;
      }

      decl = decl->getDefinition();

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      if (auto native = resolveSwiftDeclIfAnnotated<ProtocolDecl>(decl, name,
                                                                  dc))
        return native;

      // Create the protocol declaration and record it.
      auto result = new (Impl.SwiftContext)
                      ProtocolDecl(dc,
                                   Impl.importSourceLoc(decl->getLocStart()),
                                   Impl.importSourceLoc(decl->getLocation()),
                                   name,
                                   { });
      result->computeType();
      
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

      // Create the archetype for the implicit 'Self'.
      auto selfId = Impl.SwiftContext.Id_Self;
      auto selfDecl = result->getSelf();
      auto selfArchetype = ArchetypeType::getNew(Impl.SwiftContext, nullptr,
                                                 result, selfId,
                                                 Type(result->getDeclaredType()),
                                                 Type(), /*Index=*/0);
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

      result->setClangNode(decl);
      result->setCircularityCheck(CircularityCheck::Checked);

      // Import protocols this protocol conforms to.
      importObjCProtocols(result, decl->getReferencedProtocols());
      result->setCheckedInheritanceClause();

      // Note that this is an Objective-C protocol.
      result->setIsObjC(true);
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
        decl->getMutableAttrs().setAttr(AK_requires_stored_property_inits,
                                        SourceLoc());
        cast<ClassDecl>(decl)->setRequiresStoredPropertyInits(true);
      }
    }

    Decl *VisitObjCInterfaceDecl(const clang::ObjCInterfaceDecl *decl) {
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      if (!decl->hasDefinition()) {
        // Special case for Protocol, which gets forward-declared everywhere but
        // really lives in ObjectiveC.
        // FIXME: This is a workaround for a Clang modules bug.
        // See http://llvm.org/bugs/show_bug.cgi?id=19061
        clang::ASTContext &clangCtx = Impl.getClangASTContext();
        if (decl->getCanonicalDecl() ==
            clangCtx.getObjCProtocolDecl()->getCanonicalDecl()) {
          Type nsObjectTy = Impl.getNSObjectType();
          if (!nsObjectTy)
            return nullptr;

          const ClassDecl *nsObjectDecl =
            nsObjectTy->getClassOrBoundGenericClass();
          auto dc = nsObjectDecl->getDeclContext();

          auto result = new (Impl.SwiftContext) ClassDecl(SourceLoc(), name,
                                                          SourceLoc(), {},
                                                          nullptr, dc);
          result->setAddedImplicitInitializers();
          result->computeType();
          Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
          result->setClangNode(decl);
          result->setCircularityCheck(CircularityCheck::Checked);
          result->setSuperclass(nsObjectTy);
          result->setCheckedInheritanceClause();
          result->setIsObjC(true);
          Impl.registerExternalDecl(result);
          return result;
        }

        // Otherwise, check if this class is implemented in its adapter.
        // FIXME: This only matters for the module currently being built.
        if (auto clangModule = Impl.getClangModuleForDecl(decl, true))
          if (auto adapter = clangModule->getAdapterModule())
            if (auto native = resolveSwiftDecl<ClassDecl>(decl, name, adapter))
              return native;
      }

      // FIXME: Figure out how to deal with incomplete types, since that
      // notion doesn't exist in Swift.
      decl = decl->getDefinition();
      if (!decl) {
        forwardDeclaration = true;
        return nullptr;
      }

      auto dc = Impl.importDeclContextOf(decl);
      if (!dc)
        return nullptr;

      if (auto native = resolveSwiftDeclIfAnnotated<ClassDecl>(decl, name, dc))
        return native;

      // Create the class declaration and record it.
      auto result = new (Impl.SwiftContext)
                      ClassDecl(Impl.importSourceLoc(decl->getLocStart()),
                                name,
                                Impl.importSourceLoc(decl->getLocation()),
                                { }, nullptr, dc);
      result->computeType();
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;
      result->setClangNode(decl);
      result->setCircularityCheck(CircularityCheck::Checked);

      // If this Objective-C class has a supertype, import it.
      if (auto objcSuper = decl->getSuperClass()) {
        auto super = cast_or_null<ClassDecl>(Impl.importDecl(objcSuper));
        if (!super)
          return nullptr;

        result->setSuperclass(super->getDeclaredType());
      }

      // Import protocols this class conforms to.
      importObjCProtocols(result, decl->getReferencedProtocols());
      result->setCheckedInheritanceClause();

      // Note that this is an Objective-C class.
      result->setIsObjC(true);

      // Add inferred attributes.
#define INFERRED_ATTRIBUTES(ModuleName, ClassName, AttributeSet)        \
      if (name.str().equals(#ClassName) &&                              \
          result->getParentModule()->Name.str().equals(#ModuleName)) {  \
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

    Decl *VisitObjCPropertyDecl(const clang::ObjCPropertyDecl *decl,
                                DeclContext *dc) {
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      // Check whether there is a function with the same name as this
      // property. If so, suppress the property; the user will have to use
      // the methods directly, to avoid ambiguities.
      auto containerTy = dc->getDeclaredTypeInContext();
      VarDecl *overridden = nullptr;
      SmallVector<ValueDecl *, 2> lookup;
      dc->lookupQualified(containerTy, name, NL_QualifiedDefault,
                          Impl.getTypeResolver(), lookup);
      for (auto result : lookup) {
        if (isa<FuncDecl>(result))
          return nullptr;

        if (auto var = dyn_cast<VarDecl>(result))
          overridden = var;
      }

      auto type = Impl.importType(decl->getType(), ImportTypeKind::Property);
      if (!type)
        return nullptr;

      // Import the getter.
      FuncDecl *getter = nullptr;
      if (auto clangGetter = decl->getGetterMethodDecl()) {
        getter = cast_or_null<FuncDecl>(VisitObjCMethodDecl(clangGetter, dc));
        if (!getter)
          return nullptr;
      }

      // Import the setter, if there is one.
      FuncDecl *setter = nullptr;
      if (auto clangSetter = decl->getSetterMethodDecl()) {
        setter = cast_or_null<FuncDecl>(VisitObjCMethodDecl(clangSetter, dc));
        if (!setter)
          return nullptr;
      }

      // Check whether the property already got imported.
      if (dc == Impl.importDeclContextOf(decl)) {
        auto known = Impl.ImportedDecls.find(decl->getCanonicalDecl());
        if (known != Impl.ImportedDecls.end())
          return known->second;
      }

      auto result = new (Impl.SwiftContext) VarDecl(
          /*static*/ false, /*IsLet*/ false,
          Impl.importSourceLoc(decl->getLocation()),
          name, type, dc);

      // Build thunks.
      FuncDecl *getterThunk = buildGetterThunk(getter, dc, nullptr);

      FuncDecl *setterThunk = nullptr;
      if (setter)
        setterThunk = buildSetterThunk(setter, dc, nullptr);

      // Turn this into a computed property.
      // FIXME: Fake locations for '{' and '}'?
      result->makeComputed(SourceLoc(), getterThunk, setterThunk, SourceLoc());
      result->setIsObjC(true);

      // Handle attributes.
      if (decl->hasAttr<clang::IBOutletAttr>())
        result->getMutableAttrs().setAttr(AK_IBOutlet, SourceLoc());
      if (decl->getPropertyImplementation() == clang::ObjCPropertyDecl::Optional
          && isa<ProtocolDecl>(dc))
        result->getMutableAttrs().setAttr(AK_optional, SourceLoc());
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
classifyEnum(const clang::EnumDecl *decl) {
  Identifier name;
  if (decl->getDeclName())
    name = importName(decl->getDeclName());
  else if (decl->getTypedefNameForAnonDecl())
    name = importName(decl->getTypedefNameForAnonDecl()->getDeclName());

  // Anonymous enumerations simply get mapped to constants of the
  // underlying type of the enum, because there is no way to conjure up a
  // name for the Swift type.
  if (name.empty())
    return EnumKind::Constants;
  
  // Was the enum declared using NS_ENUM or NS_OPTIONS?
  // FIXME: Use Clang attributes instead of grovelling the macro expansion loc.
  auto loc = decl->getLocStart();
  if (loc.isMacroID()) {
    StringRef MacroName = getClangPreprocessor().getImmediateMacroName(loc);
    if (MacroName == "CF_ENUM")
      return EnumKind::Enum;
    if (MacroName == "CF_OPTIONS")
      return EnumKind::Options;
  }
  
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

  auto Canon = cast<clang::NamedDecl>(ClangDecl->getCanonicalDecl());
  (void)Canon;
  // Note that the decl was imported from Clang.  Don't mark Swift decls as
  // imported.
  if (!Result->getDeclContext()->isModuleScopeContext() ||
      isa<ClangModuleUnit>(Result->getDeclContext())) {
#ifndef NDEBUG
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
#endif
    (void) SkippedOverTypedef;
    Result->setClangNode(ClangDecl);
  }
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
  while (!RegisteredExternalDecls.empty()) {
    Decl *D = RegisteredExternalDecls.pop_back_val();
    SwiftContext.addedExternalDecl(D);
    if (auto typeResolver = getTypeResolver())
      if (auto *nominal = dyn_cast<NominalTypeDecl>(D))
        typeResolver->resolveExternalDeclImplicitMembers(nominal);
  }
}

Decl *ClangImporter::Implementation::importDeclAndCacheImpl(
    const clang::NamedDecl *ClangDecl,
    bool SuperfluousTypedefsAreTransparent) {
  if (!ClangDecl)
    return nullptr;

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

  if (TypedefIsSuperfluous)
    SuperfluousTypedefs.insert(Canon);

  if (!HadForwardDeclaration)
    ImportedDecls[Canon] = Result;

  if (!SuperfluousTypedefsAreTransparent && TypedefIsSuperfluous)
    return nullptr;

  return Result;
}

Decl *
ClangImporter::Implementation::importMirroredDecl(const clang::NamedDecl *decl,
                                                  DeclContext *dc,
                                                  bool forceClassMethod) {
  if (!decl)
    return nullptr;

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
    assert(!result->getClangDecl() || result->getClangDecl() == canon);
    result->setClangNode(decl);
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
                                              bool isStatic) {
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
    llvm::SmallString<16> printedValue;
    if (value.getKind() == clang::APValue::Int) {
      value.getInt().toString(printedValue);
    } else {
      assert(value.getFloat().isFinite() && "can't handle infinities or NaNs");
      value.getFloat().toString(printedValue);
    }

    // If this was a negative number, record that and strip off the '-'.
    // FIXME: This is hideous!
    // FIXME: Actually make the negation work.
    bool isNegative = printedValue[0] == '-';
    if (isNegative)
      printedValue.erase(printedValue.begin());

    // Create the expression node.
    StringRef printedValueCopy(context.AllocateCopy(printedValue).data(),
                               printedValue.size());
    if (value.getKind() == clang::APValue::Int) {
      expr = new (context) IntegerLiteralExpr(printedValueCopy, SourceLoc(),
                                              /*Implicit=*/true);
    } else {
      expr = new (context) FloatLiteralExpr(printedValueCopy, SourceLoc(),
                                            /*Implicit=*/true);
    }

    if (!isNegative)
      break;

    // If it was a negative number, negate the integer literal.
    auto minusRef = getOperatorRef(context, context.getIdentifier("-"));
    if (!minusRef)
      return nullptr;
    expr = new (context) PrefixUnaryExpr(minusRef, expr);
    break;
  }
  }

  assert(expr);
  return createConstant(name, dc, type, expr, convertKind, isStatic);
}


ValueDecl *
ClangImporter::Implementation::createConstant(Identifier name, DeclContext *dc,
                                              Type type, StringRef value,
                                              ConstantConvertKind convertKind,
                                              bool isStatic) {
  auto expr = new (SwiftContext) StringLiteralExpr(value, SourceRange());
  return createConstant(name, dc, type, expr, convertKind, isStatic);
}


ValueDecl *
ClangImporter::Implementation::createConstant(Identifier name, DeclContext *dc,
                                              Type type, Expr *valueExpr,
                                              ConstantConvertKind convertKind,
                                              bool isStatic) {
  auto &context = SwiftContext;

  auto var = new (context) VarDecl(isStatic, /*IsLet*/ false,
                                   SourceLoc(), name, type, dc);

  // Form the argument patterns.
  SmallVector<Pattern *, 3> getterArgs;

  // 'self'
  if (dc->isTypeContext()) {
    auto selfTy = dc->getDeclaredTypeInContext();
    if (isStatic)
      selfTy = MetatypeType::get(selfTy, context);
    Pattern *anyP = new (context) AnyPattern(SourceLoc(), /*implicit*/ true);
    anyP->setType(selfTy);
    getterArgs.push_back(anyP);
  }
  
  // empty tuple
  getterArgs.push_back(TuplePattern::create(context, SourceLoc(), { },
                                            SourceLoc()));
  getterArgs.back()->setType(TupleType::getEmpty(context));

  // Form the type of the getter.
  auto getterType = type;
  for (auto it = getterArgs.rbegin(), itEnd = getterArgs.rend();
       it != itEnd; ++it) {
    getterType = FunctionType::get((*it)->getType(), getterType);
  }

  // Create the getter function declaration.
  auto func = FuncDecl::create(context, SourceLoc(), StaticSpellingKind::None,
                               SourceLoc(), Identifier(),
                               SourceLoc(), nullptr, getterType, getterArgs,
                               getterArgs, TypeLoc::withoutLoc(type), dc);
  func->setStatic(isStatic);
  func->setBodyResultType(type);

  auto expr = valueExpr;

  // If we need a conversion, add one now.
  switch (convertKind) {
  case ConstantConvertKind::None:
    break;

  case ConstantConvertKind::Construction: {
    auto typeRef = new (context) MetatypeExpr(nullptr, SourceLoc(),
                                              MetatypeType::get(type, context));
    expr = new (context) CallExpr(typeRef, expr, /*Implicit=*/true);
    break;
   }

  case ConstantConvertKind::Coerce:
    break;

  case ConstantConvertKind::Downcast: {
    auto cast = new (context) ConditionalCheckedCastExpr(expr,
                                                     SourceLoc(),
                                                     TypeLoc::withoutLoc(type));
    cast->setCastKind(CheckedCastKind::Downcast);
    cast->setImplicit();
    expr = new (context) ForceValueExpr(cast, SourceLoc());
    break;
  }
  }

  // Create the return statement.
  auto ret = new (context) ReturnStmt(SourceLoc(), expr);

  // Finally, set the body.
  func->setBody(BraceStmt::create(context, SourceLoc(),
                                  ASTNode(ret),
                                  SourceLoc()));

  // Set the function up as the getter.
  var->makeComputed(SourceLoc(), func, nullptr, SourceLoc());

  // Register this thunk as an external definition.
  registerExternalDecl(func);

  return var;
}

ArrayRef<Decl *>
ClangImporter::Implementation::loadAllMembers(const Decl *D, uint64_t unused) {
  assert(D->hasClangNode());
  auto clangDecl = cast<clang::ObjCContainerDecl>(D->getClangDecl());

  SmallVector<Decl *, 4> members;
  SwiftDeclConverter converter(*this);

  const DeclContext *DC;
  ArrayRef<ProtocolDecl *> protos;

  if (auto clangClass = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl)) {
    auto swiftClass = cast<ClassDecl>(D);
    protos = swiftClass->getProtocols();
    DC = swiftClass;

    clangDecl = clangClass = clangClass->getDefinition();

    // Imported inherited initializers.
    if (clangClass->getName() != "Protocol") {
      converter.importInheritedConstructors(clangClass,
                                            const_cast<DeclContext *>(DC),
                                            members);
    }

  } else if (auto clangProto = dyn_cast<clang::ObjCProtocolDecl>(clangDecl)) {
    DC = cast<ProtocolDecl>(D);
    clangDecl = clangProto->getDefinition();

  } else {
    auto extension = cast<ExtensionDecl>(D);
    DC = extension;
    protos = extension->getProtocols();
  }

  converter.importObjCMembers(clangDecl, const_cast<DeclContext *>(DC),
                              members);

  // Import mirrored declarations for protocols to which this category
  // or extension conforms.
  // FIXME: This is supposed to be a short-term hack.
  converter.importMirroredProtocolMembers(clangDecl,
                                          const_cast<DeclContext *>(DC),
                                          protos, members, SwiftContext);

  return SwiftContext.AllocateCopy(members);
}

