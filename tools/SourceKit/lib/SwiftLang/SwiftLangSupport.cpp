//===--- SwiftLangSupport.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SwiftLangSupport.h"
#include "SwiftASTManager.h"
#include "SourceKit/Core/Context.h"
#include "SourceKit/Support/UIdent.h"

#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/USRGeneration.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/IDE/CodeCompletionCache.h"
#include "swift/IDE/SyntaxModel.h"
#include "swift/IDE/Utils.h"
#include "swift/Config.h"

#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

#include <sys/param.h>

using namespace SourceKit;
using namespace swift;
using namespace swift::ide;

static UIdent KindDeclFunctionFree("source.lang.swift.decl.function.free");
static UIdent KindRefFunctionFree("source.lang.swift.ref.function.free");
static UIdent KindDeclMethodInstance(
      "source.lang.swift.decl.function.method.instance");
static UIdent KindRefMethodInstance(
      "source.lang.swift.ref.function.method.instance");
static UIdent KindDeclMethodStatic(
      "source.lang.swift.decl.function.method.static");
static UIdent KindRefMethodStatic(
      "source.lang.swift.ref.function.method.static");
static UIdent KindDeclMethodClass(
      "source.lang.swift.decl.function.method.class");
static UIdent KindRefMethodClass(
      "source.lang.swift.ref.function.method.class");
static UIdent KindDeclAccessorGetter(
      "source.lang.swift.decl.function.accessor.getter");
static UIdent KindRefAccessorGetter(
      "source.lang.swift.ref.function.accessor.getter");
static UIdent KindDeclAccessorSetter(
      "source.lang.swift.decl.function.accessor.setter");
static UIdent KindRefAccessorSetter(
      "source.lang.swift.ref.function.accessor.setter");
static UIdent KindDeclAccessorWillSet(
      "source.lang.swift.decl.function.accessor.willset");
static UIdent KindRefAccessorWillSet(
      "source.lang.swift.ref.function.accessor.willset");
static UIdent KindDeclAccessorDidSet(
      "source.lang.swift.decl.function.accessor.didset");
static UIdent KindRefAccessorDidSet(
      "source.lang.swift.ref.function.accessor.didset");
static UIdent KindDeclAccessorAddress(
      "source.lang.swift.decl.function.accessor.address");
static UIdent KindRefAccessorAddress(
      "source.lang.swift.ref.function.accessor.address");
static UIdent KindDeclAccessorMutableAddress(
      "source.lang.swift.decl.function.accessor.mutableaddress");
static UIdent KindRefAccessorMutableAddress(
      "source.lang.swift.ref.function.accessor.mutableaddress");
static UIdent KindDeclConstructor("source.lang.swift.decl.function.constructor");
static UIdent KindRefConstructor("source.lang.swift.ref.function.constructor");
static UIdent KindDeclDestructor("source.lang.swift.decl.function.destructor");
static UIdent KindRefDestructor("source.lang.swift.ref.function.destructor");
static UIdent KindDeclFunctionPrefixOperator("source.lang.swift.decl.function.operator.prefix");
static UIdent KindDeclFunctionPostfixOperator("source.lang.swift.decl.function.operator.postfix");
static UIdent KindDeclFunctionInfixOperator("source.lang.swift.decl.function.operator.infix");
static UIdent KindRefFunctionPrefixOperator("source.lang.swift.ref.function.operator.prefix");
static UIdent KindRefFunctionPostfixOperator("source.lang.swift.ref.function.operator.postfix");
static UIdent KindRefFunctionInfixOperator("source.lang.swift.ref.function.operator.infix");
static UIdent KindDeclSubscript("source.lang.swift.decl.function.subscript");
static UIdent KindRefSubscript("source.lang.swift.ref.function.subscript");
static UIdent KindDeclVarGlobal("source.lang.swift.decl.var.global");
static UIdent KindRefVarGlobal("source.lang.swift.ref.var.global");
static UIdent KindDeclVarInstance("source.lang.swift.decl.var.instance");
static UIdent KindRefVarInstance("source.lang.swift.ref.var.instance");
static UIdent KindDeclVarStatic("source.lang.swift.decl.var.static");
static UIdent KindRefVarStatic("source.lang.swift.ref.var.static");
static UIdent KindDeclVarClass("source.lang.swift.decl.var.class");
static UIdent KindRefVarClass("source.lang.swift.ref.var.class");
static UIdent KindDeclVarLocal("source.lang.swift.decl.var.local");
static UIdent KindRefVarLocal("source.lang.swift.ref.var.local");
static UIdent KindDeclVarParam("source.lang.swift.decl.var.parameter");
static UIdent KindDeclModule("source.lang.swift.decl.module");
static UIdent KindDeclClass("source.lang.swift.decl.class");
static UIdent KindRefClass("source.lang.swift.ref.class");
static UIdent KindDeclStruct("source.lang.swift.decl.struct");
static UIdent KindRefStruct("source.lang.swift.ref.struct");
static UIdent KindDeclEnum("source.lang.swift.decl.enum");
static UIdent KindRefEnum("source.lang.swift.ref.enum");
static UIdent KindDeclEnumCase("source.lang.swift.decl.enumcase");
static UIdent KindDeclEnumElement("source.lang.swift.decl.enumelement");
static UIdent KindRefEnumElement("source.lang.swift.ref.enumelement");
static UIdent KindDeclProtocol("source.lang.swift.decl.protocol");
static UIdent KindRefProtocol("source.lang.swift.ref.protocol");
static UIdent KindDeclExtension("source.lang.swift.decl.extension");
static UIdent KindDeclExtensionStruct("source.lang.swift.decl.extension.struct");
static UIdent KindDeclExtensionClass("source.lang.swift.decl.extension.class");
static UIdent KindDeclExtensionEnum("source.lang.swift.decl.extension.enum");
static UIdent KindDeclExtensionProtocol("source.lang.swift.decl.extension.protocol");
static UIdent KindDeclTypeAlias("source.lang.swift.decl.typealias");
static UIdent KindRefTypeAlias("source.lang.swift.ref.typealias");
static UIdent KindDeclGenericTypeParam("source.lang.swift.decl.generic_type_param");
static UIdent KindRefGenericTypeParam("source.lang.swift.ref.generic_type_param");
static UIdent KindRefModule("source.lang.swift.ref.module");
static UIdent KindStmtForEach("source.lang.swift.stmt.foreach");
static UIdent KindStmtFor("source.lang.swift.stmt.for");
static UIdent KindStmtWhile("source.lang.swift.stmt.while");
static UIdent KindStmtRepeatWhile("source.lang.swift.stmt.repeatwhile");
static UIdent KindStmtIf("source.lang.swift.stmt.if");
static UIdent KindStmtGuard("source.lang.swift.stmt.guard");
static UIdent KindStmtSwitch("source.lang.swift.stmt.switch");
static UIdent KindStmtCase("source.lang.swift.stmt.case");
static UIdent KindStmtBrace("source.lang.swift.stmt.brace");
static UIdent KindExprCall("source.lang.swift.expr.call");
static UIdent KindExprArray("source.lang.swift.expr.array");
static UIdent KindExprDictionary("source.lang.swift.expr.dictionary");
static UIdent KindExprObjectLiteral("source.lang.swift.expr.object_literal");

static UIdent KindStructureElemId("source.lang.swift.structure.elem.id");
static UIdent KindStructureElemExpr("source.lang.swift.structure.elem.expr");
static UIdent KindStructureElemInitExpr("source.lang.swift.structure.elem.init_expr");
static UIdent KindStructureElemCondExpr("source.lang.swift.structure.elem.condition_expr");
static UIdent KindStructureElemPattern("source.lang.swift.structure.elem.pattern");
static UIdent KindStructureElemTypeRef("source.lang.swift.structure.elem.typeref");


std::unique_ptr<LangSupport>
LangSupport::createSwiftLangSupport(SourceKit::Context &SKCtx) {
  return std::unique_ptr<LangSupport>(new SwiftLangSupport(SKCtx));
}

namespace {

class UIdentVisitor : public ASTVisitor<UIdentVisitor,
                                        UIdent, UIdent, UIdent, UIdent > {
  bool IsRef;

public:
  explicit UIdentVisitor(bool IsRef) : IsRef(IsRef) { }

  /// TODO: reconsider whether having a default case is a good idea.
  UIdent visitDecl(const Decl *D) { return UIdent(); }

  UIdent visitFuncDecl(const FuncDecl *D);
  UIdent visitVarDecl(const VarDecl *D);
  UIdent visitExtensionDecl(const ExtensionDecl *D);
  UIdent visitAssociatedTypeDecl(const AssociatedTypeDecl *D) {
    return IsRef ? KindRefTypeAlias : KindDeclTypeAlias;
  }

#define UID_FOR(CLASS) \
  UIdent visit##CLASS##Decl(const CLASS##Decl *) { \
    return IsRef ? KindRef##CLASS : KindDecl##CLASS; \
  }
  UID_FOR(Class)
  UID_FOR(Struct)
  UID_FOR(Enum)
  UID_FOR(EnumElement)
  UID_FOR(Protocol)
  UID_FOR(TypeAlias)
  UID_FOR(GenericTypeParam)
  UID_FOR(Constructor)
  UID_FOR(Destructor)
  UID_FOR(Subscript)
#undef UID_FOR
};

} // anonymous namespace

UIdent UIdentVisitor::visitFuncDecl(const FuncDecl *D) {
  if (D->isAccessor()) {
    return SwiftLangSupport::getUIDForAccessor(D->getAccessorStorageDecl(),
                                               D->getAccessorKind(),
                                               IsRef);
  }

  if (D->isOperator()) {
    switch (D->getOperatorDecl()->getKind()) {
    case DeclKind::PrefixOperator:
      return IsRef ? KindRefFunctionPrefixOperator : KindDeclFunctionPrefixOperator;
    case DeclKind::PostfixOperator:
      return IsRef ? KindRefFunctionPostfixOperator : KindDeclFunctionPostfixOperator;
    case DeclKind::InfixOperator:
      return IsRef ? KindRefFunctionInfixOperator : KindDeclFunctionInfixOperator;
    default:
      llvm_unreachable("unexpected operator kind");
    }
  }

  const DeclContext *DC = D->getDeclContext();
  if (DC->isTypeContext()) {
    if (D->isStatic()) {
      if (D->getCorrectStaticSpelling() == StaticSpellingKind::KeywordClass)
        return IsRef ? KindRefMethodClass : KindDeclMethodClass;
      else
        return IsRef ? KindRefMethodStatic : KindDeclMethodStatic;
    }
    return IsRef ? KindRefMethodInstance : KindDeclMethodInstance;
  }
  return IsRef ? KindRefFunctionFree : KindDeclFunctionFree;
}

UIdent UIdentVisitor::visitVarDecl(const VarDecl *D) {
  const DeclContext *DC = D->getDeclContext();
  if (DC->isTypeContext()) {
    if (D->isStatic()) {
      if (D->getCorrectStaticSpelling() == StaticSpellingKind::KeywordClass)
        return IsRef ? KindRefVarClass : KindDeclVarClass;
      else
        return IsRef ? KindRefVarStatic : KindDeclVarStatic;
    }
    return IsRef ? KindRefVarInstance : KindDeclVarInstance;
  }
  if (DC->isLocalContext())
    return IsRef ? KindRefVarLocal : KindDeclVarLocal;
  return IsRef ? KindRefVarGlobal : KindDeclVarGlobal;
}

UIdent UIdentVisitor::visitExtensionDecl(const ExtensionDecl *D) {
  assert(!IsRef && "reference to an extension ?");
  if (NominalTypeDecl *NTD = D->getExtendedType()->getAnyNominal()) {
    if (isa<StructDecl>(NTD))
      return KindDeclExtensionStruct;
    if (isa<ClassDecl>(NTD))
      return KindDeclExtensionClass;
    if (isa<EnumDecl>(NTD))
      return KindDeclExtensionEnum;
    if (isa<ProtocolDecl>(NTD))
      return KindDeclExtensionProtocol;
  }
  return UIdent();
}

SwiftLangSupport::SwiftLangSupport(SourceKit::Context &SKCtx)
    : SKCtx(SKCtx), CCCache(new SwiftCompletionCache) {
  llvm::SmallString<128> LibPath(SKCtx.getRuntimeLibPath());
  llvm::sys::path::append(LibPath, "swift");
  RuntimeResourcePath = LibPath.str();

  ASTMgr.reset(new SwiftASTManager(*this));
  // By default, just use the in-memory cache.
  CCCache->inMemory = llvm::make_unique<ide::CodeCompletionCache>();
}

SwiftLangSupport::~SwiftLangSupport() {
}

UIdent SwiftLangSupport::getUIDForDecl(const Decl *D, bool IsRef) {
  return UIdentVisitor(IsRef).visit(const_cast<Decl*>(D));
}

UIdent SwiftLangSupport::getUIDForLocalVar(bool IsRef) {
  return IsRef ? KindRefVarLocal : KindDeclVarLocal;
}

UIdent SwiftLangSupport::getUIDForAccessor(const ValueDecl *D,
                                           AccessorKind AccKind,
                                           bool IsRef) {
  switch (AccKind) {
  case AccessorKind::NotAccessor:
    llvm_unreachable("expected accessor");
  case AccessorKind::IsMaterializeForSet:
    llvm_unreachable("unexpected MaterializeForSet");
  case AccessorKind::IsGetter:
    return IsRef ? KindRefAccessorGetter : KindDeclAccessorGetter;
  case AccessorKind::IsSetter:
    return IsRef ? KindRefAccessorSetter : KindDeclAccessorSetter;
  case AccessorKind::IsWillSet:
    return IsRef ? KindRefAccessorWillSet : KindDeclAccessorWillSet;
  case AccessorKind::IsDidSet:
    return IsRef ? KindRefAccessorDidSet : KindDeclAccessorDidSet;
  case AccessorKind::IsAddressor:
    return IsRef ? KindRefAccessorAddress : KindDeclAccessorAddress;
  case AccessorKind::IsMutableAddressor:
    return IsRef ? KindRefAccessorMutableAddress
                 : KindDeclAccessorMutableAddress;
  }
}

SourceKit::UIdent SwiftLangSupport::getUIDForModuleRef() {
  return KindRefModule;
}

UIdent SwiftLangSupport::getUIDForCodeCompletionDeclKind(
    ide::CodeCompletionDeclKind Kind, bool IsRef) {
  if (IsRef) {
    // FIXME: The code-completion kind is also used for semantic annotations.
    // We should either create a new kind or rename the code-completion kind to
    // something more general.
    switch (Kind) {
    case CodeCompletionDeclKind::Module: return KindRefModule;
    case CodeCompletionDeclKind::Class: return KindRefClass;
    case CodeCompletionDeclKind::Struct: return KindRefStruct;
    case CodeCompletionDeclKind::Enum: return KindRefEnum;
    case CodeCompletionDeclKind::EnumElement: return KindRefEnumElement;
    case CodeCompletionDeclKind::Protocol: return KindRefProtocol;
    case CodeCompletionDeclKind::TypeAlias: return KindRefTypeAlias;
    case CodeCompletionDeclKind::GenericTypeParam: return KindRefGenericTypeParam;
    case CodeCompletionDeclKind::Constructor: return KindRefConstructor;
    case CodeCompletionDeclKind::Destructor: return KindRefDestructor;
    case CodeCompletionDeclKind::Subscript: return KindRefSubscript;
    case CodeCompletionDeclKind::StaticMethod: return KindRefMethodClass;
    case CodeCompletionDeclKind::InstanceMethod: return KindRefMethodInstance;
    case CodeCompletionDeclKind::PrefixOperatorFunction: return KindRefFunctionPrefixOperator;
    case CodeCompletionDeclKind::PostfixOperatorFunction: return KindRefFunctionPostfixOperator;
    case CodeCompletionDeclKind::InfixOperatorFunction: return KindRefFunctionInfixOperator;
    case CodeCompletionDeclKind::FreeFunction: return KindRefFunctionFree;
    case CodeCompletionDeclKind::StaticVar: return KindRefVarClass;
    case CodeCompletionDeclKind::InstanceVar: return KindRefVarInstance;
    case CodeCompletionDeclKind::LocalVar: return KindRefVarLocal;
    case CodeCompletionDeclKind::GlobalVar: return KindRefVarGlobal;
    }
  }

  switch (Kind) {
  case CodeCompletionDeclKind::Module: return KindDeclModule;
  case CodeCompletionDeclKind::Class: return KindDeclClass;
  case CodeCompletionDeclKind::Struct: return KindDeclStruct;
  case CodeCompletionDeclKind::Enum: return KindDeclEnum;
  case CodeCompletionDeclKind::EnumElement: return KindDeclEnumElement;
  case CodeCompletionDeclKind::Protocol: return KindDeclProtocol;
  case CodeCompletionDeclKind::TypeAlias: return KindDeclTypeAlias;
  case CodeCompletionDeclKind::GenericTypeParam: return KindDeclGenericTypeParam;
  case CodeCompletionDeclKind::Constructor: return KindDeclConstructor;
  case CodeCompletionDeclKind::Destructor: return KindDeclDestructor;
  case CodeCompletionDeclKind::Subscript: return KindDeclSubscript;
  case CodeCompletionDeclKind::StaticMethod: return KindDeclMethodClass;
  case CodeCompletionDeclKind::InstanceMethod: return KindDeclMethodInstance;
  case CodeCompletionDeclKind::PrefixOperatorFunction: return KindDeclFunctionPrefixOperator;
  case CodeCompletionDeclKind::PostfixOperatorFunction: return KindDeclFunctionPostfixOperator;
  case CodeCompletionDeclKind::InfixOperatorFunction: return KindDeclFunctionInfixOperator;
  case CodeCompletionDeclKind::FreeFunction: return KindDeclFunctionFree;
  case CodeCompletionDeclKind::StaticVar: return KindDeclVarClass;
  case CodeCompletionDeclKind::InstanceVar: return KindDeclVarInstance;
  case CodeCompletionDeclKind::LocalVar: return KindDeclVarLocal;
  case CodeCompletionDeclKind::GlobalVar: return KindDeclVarGlobal;
  }
}

UIdent SwiftLangSupport::getUIDForSyntaxNodeKind(SyntaxNodeKind SC) {
  static UIdent KindKeyword("source.lang.swift.syntaxtype.keyword");
  static UIdent KindIdentifier("source.lang.swift.syntaxtype.identifier");
  static UIdent KindTypeIdentifier("source.lang.swift.syntaxtype.typeidentifier");
  static UIdent KindBuildConfigKeyword("source.lang.swift.syntaxtype.buildconfig.keyword");
  static UIdent KindBuildConfigId("source.lang.swift.syntaxtype.buildconfig.id");
  static UIdent KindAttributeId("source.lang.swift.syntaxtype.attribute.id");
  static UIdent KindAttributeBuiltin("source.lang.swift.syntaxtype.attribute.builtin");
  static UIdent KindNumber("source.lang.swift.syntaxtype.number");
  static UIdent KindString("source.lang.swift.syntaxtype.string");
  static UIdent KindStringInterpolation("source.lang.swift.syntaxtype.string_interpolation_anchor");
  static UIdent KindComment("source.lang.swift.syntaxtype.comment");
  static UIdent KindDocComment("source.lang.swift.syntaxtype.doccomment");
  static UIdent KindDocCommentField("source.lang.swift.syntaxtype.doccomment.field");
  static UIdent KindCommentMarker("source.lang.swift.syntaxtype.comment.mark");
  static UIdent KindCommentURL("source.lang.swift.syntaxtype.comment.url");
  static UIdent KindPlaceholder("source.lang.swift.syntaxtype.placeholder");
  static UIdent KindObjectLiteral("source.lang.swift.syntaxtype.objectliteral");

  switch (SC) {
  case SyntaxNodeKind::Keyword:
    return KindKeyword;
  case SyntaxNodeKind::Identifier:
  case SyntaxNodeKind::DollarIdent:
    return KindIdentifier;
  case SyntaxNodeKind::Integer:
  case SyntaxNodeKind::Floating:
    return KindNumber;
  case SyntaxNodeKind::String:
    return KindString;
  case SyntaxNodeKind::StringInterpolationAnchor:
    return KindStringInterpolation;
  case SyntaxNodeKind::CommentLine:
  case SyntaxNodeKind::CommentBlock:
    return KindComment;
  case SyntaxNodeKind::CommentMarker:
    return KindCommentMarker;
  case SyntaxNodeKind::CommentURL:
    return KindCommentURL;
  case SyntaxNodeKind::DocCommentLine:
  case SyntaxNodeKind::DocCommentBlock:
    return KindDocComment;
  case SyntaxNodeKind::DocCommentField:
    return KindDocCommentField;
  case SyntaxNodeKind::TypeId:
    return KindTypeIdentifier;
  case SyntaxNodeKind::BuildConfigKeyword:
    return KindBuildConfigKeyword;
  case SyntaxNodeKind::BuildConfigId:
    return KindBuildConfigId;
  case SyntaxNodeKind::AttributeId:
    return KindAttributeId;
  case SyntaxNodeKind::AttributeBuiltin:
    return KindAttributeBuiltin;
  case SyntaxNodeKind::EditorPlaceholder:
    return KindPlaceholder;
  case SyntaxNodeKind::ObjectLiteral:
    return KindObjectLiteral;
  }
}

UIdent SwiftLangSupport::getUIDForSyntaxStructureKind(
    SyntaxStructureKind Kind) {
  switch (Kind) {
    case SyntaxStructureKind::Class:
      return KindDeclClass;
    case SyntaxStructureKind::Struct:
      return KindDeclStruct;
    case SyntaxStructureKind::Protocol:
      return KindDeclProtocol;
    case SyntaxStructureKind::Enum:
      return KindDeclEnum;
    case SyntaxStructureKind::Extension:
      return KindDeclExtension;
    case SyntaxStructureKind::FreeFunction:
      return KindDeclFunctionFree;
    case SyntaxStructureKind::InstanceFunction:
      return KindDeclMethodInstance;
    case SyntaxStructureKind::StaticFunction:
      return KindDeclMethodStatic;
    case SyntaxStructureKind::ClassFunction:
      return KindDeclMethodClass;
    case SyntaxStructureKind::GlobalVariable:
      return KindDeclVarGlobal;
    case SyntaxStructureKind::InstanceVariable:
      return KindDeclVarInstance;
    case SyntaxStructureKind::StaticVariable:
      return KindDeclVarStatic;
    case SyntaxStructureKind::ClassVariable:
      return KindDeclVarClass;
    case SyntaxStructureKind::EnumCase:
      return KindDeclEnumCase;
    case SyntaxStructureKind::EnumElement:
      return KindDeclEnumElement;
    case SyntaxStructureKind::Parameter:
      return KindDeclVarParam;
    case SyntaxStructureKind::ForEachStatement:
      return KindStmtForEach;
    case SyntaxStructureKind::ForStatement:
      return KindStmtFor;
    case SyntaxStructureKind::WhileStatement:
      return KindStmtWhile;
    case SyntaxStructureKind::RepeatWhileStatement:
      return KindStmtRepeatWhile;
    case SyntaxStructureKind::IfStatement:
      return KindStmtIf;
    case SyntaxStructureKind::GuardStatement:
      return KindStmtGuard;
    case SyntaxStructureKind::SwitchStatement:
      return KindStmtSwitch;
    case SyntaxStructureKind::CaseStatement:
      return KindStmtCase;
    case SyntaxStructureKind::BraceStatement:
      return KindStmtBrace;
    case SyntaxStructureKind::CallExpression:
      return KindExprCall;
    case SyntaxStructureKind::ArrayExpression:
      return KindExprArray;
    case SyntaxStructureKind::DictionaryExpression:
      return KindExprDictionary;
    case SyntaxStructureKind::ObjectLiteralExpression:
      return KindExprObjectLiteral;
  }
}

UIdent SwiftLangSupport::getUIDForSyntaxStructureElementKind(
    SyntaxStructureElementKind Kind) {
  switch (Kind) {
    case SyntaxStructureElementKind::Id: return KindStructureElemId;
    case SyntaxStructureElementKind::Expr: return KindStructureElemExpr;
    case SyntaxStructureElementKind::InitExpr: return KindStructureElemInitExpr;
    case SyntaxStructureElementKind::ConditionExpr: return KindStructureElemCondExpr;
    case SyntaxStructureElementKind::Pattern: return KindStructureElemPattern;
    case SyntaxStructureElementKind::TypeRef: return KindStructureElemTypeRef;
  }
}

bool SwiftLangSupport::printDisplayName(const swift::ValueDecl *D,
                                        llvm::raw_ostream &OS) {
  if (!D->hasName())
    return true;

  OS << D->getFullName();
  return false;
}

bool SwiftLangSupport::printUSR(const ValueDecl *D, llvm::raw_ostream &OS) {
  return ide::printDeclUSR(D, OS);
}

bool SwiftLangSupport::printAccessorUSR(const AbstractStorageDecl *D,
                                        AccessorKind AccKind,
                                        llvm::raw_ostream &OS) {
  return ide::printAccessorUSR(D, AccKind, OS);
}

std::string SwiftLangSupport::resolvePathSymlinks(StringRef FilePath) {
  std::string InputPath = FilePath;
  char full_path[MAXPATHLEN];
  if (const char *path = realpath(InputPath.c_str(), full_path))
    return path;
  return InputPath;
}

CloseClangModuleFiles::~CloseClangModuleFiles() {
  clang::Preprocessor &PP = loader.getClangPreprocessor();
  clang::ModuleMap &ModMap = PP.getHeaderSearchInfo().getModuleMap();
  for (auto I = ModMap.module_begin(), E = ModMap.module_end(); I != E; ++I) {
    clang::Module *M = I->second;
    if (!M->isSubModule() && M->getASTFile())
      M->getASTFile()->closeFile();
  }
}
