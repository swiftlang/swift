//===--- SwiftLangSupport.cpp ---------------------------------------------===//
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

#include "SwiftLangSupport.h"
#include "SwiftASTManager.h"
#include "SourceKit/Core/Context.h"
#include "SourceKit/SwiftLang/Factory.h"
#include "SourceKit/Support/UIdent.h"

#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/SILOptions.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Config.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/IDE/CodeCompletionCache.h"
#include "swift/IDE/SyntaxModel.h"
#include "swift/IDE/Utils.h"

#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

#if !defined(_WIN32)
#include <sys/param.h>
#else
#define WIN32_MEAN_AND_LEAN
#define NOMINMAX
#include <windows.h>
#endif

using namespace SourceKit;
using namespace swift;
using namespace swift::ide;
using swift::index::SymbolKind;
using swift::index::SymbolSubKind;
using swift::index::SymbolProperty;
using swift::index::SymbolPropertySet;
using swift::index::SymbolInfo;
using swift::index::SymbolRole;
using swift::index::SymbolRoleSet;

#define KIND(NAME, CONTENT) static UIdent Kind##NAME(CONTENT);
#include "SourceKit/Core/ProtocolUIDs.def"

#define REFACTORING(KIND, NAME, ID) static UIdent Kind##Refactoring##KIND("source.refactoring.kind."#ID);
#include "swift/IDE/RefactoringKinds.def"

std::unique_ptr<LangSupport>
SourceKit::createSwiftLangSupport(SourceKit::Context &SKCtx) {
  return std::unique_ptr<LangSupport>(new SwiftLangSupport(SKCtx));
}

const std::string LangSupport::SynthesizedUSRSeparator = "::SYNTHESIZED::";

namespace {

class UIdentVisitor : public ASTVisitor<UIdentVisitor,
                                        UIdent, UIdent, UIdent, UIdent > {
  const bool IsRef;

public:
  explicit UIdentVisitor(bool IsRef) : IsRef(IsRef) { }

  /// TODO: reconsider whether having a default case is a good idea.
  UIdent visitDecl(const Decl *D) { return UIdent(); }

  UIdent visitFuncDecl(const FuncDecl *D);
  UIdent visitVarDecl(const VarDecl *D);
  UIdent visitParamDecl(const ParamDecl *D);
  UIdent visitExtensionDecl(const ExtensionDecl *D);

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
  UID_FOR(AssociatedType)
  UID_FOR(GenericTypeParam)
  UID_FOR(Constructor)
  UID_FOR(Destructor)
  UID_FOR(Subscript)
#undef UID_FOR
};

} // anonymous namespace

UIdent UIdentVisitor::visitFuncDecl(const FuncDecl *D) {
  if (auto AD = dyn_cast<AccessorDecl>(D)) {
    return SwiftLangSupport::getUIDForAccessor(AD->getStorage(),
                                               AD->getAccessorKind(),
                                               IsRef);
  }

  if (auto *Op = D->getOperatorDecl()) {
    switch (Op->getKind()) {
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

UIdent UIdentVisitor::visitParamDecl(const ParamDecl *D) {
  // There is no KindRefVarParam. It's not usually an interesting difference.
  return IsRef ? KindRefVarLocal : KindDeclVarParam;
}

UIdent UIdentVisitor::visitExtensionDecl(const ExtensionDecl *D) {
  assert(!IsRef && "reference to an extension ?");
  if (NominalTypeDecl *NTD = D->getExtendedNominal()) {
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
    : NotificationCtr(SKCtx.getNotificationCenter()),
      CCCache(new SwiftCompletionCache) {
  llvm::SmallString<128> LibPath(SKCtx.getRuntimeLibPath());
  llvm::sys::path::append(LibPath, "swift");
  RuntimeResourcePath = LibPath.str();

  Stats = std::make_shared<SwiftStatistics>();
  EditorDocuments = std::make_shared<SwiftEditorDocumentFileMap>();
  ASTMgr = std::make_shared<SwiftASTManager>(EditorDocuments, Stats,
                                             RuntimeResourcePath);
  // By default, just use the in-memory cache.
  CCCache->inMemory = llvm::make_unique<ide::CodeCompletionCache>();
}

SwiftLangSupport::~SwiftLangSupport() {
}

UIdent SwiftLangSupport::getUIDForDecl(const Decl *D, bool IsRef) {
  return UIdentVisitor(IsRef).visit(const_cast<Decl*>(D));
}

UIdent SwiftLangSupport::getUIDForExtensionOfDecl(const Decl *D) {
  switch (D->getKind()) {
    case swift::DeclKind::Struct:
      return KindDeclExtensionStruct;
    case swift::DeclKind::Enum:
      return KindDeclExtensionEnum;
    case swift::DeclKind::Class:
      return KindDeclExtensionClass;
    case swift::DeclKind::Protocol:
      return KindDeclExtensionProtocol;
    default:
      llvm_unreachable("cannot have extension.");
  }
}

UIdent SwiftLangSupport::getUIDForLocalVar(bool IsRef) {
  return IsRef ? KindRefVarLocal : KindDeclVarLocal;
}

UIdent SwiftLangSupport::getUIDForAccessor(const ValueDecl *D,
                                           AccessorKind AccKind,
                                           bool IsRef) {
  switch (AccKind) {
  case AccessorKind::Get:
    return IsRef ? KindRefAccessorGetter : KindDeclAccessorGetter;
  case AccessorKind::Set:
    return IsRef ? KindRefAccessorSetter : KindDeclAccessorSetter;
  case AccessorKind::WillSet:
    return IsRef ? KindRefAccessorWillSet : KindDeclAccessorWillSet;
  case AccessorKind::DidSet:
    return IsRef ? KindRefAccessorDidSet : KindDeclAccessorDidSet;
  case AccessorKind::Address:
    return IsRef ? KindRefAccessorAddress : KindDeclAccessorAddress;
  case AccessorKind::MutableAddress:
    return IsRef ? KindRefAccessorMutableAddress
                 : KindDeclAccessorMutableAddress;
  case AccessorKind::Read:
    return IsRef ? KindRefAccessorRead : KindDeclAccessorRead;
  case AccessorKind::Modify:
    return IsRef ? KindRefAccessorModify : KindDeclAccessorModify;
  }

  llvm_unreachable("Unhandled AccessorKind in switch.");
}

SourceKit::UIdent SwiftLangSupport::getUIDForModuleRef() {
  return KindRefModule;
}

UIdent SwiftLangSupport::getUIDForRefactoringKind(ide::RefactoringKind Kind){
  switch(Kind) {
  case ide::RefactoringKind::None: llvm_unreachable("cannot end up here.");
#define REFACTORING(KIND, NAME, ID)                                            \
  case ide::RefactoringKind::KIND: return KindRefactoring##KIND;
#include "swift/IDE/RefactoringKinds.def"
  }
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
    case CodeCompletionDeclKind::AssociatedType: return KindRefAssociatedType;
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
    case CodeCompletionDeclKind::PrecedenceGroup: return KindRefPrecedenceGroup;
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
  case CodeCompletionDeclKind::AssociatedType: return KindDeclAssociatedType;
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
  case CodeCompletionDeclKind::PrecedenceGroup: return KindDeclPrecedenceGroup;
  case CodeCompletionDeclKind::FreeFunction: return KindDeclFunctionFree;
  case CodeCompletionDeclKind::StaticVar: return KindDeclVarClass;
  case CodeCompletionDeclKind::InstanceVar: return KindDeclVarInstance;
  case CodeCompletionDeclKind::LocalVar: return KindDeclVarLocal;
  case CodeCompletionDeclKind::GlobalVar: return KindDeclVarGlobal;
  }

  llvm_unreachable("Unhandled CodeCompletionDeclKind in switch.");
}

UIdent SwiftLangSupport::getUIDForSyntaxNodeKind(SyntaxNodeKind SC) {
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
  case SyntaxNodeKind::PoundDirectiveKeyword:
    return KindPoundDirectiveKeyword;
  case SyntaxNodeKind::AttributeId:
    return KindAttributeId;
  case SyntaxNodeKind::AttributeBuiltin:
    return KindAttributeBuiltin;
  case SyntaxNodeKind::EditorPlaceholder:
    return KindPlaceholder;
  case SyntaxNodeKind::ObjectLiteral:
    return KindObjectLiteral;
  }

  llvm_unreachable("Unhandled SyntaxNodeKind in switch.");
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
    case SyntaxStructureKind::LocalVariable:
      return KindDeclVarLocal;
    case SyntaxStructureKind::EnumCase:
      return KindDeclEnumCase;
    case SyntaxStructureKind::EnumElement:
      return KindDeclEnumElement;
    case SyntaxStructureKind::TypeAlias:
      return KindDeclTypeAlias;
    case SyntaxStructureKind::Subscript:
      return KindDeclSubscript;
    case SyntaxStructureKind::AssociatedType:
      return KindDeclAssociatedType;
    case SyntaxStructureKind::GenericTypeParam:
      return KindDeclGenericTypeParam;
    case SyntaxStructureKind::Parameter:
      return KindDeclVarParam;
    case SyntaxStructureKind::ForEachStatement:
      return KindStmtForEach;
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
    case SyntaxStructureKind::TupleExpression:
      return KindExprTuple;
    case SyntaxStructureKind::ClosureExpression:
      return KindExprClosure;
    case SyntaxStructureKind::Argument:
      return KindExprArg;
  }

  llvm_unreachable("Unhandled SyntaxStructureKind in switch.");
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

  llvm_unreachable("Unhandled SyntaxStructureElementKind in switch.");
}

SourceKit::UIdent SwiftLangSupport::
getUIDForRangeKind(swift::ide::RangeKind Kind) {
  switch (Kind) {
    case swift::ide::RangeKind::SingleExpression: return KindRangeSingleExpression;
    case swift::ide::RangeKind::SingleStatement: return KindRangeSingleStatement;
    case swift::ide::RangeKind::SingleDecl: return KindRangeSingleDeclaration;
    case swift::ide::RangeKind::MultiStatement: return KindRangeMultiStatement;
    case swift::ide::RangeKind::MultiTypeMemberDecl:
      return KindRangeMultiTypeMemberDeclaration;
    case swift::ide::RangeKind::PartOfExpression: return KindRangeInvalid;
    case swift::ide::RangeKind::Invalid: return KindRangeInvalid;
  }

  llvm_unreachable("Unhandled RangeKind in switch.");
}

SourceKit::UIdent SwiftLangSupport::
getUIDForRegionType(swift::ide::RegionType Type) {
  switch (Type) {
    case swift::ide::RegionType::ActiveCode: return KindEditActive;
    case swift::ide::RegionType::InactiveCode: return KindEditInactive;
    case swift::ide::RegionType::Selector: return KindEditSelector;
    case swift::ide::RegionType::String: return KindEditString;
    case swift::ide::RegionType::Comment: return KindEditComment;
    case swift::ide::RegionType::Mismatch: return KindEditMismatch;
    case swift::ide::RegionType::Unmatched: return KindEditUnknown;
  }
}

SourceKit::UIdent SwiftLangSupport::
getUIDForRefactoringRangeKind(ide::RefactoringRangeKind Kind) {
  switch (Kind) {
  case ide::RefactoringRangeKind::BaseName:
    return KindRenameRangeBase;
  case ide::RefactoringRangeKind::KeywordBaseName:
    return KindRenameRangeKeywordBase;
  case ide::RefactoringRangeKind::ParameterName:
    return KindRenameRangeParam;
  case ide::RefactoringRangeKind::NoncollapsibleParameterName:
    return KindRenameRangeNoncollapsibleParam;
  case ide::RefactoringRangeKind::DeclArgumentLabel:
    return KindRenameRangeDeclArgLabel;
  case ide::RefactoringRangeKind::CallArgumentLabel:
    return KindRenameRangeCallArgLabel;
  case ide::RefactoringRangeKind::CallArgumentColon:
    return KindRenameRangeCallArgColon;
  case ide::RefactoringRangeKind::CallArgumentCombined:
    return KindRenameRangeCallArgCombined;
  case ide::RefactoringRangeKind::SelectorArgumentLabel:
    return KindRenameRangeSelectorArgLabel;
  }
}

UIdent SwiftLangSupport::getUIDForSymbol(SymbolInfo sym, bool isRef) {

#define UID_FOR(CLASS) isRef ? KindRef##CLASS : KindDecl##CLASS;

  switch (sym.SubKind) {
  default: break;
  case SymbolSubKind::AccessorGetter: return UID_FOR(AccessorGetter);
  case SymbolSubKind::AccessorSetter: return UID_FOR(AccessorSetter);
  case SymbolSubKind::SwiftAccessorWillSet: return UID_FOR(AccessorWillSet);
  case SymbolSubKind::SwiftAccessorDidSet: return UID_FOR(AccessorDidSet);
  case SymbolSubKind::SwiftAccessorAddressor: return UID_FOR(AccessorAddress);
  case SymbolSubKind::SwiftAccessorMutableAddressor: return UID_FOR(AccessorMutableAddress);
  }

#define SIMPLE_CASE(KIND) \
  case SymbolKind::KIND: \
    return UID_FOR(KIND);

  switch (sym.Kind) {
  SIMPLE_CASE(Enum)
  SIMPLE_CASE(Struct)
  SIMPLE_CASE(Class)
  SIMPLE_CASE(Protocol)
  SIMPLE_CASE(Constructor)
  SIMPLE_CASE(Destructor)

  case SymbolKind::EnumConstant:
    return UID_FOR(EnumElement);

  case SymbolKind::TypeAlias:
    if (sym.SubKind == SymbolSubKind::SwiftAssociatedType)
      return UID_FOR(AssociatedType);
    if (sym.SubKind == SymbolSubKind::SwiftGenericTypeParam)
      return UID_FOR(GenericTypeParam);
    return UID_FOR(TypeAlias);

  case SymbolKind::Function:
    if (sym.SubKind == SymbolSubKind::SwiftPrefixOperator)
      return UID_FOR(FunctionPrefixOperator);
    if (sym.SubKind == SymbolSubKind::SwiftPostfixOperator)
      return UID_FOR(FunctionPostfixOperator);
    if (sym.SubKind == SymbolSubKind::SwiftInfixOperator)
      return UID_FOR(FunctionInfixOperator);
    return UID_FOR(FunctionFree);
  case SymbolKind::Variable:
    return UID_FOR(VarGlobal);
  case SymbolKind::InstanceMethod:
    return UID_FOR(MethodInstance);
  case SymbolKind::ClassMethod:
    return UID_FOR(MethodClass);
  case SymbolKind::StaticMethod:
    return UID_FOR(MethodStatic);
  case SymbolKind::InstanceProperty:
    if (sym.SubKind == SymbolSubKind::SwiftSubscript)
      return UID_FOR(Subscript);
    return UID_FOR(VarInstance);
  case SymbolKind::ClassProperty:
    return UID_FOR(VarClass);
  case SymbolKind::StaticProperty:
    return UID_FOR(VarStatic);

  case SymbolKind::Extension:
    assert(!isRef && "reference to extension decl?");
    if (sym.SubKind == SymbolSubKind::SwiftExtensionOfStruct) {
      return KindDeclExtensionStruct;
    } else if (sym.SubKind == SymbolSubKind::SwiftExtensionOfClass) {
      return KindDeclExtensionClass;
    } else if (sym.SubKind == SymbolSubKind::SwiftExtensionOfEnum) {
      return KindDeclExtensionEnum;
    } else if (sym.SubKind == SymbolSubKind::SwiftExtensionOfProtocol) {
      return KindDeclExtensionProtocol;
    } else {
      llvm_unreachable("missing extension sub kind");
    }

  case SymbolKind::Module:
    return KindRefModule;

  default:
    // TODO: reconsider whether having a default case is a good idea.
    return UIdent();
  }

#undef SIMPLE_CASE
#undef UID_FOR
}

SourceKit::UIdent SwiftLangSupport::getUIDForNameKind(swift::ide::NameKind Kind) {
  switch(Kind) {
  case swift::ide::NameKind::ObjC: return KindNameObjc;
  case swift::ide::NameKind::Swift: return KindNameSwift;
  }

  llvm_unreachable("Unhandled NameKind in switch.");
}

swift::ide::NameKind SwiftLangSupport::getNameKindForUID(SourceKit::UIdent Id) {
  if (Id == KindNameObjc)
    return swift::ide::NameKind::ObjC;
  assert(Id == KindNameSwift);
  return swift::ide::NameKind::Swift;
}

Optional<UIdent> SwiftLangSupport::getUIDForDeclAttribute(const swift::DeclAttribute *Attr) {
  // Check special-case names first.
  switch (Attr->getKind()) {
    case DAK_IBAction: {
      static UIdent Attr_IBAction("source.decl.attribute.ibaction");
      return Attr_IBAction;
    }
    case DAK_IBOutlet: {
      static UIdent Attr_IBOutlet("source.decl.attribute.iboutlet");
      return Attr_IBOutlet;
    }
    case DAK_IBDesignable: {
      static UIdent Attr_IBDesignable("source.decl.attribute.ibdesignable");
      return Attr_IBDesignable;
    }
    case DAK_IBInspectable: {
      static UIdent Attr_IBInspectable("source.decl.attribute.ibinspectable");
      return Attr_IBInspectable;
    }
    case DAK_GKInspectable: {
      static UIdent Attr_GKInspectable("source.decl.attribute.gkinspectable");
      return Attr_GKInspectable;
    }
    case DAK_ObjC: {
      static UIdent Attr_Objc("source.decl.attribute.objc");
      static UIdent Attr_ObjcNamed("source.decl.attribute.objc.name");
      if (cast<ObjCAttr>(Attr)->hasName()) {
        return Attr_ObjcNamed;
      } else {
        return Attr_Objc;
      }
    }
    case DAK_AccessControl: {
      static UIdent Attr_Private("source.decl.attribute.private");
      static UIdent Attr_FilePrivate("source.decl.attribute.fileprivate");
      static UIdent Attr_Internal("source.decl.attribute.internal");
      static UIdent Attr_Public("source.decl.attribute.public");
      static UIdent Attr_Open("source.decl.attribute.open");

      switch (cast<AbstractAccessControlAttr>(Attr)->getAccess()) {
        case AccessLevel::Private:
          return Attr_Private;
        case AccessLevel::FilePrivate:
          return Attr_FilePrivate;
        case AccessLevel::Internal:
          return Attr_Internal;
        case AccessLevel::Public:
          return Attr_Public;
        case AccessLevel::Open:
          return Attr_Open;
      }
    }
    case DAK_SetterAccess: {
      static UIdent Attr_Private("source.decl.attribute.setter_access.private");
      static UIdent Attr_FilePrivate("source.decl.attribute.setter_access.fileprivate");
      static UIdent Attr_Internal("source.decl.attribute.setter_access.internal");
      static UIdent Attr_Public("source.decl.attribute.setter_access.public");
      static UIdent Attr_Open("source.decl.attribute.setter_access.open");

      switch (cast<AbstractAccessControlAttr>(Attr)->getAccess()) {
        case AccessLevel::Private:
          return Attr_Private;
        case AccessLevel::FilePrivate:
          return Attr_FilePrivate;
        case AccessLevel::Internal:
          return Attr_Internal;
        case AccessLevel::Public:
          return Attr_Public;
        case AccessLevel::Open:
          return Attr_Open;
      }
    }

    // Ignore these.
    case DAK_ShowInInterface:
    case DAK_RawDocComment:
    case DAK_HasInitialValue:
      return None;
    default:
      break;
  }

  switch (Attr->getKind()) {
    case DAK_Count:
      break;
#define DECL_ATTR(X, CLASS, ...)\
    case DAK_##CLASS: {\
      static UIdent Attr_##X("source.decl.attribute."#X); \
      return Attr_##X; \
    }
#include "swift/AST/Attr.def"
  }

  return None;
}

std::vector<UIdent> SwiftLangSupport::UIDsFromDeclAttributes(const DeclAttributes &Attrs) {
  std::vector<UIdent> AttrUIDs;

  for (auto Attr : Attrs) {
    if (auto AttrUID = getUIDForDeclAttribute(Attr)) {
      AttrUIDs.push_back(AttrUID.getValue());
    }
  }

  return AttrUIDs;
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

bool SwiftLangSupport::printDeclTypeUSR(const ValueDecl *D, llvm::raw_ostream &OS) {
  return ide::printDeclTypeUSR(D, OS);
}

bool SwiftLangSupport::printTypeUSR(Type Ty, llvm::raw_ostream &OS) {
  return ide::printTypeUSR(Ty, OS);
}

bool SwiftLangSupport::printAccessorUSR(const AbstractStorageDecl *D,
                                        AccessorKind AccKind,
                                        llvm::raw_ostream &OS) {
  return ide::printAccessorUSR(D, AccKind, OS);
}

std::string SwiftLangSupport::resolvePathSymlinks(StringRef FilePath) {
  std::string InputPath = FilePath;
#if !defined(_WIN32)
  char full_path[MAXPATHLEN];
  if (const char *path = realpath(InputPath.c_str(), full_path))
    return path;

  return InputPath;
#else
  char full_path[MAX_PATH];

  HANDLE fileHandle = CreateFileA(
      InputPath.c_str(), GENERIC_READ, 0, nullptr, OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, nullptr);

  if (fileHandle == INVALID_HANDLE_VALUE)
    return InputPath;

  DWORD success = GetFinalPathNameByHandleA(
      fileHandle, full_path, sizeof(full_path), FILE_NAME_NORMALIZED);
  return (success ? full_path : InputPath);
#endif
}

void SwiftLangSupport::getStatistics(StatisticsReceiver receiver) {
  std::vector<Statistic *> stats = {
#define SWIFT_STATISTIC(VAR, UID, DESC) &Stats->VAR,
#include "SwiftStatistics.def"
  };
  receiver(stats);
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

void SourceKit::disableExpensiveSILOptions(SILOptions &Opts) {
  // Disable the sanitizers.
  Opts.Sanitizers = {};

  // Disable PGO and code coverage.
  Opts.GenerateProfile = false;
  Opts.EmitProfileCoverageMapping = false;
  Opts.UseProfile = "";
}
