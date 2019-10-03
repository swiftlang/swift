//===--- ASTPrinter.cpp - Swift Language AST Printer ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements printing for the Swift ASTs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Comment.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Basic/QuotedString.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Config.h"
#include "swift/Parse/Lexer.h"
#include "swift/Strings.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <queue>

using namespace swift;

void PrintOptions::setBaseType(Type T) {
  TransformContext = TypeTransformContext(T);
}

void PrintOptions::initForSynthesizedExtension(TypeOrExtensionDecl D) {
  TransformContext = TypeTransformContext(D);
}

void PrintOptions::clearSynthesizedExtension() {
  TransformContext.reset();
}

static bool isPublicOrUsableFromInline(const ValueDecl *VD) {
  AccessScope scope =
      VD->getFormalAccessScope(/*useDC*/nullptr,
                               /*treatUsableFromInlineAsPublic*/true);
  return scope.isPublic();
}

static bool isPublicOrUsableFromInline(Type ty) {
  // Note the double negative here: we're looking for any referenced decls that
  // are *not* public-or-usableFromInline.
  return !ty.findIf([](Type typePart) -> bool {
    // FIXME: If we have an internal typealias for a non-internal type, we ought
    // to be able to print it by desugaring.
    if (auto *aliasTy = dyn_cast<TypeAliasType>(typePart.getPointer()))
      return !isPublicOrUsableFromInline(aliasTy->getDecl());
    if (auto *nominal = typePart->getAnyNominal())
      return !isPublicOrUsableFromInline(nominal);
    return false;
  });
}

static bool contributesToParentTypeStorage(const AbstractStorageDecl *ASD) {
  auto *DC = ASD->getDeclContext()->getAsDecl();
  if (!DC) return false;
  auto *ND = dyn_cast<NominalTypeDecl>(DC);
  if (!ND) return false;
  return !ND->isResilient() && ASD->hasStorage() && !ASD->isStatic();
}

PrintOptions PrintOptions::printSwiftInterfaceFile(bool preferTypeRepr) {
  PrintOptions result;
  result.PrintLongAttrsOnSeparateLines = true;
  result.TypeDefinitions = true;
  result.PrintIfConfig = false;
  result.FullyQualifiedTypes = true;
  result.UseExportedModuleNames = true;
  result.AllowNullTypes = false;
  result.SkipImports = true;
  result.OmitNameOfInaccessibleProperties = true;
  result.FunctionDefinitions = true;
  result.CollapseSingleGetterProperty = false;
  result.VarInitializers = true;
  result.EnumRawValues = EnumRawValueMode::PrintObjCOnly;
  result.OpaqueReturnTypePrinting =
      OpaqueReturnTypePrintingMode::StableReference;
  result.PreferTypeRepr = preferTypeRepr;

  // We should print __consuming, __owned, etc for the module interface file.
  result.SkipUnderscoredKeywords = false;

  result.FunctionBody = [](const ValueDecl *decl, ASTPrinter &printer) {
    auto AFD = dyn_cast<AbstractFunctionDecl>(decl);
    if (!AFD)
      return;
    if (AFD->getResilienceExpansion() != ResilienceExpansion::Minimal)
      return;
    if (!AFD->hasInlinableBodyText())
      return;

    SmallString<128> scratch;
    printer << " " << AFD->getInlinableBodyText(scratch);
  };

  class ShouldPrintForModuleInterface : public ShouldPrintChecker {
    bool shouldPrint(const Decl *D, const PrintOptions &options) override {
      // Skip anything that is marked `@_implementationOnly` itself.
      if (D->getAttrs().hasAttribute<ImplementationOnlyAttr>())
        return false;

      // Skip anything that isn't 'public' or '@usableFromInline'.
      if (auto *VD = dyn_cast<ValueDecl>(D)) {
        if (!isPublicOrUsableFromInline(VD)) {
          // We do want to print private stored properties, without their
          // original names present.
          if (auto *ASD = dyn_cast<AbstractStorageDecl>(VD))
            if (contributesToParentTypeStorage(ASD))
              return true;
          return false;
        }
      }

      // Skip extensions that extend things we wouldn't print.
      if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
        if (!shouldPrint(ED->getExtendedNominal(), options))
          return false;
        for (const Requirement &req : ED->getGenericRequirements()) {
          if (!isPublicOrUsableFromInline(req.getFirstType()))
            return false;

          switch (req.getKind()) {
          case RequirementKind::Conformance:
          case RequirementKind::Superclass:
          case RequirementKind::SameType:
            if (!isPublicOrUsableFromInline(req.getSecondType()))
              return false;
            break;
          case RequirementKind::Layout:
            break;
          }
        }
      }

      // Skip typealiases that just redeclare generic parameters.
      if (auto *alias = dyn_cast<TypeAliasDecl>(D)) {
        if (alias->isImplicit()) {
          const Decl *parent =
              D->getDeclContext()->getAsDecl();
          if (auto *genericCtx = parent->getAsGenericContext()) {
            bool matchesGenericParam =
                llvm::any_of(genericCtx->getInnermostGenericParamTypes(),
                             [alias](const GenericTypeParamType *param) {
              return param->getName() == alias->getName();
            });
            if (matchesGenericParam)
              return false;
          }
        }
      }

      return ShouldPrintChecker::shouldPrint(D, options);
    }
  };
  result.CurrentPrintabilityChecker =
      std::make_shared<ShouldPrintForModuleInterface>();

  // FIXME: We don't really need 'public' on everything; we could just change
  // the default to 'public' and mark the 'internal' things.
  result.PrintAccess = true;

  result.ExcludeAttrList = {DAK_AccessControl, DAK_SetterAccess, DAK_Lazy};

  return result;
}

TypeTransformContext::TypeTransformContext(Type T)
    : BaseType(T.getPointer()) {
  assert(T->mayHaveMembers());
}

TypeTransformContext::TypeTransformContext(TypeOrExtensionDecl D)
    : BaseType(nullptr), Decl(D) {
  if (auto NTD = Decl.Decl.dyn_cast<NominalTypeDecl *>())
    BaseType = NTD->getDeclaredTypeInContext().getPointer();
  else {
    auto *ED = Decl.Decl.get<ExtensionDecl *>();
    BaseType = ED->getDeclaredTypeInContext().getPointer();
  }
}

TypeOrExtensionDecl TypeTransformContext::getDecl() const { return Decl; }

DeclContext *TypeTransformContext::getDeclContext() const {
  return Decl.getAsDecl()->getDeclContext();
}

Type TypeTransformContext::getBaseType() const {
  return Type(BaseType);
}

bool TypeTransformContext::isPrintingSynthesizedExtension() const {
  return !Decl.isNull();
}

std::string ASTPrinter::sanitizeUtf8(StringRef Text) {
  llvm::SmallString<256> Builder;
  Builder.reserve(Text.size());
  const llvm::UTF8* Data = reinterpret_cast<const llvm::UTF8*>(Text.begin());
  const llvm::UTF8* End = reinterpret_cast<const llvm::UTF8*>(Text.end());
  StringRef Replacement = u8"\ufffd";
  while (Data < End) {
    auto Step = llvm::getNumBytesForUTF8(*Data);
    if (Data + Step > End) {
      Builder.append(Replacement);
      break;
    }

    if (llvm::isLegalUTF8Sequence(Data, Data + Step)) {
      Builder.append(Data, Data + Step);
    } else {

      // If malformed, add replacement characters.
      Builder.append(Replacement);
    }
    Data += Step;
  }
  return Builder.str();
}

void ASTPrinter::anchor() {}

void ASTPrinter::printIndent() {
  llvm::SmallString<16> Str;
  for (unsigned i = 0; i != CurrentIndentation; ++i)
    Str += ' ';

  printText(Str);
}

void ASTPrinter::printTextImpl(StringRef Text) {
  forceNewlines();
  printText(Text);
}

void ASTPrinter::printEscapedStringLiteral(StringRef str) {
  SmallString<128> encodeBuf;
  StringRef escaped =
    Lexer::getEncodedStringSegment(str, encodeBuf,
                                   /*isFirstSegment*/true,
                                   /*isLastSegment*/true,
                                   /*indentToStrip*/~0U /* sentinel */);

  // FIXME: This is wasteful, but ASTPrinter is an abstract class that doesn't
  //        have a directly-accessible ostream.
  SmallString<128> escapeBuf;
  llvm::raw_svector_ostream os(escapeBuf);
  os << QuotedString(escaped);
  printTextImpl(escapeBuf.str());
}

void ASTPrinter::printTypeRef(Type T, const TypeDecl *RefTo, Identifier Name,
                              PrintNameContext Context) {
  if (isa<GenericTypeParamDecl>(RefTo)) {
    Context = PrintNameContext::GenericParameter;
  } else if (T && T->is<DynamicSelfType>()) {
    assert(T->castTo<DynamicSelfType>()->getSelfType()->getAnyNominal() &&
           "protocol Self handled as GenericTypeParamDecl");
    Context = PrintNameContext::ClassDynamicSelf;
  }

  printName(Name, Context);
}

void ASTPrinter::printModuleRef(ModuleEntity Mod, Identifier Name) {
  printName(Name);
}

void ASTPrinter::callPrintDeclPre(const Decl *D,
                                  Optional<BracketOptions> Bracket) {
  forceNewlines();

  if (SynthesizeTarget && isa<ExtensionDecl>(D))
    printSynthesizedExtensionPre(cast<ExtensionDecl>(D), SynthesizeTarget, Bracket);
  else
    printDeclPre(D, Bracket);
}

ASTPrinter &ASTPrinter::operator<<(unsigned long long N) {
  llvm::SmallString<32> Str;
  llvm::raw_svector_ostream OS(Str);
  OS << N;
  printTextImpl(OS.str());
  return *this;
}

ASTPrinter &ASTPrinter::operator<<(UUID UU) {
  llvm::SmallString<UUID::StringBufferSize> Str;
  UU.toString(Str);
  printTextImpl(Str);
  return *this;
}

ASTPrinter &ASTPrinter::operator<<(DeclName name) {
  llvm::SmallString<32> str;
  llvm::raw_svector_ostream os(str);
  name.print(os);
  printTextImpl(os.str());
  return *this;
}

llvm::raw_ostream &swift::
operator<<(llvm::raw_ostream &OS, tok keyword) {
  switch (keyword) {
#define KEYWORD(KW) case tok::kw_##KW: OS << #KW; break;
#define POUND_KEYWORD(KW) case tok::pound_##KW: OS << "#"#KW; break;
#define PUNCTUATOR(PUN, TEXT) case tok::PUN: OS << TEXT; break;
#include "swift/Syntax/TokenKinds.def"
  default:
    llvm_unreachable("unexpected keyword or punctuator kind");
  }
  return OS;
}

uint8_t swift::getKeywordLen(tok keyword) {
  switch (keyword) {
#define KEYWORD(KW) case tok::kw_##KW: return StringRef(#KW).size();
#define POUND_KEYWORD(KW) case tok::pound_##KW: return StringRef("#"#KW).size();
#define PUNCTUATOR(PUN, TEXT) case tok::PUN: return StringRef(TEXT).size();
#include "swift/Syntax/TokenKinds.def"
  default:
    llvm_unreachable("unexpected keyword or punctuator kind");
  }
}

StringRef swift::getCodePlaceholder() { return "<#code#>"; }

ASTPrinter &operator<<(ASTPrinter &printer, tok keyword) {
  SmallString<16> Buffer;
  llvm::raw_svector_ostream OS(Buffer);
  OS << keyword;
  printer.printKeyword(Buffer.str(), PrintOptions());
  return printer;
}

/// Determine whether to escape the given keyword in the given context.
static bool escapeKeywordInContext(StringRef keyword, PrintNameContext context){

  bool isKeyword = llvm::StringSwitch<bool>(keyword)
#define KEYWORD(KW) \
      .Case(#KW, true)
#include "swift/Syntax/TokenKinds.def"
      .Default(false);

  switch (context) {
  case PrintNameContext::Normal:
  case PrintNameContext::Attribute:
    return isKeyword;
  case PrintNameContext::Keyword:
    return false;

  case PrintNameContext::ClassDynamicSelf:
  case PrintNameContext::GenericParameter:
    return isKeyword && keyword != "Self";

  case PrintNameContext::TypeMember:
    return isKeyword || !canBeMemberName(keyword);

  case PrintNameContext::FunctionParameterExternal:
  case PrintNameContext::FunctionParameterLocal:
  case PrintNameContext::TupleElement:
    return !canBeArgumentLabel(keyword);
  }

  llvm_unreachable("Unhandled PrintNameContext in switch.");
}

void ASTPrinter::printName(Identifier Name, PrintNameContext Context) {
  callPrintNamePre(Context);

  if (Name.empty()) {
    *this << "_";
    printNamePost(Context);
    return;
  }

  bool shouldEscapeKeyword = escapeKeywordInContext(Name.str(), Context);

  if (shouldEscapeKeyword)
    *this << "`";
  *this << Name.str();
  if (shouldEscapeKeyword)
    *this << "`";

  printNamePost(Context);
}

void StreamPrinter::printText(StringRef Text) {
  OS << Text;
}

/// Whether we will be printing a TypeLoc by using the TypeRepr printer
static bool willUseTypeReprPrinting(TypeLoc tyLoc,
                                    Type currentType,
                                    PrintOptions options) {
  // Special case for when transforming archetypes
  if (currentType && tyLoc.getType())
    return false;

  return ((options.PreferTypeRepr && tyLoc.hasLocation()) ||
          (tyLoc.getType().isNull() && tyLoc.getTypeRepr()));
}

namespace {
/// AST pretty-printer.
class PrintAST : public ASTVisitor<PrintAST> {
  ASTPrinter &Printer;
  PrintOptions Options;
  unsigned IndentLevel = 0;
  Decl *Current = nullptr;
  Type CurrentType;

  friend DeclVisitor<PrintAST>;

  /// RAII object that increases the indentation level.
  class IndentRAII {
    PrintAST &Self;
    bool DoIndent;

  public:
    IndentRAII(PrintAST &self, bool DoIndent = true)
        : Self(self), DoIndent(DoIndent) {
      if (DoIndent)
        Self.IndentLevel += Self.Options.Indent;
    }

    ~IndentRAII() {
      if (DoIndent)
        Self.IndentLevel -= Self.Options.Indent;
    }
  };

  /// Indent the current number of indentation spaces.
  void indent() {
    Printer.setIndent(IndentLevel);
  }

  /// Record the location of this declaration, which is about to
  /// be printed, marking the name and signature end locations.
  template<typename FnTy>
  void recordDeclLoc(Decl *decl, const FnTy &NameFn,
                     llvm::function_ref<void()> ParamFn = []{}) {
    Printer.callPrintDeclLoc(decl);
    NameFn();
    Printer.printDeclNameEndLoc(decl);
    ParamFn();
    Printer.printDeclNameOrSignatureEndLoc(decl);
  }

  void printSourceRange(CharSourceRange Range, ASTContext &Ctx) {
    Printer << Ctx.SourceMgr.extractText(Range);
  }

  static std::string sanitizeClangDocCommentStyle(StringRef Line) {
    static StringRef ClangStart = "/*!";
    static StringRef SwiftStart = "/**";
    auto Pos = Line.find(ClangStart);
    if (Pos == StringRef::npos)
      return Line.str();
    StringRef Segment[2];
    // The text before "/*!"
    Segment[0] = Line.substr(0, Pos);
    // The text after "/*!"
    Segment[1] = Line.substr(Pos).substr(ClangStart.size());
    // Only sanitize when "/*!" appears at the start of this line.
    if (Segment[0].trim().empty()) {
      return (llvm::Twine(Segment[0]) + SwiftStart + Segment[1]).str();
    }
    return Line.str();
  }

  void printClangDocumentationComment(const clang::Decl *D) {
    const auto &ClangContext = D->getASTContext();
    const clang::RawComment *RC = ClangContext.getRawCommentForAnyRedecl(D);
    if (!RC)
      return;

    bool Invalid;
    unsigned StartLocCol =
        ClangContext.getSourceManager().getSpellingColumnNumber(
            RC->getBeginLoc(), &Invalid);
    if (Invalid)
      StartLocCol = 0;

    unsigned WhitespaceToTrim = StartLocCol ? StartLocCol - 1 : 0;

    SmallVector<StringRef, 8> Lines;

    StringRef RawText =
        RC->getRawText(ClangContext.getSourceManager()).rtrim("\n\r");
    trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);
    bool FirstLine = true;
    for (auto Line : Lines) {
      if (FirstLine)
        Printer << sanitizeClangDocCommentStyle(ASTPrinter::sanitizeUtf8(Line));
      else
        Printer << ASTPrinter::sanitizeUtf8(Line);
      Printer.printNewline();
      FirstLine = false;
    }
  }

  void printRawComment(RawComment RC) {
    indent();

    SmallVector<StringRef, 8> Lines;
    for (const auto &SRC : RC.Comments) {
      Lines.clear();

      StringRef RawText = SRC.RawText.rtrim("\n\r");
      unsigned WhitespaceToTrim = SRC.StartColumn - 1;
      trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);

      for (auto Line : Lines) {
        Printer << Line;
        Printer.printNewline();
      }
    }
  }

  void printSwiftDocumentationComment(const Decl *D) {
    if (Options.CascadeDocComment)
      D = getDocCommentProvidingDecl(D);
    if (!D)
      return;
    auto RC = D->getRawComment();
    if (RC.isEmpty())
      return;
    printRawComment(RC);
  }

  void printDocumentationComment(const Decl *D) {
    if (!Options.PrintDocumentationComments)
      return;

    // Try to print a comment from Clang.
    auto MaybeClangNode = D->getClangNode();
    if (MaybeClangNode) {
      if (auto *CD = MaybeClangNode.getAsDecl())
        printClangDocumentationComment(CD);
      return;
    }

    printSwiftDocumentationComment(D);
  }

  void printStaticKeyword(StaticSpellingKind StaticSpelling) {
    switch (StaticSpelling) {
    case StaticSpellingKind::None:
      llvm_unreachable("should not be called for non-static decls");
    case StaticSpellingKind::KeywordStatic:
      Printer << tok::kw_static << " ";
      break;
    case StaticSpellingKind::KeywordClass:
      Printer << tok::kw_class << " ";
      break;
    }
  }

  void printAccess(AccessLevel access, StringRef suffix = "") {
    switch (access) {
    case AccessLevel::Private:
      Printer << tok::kw_private;
      break;
    case AccessLevel::FilePrivate:
      Printer << tok::kw_fileprivate;
      break;
    case AccessLevel::Internal:
      if (!Options.PrintInternalAccessKeyword)
        return;
      Printer << tok::kw_internal;
      break;
    case AccessLevel::Public:
      Printer << tok::kw_public;
      break;
    case AccessLevel::Open:
      Printer.printKeyword("open", Options);
      break;
    }
    Printer << suffix << " ";
  }

  void printAccess(const ValueDecl *D) {
    assert(!llvm::is_contained(Options.ExcludeAttrList, DAK_AccessControl) ||
           llvm::is_contained(Options.ExcludeAttrList, DAK_SetterAccess));

    if (!Options.PrintAccess || isa<ProtocolDecl>(D->getDeclContext()))
      return;
    if (D->getAttrs().hasAttribute<AccessControlAttr>() &&
        !llvm::is_contained(Options.ExcludeAttrList, DAK_AccessControl))
      return;

    printAccess(D->getFormalAccess());
    bool shouldSkipSetterAccess =
      llvm::is_contained(Options.ExcludeAttrList, DAK_SetterAccess);

    if (auto storageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      if (auto setter = storageDecl->getAccessor(AccessorKind::Set)) {
        AccessLevel setterAccess = setter->getFormalAccess();
        if (setterAccess != D->getFormalAccess() && !shouldSkipSetterAccess)
          printAccess(setterAccess, "(set)");
      }
    }
  }

  void printTypeWithOptions(Type T, PrintOptions options) {
    if (options.TransformContext) {
      // FIXME: it's not clear exactly what we want to keep from the existing
      // options, and what we want to discard.
      PrintOptions FreshOptions;
      FreshOptions.ExcludeAttrList = options.ExcludeAttrList;
      FreshOptions.ExclusiveAttrList = options.ExclusiveAttrList;
      FreshOptions.PrintOptionalAsImplicitlyUnwrapped = options.PrintOptionalAsImplicitlyUnwrapped;
      T.print(Printer, FreshOptions);
      return;
    }

    T.print(Printer, options);
  }

  void printType(Type T) { printTypeWithOptions(T, Options); }

  void printTransformedTypeWithOptions(Type T, PrintOptions options) {
    if (CurrentType) {
      if (T->hasArchetype()) {
        // Get the interface type, since TypeLocs still have
        // contextual types in them.
        T = T->mapTypeOutOfContext();
      }

      auto *M = Current->getDeclContext()->getParentModule();
      SubstitutionMap subMap;

      if (auto *NTD = dyn_cast<NominalTypeDecl>(Current))
        subMap = CurrentType->getContextSubstitutionMap(M, NTD);
      else if (auto *ED = dyn_cast<ExtensionDecl>(Current))
        subMap = CurrentType->getContextSubstitutionMap(M, ED);
      else {
        subMap = CurrentType->getMemberSubstitutionMap(
          M, cast<ValueDecl>(Current));
      }

      T = T.subst(subMap, SubstFlags::DesugarMemberTypes);
    }

    printTypeWithOptions(T, options);
  }

  void printTransformedType(Type T) {
    printTransformedTypeWithOptions(T, Options);
  }

  void printTypeLocWithOptions(const TypeLoc &TL, PrintOptions options) {
    if (CurrentType && TL.getType()) {
      printTransformedTypeWithOptions(TL.getType(), options);
      return;
    }

    // Print a TypeRepr if instructed to do so by options, or if the type
    // is null.
    if (willUseTypeReprPrinting(TL, CurrentType, options)) {
      if (auto repr = TL.getTypeRepr())
        repr->print(Printer, options);
      return;
    }

    TL.getType().print(Printer, options);
  }

  void printTypeLoc(const TypeLoc &TL) { printTypeLocWithOptions(TL, Options); }

  void printTypeLocForImplicitlyUnwrappedOptional(TypeLoc TL, bool IUO) {
    PrintOptions options = Options;
    options.PrintOptionalAsImplicitlyUnwrapped = IUO;
    printTypeLocWithOptions(TL, options);
  }

  void printContextIfNeeded(const Decl *decl) {
    if (IndentLevel > 0)
      return;

    switch (Options.ShouldQualifyNestedDeclarations) {
    case PrintOptions::QualifyNestedDeclarations::Never:
      return;
    case PrintOptions::QualifyNestedDeclarations::TypesOnly:
      if (!isa<TypeDecl>(decl))
        return;
      break;
    case PrintOptions::QualifyNestedDeclarations::Always:
      break;
    }

    auto *container = dyn_cast<NominalTypeDecl>(decl->getDeclContext());
    if (!container)
      return;
    printType(container->getDeclaredInterfaceType());
    Printer << ".";
  }

  void printAttributes(const Decl *D);
  void printTypedPattern(const TypedPattern *TP);
  void printBraceStmt(const BraceStmt *stmt, bool newlineIfEmpty = true);
  void printAccessorDecl(const AccessorDecl *decl);

public:
  void printPattern(const Pattern *pattern);

  enum GenericSignatureFlags {
    PrintParams = 1,
    PrintRequirements = 2,
    InnermostOnly = 4,
    SwapSelfAndDependentMemberType = 8,
    PrintInherited = 16,
  };

  void printInheritedFromRequirementSignature(ProtocolDecl *proto,
                                              Decl *attachingTo);
  void printWhereClauseFromRequirementSignature(ProtocolDecl *proto,
                                                Decl *attachingTo);
  void printTrailingWhereClause(TrailingWhereClause *whereClause);

  void printGenericSignature(GenericSignature genericSig,
                             unsigned flags);
  void
  printGenericSignature(GenericSignature genericSig, unsigned flags,
                        llvm::function_ref<bool(const Requirement &)> filter);
  void printSingleDepthOfGenericSignature(
      TypeArrayView<GenericTypeParamType> genericParams,
      ArrayRef<Requirement> requirements, unsigned flags,
      llvm::function_ref<bool(const Requirement &)> filter);
  void printRequirement(const Requirement &req);

private:
  bool shouldPrint(const Decl *D, bool Notify = false);
  bool shouldPrintPattern(const Pattern *P);
  void printPatternType(const Pattern *P);
  void printAccessors(const AbstractStorageDecl *ASD);
  void printMutatingModifiersIfNeeded(const AccessorDecl *accessor);
  void printMembersOfDecl(Decl * NTD, bool needComma = false,
                          bool openBracket = true, bool closeBracket = true);
  void printMembers(ArrayRef<Decl *> members, bool needComma = false,
                    bool openBracket = true, bool closeBracket = true);
  void printGenericDeclGenericParams(GenericContext *decl);
  void printGenericDeclGenericRequirements(GenericContext *decl);
  void printInherited(const Decl *decl);
  void printBodyIfNecessary(const AbstractFunctionDecl *decl);

  void printEnumElement(EnumElementDecl *elt);

  /// \returns true if anything was printed.
  bool printASTNodes(const ArrayRef<ASTNode> &Elements, bool NeedIndent = true);

  void printOneParameter(const ParamDecl *param, ParameterTypeFlags paramFlags,
                         bool ArgNameIsAPIByDefault);

  void printParameterList(ParameterList *PL,
                          ArrayRef<AnyFunctionType::Param> params,
                          bool isAPINameByDefault);

  /// Print the function parameters in curried or selector style,
  /// to match the original function declaration.
  void printFunctionParameters(AbstractFunctionDecl *AFD);

#define DECL(Name,Parent) void visit##Name##Decl(Name##Decl *decl);
#define ABSTRACT_DECL(Name, Parent)
#define DECL_RANGE(Name,Start,End)
#include "swift/AST/DeclNodes.def"

#define STMT(Name, Parent) void visit##Name##Stmt(Name##Stmt *stmt);
#include "swift/AST/StmtNodes.def"

  void printSynthesizedExtension(Type ExtendedType, ExtensionDecl *ExtDecl);

  void printExtension(ExtensionDecl* ExtDecl);

public:
  PrintAST(ASTPrinter &Printer, const PrintOptions &Options)
      : Printer(Printer), Options(Options) {
    if (Options.TransformContext)
      CurrentType = Options.TransformContext->getBaseType();
  }

  using ASTVisitor::visit;

  bool visit(Decl *D) {
    #if SWIFT_BUILD_ONLY_SYNTAXPARSERLIB
      return false; // not needed for the parser library.
    #endif

    if (!shouldPrint(D, true))
      return false;

    Decl *Old = Current;
    Current = D;
    SWIFT_DEFER { Current = Old; };

    Type OldType = CurrentType;
    if (CurrentType && (Old != nullptr || Options.PrintAsMember)) {
      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        auto Subs = CurrentType->getContextSubstitutionMap(
          Options.CurrentModule, NTD->getDeclContext());
        CurrentType = NTD->getDeclaredInterfaceType().subst(Subs);
      }
    }

    SWIFT_DEFER { CurrentType = OldType; };

    bool Synthesize =
        Options.TransformContext &&
        Options.TransformContext->isPrintingSynthesizedExtension() &&
        isa<ExtensionDecl>(D);
    if (Synthesize) {
      Printer.setSynthesizedTarget(Options.TransformContext->getDecl());
    }

    // We want to print a newline before doc comments.  Swift code already
    // handles this, but we need to insert it for clang doc comments when not
    // printing other clang comments. Do it now so the printDeclPre callback
    // happens after the newline.
    if (Options.PrintDocumentationComments &&
        !Options.PrintRegularClangComments &&
        D->hasClangNode()) {
      auto clangNode = D->getClangNode();
      auto clangDecl = clangNode.getAsDecl();
      if (clangDecl &&
          clangDecl->getASTContext().getRawCommentForAnyRedecl(clangDecl)) {
        Printer.printNewline();
        indent();
      }
    }

    Printer.callPrintDeclPre(D, Options.BracketOptions);

    ASTVisitor::visit(D);

    if (Synthesize) {
      Printer.setSynthesizedTarget({});
      Printer.printSynthesizedExtensionPost(cast<ExtensionDecl>(D),
                                            Options.TransformContext->getDecl(),
                                            Options.BracketOptions);
    } else {
      Printer.callPrintDeclPost(D, Options.BracketOptions);
    }

    return true;
  }

};
} // unnamed namespace

static StaticSpellingKind getCorrectStaticSpelling(const Decl *D) {
  if (auto *ASD = dyn_cast<AbstractStorageDecl>(D)) {
    return ASD->getCorrectStaticSpelling();
  } else if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    return PBD->getCorrectStaticSpelling();
  } else if (auto *FD = dyn_cast<FuncDecl>(D)) {
    return FD->getCorrectStaticSpelling();
  } else {
    return StaticSpellingKind::None;
  }
}

static bool hasMutatingGetter(const AbstractStorageDecl *ASD) {
  return ASD->getAccessor(AccessorKind::Get) && ASD->isGetterMutating();
}

static bool hasNonMutatingSetter(const AbstractStorageDecl *ASD) {
  if (!ASD->isSettable(nullptr)) return false;
  auto setter = ASD->getAccessor(AccessorKind::Set);
  return setter && setter->isExplicitNonMutating();
}

static bool hasLessAccessibleSetter(const AbstractStorageDecl *ASD) {
  return ASD->getSetterFormalAccess() < ASD->getFormalAccess();
}

void PrintAST::printAttributes(const Decl *D) {
  if (Options.SkipAttributes)
    return;

  // Save the current number of exclude attrs to restore once we're done.
  unsigned originalExcludeAttrCount = Options.ExcludeAttrList.size();

  if (Options.PrintImplicitAttrs) {

    // Don't print a redundant 'final' if we are printing a 'static' decl.
    if (D->getDeclContext()->getSelfClassDecl() &&
        getCorrectStaticSpelling(D) == StaticSpellingKind::KeywordStatic) {
      Options.ExcludeAttrList.push_back(DAK_Final);
    }

    if (auto vd = dyn_cast<VarDecl>(D)) {
      // Don't print @_hasInitialValue if we're printing an initializer
      // expression or if the storage is resilient.
      if (vd->isInitExposedToClients() || vd->isResilient())
        Options.ExcludeAttrList.push_back(DAK_HasInitialValue);

      if (!Options.PrintForSIL) {
        // Don't print @_hasStorage if the value is simply stored, or the
        // decl is resilient.
        if (vd->isResilient() ||
            (vd->getImplInfo().isSimpleStored() &&
             !hasLessAccessibleSetter(vd)))
          Options.ExcludeAttrList.push_back(DAK_HasStorage);
      }
    }

    // Don't print any contextual decl modifiers.
    // We will handle 'mutating' and 'nonmutating' separately.
    if (isa<AccessorDecl>(D)) {
#define EXCLUDE_ATTR(Class) Options.ExcludeAttrList.push_back(DAK_##Class);
#define CONTEXTUAL_DECL_ATTR(X, Class, Y, Z) EXCLUDE_ATTR(Class)
#define CONTEXTUAL_SIMPLE_DECL_ATTR(X, Class, Y, Z) EXCLUDE_ATTR(Class)
#define CONTEXTUAL_DECL_ATTR_ALIAS(X, Class) EXCLUDE_ATTR(Class)
#include "swift/AST/Attr.def"
    }

    // If the declaration is implicitly @objc, print the attribute now.
    if (auto VD = dyn_cast<ValueDecl>(D)) {
      if (VD->isObjC() && !isa<EnumElementDecl>(VD) &&
          !VD->getAttrs().hasAttribute<ObjCAttr>()) {
        Printer.printAttrName("@objc");
        Printer << " ";
      }
    }
  }

  D->getAttrs().print(Printer, Options, D);

  // Print the implicit 'final' attribute.
  if (auto VD = dyn_cast<ValueDecl>(D)) {
    auto VarD = dyn_cast<VarDecl>(D);
    if (VD->isFinal() &&
        !VD->getAttrs().hasAttribute<FinalAttr>() &&
        // Don't print a redundant 'final' if printing a 'let' or 'static' decl.
        !(VarD && VarD->isLet()) &&
        getCorrectStaticSpelling(D) != StaticSpellingKind::KeywordStatic &&
        VD->getKind() != DeclKind::Accessor) {
      Printer.printAttrName("final");
      Printer << " ";
    }
  }

  // Explicitly print 'mutating' and 'nonmutating' before getters and setters
  // for which that is true.
  if (auto accessor = dyn_cast<AccessorDecl>(D)) {
    printMutatingModifiersIfNeeded(accessor);
  }

  Options.ExcludeAttrList.resize(originalExcludeAttrCount);
}

void PrintAST::printTypedPattern(const TypedPattern *TP) {
  printPattern(TP->getSubPattern());
  Printer << ": ";

  // Make sure to check if the underlying var decl is an implicitly unwrapped
  // optional.
  bool isIUO = false;
  if (auto *named = dyn_cast<NamedPattern>(TP->getSubPattern()))
    if (auto decl = named->getDecl())
      isIUO = decl->isImplicitlyUnwrappedOptional();

  printTypeLocForImplicitlyUnwrappedOptional(TP->getTypeLoc(), isIUO);
}

/// Determines if we are required to print the name of a property declaration,
/// or if we can elide it by printing a '_' instead.
static bool mustPrintPropertyName(VarDecl *decl, PrintOptions opts) {
  // If we're not allowed to omit the name, we must print it.
  if (!opts.OmitNameOfInaccessibleProperties) return true;

  // If it contributes to the parent's storage, we must print it because clients
  // need to be able to directly access the storage.
  // FIXME: We might be able to avoid printing names for some of these
  //        if we serialized references to them using field indices.
  if (contributesToParentTypeStorage(decl)) return true;

  // If it's public or @usableFromInline, we must print the name because it's a
  // visible entry-point.
  if (isPublicOrUsableFromInline(decl)) return true;

  // If it has an initial value, we must print the name because it's used in
  // the mangled name of the initializer expression generator function.
  // FIXME: We _could_ figure out a way to generate an entry point
  //        for the initializer expression without revealing the name. We just
  //        don't have a mangling for it.
  if (decl->hasInitialValue()) return true;

  // If none of those are true, we can elide the name of the variable.
  return false;
}

/// Gets the print name context of a given decl, choosing between TypeMember
/// and Normal, depending if this decl lives in a nominal type decl.
static PrintNameContext getTypeMemberPrintNameContext(const Decl *d) {
  return d->getDeclContext()->isTypeContext() ?
      PrintNameContext::TypeMember :
      PrintNameContext::Normal;
}

void PrintAST::printPattern(const Pattern *pattern) {
  switch (pattern->getKind()) {
  case PatternKind::Any:
    Printer << "_";
    break;

  case PatternKind::Named: {
    auto named = cast<NamedPattern>(pattern);
    auto decl = named->getDecl();
    recordDeclLoc(decl, [&]{
      // FIXME: This always returns true now, because of the FIXMEs listed in
      //        mustPrintPropertyName.
      if (mustPrintPropertyName(decl, Options))
        Printer.printName(named->getBoundName(),
                          getTypeMemberPrintNameContext(decl));
      else
        Printer << "_";
    });
    break;
  }

  case PatternKind::Paren:
    Printer << "(";
    printPattern(cast<ParenPattern>(pattern)->getSubPattern());
    Printer << ")";
    break;

  case PatternKind::Tuple: {
    Printer << "(";
    auto TP = cast<TuplePattern>(pattern);
    auto Fields = TP->getElements();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      const auto &Elt = Fields[i];
      if (i != 0)
        Printer << ", ";

      printPattern(Elt.getPattern());
    }
    Printer << ")";
    break;
  }

  case PatternKind::Typed:
    printTypedPattern(cast<TypedPattern>(pattern));
    break;

  case PatternKind::Is: {
    auto isa = cast<IsPattern>(pattern);
    Printer << tok::kw_is << " ";
    isa->getCastTypeLoc().getType().print(Printer, Options);
    break;
  }

  case PatternKind::EnumElement: {
    auto elt = cast<EnumElementPattern>(pattern);
    // FIXME: Print element expr.
    if (elt->hasSubPattern())
      printPattern(elt->getSubPattern());
    break;
  }

  case PatternKind::OptionalSome:
    printPattern(cast<OptionalSomePattern>(pattern)->getSubPattern());
    Printer << '?';
    break;

  case PatternKind::Bool:
    Printer << (cast<BoolPattern>(pattern)->getValue() ? tok::kw_true
                                                       : tok::kw_false);
    break;

  case PatternKind::Expr:
    // FIXME: Print expr.
    break;

  case PatternKind::Var:
    if (!Options.SkipIntroducerKeywords)
      Printer << (cast<VarPattern>(pattern)->isLet() ? tok::kw_let
                                                     : tok::kw_var)
              << " ";
    printPattern(cast<VarPattern>(pattern)->getSubPattern());
  }
}

/// If we can't find the depth of a type, return ErrorDepth.
static const unsigned ErrorDepth = ~0U;
/// A helper function to return the depth of a type.
static unsigned getDepthOfType(Type ty) {
  unsigned depth = ErrorDepth;

  auto combineDepth = [&depth](unsigned newDepth) -> bool {
    // If there is no current depth (depth == ErrorDepth), then assign to
    // newDepth; otherwise, choose the deeper of the current and new depth.

    // Since ErrorDepth == ~0U, ErrorDepth + 1 == 0, which is smaller than any
    // valid depth + 1.
    depth = std::max(depth+1U, newDepth+1U) - 1U;
    return false;
  };

  ty.findIf([combineDepth](Type t) -> bool {
    if (auto paramTy = t->getAs<GenericTypeParamType>())
      return combineDepth(paramTy->getDepth());

    if (auto depMemTy = dyn_cast<DependentMemberType>(t->getCanonicalType())) {
      CanType rootTy;
      do {
        rootTy = depMemTy.getBase();
      } while ((depMemTy = dyn_cast<DependentMemberType>(rootTy)));
      if (auto rootParamTy = dyn_cast<GenericTypeParamType>(rootTy))
        return combineDepth(rootParamTy->getDepth());
    }

    return false;
  });

  return depth;
}

namespace {
struct RequirementPrintLocation {
  /// The Decl where the requirement should be attached (whether inherited or in
  /// a where clause)
  Decl *AttachedTo;
  /// Whether the requirement needs to be in a where clause.
  bool InWhereClause;
};
} // end anonymous namespace

/// Heuristically work out a good place for \c req to be printed inside \c
/// proto.
///
/// This depends only on the protocol so that we make the same decisions for all
/// requirements in all associated types, guaranteeing that all of them will be
/// printed somewhere. That is, taking an AssociatedTypeDecl as an argument and
/// asking "should this requirement be printed on this ATD?" seems more likely
/// to result in inconsistencies in what is printed where, versus what this
/// function does: asking "where should this requirement be printed?" and then
/// callers check if the location is the ATD.
static RequirementPrintLocation
bestRequirementPrintLocation(ProtocolDecl *proto, const Requirement &req) {
  auto protoSelf = proto->getProtocolSelfType();
  // Returns the most relevant decl within proto connected to outerType (or null
  // if one doesn't exist), and whether the type is an "direct use",
  // i.e. outerType itself is Self or Self.T, but not, say, Self.T.U, or
  // Array<Self.T>. (The first's decl will be proto, while the other three will
  // be Self.T.)
  auto findRelevantDeclAndDirectUse = [&](Type outerType) {
    TypeDecl *relevantDecl = nullptr;
    Type foundType;
    (void)outerType.findIf([&](Type t) {
      if (t->isEqual(protoSelf)) {
        relevantDecl = proto;
        foundType = t;
        return true;
      } else if (auto DMT = t->getAs<DependentMemberType>()) {
        auto assocType = DMT->getAssocType();
        if (assocType && assocType->getProtocol() == proto) {
          relevantDecl = assocType;
          foundType = t;
          return true;
        }
      }

      // not here, so let's keep looking.
      return false;
    });

    // If we didn't find anything, relevantDecl and foundType will be null, as
    // desired.
    auto directUse = foundType && outerType->isEqual(foundType);
    return std::make_pair(relevantDecl, directUse);
  };

  Decl *bestDecl;
  bool inWhereClause;

  switch (req.getKind()) {
  case RequirementKind::Conformance:
  case RequirementKind::Superclass:
  case RequirementKind::Layout: {
    auto subject = req.getFirstType();
    auto result = findRelevantDeclAndDirectUse(subject);

    bestDecl = result.first;
    inWhereClause = !bestDecl || !result.second;
    break;
  }
  case RequirementKind::SameType: {
    auto lhs = req.getFirstType();
    auto rhs = req.getSecondType();

    auto lhsResult = findRelevantDeclAndDirectUse(lhs);
    auto rhsResult = findRelevantDeclAndDirectUse(rhs);

    // Default to using the left type's decl.
    bestDecl = lhsResult.first;

    // But maybe the right type's one is "obviously" better!
    // e.g. Int == Self.T
    auto lhsDoesntExist = !lhsResult.first;
    // e.g. Self.T.U == Self.V should go on V (first two conditions), but
    // Self.T.U == Self should go on T (third condition).
    auto rhsBetterDirect =
        !lhsResult.second && rhsResult.second && rhsResult.first != proto;
    auto rhsOfSelfToAssoc = lhsResult.first == proto && rhsResult.first;
    // e.g. Self == Self.T.U
    if (lhsDoesntExist || rhsBetterDirect || rhsOfSelfToAssoc)
      bestDecl = rhsResult.first;

    // Same-type requirements can only occur in where clauses
    inWhereClause = true;
    break;
  }
  }
  // Didn't find anything that we think is relevant, so let's default to a where
  // clause on the protocol.
  if (!bestDecl) {
    bestDecl = proto;
    inWhereClause = true;
  }

  return {/*AttachedTo=*/bestDecl, inWhereClause};
}

void PrintAST::printInheritedFromRequirementSignature(ProtocolDecl *proto,
                                                      Decl *attachingTo) {
  printGenericSignature(
      GenericSignature::get({proto->getProtocolSelfType()} ,
                            proto->getRequirementSignature()),
      PrintInherited,
      [&](const Requirement &req) {
        auto location = bestRequirementPrintLocation(proto, req);
        return location.AttachedTo == attachingTo && !location.InWhereClause;
      });
}

void PrintAST::printWhereClauseFromRequirementSignature(ProtocolDecl *proto,
                                                        Decl *attachingTo) {
  unsigned flags = PrintRequirements;
  if (isa<AssociatedTypeDecl>(attachingTo))
    flags |= SwapSelfAndDependentMemberType;
  printGenericSignature(
      GenericSignature::get({proto->getProtocolSelfType()} ,
                            proto->getRequirementSignature()),
      flags,
      [&](const Requirement &req) {
        auto location = bestRequirementPrintLocation(proto, req);
        return location.AttachedTo == attachingTo && location.InWhereClause;
      });
}

void PrintAST::printTrailingWhereClause(TrailingWhereClause *whereClause) {
  Printer << " " << tok::kw_where << " ";
  interleave(
      whereClause->getRequirements(),
      [&](const RequirementRepr &req) {
        Printer.callPrintStructurePre(PrintStructureKind::GenericRequirement);
        req.print(Printer);
        Printer.printStructurePost(PrintStructureKind::GenericRequirement);
      },
      [&] { Printer << ", "; });
}

/// A helper function to return the depth of a requirement.
static unsigned getDepthOfRequirement(const Requirement &req) {
  switch (req.getKind()) {
  case RequirementKind::Conformance:
  case RequirementKind::Layout:
    return getDepthOfType(req.getFirstType());

  case RequirementKind::Superclass:
  case RequirementKind::SameType: {
    // Return the max valid depth of firstType and secondType.
    unsigned firstDepth = getDepthOfType(req.getFirstType());
    unsigned secondDepth = getDepthOfType(req.getSecondType());

    unsigned maxDepth;
    if (firstDepth == ErrorDepth && secondDepth != ErrorDepth)
      maxDepth = secondDepth;
    else if (firstDepth != ErrorDepth && secondDepth == ErrorDepth)
      maxDepth = firstDepth;
    else
      maxDepth = std::max(firstDepth, secondDepth);

    return maxDepth;
  }
  }
  llvm_unreachable("bad RequirementKind");
}

static void getRequirementsAtDepth(GenericSignature genericSig,
                                   unsigned depth,
                                   SmallVectorImpl<Requirement> &result) {
  for (auto reqt : genericSig->getRequirements()) {
    unsigned currentDepth = getDepthOfRequirement(reqt);
    assert(currentDepth != ErrorDepth);
    if (currentDepth == depth)
      result.push_back(reqt);
  }
}

void PrintAST::printGenericSignature(GenericSignature genericSig,
                                     unsigned flags) {
  printGenericSignature(genericSig, flags,
                        // print everything
                        [&](const Requirement &) { return true; });
}

void PrintAST::printGenericSignature(
    GenericSignature genericSig, unsigned flags,
    llvm::function_ref<bool(const Requirement &)> filter) {
  auto requirements = genericSig->getRequirements();

  if (flags & InnermostOnly) {
    auto genericParams = genericSig->getInnermostGenericParams();

    printSingleDepthOfGenericSignature(genericParams, requirements, flags,
                                       filter);
    return;
  }

  auto genericParams = genericSig->getGenericParams();

  if (!Options.PrintInSILBody) {
    printSingleDepthOfGenericSignature(genericParams, requirements, flags,
                                       filter);
    return;
  }

  // In order to recover the nested GenericParamLists, we divide genericParams
  // and requirements according to depth.
  unsigned paramIdx = 0, numParam = genericParams.size();
  while (paramIdx < numParam) {
    unsigned depth = genericParams[paramIdx]->getDepth();

    // Move index to genericParams.
    unsigned lastParamIdx = paramIdx;
    do {
      lastParamIdx++;
    } while (lastParamIdx < numParam &&
             genericParams[lastParamIdx]->getDepth() == depth);

    // Collect requirements for this level.
    SmallVector<Requirement, 2> requirementsAtDepth;
    getRequirementsAtDepth(genericSig, depth, requirementsAtDepth);

    printSingleDepthOfGenericSignature(
        genericParams.slice(paramIdx, lastParamIdx - paramIdx),
        requirementsAtDepth, flags, filter);

    paramIdx = lastParamIdx;
  }
}

void PrintAST::printSingleDepthOfGenericSignature(
    TypeArrayView<GenericTypeParamType> genericParams,
    ArrayRef<Requirement> requirements, unsigned flags,
    llvm::function_ref<bool(const Requirement &)> filter) {
  bool printParams = (flags & PrintParams);
  bool printRequirements = (flags & PrintRequirements);
  bool printInherited = (flags & PrintInherited);
  bool swapSelfAndDependentMemberType =
    (flags & SwapSelfAndDependentMemberType);

  SubstitutionMap subMap;
  if (CurrentType) {
    if (!CurrentType->isExistentialType()) {
      auto *DC = Current->getInnermostDeclContext()->getInnermostTypeContext();
      auto *M = DC->getParentModule();
      subMap = CurrentType->getContextSubstitutionMap(M, DC);
    }
  }

  auto substParam = [&](Type param) -> Type {
    return param.subst(subMap);
  };

  if (printParams) {
    // Print the generic parameters.
    Printer << "<";
    interleave(genericParams,
               [&](GenericTypeParamType *param) {
                 if (!subMap.empty()) {
                   if (auto argTy = substParam(param))
                     printType(argTy);
                   else
                     printType(param);
                 } else if (auto *GP = param->getDecl()) {
                   Printer.callPrintStructurePre(
                       PrintStructureKind::GenericParameter, GP);
                   Printer.printName(GP->getName(),
                                     PrintNameContext::GenericParameter);
                   Printer.printStructurePost(
                       PrintStructureKind::GenericParameter, GP);
                 } else {
                   printType(param);
                 }
               },
               [&] { Printer << ", "; });
  }

  if (printRequirements || printInherited) {
    bool isFirstReq = true;
    for (const auto &req : requirements) {
      if (!filter(req))
        continue;

      auto first = req.getFirstType();
      Type second;

      if (req.getKind() != RequirementKind::Layout)
        second = req.getSecondType();

      if (!subMap.empty()) {
        Type subFirst = substParam(first);
        if (!subFirst->hasError())
          first = subFirst;
        if (second) {
          Type subSecond = substParam(second);
          if (!subSecond->hasError())
            second = subSecond;
          if (!(first->is<ArchetypeType>() || first->isTypeParameter()) &&
              !(second->is<ArchetypeType>() || second->isTypeParameter()))
            continue;
        }
      }

      if (isFirstReq) {
        if (printRequirements)
          Printer << " " << tok::kw_where << " ";
        else
          Printer << " : ";

        isFirstReq = false;
      } else {
        Printer << ", ";
      }

      // Swap the order of Self == Self.A requirements if requested.
      if (swapSelfAndDependentMemberType &&
          req.getKind() == RequirementKind::SameType &&
          first->is<GenericTypeParamType>() &&
          second->is<DependentMemberType>())
        std::swap(first, second);

      if (printInherited) {
        // We only print the second part of a requirement in the "inherited"
        // clause.
        switch (req.getKind()) {
        case RequirementKind::Layout:
          req.getLayoutConstraint()->print(Printer, Options);
          break;

        case RequirementKind::Conformance:
        case RequirementKind::Superclass:
          printType(second);
          break;

        case RequirementKind::SameType:
          llvm_unreachable("same-type constraints belong in the where clause");
          break;
        }
      } else {
        Printer.callPrintStructurePre(PrintStructureKind::GenericRequirement);

        // We don't substitute type for the printed requirement so that the
        // printed requirement agrees with separately reported generic parameters.
        printRequirement(req);
        Printer.printStructurePost(PrintStructureKind::GenericRequirement);
      }
    }
  }

  if (printParams)
    Printer << ">";
}

void PrintAST::printRequirement(const Requirement &req) {
  printType(req.getFirstType());
  switch (req.getKind()) {
  case RequirementKind::Layout:
    Printer << " : ";
    req.getLayoutConstraint()->print(Printer, Options);
    return;
  case RequirementKind::Conformance:
  case RequirementKind::Superclass:
    Printer << " : ";
    break;
  case RequirementKind::SameType:
    Printer << " == ";
    break;
  }
  printType(req.getSecondType());
}

bool PrintAST::shouldPrintPattern(const Pattern *P) {
  return Options.shouldPrint(P);
}

void PrintAST::printPatternType(const Pattern *P) {
  if (P->hasType()) {
    Printer << ": ";
    printType(P->getType());
  }
}

bool ShouldPrintChecker::shouldPrint(const Pattern *P,
                                     const PrintOptions &Options) {
  bool ShouldPrint = false;
  P->forEachVariable([&](const VarDecl *VD) {
    ShouldPrint |= shouldPrint(VD, Options);
  });
  return ShouldPrint;
}

bool ShouldPrintChecker::shouldPrint(const Decl *D,
                                     const PrintOptions &Options) {
  #if SWIFT_BUILD_ONLY_SYNTAXPARSERLIB
    return false; // not needed for the parser library.
  #endif

  if (auto *ED= dyn_cast<ExtensionDecl>(D)) {
    if (Options.printExtensionContentAsMembers(ED))
      return false;
  }

  if (Options.SkipMissingMemberPlaceholders && isa<MissingMemberDecl>(D))
    return false;

  if (Options.SkipDeinit && isa<DestructorDecl>(D)) {
    return false;
  }

  if (Options.SkipImports && isa<ImportDecl>(D)) {
    return false;
  }

  if (Options.SkipImplicit && D->isImplicit()) {
    const auto &IgnoreList = Options.TreatAsExplicitDeclList;
    if (std::find(IgnoreList.begin(), IgnoreList.end(), D) == IgnoreList.end())
        return false;
  }

  if (Options.SkipUnavailable &&
      D->getAttrs().isUnavailable(D->getASTContext()))
    return false;

  if (Options.ExplodeEnumCaseDecls) {
    if (isa<EnumElementDecl>(D))
      return true;
    if (isa<EnumCaseDecl>(D))
      return false;
  } else if (auto *EED = dyn_cast<EnumElementDecl>(D)) {
    // Enum elements are printed as part of the EnumCaseDecl, unless they were
    // imported without source info.
    return !EED->getSourceRange().isValid();
  }

  if (auto *ASD = dyn_cast<AbstractStorageDecl>(D)) {
    if (Options.OmitNameOfInaccessibleProperties &&
        contributesToParentTypeStorage(ASD))
      return true;
  }

  // Skip declarations that are not accessible.
  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    if (Options.AccessFilter > AccessLevel::Private &&
        VD->getFormalAccess() < Options.AccessFilter)
      return false;
  }

  if (Options.SkipPrivateStdlibDecls &&
      D->isPrivateStdlibDecl(!Options.SkipUnderscoredStdlibProtocols))
    return false;

  if (Options.SkipEmptyExtensionDecls && isa<ExtensionDecl>(D)) {
    auto Ext = cast<ExtensionDecl>(D);
    // If the extension doesn't add protocols or has no members that we should
    // print then skip printing it.
    SmallVector<TypeLoc, 8> ProtocolsToPrint;
    getInheritedForPrinting(Ext, Options, ProtocolsToPrint);
    if (ProtocolsToPrint.empty()) {
      bool HasMemberToPrint = false;
      for (auto Member : Ext->getMembers()) {
        if (shouldPrint(Member, Options)) {
          HasMemberToPrint = true;
          break;
        }
      }
      if (!HasMemberToPrint)
        return false;
    }
  }

  // If asked to skip overrides and witnesses, do so.
  if (Options.SkipOverrides) {
    if (auto *VD = dyn_cast<ValueDecl>(D)) {
      if (VD->getOverriddenDecl()) return false;
      if (!VD->getSatisfiedProtocolRequirements().empty()) return false;

      if (auto clangDecl = VD->getClangDecl()) {
        // If the Clang declaration is from a protocol but was mirrored into
        // class or extension thereof, treat it as an override.
        if (isa<clang::ObjCProtocolDecl>(clangDecl->getDeclContext()) &&
            VD->getDeclContext()->getSelfClassDecl())
          return false;

        // Check whether Clang considers it an override.
        if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
          SmallVector<const clang::ObjCMethodDecl *, 4> overriddenMethods;
          objcMethod->getOverriddenMethods(overriddenMethods);
          if (!overriddenMethods.empty()) return false;
        } else if (auto objcProperty
                     = dyn_cast<clang::ObjCPropertyDecl>(clangDecl)) {
          if (auto getter = objcProperty->getGetterMethodDecl()) {
            SmallVector<const clang::ObjCMethodDecl *, 4> overriddenMethods;
            getter->getOverriddenMethods(overriddenMethods);
            if (!overriddenMethods.empty()) return false;
          }
        }
      }
    }
  }

  // We need to handle PatternBindingDecl as a special case here because its
  // attributes can only be retrieved from the inside VarDecls.
  if (auto *PD = dyn_cast<PatternBindingDecl>(D)) {
    auto ShouldPrint = false;
    for (auto entry : PD->getPatternList()) {
      ShouldPrint |= shouldPrint(entry.getPattern(), Options);
      if (ShouldPrint)
        return true;
    }
    return false;
  }
  return true;
}

bool PrintAST::shouldPrint(const Decl *D, bool Notify) {
  auto Result = Options.shouldPrint(D);
  if (!Result && Notify)
    Printer.callAvoidPrintDeclPost(D);
  return Result;
}

void PrintAST::printBraceStmt(const BraceStmt *stmt, bool newlineIfEmpty) {
  Printer << "{";
  if (printASTNodes(stmt->getElements()) || newlineIfEmpty) {
    Printer.printNewline();
    indent();
  }
  Printer << "}";
}

void PrintAST::printBodyIfNecessary(const AbstractFunctionDecl *decl) {
  if (auto BodyFunc = Options.FunctionBody) {
    BodyFunc(decl, Printer);
    indent();
    return;
  }

  if (!Options.FunctionDefinitions || !decl->getBody())
    return;

  Printer << " ";
  printBraceStmt(decl->getBody(), /*newlineIfEmpty*/!isa<AccessorDecl>(decl));
}

void PrintAST::printMutatingModifiersIfNeeded(const AccessorDecl *accessor) {
  if (accessor->isAssumedNonMutating() && accessor->isMutating()) {
    Printer.printKeyword("mutating", Options, " ");
  } else if (accessor->isExplicitNonMutating()) {
    Printer.printKeyword("nonmutating", Options, " ");
  }
}

void PrintAST::printAccessors(const AbstractStorageDecl *ASD) {
  if (isa<VarDecl>(ASD) && !Options.PrintPropertyAccessors)
    return;
  if (isa<SubscriptDecl>(ASD) && !Options.PrintSubscriptAccessors)
    return;

  auto impl = ASD->getImplInfo();

  // Don't print accessors for trivially stored properties...
  if (impl.isSimpleStored()) {
    // ...unless we're printing for SIL, which expects a { get set? } on
    //    trivial properties
    if (Options.PrintForSIL) {
      Printer << " { get " << (impl.supportsMutation() ? "set }" : "}");
    }
    // ...or you're private/internal(set), at which point we'll print
    //    @_hasStorage var x: T { get }
    else if (ASD->isSettable(nullptr) && hasLessAccessibleSetter(ASD)) {
      Printer << " {";
      {
        IndentRAII indentMore(*this);
        indent();
        Printer.printNewline();
        Printer << "get";
        Printer.printNewline();
      }
      Printer << "}";
    }
    return;
  }

  // AbstractAccessors is suppressed by FunctionDefinitions.
  bool PrintAbstract =
    Options.AbstractAccessors && !Options.FunctionDefinitions;

  // We sometimes want to print the accessors abstractly
  // instead of listing out how they're actually implemented.
  bool inProtocol = isa<ProtocolDecl>(ASD->getDeclContext());
  if ((inProtocol && !Options.PrintAccessorBodiesInProtocols) ||
      PrintAbstract) {
    bool settable = ASD->isSettable(nullptr);
    bool mutatingGetter = hasMutatingGetter(ASD);
    bool nonmutatingSetter = hasNonMutatingSetter(ASD);

    // We're about to print something like this:
    //   { mutating? get (nonmutating? set)? }
    // But don't print "{ get set }" if we don't have to.
    if (!inProtocol && !Options.PrintGetSetOnRWProperties &&
        settable && !mutatingGetter && !nonmutatingSetter) {
      return;
    }

    Printer << " {";
    if (mutatingGetter) {
      Printer << " ";
      Printer.printKeyword("mutating", Options);
    }
    Printer << " ";
    Printer.printKeyword("get", Options);
    if (settable) {
      if (nonmutatingSetter) {
        Printer << " ";
        Printer.printKeyword("nonmutating", Options);
      }
      Printer << " ";
      Printer.printKeyword("set", Options);
    }
    Printer << " }";
    return;
  }

  // Should we print the 'modify' accessor?
  auto shouldHideModifyAccessor = [&] {
    if (impl.getReadWriteImpl() != ReadWriteImplKind::Modify)
      return true;
    // Always hide in a protocol.
    return isa<ProtocolDecl>(ASD->getDeclContext());
  };

  auto isGetSetImpl = [&] {
    return ((impl.getReadImpl() == ReadImplKind::Stored ||
             impl.getReadImpl() == ReadImplKind::Get) &&
            (impl.getWriteImpl() == WriteImplKind::Stored ||
             impl.getWriteImpl() == WriteImplKind::Set) &&
            (shouldHideModifyAccessor()));
  };

  // Honor !Options.PrintGetSetOnRWProperties in the only remaining
  // case where we could end up printing { get set }.
  if ((PrintAbstract || isGetSetImpl()) &&
      !Options.PrintGetSetOnRWProperties &&
      !Options.FunctionDefinitions &&
      !ASD->isGetterMutating() &&
      !ASD->getAccessor(AccessorKind::Set)->isExplicitNonMutating()) {
    return;
  }

  // Otherwise, print all the concrete defining accessors.
  bool PrintAccessorBody = Options.FunctionDefinitions;

  // Helper to print an accessor. Returns true if the
  // accessor was present but skipped.
  auto PrintAccessor = [&](AccessorKind Kind) -> bool {
    auto *Accessor = ASD->getAccessor(Kind);
    if (!Accessor || !shouldPrint(Accessor))
      return true;
    if (!PrintAccessorBody) {
      Printer << " ";
      printMutatingModifiersIfNeeded(Accessor);
      Printer.printKeyword(getAccessorLabel(Accessor->getAccessorKind()), Options);
    } else {
      {
        IndentRAII IndentMore(*this);
        indent();
        visit(Accessor);
      }
      indent();
      Printer.printNewline();
    }
    return false;
  };

  // Determine if we should print the getter without the 'get { ... }'
  // block around it.
  bool isOnlyGetter = impl.getReadImpl() == ReadImplKind::Get &&
                      ASD->getAccessor(AccessorKind::Get);
  bool isGetterMutating = ASD->supportsMutation() || ASD->isGetterMutating();
  if (isOnlyGetter && !isGetterMutating && PrintAccessorBody &&
      Options.FunctionBody && Options.CollapseSingleGetterProperty) {
    Options.FunctionBody(ASD->getAccessor(AccessorKind::Get), Printer);
    indent();
    return;
  }

  Printer << " {";

  if (PrintAccessorBody)
    Printer.printNewline();

  if (PrintAbstract) {
    PrintAccessor(AccessorKind::Get);
    if (ASD->supportsMutation())
      PrintAccessor(AccessorKind::Set);
  } else {
    switch (impl.getReadImpl()) {
    case ReadImplKind::Stored:
    case ReadImplKind::Inherited:
      break;
    case ReadImplKind::Get:
      PrintAccessor(AccessorKind::Get);
      break;
    case ReadImplKind::Address:
      PrintAccessor(AccessorKind::Address);
      break;
    case ReadImplKind::Read:
      PrintAccessor(AccessorKind::Read);
      break;
    }
    switch (impl.getWriteImpl()) {
    case WriteImplKind::Immutable:
      break;
    case WriteImplKind::Stored:
      llvm_unreachable("simply-stored variable should have been filtered out");
    case WriteImplKind::StoredWithObservers:
    case WriteImplKind::InheritedWithObservers: {
      PrintAccessor(AccessorKind::Get);
      PrintAccessor(AccessorKind::Set);
      break;
    }
    case WriteImplKind::Set:
      PrintAccessor(AccessorKind::Set);
      if (!shouldHideModifyAccessor())
        PrintAccessor(AccessorKind::Modify);
      break;
    case WriteImplKind::MutableAddress:
      PrintAccessor(AccessorKind::MutableAddress);
      PrintAccessor(AccessorKind::WillSet);
      PrintAccessor(AccessorKind::DidSet);
      break;
    case WriteImplKind::Modify:
      PrintAccessor(AccessorKind::Modify);
      break;
    }
  }

  if (!PrintAccessorBody)
    Printer << " ";

  Printer << "}";

  indent();
}

void PrintAST::printMembersOfDecl(Decl *D, bool needComma,
                                  bool openBracket,
                                  bool closeBracket) {
  llvm::SmallVector<Decl *, 3> Members;
  auto AddDeclFunc = [&](DeclRange Range) {
    for (auto RD : Range)
      Members.push_back(RD);
  };

  if (auto Ext = dyn_cast<ExtensionDecl>(D)) {
    AddDeclFunc(Ext->getMembers());
  } else if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
    AddDeclFunc(NTD->getMembers());
    for (auto Ext : NTD->getExtensions()) {
      if (Options.printExtensionContentAsMembers(Ext))
        AddDeclFunc(Ext->getMembers());
    }
    if (Options.PrintExtensionFromConformingProtocols) {
      for (auto Conf : NTD->getAllConformances()) {
        for (auto Ext : Conf->getProtocol()->getExtensions()) {
          if (Options.printExtensionContentAsMembers(Ext))
            AddDeclFunc(Ext->getMembers());
        }
      }
    }
  }
  printMembers(Members, needComma, openBracket, closeBracket);
}

void PrintAST::printMembers(ArrayRef<Decl *> members, bool needComma,
                            bool openBracket, bool closeBracket) {
  if (openBracket) {
    Printer << " {";
    Printer.printNewline();
  }
  {
    IndentRAII indentMore(*this);
    for (auto i = members.begin(), iEnd = members.end(); i != iEnd; ++i) {
      auto member = *i;

      if (!shouldPrint(member, true))
        continue;

      if (!member->shouldPrintInContext(Options))
        continue;

      if (Options.EmptyLineBetweenMembers)
        Printer.printNewline();
      indent();
      visit(member);
      if (needComma && std::next(i) != iEnd)
        Printer << ",";
      Printer.printNewline();
    }
  }
  indent();
  if (closeBracket)
    Printer << "}";
}

void PrintAST::printGenericDeclGenericParams(GenericContext *decl) {
  if (decl->isGeneric())
    if (auto GenericSig = decl->getGenericSignature())
      printGenericSignature(GenericSig, PrintParams | InnermostOnly);
}

void PrintAST::printGenericDeclGenericRequirements(GenericContext *decl) {
  if (decl->isGeneric()) {
    if (auto genericSig = decl->getGenericSignature()) {
      auto baseGenericSig = decl->getParent()
          ->getGenericSignatureOfContext();
      printGenericSignature(genericSig, PrintRequirements,
                            [baseGenericSig](const Requirement &req) {
                              if (baseGenericSig)
                                return !baseGenericSig->isRequirementSatisfied(req);
                              return true;
                            });
    }
  }
}

void PrintAST::printInherited(const Decl *decl) {
  SmallVector<TypeLoc, 6> TypesToPrint;
  getInheritedForPrinting(decl, Options, TypesToPrint);
  if (TypesToPrint.empty())
    return;

  Printer << " : ";

  interleave(TypesToPrint, [&](TypeLoc TL) {
    printTypeLoc(TL);
  }, [&]() {
    Printer << ", ";
  });
}

static void getModuleEntities(const clang::Module *ClangMod,
                              SmallVectorImpl<ModuleEntity> &ModuleEnts) {
  if (!ClangMod)
    return;

  getModuleEntities(ClangMod->Parent, ModuleEnts);
  ModuleEnts.push_back(ClangMod);
}

static void getModuleEntities(ImportDecl *Import,
                              SmallVectorImpl<ModuleEntity> &ModuleEnts) {
  if (auto *ClangMod = Import->getClangModule()) {
    getModuleEntities(ClangMod, ModuleEnts);
    return;
  }

  auto Mod = Import->getModule();
  if (!Mod)
    return;

  if (auto *ClangMod = Mod->findUnderlyingClangModule()) {
    getModuleEntities(ClangMod, ModuleEnts);
  } else {
    ModuleEnts.push_back(Mod);
  }
}

void PrintAST::visitImportDecl(ImportDecl *decl) {
  printAttributes(decl);
  Printer << tok::kw_import << " ";

  switch (decl->getImportKind()) {
  case ImportKind::Module:
    break;
  case ImportKind::Type:
    Printer << tok::kw_typealias << " ";
    break;
  case ImportKind::Struct:
    Printer << tok::kw_struct << " ";
    break;
  case ImportKind::Class:
    Printer << tok::kw_class << " ";
    break;
  case ImportKind::Enum:
    Printer << tok::kw_enum << " ";
    break;
  case ImportKind::Protocol:
    Printer << tok::kw_protocol << " ";
    break;
  case ImportKind::Var:
    Printer << tok::kw_var << " ";
    break;
  case ImportKind::Func:
    Printer << tok::kw_func << " ";
    break;
  }

  SmallVector<ModuleEntity, 4> ModuleEnts;
  getModuleEntities(decl, ModuleEnts);

  ArrayRef<ModuleEntity> Mods = ModuleEnts;
  interleave(decl->getFullAccessPath(),
             [&](const ImportDecl::AccessPathElement &Elem) {
               if (!Mods.empty()) {
                 Printer.printModuleRef(Mods.front(), Elem.first);
                 Mods = Mods.slice(1);
               } else {
                 Printer << Elem.first.str();
               }
             },
             [&] { Printer << "."; });
}

static void printExtendedTypeName(Type ExtendedType, ASTPrinter &Printer,
                                  PrintOptions Options) {
  Options.FullyQualifiedTypes = false;
  Options.FullyQualifiedTypesIfAmbiguous = false;

  // Strip off generic arguments, if any.
  auto Ty = ExtendedType->getAnyNominal()->getDeclaredType();

  Ty->print(Printer, Options);
}

void PrintAST::printSynthesizedExtension(Type ExtendedType,
                                         ExtensionDecl *ExtDecl) {
  if (Options.BracketOptions.shouldOpenExtension(ExtDecl)) {
    printDocumentationComment(ExtDecl);
    printAttributes(ExtDecl);
    Printer << tok::kw_extension << " ";

    printExtendedTypeName(ExtendedType, Printer, Options);
    printInherited(ExtDecl);
    printGenericDeclGenericRequirements(ExtDecl);
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(ExtDecl, false,
                       Options.BracketOptions.shouldOpenExtension(ExtDecl),
                       Options.BracketOptions.shouldCloseExtension(ExtDecl));
  }
}

void PrintAST::printExtension(ExtensionDecl *decl) {
  if (Options.BracketOptions.shouldOpenExtension(decl)) {
    printDocumentationComment(decl);
    printAttributes(decl);
    Printer << "extension ";
    recordDeclLoc(decl, [&]{
      // We cannot extend sugared types.
      Type extendedType = decl->getExtendedType();
      if (!extendedType) {
        // Fallback to TypeRepr.
        printTypeLoc(decl->getExtendedTypeRepr());
        return;
      }
      if (!extendedType->getAnyNominal()) {
        // Fallback to the type.  This usually means we're trying to print an
        // UnboundGenericType.
        printTypeLoc(TypeLoc::withoutLoc(extendedType));
        return;
      }
      printExtendedTypeName(extendedType, Printer, Options);
    });
    printInherited(decl);

    if (auto genericSig = decl->getGenericSignature()) {
      auto baseGenericSig = decl->getExtendedNominal()->getGenericSignature();
      assert(baseGenericSig &&
             "an extension can't be generic if the base type isn't");
      printGenericSignature(genericSig, PrintRequirements,
                            [baseGenericSig](const Requirement &req) -> bool {
        // Only include constraints that are not satisfied by the base type.
        return !baseGenericSig->isRequirementSatisfied(req);
      });
    }
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false,
                       Options.BracketOptions.shouldOpenExtension(decl),
                       Options.BracketOptions.shouldCloseExtension(decl));
  }
}

void PrintAST::visitExtensionDecl(ExtensionDecl *decl) {
  if (Options.TransformContext &&
      Options.TransformContext->isPrintingSynthesizedExtension()) {
    auto extendedType = Options.TransformContext->getBaseType();
    if (extendedType->hasArchetype())
      extendedType = extendedType->mapTypeOutOfContext();
    printSynthesizedExtension(extendedType, decl);
  } else
    printExtension(decl);
}

void PrintAST::visitPatternBindingDecl(PatternBindingDecl *decl) {
  // FIXME: We're not printing proper "{ get set }" annotations in pattern
  // binding decls.  As a hack, scan the decl to find out if any of the
  // variables are immutable, and if so, we print as 'let'.  This allows us to
  // handle the 'let x = 4' case properly at least.
  const VarDecl *anyVar = nullptr;
  for (auto entry : decl->getPatternList()) {
    entry.getPattern()->forEachVariable([&](VarDecl *V) {
      anyVar = V;
    });
    if (anyVar) break;
  }

  if (anyVar)
    printDocumentationComment(anyVar);

  // FIXME: PatternBindingDecls don't have attributes themselves, so just assume
  // the variables all have the same attributes. This isn't exactly true
  // after type-checking, but it's close enough for now.
  if (anyVar) {
    printAttributes(anyVar);
    printAccess(anyVar);
  }

  if (decl->isStatic())
    printStaticKeyword(decl->getCorrectStaticSpelling());

  if (anyVar) {
    Printer << (anyVar->isSettable(anyVar->getDeclContext()) ? "var " : "let ");
  } else {
    Printer << "let ";
  }

  bool isFirst = true;
  for (auto &entry : decl->getPatternList()) {
    if (!shouldPrintPattern(entry.getPattern()))
      continue;
    if (isFirst)
      isFirst = false;
    else
      Printer << ", ";

    printPattern(entry.getPattern());

    // We also try to print type for named patterns, e.g. var Field = 10;
    // and tuple patterns, e.g. var (T1, T2) = (10, 10)
    if (isa<NamedPattern>(entry.getPattern()) ||
        isa<TuplePattern>(entry.getPattern())) {
      printPatternType(entry.getPattern());
    }

    if (Options.VarInitializers) {
      auto vd = entry.getAnchoringVarDecl();
      if (entry.hasInitStringRepresentation() &&
          vd->isInitExposedToClients()) {
        SmallString<128> scratch;
        Printer << " = " << entry.getInitStringRepresentation(scratch);
      }
    }

    // If we're just printing a single pattern and it has accessors,
    // print the accessors here. It is an error to add accessors to a
    // pattern binding with multiple entries.
    if (auto var = decl->getSingleVar()) {
      printAccessors(var);
    }
  }
}

void PrintAST::visitTopLevelCodeDecl(TopLevelCodeDecl *decl) {
  printASTNodes(decl->getBody()->getElements(), /*NeedIndent=*/false);
}

void PrintAST::visitIfConfigDecl(IfConfigDecl *ICD) {
  if (!Options.PrintIfConfig)
    return;

  for (auto &Clause : ICD->getClauses()) {
    if (&Clause == &*ICD->getClauses().begin())
      Printer << tok::pound_if << " /* condition */"; // FIXME: print condition
    else if (Clause.Cond)
      Printer << tok::pound_elseif << " /* condition */"; // FIXME: print condition
    else
      Printer << tok::pound_else;
    printASTNodes(Clause.Elements);
    Printer.printNewline();
    indent();
  }
  Printer << tok::pound_endif;
}

void PrintAST::visitPoundDiagnosticDecl(PoundDiagnosticDecl *PDD) {
  /// TODO: Should we even print #error/#warning?
  if (PDD->isError()) {
    Printer << tok::pound_error;
  } else {
    Printer << tok::pound_warning; 
  }

  Printer << "(\"" << PDD->getMessage()->getValue() << "\")";
}

void PrintAST::visitOpaqueTypeDecl(OpaqueTypeDecl *decl) {
  // TODO: If we introduce explicit opaque type decls, print them.
  assert(decl->getName().empty());
}

void PrintAST::visitTypeAliasDecl(TypeAliasDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << tok::kw_typealias << " ";
  printContextIfNeeded(decl);
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName(), getTypeMemberPrintNameContext(decl));
    }, [&]{ // Signature
      printGenericDeclGenericParams(decl);
    });
  bool ShouldPrint = true;
  Type Ty = decl->getUnderlyingType();

  // If the underlying type is private, don't print it.
  if (Options.SkipPrivateStdlibDecls && Ty && Ty.isPrivateStdlibType())
    ShouldPrint = false;

  if (ShouldPrint) {
    Printer << " = ";
    // FIXME: An inferred associated type witness type alias may reference
    // an opaque type, but OpaqueTypeArchetypes are always canonicalized
    // so lose type sugar for generic params. Bind the generic environment so
    // we can map params back into the generic environment and print them
    // correctly.
    //
    // Remove this when we have a way to represent non-canonical archetypes
    // preserving sugar.
    llvm::SaveAndRestore<GenericEnvironment*> setGenericEnv(Options.GenericEnv,
                                                decl->getGenericEnvironment());
    printTypeLoc(TypeLoc(decl->getUnderlyingTypeRepr(), Ty));
    printGenericDeclGenericRequirements(decl);
  }
}

void PrintAST::visitGenericTypeParamDecl(GenericTypeParamDecl *decl) {
  recordDeclLoc(decl, [&] {
    Printer.printName(decl->getName(), PrintNameContext::GenericParameter);
  });

  printInherited(decl);
}

void PrintAST::visitAssociatedTypeDecl(AssociatedTypeDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << tok::kw_associatedtype << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName(), PrintNameContext::TypeMember);
    });

  auto proto = decl->getProtocol();
  printInheritedFromRequirementSignature(proto, decl);

  if (decl->hasDefaultDefinitionType()) {
    Printer << " = ";
    decl->getDefaultDefinitionType().print(Printer, Options);
  }

  // As with protocol's trailing where clauses, use the requirement signature
  // when available.
  printWhereClauseFromRequirementSignature(proto, decl);
}

void PrintAST::visitEnumDecl(EnumDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    if (!Options.SkipIntroducerKeywords)
      Printer << tok::kw_enum << " ";
    printContextIfNeeded(decl);
    recordDeclLoc(decl,
      [&]{
        Printer.printName(decl->getName(), getTypeMemberPrintNameContext(decl));
      }, [&]{ // Signature
        printGenericDeclGenericParams(decl);
      });
    printInherited(decl);
    printGenericDeclGenericRequirements(decl);
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false, true,
                       Options.BracketOptions.shouldCloseNominal(decl));
  }
}

void PrintAST::visitStructDecl(StructDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    if (!Options.SkipIntroducerKeywords)
      Printer << tok::kw_struct << " ";
    printContextIfNeeded(decl);
    recordDeclLoc(decl,
      [&]{
        Printer.printName(decl->getName(), getTypeMemberPrintNameContext(decl));
      }, [&]{ // Signature
        printGenericDeclGenericParams(decl);
      });
    printInherited(decl);
    printGenericDeclGenericRequirements(decl);
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false, true,
                       Options.BracketOptions.shouldCloseNominal(decl));
  }
}

void PrintAST::visitClassDecl(ClassDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    if (!Options.SkipIntroducerKeywords)
      Printer << tok::kw_class << " ";
    printContextIfNeeded(decl);
    recordDeclLoc(decl,
      [&]{
        Printer.printName(decl->getName(), getTypeMemberPrintNameContext(decl));
      }, [&]{ // Signature
        printGenericDeclGenericParams(decl);
      });

    printInherited(decl);
    printGenericDeclGenericRequirements(decl);
  }

  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false, true,
                       Options.BracketOptions.shouldCloseNominal(decl));
  }
}

void PrintAST::visitProtocolDecl(ProtocolDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    if (!Options.SkipIntroducerKeywords)
      Printer << tok::kw_protocol << " ";
    printContextIfNeeded(decl);
    recordDeclLoc(decl,
      [&]{
        Printer.printName(decl->getName());
      });

    printInheritedFromRequirementSignature(decl, decl);

    // The trailing where clause is a syntactic thing, which isn't serialized
    // (etc.) and thus isn't available for printing things out of
    // already-compiled SIL modules. The requirement signature is available in
    // such cases, so let's go with that when we can.
    printWhereClauseFromRequirementSignature(decl, decl);
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false, true,
                       Options.BracketOptions.shouldCloseNominal(decl));
  }
}
static bool isStructOrClassContext(DeclContext *dc) {
  auto *nominal = dc->getSelfNominalTypeDecl();
  if (nominal == nullptr)
    return false;
  return isa<ClassDecl>(nominal) || isa<StructDecl>(nominal);
}

static bool isEscaping(Type type) {
  if (auto *funcType = type->getAs<AnyFunctionType>()) {
    if (funcType->getExtInfo().getRepresentation() ==
          FunctionTypeRepresentation::CFunctionPointer)
      return false;

    return !funcType->getExtInfo().isNoEscape();
  }

  return false;
}

static void printParameterFlags(ASTPrinter &printer, PrintOptions options,
                                ParameterTypeFlags flags, bool escaping) {
  if (!options.excludeAttrKind(TAK_autoclosure) && flags.isAutoClosure())
    printer << "@autoclosure ";

  switch (flags.getValueOwnership()) {
  case ValueOwnership::Default:
    /* nothing */
    break;
  case ValueOwnership::InOut:
    printer.printKeyword("inout", options, " ");
    break;
  case ValueOwnership::Shared:
    printer.printKeyword("__shared", options, " ");
    break;
  case ValueOwnership::Owned:
    printer.printKeyword("__owned", options, " ");
    break;
  }

  if (!options.excludeAttrKind(TAK_escaping) && escaping)
    printer << "@escaping ";
}

void PrintAST::visitVarDecl(VarDecl *decl) {
  printDocumentationComment(decl);
  // Print @_hasStorage when the attribute is not already
  // on, decl has storage and it is on a class.
  if (Options.PrintForSIL && decl->hasStorage() &&
      isStructOrClassContext(decl->getDeclContext()) &&
      !decl->getAttrs().hasAttribute<HasStorageAttr>())
    Printer << "@_hasStorage ";
  printAttributes(decl);
  printAccess(decl);
  if (!Options.SkipIntroducerKeywords) {
    if (decl->isStatic() && Options.PrintStaticKeyword)
      printStaticKeyword(decl->getCorrectStaticSpelling());
    if (decl->getKind() == DeclKind::Var
        || Options.PrintParameterSpecifiers) {
      // Map all non-let specifiers to 'var'.  This is not correct, but
      // SourceKit relies on this for info about parameter decls.
      if (decl->isLet())
        Printer << tok::kw_let;
      else
        Printer << tok::kw_var;

      Printer << " ";
    }
  }
  printContextIfNeeded(decl);
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName(), getTypeMemberPrintNameContext(decl));
    });
  if (decl->hasInterfaceType()) {
    Printer << ": ";
    auto tyLoc = decl->getTypeLoc();
    if (!tyLoc.getTypeRepr())
      tyLoc = TypeLoc::withoutLoc(decl->getInterfaceType());

    Printer.printDeclResultTypePre(decl, tyLoc);

    // HACK: When printing result types for vars with opaque result types,
    //       always print them using the `some` keyword instead of printing
    //       the full stable reference.
    llvm::SaveAndRestore<PrintOptions::OpaqueReturnTypePrintingMode>
    x(Options.OpaqueReturnTypePrinting,
      PrintOptions::OpaqueReturnTypePrintingMode::WithOpaqueKeyword);

    printTypeLocForImplicitlyUnwrappedOptional(
      tyLoc, decl->isImplicitlyUnwrappedOptional());
  }

  printAccessors(decl);
}

void PrintAST::visitParamDecl(ParamDecl *decl) {
  visitVarDecl(decl);
}

void PrintAST::printOneParameter(const ParamDecl *param,
                                 ParameterTypeFlags paramFlags,
                                 bool ArgNameIsAPIByDefault) {
  Printer.callPrintStructurePre(PrintStructureKind::FunctionParameter, param);
  SWIFT_DEFER {
    Printer.printStructurePost(PrintStructureKind::FunctionParameter, param);
  };

  auto printArgName = [&]() {
    // Print argument name.
    auto ArgName = param->getArgumentName();
    auto BodyName = param->getName();
    switch (Options.ArgAndParamPrinting) {
    case PrintOptions::ArgAndParamPrintingMode::EnumElement:
      if (ArgName.empty() && BodyName.empty() && !param->getDefaultValue()) {
        // Don't print anything, in the style of a tuple element.
        return;
      }
      // Else, print the argument only.
      LLVM_FALLTHROUGH;
    case PrintOptions::ArgAndParamPrintingMode::ArgumentOnly:
      Printer.printName(ArgName, PrintNameContext::FunctionParameterExternal);

      if (!ArgNameIsAPIByDefault && !ArgName.empty())
        Printer << " _";
      break;
    case PrintOptions::ArgAndParamPrintingMode::MatchSource:
      if (ArgName == BodyName && ArgNameIsAPIByDefault) {
        Printer.printName(ArgName, PrintNameContext::FunctionParameterExternal);
        break;
      }
      if (ArgName.empty() && !ArgNameIsAPIByDefault) {
        Printer.printName(BodyName, PrintNameContext::FunctionParameterLocal);
        break;
      }
      LLVM_FALLTHROUGH;
    case PrintOptions::ArgAndParamPrintingMode::BothAlways:
      Printer.printName(ArgName, PrintNameContext::FunctionParameterExternal);
      Printer << " ";
      Printer.printName(BodyName, PrintNameContext::FunctionParameterLocal);
      break;
    }
    Printer << ": ";
  };

  auto TheTypeLoc = param->getTypeLoc();

  printAttributes(param);
  printArgName();

  if (!TheTypeLoc.getTypeRepr() && param->hasInterfaceType())
    TheTypeLoc = TypeLoc::withoutLoc(param->getInterfaceType());

  // If the parameter is variadic, we will print the "..." after it, but we have
  // to strip off the added array type.
  if (param->isVariadic() && TheTypeLoc.getType()) {
    if (auto *BGT = TheTypeLoc.getType()->getAs<BoundGenericType>())
      TheTypeLoc.setType(BGT->getGenericArgs()[0]);
  }

  if (!param->isVariadic() &&
      !willUseTypeReprPrinting(TheTypeLoc, CurrentType, Options)) {
    auto type = TheTypeLoc.getType();
    printParameterFlags(Printer, Options, paramFlags,
                        isEscaping(type));
  }

  printTypeLocForImplicitlyUnwrappedOptional(
    TheTypeLoc, param->isImplicitlyUnwrappedOptional());

  if (param->isVariadic())
    Printer << "...";

  if (param->isDefaultArgument()) {
    SmallString<128> scratch;
    auto defaultArgStr = param->getDefaultValueStringRepresentation(scratch);

    assert(!defaultArgStr.empty() && "empty default argument?");
    Printer << " = ";

    switch (param->getDefaultArgumentKind()) {
    case DefaultArgumentKind::File:
    case DefaultArgumentKind::Line:
    case DefaultArgumentKind::Column:
    case DefaultArgumentKind::Function:
    case DefaultArgumentKind::DSOHandle:
    case DefaultArgumentKind::NilLiteral:
      Printer.printKeyword(defaultArgStr, Options);
      break;
    default:
      Printer << defaultArgStr;
      break;
    }
  }
}

void PrintAST::printParameterList(ParameterList *PL,
                                  ArrayRef<AnyFunctionType::Param> params,
                                  bool isAPINameByDefault) {
  Printer << "(";
  const unsigned paramSize = params.size();
  for (unsigned i = 0, e = PL->size(); i != e; ++i) {
    if (i > 0)
      Printer << ", ";
    auto paramFlags = (i < paramSize)
                    ? params[i].getParameterFlags()
                    : ParameterTypeFlags();
    printOneParameter(PL->get(i), paramFlags,
                      isAPINameByDefault);
  }
  Printer << ")";
}

void PrintAST::printFunctionParameters(AbstractFunctionDecl *AFD) {
  auto BodyParams = AFD->getParameters();
  auto curTy = AFD->hasInterfaceType() ? AFD->getInterfaceType() : nullptr;

  // Skip over the implicit 'self'.
  if (AFD->hasImplicitSelfDecl()) {
    if (curTy)
      if (auto funTy = curTy->getAs<AnyFunctionType>())
        curTy = funTy->getResult();
  }

  ArrayRef<AnyFunctionType::Param> parameterListTypes;
  if (curTy) {
    if (auto funTy = curTy->getAs<AnyFunctionType>()) {
      parameterListTypes = funTy->getParams();
    }
  }

  printParameterList(BodyParams, parameterListTypes,
                     AFD->argumentNameIsAPIByDefault());

  if (AFD->hasThrows()) {
    if (AFD->getAttrs().hasAttribute<RethrowsAttr>())
      Printer << " " << tok::kw_rethrows;
    else
      Printer << " " << tok::kw_throws;
  }
}

bool PrintAST::printASTNodes(const ArrayRef<ASTNode> &Elements,
                             bool NeedIndent) {
  IndentRAII IndentMore(*this, NeedIndent);
  bool PrintedSomething = false;
  for (auto element : Elements) {
    PrintedSomething = true;
    Printer.printNewline();
    indent();
    if (auto decl = element.dyn_cast<Decl*>()) {
      if (decl->shouldPrintInContext(Options))
        visit(decl);
    } else if (auto stmt = element.dyn_cast<Stmt*>()) {
      visit(stmt);
    } else {
      // FIXME: print expression
      // visit(element.get<Expr*>());
    }
  }
  return PrintedSomething;
}

void PrintAST::visitAccessorDecl(AccessorDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  switch (auto kind = decl->getAccessorKind()) {
  case AccessorKind::Get:
  case AccessorKind::Address:
  case AccessorKind::Read:
  case AccessorKind::Modify:
  case AccessorKind::DidSet:
  case AccessorKind::MutableAddress:
    recordDeclLoc(decl,
      [&]{
        Printer << getAccessorLabel(decl->getAccessorKind());
      });
    break;
  case AccessorKind::Set:
  case AccessorKind::WillSet:
    recordDeclLoc(decl,
      [&]{
        Printer << getAccessorLabel(decl->getAccessorKind());

        auto params = decl->getParameters();
        if (params->size() != 0 && !params->get(0)->isImplicit()) {
          auto Name = params->get(0)->getName();
          if (!Name.empty()) {
            Printer << "(";
            Printer.printName(Name);
            Printer << ")";
          }
        }
      });
  }

  printBodyIfNecessary(decl);
}

void PrintAST::visitFuncDecl(FuncDecl *decl) {
  ASTContext &Ctx = decl->getASTContext();

  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    SourceLoc StartLoc = decl->getStartLoc();
    SourceLoc EndLoc;
    if (!decl->getBodyResultTypeLoc().isNull()) {
      EndLoc = decl->getBodyResultTypeLoc().getSourceRange().End;
    } else {
      EndLoc = decl->getSignatureSourceRange().End;
    }
    CharSourceRange Range =
      Lexer::getCharSourceRangeFromSourceRange(Ctx.SourceMgr,
                                               SourceRange(StartLoc, EndLoc));
    printSourceRange(Range, Ctx);
  } else {
    if (!Options.SkipIntroducerKeywords) {
      if (decl->isStatic() && Options.PrintStaticKeyword)
        printStaticKeyword(decl->getCorrectStaticSpelling());
      if (decl->isMutating() && !decl->getAttrs().hasAttribute<MutatingAttr>()) {
        Printer.printKeyword("mutating", Options, " ");
      } else if (decl->isConsuming() && !decl->getAttrs().hasAttribute<ConsumingAttr>()) {
        Printer.printKeyword("__consuming", Options, " ");
      }
      Printer << tok::kw_func << " ";
    }
    printContextIfNeeded(decl);
    recordDeclLoc(decl,
      [&]{ // Name
        if (!decl->hasName()) {
          Printer << "<anonymous>";
        } else {
          Printer.printName(decl->getName(),
                            getTypeMemberPrintNameContext(decl));
          if (decl->isOperator())
            Printer << " ";
        }
      }, [&] { // Parameters
        printGenericDeclGenericParams(decl);
        printFunctionParameters(decl);
      });

    Type ResultTy = decl->getResultInterfaceType();
    if (ResultTy && !ResultTy->isVoid()) {
      TypeLoc ResultTyLoc = decl->getBodyResultTypeLoc();

      // When printing a protocol requirement with types substituted for a
      // conforming class, replace occurrences of the 'Self' generic parameter
      // in the result type with DynamicSelfType, instead of the static
      // conforming type.
      auto *proto = dyn_cast<ProtocolDecl>(decl->getDeclContext());
      if (proto && Options.TransformContext) {
        auto BaseType = Options.TransformContext->getBaseType();
        if (BaseType->getClassOrBoundGenericClass()) {
          ResultTy = ResultTy.subst(
            [&](Type t) -> Type {
              if (t->isEqual(proto->getSelfInterfaceType()))
                return DynamicSelfType::get(t, Ctx);
              return t;
            },
            MakeAbstractConformanceForGenericType());
          ResultTyLoc = TypeLoc::withoutLoc(ResultTy);
        }
      }

      if (!ResultTyLoc.getTypeRepr())
        ResultTyLoc = TypeLoc::withoutLoc(ResultTy);
      // FIXME: Hacky way to workaround the fact that 'Self' as return
      // TypeRepr is not getting 'typechecked'. See
      // \c resolveTopLevelIdentTypeComponent function in TypeCheckType.cpp.
      if (auto *simId = dyn_cast_or_null<SimpleIdentTypeRepr>(ResultTyLoc.getTypeRepr())) {
        if (simId->getIdentifier() == Ctx.Id_Self)
          ResultTyLoc = TypeLoc::withoutLoc(ResultTy);
      }
      Printer << " -> ";

      Printer.printDeclResultTypePre(decl, ResultTyLoc);
      Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);

      // HACK: When printing result types for funcs with opaque result types,
      //       always print them using the `some` keyword instead of printing
      //       the full stable reference.
      llvm::SaveAndRestore<PrintOptions::OpaqueReturnTypePrintingMode>
      x(Options.OpaqueReturnTypePrinting,
        PrintOptions::OpaqueReturnTypePrintingMode::WithOpaqueKeyword);

      printTypeLocForImplicitlyUnwrappedOptional(
          ResultTyLoc, decl->isImplicitlyUnwrappedOptional());
      Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
    }
    printGenericDeclGenericRequirements(decl);
  }

  printBodyIfNecessary(decl);
  
  // If the function has an opaque result type, print the opaque type decl.
  if (auto opaqueResult = decl->getOpaqueResultTypeDecl()) {
    Printer.printNewline();
    visit(opaqueResult);
  }
}

void PrintAST::printEnumElement(EnumElementDecl *elt) {
  recordDeclLoc(elt,
    [&]{
      Printer.printName(elt->getName(), getTypeMemberPrintNameContext(elt));
    });

  if (auto *PL = elt->getParameterList()) {
    llvm::SaveAndRestore<PrintOptions::ArgAndParamPrintingMode>
      mode(Options.ArgAndParamPrinting,
           PrintOptions::ArgAndParamPrintingMode::EnumElement);


    auto params = ArrayRef<AnyFunctionType::Param>();
    if (elt->hasInterfaceType() && !elt->getInterfaceType()->hasError()) {
      // Walk to the params of the associated values.
      // (EnumMetaType) -> (AssocValues) -> Enum
      params = elt->getInterfaceType()->castTo<AnyFunctionType>()
                                      ->getResult()
                                      ->castTo<AnyFunctionType>()
                                      ->getParams();
    }

    // @escaping is not valid in enum element position, even though the
    // attribute is implicitly added. Ignore it when printing the parameters.
    Options.ExcludeAttrList.push_back(TAK_escaping);
    printParameterList(PL, params,
                       /*isAPINameByDefault*/true);
    Options.ExcludeAttrList.pop_back();
  }

  switch (Options.EnumRawValues) {
  case PrintOptions::EnumRawValueMode::Skip:
    return;
  case PrintOptions::EnumRawValueMode::PrintObjCOnly:
    if (!elt->isObjC())
      return;
    break;
  case PrintOptions::EnumRawValueMode::Print:
    break;
  }

  auto *raw = elt->getStructuralRawValueExpr();
  if (!raw || raw->isImplicit())
    return;

  // Print the explicit raw value expression.
  Printer << " = ";
  switch (raw->getKind()) {
  case ExprKind::IntegerLiteral:
  case ExprKind::FloatLiteral: {
    auto *numLiteral = cast<NumberLiteralExpr>(raw);
    Printer.callPrintStructurePre(PrintStructureKind::NumberLiteral);
    if (numLiteral->isNegative())
      Printer << "-";
    Printer << numLiteral->getDigitsText();
    Printer.printStructurePost(PrintStructureKind::NumberLiteral);
    break;
  }
  case ExprKind::StringLiteral: {
    Printer.callPrintStructurePre(PrintStructureKind::StringLiteral);
    llvm::SmallString<32> str;
    llvm::raw_svector_ostream os(str);
    os << QuotedString(cast<StringLiteralExpr>(raw)->getValue());
    Printer << str;
    Printer.printStructurePost(PrintStructureKind::StringLiteral);
    break;
  }
  default:
    break; // Incorrect raw value; skip it for error recovery.
  }
}

void PrintAST::visitEnumCaseDecl(EnumCaseDecl *decl) {
  auto elems = decl->getElements();
  if (!elems.empty()) {
    // Documentation comments over the case are attached to the enum elements.
    printDocumentationComment(elems[0]);
    printAttributes(elems[0]);
  }
  Printer << tok::kw_case << " ";

  interleave(elems.begin(), elems.end(),
    [&](EnumElementDecl *elt) {
      printEnumElement(elt);
    },
    [&] { Printer << ", "; });
}

void PrintAST::visitEnumElementDecl(EnumElementDecl *decl) {
  printDocumentationComment(decl);
  // In cases where there is no parent EnumCaseDecl (such as imported or
  // deserialized elements), print the element independently.
  printAttributes(decl);
  Printer << tok::kw_case << " ";
  printEnumElement(decl);
}

void PrintAST::visitSubscriptDecl(SubscriptDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);
  if (!Options.SkipIntroducerKeywords && decl->isStatic() &&
      Options.PrintStaticKeyword)
    printStaticKeyword(decl->getCorrectStaticSpelling());
  printContextIfNeeded(decl);
  recordDeclLoc(decl, [&]{
    Printer << "subscript";
  }, [&] { // Parameters
    printGenericDeclGenericParams(decl);
    auto params = ArrayRef<AnyFunctionType::Param>();
    if (decl->hasInterfaceType() && !decl->getInterfaceType()->hasError()) {
      // Walk to the params of the subscript's indices.
      params = decl->getInterfaceType()->castTo<AnyFunctionType>()->getParams();
    }
    printParameterList(decl->getIndices(), params,
                       /*isAPINameByDefault*/false);
  });
  Printer << " -> ";

  TypeLoc elementTy = decl->getElementTypeLoc();
  Printer.printDeclResultTypePre(decl, elementTy);
  Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
  if (!elementTy.getTypeRepr())
    elementTy = TypeLoc::withoutLoc(decl->getElementInterfaceType());

  // HACK: When printing result types for subscripts with opaque result types,
  //       always print them using the `some` keyword instead of printing
  //       the full stable reference.
  llvm::SaveAndRestore<PrintOptions::OpaqueReturnTypePrintingMode>
  x(Options.OpaqueReturnTypePrinting,
    PrintOptions::OpaqueReturnTypePrintingMode::WithOpaqueKeyword);

  printTypeLocForImplicitlyUnwrappedOptional(
    elementTy, decl->isImplicitlyUnwrappedOptional());
  Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
  printGenericDeclGenericRequirements(decl);
  printAccessors(decl);
}

void PrintAST::visitConstructorDecl(ConstructorDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);

  if ((decl->getInitKind() == CtorInitializerKind::Convenience ||
       decl->getInitKind() == CtorInitializerKind::ConvenienceFactory) &&
      !decl->getAttrs().hasAttribute<ConvenienceAttr>()) {
    // Protocol extension initializers are modeled as convenience initializers,
    // but they're not written that way in source. Check if we're actually
    // printing onto a class.
    bool isClassContext;
    if (CurrentType) {
      isClassContext = CurrentType->getClassOrBoundGenericClass() != nullptr;
    } else {
      const DeclContext *dc = decl->getDeclContext();
      isClassContext = dc->getSelfClassDecl() != nullptr;
    }
    if (isClassContext) {
      Printer.printKeyword("convenience", Options, " ");
    } else {
      assert(decl->getDeclContext()->getExtendedProtocolDecl() &&
             "unexpected convenience initializer");
    }
  } else if (decl->getInitKind() == CtorInitializerKind::Factory) {
    Printer << "/*not inherited*/ ";
  }

  printContextIfNeeded(decl);
  recordDeclLoc(decl,
    [&]{
      Printer << "init";
    }, [&] { // Signature
      if (decl->isFailable()) {
        if (decl->isImplicitlyUnwrappedOptional())
          Printer << "!";
        else
          Printer << "?";
      }

      printGenericDeclGenericParams(decl);
      printFunctionParameters(decl);
    });

  printGenericDeclGenericRequirements(decl);

  printBodyIfNecessary(decl);
}

void PrintAST::visitDestructorDecl(DestructorDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printContextIfNeeded(decl);
  recordDeclLoc(decl,
    [&]{
      Printer << "deinit";
    });

  printBodyIfNecessary(decl);
}

void PrintAST::visitInfixOperatorDecl(InfixOperatorDecl *decl) {
  Printer.printKeyword("infix", Options, " ");
  Printer << tok::kw_operator << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  if (auto *group = decl->getPrecedenceGroup())
    Printer << " : " << group->getName();
  auto designatedNominalTypes = decl->getDesignatedNominalTypes();
  auto first = true;
  for (auto typeDecl : designatedNominalTypes) {
    if (first && !decl->getPrecedenceGroup())
      Printer << " : " << typeDecl->getName();
    else
      Printer << ", " << typeDecl->getName();
    first = false;
  }
}

void PrintAST::visitPrecedenceGroupDecl(PrecedenceGroupDecl *decl) {
  Printer << tok::kw_precedencegroup << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  Printer << " {";
  Printer.printNewline();
  {
    IndentRAII indentMore(*this);
    if (!decl->isAssociativityImplicit() ||
        !decl->isNonAssociative()) {
      indent();
      Printer.printKeyword("associativity", Options, ": ");
      switch (decl->getAssociativity()) {
      case Associativity::None:
        Printer.printKeyword("none", Options);
        break;
      case Associativity::Left:
        Printer.printKeyword("left", Options);
        break;
      case Associativity::Right:
        Printer.printKeyword("right", Options);
        break;
      }
      Printer.printNewline();
    }
    if (!decl->isAssignmentImplicit() ||
        decl->isAssignment()) {
      indent();
      Printer.printKeyword("assignment", Options, ": ");
      Printer.printKeyword(decl->isAssignment() ? "true" : "false", Options);
      Printer.printNewline();
    }
    if (!decl->getHigherThan().empty()) {
      indent();
      Printer.printKeyword("higherThan", Options, ": ");
      if (!decl->getHigherThan().empty()) {
        Printer << decl->getHigherThan()[0].Name;
        for (auto &rel : decl->getHigherThan().slice(1))
          Printer << ", " << rel.Name;
      }
      Printer.printNewline();
    }
    if (!decl->getLowerThan().empty()) {
      indent();
      Printer.printKeyword("lowerThan", Options, ": ");
      if (!decl->getLowerThan().empty()) {
        Printer << decl->getLowerThan()[0].Name;
        for (auto &rel : decl->getLowerThan().slice(1))
          Printer << ", " << rel.Name;
      }
      Printer.printNewline();
    }
  }
  indent();
  Printer << "}";
}

void PrintAST::visitPrefixOperatorDecl(PrefixOperatorDecl *decl) {
  Printer.printKeyword("prefix", Options, " ");
  Printer << tok::kw_operator << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  auto designatedNominalTypes = decl->getDesignatedNominalTypes();
  auto first = true;
  for (auto typeDecl : designatedNominalTypes) {
    if (first)
      Printer << " : " << typeDecl->getName();
    else
      Printer << ", " << typeDecl->getName();
    first = false;
  }
}

void PrintAST::visitPostfixOperatorDecl(PostfixOperatorDecl *decl) {
  Printer.printKeyword("postfix", Options, " ");
  Printer << tok::kw_operator << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  auto designatedNominalTypes = decl->getDesignatedNominalTypes();
  auto first = true;
  for (auto typeDecl : designatedNominalTypes) {
    if (first)
      Printer << " : " << typeDecl->getName();
    else
      Printer << ", " << typeDecl->getName();
    first = false;
  }
}

void PrintAST::visitModuleDecl(ModuleDecl *decl) { }

void PrintAST::visitMissingMemberDecl(MissingMemberDecl *decl) {
  Printer << "/* placeholder for ";
  recordDeclLoc(decl, [&]{ Printer << decl->getFullName(); });
  unsigned numVTableEntries = decl->getNumberOfVTableEntries();
  if (numVTableEntries > 0)
    Printer << " (vtable entries: " << numVTableEntries << ")";
  unsigned numFieldOffsetVectorEntries = decl->getNumberOfFieldOffsetVectorEntries();
  if (numFieldOffsetVectorEntries > 0)
    Printer << " (field offsets: " << numFieldOffsetVectorEntries << ")";
  Printer << " */";
}

void PrintAST::visitBraceStmt(BraceStmt *stmt) {
  printBraceStmt(stmt);
}

void PrintAST::visitReturnStmt(ReturnStmt *stmt) {
  Printer << tok::kw_return;
  if (stmt->hasResult()) {
    Printer << " ";
    // FIXME: print expression.
  }
}

void PrintAST::visitYieldStmt(YieldStmt *stmt) {
  Printer.printKeyword("yield", Options, " ");
  bool parens = (stmt->getYields().size() != 1
                 || stmt->getLParenLoc().isValid());
  if (parens) Printer << "(";
  bool first = true;
  for (auto yield : stmt->getYields()) {
    if (first) {
      first = false;
    } else {
      Printer << ", ";
    }

    // FIXME: print expression.
    (void) yield;
  }
  if (parens) Printer << ")";
}

void PrintAST::visitThrowStmt(ThrowStmt *stmt) {
  Printer << tok::kw_throw << " ";
  // FIXME: print expression.
}

void PrintAST::visitPoundAssertStmt(PoundAssertStmt *stmt) {
  Printer << tok::pound_assert << " ";
  // FIXME: print expression.
}

void PrintAST::visitDeferStmt(DeferStmt *stmt) {
  Printer << tok::kw_defer << " ";
  visit(stmt->getBodyAsWritten());
}

void PrintAST::visitIfStmt(IfStmt *stmt) {
  Printer << tok::kw_if << " ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getThenStmt());
  if (auto elseStmt = stmt->getElseStmt()) {
    Printer << " " << tok::kw_else << " ";
    visit(elseStmt);
  }
}
void PrintAST::visitGuardStmt(GuardStmt *stmt) {
  Printer << tok::kw_guard << " ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitWhileStmt(WhileStmt *stmt) {
  Printer << tok::kw_while << " ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitRepeatWhileStmt(RepeatWhileStmt *stmt) {
  Printer << tok::kw_do << " ";
  visit(stmt->getBody());
  Printer << " " << tok::kw_while << " ";
  // FIXME: print condition
}

void PrintAST::visitDoStmt(DoStmt *stmt) {
  Printer << tok::kw_do << " ";
  visit(stmt->getBody());
}

void PrintAST::visitDoCatchStmt(DoCatchStmt *stmt) {
  Printer << tok::kw_do << " ";
  visit(stmt->getBody());
  for (auto clause : stmt->getCatches()) {
    visitCatchStmt(clause);
  }
}

void PrintAST::visitCatchStmt(CatchStmt *stmt) {
  Printer << tok::kw_catch << " ";
  printPattern(stmt->getErrorPattern());
  if (auto guard = stmt->getGuardExpr()) {
    Printer << " " << tok::kw_where << " ";
    // FIXME: print guard expression
    (void) guard;
  }
  Printer << ' ';
  visit(stmt->getBody());
}

void PrintAST::visitForEachStmt(ForEachStmt *stmt) {
  Printer << tok::kw_for << " ";
  printPattern(stmt->getPattern());
  Printer << " " << tok::kw_in << " ";
  // FIXME: print container
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitBreakStmt(BreakStmt *stmt) {
  Printer << tok::kw_break;
}

void PrintAST::visitContinueStmt(ContinueStmt *stmt) {
  Printer << tok::kw_continue;
}

void PrintAST::visitFallthroughStmt(FallthroughStmt *stmt) {
  Printer << tok::kw_fallthrough;
}

void PrintAST::visitSwitchStmt(SwitchStmt *stmt) {
  Printer << tok::kw_switch << " ";
  // FIXME: print subject
  Printer << "{";
  Printer.printNewline();
  for (auto N : stmt->getRawCases()) {
    if (N.is<Stmt*>())
      visit(cast<CaseStmt>(N.get<Stmt*>()));
    else
      visit(cast<IfConfigDecl>(N.get<Decl*>()));
  }
  Printer.printNewline();
  indent();
  Printer << "}";
}

void PrintAST::visitCaseStmt(CaseStmt *CS) {
  if (CS->hasUnknownAttr())
    Printer << "@unknown ";

  if (CS->isDefault()) {
    Printer << tok::kw_default;
  } else {
    auto PrintCaseLabelItem = [&](const CaseLabelItem &CLI) {
      if (auto *P = CLI.getPattern())
        printPattern(P);
      if (CLI.getGuardExpr()) {
        Printer << " " << tok::kw_where << " ";
        // FIXME: print guard expr
      }
    };
    Printer << tok::kw_case << " ";
    interleave(CS->getCaseLabelItems(), PrintCaseLabelItem,
               [&] { Printer << ", "; });
  }
  Printer << ":";
  Printer.printNewline();

  printASTNodes((cast<BraceStmt>(CS->getBody())->getElements()));
}

void PrintAST::visitFailStmt(FailStmt *stmt) {
  Printer << tok::kw_return << " " << tok::kw_nil;
}

void Decl::print(raw_ostream &os) const {
  PrintOptions options;
  options.FunctionDefinitions = true;
  options.TypeDefinitions = true;
  options.VarInitializers = true;
  // FIXME: Move all places where SIL printing is happening to explicit options.
  // For example, see \c ProjectionPath::print.
  options.PreferTypeRepr = false;

  print(os, options);
}

void Decl::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}

bool Decl::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  PrintAST printer(Printer, Opts);
  return printer.visit(const_cast<Decl *>(this));
}

bool Decl::shouldPrintInContext(const PrintOptions &PO) const {
  // Skip getters/setters. They are part of the variable or subscript.
  if (isa<AccessorDecl>(this))
    return false;

  if (PO.ExplodePatternBindingDecls) {
    if (isa<VarDecl>(this))
      return true;
    if (isa<PatternBindingDecl>(this))
      return false;
  } else {
    // Try to preserve the PatternBindingDecl structure.

    // Skip stored variables, unless they came from a Clang module.
    // Stored variables in Swift source will be picked up by the
    // PatternBindingDecl.
    if (auto *VD = dyn_cast<VarDecl>(this)) {
      if (!VD->hasClangNode() && VD->hasStorage())
        return false;
    }

    // Skip pattern bindings that consist of just one variable with
    // interesting accessors.
    if (auto pbd = dyn_cast<PatternBindingDecl>(this)) {
      if (pbd->getPatternList().size() == 1) {
        auto pattern =
          pbd->getPatternList()[0].getPattern()->getSemanticsProvidingPattern();
        if (auto named = dyn_cast<NamedPattern>(pattern)) {
          if (!named->getDecl()->hasStorage())
            return false;
        }
      }
    }
  }

  if (isa<IfConfigDecl>(this)) {
    return PO.PrintIfConfig;
  }

  // Print everything else.
  return true;
}

void Pattern::print(llvm::raw_ostream &OS, const PrintOptions &Options) const {
  StreamPrinter StreamPrinter(OS);
  PrintAST Printer(StreamPrinter, Options);
  Printer.printPattern(this);
}

//===----------------------------------------------------------------------===//
//  Type Printing
//===----------------------------------------------------------------------===//

namespace {
class TypePrinter : public TypeVisitor<TypePrinter> {
  using super = TypeVisitor;

  ASTPrinter &Printer;
  const PrintOptions &Options;

  void printGenericArgs(ArrayRef<Type> Args) {
    if (Args.empty())
      return;

    Printer << "<";
    interleave(Args, [&](Type Arg) { visit(Arg); }, [&] { Printer << ", "; });
    Printer << ">";
  }

  /// Helper function for printing a type that is embedded within a larger type.
  ///
  /// This is necessary whenever the inner type may not normally be represented
  /// as a 'type-simple' production in the type grammar.
  void printWithParensIfNotSimple(Type T) {
    if (T.isNull()) {
      visit(T);
      return;
    }

    bool isSimple = T->hasSimpleTypeRepr();
    if (!isSimple && T->is<OpaqueTypeArchetypeType>()) {
      auto opaqueTy = T->castTo<OpaqueTypeArchetypeType>();
      switch (Options.OpaqueReturnTypePrinting) {
      case PrintOptions::OpaqueReturnTypePrintingMode::StableReference:
      case PrintOptions::OpaqueReturnTypePrintingMode::Description:
        isSimple = true;
        break;
      case PrintOptions::OpaqueReturnTypePrintingMode::WithOpaqueKeyword:
        isSimple = false;
        break;
      case PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword: {
        isSimple = opaqueTy->getExistentialType()->hasSimpleTypeRepr();
        break;
      }
      }
    }

    if (isSimple) {
      visit(T);
    } else {
      Printer << "(";
      visit(T);
      Printer << ")";
    }
  }

  template <typename T>
  void printModuleContext(T *Ty) {
    FileUnit *File = cast<FileUnit>(Ty->getDecl()->getModuleScopeContext());
    ModuleDecl *Mod = File->getParentModule();

    Identifier Name = Mod->getName();
    if (Options.UseExportedModuleNames)
      Name = Mod->getASTContext().getIdentifier(File->getExportedModuleName());

    Printer.printModuleRef(Mod, Name);
    Printer << ".";
  }

  template <typename T>
  void printTypeDeclName(
      T *Ty, PrintNameContext NameContext = PrintNameContext::Normal) {
    TypeDecl *TD = Ty->getDecl();
    Printer.printTypeRef(Ty, TD, TD->getName(), NameContext);
  }

  // FIXME: we should have a callback that would tell us
  // whether it's kosher to print a module name or not
  bool isLLDBExpressionModule(ModuleDecl *M) {
    if (!M)
      return false;
    return M->getName().str().startswith(LLDB_EXPRESSIONS_MODULE_NAME_PREFIX);
  }

  bool shouldPrintFullyQualified(TypeBase *T) {
    if (Options.FullyQualifiedTypes)
      return true;

    if (!Options.FullyQualifiedTypesIfAmbiguous)
      return false;

    Decl *D;
    if (auto *TAT = dyn_cast<TypeAliasType>(T))
      D = TAT->getDecl();
    else
      D = T->getAnyGeneric();

    // If we cannot find the declaration, be extra careful and print
    // the type qualified.
    if (!D)
      return true;

    ModuleDecl *M = D->getDeclContext()->getParentModule();

    if (Options.CurrentModule && M == Options.CurrentModule) {
      return false;
    }

    // Don't print qualifiers for types from the standard library.
    if (M->isStdlibModule() ||
        M->getName() == M->getASTContext().Id_ObjectiveC ||
        M->isSystemModule() ||
        isLLDBExpressionModule(M))
      return false;

    // Don't print qualifiers for imported types.
    for (auto File : M->getFiles()) {
      if (File->getKind() == FileUnitKind::ClangModule ||
          File->getKind() == FileUnitKind::DWARFModule)
        return false;
    }

    return true;
  }

public:
  TypePrinter(ASTPrinter &Printer, const PrintOptions &PO)
      : Printer(Printer), Options(PO) {}

  template <typename T>
  void printQualifiedType(T *Ty) {
    PrintNameContext NameContext = PrintNameContext::Normal;

    // If we printed a parent type or a module qualification, let the printer
    // know we're printing a type member so it escapes `Type` and `Protocol`.
    if (auto parent = Ty->getParent()) {
      visitParentType(parent);
      Printer << ".";
      NameContext = PrintNameContext::TypeMember;
    } else if (shouldPrintFullyQualified(Ty)) {
      printModuleContext(Ty);
      NameContext = PrintNameContext::TypeMember;
    }

    printTypeDeclName(Ty, NameContext);
  }

  void visit(Type T) {
    Printer.printTypePre(TypeLoc::withoutLoc(T));
    SWIFT_DEFER { Printer.printTypePost(TypeLoc::withoutLoc(T)); };

    super::visit(T);
  }

  void visitErrorType(ErrorType *T) {
    if (auto originalType = T->getOriginalType())
      visit(originalType);
    else
      Printer << "<<error type>>";
  }

  void visitUnresolvedType(UnresolvedType *T) {
    if (T->getASTContext().LangOpts.DebugConstraintSolver)
      Printer << "<<unresolvedtype>>";
    else
      Printer << "_";
  }

#ifdef ASTPRINTER_HANDLE_BUILTINTYPE
#error "ASTPRINTER_HANDLE_BUILTINTYPE should not be defined?!"
#endif

#define ASTPRINTER_PRINT_BUILTINTYPE(NAME)                                     \
  void visit##NAME(NAME *T) {                                                  \
    SmallString<32> buffer;                                                    \
    T->getTypeName(buffer);                                                    \
    Printer << buffer;                                                         \
  }
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinRawPointerType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinNativeObjectType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinBridgeObjectType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinUnsafeValueBufferType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinIntegerLiteralType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinVectorType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinIntegerType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinFloatType)
#undef ASTPRINTER_PRINT_BUILTINTYPE

  void visitSILTokenType(SILTokenType *T) {
    Printer << BUILTIN_TYPE_NAME_SILTOKEN;
  }

  void visitTypeAliasType(TypeAliasType *T) {
    if (Options.PrintForSIL || Options.PrintTypeAliasUnderlyingType) {
      visit(T->getSinglyDesugaredType());
      return;
    }

    printQualifiedType(T);
    printGenericArgs(T->getInnermostGenericArgs());
  }

  void visitParenType(ParenType *T) {
    Printer << "(";
    printParameterFlags(Printer, Options, T->getParameterFlags(),
                        /*escaping*/ false);
    visit(T->getUnderlyingType()->getInOutObjectType());
    Printer << ")";
  }

  void visitTupleType(TupleType *T) {
    Printer.callPrintStructurePre(PrintStructureKind::TupleType);
    SWIFT_DEFER { Printer.printStructurePost(PrintStructureKind::TupleType); };

    Printer << "(";

    auto Fields = T->getElements();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      if (i)
        Printer << ", ";
      const TupleTypeElt &TD = Fields[i];
      Type EltType = TD.getRawType();

      Printer.callPrintStructurePre(PrintStructureKind::TupleElement);
      SWIFT_DEFER {
        Printer.printStructurePost(PrintStructureKind::TupleElement);
      };

      if (TD.hasName()) {
        Printer.printName(TD.getName(), PrintNameContext::TupleElement);
        Printer << ": ";
      }
      if (TD.isVararg()) {
        visit(TD.getVarargBaseTy());
        Printer << "...";
      } else {
        printParameterFlags(Printer, Options, TD.getParameterFlags(),
                            /*escaping*/ false);
        visit(EltType);
      }
    }
    Printer << ")";
  }

  void visitUnboundGenericType(UnboundGenericType *T) {
    printQualifiedType(T);
  }

  void visitBoundGenericType(BoundGenericType *T) {
    if (Options.SynthesizeSugarOnTypes) {
      auto *NT = T->getDecl();
      auto &Ctx = T->getASTContext();
      if (NT == Ctx.getArrayDecl()) {
        Printer << "[";
        visit(T->getGenericArgs()[0]);
        Printer << "]";
        return;
      }
      if (NT == Ctx.getDictionaryDecl()) {
        Printer << "[";
        visit(T->getGenericArgs()[0]);
        Printer << " : ";
        visit(T->getGenericArgs()[1]);
        Printer << "]";
        return;
      }
      if (NT == Ctx.getOptionalDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        Printer << "?";
        return;
      }
    }
    printQualifiedType(T);
    printGenericArgs(T->getGenericArgs());
  }

  void visitParentType(Type T) {
    PrintOptions innerOptions = Options;
    innerOptions.SynthesizeSugarOnTypes = false;

    if (auto sugarType = dyn_cast<SyntaxSugarType>(T.getPointer()))
      T = sugarType->getImplementationType();

    TypePrinter(Printer, innerOptions).visit(T);
  }

  void visitEnumType(EnumType *T) {
    printQualifiedType(T);
  }

  void visitStructType(StructType *T) {
    printQualifiedType(T);
  }

  void visitClassType(ClassType *T) {
    printQualifiedType(T);
  }

  void visitAnyMetatypeType(AnyMetatypeType *T) {
    if (T->hasRepresentation()) {
      switch (T->getRepresentation()) {
      case MetatypeRepresentation::Thin:  Printer << "@thin ";  break;
      case MetatypeRepresentation::Thick: Printer << "@thick "; break;
      case MetatypeRepresentation::ObjC:  Printer << "@objc_metatype "; break;
      }
    }
    printWithParensIfNotSimple(T->getInstanceType());

    // We spell normal metatypes of existential types as .Protocol.
    if (isa<MetatypeType>(T) &&
        T->getInstanceType()->isAnyExistentialType()) {
      Printer << ".Protocol";
    } else {
      Printer << ".Type";
    }
  }

  void visitModuleType(ModuleType *T) {
    Printer << "module<";
    Printer.printModuleRef(T->getModule(), T->getModule()->getName());
    Printer << ">";
  }

  void visitDynamicSelfType(DynamicSelfType *T) {
    if (Options.PrintInSILBody) {
      Printer << "@dynamic_self ";
      visit(T->getSelfType());
      return;
    }

    // Try to print as a reference to the static type so that we will get a USR,
    // in cursor info.
    auto staticSelfT = T->getSelfType();

    if (auto *NTD = staticSelfT->getAnyNominal()) {
      if (isa<ClassDecl>(NTD)) {
        auto Name = T->getASTContext().Id_Self;
        Printer.printTypeRef(T, NTD, Name);
        return;
      }
    }

    visit(staticSelfT);
  }

  void printFunctionExtInfo(AnyFunctionType::ExtInfo info) {
    if (Options.SkipAttributes)
      return;


    if (Options.PrintFunctionRepresentationAttrs &&
        !Options.excludeAttrKind(TAK_convention) &&
        info.getSILRepresentation() != SILFunctionType::Representation::Thick) {
      Printer.callPrintStructurePre(PrintStructureKind::BuiltinAttribute);
      Printer.printAttrName("@convention");
      Printer << "(";
      // TODO: coalesce into a single convention attribute.
      switch (info.getSILRepresentation()) {
      case SILFunctionType::Representation::Thick:
        llvm_unreachable("thick is not printed");
      case SILFunctionType::Representation::Thin:
        Printer << "thin";
        break;
      case SILFunctionType::Representation::Block:
        Printer << "block";
        break;
      case SILFunctionType::Representation::CFunctionPointer:
        Printer << "c";
        break;
      case SILFunctionType::Representation::Method:
        Printer << "method";
        break;
      case SILFunctionType::Representation::ObjCMethod:
        Printer << "objc_method";
        break;
      case SILFunctionType::Representation::WitnessMethod:
        Printer << "witness_method";
        break;
      case SILFunctionType::Representation::Closure:
        Printer << "closure";
        break;
      }
      Printer << ")";
      Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
      Printer << " ";
    }
  }

  void printFunctionExtInfo(
      SILFunctionType::ExtInfo info,
      Optional<ProtocolConformanceRef> witnessMethodConformance) {
    if (Options.SkipAttributes)
      return;

    if (Options.PrintFunctionRepresentationAttrs &&
        !Options.excludeAttrKind(TAK_convention) &&
        info.getRepresentation() != SILFunctionType::Representation::Thick) {
      Printer.callPrintStructurePre(PrintStructureKind::BuiltinAttribute);
      Printer.printAttrName("@convention");
      Printer << "(";
      switch (info.getRepresentation()) {
      case SILFunctionType::Representation::Thick:
        llvm_unreachable("thick is not printed");
      case SILFunctionType::Representation::Thin:
        Printer << "thin";
        break;
      case SILFunctionType::Representation::Block:
        Printer << "block";
        break;
      case SILFunctionType::Representation::CFunctionPointer:
        Printer << "c";
        break;
      case SILFunctionType::Representation::Method:
        Printer << "method";
        break;
      case SILFunctionType::Representation::ObjCMethod:
        Printer << "objc_method";
        break;
      case SILFunctionType::Representation::WitnessMethod:
        Printer << "witness_method: ";
        printTypeDeclName(
            witnessMethodConformance->getRequirement()->getDeclaredType());
        break;
      case SILFunctionType::Representation::Closure:
        Printer << "closure";
        break;
      }
      Printer << ")";
      Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
      Printer << " ";
    }

    if (info.isPseudogeneric()) {
      Printer.printSimpleAttr("@pseudogeneric") << " ";
    }
    if (info.isNoEscape()) {
      Printer.printSimpleAttr("@noescape") << " ";
    }
  }

  void visitAnyFunctionTypeParams(ArrayRef<AnyFunctionType::Param> Params,
                                  bool printLabels) {
    Printer << "(";

    for (unsigned i = 0, e = Params.size(); i != e; ++i) {
      if (i)
        Printer << ", ";
      const AnyFunctionType::Param &Param = Params[i];

      Printer.callPrintStructurePre(PrintStructureKind::FunctionParameter);
      SWIFT_DEFER {
        Printer.printStructurePost(PrintStructureKind::FunctionParameter);
      };

      if (printLabels && Param.hasLabel()) {
        Printer.printName(Param.getLabel(),
                          PrintNameContext::FunctionParameterExternal);
        Printer << ": ";
      }

      auto type = Param.getPlainType();
      if (Param.isVariadic()) {
        visit(type);
        Printer << "...";
      } else {
        printParameterFlags(Printer, Options, Param.getParameterFlags(),
                            isEscaping(type));
        visit(type);
      }
    }

    Printer << ")";
  }

  void visitFunctionType(FunctionType *T) {
    Printer.callPrintStructurePre(PrintStructureKind::FunctionType);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::FunctionType);
    };

    printFunctionExtInfo(T->getExtInfo());

    // If we're stripping argument labels from types, do it when printing.
    visitAnyFunctionTypeParams(T->getParams(), /*printLabels*/false);

    if (T->throws())
      Printer << " " << tok::kw_throws;

    Printer << " -> ";

    Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
    T->getResult().print(Printer, Options);
    Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
  }

  void printGenericSignature(GenericSignature genericSig,
                             unsigned flags) {
    PrintAST(Printer, Options).printGenericSignature(genericSig, flags);
  }

  void visitGenericFunctionType(GenericFunctionType *T) {
    Printer.callPrintStructurePre(PrintStructureKind::FunctionType);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::FunctionType);
    };

    printFunctionExtInfo(T->getExtInfo());
    printGenericSignature(T->getGenericSignature(),
                          PrintAST::PrintParams |
                          PrintAST::PrintRequirements);
    Printer << " ";

   visitAnyFunctionTypeParams(T->getParams(), /*printLabels*/true);

    if (T->throws())
      Printer << " " << tok::kw_throws;

    Printer << " -> ";
    Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
    T->getResult().print(Printer, Options);
    Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
  }

  void printSILCoroutineKind(SILCoroutineKind kind) {
    switch (kind) {
    case SILCoroutineKind::None:
      return;
    case SILCoroutineKind::YieldOnce:
      Printer << "@yield_once ";
      return;
    case SILCoroutineKind::YieldMany:
      Printer << "@yield_many ";
      return;
    }
    llvm_unreachable("bad convention");
  }

  void printCalleeConvention(ParameterConvention conv) {
    switch (conv) {
    case ParameterConvention::Direct_Unowned:
      return;
    case ParameterConvention::Direct_Owned:
      Printer << "@callee_owned ";
      return;
    case ParameterConvention::Direct_Guaranteed:
      Printer << "@callee_guaranteed ";
      return;
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_In_Constant:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_InoutAliasable:
    case ParameterConvention::Indirect_In_Guaranteed:
      llvm_unreachable("callee convention cannot be indirect");
    }
    llvm_unreachable("bad convention");
  }

  void visitSILFunctionType(SILFunctionType *T) {
    printSILCoroutineKind(T->getCoroutineKind());
    printFunctionExtInfo(T->getExtInfo(),
                         T->getWitnessMethodConformanceOrNone());
    printCalleeConvention(T->getCalleeConvention());
    if (auto sig = T->getGenericSignature()) {
      printGenericSignature(sig,
                            PrintAST::PrintParams |
                            PrintAST::PrintRequirements);
      Printer << " ";
    }

    Printer << "(";
    bool first = true;
    for (auto param : T->getParameters()) {
      Printer.printSeparator(first, ", ");
      param.print(Printer, Options);
    }
    Printer << ") -> ";

    unsigned totalResults =
      T->getNumYields() + T->getNumResults() + unsigned(T->hasErrorResult());

    if (totalResults != 1) Printer << "(";

    first = true;

    for (auto yield : T->getYields()) {
      Printer.printSeparator(first, ", ");
      Printer << "@yields ";
      yield.print(Printer, Options);
    }

    for (auto result : T->getResults()) {
      Printer.printSeparator(first, ", ");
      result.print(Printer, Options);
    }

    if (T->hasErrorResult()) {
      // The error result is implicitly @owned; don't print that.
      assert(T->getErrorResult().getConvention() == ResultConvention::Owned);
      Printer.printSeparator(first, ", ");
      Printer << "@error ";
      T->getErrorResult().getType().print(Printer, Options);
    }

    if (totalResults != 1) Printer << ")";
  }

  void visitSILBlockStorageType(SILBlockStorageType *T) {
    Printer << "@block_storage ";
    printWithParensIfNotSimple(T->getCaptureType());
  }

  void visitSILBoxType(SILBoxType *T) {
    {
      // A box layout has its own independent generic environment. Don't try
      // to print it with the environment's generic params.
      PrintOptions subOptions = Options;
      subOptions.GenericEnv = nullptr;
      TypePrinter sub(Printer, subOptions);

      // Capture list used here to ensure we don't print anything using `this`
      // printer, but only the sub-Printer.
      [&sub, T]{
        if (auto sig = T->getLayout()->getGenericSignature()) {
          sub.printGenericSignature(sig,
                            PrintAST::PrintParams | PrintAST::PrintRequirements);
          sub.Printer << " ";
        }
        sub.Printer << "{";
        interleave(T->getLayout()->getFields(),
                   [&](const SILField &field) {
                     sub.Printer <<
                       (field.isMutable() ? " var " : " let ");
                     sub.visit(field.getLoweredType());
                   },
                   [&]{
                     sub.Printer << ",";
                   });
        sub.Printer << " }";
      }();
    }

    // The arguments to the layout, if any, do come from the outer environment.
    if (auto subMap = T->getSubstitutions()) {
      Printer << " <";
      interleave(subMap.getReplacementTypes(),
                 [&](Type type) {
                   visit(type);
                 }, [&]{
                   Printer << ", ";
                 });
      Printer << ">";
    }
  }

  void visitArraySliceType(ArraySliceType *T) {
    Printer << "[";
    visit(T->getBaseType());
    Printer << "]";
  }

  void visitDictionaryType(DictionaryType *T) {
    Printer << "[";
    visit(T->getKeyType());
    Printer << " : ";
    visit(T->getValueType());
    Printer << "]";
  }

  void visitOptionalType(OptionalType *T) {
    auto printAsIUO = Options.PrintOptionalAsImplicitlyUnwrapped;

    // Printing optionals with a trailing '!' applies only to
    // top-level optionals, not to any nested within.
    const_cast<PrintOptions &>(Options).PrintOptionalAsImplicitlyUnwrapped =
        false;
    printWithParensIfNotSimple(T->getBaseType());
    const_cast<PrintOptions &>(Options).PrintOptionalAsImplicitlyUnwrapped =
        printAsIUO;

    if (printAsIUO)
      Printer << "!";
    else
      Printer << "?";
  }

  void visitProtocolType(ProtocolType *T) {
    printQualifiedType(T);
  }

  void visitProtocolCompositionType(ProtocolCompositionType *T) {
    if (T->getMembers().empty()) {
      if (T->hasExplicitAnyObject())
        Printer << "AnyObject";
      else
        Printer << "Any";
    } else {
      interleave(T->getMembers(), [&](Type Ty) { visit(Ty); },
                 [&] { Printer << " & "; });
      if (T->hasExplicitAnyObject())
        Printer << " & AnyObject";
    }
  }

  void visitLValueType(LValueType *T) {
    Printer << "@lvalue ";
    visit(T->getObjectType());
  }

  void visitInOutType(InOutType *T) {
    Printer << tok::kw_inout << " ";
    visit(T->getObjectType());
  }

  void visitOpenedArchetypeType(OpenedArchetypeType *T) {
    if (Options.PrintForSIL)
      Printer << "@opened(\"" << T->getOpenedExistentialID() << "\") ";
    visit(T->getOpenedExistentialType());
  }
  
  void printArchetypeCommon(ArchetypeType *T) {
    if (Options.AlternativeTypeNames) {
      auto found = Options.AlternativeTypeNames->find(T->getCanonicalType());
      if (found != Options.AlternativeTypeNames->end()) {
        Printer << found->second.str();
        return;
      }
    }

    auto Name = T->getName();
    if (Name.empty())
      Printer << "<anonymous>";
    else {
      PrintNameContext context = PrintNameContext::Normal;
      if (Name == T->getASTContext().Id_Self)
        context = PrintNameContext::GenericParameter;
      Printer.printName(Name, context);
    }
  }
  
  void visitNestedArchetypeType(NestedArchetypeType *T) {
    printWithParensIfNotSimple(T->getParent());
    Printer << ".";
    printArchetypeCommon(T);
  }
  
  void visitPrimaryArchetypeType(PrimaryArchetypeType *T) {
    printArchetypeCommon(T);
  }
  
  void visitOpaqueTypeArchetypeType(OpaqueTypeArchetypeType *T) {
    switch (Options.OpaqueReturnTypePrinting) {
    case PrintOptions::OpaqueReturnTypePrintingMode::WithOpaqueKeyword:
      Printer.printKeyword("some", Options, /*Suffix=*/" ");
      LLVM_FALLTHROUGH;
    case PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword: {
      visit(T->getExistentialType());
      return;
    }
    case PrintOptions::OpaqueReturnTypePrintingMode::StableReference: {
      // Print the source of the opaque return type as a mangled name.
      // We'll use type reconstruction while parsing the attribute to
      // turn this back into a reference to the naming decl for the opaque
      // type.
      Printer << "@_opaqueReturnTypeOf(";
      OpaqueTypeDecl *decl = T->getDecl();
      
      Printer.printEscapedStringLiteral(
                                   decl->getOpaqueReturnTypeIdentifier().str());
      
      Printer << ", " << T->getInterfaceType()
                          ->castTo<GenericTypeParamType>()
                          ->getIndex();
      
      Printer << u8") \U0001F9B8";
      printGenericArgs(T->getSubstitutions().getReplacementTypes());
      return;
    }
    case PrintOptions::OpaqueReturnTypePrintingMode::Description: {
      // TODO(opaque): present opaque types with user-facing syntax. we should
      // probably print this as `some P` and record the fact that we printed that
      // so that diagnostics can add followup notes.
      Printer << "(return type of " << T->getDecl()->getNamingDecl()->printRef();
      Printer << ')';
      if (!T->getSubstitutions().empty()) {
        Printer << '<';
        auto replacements = T->getSubstitutions().getReplacementTypes();
        interleave(replacements.begin(), replacements.end(),
                   [&](Type t) { visit(t); },
                   [&] { Printer << ", "; });
        Printer << '>';
      }
      return;
    }
    }
  }

  void visitGenericTypeParamType(GenericTypeParamType *T) {
    if (T->getDecl() == nullptr) {
      // If we have an alternate name for this type, use it.
      if (Options.AlternativeTypeNames) {
        auto found = Options.AlternativeTypeNames->find(T->getCanonicalType());
        if (found != Options.AlternativeTypeNames->end()) {
          Printer << found->second.str();
          return;
        }
      }

      // When printing SIL types, use a generic environment to map them from
      // canonical types to sugared types.
      if (Options.GenericEnv)
        T = Options.GenericEnv->getSugaredType(T);
    }

    auto Name = T->getName();
    if (Name.empty())
      Printer << "<anonymous>";
    else {
      if (T->getDecl() &&
          T->getDecl()->getDeclContext()->getSelfProtocolDecl()) {
        Printer.printTypeRef(T, T->getDecl(), Name);
        return;
      }

      PrintNameContext context = PrintNameContext::Normal;
      if (Name == T->getASTContext().Id_Self)
        context = PrintNameContext::GenericParameter;
      Printer.printName(Name, context);
    }
  }

  void visitDependentMemberType(DependentMemberType *T) {
    visitParentType(T->getBase());
    Printer << ".";
    Printer.printName(T->getName());
  }

#define REF_STORAGE(Name, name, ...) \
  void visit##Name##StorageType(Name##StorageType *T) { \
    if (Options.PrintStorageRepresentationAttrs) \
      Printer << "@sil_" #name " "; \
    visit(T->getReferentType()); \
  }
#include "swift/AST/ReferenceStorage.def"

  void visitTypeVariableType(TypeVariableType *T) {
    if (T->getASTContext().LangOpts.DebugConstraintSolver) {
      Printer << "$T" << T->getID();
      return;
    }

    Printer << "_";
  }
};
} // unnamed namespace

void Type::print(raw_ostream &OS, const PrintOptions &PO) const {
  StreamPrinter Printer(OS);
  print(Printer, PO);
}
void Type::print(ASTPrinter &Printer, const PrintOptions &PO) const {
  if (isNull()) {
    if (!PO.AllowNullTypes) {
      // Use report_fatal_error instead of assert to trap in release builds too.
      llvm::report_fatal_error("Cannot pretty-print a null type");
    }
    Printer << "<null>";
    return;
  }
  TypePrinter(Printer, PO).visit(*this);
}

void AnyFunctionType::printParams(ArrayRef<AnyFunctionType::Param> Params,
                                  raw_ostream &OS,
                                  const PrintOptions &PO) {
  StreamPrinter Printer(OS);
  printParams(Params, Printer, PO);
}
void AnyFunctionType::printParams(ArrayRef<AnyFunctionType::Param> Params,
                                  ASTPrinter &Printer,
                                  const PrintOptions &PO) {
  TypePrinter(Printer, PO).visitAnyFunctionTypeParams(Params,
                                                      /*printLabels*/true);
}

std::string
AnyFunctionType::getParamListAsString(ArrayRef<AnyFunctionType::Param> Params,
                                      const PrintOptions &PO) {
  SmallString<16> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  AnyFunctionType::printParams(Params, OS);
  return OS.str();
}

void LayoutConstraintInfo::print(raw_ostream &OS,
                                 const PrintOptions &PO) const {
  StreamPrinter Printer(OS);
  print(Printer, PO);
}

void LayoutConstraint::print(raw_ostream &OS,
                             const PrintOptions &PO) const {
  assert(*this);
  getPointer()->print(OS, PO);
}

void LayoutConstraintInfo::print(ASTPrinter &Printer,
                                 const PrintOptions &PO) const {
  Printer << getName();
  switch (getKind()) {
  case LayoutConstraintKind::UnknownLayout:
  case LayoutConstraintKind::RefCountedObject:
  case LayoutConstraintKind::NativeRefCountedObject:
  case LayoutConstraintKind::Class:
  case LayoutConstraintKind::NativeClass:
  case LayoutConstraintKind::Trivial:
    return;
  case LayoutConstraintKind::TrivialOfAtMostSize:
  case LayoutConstraintKind::TrivialOfExactSize:
    Printer << "(";
    Printer << SizeInBits;
    if (Alignment)
      Printer << ", " << Alignment;
    Printer << ")";
    break;
  }
}

void GenericSignatureImpl::print(raw_ostream &OS, PrintOptions PO) const {
  GenericSignature(const_cast<GenericSignatureImpl *>(this)).print(OS, PO);
}
void GenericSignatureImpl::print(ASTPrinter &Printer, PrintOptions PO) const {
  GenericSignature(const_cast<GenericSignatureImpl *>(this)).print(Printer, PO);
}

void GenericSignature::print(raw_ostream &OS, PrintOptions Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}

void GenericSignature::print(ASTPrinter &Printer, PrintOptions Opts) const {
  if (isNull()) {
    Printer << "<null>";
    return;
  }
  PrintAST(Printer, Opts).printGenericSignature(*this,
                                                PrintAST::PrintParams |
                                                PrintAST::PrintRequirements);
}

void GenericSignature::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Requirement::dump() const {
  dump(llvm::errs());
  llvm::errs() << '\n';
}
void Requirement::dump(raw_ostream &out) const {
  switch (getKind()) {
  case RequirementKind::Conformance:
    out << "conforms_to: ";
    break;
  case RequirementKind::Layout:
    out << "layout: ";
    break;
  case RequirementKind::Superclass:
    out << "superclass: ";
    break;
  case RequirementKind::SameType:
    out << "same_type: ";
    break;
  }

  if (getFirstType())
    out << getFirstType() << " ";
  if (getKind() != RequirementKind::Layout && getSecondType())
    out << getSecondType();
  else if (getLayoutConstraint())
    out << getLayoutConstraint();
}

void Requirement::print(raw_ostream &os, const PrintOptions &opts) const {
  StreamPrinter printer(os);
  PrintAST(printer, opts).printRequirement(*this);
}

void Requirement::print(ASTPrinter &printer, const PrintOptions &opts) const {
  PrintAST(printer, opts).printRequirement(*this);
}

std::string GenericSignatureImpl::getAsString() const {
  return GenericSignature(const_cast<GenericSignatureImpl *>(this))
      .getAsString();
}

std::string GenericSignature::getAsString() const {
  std::string result;
  llvm::raw_string_ostream out(result);
  print(out);
  return out.str();
}

static StringRef getStringForParameterConvention(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Indirect_In: return "@in ";
  case ParameterConvention::Indirect_In_Constant:
    return "@in_constant ";
  case ParameterConvention::Indirect_In_Guaranteed:  return "@in_guaranteed ";
  case ParameterConvention::Indirect_Inout: return "@inout ";
  case ParameterConvention::Indirect_InoutAliasable: return "@inout_aliasable ";
  case ParameterConvention::Direct_Owned: return "@owned ";
  case ParameterConvention::Direct_Unowned: return "";
  case ParameterConvention::Direct_Guaranteed: return "@guaranteed ";
  }
  llvm_unreachable("bad parameter convention");
}

StringRef swift::getCheckedCastKindName(CheckedCastKind kind) {
  switch (kind) {
  case CheckedCastKind::Unresolved:
    return "unresolved";
  case CheckedCastKind::Coercion:
    return "coercion";
  case CheckedCastKind::ValueCast:
    return "value_cast";
  case CheckedCastKind::ArrayDowncast:
    return "array_downcast";
  case CheckedCastKind::DictionaryDowncast:
    return "dictionary_downcast";
  case CheckedCastKind::SetDowncast:
    return "set_downcast";
  case CheckedCastKind::BridgingCoercion:
    return "bridging_coercion";
  }
  llvm_unreachable("bad checked cast name");
}

void SILParameterInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void SILParameterInfo::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}
void SILParameterInfo::print(ASTPrinter &Printer,
                             const PrintOptions &Opts) const {
  Printer << getStringForParameterConvention(getConvention());
  getType().print(Printer, Opts);
}

static StringRef getStringForResultConvention(ResultConvention conv) {
  switch (conv) {
  case ResultConvention::Indirect: return "@out ";
  case ResultConvention::Owned: return "@owned ";
  case ResultConvention::Unowned: return "";
  case ResultConvention::UnownedInnerPointer: return "@unowned_inner_pointer ";
  case ResultConvention::Autoreleased: return "@autoreleased ";
  }
  llvm_unreachable("bad result convention");
}

void SILResultInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void SILResultInfo::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}
void SILResultInfo::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  Printer << getStringForResultConvention(getConvention());
  getType().print(Printer, Opts);
}

std::string Type::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

std::string TypeBase::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

std::string Type::getStringAsComponent(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);

  if (getPointer()->hasSimpleTypeRepr()) {
    print(OS, PO);
  } else {
    OS << "(";
    print(OS, PO);
    OS << ")";
  }

  return OS.str();
}

std::string TypeBase::getStringAsComponent(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);

  if (hasSimpleTypeRepr()) {
    print(OS, PO);
  } else {
    OS << "(";
    print(OS, PO);
    OS << ")";
  }

  return OS.str();
}

void TypeBase::dumpPrint() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void TypeBase::print(raw_ostream &OS, const PrintOptions &PO) const {
  Type(const_cast<TypeBase *>(this)).print(OS, PO);
}
void TypeBase::print(ASTPrinter &Printer, const PrintOptions &PO) const {
  Type(const_cast<TypeBase *>(this)).print(Printer, PO);
}

std::string LayoutConstraint::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

std::string LayoutConstraintInfo::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

void ProtocolConformance::printName(llvm::raw_ostream &os,
                                    const PrintOptions &PO) const {
  if (getKind() == ProtocolConformanceKind::Normal) {
    if (auto genericSig = getGenericSignature()) {
      StreamPrinter sPrinter(os);
      TypePrinter typePrinter(sPrinter, PO);
      typePrinter
          .printGenericSignature(genericSig,
                                 PrintAST::PrintParams |
                                 PrintAST::PrintRequirements);
      os << ' ';
    }
  }

  getType()->print(os, PO);
  os << ": ";

  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto normal = cast<NormalProtocolConformance>(this);
    os << normal->getProtocol()->getName()
       << " module " << normal->getDeclContext()->getParentModule()->getName();
    break;
  }
  case ProtocolConformanceKind::Self: {
    auto self = cast<SelfProtocolConformance>(this);
    os << self->getProtocol()->getName()
       << " module " << self->getDeclContext()->getParentModule()->getName();
    break;
  }
  case ProtocolConformanceKind::Specialized: {
    auto spec = cast<SpecializedProtocolConformance>(this);
    os << "specialize <";
    interleave(spec->getSubstitutionMap().getReplacementTypes(),
               [&](Type type) { type.print(os, PO); },
               [&] { os << ", "; });

    os << "> (";
    spec->getGenericConformance()->printName(os, PO);
    os << ")";
    break;
  }
  case ProtocolConformanceKind::Inherited: {
    auto inherited = cast<InheritedProtocolConformance>(this);
    os << "inherit (";
    inherited->getInheritedConformance()->printName(os, PO);
    os << ")";
    break;
  }
  }
}

void swift::printEnumElementsAsCases(
    llvm::DenseSet<EnumElementDecl *> &UnhandledElements,
    llvm::raw_ostream &OS) {
  // Sort the missing elements to a vector because set does not guarantee
  // orders.
  SmallVector<EnumElementDecl *, 4> SortedElements;
  SortedElements.insert(SortedElements.begin(), UnhandledElements.begin(),
                        UnhandledElements.end());
  std::sort(SortedElements.begin(), SortedElements.end(),
            [](EnumElementDecl *LHS, EnumElementDecl *RHS) {
              return LHS->getNameStr().compare(RHS->getNameStr()) < 0;
            });

  auto printPayloads = [](ParameterList *PL, llvm::raw_ostream &OS) {
    // If the enum element has no payloads, return.
    if (!PL)
      return;
    OS << "(";
    // Print each element in the pattern match.
    for (auto i = PL->begin(); i != PL->end(); ++i) {
      auto *param = *i;
      if (param->hasName()) {
        OS << tok::kw_let << " " << param->getName().str();
      } else {
        OS << "_";
      }
      if (i + 1 != PL->end()) {
        OS << ", ";
      }
    }
    OS << ")";
  };

  // Print each enum element name.
  std::for_each(SortedElements.begin(), SortedElements.end(),
                [&](EnumElementDecl *EE) {
                  OS << tok::kw_case << " ." << EE->getNameStr();
                  printPayloads(EE->getParameterList(), OS);
                  OS << ": " << getCodePlaceholder() << "\n";
                });
}

void
swift::getInheritedForPrinting(const Decl *decl, const PrintOptions &options,
                               llvm::SmallVectorImpl<TypeLoc> &Results) {
  ArrayRef<TypeLoc> inherited;
  if (auto td = dyn_cast<TypeDecl>(decl)) {
    inherited = td->getInherited();
  } else if (auto ed = dyn_cast<ExtensionDecl>(decl)) {
    inherited = ed->getInherited();
  }

  // Collect explicit inherited types.
  for (auto TL: inherited) {
    if (auto ty = TL.getType()) {
      bool foundUnprintable = ty.findIf([&options](Type subTy) {
        if (auto aliasTy = dyn_cast<TypeAliasType>(subTy.getPointer()))
          return !options.shouldPrint(aliasTy->getDecl());
        if (auto NTD = subTy->getAnyNominal())
          return !options.shouldPrint(NTD);
        return false;
      });
      if (foundUnprintable)
        continue;
    }
    Results.push_back(TL);
  }

  // Collect synthesized conformances.
  auto &ctx = decl->getASTContext();
  for (auto attr : decl->getAttrs().getAttributes<SynthesizedProtocolAttr>()) {
    if (auto *proto = ctx.getProtocol(attr->getProtocolKind())) {
      if (!options.shouldPrint(proto))
        continue;
      if (attr->getProtocolKind() == KnownProtocolKind::RawRepresentable &&
          isa<EnumDecl>(decl) &&
          cast<EnumDecl>(decl)->hasRawType())
        continue;
      Results.push_back(TypeLoc::withoutLoc(proto->getDeclaredType()));
    }
  }
}
