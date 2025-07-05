//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
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
#include "FeatureSet.h"
#include "InlinableText.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/Comment.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SILLayout.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Feature.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Basic/QuotedString.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/Unicode.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "swift/Config.h"
#include "swift/Parse/Lexer.h"
#include "swift/Strings.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/MacroInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <queue>

using namespace swift;

#ifndef NDEBUG
static llvm::cl::opt<bool> NumberSuppressionChecks(
    "swift-ast-printer-number-suppression-checks",
    llvm::cl::desc("Used to number suppression checks in swift interface files "
                   "to make it easier to FileCheck them. Only available with "
                   "asserts enabled and intended for compiler tests."),
    llvm::cl::init(false), llvm::cl::Hidden);
#endif

// Defined here to avoid repeatedly paying the price of template instantiation.
const std::function<bool(const ExtensionDecl *)>
    PrintOptions::defaultPrintExtensionContentAsMembers
        = [] (const ExtensionDecl *) { return false; };

void PrintOptions::setBaseType(Type T) {
  if (T->is<ErrorType>())
    return;
  if (auto DynamicSelf = T->getAs<DynamicSelfType>()) {
    // TypeTransformContext requires `T` to have members. Look through dynamic
    // Self.
    T = DynamicSelf->getSelfType();
  }
  TransformContext = TypeTransformContext(T);
}

void PrintOptions::initForSynthesizedExtension(TypeOrExtensionDecl D) {
  TransformContext = TypeTransformContext(D);
}

void PrintOptions::initForSynthesizedExtensionInScope(TypeOrExtensionDecl D,
                                                      OverrideScope &scope) const {
  assert(this == &scope.Options);
  OVERRIDE_PRINT_OPTION_UNCONDITIONAL(scope, TransformContext,
                                      TypeTransformContext(D));
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

static bool isPackage(const ValueDecl *VD) {
  AccessScope scope =
      VD->getFormalAccessScope(/*useDC*/nullptr,
                               /*treatUsableFromInlineAsPublic*/true);
  return scope.isPackage();
}

static bool isPackage(Type ty) {
  // \see isPublicOrUsableFromInline(Type ty)
  return !ty.findIf([](Type typePart) -> bool {
    if (auto *aliasTy = dyn_cast<TypeAliasType>(typePart.getPointer()))
      return !isPackage(aliasTy->getDecl());
    if (auto *nominal = typePart->getAnyNominal())
      return !isPackage(nominal);
    return false;
  });
}

static bool isPrespecilizationDeclWithTarget(const ValueDecl *vd) {
  // Add exported prespecialized symbols.
  for (auto *attr : vd->getAttrs().getAttributes<AbstractSpecializeAttr>()) {
    if (!attr->isExported())
      continue;
    if (attr->getTargetFunctionDecl(vd))
      return true;
  }
  return false;
}

static bool contributesToParentTypeStorage(const AbstractStorageDecl *ASD) {
  auto *DC = ASD->getDeclContext()->getAsDecl();
  if (!DC) return false;
  auto *ND = dyn_cast<NominalTypeDecl>(DC);
  if (!ND) return false;
  return !ND->isResilient() && ASD->hasStorage() && !ASD->isStatic();
}

static bool isInObjCImpl(const ValueDecl *VD) {
  auto *ED = dyn_cast<ExtensionDecl>(VD->getDeclContext());
  return ED && ED->isObjCImplementation();
}

/// Triggering type checking requests while printing is desirable in compiler
/// modes in which type checking is lazy and the printed content is expected to
/// be complete (for example, when printing a .swiftinterface). In other
/// contexts, though, triggering type checking could cause re-entrancy and
/// should be avoided.
static bool shouldPrintAllSemanticDetails(const PrintOptions &options) {
  if (options.IsForSwiftInterface)
    return true;

  return false;
}

/// Get the non-recursive printing options that should be applied when
/// printing the type of a value decl.
static NonRecursivePrintOptions getNonRecursiveOptions(const ValueDecl *D) {
  NonRecursivePrintOptions options;
  if (D->isImplicitlyUnwrappedOptional())
    options |= NonRecursivePrintOption::ImplicitlyUnwrappedOptional;
  return options;
}

bool PrintOptions::excludeAttr(const DeclAttribute *DA) const {
  if (excludeAttrKind(DA->getKind())) {
    return true;
  }
  if (auto CA = dyn_cast<CustomAttr>(DA)) {
    if (std::any_of(ExcludeCustomAttrList.begin(), ExcludeCustomAttrList.end(),
                    [CA](CustomAttr *other) { return other == CA; }))
      return true;
  }
  return false;
}

/// Forces printing types with the `some` keyword, instead of the full stable
/// reference.
struct PrintWithOpaqueResultTypeKeywordRAII {
  PrintWithOpaqueResultTypeKeywordRAII(const PrintOptions &options)
      : Options(const_cast<PrintOptions&>(options)) {
    SavedMode = options.OpaqueReturnTypePrinting;
    Options.OpaqueReturnTypePrinting =
        PrintOptions::OpaqueReturnTypePrintingMode::WithOpaqueKeyword;
  }
  ~PrintWithOpaqueResultTypeKeywordRAII() {
    Options.OpaqueReturnTypePrinting = SavedMode;
  }

private:
  PrintOptions &Options;
  PrintOptions::OpaqueReturnTypePrintingMode SavedMode;
};

PrintOptions PrintOptions::printSwiftInterfaceFile(ModuleDecl *ModuleToPrint,
                                                   bool preferTypeRepr,
                                                   bool printFullConvention,
                                                   InterfaceMode interfaceMode,
                                                   bool useExportedModuleNames,
                                                   bool aliasModuleNames,
                                                   llvm::SmallSet<StringRef, 4>
                                                     *aliasModuleNamesTargets
                                                   ) {
  PrintOptions result;
  result.IsForSwiftInterface = true;
  result.PrintLongAttrsOnSeparateLines = true;
  result.TypeDefinitions = true;
  result.CurrentModule = ModuleToPrint;
  result.FullyQualifiedTypes = true;
  result.FullyQualifiedTypesIfAmbiguous = true;
  result.FullyQualifiedExtendedTypesIfAmbiguous = true;
  result.UseExportedModuleNames = useExportedModuleNames;
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
  result.AliasModuleNames = aliasModuleNames;
  result.AliasModuleNamesTargets = aliasModuleNamesTargets;
  if (printFullConvention)
    result.PrintFunctionRepresentationAttrs =
      PrintOptions::FunctionRepresentationMode::Full;
  result.AlwaysTryPrintParameterLabels = true;
  result.InterfaceContentKind = interfaceMode;
  result.DesugarExistentialConstraint = true;

  // We should print __consuming, __owned, etc for the module interface file.
  result.SkipUnderscoredKeywords = false;

  // We should provide backward-compatible Swift interfaces when we can.
  result.PrintCompatibilityFeatureChecks = true;

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
      if (!D)
        return false;

      // Skip anything that is marked `@_implementationOnly` itself.
      if (D->getAttrs().hasAttribute<ImplementationOnlyAttr>())
        return false;

      // Skip SPI decls if `PrintSPIs`.
      if (options.printPublicInterface() && D->isSPI())
        return false;

      if (auto *VD = dyn_cast<ValueDecl>(D)) {
        // Skip anything that isn't 'public' or '@usableFromInline' or has a
        // _specialize attribute with a targetFunction parameter.
        if (!isPublicOrUsableFromInline(VD) &&
            !isPrespecilizationDeclWithTarget(VD)) {
          // We do want to print private stored properties, without their
          // original names present.
          if (auto *ASD = dyn_cast<AbstractStorageDecl>(VD))
            if (contributesToParentTypeStorage(ASD))
              return true;

          if (!options.printPackageInterface() || !isPackage(VD))
            return false;
        }

        // Skip member implementations and @objc overrides in @objcImpl
        // extensions.
        if (VD->isObjCMemberImplementation()
            || (isInObjCImpl(VD) && VD->getOverriddenDecl() && VD->isObjC())) {
          return false;
        }
      }

      // Skip extensions that extend things we wouldn't print.
      if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
        if (!shouldPrint(ED->getExtendedNominal(), options))
          return false;

        // Skip extensions to implementation-only imported types that have
        // no public members.
        auto localModule = ED->getParentModule();
        auto nominalModule = ED->getExtendedNominal()->getParentModule();
        if (localModule != nominalModule &&
            localModule->isImportedImplementationOnly(nominalModule)) {

          bool shouldPrintMembers = llvm::any_of(
                                      ED->getAllMembers(),
                                      [&](const Decl *member) -> bool {
            return shouldPrint(member, options);
          });

          if (!shouldPrintMembers)
            return false;
        }

        for (const Requirement &req : ED->getGenericRequirements()) {
          if (!isPublicOrUsableFromInline(req.getFirstType())) {
            if (!options.printPackageInterface() || !isPackage(req.getSecondType()))
              return false;
          }

          switch (req.getKind()) {
          case RequirementKind::Conformance:
          case RequirementKind::Superclass:
          case RequirementKind::SameType:
            if (!isPublicOrUsableFromInline(req.getSecondType())) {
              if (!options.printPackageInterface() || !isPackage(req.getSecondType()))
                return false;
            }
            break;
          case RequirementKind::SameShape:
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

      // Skip stub constructors.
      if (auto *ctor = dyn_cast<ConstructorDecl>(D)) {
        if (ctor->hasStubImplementation())
          return false;
      }

      // Skip enum cases containing enum elements we wouldn't print.
      if (auto *ECD = dyn_cast<EnumCaseDecl>(D)) {
        if (auto *element = ECD->getFirstElement()) {
          // Enum elements are usually not printed, so we have to override the
          // print option controlling that.
          PrintOptions::OverrideScope scope(options);
          OVERRIDE_PRINT_OPTION(scope, ExplodeEnumCaseDecls, true);
          if (!shouldPrint(element, options))
            return false;
        }
      }

      // The `using` declarations are private to the file at the moment
      // and shouldn't appear in swift interfaces.
      if (isa<UsingDecl>(D))
        return false;

      return ShouldPrintChecker::shouldPrint(D, options);
    }
  };
  result.CurrentPrintabilityChecker =
      std::make_shared<ShouldPrintForModuleInterface>();

  // FIXME: We don't really need 'public' on everything; we could just change
  // the default to 'public' and mark the 'internal' things.
  result.PrintAccess = true;

  result.ExcludeAttrList = {
      DeclAttrKind::AccessControl,
      DeclAttrKind::SetterAccess,
      DeclAttrKind::Lazy,
      DeclAttrKind::ObjCImplementation,
      DeclAttrKind::StaticInitializeObjCMetadata,
      DeclAttrKind::RestatedObjCConformance,
      DeclAttrKind::NonSendable,
      DeclAttrKind::AllowFeatureSuppression,
  };

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

// Returns true if the given declaration is backed by a C++ template
// specialization.
static bool isSpecializedCxxDecl(const Decl *D) {
  return D->hasClangNode() &&
         isa<clang::ClassTemplateSpecializationDecl>(D->getClangDecl());
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

  printName(Name, Context, isSpecializedCxxDecl(RefTo));
}

void ASTPrinter::printModuleRef(ModuleEntity Mod, Identifier Name) {
  printName(Name);
}

void ASTPrinter::callPrintDeclPre(const Decl *D,
                                  std::optional<BracketOptions> Bracket) {
  forceNewlines();

  if (SynthesizeTarget && isa<ExtensionDecl>(D))
    printSynthesizedExtensionPre(cast<ExtensionDecl>(D), SynthesizeTarget, Bracket);
  else
    printDeclPre(D, Bracket);
}

ASTPrinter &ASTPrinter::operator<<(QuotedString s) {
  llvm::SmallString<32> Str;
  llvm::raw_svector_ostream OS(Str);
  OS << s;
  printTextImpl(OS.str());
  return *this;
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

ASTPrinter &ASTPrinter::operator<<(Identifier name) {
  return *this << DeclName(name);
}

ASTPrinter &ASTPrinter::operator<<(DeclBaseName name) {
  return *this << DeclName(name);
}

ASTPrinter &ASTPrinter::operator<<(DeclName name) {
  llvm::SmallString<32> str;
  llvm::raw_svector_ostream os(str);
  name.print(os);
  printTextImpl(os.str());
  return *this;
}

ASTPrinter &ASTPrinter::operator<<(DeclNameRef ref) {
  llvm::SmallString<32> str;
  llvm::raw_svector_ostream os(str);
  ref.print(os);
  printTextImpl(os.str());
  return *this;
}

llvm::raw_ostream &swift::
operator<<(llvm::raw_ostream &OS, tok keyword) {
  switch (keyword) {
#define KEYWORD(KW) case tok::kw_##KW: OS << #KW; break;
#define POUND_KEYWORD(KW) case tok::pound_##KW: OS << "#"#KW; break;
#define PUNCTUATOR(PUN, TEXT) case tok::PUN: OS << TEXT; break;
#include "swift/AST/TokenKinds.def"
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
#include "swift/AST/TokenKinds.def"
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
bool swift::escapeIdentifierInContext(Identifier name, PrintNameContext context,
                                      bool isSpecializedCxxTemplate) {
  StringRef keyword = name.str();
  bool isKeyword = llvm::StringSwitch<bool>(keyword)
#define KEYWORD(KW) \
      .Case(#KW, true)
#include "swift/AST/TokenKinds.def"
      .Default(false);

  // NB: ClangImporter synthesizes C++ template specializations with Identifiers
  // that contain the full type signature; e.g., a type named literally
  // `X<Y, Z>`. These would normally be interpreted as raw identifiers and
  // escaped with backticks, but we need to avoid that for those specific decls.

  switch (context) {
  case PrintNameContext::Normal:
  case PrintNameContext::Attribute:
    return isKeyword ||
           (!isSpecializedCxxTemplate && name.mustAlwaysBeEscaped());
  case PrintNameContext::Keyword:
  case PrintNameContext::IntroducerKeyword:
    return false;

  case PrintNameContext::ClassDynamicSelf:
  case PrintNameContext::GenericParameter:
    return isKeyword && keyword != "Self";

  case PrintNameContext::TypeMember:
    return isKeyword || !canBeMemberName(keyword) ||
           (!isSpecializedCxxTemplate && name.mustAlwaysBeEscaped());

  case PrintNameContext::FunctionParameterExternal:
  case PrintNameContext::FunctionParameterLocal:
  case PrintNameContext::TupleElement:
    return !canBeArgumentLabel(keyword) ||
           (!isSpecializedCxxTemplate && name.mustAlwaysBeEscaped());
  }

  llvm_unreachable("Unhandled PrintNameContext in switch.");
}

void ASTPrinter::printName(Identifier Name, PrintNameContext Context,
                           bool IsSpecializedCxxTemplate) {
  callPrintNamePre(Context);

  if (Name.empty()) {
    *this << "_";
    printNamePost(Context);
    return;
  }

  bool shouldEscapeIdentifier =
      escapeIdentifierInContext(Name, Context, IsSpecializedCxxTemplate);

  if (shouldEscapeIdentifier)
    *this << "`";
  *this << Name.str();
  if (shouldEscapeIdentifier)
    *this << "`";

  printNamePost(Context);
}

void StreamPrinter::printText(StringRef Text) {
  OS << Text;
}

/// Whether we will be printing a TypeLoc by using the TypeRepr printer
static bool willUseTypeReprPrinting(TypeLoc tyLoc,
                                    Type currentType,
                                    const PrintOptions &options) {
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
  const PrintOptions &Options;
  unsigned IndentLevel = 0;
  Decl *Current = nullptr;
  Type CurrentType;

  void setCurrentType(Type NewCurrentType) {
    CurrentType = NewCurrentType;
    assert(CurrentType.isNull() ||
           !CurrentType->hasArchetype() &&
               "CurrentType should be an interface type");
  }

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
    indent();
    bool FirstLine = true;
    for (auto Line : Lines) {
      if (FirstLine)
        Printer << sanitizeClangDocCommentStyle(unicode::sanitizeUTF8(Line));
      else
        Printer << unicode::sanitizeUTF8(Line);
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
      unsigned WhitespaceToTrim = SRC.ColumnIndent - 1;
      trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);

      for (auto Line : Lines) {
        Printer << Line;
        Printer.printNewline();
      }
    }
  }

  void printSwiftDocumentationComment(const Decl *D) {
    if (Options.CascadeDocComment)
      D = D->getDocCommentProvidingDecl();
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
    case AccessLevel::Package:
      Printer.printKeyword("package", Options);
      break;
    case AccessLevel::Open:
      Printer.printKeyword("open", Options);
      break;
    }
    Printer << suffix << " ";
  }

  void printAccess(const ValueDecl *D) {
    assert(!llvm::is_contained(Options.ExcludeAttrList,
                               DeclAttrKind::AccessControl) ||
           llvm::is_contained(Options.ExcludeAttrList,
                              DeclAttrKind::SetterAccess));

    if (!Options.PrintAccess || isa<ProtocolDecl>(D->getDeclContext()))
      return;
    if (D->getAttrs().hasAttribute<AccessControlAttr>() &&
        !llvm::is_contained(Options.ExcludeAttrList,
                            DeclAttrKind::AccessControl))
      return;
    if (D->getDeclContext()->isLocalContext())
      return;
    
    if (Options.SuppressIsolatedDeinit &&
        D->getFormalAccess() == AccessLevel::Open &&
        usesFeatureIsolatedDeinit(D)) {
      printAccess(AccessLevel::Public);
    } else {
      printAccess(D->getFormalAccess());
    }
    bool shouldSkipSetterAccess =
        llvm::is_contained(Options.ExcludeAttrList, DeclAttrKind::SetterAccess);

    if (auto storageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      if (auto setter = storageDecl->getAccessor(AccessorKind::Set)) {
        AccessLevel setterAccess = setter->getFormalAccess();
        if (setterAccess != D->getFormalAccess() && !shouldSkipSetterAccess)
          printAccess(setterAccess, "(set)");
      }
    }
  }

  void printType(Type T,
                 NonRecursivePrintOptions outermostOptions = std::nullopt) {
    if (Options.TransformContext) {
      // FIXME: it's not clear exactly what we want to keep from the existing
      // options, and what we want to discard.
      PrintOptions FreshOptions;
      FreshOptions.ExcludeAttrList = Options.ExcludeAttrList;
      FreshOptions.ExclusiveAttrList = Options.ExclusiveAttrList;
      FreshOptions.TransformContext = Options.TransformContext;
      FreshOptions.CurrentModule = Options.CurrentModule;
      FreshOptions.FullyQualifiedTypesIfAmbiguous = Options.FullyQualifiedTypesIfAmbiguous;
      T.print(Printer, FreshOptions, outermostOptions);
      return;
    }

    T.print(Printer, Options, outermostOptions);
  }

  Type getTransformedType(Type T) {
    if (CurrentType && Current && CurrentType->mayHaveMembers()) {
      SubstitutionMap subMap;

      if (auto *NTD = dyn_cast<NominalTypeDecl>(Current))
        subMap = CurrentType->getContextSubstitutionMap(NTD);
      else if (auto *ED = dyn_cast<ExtensionDecl>(Current))
        subMap = CurrentType->getContextSubstitutionMap(ED);
      else {
        Decl *subTarget = Current;
        if (isa<ParamDecl>(Current)) {
          auto *DC = Current->getDeclContext();
          if (auto *FD = dyn_cast<AbstractFunctionDecl>(DC))
            subTarget = FD;
        }
        subMap = CurrentType->getMemberSubstitutionMap(
          cast<ValueDecl>(subTarget));
      }

      T = T.subst(subMap, SubstFlags::DesugarMemberTypes);
    }

    return T;
  }

  void printTransformedType(Type T,
                            NonRecursivePrintOptions outermostOptions = std::nullopt) {
    T = getTransformedType(T);

    PrintOptions::OverrideScope scope(Options);
    if (CurrentType && Current && CurrentType->mayHaveMembers())
      OVERRIDE_PRINT_OPTION_UNCONDITIONAL(scope, TransformContext,
                                          TypeTransformContext(CurrentType));

    printType(T, outermostOptions);
  }

  void printTypeLoc(const TypeLoc &TL,
        NonRecursivePrintOptions outermostOptions = std::nullopt,
        std::optional<llvm::function_ref<void()>> printBeforeType = std::nullopt) {
    if (CurrentType && TL.getType()) {
      if (printBeforeType) (*printBeforeType)();
      printTransformedType(TL.getType(), outermostOptions);
      return;
    }

    // Print a TypeRepr if instructed to do so by options, or if the type
    // is null.
    if (willUseTypeReprPrinting(TL, CurrentType, Options)) {
      if (auto repr = TL.getTypeRepr())
        repr->print(Printer, Options, outermostOptions);
      return;
    }

    if (printBeforeType) (*printBeforeType)();
    TL.getType().print(Printer, Options, outermostOptions);
  }

  /// Print a TypeLoc.  If we decide to print based on the type, rather than
  /// based on the TypeRepr, call the given function before printing the type;
  /// this is useful if there are attributes in the TypeRepr which don't end
  /// up being part of the type, such as `@unchecked` in inheritance clauses.
  void printTypeLoc(const TypeLoc &TL,
                    llvm::function_ref<void()> printBeforeType) {
    printTypeLoc(TL, /*non-recursive options*/std::nullopt, printBeforeType);
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
  void printKeyPathComponents(KeyPathExpr *expr, ArrayRef<KeyPathExpr::Component> components);
  void printClosure(AbstractClosureExpr *closure, CaptureListExpr *captureList);

public:
  void printPattern(const Pattern *pattern);

  enum GenericSignatureFlags {
    PrintParams = 1,
    PrintRequirements = 2,
    InnermostOnly = 4,
    SwapSelfAndDependentMemberType = 8,
    PrintInherited = 16,
    PrintInverseRequirements = 32,
    PrintExplicitInvertibleRequirements = 64,
  };

  /// The default generic signature flags for printing requirements.
  unsigned defaultGenericRequirementFlags() const {
    return defaultGenericRequirementFlags(Options);
  }

  /// The default generic signature flags for printing requirements.
  static unsigned 
  defaultGenericRequirementFlags(const PrintOptions &options) {
    return PrintRequirements | PrintInverseRequirements;
  }

  void printInheritedFromRequirementSignature(ProtocolDecl *proto,
                                              TypeDecl *attachingTo);
  void printWhereClauseFromRequirementSignature(ProtocolDecl *proto,
                                                TypeDecl *attachingTo);
  void printInherited(const Decl *decl);

  void printGenericSignature(GenericSignature genericSig,
                             unsigned flags);
  void
  printGenericSignature(GenericSignature genericSig,
                        unsigned flags,
                        llvm::function_ref<bool(const Requirement &)> filter,
                        InverseFilter inverseFilter);
  void printSingleDepthOfGenericSignature(
      ArrayRef<GenericTypeParamType *> genericParams,
      ArrayRef<Requirement> requirements,
      ArrayRef<InverseRequirement> inverses,
      unsigned flags,
      llvm::function_ref<bool(const Requirement &)> filter);
  void printSingleDepthOfGenericSignature(
      ArrayRef<GenericTypeParamType *> genericParams,
      ArrayRef<Requirement> requirements,
      ArrayRef<InverseRequirement> inverses,
      bool &isFirstReq, unsigned flags,
      llvm::function_ref<bool(const Requirement &)> filter);
  void printRequirementSignature(ProtocolDecl *owner,
                                 RequirementSignature sig,
                                 unsigned flags,
                                 TypeDecl *attachingTo);
  void printRequirement(const Requirement &req);
  void printRequirement(const InverseRequirement &inverse,
                        bool forInherited);

private:
  bool shouldPrint(const Decl *D, bool Notify = false);
  bool shouldPrintPattern(const Pattern *P);
  void printPatternType(const Pattern *P);
  void printAccessors(const AbstractStorageDecl *ASD);
  void printSelfAccessKindModifiersIfNeeded(const FuncDecl *FD);
  void printMembersOfDecl(Decl *NTD, bool needComma = false,
                          bool openBracket = true, bool closeBracket = true,
                          bool doIndent = true);
  void printMembers(ArrayRef<Decl *> members, bool needComma = false,
                    bool openBracket = true, bool closeBracket = true,
                    bool doIndent = true);
  void printGenericDeclGenericParams(GenericContext *decl);
  void printDeclGenericRequirements(GenericContext *decl);
  void printPrimaryAssociatedTypes(ProtocolDecl *decl);
  void printBodyIfNecessary(const AbstractFunctionDecl *decl);
  void printThrownErrorIfNecessary(const AbstractFunctionDecl *decl);
  void printEnumElement(EnumElementDecl *elt);

  /// \returns true if anything was printed.
  bool printASTNodes(const ArrayRef<ASTNode> &Elements, bool NeedIndent = true);

  void printOneParameter(const ParamDecl *param, ParameterTypeFlags paramFlags,
                         bool ArgNameIsAPIByDefault, bool isEnumElement);

  void printParameterList(ParameterList *PL,
                          ArrayRef<AnyFunctionType::Param> params,
                          bool isAPINameByDefault,
                          bool isEnumElement);

  /// Print the function parameters in curried or selector style,
  /// to match the original function declaration.
  void printFunctionParameters(AbstractFunctionDecl *AFD);

  void printArgument(const Argument &arg);
  
  void printArgumentList(ArgumentList *args, bool forSubscript = false);

  void printStmtCondition(StmtCondition stmt);

#define DECL(Name,Parent) void visit##Name##Decl(Name##Decl *decl);
#define ABSTRACT_DECL(Name, Parent)
#define DECL_RANGE(Name,Start,End)
#include "swift/AST/DeclNodes.def"

#define STMT(Name, Parent) void visit##Name##Stmt(Name##Stmt *stmt);
#include "swift/AST/StmtNodes.def"

#define EXPR(Name,Parent) void visit##Name##Expr(Name##Expr *expr);
#define ABSTRACT_EXPR(Name, Parent)
#define DECL_RANGE(Name,Start,End)
#include "swift/AST/ExprNodes.def"

  void printSynthesizedExtension(Type ExtendedType, ExtensionDecl *ExtDecl);
  void printSynthesizedExtensionImpl(Type ExtendedType, ExtensionDecl *ExtDecl);

  void printExtension(ExtensionDecl* ExtDecl);
  void printExtendedTypeName(TypeLoc ExtendedTypeLoc);

public:
  PrintAST(ASTPrinter &Printer, const PrintOptions &Options)
      : Printer(Printer), Options(Options) {
    if (Options.TransformContext) {
      Type CurrentType = Options.TransformContext->getBaseType();
      if (CurrentType && CurrentType->hasArchetype()) {
        // ExistentialArchetypeTypes get replaced by a GenericTypeParamType without a
        // name in mapTypeOutOfContext. The GenericTypeParamType has no children
        // so we can't use it for TypeTransformContext.
        // To work around this, replace the ExistentialArchetypeType with the type of
        // the protocol itself.
        if (auto *Opened = CurrentType->getAs<ExistentialArchetypeType>()) {
          assert(Opened->isRoot());
          CurrentType = Opened->getExistentialType();
        }
        CurrentType = CurrentType->mapTypeOutOfContext();
      }
      setCurrentType(CurrentType);
    }
  }

  using ASTVisitor::visit;

  bool visit(Expr *E) {
    if (!Options.PrintExprs) {
      return false;
    }
    ASTVisitor::visit(E);
    return true;
  }

  bool visit(Decl *D) {
    bool Synthesize =
        Options.TransformContext &&
        Options.TransformContext->isPrintingSynthesizedExtension() &&
        isa<ExtensionDecl>(D);

    if (!shouldPrint(D, true) && !Synthesize)
      return false;

    if (isa<MacroExpansionDecl>(D))
      return true;

    Decl *Old = Current;
    Current = D;
    SWIFT_DEFER { Current = Old; };

    Type OldType = CurrentType;
    if (CurrentType && (Old != nullptr || Options.PrintAsMember)) {
      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        auto Subs = CurrentType->getContextSubstitutionMap(
          NTD->getDeclContext());
        setCurrentType(NTD->getDeclaredInterfaceType().subst(Subs));
      }
    }

    SWIFT_DEFER { setCurrentType(OldType); };

    if (Synthesize) {
      Printer.setSynthesizedTarget(Options.TransformContext->getDecl());
    }

    Printer.callPrintDeclPre(D, Options.BracketOptions);

    if (Options.PrintCompatibilityFeatureChecks) {
      printWithCompatibilityFeatureChecks(Printer, Options, D,
                                          [&] { ASTVisitor::visit(D); });
    } else {
      ASTVisitor::visit(D);
    }

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

static bool hasAsyncGetter(const AbstractStorageDecl *ASD) {
  if (auto getter = ASD->getAccessor(AccessorKind::Get)) {
    assert(!getter->getAttrs().hasAttribute<ReasyncAttr>());
    return getter->hasAsync();
  }

  return false;
}

static bool hasThrowsGetter(const AbstractStorageDecl *ASD) {
  if (auto getter = ASD->getAccessor(AccessorKind::Get)) {
    assert(!getter->getAttrs().hasAttribute<RethrowsAttr>());
    return getter->hasThrows();
  }

  return false;
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

bool canPrintSyntheticSILGenName(const Decl *D) {
  ASSERT(!D->getAttrs().hasAttribute<SILGenNameAttr>());

  // Only print on functions, and only those functions with no barriers to use.
  if (auto FD = dyn_cast<AbstractFunctionDecl>(D); !FD
        || FD->getAttrs().hasAttribute<BackDeployedAttr>()
        || FD->getOpaqueResultTypeDecl())
    return false;

  return true;
}

void PrintAST::printAttributes(const Decl *D) {
  if (Options.SkipAttributes)
    return;

  // Force semantic attrs to be computed if appropriate.
  if (shouldPrintAllSemanticDetails(Options))
    (void)D->getSemanticAttrs();

  auto attrs = D->getAttrs();

  // Save the current number of exclude attrs to restore once we're done.
  unsigned originalExcludeAttrCount = Options.ExcludeAttrList.size();

  // ExcludeCustomAttrList stores instances of attributes, which are specific
  // for each decl They cannot be shared across different decls.
  assert(Options.ExcludeCustomAttrList.empty());

  PrintOptions::OverrideScope scope(Options);
  scope.addFinalizer([originalExcludeAttrCount](PrintOptions &options) {
    options.ExcludeAttrList.resize(originalExcludeAttrCount);
    options.ExcludeCustomAttrList.clear();
  });

  // If there is an `@abi` attr, we need to print it first so that it isn't
  // affected by subsequent mutation of `Options.ExcludeAttrList`.
  if (auto abiAttr = attrs.getAttribute<ABIAttr>()) {
    if (Options.PrintImplicitAttrs && !Options.excludeAttr(abiAttr)) {
      abiAttr->print(Printer, Options, D);
      scope.Options.ExcludeAttrList.push_back(DeclAttrKind::ABI);
    }
  }

  if (Options.PrintSyntheticSILGenName
        && !D->getAttrs().hasAttribute<SILGenNameAttr>()) {
    if (canPrintSyntheticSILGenName(D)) {
      auto mangledName = Mangle::ASTMangler(D->getASTContext())
                            .mangleAnyDecl(cast<ValueDecl>(D), /*prefix=*/true,
                                           /*respectOriginallyDefinedIn=*/true);
      Printer.printAttrName("@_silgen_name");
      Printer << "(";
      Printer.printEscapedStringLiteral(mangledName);
      Printer << ")\n";
    }
    else {
      Printer.printAttrName("@available");
      Printer << "(*, unavailable, message: ";
      Printer.printEscapedStringLiteral(
          "this compiler cannot match the ABI specified by the @abi attribute");
      Printer << ")\n";
      scope.Options.ExcludeAttrList.push_back(DeclAttrKind::Available);
    }
  }

  if (Options.PrintImplicitAttrs) {

    // Don't print a redundant 'final' if we are printing a 'static' decl.
    if (D->getDeclContext()->getSelfClassDecl() &&
        getCorrectStaticSpelling(D) == StaticSpellingKind::KeywordStatic) {
      scope.Options.ExcludeAttrList.push_back(DeclAttrKind::Final);
    }

    if (auto vd = dyn_cast<VarDecl>(D)) {
      // Don't print @_hasInitialValue if we're printing an initializer
      // expression, if the storage is resilient, or if it's in an
      // @objcImplementation extension (where final properties should appear
      // computed).
      if (vd->isInitExposedToClients() || vd->isResilient() || isInObjCImpl(vd))
        scope.Options.ExcludeAttrList.push_back(DeclAttrKind::HasInitialValue);

      if (!Options.PrintForSIL) {
        // Don't print @_hasStorage if the value is simply stored, or the
        // decl is resilient or in an `@objc @implementation` extension.
        if (vd->isResilient() || isInObjCImpl(vd) ||
            (vd->getImplInfo().isSimpleStored() &&
             !hasLessAccessibleSetter(vd)))
          scope.Options.ExcludeAttrList.push_back(DeclAttrKind::HasStorage);
      }
    }

    // Add SPIs to both private and package interfaces
    if (!Options.printPublicInterface() &&
        DeclAttribute::canAttributeAppearOnDeclKind(
            DeclAttrKind::SPIAccessControl, D->getKind()) &&
        !Options.excludeAttrKind(DeclAttrKind::SPIAccessControl)) {
      interleave(D->getSPIGroups(),
             [&](Identifier spiName) {
               Printer.printAttrName("_spi", true);
               Printer << "(" << spiName << ") ";
             },
             [&] { Printer << ""; });
      scope.Options.ExcludeAttrList.push_back(DeclAttrKind::SPIAccessControl);
    }

    // Don't print any contextual decl modifiers.
    // We will handle 'mutating' and 'nonmutating' separately.
    if (isa<AccessorDecl>(D)) {
#define EXCLUDE_ATTR(Class)                                                    \
      scope.Options.ExcludeAttrList.push_back(DeclAttrKind::Class);
#define CONTEXTUAL_DECL_ATTR(X, Class, ...) EXCLUDE_ATTR(Class)
#define CONTEXTUAL_SIMPLE_DECL_ATTR(X, Class, ...) EXCLUDE_ATTR(Class)
#define CONTEXTUAL_DECL_ATTR_ALIAS(X, Class) EXCLUDE_ATTR(Class)
#include "swift/AST/DeclAttr.def"
    }

    // If the declaration is implicitly @objc, print the attribute now.
    if (auto VD = dyn_cast<ValueDecl>(D)) {
      if (VD->isObjC() && !isa<EnumElementDecl>(VD) &&
          !attrs.hasAttribute<ObjCAttr>() && ABIRoleInfo(D).providesAPI()) {
        Printer.printAttrName("@objc");
        Printer << " ";
      }
    }

    // If the declaration has designated inits that won't be visible to
    // clients, or if it inherits superclass convenience initializers,
    // then print those attributes specially.
    if (auto CD = dyn_cast<ClassDecl>(D)) {
      if (CD->inheritsSuperclassInitializers()) {
        Printer.printAttrName("@_inheritsConvenienceInitializers");
        Printer << " ";
      }
      if (CD->hasMissingDesignatedInitializers()) {
        Printer.printAttrName("@_hasMissingDesignatedInitializers");
        Printer << " ";
      }
    }
  }

  // If we are suppressing @implementation, also suppress @objc on extensions.
  if (auto ED = dyn_cast<ExtensionDecl>(D)) {
    if (ED->isObjCImplementation() &&
            Options.excludeAttrKind(DeclAttrKind::ObjCImplementation)) {
      scope.Options.ExcludeAttrList.push_back(DeclAttrKind::ObjC);
    }
  }

  // We will handle ownership specifiers separately.
  if (isa<FuncDecl>(D)) {
    scope.Options.ExcludeAttrList.push_back(DeclAttrKind::Mutating);
    scope.Options.ExcludeAttrList.push_back(DeclAttrKind::NonMutating);
    scope.Options.ExcludeAttrList.push_back(DeclAttrKind::LegacyConsuming);
    scope.Options.ExcludeAttrList.push_back(DeclAttrKind::Consuming);
    scope.Options.ExcludeAttrList.push_back(DeclAttrKind::Borrowing);
  }

  if (isa<DestructorDecl>(D) && Options.SuppressIsolatedDeinit) {
    scope.Options.ExcludeAttrList.push_back(DeclAttrKind::Nonisolated);
    scope.Options.ExcludeAttrList.push_back(DeclAttrKind::Isolated);
    if (auto globalActor = D->getGlobalActorAttr()) {
      scope.Options.ExcludeCustomAttrList.push_back(globalActor->first);
    }
  }
  
  attrs.print(Printer, Options, D);
}

void PrintAST::printTypedPattern(const TypedPattern *TP) {
  printPattern(TP->getSubPattern());
  Printer << ": ";

  PrintWithOpaqueResultTypeKeywordRAII x(Options);

  // Make sure to check if the underlying var decl is an implicitly unwrapped
  // optional.
  NonRecursivePrintOptions outermostOptions;
  if (auto *named = dyn_cast<NamedPattern>(TP->getSubPattern()))
    if (auto decl = named->getDecl())
      outermostOptions = getNonRecursiveOptions(decl);

  const auto TyLoc = TypeLoc(TP->getTypeRepr(),
                             TP->hasType() ? TP->getType() : Type());
  printTypeLoc(TyLoc, outermostOptions);
}

/// Determines if we are required to print the name of a property declaration,
/// or if we can elide it by printing a '_' instead.
static bool mustPrintPropertyName(VarDecl *decl, const PrintOptions &opts) {
  // If we're not allowed to omit the name, we must print it.
  if (!opts.OmitNameOfInaccessibleProperties) return true;

  // If it contributes to the parent's storage, we must print it because clients
  // need to be able to directly access the storage.
  // FIXME: We might be able to avoid printing names for some of these
  //        if we serialized references to them using field indices.
  if (contributesToParentTypeStorage(decl)) return true;

  // Print a package decl if in print package mode (for .package.swiftinterface),
  if (opts.printPackageInterface() && isPackage(decl))
    return true;

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
    isa->getCastType().print(Printer, Options);
    break;
  }

  case PatternKind::EnumElement: {
    auto elt = cast<EnumElementPattern>(pattern);
    Printer << "." << elt->getElementDecl()->getBaseName();
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

  case PatternKind::Expr: {
    auto expr = cast<ExprPattern>(pattern)->getSubExpr();
    visit(expr);
    break;
  }

  case PatternKind::Binding: {
    auto bPattern = cast<BindingPattern>(pattern);
    Printer.printIntroducerKeyword(
        bPattern->getIntroducerStringRef(),
        Options, " ");
    printPattern(bPattern->getSubPattern());
  }
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
  auto protoSelf = proto->getSelfInterfaceType();
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
  case RequirementKind::SameShape:
    llvm_unreachable("Same-shape requirements not supported here");
  case RequirementKind::Layout:
  case RequirementKind::Conformance:
  case RequirementKind::Superclass: {
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
                                                      TypeDecl *attachingTo) {
  unsigned flags = PrintInherited;

  // The invertible protocols themselves do not need to state inverses in their
  // inheritance clause, because they do not gain any default requirements.
  // HACK: also exclude Sendable from getting inverses printed.
  if (!proto->getInvertibleProtocolKind() &&
      !proto->isSpecificProtocol(KnownProtocolKind::Sendable))
    flags |= PrintInverseRequirements;

  printRequirementSignature(
      proto, proto->getRequirementSignature(),
      flags,
      attachingTo);
}

void PrintAST::printWhereClauseFromRequirementSignature(ProtocolDecl *proto,
                                                        TypeDecl *attachingTo) {
  unsigned flags = defaultGenericRequirementFlags();
  if (isa<AssociatedTypeDecl>(attachingTo))
    flags |= SwapSelfAndDependentMemberType;
  printRequirementSignature(proto, proto->getRequirementSignature(), flags,
                            attachingTo);
}

/// A helper function to return the depth of a requirement.
static unsigned getDepthOfRequirement(const Requirement &req) {
  switch (req.getKind()) {
  case RequirementKind::Conformance:
  case RequirementKind::Layout:
    return getDepthOfType(req.getFirstType());

  case RequirementKind::Superclass:
  case RequirementKind::SameType:
  case RequirementKind::SameShape: {
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

void PrintAST::printGenericSignature(GenericSignature genericSig,
                                     unsigned flags) {
  ASSERT(!((flags & InnermostOnly) && (flags & PrintInverseRequirements))
         && "InnermostOnly + PrintInverseRequirements is not handled");

  printGenericSignature(genericSig, flags,
                        // print everything
                        [&](const Requirement &) { return true; },
                        AllInverses());
}

InversesAtDepth::InversesAtDepth(GenericContext *level) {
  includedDepth = std::nullopt;
  // Does this generic context have its own generic parameters?
  if (auto *list = level->getGenericParams()) {
    includedDepth = list->getParams().back()->getDepth(); // use this depth.
  }
}
bool InversesAtDepth::operator()(const InverseRequirement &inverse) const {
  if (includedDepth) {
    auto d = inverse.subject->castTo<GenericTypeParamType>()->getDepth();
    return d == includedDepth.value();
  }
  return false;
}

void PrintAST::printGenericSignature(
    GenericSignature genericSig,
    unsigned flags,
    llvm::function_ref<bool(const Requirement &)> filter,
    InverseFilter inverseFilter) {

  SmallVector<Requirement, 2> requirements;
  SmallVector<InverseRequirement, 2> inverses;

  if (flags & PrintExplicitInvertibleRequirements) {
    // Collect the inverted requirements…
    SmallVector<Requirement, 2> filteredRequirements;
    genericSig->getRequirementsWithInverses(filteredRequirements, inverses);
    llvm::erase_if(inverses, [&](InverseRequirement inverse) -> bool {
      return !inverseFilter(inverse);
    });
    // …and also all of the unfiltered requirements, including the ones for
    // invertible protocols.
    requirements.append(genericSig.getRequirements().begin(),
                        genericSig.getRequirements().end());
  } else if (flags & PrintInverseRequirements) {
    genericSig->getRequirementsWithInverses(requirements, inverses);
    llvm::erase_if(inverses, [&](InverseRequirement inverse) -> bool {
      return !inverseFilter(inverse);
    });
  } else {
    requirements.append(genericSig.getRequirements().begin(),
                        genericSig.getRequirements().end());
  }

  if (flags & InnermostOnly) {
    auto genericParams = genericSig.getInnermostGenericParams();

    printSingleDepthOfGenericSignature(genericParams, requirements, inverses,
                                       flags, filter);
    return;
  }

  auto genericParams = genericSig.getGenericParams();

  if (!Options.PrintInSILBody) {
    printSingleDepthOfGenericSignature(genericParams, requirements, inverses,
                                       flags, filter);
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
      ++lastParamIdx;
    } while (lastParamIdx < numParam &&
             genericParams[lastParamIdx]->getDepth() == depth);

    // Collect requirements and generic parameters for this level.
    auto genericParamsAtDepth =
        genericParams.slice(paramIdx, lastParamIdx - paramIdx);

    SmallVector<InverseRequirement, 2> inversesAtDepth;
    for (auto inverseReq : inverses) {
      if (inverseReq.subject->castTo<GenericTypeParamType>()->getDepth() == depth)
        inversesAtDepth.push_back(inverseReq);
    }

    SmallVector<Requirement, 2> requirementsAtDepth;
    for (auto reqt : requirements) {
      unsigned currentDepth = getDepthOfRequirement(reqt);
      assert(currentDepth != ErrorDepth);
      if (currentDepth == depth)
        requirementsAtDepth.push_back(reqt);
    }

    printSingleDepthOfGenericSignature(
        genericParamsAtDepth, requirementsAtDepth, inversesAtDepth,
        flags, filter);

    paramIdx = lastParamIdx;
  }
}

void PrintAST::printSingleDepthOfGenericSignature(
    ArrayRef<GenericTypeParamType *> genericParams,
    ArrayRef<Requirement> requirements,
    ArrayRef<InverseRequirement> inverses,
    unsigned flags,
    llvm::function_ref<bool(const Requirement &)> filter) {
  bool isFirstReq = true;
  printSingleDepthOfGenericSignature(genericParams, requirements, inverses,
                                     isFirstReq, flags, filter);
}

void PrintAST::printSingleDepthOfGenericSignature(
    ArrayRef<GenericTypeParamType *> genericParams,
    ArrayRef<Requirement> requirements,
    ArrayRef<InverseRequirement> inverses,
    bool &isFirstReq, unsigned flags,
    llvm::function_ref<bool(const Requirement &)> filter) {
  bool printParams = (flags & PrintParams);
  bool printRequirements = (flags & PrintRequirements);
  printRequirements &= Options.PrintGenericRequirements;
  bool printInherited = (flags & PrintInherited);
  bool swapSelfAndDependentMemberType =
    (flags & SwapSelfAndDependentMemberType);

  unsigned typeContextDepth = 0;
  SubstitutionMap subMap;

  if (CurrentType && Current) {
    if (!CurrentType->isExistentialType()) {
      auto *DC = Current->getInnermostDeclContext()->getInnermostTypeContext();
      subMap = CurrentType->getContextSubstitutionMap(DC);
      typeContextDepth = subMap.getGenericSignature().getNextDepth();
    }
  }

  auto substParam = [&](Type param) -> Type {
    if (subMap.empty())
      return param;

    OuterSubstitutions replacer{subMap, typeContextDepth};
    return param.subst(replacer, replacer,
                       SubstFlags::PreservePackExpansionLevel);
  };

  /// Separate the explicit generic parameters from the implicit, opaque
  /// generic parameters. We only print the former.
  ArrayRef<GenericTypeParamType *> opaqueGenericParams;
  for (unsigned index : indices(genericParams)) {
    auto gpDecl = genericParams[index]->getOpaqueDecl();
    if (!gpDecl)
      continue;

    // We found the first implicit opaque type parameter. Split the
    // generic parameters array at this position.
    opaqueGenericParams = genericParams.slice(index);
    genericParams = genericParams.slice(0, index);
    break;
  }

  // Determines whether a given type is based on one of the opaque generic
  // parameters.
  auto dependsOnOpaque = [&](Type type) {
    if (opaqueGenericParams.empty())
      return false;

    if (!type->isTypeParameter())
      return false;

    auto rootGP = type->getRootGenericParam();
    for (auto opaqueGP : opaqueGenericParams) {
      if (rootGP->isEqual(opaqueGP))
        return true;
    }

    return false;
  };

  if (printParams && !genericParams.empty()) {
    // Print the generic parameters.
    Printer << "<";
    llvm::interleave(
        genericParams,
        [&](GenericTypeParamType *param) {
          if (!subMap.empty()) {
            printType(substParam(param));
          } else if (auto *GP = param->getDecl()) {
            if (param->isParameterPack())
              Printer << "each ";
            if (param->isValue())
              Printer << "let ";

            Printer.callPrintStructurePre(PrintStructureKind::GenericParameter,
                                          GP);
            Printer.printName(GP->getName(),
                              PrintNameContext::GenericParameter);

            if (param->isValue()) {
              Printer << " : ";
              printType(param->getValueType());
            }

            Printer.printStructurePost(PrintStructureKind::GenericParameter,
                                       GP);
          } else {
            if (param->isValue())
              Printer << "let ";

            printType(param);

            if (param->isValue()) {
              Printer << " : ";
              printType(param->getValueType());
            }
          }
        },
        [&] { Printer << ", "; });
  }

  if (printRequirements || printInherited) {
    for (const auto &req : requirements) {
      if (!filter(req))
        continue;

      auto first = req.getFirstType();

      if (dependsOnOpaque(first))
        continue;

      Type second;

      if (req.getKind() != RequirementKind::Layout) {
        second = req.getSecondType();
        if (dependsOnOpaque(second))
          continue;
      }

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
        case RequirementKind::SameShape:
          llvm_unreachable("Same-shape requirement not supported here");

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
        printRequirement(req);
        Printer.printStructurePost(PrintStructureKind::GenericRequirement);
      }
    }

    for (auto inverse : inverses) {
      if (dependsOnOpaque(inverse.subject))
        continue;

      if (isFirstReq) {
        if (printRequirements)
          Printer << " " << tok::kw_where << " ";
        else
          Printer << " : ";

        isFirstReq = false;
      } else {
        Printer << ", ";
      }

      printRequirement(inverse, printInherited);
    }
  }

  if (printParams && !genericParams.empty())
    Printer << ">";
}

void PrintAST::printRequirementSignature(ProtocolDecl *owner,
                                         RequirementSignature sig,
                                         unsigned flags,
                                         TypeDecl *attachingTo) {
  SmallVector<Requirement, 2> requirements;
  SmallVector<InverseRequirement, 2> inverses;

  if (flags & PrintInverseRequirements) {
    sig.getRequirementsWithInverses(owner, requirements, inverses);
  } else {
    requirements.append(sig.getRequirements().begin(),
                        sig.getRequirements().end());
  }

  if (attachingTo) {
    llvm::erase_if(requirements,
                   [&](Requirement req) {
      // Skip the inferred 'Self : AnyObject' constraint if this is an
      // @objc protocol.
      if ((req.getKind() == RequirementKind::Layout) &&
          req.getFirstType()->isEqual(owner->getSelfInterfaceType()) &&
          req.getLayoutConstraint()->getKind() ==
              LayoutConstraintKind::Class &&
          owner->isObjC()) {
        return true;
      }

      auto location = bestRequirementPrintLocation(owner, req);
      if (location.AttachedTo != attachingTo)
        return true;

      if (flags & PrintRequirements)
        return !location.InWhereClause;

      return location.InWhereClause;
    });

    auto interfaceTy =
      (isa<ProtocolDecl>(attachingTo)
       ? cast<ProtocolDecl>(attachingTo)->getSelfInterfaceType()
       : cast<AssociatedTypeDecl>(attachingTo)->getDeclaredInterfaceType());

    llvm::erase_if(inverses,
                   [&](InverseRequirement req) {
      // We print inverse requirements in the inheritance clause only.
      if (flags & PrintRequirements)
        return true;
      return !req.subject->isEqual(interfaceTy);
    });
  }

  printSingleDepthOfGenericSignature(
     owner->getGenericSignature().getGenericParams(), requirements, inverses,
     flags, [&](Requirement) { return true; });
}

void PrintAST::printRequirement(const Requirement &req) {
  SmallVector<Type, 2> rootParameterPacks;
  getTransformedType(req.getFirstType())
      ->getTypeParameterPacks(rootParameterPacks);
  if (req.getKind() != RequirementKind::Layout)
    getTransformedType(req.getSecondType())
        ->getTypeParameterPacks(rootParameterPacks);
  bool isPackRequirement = !rootParameterPacks.empty();

  switch (req.getKind()) {
  case RequirementKind::SameShape:
    Printer << "(repeat (";
    printTransformedType(req.getFirstType());
    Printer << ", ";
    printTransformedType(req.getSecondType());
    Printer << ")) : Any";
    return;
  case RequirementKind::Layout:
    if (isPackRequirement)
      Printer << "repeat ";
    printTransformedType(req.getFirstType());
    Printer << " : ";
    req.getLayoutConstraint()->print(Printer, Options);
    return;
  case RequirementKind::Conformance:
  case RequirementKind::Superclass:
    if (isPackRequirement)
      Printer << "repeat ";
    printTransformedType(req.getFirstType());
    Printer << " : ";
    break;
  case RequirementKind::SameType:
    if (isPackRequirement)
      Printer << "repeat ";
    printTransformedType(req.getFirstType());
    Printer << " == ";
    break;
  }
  printTransformedType(req.getSecondType());
}

void PrintAST::printRequirement(const InverseRequirement &inverse,
                                bool forInherited) {
  if (!forInherited) {
    Printer.callPrintStructurePre(PrintStructureKind::GenericRequirement);
    printTransformedType(inverse.subject);
    Printer << " : ";
    Printer.printStructurePost(PrintStructureKind::GenericRequirement);
  }

  Printer << "~";
  Printer << getProtocolName(getKnownProtocolKind(inverse.getKind()));
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

bool isNonSendableExtension(const Decl *D) {
  const ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D);
  if (!ED || !ED->isUnavailable())
    return false;

  auto *NTD = ED->getExtendedNominal();
  if (!NTD)
    return false;

  auto nonSendable = NTD->getAttrs().getEffectiveSendableAttr();
  if (!isa_and_nonnull<NonSendableAttr>(nonSendable))
    return false;

  // GetImplicitSendableRequest::evaluate() creates its extension with the
  // attribute's AtLoc, so this is a good way to quickly check if the extension
  // was synthesized for an '@_nonSendable' attribute.
  return ED->getLocFromSource() == nonSendable->AtLoc;
}

bool ShouldPrintChecker::shouldPrint(const Decl *D,
                                     const PrintOptions &Options) {
  if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
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

  // Optionally skip these checks for extensions synthesized for '@_nonSendable'
  if (!Options.AlwaysPrintNonSendableExtensions || !isNonSendableExtension(D)) {
    if (Options.SkipImplicit && D->isImplicit()) {
      const auto &IgnoreList = Options.TreatAsExplicitDeclList;
      if (!llvm::is_contained(IgnoreList, D))
        return false;
    }

    if (Options.SkipUnavailable && D->isUnavailable())
      return false;
  }

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

  // Skip clang decls marked with the swift_private attribute.
  if (Options.SkipSwiftPrivateClangDecls) {
    if (auto ClangD = D->getClangDecl()) {
      if (ClangD->hasAttr<clang::SwiftPrivateAttr>())
        return false;
    }
  }

  if (Options.SkipPrivateSystemDecls &&
      D->isPrivateSystemDecl(!Options.SkipUnderscoredSystemProtocols))
    return false;

  auto &ctx = D->getASTContext();
  if (Options.SkipUnsafeCXXMethods)
    if (auto func = dyn_cast<FuncDecl>(D))
      if (ctx.getClangModuleLoader()->isUnsafeCXXMethod(func))
        return false;

  if (Options.SkipEmptyExtensionDecls && isa<ExtensionDecl>(D)) {
    auto Ext = cast<ExtensionDecl>(D);
    // If the extension doesn't add protocols or has no members that we should
    // print then skip printing it.
    SmallVector<InheritedEntry, 8> ProtocolsToPrint;
    getInheritedForPrinting(Ext, Options, ProtocolsToPrint);
    if (ProtocolsToPrint.empty()) {
      bool HasMemberToPrint = false;
      for (auto Member : Ext->getAllMembers()) {
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
    for (auto idx : range(PD->getNumPatternEntries())) {
      ShouldPrint |= shouldPrint(PD->getPattern(idx), Options);
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

void PrintAST::printSelfAccessKindModifiersIfNeeded(const FuncDecl *FD) {
  if (!Options.PrintSelfAccessKindKeyword)
    return;

  const auto *AD = dyn_cast<AccessorDecl>(FD);

  switch (FD->getSelfAccessKind()) {
  case SelfAccessKind::Mutating:
    if ((!AD || AD->isAssumedNonMutating()) &&
        !Options.excludeAttrKind(DeclAttrKind::Mutating))
      Printer.printKeyword("mutating", Options, " ");
    break;
  case SelfAccessKind::NonMutating:
    if (AD && AD->isExplicitNonMutating() &&
        !Options.excludeAttrKind(DeclAttrKind::NonMutating))
      Printer.printKeyword("nonmutating", Options, " ");
    break;
  case SelfAccessKind::LegacyConsuming:
    if (!Options.excludeAttrKind(DeclAttrKind::LegacyConsuming))
      Printer.printKeyword("__consuming", Options, " ");
    break;
  case SelfAccessKind::Consuming:
    if (!Options.excludeAttrKind(DeclAttrKind::Consuming))
      Printer.printKeyword("consuming", Options, " ");
    break;
  case SelfAccessKind::Borrowing:
    if (!Options.excludeAttrKind(DeclAttrKind::Borrowing))
      Printer.printKeyword("borrowing", Options, " ");
    break;
  }
}

void PrintAST::printAccessors(const AbstractStorageDecl *ASD) {
  if (isa<VarDecl>(ASD) && !Options.PrintPropertyAccessors)
    return;
  if (isa<SubscriptDecl>(ASD) && !Options.PrintSubscriptAccessors)
    return;

  auto impl = ASD->getImplInfo();

  // AbstractAccessors is suppressed by FunctionDefinitions.
  bool PrintAbstract =
    Options.AbstractAccessors && !Options.FunctionDefinitions;

  // Don't print accessors for trivially stored properties...
  if (impl.isSimpleStored()) {
    // ...unless we're printing for SIL, which expects a { get set? } on
    //    trivial properties, or in an @objcImpl extension, which treats
    //    final stored properties as computed.
    if (Options.PrintForSIL || isInObjCImpl(ASD)) {
      Printer << " { get " << (impl.supportsMutation() ? "set }" : "}");
    }
    // ...or you're private/internal(set), at which point we'll print
    //    @_hasStorage var x: T { get }
    else if (ASD->isSettable(nullptr) && hasLessAccessibleSetter(ASD)) {
      if (PrintAbstract) {
        Printer << " { get }";
      } else {
        Printer << " {";
        {
          IndentRAII indentMore(*this);
          indent();
          Printer.printNewline();
          Printer << "get";
        }
        indent();
        Printer.printNewline();
        Printer << "}";
      }
    }
    return;
  }

  // prints with a space prefixed
  auto printWithSpace = [&](StringRef word) {
    Printer << " ";
    Printer.printKeyword(word, Options);
  };

  const bool asyncGet = hasAsyncGetter(ASD);
  const bool throwsGet = hasThrowsGetter(ASD);

  // We sometimes want to print the accessors abstractly
  // instead of listing out how they're actually implemented.
  bool inProtocol = isa<ProtocolDecl>(ASD->getDeclContext());
  if ((inProtocol && !Options.PrintAccessorBodiesInProtocols) ||
      PrintAbstract) {
    bool settable = ASD->isSettable(nullptr);
    bool mutatingGetter = hasMutatingGetter(ASD);
    bool nonmutatingSetter = hasNonMutatingSetter(ASD);

    // We're about to print something like this:
    //   { mutating? get async? throws? (nonmutating? set)? }
    // But don't print "{ get set }" if we don't have to.
    if (!inProtocol && !Options.PrintGetSetOnRWProperties &&
        settable && !mutatingGetter && !nonmutatingSetter
        && !asyncGet && !throwsGet) {
      return;
    }

    Printer << " {";
    if (mutatingGetter) printWithSpace("mutating");

    printWithSpace("get");

    if (asyncGet) printWithSpace("async");

    if (throwsGet) {
      printWithSpace("throws");
      printThrownErrorIfNecessary(ASD->getAccessor(AccessorKind::Get));
    }

    if (settable) {
      if (nonmutatingSetter) printWithSpace("nonmutating");

      printWithSpace("set");
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
      !ASD->getAccessor(AccessorKind::Set)->isExplicitNonMutating() &&
      !asyncGet && !throwsGet) {
    return;
  }

  // Otherwise, print all the concrete defining accessors.
  bool PrintAccessorBody = Options.FunctionDefinitions;

  // Determine if we should print the getter without the 'get { ... }'
  // block around it.
  bool isOnlyGetter = impl.getReadImpl() == ReadImplKind::Get &&
                      ASD->getAccessor(AccessorKind::Get);
  bool isGetterMutating = ASD->supportsMutation() || ASD->isGetterMutating();
  bool hasEffects = asyncGet || throwsGet;
  if (isOnlyGetter && !isGetterMutating && !hasEffects && PrintAccessorBody &&
      Options.FunctionBody && Options.CollapseSingleGetterProperty) {
    Options.FunctionBody(ASD->getAccessor(AccessorKind::Get), Printer);
    indent();
    return;
  }

  // Force implicit accessors to be created if they haven't been already.
  if (shouldPrintAllSemanticDetails(Options)) {
    ASD->visitEmittedAccessors([](AccessorDecl *accessor) {
      (void)accessor;
    });
  }

  // Collect the accessor declarations that we should print.
  SmallVector<AccessorDecl *, 4> accessorsToPrint;
  auto AddAccessorToPrint = [&](AccessorKind kind) {
    if (Options.SuppressCoroutineAccessors &&
        requiresFeatureCoroutineAccessors(kind))
      return;
    auto *Accessor = ASD->getAccessor(kind);
    if (!Accessor)
      return;
    if (shouldPrint(Accessor))
      accessorsToPrint.push_back(Accessor);
  };

  if (ASD->hasInitAccessor())
    AddAccessorToPrint(AccessorKind::Init);

  if (PrintAbstract) {
    AddAccessorToPrint(AccessorKind::Get);

    if (ASD->supportsMutation())
      AddAccessorToPrint(AccessorKind::Set);
  } else {
    switch (impl.getReadImpl()) {
    case ReadImplKind::Stored:
    case ReadImplKind::Inherited:
      break;
    case ReadImplKind::Get:
      AddAccessorToPrint(AccessorKind::Get);
      break;
    case ReadImplKind::Address:
      AddAccessorToPrint(AccessorKind::Address);
      break;
    case ReadImplKind::Read:
      AddAccessorToPrint(AccessorKind::Read);
      break;
    case ReadImplKind::Read2:
      if (ASD->getAccessor(AccessorKind::Read) &&
          Options.SuppressCoroutineAccessors) {
        AddAccessorToPrint(AccessorKind::Read);
      }
      AddAccessorToPrint(AccessorKind::Read2);
      break;
    }
    switch (impl.getWriteImpl()) {
    case WriteImplKind::Immutable:
      break;
    case WriteImplKind::Stored:
      llvm_unreachable("simply-stored variable should have been filtered out");
    case WriteImplKind::StoredWithObservers:
    case WriteImplKind::InheritedWithObservers: {
      AddAccessorToPrint(AccessorKind::Get);
      AddAccessorToPrint(AccessorKind::Set);
      break;
    }
    case WriteImplKind::Set:
      AddAccessorToPrint(AccessorKind::Set);
      if (!shouldHideModifyAccessor()) {
        AddAccessorToPrint(AccessorKind::Modify);
        AddAccessorToPrint(AccessorKind::Modify2);
      }
      break;
    case WriteImplKind::MutableAddress:
      AddAccessorToPrint(AccessorKind::MutableAddress);
      AddAccessorToPrint(AccessorKind::WillSet);
      AddAccessorToPrint(AccessorKind::DidSet);
      break;
    case WriteImplKind::Modify:
      AddAccessorToPrint(AccessorKind::Modify);
      break;
    case WriteImplKind::Modify2:
      if (ASD->getAccessor(AccessorKind::Modify) &&
          Options.SuppressCoroutineAccessors) {
        AddAccessorToPrint(AccessorKind::Modify);
      }
      AddAccessorToPrint(AccessorKind::Modify2);
      break;
    }
  }

  // If we're not printing the accessor bodies and none of the accessors have
  // attributes then we can print in a shorter, compact form.
  bool PrintCompactAccessors =
      !PrintAccessorBody &&
      std::all_of(accessorsToPrint.begin(), accessorsToPrint.end(),
                  [](AccessorDecl *accessor) {
                    return accessor->getAttrs().isEmpty();
                  });

  Printer << " {";

  if (!PrintCompactAccessors)
    Printer.printNewline();

  for (auto *accessor : accessorsToPrint) {
    if (PrintCompactAccessors) {
      Printer << " ";
      printSelfAccessKindModifiersIfNeeded(accessor);

      Printer.printKeyword(getAccessorLabel(accessor->getAccessorKind()),
                           Options);

      // handle any effects specifiers
      if (accessor->getAccessorKind() == AccessorKind::Get) {
        if (asyncGet)
          printWithSpace("async");
        if (throwsGet) {
          printWithSpace("throws");
          printThrownErrorIfNecessary(accessor);
        }
      }
    } else {
      {
        IndentRAII IndentMore(*this);
        indent();
        visit(accessor);
      }
      indent();
      Printer.printNewline();
    }
  }

  if (PrintCompactAccessors)
    Printer << " ";

  Printer << "}";

  indent();
}

// This provides logic for looking up all members of a namespace. This is
// intentionally implemented only in the printer and should *only* be used for
// debugging, testing, generating module dumps, etc. (In other words, if you're
// trying to get all the members of a namespace in another part of the compiler,
// you're probably doing something wrong. This is a very expensive operation,
// so we want to do it only when absolutely necessary.)
static void addNamespaceMembers(Decl *decl,
                                llvm::SmallVector<Decl *, 16> &members) {
  auto &ctx = decl->getASTContext();
  auto namespaceDecl = cast<clang::NamespaceDecl>(decl->getClangDecl());

  // This is only to keep track of the members we've already seen.
  llvm::SmallPtrSet<Decl *, 16> addedMembers;
  const auto *declOwner = namespaceDecl->getOwningModule();
  if (declOwner)
    declOwner = declOwner->getTopLevelModule();
  auto Redecls = llvm::SmallVector<clang::NamespaceDecl *, 2>(namespaceDecl->redecls());
  std::stable_sort(Redecls.begin(), Redecls.end(), [&](clang::NamespaceDecl *LHS, clang::NamespaceDecl *RHS) {
    // A namespace redeclaration will not have an owning Clang module if it is
    // declared in a bridging header.
    if (!LHS->getOwningModule() || !RHS->getOwningModule())
      return (bool)LHS->getOwningModule() < (bool)RHS->getOwningModule();
    return LHS->getOwningModule()->Name < RHS->getOwningModule()->Name;
  });
  for (auto redecl : Redecls) {
    // Skip namespace declarations that come from other top-level modules.
    if (const auto *redeclOwner = redecl->getOwningModule()) {
      if (declOwner && declOwner != redeclOwner->getTopLevelModule())
        continue;
    }
    for (auto member : redecl->decls()) {
      if (auto classTemplate = dyn_cast<clang::ClassTemplateDecl>(member)) {
        // Add all specializations to a worklist so we don't accidentally mutate
        // the list of decls we're iterating over.
        llvm::SmallPtrSet<const clang::ClassTemplateSpecializationDecl *, 16> specWorklist;
        for (auto spec : classTemplate->specializations())
          specWorklist.insert(spec);
        for (auto spec : specWorklist) {
          if (auto import =
                  ctx.getClangModuleLoader()->importDeclDirectly(spec))
            if (addedMembers.insert(import).second)
              members.push_back(import);
        }
      }

      auto lookupAndAddMembers = [&](DeclName name) {
        auto allResults = evaluateOrDefault(
            ctx.evaluator, ClangDirectLookupRequest({decl, redecl, name}), {});

        for (auto found : allResults) {
          auto clangMember = found.get<clang::NamedDecl *>();
          if (auto importedDecl =
                  ctx.getClangModuleLoader()->importDeclDirectly(clangMember)) {
            if (addedMembers.insert(importedDecl).second) {
              members.push_back(importedDecl);

              // Handle macro-expanded declarations.
              importedDecl->visitAuxiliaryDecls([&](Decl *decl) {
                auto valueDecl = dyn_cast<ValueDecl>(decl);
                if (!valueDecl)
                  return;

                // Bail out if the auxiliary decl was not produced by a macro.
                auto module = decl->getDeclContext()->getParentModule();
                auto *sf = module->getSourceFileContainingLocation(decl->getLoc());
                if (!sf || sf->Kind != SourceFileKind::MacroExpansion)
                  return;

                members.push_back(valueDecl);
              });
            }
          }
        }
      };

      auto namedDecl = dyn_cast<clang::NamedDecl>(member);
      if (!namedDecl)
        continue;
      auto name = ctx.getClangModuleLoader()->importName(namedDecl);
      if (!name)
        continue;
      lookupAndAddMembers(name);

      // Unscoped enums could have their enumerators present
      // in the parent namespace.
      if (auto *ed = dyn_cast<clang::EnumDecl>(member)) {
        if (!ed->isScoped()) {
          for (const auto *ecd : ed->enumerators()) {
            auto name = ctx.getClangModuleLoader()->importName(ecd);
            if (!name)
              continue;
            lookupAndAddMembers(name);
          }
        }
      }
    }
  }
}

void PrintAST::printMembersOfDecl(Decl *D, bool needComma, bool openBracket,
                                  bool closeBracket, bool doIndent) {
  llvm::SmallVector<Decl *, 16> Members;
  auto AddMembers = [&](IterableDeclContext *idc) {
    if (Options.PrintCurrentMembersOnly) {
      for (auto RD : idc->getMembers())
        Members.push_back(RD);
    } else {
      for (auto RD : idc->getAllMembers())
        Members.push_back(RD);
    }
  };

  if (auto Ext = dyn_cast<ExtensionDecl>(D)) {
    AddMembers(Ext);
  } else if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
    AddMembers(NTD);
    for (auto Ext : NTD->getExtensions()) {
      if (Options.printExtensionContentAsMembers(Ext))
        AddMembers(Ext);
    }
    if (Options.PrintExtensionFromConformingProtocols) {
      if (!isa<ProtocolDecl>(NTD)) {
        for (auto Conf : NTD->getAllConformances()) {
          for (auto Ext : Conf->getProtocol()->getExtensions()) {
            if (Options.printExtensionContentAsMembers(Ext))
              AddMembers(Ext);
          }
        }
      }
    }
    if (isa_and_nonnull<clang::NamespaceDecl>(D->getClangDecl()))
      addNamespaceMembers(D, Members);
  }
  printMembers(Members, needComma, openBracket, closeBracket, doIndent);
}

void PrintAST::printMembers(ArrayRef<Decl *> members, bool needComma,
                            bool openBracket, bool closeBracket,
                            bool doIndent) {
  if (openBracket) {
    Printer << " {";
    if (!Options.PrintEmptyMembersOnSameLine || !members.empty())
      Printer.printNewline();
  }
  {
    IndentRAII indentMore(*this, /*DoIndent=*/doIndent);
    for (auto i = members.begin(), iEnd = members.end(); i != iEnd; ++i) {
      auto member = *i;

      if (!shouldPrint(member, true))
        continue;

      if (!member->shouldPrintInContext(Options))
        continue;

      if (Options.EmptyLineBetweenDecls)
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
    if (auto GenericSig = decl->getGenericSignature()) {
      Printer.printStructurePre(PrintStructureKind::DeclGenericParameterClause);
      printGenericSignature(GenericSig, PrintParams | InnermostOnly);
      Printer.printStructurePost(PrintStructureKind::DeclGenericParameterClause);
    }
}

void PrintAST::printDeclGenericRequirements(GenericContext *decl) {
  const auto genericSig = decl->getGenericSignature();
  if (!genericSig)
    return;

  // If the declaration is itself non-generic, it might still
  // carry a contextual where clause.
  const auto parentSig = decl->getParent()->getGenericSignatureOfContext();
  if (parentSig && parentSig->isEqual(genericSig))
    return;

  unsigned flags = defaultGenericRequirementFlags();

  // In many cases, inverses should not be printed for outer generic parameters.
  // Exceptions to that include extensions, as it's valid to write an inverse
  // on the generic parameters they get from the extended nominal.
  InverseFilter inverseFilter = AllInverses();
  if (!isa<ExtensionDecl>(decl))
    inverseFilter = InversesAtDepth(decl);

  Printer.printStructurePre(PrintStructureKind::DeclGenericParameterClause);
  printGenericSignature(genericSig,
                        flags,
                        [parentSig](const Requirement &req) {
                          if (parentSig)
                            return !parentSig->isRequirementSatisfied(req);
                          return true;
                        },
                        inverseFilter);
  Printer.printStructurePost(PrintStructureKind::DeclGenericParameterClause);
}

void PrintAST::printInherited(const Decl *decl) {
  if (!Options.PrintInherited) {
    return;
  }
  SmallVector<InheritedEntry, 6> TypesToPrint;
  getInheritedForPrinting(decl, Options, TypesToPrint);
  if (TypesToPrint.empty())
    return;

  if (Options.PrintSpaceBeforeInheritance) {
    Printer << " ";
  }
  Printer << ": ";

  interleave(TypesToPrint, [&](InheritedEntry inherited) {
    printTypeLoc(inherited, [&] {
      if (inherited.isUnchecked())
        Printer << "@unchecked ";
      if (inherited.isRetroactive() &&
          !llvm::is_contained(Options.ExcludeAttrList, TypeAttrKind::Retroactive))
        Printer << "@retroactive ";
      if (inherited.isPreconcurrency())
        Printer << "@preconcurrency ";
      switch (inherited.getExplicitSafety()) {
      case ExplicitSafety::Unspecified:
      case ExplicitSafety::Safe:
        break;

      case ExplicitSafety::Unsafe:
        if (!llvm::is_contained(Options.ExcludeAttrList, TypeAttrKind::Unsafe))
          Printer << "@unsafe ";
        break;
      }

      if (auto globalActor = inherited.getGlobalActorIsolationType()) {
        TypeLoc globalActorTL(globalActor->getTypeRepr(),
                              globalActor->getInstanceType());
        Printer << "@";
        printTypeLoc(globalActorTL);
        Printer << " ";
      }

      if (inherited.isNonisolated())
        Printer << "nonisolated ";

      if (inherited.isSuppressed())
        Printer << "~";
    });
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
  Printer.printIntroducerKeyword("import", Options, " ");

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
  llvm::interleave(decl->getImportPath(),
                   [&](const ImportPath::Element &Elem) {
                     if (!Mods.empty()) {
                       // Should print the module real name in case module
                       // aliasing is used (see -module-alias), since that's
                       // the actual binary name.
                       Identifier Name = decl->getASTContext().getRealModuleName(Elem.Item);
                       if (Options.MapCrossImportOverlaysToDeclaringModule) {
                         if (auto *MD = Mods.front().getAsSwiftModule()) {
                           ModuleDecl *Declaring = const_cast<ModuleDecl*>(MD)
                             ->getDeclaringModuleIfCrossImportOverlay();
                           if (Declaring)
                             Name = Declaring->getRealName();
                         }
                       }
                       Printer.printModuleRef(Mods.front(), Name);
                       Mods = Mods.slice(1);
                     } else {
                       Printer << Elem.Item.str();
                     }
                   },
                   [&] { Printer << "."; });
}

void PrintAST::visitUsingDecl(UsingDecl *decl) {
  Printer.printIntroducerKeyword("using", Options, " ");
  Printer << decl->getSpecifierName();
}

void PrintAST::printExtendedTypeName(TypeLoc ExtendedTypeLoc) {
  PrintOptions::OverrideScope scope(Options);
  OVERRIDE_PRINT_OPTION(scope, FullyQualifiedTypesIfAmbiguous,
                        Options.FullyQualifiedExtendedTypesIfAmbiguous);

  // Strip off generic arguments, if any.
  auto Ty = ExtendedTypeLoc.getType()->getAnyNominal()->getDeclaredType();
  printTypeLoc(TypeLoc(ExtendedTypeLoc.getTypeRepr(), Ty));
}

void PrintAST::printSynthesizedExtension(Type ExtendedType,
                                         ExtensionDecl *ExtDecl) {
  if (Options.PrintCompatibilityFeatureChecks &&
      Options.BracketOptions.shouldOpenExtension(ExtDecl) &&
      Options.BracketOptions.shouldCloseExtension(ExtDecl)) {
    printWithCompatibilityFeatureChecks(Printer, Options, ExtDecl, [&]{
      printSynthesizedExtensionImpl(ExtendedType, ExtDecl);
    });
  } else {
    printSynthesizedExtensionImpl(ExtendedType, ExtDecl);
  }
}

void PrintAST::printSynthesizedExtensionImpl(Type ExtendedType,
                                             ExtensionDecl *ExtDecl) {
  auto printRequirementsFrom = [&](ExtensionDecl *ED, bool &IsFirst) {
    SmallVector<Requirement, 2> requirements;
    SmallVector<InverseRequirement, 2> inverses;
    auto Sig = ED->getGenericSignature();
    Sig->getRequirementsWithInverses(requirements, inverses);
    printSingleDepthOfGenericSignature(
        Sig.getGenericParams(),
        requirements,
        inverses,
        IsFirst,
        PrintAST::defaultGenericRequirementFlags(Options),
        [](const Requirement &Req){
          return true;
        });
  };

  auto printCombinedRequirementsIfNeeded = [&]() -> bool {
    if (!Options.TransformContext ||
        !Options.TransformContext->isPrintingSynthesizedExtension())
      return false;

    // Combined requirements only needed if the transform context is an enabling
    // extension of the protocol rather than a nominal (which can't have
    // constraints of its own).
    ExtensionDecl *Target = dyn_cast<ExtensionDecl>(
      Options.TransformContext->getDecl().getAsDecl());
    if (!Target || Target == ExtDecl)
      return false;

    bool IsFirst = true;
    if (ExtDecl->isConstrainedExtension()) {
      printRequirementsFrom(ExtDecl, IsFirst);
    }
    if (Target->isConstrainedExtension()) {
      if (auto *NTD = Target->getExtendedNominal()) {
        // Update the current decl and type transform for Target rather than
        // ExtDecl.
        PrintOptions::OverrideScope scope(Options);
        Options.initForSynthesizedExtensionInScope(NTD, scope);

        llvm::SaveAndRestore<Decl*> TempCurrent(Current, NTD);
        printRequirementsFrom(Target, IsFirst);
      }
    }
    return true;
  };

  if (Options.BracketOptions.shouldOpenExtension(ExtDecl)) {
    printDocumentationComment(ExtDecl);
    printAttributes(ExtDecl);
    Printer.printIntroducerKeyword("extension", Options, " ");

    printExtendedTypeName(TypeLoc::withoutLoc(ExtendedType));
    printInherited(ExtDecl);

    // We may need to combine requirements from ExtDecl (which has the members
    // to print) and the TransformContexts' decl if it is an enabling extension
    // of the base NominalDecl (which can have its own requirements) rather than
    // base NominalDecl itself (which can't). E.g:
    //
    //   protocol Foo {}
    //   extension Foo where <requirements from ExtDecl> { ... }
    //   struct Bar {}
    //   extension Bar: Foo where <requirements from TransformContext> { ... }
    //
    // should produce a synthesized extension of Bar with both sets of
    // requirements:
    //
    //   extension Bar where <requirements from ExtDecl+TransformContext> { ... }
    //
    if (!printCombinedRequirementsIfNeeded())
      printDeclGenericRequirements(ExtDecl);

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
    Printer.printIntroducerKeyword("extension", Options, " ");
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
      printExtendedTypeName(TypeLoc(decl->getExtendedTypeRepr(), extendedType));
    });
    printInherited(decl);

    if (auto genericSig = decl->getGenericSignature()) {
      auto baseGenericSig = decl->getExtendedNominal()->getGenericSignature();
      assert(baseGenericSig &&
             "an extension can't be generic if the base type isn't");

      auto genSigFlags = defaultGenericRequirementFlags();

      // Disable printing inverses if the extension is adding a conformance
      // for an invertible protocol itself, as we do not infer any requirements
      // in such an extension. We need to print the whole signature:
      //     extension S: Copyable where T: Copyable
      if (decl->isAddingConformanceToInvertible())
        genSigFlags |= PrintExplicitInvertibleRequirements;

      printGenericSignature(genericSig,
                            genSigFlags,
                            [baseGenericSig](const Requirement &req) -> bool {
        // Only include constraints that are not satisfied by the base type.
        return !baseGenericSig->isRequirementSatisfied(req);
      },
      AllInverses());
    }
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false,
                       Options.BracketOptions.shouldOpenExtension(decl),
                       Options.BracketOptions.shouldCloseExtension(decl));
  }
}

static void suppressingFeatureIsolatedAny(PrintOptions &options,
                                          llvm::function_ref<void()> action) {
  llvm::SaveAndRestore<bool> scope(options.SuppressIsolatedAny, true);
  action();
}

static void
suppressingFeatureSendingArgsAndResults(PrintOptions &options,
                                        llvm::function_ref<void()> action) {
  llvm::SaveAndRestore<bool> scope(options.SuppressSendingArgsAndResults, true);
  action();
}

static void
suppressingFeatureBitwiseCopyable2(PrintOptions &options,
                                   llvm::function_ref<void()> action) {
  llvm::SaveAndRestore<bool> scope(options.SuppressBitwiseCopyable, true);
  action();
}

static void
suppressingFeatureIsolatedDeinit(PrintOptions &options,
                                 llvm::function_ref<void()> action) {
  llvm::SaveAndRestore<bool> scope(options.SuppressIsolatedDeinit, true);
  action();
}

static void
suppressingFeatureLifetimes(PrintOptions &options,
                                     llvm::function_ref<void()> action) {
  llvm::SaveAndRestore<bool> scope(options.SuppressLifetimes, true);
  action();
}

namespace {
struct ExcludeAttrRAII {
  std::vector<AnyAttrKind> &ExcludeAttrList;
  unsigned OriginalExcludeAttrCount;
  
  ExcludeAttrRAII(std::vector<AnyAttrKind> &ExcludeAttrList,
                  DeclAttrKind excluded)
    : ExcludeAttrList(ExcludeAttrList),
      OriginalExcludeAttrCount(ExcludeAttrList.size())
  {
    ExcludeAttrList.push_back(excluded);
  }
  
  ~ExcludeAttrRAII() {
    ExcludeAttrList.resize(OriginalExcludeAttrCount);
  }
};
}

static void
suppressingFeatureMemorySafetyAttributes(PrintOptions &options,
                                       llvm::function_ref<void()> action) {
  ExcludeAttrRAII scope(options.ExcludeAttrList, DeclAttrKind::Unsafe);
  ExcludeAttrRAII scope2(options.ExcludeAttrList, DeclAttrKind::Safe);
  options.ExcludeAttrList.push_back(TypeAttrKind::Unsafe);
  SWIFT_DEFER {
    assert(options.ExcludeAttrList.back() == TypeAttrKind::Unsafe);
    options.ExcludeAttrList.pop_back();
  };

  action();
}

static void
suppressingFeatureCoroutineAccessors(PrintOptions &options,
                                     llvm::function_ref<void()> action) {
  llvm::SaveAndRestore<bool> scope(options.SuppressCoroutineAccessors, true);
  action();
}

static void
suppressingFeatureABIAttributeSE0479(PrintOptions &options,
                                     llvm::function_ref<void()> action) {
  llvm::SaveAndRestore<bool> scope1(options.PrintSyntheticSILGenName, true);
  ExcludeAttrRAII scope2(options.ExcludeAttrList, DeclAttrKind::ABI);
  action();
}

static void
suppressingFeatureAddressableTypes(PrintOptions &options,
                                   llvm::function_ref<void()> action) {
  ExcludeAttrRAII scope(options.ExcludeAttrList,
                        DeclAttrKind::AddressableForDependencies);
  action();
}

static void
suppressingFeatureNonexhaustiveAttribute(PrintOptions &options,
                                         llvm::function_ref<void()> action) {
  ExcludeAttrRAII scope(options.ExcludeAttrList, DeclAttrKind::Nonexhaustive);
  action();
}

/// Suppress the printing of a particular feature.
static void suppressingFeature(const PrintOptions &_options, Feature feature,
                               llvm::function_ref<void()> action) {
  // Just as we do in OverrideScope, we assume that PrintOptions objects are
  // always actually temporarily mutable.  We do this here to avoid needing
  // a const_cast in each of the individual suppression functions above.
  auto &options = const_cast<PrintOptions&>(_options);

  switch (feature) {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  case Feature::FeatureName:                                                   \
    llvm_unreachable("not a suppressible feature");
#define SUPPRESSIBLE_LANGUAGE_FEATURE(FeatureName, SENumber, Description)      \
  case Feature::FeatureName:                                                   \
    suppressingFeature##FeatureName(options, action);                          \
    return;
#define CONDITIONALLY_SUPPRESSIBLE_LANGUAGE_FEATURE(FeatureName, SENumber, Description)      \
  SUPPRESSIBLE_LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#include "swift/Basic/Features.def"
  }
  llvm_unreachable("exhaustive switch");
}

static void printCompatibilityCheckIf(ASTPrinter &printer, bool isElseIf,
                                      bool includeCompilerCheck,
                                      const BasicFeatureSet &features) {
  assert(!features.empty());

  printer << (isElseIf ? "#elseif " : "#if ");
  if (includeCompilerCheck)
    printer << "compiler(>=5.3) && ";

  bool first = true;
  for (auto feature : features) {
    if (!first) {
      printer << " && ";
    } else {
      first = false;
    }
    printer << "$" << Feature(feature).getName();
  }

#ifndef NDEBUG
  if (NumberSuppressionChecks) {
    static unsigned totalSuppressionChecks = 0;
    printer << " // Suppression Count: " << totalSuppressionChecks;
    ++totalSuppressionChecks;
  }
#endif

  printer.printNewline();
}

/// Generate a #if ... #elseif ... #endif chain for the given
/// suppressible feature checks.
static void printWithSuppressibleFeatureChecks(ASTPrinter &printer,
                                               const PrintOptions &options,
                                               bool firstInChain,
                                               bool includeCompilerCheck,
                            FeatureSet::SuppressibleGenerator &generator,
                            llvm::function_ref<void()> printBody) {
  // If we've run out of features to check for, enter an `#else`,
  // print the body one last time, and close the chain with `#endif`.
  // Note that, if we didn't have any suppressible features at all,
  // we shouldn't have started this recursion.
  if (generator.empty()) {
    printer << "#else";
    printer.printNewline();
    printBody();
    printer.printNewline();
    printer << "#endif";
    return;
  }

  // Otherwise, enter a `#if` or `#elseif` for the next feature.
  Feature feature = generator.next();
  printCompatibilityCheckIf(printer, /*elseif*/ !firstInChain,
                            includeCompilerCheck, {feature});

  // Print the body.
  printBody();
  printer.printNewline();

  // Start suppressing the feature and recurse to either generate
  // more `#elseif` clauses or finish off with `#endif`.
  suppressingFeature(options, feature, [&] {
    printWithSuppressibleFeatureChecks(printer, options, /*first*/ false,
                                       includeCompilerCheck, generator,
                                       printBody);
  });
}

/// Generate the appropriate #if block(s) necessary to protect the use
/// of compiler-version-dependent features in the given function.
///
/// In the most general form, with both required features and multiple
/// suppressible features in play, the generated code pattern looks like
/// the following (assuming that feature $bar implies feature $baz):
///
/// ```
///   #if compiler(>=5.3) && $foo
///   #if $bar
///   @foo @bar @baz func @test() {}
///   #elseif $baz
///   @foo @baz func @test() {}
///   #else
///   @foo func @test() {}
///   #endif
///   #endif
/// ```
void swift::printWithCompatibilityFeatureChecks(ASTPrinter &printer,
                                                const PrintOptions &options,
                                                Decl *decl,
                                 llvm::function_ref<void()> printBody) {
  // A single accessor does not get a feature check,
  // it should go around the whole decl.
  if (isa<AccessorDecl>(decl)) {
    printBody();
    return;
  }

  FeatureSet features = getUniqueFeaturesUsed(decl);
  if (features.empty()) {
    printBody();
    return;
  }

  // Enter a `#if` for the required features, if any.
  bool hasRequiredFeatures = features.hasAnyRequired();
  if (hasRequiredFeatures) {
    printCompatibilityCheckIf(printer,
                              /*elseif*/ false,
                              /*compiler check*/ true,
                              features.requiredFeatures());
  }

  // Do the recursive suppression logic if we have suppressible
  // features, or else just print the body.
  if (features.hasAnySuppressible()) {
    auto generator = features.generateSuppressibleFeatures();

    // NOTE: We emit the compiler check here as well since that also implicitly
    // ensures that we ignore parsing errors in the if block. It is harmless
    // otherwise.
    printWithSuppressibleFeatureChecks(printer, options,
                                       /*first*/ true,
                                       /*compiler check*/ true, generator,
                                       printBody);
  } else {
    printBody();
  }

  // Close the `#if` for the required features.
  if (hasRequiredFeatures) {
    printer.printNewline();
    printer << "#endif";
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
  for (auto idx : range(decl->getNumPatternEntries())) {
    decl->getPattern(idx)->forEachVariable([&](VarDecl *V) {
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

  bool printAsVar = false;
  if (anyVar) {
    printAsVar = (anyVar->isSettable(anyVar->getDeclContext()) ||
                  isInObjCImpl(anyVar));
  }

  Printer << (printAsVar ? "var " : "let ");

  bool isFirst = true;
  for (auto idx : range(decl->getNumPatternEntries())) {
    auto *pattern = decl->getPattern(idx);

    if (shouldPrintAllSemanticDetails(Options)) {
      // Force the entry to be typechecked before attempting to print.
      if (!pattern->hasType())
        (void)decl->getCheckedPatternBindingEntry(idx);

      // HACK: If the pattern type is a typealias, trigger a request that will
      // fully typecheck the init. This ensures typealiases are desguared
      // consistently.
      if (decl->isInitialized(idx)) {
        if (auto type = pattern->getType())
          if (type->getKind() == TypeKind::TypeAlias)
            (void)decl->getCheckedAndContextualizedInit(idx);
      }
    }

    if (!shouldPrintPattern(pattern))
      continue;
    if (isFirst)
      isFirst = false;
    else
      Printer << ", ";

    printPattern(pattern);

    // We also try to print type for named patterns, e.g. var Field = 10;
    // and tuple patterns, e.g. var (T1, T2) = (10, 10)
    if (isa<NamedPattern>(pattern) || isa<TuplePattern>(pattern)) {
      printPatternType(pattern);
    }

    if (Options.PrintExprs) {
      if (auto initExpr = decl->getInit(idx)) {
        Printer << " = ";
        visit(initExpr);
      }
    } else if (Options.VarInitializers) {
      auto *vd = decl->getAnchoringVarDecl(idx);
      if (decl->hasInitStringRepresentation(idx) &&
          vd->isInitExposedToClients()) {
        SmallString<128> scratch;
        Printer << " = " << decl->getInitStringRepresentation(idx, scratch);
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

void PrintAST::visitOpaqueTypeDecl(OpaqueTypeDecl *decl) {
  // TODO: If we introduce explicit opaque type decls, print them.
  assert(decl->getName().empty());
}

void PrintAST::visitTypeAliasDecl(TypeAliasDecl *decl) {
  auto name = decl->getName();
  bool suppressingBitwiseCopyable =
      Options.SuppressBitwiseCopyable &&
      decl->getModuleContext()->isStdlibModule() &&
      (decl->getNameStr() == "_BitwiseCopyable");
  if (suppressingBitwiseCopyable) {
    name = decl->getASTContext().getIdentifier("BitwiseCopyable");
  }
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);
  Printer.printIntroducerKeyword("typealias", Options, " ");
  printContextIfNeeded(decl);
  recordDeclLoc(decl,
    [&]{
      Printer.printName(name, getTypeMemberPrintNameContext(decl));
    }, [&]{ // Signature
      printGenericDeclGenericParams(decl);
    });
  if (suppressingBitwiseCopyable) {
    Printer << " = Swift._BitwiseCopyable";
    return;
  }
  bool ShouldPrint = true;
  Type Ty = decl->getUnderlyingType();

  // If the underlying type is private, don't print it.
  if (Options.SkipPrivateSystemDecls && Ty && Ty.isPrivateSystemType())
    ShouldPrint = false;

  if (ShouldPrint) {
    Printer << " = ";
    // FIXME: An inferred associated type witness type alias may reference
    // an opaque type, but OpaqueTypeArchetypes are always canonicalized
    // so lose type sugar for generic params. Bind the generic signature so
    // we can map params back into the generic signature and print them
    // correctly.
    //
    // Remove this when we have a way to represent non-canonical archetypes
    // preserving sugar.
    PrintOptions::OverrideScope scope(Options);
    OVERRIDE_PRINT_OPTION(scope, GenericSig, decl->getGenericSignature().getPointer());
    printTypeLoc(TypeLoc(decl->getUnderlyingTypeRepr(), Ty));
    printDeclGenericRequirements(decl);
  }
}

void PrintAST::visitGenericTypeParamDecl(GenericTypeParamDecl *decl) {
  recordDeclLoc(decl, [&] {
    if (decl->isParameterPack())
      Printer << "each ";
    if (decl->isValue())
      Printer << "let ";
    Printer.printName(decl->getName(), PrintNameContext::GenericParameter);
  });

  printInherited(decl);
}

void PrintAST::visitAssociatedTypeDecl(AssociatedTypeDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  Printer.printIntroducerKeyword("associatedtype", Options, " ");
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
  if (const auto *namespaceDecl =
          dyn_cast_or_null<clang::NamespaceDecl>(decl->getClangDecl())) {
    // Enum that correponds to the C++ namespace should only be printed once.
    if (!Printer.shouldPrintRedeclaredClangDecl(
            namespaceDecl->getFirstDecl()))
      return;

    if (Options.SkipInlineCXXNamespace && namespaceDecl->isInline()) {
      // Print members directly if this is an inline namespace.
      printMembersOfDecl(decl, false, /*openBracket=*/false,
                         /*closeBracket=*/false, /*doIndent=*/false);
      return;
    }
  }
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    Printer.printIntroducerKeyword("enum", Options, " ");
    printContextIfNeeded(decl);
    recordDeclLoc(
        decl,
        [&] {
          Printer.printName(decl->getName(),
                            getTypeMemberPrintNameContext(decl),
                            isSpecializedCxxDecl(decl));
        },
        [&] { // Signature
          printGenericDeclGenericParams(decl);
        });
    printInherited(decl);
    printDeclGenericRequirements(decl);
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
    Printer.printIntroducerKeyword("struct", Options, " ");
    printContextIfNeeded(decl);
    recordDeclLoc(
        decl,
        [&] {
          Printer.printName(decl->getName(),
                            getTypeMemberPrintNameContext(decl),
                            isSpecializedCxxDecl(decl));
        },
        [&] { // Signature
          printGenericDeclGenericParams(decl);
        });
    printInherited(decl);
    printDeclGenericRequirements(decl);
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
    Printer.printIntroducerKeyword(
        decl->isExplicitActor() ? "actor" : "class", Options, " ");
    printContextIfNeeded(decl);
    recordDeclLoc(
        decl,
        [&] {
          Printer.printName(decl->getName(),
                            getTypeMemberPrintNameContext(decl),
                            isSpecializedCxxDecl(decl));
        },
        [&] { // Signature
          printGenericDeclGenericParams(decl);
        });

    printInherited(decl);
    printDeclGenericRequirements(decl);
  }

  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false, true,
                       Options.BracketOptions.shouldCloseNominal(decl));
  }
}

void PrintAST::printPrimaryAssociatedTypes(ProtocolDecl *decl) {
  auto primaryAssocTypes = decl->getPrimaryAssociatedTypes();
  if (primaryAssocTypes.empty())
    return;

  Printer.printStructurePre(PrintStructureKind::DeclGenericParameterClause);

  Printer << "<";
  llvm::interleave(
      primaryAssocTypes,
      [&](AssociatedTypeDecl *assocType) {
        Printer.callPrintStructurePre(PrintStructureKind::GenericParameter,
                                      assocType);
        Printer.printTypeRef(assocType->getDeclaredInterfaceType(), assocType,
                             assocType->getName(),
                             PrintNameContext::GenericParameter);
        Printer.printStructurePost(PrintStructureKind::GenericParameter,
                                   assocType);
      },
      [&] { Printer << ", "; });
  Printer << ">";

  Printer.printStructurePost(PrintStructureKind::DeclGenericParameterClause);
}

void PrintAST::visitProtocolDecl(ProtocolDecl *decl) {
  auto name = decl->getName();
  if (Options.SuppressBitwiseCopyable &&
      decl->getModuleContext()->isStdlibModule() &&
      (decl->getNameStr() == "BitwiseCopyable")) {
    name = decl->getASTContext().getIdentifier("_BitwiseCopyable");
  }
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    Printer.printIntroducerKeyword("protocol", Options, " ");
    printContextIfNeeded(decl);
    recordDeclLoc(decl,
      [&]{
        Printer.printName(name);
      });

    printPrimaryAssociatedTypes(decl);
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

void PrintAST::visitBuiltinTupleDecl(BuiltinTupleDecl *decl) {
  llvm_unreachable("Not implemented");
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

static void printParameterFlags(ASTPrinter &printer,
                                const PrintOptions &options,
                                const ParamDecl *param,
                                ParameterTypeFlags flags,
                                bool escaping,
                                bool isIsolatedToCaller = false) {
  // Always print `nonisolated(nonsending)` specifier on a parameter
  // first, to avoid any issues with ordering.
  if (isIsolatedToCaller) {
    printer.printKeyword("nonisolated(nonsending)", options, " ");
  }

  switch (flags.getOwnershipSpecifier()) {
  case ParamSpecifier::Default:
    /*nothing*/
    break;
  case ParamSpecifier::InOut:
    printer.printKeyword("inout", options, " ");
    break;
  case ParamSpecifier::Borrowing:
    printer.printKeyword("borrowing", options, " ");
    break;
  case ParamSpecifier::Consuming:
    printer.printKeyword("consuming", options, " ");
    break;
  case ParamSpecifier::LegacyShared:
    printer.printKeyword("__shared", options, " ");
    break;
  case ParamSpecifier::LegacyOwned:
    printer.printKeyword("__owned", options, " ");
    break;
  case ParamSpecifier::ImplicitlyCopyableConsuming:
    // Nothing... we infer from sending.
    assert(flags.isSending() && "Only valid when sending is enabled");
    break;
  }

  if (flags.isSending()) {
    if (!options.SuppressSendingArgsAndResults) {
      printer.printKeyword("sending", options, " ");
    } else if (flags.getOwnershipSpecifier() ==
               ParamSpecifier::ImplicitlyCopyableConsuming) {
      // Ok. We are suppressing sending. If our ownership specifier was
      // originally implicitly copyable consuming our argument was being passed
      // at +1. By not printing sending, we would be changing the API
      // potentially to take the parameter at +0 instead of +1. To work around
      // this, print out consuming so that we preserve the +1 parameter.
      printer.printKeyword("__owned", options, " ");
    }
  }

  if (flags.isIsolated()) {
    if (!(param && param->getInterfaceType()->isOptional() &&
          options.SuppressOptionalIsolatedParams))
      printer.printKeyword("isolated", options, " ");
  }

  if (flags.isCompileTimeLiteral())
    printer.printKeyword("_const", options, " ");

  if (!options.excludeAttrKind(TypeAttrKind::Autoclosure) &&
      flags.isAutoClosure())
    printer.printAttrName("@autoclosure ");
  if (!options.excludeAttrKind(TypeAttrKind::NoDerivative) &&
      flags.isNoDerivative())
    printer.printAttrName("@noDerivative ");

  // `inout` implies `@escaping`
  if (flags.getOwnershipSpecifier() != ParamSpecifier::InOut) {
    if (!options.excludeAttrKind(TypeAttrKind::Escaping) && escaping)
      printer.printAttrName("@escaping ");
  }

  if (flags.isConstValue())
    printer.printAttrName("@const ");
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
  if (decl->isStatic() && Options.PrintStaticKeyword)
    printStaticKeyword(decl->getCorrectStaticSpelling());
  if (decl->getKind() == DeclKind::Var || Options.PrintParameterSpecifiers) {
    // If InferPropertyIntroducerFromAccessors is set, turn all read-only
    // properties to `let`.
    auto introducer = decl->getIntroducer();
    if (Options.InferPropertyIntroducerFromAccessors &&
          !cast<VarDecl>(decl)->isSettable(nullptr))
      introducer = VarDecl::Introducer::Let;

    // Map all non-let specifiers to 'var'.  This is not correct, but
    // SourceKit relies on this for info about parameter decls.
    
    bool printAsVar = (introducer != VarDecl::Introducer::Let ||
                       isInObjCImpl(decl));
    Printer.printIntroducerKeyword(printAsVar ? "var" : "let", Options, " ");
  }
  printContextIfNeeded(decl);
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName(), getTypeMemberPrintNameContext(decl));
    });

  {
    Printer.printStructurePre(PrintStructureKind::DeclResultTypeClause);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::DeclResultTypeClause);
    };

    auto type = decl->getInterfaceType();
    Printer << ": ";
    TypeLoc tyLoc;
    if (auto *repr = decl->getTypeReprOrParentPatternTypeRepr()) {
      tyLoc = TypeLoc(repr, type);
    } else {
      tyLoc = TypeLoc::withoutLoc(type);
    }
    Printer.printDeclResultTypePre(decl, tyLoc);

    PrintWithOpaqueResultTypeKeywordRAII x(Options);
    printTypeLoc(tyLoc, getNonRecursiveOptions(decl));
  }

  printAccessors(decl);
}

void PrintAST::visitParamDecl(ParamDecl *decl) {
  visitVarDecl(decl);
}

void PrintAST::printOneParameter(const ParamDecl *param,
                                 ParameterTypeFlags paramFlags,
                                 bool ArgNameIsAPIByDefault,
                                 bool isEnumElement) {
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
      if (ArgName.empty() && BodyName.empty() && !param->hasDefaultExpr()) {
        // Don't print anything, in the style of a tuple element.
        return;
      }
      // Else, print the argument only.
      LLVM_FALLTHROUGH;
    case PrintOptions::ArgAndParamPrintingMode::ArgumentOnly:
      if (ArgName.empty() && !Options.PrintEmptyArgumentNames) {
        return;
      }
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

  printAttributes(param);

  printArgName();

  auto interfaceTy = param->getInterfaceType();

  // If type of this parameter is isolated to a caller, let's
  // strip the isolation from the type to avoid printing it as
  // part of the function type because that would break ordering
  // between specifiers and attributes.
  if (param->isCallerIsolated()) {
    if (auto *funcTy = dyn_cast<AnyFunctionType>(interfaceTy.getPointer())) {
      interfaceTy =
          funcTy->withIsolation(FunctionTypeIsolation::forNonIsolated());
    }
  }

  TypeLoc TheTypeLoc;
  if (auto *repr = param->getTypeRepr()) {
    TheTypeLoc = TypeLoc(repr, interfaceTy);
  } else {
    TheTypeLoc = TypeLoc::withoutLoc(interfaceTy);
  }

  {
    Printer.printStructurePre(PrintStructureKind::FunctionParameterType);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::FunctionParameterType);
    };
    if (!param->isVariadic() &&
        !willUseTypeReprPrinting(TheTypeLoc, CurrentType, Options)) {
      auto type = TheTypeLoc.getType();

      // We suppress `@escaping` on enum element parameters because it cannot
      // be written explicitly in this position.
      printParameterFlags(Printer, Options, param, paramFlags,
                          isEscaping(type) && !isEnumElement,
                          param->isCallerIsolated());
    }

    printTypeLoc(TheTypeLoc, getNonRecursiveOptions(param));
  }

  if (param->isDefaultArgument() && Options.PrintDefaultArgumentValue) {
    auto defaultArgKind = param->getDefaultArgumentKind();
    if (param->isIsolated() &&
        defaultArgKind == DefaultArgumentKind::ExpressionMacro &&
        Options.SuppressOptionalIsolatedParams) {
      // If we're suppressing optional isolated parameters, print
      // 'nil' instead of '#isolation'
      Printer << " = nil";
      return;
    }

    Printer.callPrintStructurePre(PrintStructureKind::DefaultArgumentClause);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::DefaultArgumentClause);
    };

    SmallString<128> scratch;
    auto defaultArgStr = param->getDefaultValueStringRepresentation(scratch);

    assert(!defaultArgStr.empty() && "empty default argument?");
    Printer << " = ";

    switch (param->getDefaultArgumentKind()) {
#define MAGIC_IDENTIFIER(NAME, STRING) case DefaultArgumentKind::NAME:
#include "swift/AST/MagicIdentifierKinds.def"
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
                                  bool isAPINameByDefault,
                                  bool isEnumElement) {
  PrintOptions::OverrideScope scope(Options);
  OVERRIDE_PRINT_OPTION(scope, PrintSyntheticSILGenName, false);

  Printer.printStructurePre(PrintStructureKind::FunctionParameterList);
  SWIFT_DEFER {
    Printer.printStructurePost(PrintStructureKind::FunctionParameterList);
  };
  Printer << "(";
  const unsigned paramSize = params.size();
  for (unsigned i = 0, e = PL->size(); i != e; ++i) {
    if (i > 0)
      Printer << ", ";
    auto paramFlags = (i < paramSize)
                    ? params[i].getParameterFlags()
                    : ParameterTypeFlags();
    printOneParameter(PL->get(i), paramFlags,
                      isAPINameByDefault, isEnumElement);
  }
  Printer << ")";
}

void PrintAST::printThrownErrorIfNecessary(const AbstractFunctionDecl *AFD) {
  auto thrownType = AFD->getThrownInterfaceType();
  if (!thrownType)
    return;

  auto errorType = AFD->getASTContext().getErrorExistentialType();
  TypeRepr *thrownTypeRepr = AFD->getThrownTypeRepr();
  if (!errorType || !thrownType->isEqual(errorType) ||
      (thrownTypeRepr && Options.PreferTypeRepr)) {
    Printer << "(";
    TypeLoc thrownTyLoc(thrownTypeRepr, thrownType);
    printTypeLoc(thrownTyLoc);
    Printer << ")";
  }
}

void PrintAST::printFunctionParameters(AbstractFunctionDecl *AFD) {
  auto BodyParams = AFD->getParameters();
  auto curTy = AFD->getInterfaceType();

  // Skip over the implicit 'self'.
  if (AFD->hasImplicitSelfDecl())
    if (auto funTy = curTy->getAs<AnyFunctionType>())
      curTy = funTy->getResult();

  ArrayRef<AnyFunctionType::Param> parameterListTypes;
  if (auto funTy = curTy->getAs<AnyFunctionType>())
    parameterListTypes = funTy->getParams();

  printParameterList(BodyParams, parameterListTypes,
                     AFD->argumentNameIsAPIByDefault(),
                     /*isEnumElement*/false);

  if (AFD->hasAsync() || AFD->hasThrows()) {
    Printer.printStructurePre(PrintStructureKind::EffectsSpecifiers);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::EffectsSpecifiers);
    };
    if (AFD->hasAsync()) {
      Printer << " ";
      if (AFD->getAttrs().hasAttribute<ReasyncAttr>())
        Printer.printKeyword("reasync", Options);
      else
        Printer.printKeyword("async", Options);
    }

    if (AFD->hasThrows()) {
      if (AFD->getAttrs().hasAttribute<RethrowsAttr>())
        Printer << " " << tok::kw_rethrows;
      else {
        Printer << " " << tok::kw_throws;
        printThrownErrorIfNecessary(AFD);
      }
    }
  }
}

bool PrintAST::printASTNodes(const ArrayRef<ASTNode> &Elements,
                             bool NeedIndent) {
  IndentRAII IndentMore(*this, NeedIndent);
  bool PrintedSomething = false;

  std::function<void(Decl *)> printDecl;
  printDecl = [&](Decl *d) {
    if (d->shouldPrintInContext(Options))
      visit(d);
    d->visitAuxiliaryDecls(printDecl);
  };

  for (auto element : Elements) {
    PrintedSomething = true;
    Printer.printNewline();
    indent();
    if (auto decl = element.dyn_cast<Decl*>()) {
      printDecl(decl);
    } else if (auto stmt = element.dyn_cast<Stmt*>()) {
      visit(stmt);
    } else {
      visit(element.get<Expr*>());
    }
  }
  return PrintedSomething;
}

void PrintAST::visitAccessorDecl(AccessorDecl *decl) {
  if (Options.SuppressCoroutineAccessors &&
      requiresFeatureCoroutineAccessors(decl->getAccessorKind())) {
    return;
  }
  printDocumentationComment(decl);
  printAttributes(decl);
  // Explicitly print 'mutating' and 'nonmutating' if needed.
  printSelfAccessKindModifiersIfNeeded(decl);

  switch (decl->getAccessorKind()) {
  case AccessorKind::Get:
  case AccessorKind::DistributedGet:
  case AccessorKind::Address:
  case AccessorKind::Read:
  case AccessorKind::Read2:
  case AccessorKind::Modify:
  case AccessorKind::Modify2:
  case AccessorKind::DidSet:
  case AccessorKind::MutableAddress:
    recordDeclLoc(decl,
      [&]{
        Printer << getAccessorLabel(decl->getAccessorKind());
      });
    break;
  case AccessorKind::Set:
  case AccessorKind::WillSet:
  case AccessorKind::Init:
    recordDeclLoc(decl,
      [&]{
        Printer << getAccessorLabel(decl->getAccessorKind());

        auto params = decl->getParameters();
        if (params->size() != 0 && !params->get(0)->isImplicit()
              && Options.PrintExplicitAccessorParameters) {
          auto Name = params->get(0)->getName();
          if (!Name.empty()) {
            Printer << "(";
            Printer.printName(Name);
            Printer << ")";
          }
        }
      });
    break;
  }

  // handle effects specifiers before the body
  if (decl->hasAsync()) Printer << " async";
  if (decl->hasThrows()) {
    Printer << " throws";
    printThrownErrorIfNecessary(decl);
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
    if (decl->getResultTypeRepr()) {
      EndLoc = decl->getResultTypeSourceRange().End;
    } else {
      EndLoc = decl->getSignatureSourceRange().End;
    }
    CharSourceRange Range =
      Lexer::getCharSourceRangeFromSourceRange(Ctx.SourceMgr,
                                               SourceRange(StartLoc, EndLoc));
    printSourceRange(Range, Ctx);
  } else {
    if (decl->isStatic() && Options.PrintStaticKeyword)
      printStaticKeyword(decl->getCorrectStaticSpelling());

    printSelfAccessKindModifiersIfNeeded(decl);
    Printer.printIntroducerKeyword("func", Options, " ");

    printContextIfNeeded(decl);
    recordDeclLoc(decl,
      [&]{ // Name
        if (!decl->hasName()) {
          Printer << "<anonymous>";
        } else {
          Printer.printName(decl->getBaseIdentifier(),
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
      Printer.printStructurePre(PrintStructureKind::DeclResultTypeClause);
      SWIFT_DEFER {
        Printer.printStructurePost(PrintStructureKind::DeclResultTypeClause);
      };
      TypeLoc ResultTyLoc(decl->getResultTypeRepr(), ResultTy);

      // When printing a protocol requirement with types substituted for a
      // conforming class, replace occurrences of the 'Self' generic parameter
      // in the result type with DynamicSelfType, instead of the static
      // conforming type.
      auto *proto = dyn_cast<ProtocolDecl>(decl->getDeclContext());
      if (proto && Options.TransformContext) {
        auto BaseType = Options.TransformContext->getBaseType();
        if (BaseType->getClassOrBoundGenericClass()) {
          ResultTy = ResultTy.transformRec(
            [&](TypeBase *t) -> std::optional<Type> {
              if (isa<DependentMemberType>(t))
                return t;
              else if (t->isEqual(proto->getSelfInterfaceType()))
                return DynamicSelfType::get(t, Ctx);
              return std::nullopt;
            });
          ResultTyLoc = TypeLoc::withoutLoc(ResultTy);
        }
      }

      // FIXME: Hacky way to workaround the fact that 'Self' as return
      // TypeRepr is not getting 'typechecked'. See
      // \c resolveTopLevelIdentTypeComponent function in TypeCheckType.cpp.
      if (ResultTyLoc.getTypeRepr() &&
          ResultTyLoc.getTypeRepr()->isSimpleUnqualifiedIdentifier(
              Ctx.Id_Self)) {
        ResultTyLoc = TypeLoc::withoutLoc(ResultTy);
      }
      Printer << " -> ";

      Printer.printDeclResultTypePre(decl, ResultTyLoc);
      Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);

      if (!Options.SuppressSendingArgsAndResults) {
        if (decl->hasSendingResult()) {
          Printer << "sending ";
        } else if (auto *ft = llvm::dyn_cast_if_present<AnyFunctionType>(
                       decl->getInterfaceType())) {
          if (ft->hasExtInfo() && ft->hasSendingResult()) {
            Printer << "sending ";
          }
        }
      }

      PrintWithOpaqueResultTypeKeywordRAII x(Options);

      // Check if we would go down the type repr path... in such a case, see if
      // we can find a type repr and if that type has a sending type repr. In
      // such a case, look through the sending type repr since we handle it here
      // ourselves.
      bool usedTypeReprPrinting = false;
      NonRecursivePrintOptions nrOptions = getNonRecursiveOptions(decl);
      if (willUseTypeReprPrinting(ResultTyLoc, CurrentType, Options)) {
        if (auto repr = ResultTyLoc.getTypeRepr()) {
          // If we are printing a sending result... and we found that we have
          // to use type repr printing, look through sending type repr.
          // Sending was already applied in our caller.
          if (auto *sendingRepr = dyn_cast<SendingTypeRepr>(repr)) {
            repr = sendingRepr->getBase();
          }
          repr->print(Printer, Options, nrOptions);
          usedTypeReprPrinting = true;
        }
      }

      // If we printed using type repr printing, do not print again.
      if (!usedTypeReprPrinting) {
        printTypeLoc(ResultTyLoc, nrOptions);
      }
      Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
    }
    printDeclGenericRequirements(decl);
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
      Printer.printName(elt->getBaseIdentifier(),
                        getTypeMemberPrintNameContext(elt));
    });

  if (auto *PL = elt->getParameterList()) {
    PrintOptions::OverrideScope scope(Options);
    OVERRIDE_PRINT_OPTION(scope, ArgAndParamPrinting,
                          PrintOptions::ArgAndParamPrintingMode::EnumElement);

    auto params = ArrayRef<AnyFunctionType::Param>();
    if (!elt->isInvalid()) {
      // Walk to the params of the associated values.
      // (EnumMetaType) -> (AssocValues) -> Enum
      auto type = elt->getInterfaceType();
      params = type->castTo<AnyFunctionType>()
                   ->getResult()
                   ->castTo<AnyFunctionType>()
                   ->getParams();
    }

    printParameterList(PL, params,
                       /*isAPINameByDefault*/true,
                       /*isEnumElement*/true);
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
  if (auto *element = decl->getFirstElement()) {
    // Documentation comments over the case are attached to the enum elements.
    printDocumentationComment(element);
    printAttributes(element);
  }
  Printer.printIntroducerKeyword("case", Options, " ");

  auto elems = decl->getElements();
  llvm::interleave(elems.begin(), elems.end(),
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
  Printer.printIntroducerKeyword("case", Options, " ");
  printEnumElement(decl);
}

void PrintAST::visitSubscriptDecl(SubscriptDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);
  if (decl->isStatic() && Options.PrintStaticKeyword)
    printStaticKeyword(decl->getCorrectStaticSpelling());
  printContextIfNeeded(decl);
  recordDeclLoc(decl, [&]{
    Printer << "subscript";
  }, [&] { // Parameters
    printGenericDeclGenericParams(decl);
    auto params = ArrayRef<AnyFunctionType::Param>();
    if (!decl->isInvalid()) {
      // Walk to the params of the subscript's indices.
      auto type = decl->getInterfaceType();
      params = type->castTo<AnyFunctionType>()->getParams();
    }
    printParameterList(decl->getIndices(), params,
                       /*isAPINameByDefault*/false,
                       /*isEnumElement*/false);
  });

  {
    Printer.printStructurePre(PrintStructureKind::DeclResultTypeClause);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::DeclResultTypeClause);
    };

    Printer << " -> ";

    TypeLoc elementTy(decl->getElementTypeRepr(),
                      decl->getElementInterfaceType());
    Printer.printDeclResultTypePre(decl, elementTy);
    Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);

    if (!Options.SuppressSendingArgsAndResults) {
      if (decl->getElementTypeRepr()) {
        if (isa<SendingTypeRepr>(decl->getResultTypeRepr()))
          Printer << "sending ";
      }
    }

    PrintWithOpaqueResultTypeKeywordRAII x(Options);
    auto nrOptions = getNonRecursiveOptions(decl);
    printTypeLoc(elementTy, nrOptions);
    Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
  }

  printDeclGenericRequirements(decl);
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
    if (Options.PrintFactoryInitializerComment) {
      Printer << "/*not inherited*/ ";
    }
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

  printDeclGenericRequirements(decl);

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
  Printer.printIntroducerKeyword("operator", Options, " ");
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  if (auto *group = decl->getPrecedenceGroup())
    Printer << " : " << group->getName();
}

void PrintAST::visitPrecedenceGroupDecl(PrecedenceGroupDecl *decl) {
  Printer.printIntroducerKeyword("precedencegroup", Options, " ");
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
  Printer.printIntroducerKeyword("operator", Options, " ");
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
}

void PrintAST::visitPostfixOperatorDecl(PostfixOperatorDecl *decl) {
  Printer.printKeyword("postfix", Options, " ");
  Printer.printIntroducerKeyword("operator", Options, " ");
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
}

void PrintAST::visitModuleDecl(ModuleDecl *decl) {
  
}

void PrintAST::visitMissingDecl(MissingDecl *missing) {
  Printer << "missing_decl";
}

void PrintAST::visitMissingMemberDecl(MissingMemberDecl *decl) {
  Printer << "/* placeholder for ";
  recordDeclLoc(decl, [&]{ Printer << decl->getName(); });
  unsigned numVTableEntries = decl->getNumberOfVTableEntries();
  if (numVTableEntries > 0)
    Printer << " (vtable entries: " << numVTableEntries << ")";
  unsigned numFieldOffsetVectorEntries = decl->getNumberOfFieldOffsetVectorEntries();
  if (numFieldOffsetVectorEntries > 0)
    Printer << " (field offsets: " << numFieldOffsetVectorEntries << ")";
  Printer << " */";
}

void PrintAST::visitMacroDecl(MacroDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccess(decl);

  Printer.printIntroducerKeyword("macro", Options, " ");
  printContextIfNeeded(decl);

  recordDeclLoc(
      decl,
      [&]{
        Printer.printName(
            decl->getBaseIdentifier(),
            getTypeMemberPrintNameContext(decl));
      },
      [&] {
        printGenericDeclGenericParams(decl);
        if (decl->parameterList) {
          auto params = ArrayRef<AnyFunctionType::Param>();
          if (!decl->isInvalid()) {
            // Walk to the params of the subscript's indices.
            auto type = decl->getInterfaceType();
            params = type->castTo<AnyFunctionType>()->getParams();
          }
          printParameterList(
              decl->parameterList, params, /*isAPINameByDefault*/true,
              /*isEnumElement*/false);
        }
      }
  );

  if (decl->resultType.getTypeRepr() ||
      !decl->getResultInterfaceType()->isVoid()) {
    Printer.printStructurePre(PrintStructureKind::DeclResultTypeClause);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::DeclResultTypeClause);
    };

    Printer << " -> ";

    TypeLoc resultTypeLoc(
        decl->resultType.getTypeRepr(), decl->getResultInterfaceType());

    Printer.printDeclResultTypePre(decl, resultTypeLoc);
    Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
    printTypeLoc(resultTypeLoc);
    Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
  }

  if (Options.PrintMacroDefinitions) {
    if (decl->definition) {
      ASTContext &ctx = decl->getASTContext();
      SmallString<64> scratch;
      Printer << " = "
              << extractInlinableText(ctx, decl->definition, scratch);
    } else {
      auto def = decl->getDefinition();
      switch (def.kind) {
      case MacroDefinition::Kind::Invalid:
      case MacroDefinition::Kind::Undefined:
        // Nothing to do.
        break;

      case MacroDefinition::Kind::External: {
        auto external = def.getExternalMacro();
        Printer << " = #externalMacro(module: \"" << external.moduleName
                << "\", " << "type: \"" << external.macroTypeName << "\")";
        break;
      }

      case MacroDefinition::Kind::Builtin:
        Printer << " = Builtin.";
        switch (def.getBuiltinKind()) {
        case BuiltinMacroKind::ExternalMacro:
          Printer << "ExternalMacro";
          break;
        case BuiltinMacroKind::IsolationMacro:
          Printer << "IsolationMacro";
          break;
        }
        break;

      case MacroDefinition::Kind::Expanded:
        Printer << " = " << def.getExpanded().getExpansionText();
        break;
      }
    }
  }

  printDeclGenericRequirements(decl);
}

void PrintAST::visitMacroExpansionDecl(MacroExpansionDecl *decl) {
  Printer << '#' << decl->getMacroName();
  Printer << '(';
  auto args = decl->getArgs()->getOriginalArgs();
  bool isFirst = true;
  // FIXME: handle trailing closures.
  for (auto arg : *args) {
    if (!isFirst) {
      Printer << ", ";
    }
    printArgument(arg);
    isFirst = false;
  }
  Printer << ')';
}

void PrintAST::visitIntegerLiteralExpr(IntegerLiteralExpr *expr) {
  Printer << expr->getDigitsText();
}

void PrintAST::visitFloatLiteralExpr(FloatLiteralExpr *expr) {
  Printer << expr->getDigitsText();
}

void PrintAST::visitNilLiteralExpr(NilLiteralExpr *expr) {
  Printer << "nil";
}

void PrintAST::visitStringLiteralExpr(StringLiteralExpr *expr) {
  Printer << "\"" << expr->getValue() << "\"";
}

void PrintAST::visitBooleanLiteralExpr(BooleanLiteralExpr *expr) {
  if (expr->getValue()) {
    Printer << "true";
  } else {
    Printer << "false";
  }
}

void PrintAST::visitRegexLiteralExpr(RegexLiteralExpr *expr) {
  Printer << expr->getParsedRegexText();
}

void PrintAST::visitErrorExpr(ErrorExpr *expr) {
  Printer << "<error>";
}

void PrintAST::visitTernaryExpr(TernaryExpr *expr) {
  if (auto condExpr = expr->getCondExpr()) {
    visit(condExpr);
  }
  Printer << " ? ";
  visit(expr->getThenExpr());
  Printer << " : ";
  if (auto elseExpr = expr->getElseExpr()) {
    visit(elseExpr);
  }
}

void PrintAST::visitIsExpr(IsExpr *expr) {
  visit(expr->getSubExpr());
  Printer << " is ";
  printType(expr->getCastType());
}

void PrintAST::visitTapExpr(TapExpr *expr) {
  
}

void PrintAST::visitTryExpr(TryExpr *expr) {
  Printer << "try ";
  visit(expr->getSubExpr());
}

void PrintAST::visitCallExpr(CallExpr *expr) {
  visit(expr->getFn());
  printArgumentList(expr->getArgs()->getOriginalArgs());
}

void PrintAST::printArgument(const Argument &arg) {
  auto label = arg.getLabel();
  if (!label.empty()) {
    Printer << label.str();
    Printer << ": ";
  }
  if (arg.isInOut()) {
    Printer << "&";
  }
  visit(arg.getExpr());
}

void PrintAST::printArgumentList(ArgumentList *args, bool forSubscript) {
  Printer << (!forSubscript ? "(" : "[");
  llvm::interleave(args->begin(), args->end(), [&](Argument arg) {
    printArgument(arg);
  }, [&] {
    Printer << ", ";
  });
  Printer << (!forSubscript ? ")" : "]");
}

void PrintAST::visitLoadExpr(LoadExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitTypeExpr(TypeExpr *expr) {
  if (auto metaType = expr->getType()->castTo<AnyMetatypeType>()) {
    // Don't print `.Type` for an expr.
    printType(metaType->getInstanceType());
  } else {
    printType(expr->getType());
  }
}

void PrintAST::visitArrayExpr(ArrayExpr *expr) {
  Printer << "[";
  bool isFirst = true;
  auto elements = expr->getElements();
  for (auto element : elements) {
    if (!isFirst) {
      Printer << ", ";
    }
    visit(element);
    isFirst = false;
  }
  Printer << "]";
}

void PrintAST::visitDictionaryExpr(DictionaryExpr *expr) {
  Printer << "[";
  bool isFirst = true;
  auto elements = expr->getElements();
  for (auto element : elements) {
    auto *tupleExpr = cast<TupleExpr>(element);
    if (!isFirst) {
      Printer << ", ";
    }
    visit(tupleExpr->getElement(0));
    Printer << ": ";
    visit(tupleExpr->getElement(1));
    isFirst = false;
  }
  Printer << "]";
}

void PrintAST::visitArrowExpr(ArrowExpr *expr) {
  visit(expr->getArgsExpr());
  if (expr->getAsyncLoc().isValid()) {
    Printer << " async";
  }
  if (expr->getThrowsLoc().isValid()) {
    Printer << " throws";
  }
  Printer << " -> ";
  visit(expr->getResultExpr());
}

void PrintAST::visitAwaitExpr(AwaitExpr *expr) {
  Printer << "await ";
  visit(expr->getSubExpr());
}

void PrintAST::visitUnsafeExpr(UnsafeExpr *expr) {
  Printer << "unsafe ";
  visit(expr->getSubExpr());
}

void PrintAST::visitConsumeExpr(ConsumeExpr *expr) {
  Printer << "consume ";
  visit(expr->getSubExpr());
}

void PrintAST::visitCopyExpr(CopyExpr *expr) {
  Printer << "copy ";
  visit(expr->getSubExpr());
}

void PrintAST::visitBorrowExpr(BorrowExpr *expr) {
  Printer << "borrow ";
  visit(expr->getSubExpr());
}

void PrintAST::visitInOutExpr(InOutExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitParenExpr(ParenExpr *expr) {
  Printer << "(";
  visit(expr->getSubExpr());
  Printer << ")";
}

void PrintAST::visitTupleExpr(TupleExpr *expr) {
  Printer << "(";
  bool isFirst = true;
  auto elements = expr->getElements();
  for (auto element : elements) {
    if (!isFirst) {
      Printer << ", ";
    }
    visit(element);
    isFirst = false;
  }
  Printer << ")";
}

void PrintAST::visitTypeJoinExpr(TypeJoinExpr *expr) {
  llvm_unreachable("Not representable in source code");
}

void PrintAST::visitAssignExpr(AssignExpr *expr) {
  visit(expr->getDest());
  Printer << " = ";
  visit(expr->getSrc());
}

void PrintAST::visitBinaryExpr(BinaryExpr *expr) {
  visit(expr->getLHS());
  Printer << " ";
  if (auto operatorRef = expr->getFn()->getMemberOperatorRef()) {
    Printer << operatorRef->getDecl()->getBaseName();
  } else if (auto *operatorRef = dyn_cast<DeclRefExpr>(expr->getFn())) {
    Printer << operatorRef->getDecl()->getBaseName();
  } else if (auto *operatorRef =
                 dyn_cast<UnresolvedDeclRefExpr>(expr->getFn())) {
    // Synthesized `==` uses this.
    Printer << operatorRef->getName();
  }
  Printer << " ";
  visit(expr->getRHS());
}

void PrintAST::visitCoerceExpr(CoerceExpr *expr) {
  visit(expr->getSubExpr());
  Printer << " as ";
  printType(expr->getCastType());
}

void PrintAST::printClosure(AbstractClosureExpr *closure, CaptureListExpr *captureList) {
  
}

void PrintAST::visitClosureExpr(ClosureExpr *expr) {
  Printer << "{ ";
  if (auto parameters = expr->getParameters()) {
    Printer << "(";
    bool isFirst = true;
    for (auto &parameter: *parameters) {
      if (!isFirst) {
        Printer << ", ";
      }
      visit(parameter);
      isFirst = false;
    }
    Printer << ") ";
    if (expr->hasExplicitResultType()) {
      Printer << "-> ";
      printType(expr->getExplicitResultType());
      Printer << " ";
    }
    Printer << "in " << '\n';
  }
  auto body = expr->getBody()->getElements();
  printASTNodes(body);
  Printer << "\n}";
}

void PrintAST::visitDeclRefExpr(DeclRefExpr *expr) {
  Printer << expr->getDecl()->getBaseName();
}

void PrintAST::visitDotSelfExpr(DotSelfExpr *expr) {
  visit(expr->getSubExpr());
  Printer << ".self";
}

void PrintAST::visitErasureExpr(ErasureExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::printKeyPathComponents(KeyPathExpr *expr, ArrayRef<KeyPathExpr::Component> components) {
  using ComponentKind = KeyPathExpr::Component::Kind;
  
  if (!components.empty()) {
    for (auto &component: components) {
      auto kind = component.getKind();

      switch (kind) {
        case ComponentKind::Invalid: {
          break;
        }
        case ComponentKind::UnresolvedMember: {
          Printer << component.getUnresolvedDeclName();
          break;
        }
        case ComponentKind::UnresolvedSubscript: {
          auto args = component.getArgs();
          printArgumentList(args, /*forSubscript*/ true);
          break;
        }
        case ComponentKind::Member: {
          Printer << component.getUnresolvedDeclName();
          break;
        }
        case ComponentKind::Subscript: {
          auto args = component.getArgs();
          printArgumentList(args, /*forSubscript*/ true);
          break;
        }
        case ComponentKind::OptionalForce: {
          Printer << "!";
          break;
        }
        case ComponentKind::OptionalChain: {
          Printer << "?";
          break;
        }
        case ComponentKind::OptionalWrap: {
          break;
        }
        case ComponentKind::Identity: {
          Printer << "self";
          break;
        }
        case ComponentKind::TupleElement: {
          Printer << component.getTupleIndex();
          break;
        }
        case ComponentKind::DictionaryKey: {
          Printer << component.getUnresolvedDeclName();
          break;
        }
        case ComponentKind::CodeCompletion: {
          break;
        }
        case ComponentKind::UnresolvedApply:
        case ComponentKind::Apply: {
          auto args = component.getArgs();
          printArgumentList(args, /*forSubscript*/ false);
          break;
        }
      }
    }
  } else {
    visit(expr->getParsedPath());
  }
}

void PrintAST::visitKeyPathExpr(KeyPathExpr *expr) {
  // FIXME: The individual components are good, but printKeyPathComponents is not being called into for regular key paths, and missing the dots between components.
  if (expr->isObjC()) {
    Printer << "#keyPath(";
    printKeyPathComponents(expr, expr->getComponents());
    Printer << ")";
  } else if (auto rootType = expr->getRootType()) {
    Printer << "\\";
    printType(rootType);
  } else {
    visit(expr->getParsedRoot());
  }
}

void PrintAST::visitSingleValueStmtExpr(SingleValueStmtExpr *expr) {
  visit(expr->getStmt());
}

void PrintAST::visitForceTryExpr(ForceTryExpr *expr) {
  Printer << "try! ";
  visit(expr->getSubExpr());
}

void PrintAST::visitSequenceExpr(SequenceExpr *expr) {
  auto elements = expr->getElements();
  llvm::interleave(elements, [&](Expr* element) {
    visit(element);
  }, [&] {
    Printer << " ";
  });
}

void PrintAST::visitSuperRefExpr(SuperRefExpr *expr) {
  Printer << "super";
}

void PrintAST::visitMemberRefExpr(MemberRefExpr *expr) {
  visit(expr->getBase());
  Printer << ".";
  Printer << expr->getMember().getDecl()->getName();
}

void PrintAST::visitSubscriptExpr(SubscriptExpr *expr) {
  visit(expr->getBase());
  printArgumentList(expr->getArgs()->getOriginalArgs(), /*forSubscript*/ true);
}

void PrintAST::visitEnumIsCaseExpr(EnumIsCaseExpr *expr) {
  visit(expr->getSubExpr());
  Printer << " is ";
  printTypeLoc(expr->getCaseTypeRepr());
}

void PrintAST::visitForceValueExpr(ForceValueExpr *expr) {
  visit(expr->getSubExpr());
  Printer << "!";
}

void PrintAST::visitCurrentContextIsolationExpr(
    CurrentContextIsolationExpr *expr) {
  if (auto actor = expr->getActor())
    visit(actor);
}

void PrintAST::visitKeyPathDotExpr(KeyPathDotExpr *expr) {
  Printer << ".";
}

void PrintAST::visitAutoClosureExpr(AutoClosureExpr *expr) {
  visit(expr->getSingleExpressionBody());
}

void PrintAST::visitCaptureListExpr(CaptureListExpr *expr) {
  // FIXME: This should be moved into `printClosure` and merged with the closure.
  // Printing implementation.
  auto captureList = expr->getCaptureList();
  Printer << "[";
  auto isFirst = true;
  for (auto &par: captureList) {
    if (!isFirst) {
      Printer << ", ";
    }
    printAttributes(par.getVar());
    Printer << par.getVar()->getName();
    for (auto init: par.PBD->initializers()) {
      auto initName = init->getReferencedDecl().getDecl()->getName();
      if (initName != par.getVar()->getName())
        Printer << " = " << initName;
    }
    isFirst = false;
  }
  Printer << "]";
}

void PrintAST::visitDynamicTypeExpr(DynamicTypeExpr *expr) {
  Printer << "type(of: ";
  visit(expr->getBase());
  Printer << ")";
}

void PrintAST::visitOpaqueValueExpr(OpaqueValueExpr *expr) {
}

void PrintAST::visitOptionalTryExpr(OptionalTryExpr *expr) {
  Printer << "try? ";
  visit(expr->getSubExpr());
}

void PrintAST::visitPrefixUnaryExpr(PrefixUnaryExpr *expr) {
  visit(expr->getFn());
  visit(expr->getOperand());
}

void PrintAST::visitBindOptionalExpr(BindOptionalExpr *expr) {
  visit(expr->getSubExpr());
  Printer << "?";
}

void PrintAST::visitBridgeToObjCExpr(BridgeToObjCExpr *expr) {
}

void PrintAST::visitObjCSelectorExpr(ObjCSelectorExpr *expr) {
  Printer << "#selector";
  Printer << "(";
  visit(expr->getSubExpr());
  Printer << ")";
}

void PrintAST::visitPostfixUnaryExpr(PostfixUnaryExpr *expr) {
  visit(expr->getOperand());
  visit(expr->getFn());
}

void PrintAST::visitTupleElementExpr(TupleElementExpr *expr) {
  visit(expr->getBase());
  Printer << ".";
  Printer << expr->getFieldNumber();
}

void PrintAST::visitDerivedToBaseExpr(DerivedToBaseExpr *expr) {
}

void PrintAST::visitDotSyntaxCallExpr(DotSyntaxCallExpr *expr) {
  auto decl = expr->getFn()->getReferencedDecl().getDecl();
  if (!decl || !decl->isOperator()) {
    visit(expr->getBase());
    Printer << ".";
  }
  visit(expr->getFn());
}

void PrintAST::visitObjectLiteralExpr(ObjectLiteralExpr *expr) {
  Printer << "#";
  Printer << expr->getLiteralKindRawName();
  printArgumentList(expr->getArgs());
}

void PrintAST::visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
  visit(expr->getBase());
  Printer << ".";
  Printer << expr->getName().getBaseName();
}

void PrintAST::visitArrayToPointerExpr(ArrayToPointerExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitBridgeFromObjCExpr(BridgeFromObjCExpr *expr) {
}

void PrintAST::visitCodeCompletionExpr(CodeCompletionExpr *expr) {
}

void PrintAST::visitInOutToPointerExpr(InOutToPointerExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitLinearFunctionExpr(LinearFunctionExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitDefaultArgumentExpr(DefaultArgumentExpr *expr) {
}

void PrintAST::visitLazyInitializerExpr(LazyInitializerExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitOpenExistentialExpr(OpenExistentialExpr *expr) {
  visit(expr->getExistentialValue());
  visit(expr->getSubExpr());
}

void PrintAST::visitStringToPointerExpr(StringToPointerExpr *expr) {
}

void PrintAST::visitVarargExpansionExpr(VarargExpansionExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitPackExpansionExpr(PackExpansionExpr *expr) {
  visit(expr->getPatternExpr());
}

void PrintAST::visitMaterializePackExpr(MaterializePackExpr *expr) {
  visit(expr->getFromExpr());
}

void PrintAST::visitPackElementExpr(PackElementExpr *expr) {
  visit(expr->getPackRefExpr());
}

void PrintAST::visitArchetypeToSuperExpr(ArchetypeToSuperExpr *expr) {
}

void PrintAST::visitDestructureTupleExpr(DestructureTupleExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitDynamicMemberRefExpr(DynamicMemberRefExpr *expr) {
  visit(expr->getBase());
  Printer << ".";
  Printer << expr->getMember().getDecl()->getName();
}

void PrintAST::visitDynamicSubscriptExpr(DynamicSubscriptExpr *expr) {
  visit(expr->getBase());
  printArgumentList(expr->getArgs()->getOriginalArgs(), /*forSubscript*/ true);
}

void PrintAST::visitPointerToPointerExpr(PointerToPointerExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
  Printer << ".";
  Printer << expr->getName();
}

void PrintAST::visitDiscardAssignmentExpr(DiscardAssignmentExpr *expr) {
  Printer << "_";
}

void PrintAST::visitEditorPlaceholderExpr(EditorPlaceholderExpr *expr) {
  Printer << expr->getPlaceholder();
}

void PrintAST::visitForcedCheckedCastExpr(ForcedCheckedCastExpr *expr) {
  visit(expr->getSubExpr());
  Printer << " as! ";
  printType(expr->getCastType());
}

void PrintAST::visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr) {
  visit(expr->getSubExpr());
  Printer << " as? ";
  printType(expr->getCastType());
}

void PrintAST::visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
  if (expr->getNameLoc().isCompound()) {
    Printer << expr->getDecls().front()->getName();
  } else {
    Printer << expr->getDecls().front()->getBaseName();
  }
}

void PrintAST::visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
  Printer << expr->getName();
}

void PrintAST::visitUnresolvedPatternExpr(UnresolvedPatternExpr *expr) {
  printPattern(expr->getSubPattern());
}

void PrintAST::visitAnyHashableErasureExpr(AnyHashableErasureExpr *expr) {
}

void PrintAST::visitConstructorRefCallExpr(ConstructorRefCallExpr *expr) {
  if (auto type = expr->getType()) {
    if (auto *funcType = type->getAs<FunctionType>()) {
      printType(funcType->getResult());
    }
  }
}

void PrintAST::visitABISafeConversionExpr(ABISafeConversionExpr *expr) {
}

void PrintAST::visitFunctionConversionExpr(FunctionConversionExpr *expr) {
}

void PrintAST::visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitKeyPathApplicationExpr(KeyPathApplicationExpr *expr) {
  visit(expr->getBase());
  Printer << "[keyPath: ";
  visit(expr->getKeyPath());
  Printer << "]";
}

void PrintAST::visitMetatypeConversionExpr(MetatypeConversionExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitOptionalEvaluationExpr(OptionalEvaluationExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitUnderlyingToOpaqueExpr(UnderlyingToOpaqueExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitUnevaluatedInstanceExpr(UnevaluatedInstanceExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
  visit(expr->getLHS());
  Printer << ".";
  visit(expr->getRHS());
}

void PrintAST::visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitClassMetatypeToObjectExpr(ClassMetatypeToObjectExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitAppliedPropertyWrapperExpr(AppliedPropertyWrapperExpr *expr) {
}

void PrintAST::visitDifferentiableFunctionExpr(DifferentiableFunctionExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *expr) {
  Printer << expr->getKindString(expr->getKind());
}

void PrintAST::visitForeignObjectConversionExpr(ForeignObjectConversionExpr *expr) {
}

void PrintAST::visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *expr) {
  Printer << "init";
}

void PrintAST::visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *expr) {
  visit(expr->getSubExpr());
}

void PrintAST::visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *expr) {
  visit(expr->getOriginalExpr());
}

void PrintAST::visitProtocolMetatypeToObjectExpr(ProtocolMetatypeToObjectExpr *expr) {
}

void PrintAST::visitUnresolvedTypeConversionExpr(UnresolvedTypeConversionExpr *expr) {
}

void PrintAST::visitConditionalBridgeFromObjCExpr(ConditionalBridgeFromObjCExpr *expr) {
}

void PrintAST::visitCovariantReturnConversionExpr(CovariantReturnConversionExpr *expr) {
}

void PrintAST::visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
  visit(expr->getInterpolationExpr());
}

void PrintAST::visitCollectionUpcastConversionExpr(CollectionUpcastConversionExpr *expr) {
}

void PrintAST::visitCovariantFunctionConversionExpr(CovariantFunctionConversionExpr *expr) {
}

void PrintAST::visitExistentialMetatypeToObjectExpr(ExistentialMetatypeToObjectExpr *expr) {
}

void PrintAST::visitUnresolvedMemberChainResultExpr(swift::UnresolvedMemberChainResultExpr *expr) {
}

void PrintAST::visitLinearFunctionExtractOriginalExpr(swift::LinearFunctionExtractOriginalExpr *expr) {
}

void PrintAST::visitLinearToDifferentiableFunctionExpr(swift::LinearToDifferentiableFunctionExpr *expr) {
}

void PrintAST::visitActorIsolationErasureExpr(ActorIsolationErasureExpr *expr) {
}

void PrintAST::visitUnsafeCastExpr(UnsafeCastExpr *expr) {
}

void PrintAST::visitExtractFunctionIsolationExpr(ExtractFunctionIsolationExpr *expr) {
  visit(expr->getFunctionExpr());
  Printer << ".isolation";
}

void PrintAST::visitPropertyWrapperValuePlaceholderExpr(swift::PropertyWrapperValuePlaceholderExpr *expr) {
}

void PrintAST::visitDifferentiableFunctionExtractOriginalExpr(swift::DifferentiableFunctionExtractOriginalExpr *expr) {
}

void PrintAST::visitUnreachableExpr(UnreachableExpr *E) {
  visit(E->getSubExpr());
}

void PrintAST::visitMacroExpansionExpr(MacroExpansionExpr *expr) {
}

void PrintAST::visitTypeValueExpr(TypeValueExpr *expr) {
  expr->getType()->print(Printer, Options);
}

void PrintAST::visitBraceStmt(BraceStmt *stmt) {
  printBraceStmt(stmt);
}

void PrintAST::visitReturnStmt(ReturnStmt *stmt) {
  if (!stmt->hasResult()) {
    if (auto *FD = dyn_cast<AbstractFunctionDecl>(Current)) {
      if (auto *Body = FD->getBody()) {
        if (Body->getLastElement().dyn_cast<Stmt *>() == stmt) {
          // Don't print empty return.
          return;
        }
      }
    }
  }
  Printer << tok::kw_return;
  if (stmt->hasResult()) {
    Printer << " ";
    visit(stmt->getResult());
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

void PrintAST::visitThenStmt(ThenStmt *stmt) {
  // For now, don't print implicit 'then' statements, since they can be
  // present when the feature is disabled.
  // TODO: Once we enable the feature, we can remove this.
  if (!stmt->isImplicit())
    Printer.printKeyword("then", Options, " ");

  visit(stmt->getResult());
}

void PrintAST::visitThrowStmt(ThrowStmt *stmt) {
  Printer << tok::kw_throw << " ";
  visit(stmt->getSubExpr());
}

void PrintAST::visitDiscardStmt(DiscardStmt *stmt) {
  Printer << "discard" << " ";
  visit(stmt->getSubExpr());
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
  printStmtCondition(stmt->getCond());
  Printer << " ";
  visit(stmt->getThenStmt());
  if (auto elseStmt = stmt->getElseStmt()) {
    Printer << " " << tok::kw_else << " ";
    visit(elseStmt);
  }
}
void PrintAST::visitGuardStmt(GuardStmt *stmt) {
  Printer << tok::kw_guard << " ";
  printStmtCondition(stmt->getCond());
  Printer << " else ";
  visit(stmt->getBody());
}

void PrintAST::visitWhileStmt(WhileStmt *stmt) {
  Printer << tok::kw_while << " ";
  printStmtCondition(stmt->getCond());
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitRepeatWhileStmt(RepeatWhileStmt *stmt) {
  Printer << tok::kw_repeat << " ";
  visit(stmt->getBody());
  Printer << " " << tok::kw_while << " ";
  visit(stmt->getCond());
}

void PrintAST::printStmtCondition(StmtCondition condition) {
  interleave(
      condition,
      [&](StmtConditionElement &elt) {
        if (auto pattern = elt.getPatternOrNull()) {
          printPattern(pattern);
          auto initializer = elt.getInitializer();
          if (initializer) {
            Printer << " = ";
            visit(initializer);
          }
        } else if (auto boolean = elt.getBooleanOrNull()) {
          visit(boolean);
        }
      },
      [&] { Printer << ", "; });
}

void PrintAST::visitDoStmt(DoStmt *stmt) {
  Printer << tok::kw_do << " ";
  visit(stmt->getBody());
}

void PrintAST::visitDoCatchStmt(DoCatchStmt *stmt) {
  Printer << tok::kw_do << " ";
  visit(stmt->getBody());
  for (auto clause : stmt->getCatches()) {
    visitCaseStmt(clause);
  }
}

void PrintAST::visitForEachStmt(ForEachStmt *stmt) {
  Printer << tok::kw_for << " ";
  printPattern(stmt->getPattern());
  Printer << " " << tok::kw_in << " ";
  // FIXME: print container
  if (auto *seq = stmt->getTypeCheckedSequence()) {
    // Look through the call to '.makeIterator()'
    
    if (auto *CE = dyn_cast<CallExpr>(seq)) {
      if (auto *SAE = dyn_cast<SelfApplyExpr>(CE->getFn()))
        seq = SAE->getBase();
    }
    visit(seq);
  } else {
    visit(stmt->getParsedSequence());
  }
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
  visit(stmt->getSubjectExpr());
  Printer << " {";
  Printer.printNewline();
  for (auto *CS : stmt->getCases()) {
    visit(CS);
    Printer.printNewline();
  }
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

  if (!printASTNodes((cast<BraceStmt>(CS->getBody())->getElements())))
    Printer.printNewline();
  indent();
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

void Decl::printInherited(ASTPrinter &Printer, const PrintOptions &Opts) const {
  PrintAST printer(Printer, Opts);
  printer.printInherited(this);
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
          pbd->getPattern(0)->getSemanticsProvidingPattern();
        if (auto named = dyn_cast<NamedPattern>(pattern)) {
          if (!named->getDecl()->hasStorage())
            return false;
        }
      }
    }
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

template <typename ExtInfo>
void printCType(ASTContext &Ctx, ASTPrinter &Printer, ExtInfo &info) {
  auto *cml = Ctx.getClangModuleLoader();
  SmallString<64> buf;
  llvm::raw_svector_ostream os(buf);
  info.getClangTypeInfo().printType(cml, os);
  Printer << ", cType: " << QuotedString(os.str());
}

namespace {
class TypePrinter : public TypeVisitor<TypePrinter, void, NonRecursivePrintOptions> {
  using super = TypeVisitor;

  ASTPrinter &Printer;
  const PrintOptions &Options;

  void printGenericArgs(ArrayRef<Type> flatArgs) {
    Printer << "<";
    interleave(flatArgs,
               [&](Type arg) { visit(arg); },
               [&] { Printer << ", "; });
    Printer << ">";
  }

  void printGenericArgs(ASTContext &ctx,
                        ArrayRef<GenericTypeParamType *> params,
                        ArrayRef<Type> args) {
    printGenericArgs(PackType::getExpandedGenericArgs(params, args));
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

    bool isSimple = isSimpleUnderPrintOptions(T);
    if (isSimple) {
      visit(T);
    } else {
      Printer << "(";
      visit(T);
      Printer << ")";
    }
  }

  /// Determine whether the given type has a simple representation
  /// under the current print options.
  bool isSimpleUnderPrintOptions(Type T) {
    if (auto typealias = dyn_cast<TypeAliasType>(T.getPointer())) {
      if (shouldDesugarTypeAliasType(typealias))
        return isSimpleUnderPrintOptions(typealias->getSinglyDesugaredType());
    } else if (auto opaque =
                 dyn_cast<OpaqueTypeArchetypeType>(T.getPointer())) {
      if (opaque->isRoot()) {
        switch (Options.OpaqueReturnTypePrinting) {
        case PrintOptions::OpaqueReturnTypePrintingMode::StableReference:
        case PrintOptions::OpaqueReturnTypePrintingMode::Description:
          return false;
        case PrintOptions::OpaqueReturnTypePrintingMode::WithOpaqueKeyword:
          return opaque->getDecl()->hasExplicitGenericParams();
        case PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword:
          return opaque->getDecl()->hasExplicitGenericParams() ||
                 isSimpleUnderPrintOptions(opaque->getExistentialType()
                                               ->castTo<ExistentialType>()
                                               ->getConstraintType());
        }
        llvm_unreachable("bad opaque-return-type printing mode");
      }
    } else if (auto existential = dyn_cast<ExistentialType>(T.getPointer())) {
      if (!existential->shouldPrintWithAny())
        return isSimpleUnderPrintOptions(existential->getConstraintType());
    } else if (auto param = dyn_cast<GenericTypeParamType>(T.getPointer())) {
      if (param->isParameterPack())
        return false;
      if (param->isValue())
        return false;
    } else if (auto archetype = dyn_cast<ArchetypeType>(T.getPointer())) {
      if (isa<PackArchetypeType>(archetype))
        return false;
      if (Options.PrintForSIL && isa<LocalArchetypeType>(archetype))
        return false;
      if (archetype->getValueType())
        return false;
    }
    return T->hasSimpleTypeRepr();
  }

  /// Returns all Clang modules that are visible from `Options.CurrentModule`.
  /// This includes any modules that are imported transitively through public
  /// (`@_exported`) imports.
  ///
  /// The returned map associates each visible Clang module with the
  /// corresponding Swift module.
  const llvm::DenseMap<const clang::Module *, ModuleDecl *> &
  getVisibleClangModules() {
    return Options.CurrentModule->getVisibleClangModules(Options.InterfaceContentKind);
  }

  template <typename T>
  void printModuleContext(T *Ty) {
    FileUnit *File = cast<FileUnit>(Ty->getDecl()->getModuleScopeContext());
    ModuleDecl *Mod = File->getParentModule();
    StringRef ExportedModuleName = File->getExportedModuleName();

    // Clang declarations need special treatment: Multiple Clang modules can
    // contain the same declarations from a textually included header, but not
    // all of these modules may be visible. We therefore need to make sure we
    // choose a module that is visible from the current module. This is possible
    // only if we know what the current module is.
    const clang::Decl *ClangDecl = Ty->getDecl()->getClangDecl();
    if (ClangDecl && Options.CurrentModule) {
      for (auto *Redecl : ClangDecl->redecls()) {
        auto *owningModule = Redecl->getOwningModule();
        if (!owningModule)
          continue;
        clang::Module *ClangModule = owningModule->getTopLevelModule();
        if (!ClangModule)
          continue;

        if (ModuleDecl *VisibleModule =
                getVisibleClangModules().lookup(ClangModule)) {
          Mod = VisibleModule;
          ExportedModuleName = ClangModule->ExportAsModule;
          break;
        }
      }
    }

    if (Options.MapCrossImportOverlaysToDeclaringModule) {
      if (ModuleDecl *Declaring = Mod->getDeclaringModuleIfCrossImportOverlay())
        Mod = Declaring;
    }

    // Should use the module real (binary) name here and everywhere else the
    // module is printed in case module aliasing is used (see -module-alias)
    Identifier Name = Mod->getRealName();
    if (Options.UseExportedModuleNames && !ExportedModuleName.empty()) {
      Name = Mod->getASTContext().getIdentifier(ExportedModuleName);
    }

    StringRef PublicModuleName = File->getPublicModuleName();
    if (Options.UsePublicModuleNames && !PublicModuleName.empty()) {
      Name = Mod->getASTContext().getIdentifier(PublicModuleName);
    }

    if (Options.UseOriginallyDefinedInModuleNames) {
      Decl *D = Ty->getDecl();
      for (auto attr: D->getAttrs().getAttributes<OriginallyDefinedInAttr>()) {
        Name = Mod->getASTContext()
          .getIdentifier(const_cast<OriginallyDefinedInAttr*>(attr)
                         ->ManglingModuleName);
        break;
      }
    }

    if (Options.AliasModuleNames && Options.AliasModuleNamesTargets &&
        Options.AliasModuleNamesTargets->contains(Name.str())) {
      auto nameTwine = MODULE_DISAMBIGUATING_PREFIX + Name.str();
      Name = Mod->getASTContext().getIdentifier(nameTwine.str());
    }

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
    return M->getRealName().str().starts_with(LLDB_EXPRESSIONS_MODULE_NAME_PREFIX);
  }

  bool shouldPrintFullyQualified(TypeBase *T) {
    if (Options.FullyQualifiedTypes)
      return true;

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
    if (M->isBuiltinModule())
      return true;

    if (!Options.FullyQualifiedTypesIfAmbiguous)
      return false;

    if (Options.CurrentModule && M == Options.CurrentModule) {
      return false;
    }

    // Don't print qualifiers for types from the standard library.
    if (M->isStdlibModule() ||
        M->getRealName() == M->getASTContext().Id_ObjectiveC ||
        M->isNonUserModule() || isLLDBExpressionModule(M))
      return false;

    // Don't print qualifiers for imported types.
    if (!Options.QualifyImportedTypes)
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
      printParentType(parent);
      NameContext = PrintNameContext::TypeMember;
    } else if (shouldPrintFullyQualified(Ty)) {
      printModuleContext(Ty);
      NameContext = PrintNameContext::TypeMember;
    }

    printTypeDeclName(Ty, NameContext);
  }

  void visit(Type T, NonRecursivePrintOptions nrOptions = std::nullopt) {
    Printer.printTypePre(TypeLoc::withoutLoc(T));
    SWIFT_DEFER { Printer.printTypePost(TypeLoc::withoutLoc(T)); };

    super::visit(T, nrOptions);
  }

  void visitErrorType(ErrorType *T, NonRecursivePrintOptions nrOptions) {
    if (auto originalType = T->getOriginalType()) {
      if (Options.PrintTypesForDebugging || Options.PrintInSILBody)
        Printer << "@error_type ";
      visit(originalType);
    }
    else
      Printer << "<<error type>>";
  }

  void visitUnresolvedType(UnresolvedType *T,
                           NonRecursivePrintOptions nrOptions) {
    if (Options.PrintTypesForDebugging)
      Printer << "<<unresolvedtype>>";
    else
      Printer << "_";
  }

  void visitErrorUnionType(ErrorUnionType *T,
                           NonRecursivePrintOptions nrOptions) {
    Printer << "error_union(";
    interleave(T->getTerms(),
               [&](Type type) {
                 visit(type);
               }, [&]{
                 Printer << ", ";
               });
    Printer << ")";
  }

  void visitPlaceholderType(PlaceholderType *T,
                            NonRecursivePrintOptions nrOptions) {
    if (Options.PrintTypesForDebugging) {
      Printer << "<<placeholder for ";
      auto originator = T->getOriginator();
      if (auto *typeVar = originator.dyn_cast<TypeVariableType *>()) {
        visit(typeVar);
      } else if (auto *VD = originator.dyn_cast<VarDecl *>()) {
        Printer << "decl = ";
        Printer << VD->getName();
      } else if (originator.is<ErrorExpr *>()) {
        Printer << "error_expr";
      } else if (auto *DMT = originator.dyn_cast<DependentMemberType *>()) {
        visit(DMT);
      } else if (originator.is<TypeRepr *>()) {
        Printer << "type_repr";
      } else {
        assert(false && "unknown originator");
      }
      Printer << ">>";
    } else {
      Printer << "<<hole>>";
    }
  }

#ifdef ASTPRINTER_HANDLE_BUILTINTYPE
#error "ASTPRINTER_HANDLE_BUILTINTYPE should not be defined?!"
#endif

#define ASTPRINTER_PRINT_BUILTINTYPE(NAME)                                     \
  void visit##NAME(NAME *T, NonRecursivePrintOptions nrOptions) {              \
    SmallString<32> buffer;                                                    \
    T->getTypeName(buffer);                                                    \
    Printer << buffer;                                                         \
  }
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinRawPointerType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinRawUnsafeContinuationType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinJobType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinExecutorType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinDefaultActorStorageType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinNonDefaultDistributedActorStorageType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinPackIndexType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinNativeObjectType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinBridgeObjectType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinUnsafeValueBufferType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinIntegerLiteralType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinVectorType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinIntegerType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinFloatType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinUnboundGenericType)
  ASTPRINTER_PRINT_BUILTINTYPE(BuiltinFixedArrayType)
#undef ASTPRINTER_PRINT_BUILTINTYPE

  void visitSILTokenType(SILTokenType *T,
                         NonRecursivePrintOptions nrOptions) {
    Printer << BUILTIN_TYPE_NAME_SILTOKEN;
  }

  bool shouldDesugarTypeAliasType(TypeAliasType *T) {
    return Options.PrintForSIL || Options.PrintTypeAliasUnderlyingType;
  }

  void visitTypeAliasType(TypeAliasType *T,
                          NonRecursivePrintOptions nrOptions) {
    if (shouldDesugarTypeAliasType(T)) {
      visit(T->getSinglyDesugaredType());
      return;
    }

    printQualifiedType(T);

    auto *typeAliasDecl = T->getDecl();
    if (typeAliasDecl->isGeneric()) {
      if (Options.PrintTypesForDebugging)
        printGenericArgs(T->getDirectGenericArgs());
      else
        printGenericArgs(T->getExpandedGenericArgs());
    }
  }

  void visitLocatableType(LocatableType *T,
                          NonRecursivePrintOptions nrOptions) {
    visit(T->getSinglyDesugaredType());
  }

  void visitPackType(PackType *T, NonRecursivePrintOptions nrOptions) {
    if (Options.PrintExplicitPackTypes || Options.PrintTypesForDebugging)
      Printer << "Pack{";

    auto Fields = T->getElementTypes();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      if (i)
        Printer << ", ";
      Type EltType = Fields[i];
      visit(EltType);
    }

    if (Options.PrintExplicitPackTypes || Options.PrintTypesForDebugging)
      Printer << "}";
  }

  void visitSILPackType(SILPackType *T, NonRecursivePrintOptions nrOptions) {
    if (!T->isElementAddress())
      Printer << "@direct ";
    Printer << "Pack{";

    auto Fields = T->getElementTypes();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      if (i)
        Printer << ", ";
      Type EltType = Fields[i];
      visit(EltType);
    }
    Printer << "}";
  }

  void visitPackExpansionType(PackExpansionType *T,
                              NonRecursivePrintOptions nrOptions) {
    SmallVector<Type, 2> rootParameterPacks;
    T->getPatternType()->getTypeParameterPacks(rootParameterPacks);

    if (rootParameterPacks.empty() &&
        (T->getCountType()->isParameterPack() ||
         T->getCountType()->is<PackArchetypeType>() ||
         Options.PrintTypesForDebugging)) {
      Printer << "/* shape: ";
      visit(T->getCountType());
      Printer << " */ ";
    }

    Printer << "repeat ";

    visit(T->getPatternType());
  }

  void visitPackElementType(PackElementType *T,
                            NonRecursivePrintOptions nrOptions) {
    Printer << "/* level: " << T->getLevel() << " */ ";
    visit(T->getPackType());
  }

  void visitTupleType(TupleType *T, NonRecursivePrintOptions nrOptions) {
    Printer.callPrintStructurePre(PrintStructureKind::TupleType);
    SWIFT_DEFER { Printer.printStructurePost(PrintStructureKind::TupleType); };

    Printer << "(";

    auto Fields = T->getElements();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      if (i)
        Printer << ", ";
      const TupleTypeElt &TD = Fields[i];
      Type EltType = TD.getType();

      Printer.callPrintStructurePre(PrintStructureKind::TupleElement);
      SWIFT_DEFER {
        Printer.printStructurePost(PrintStructureKind::TupleElement);
      };

      if (TD.hasName()) {
        Printer.printName(TD.getName(), PrintNameContext::TupleElement);
        Printer << ": ";
      } else if (e == 1 && !EltType->is<PackExpansionType>()) {
        // Unlabeled one-element tuples always print the empty label to
        // distinguish them from the older syntax for parens.
        Printer << "_: ";
      }
      visit(EltType);
    }
    Printer << ")";
  }

  void visitUnboundGenericType(UnboundGenericType *T,
                               NonRecursivePrintOptions nrOptions) {
    printQualifiedType(T);
  }

  void visitBoundGenericType(BoundGenericType *T,
                             NonRecursivePrintOptions nrOptions) {
    if (Options.SynthesizeSugarOnTypes) {
      if (T->isArray()) {
        Printer << "[";
        visit(T->getGenericArgs()[0]);
        Printer << "]";
        return;
      }
      if (T->isDictionary()) {
        Printer << "[";
        visit(T->getGenericArgs()[0]);
        Printer << " : ";
        visit(T->getGenericArgs()[1]);
        Printer << "]";
        return;
      }
      if (T->isOptional()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        Printer << "?";
        return;
      }
    }
    printQualifiedType(T);

    if (Options.PrintTypesForDebugging)
      printGenericArgs(T->getGenericArgs());
    else
      printGenericArgs(T->getExpandedGenericArgs());
  }

  void printParentType(Type T) {
    /// Don't print the parent type if it's being printed in that type context.
    if (Options.TransformContext) {
       if (auto currentType = Options.TransformContext->getBaseType()) {
         auto printingType = T;
         if (currentType->hasArchetype())
           currentType = currentType->mapTypeOutOfContext();

         if (auto errorTy = printingType->getAs<ErrorType>())
           if (auto origTy = errorTy->getOriginalType())
             printingType = origTy;

         if (printingType->hasArchetype())
           printingType = printingType->mapTypeOutOfContext();

         if (currentType->isEqual(printingType))
           return;
       }
    }
    if (Options.SkipInlineCXXNamespace) {
      // Don't print the parent type if it's a reference to an inline C++
      // namespace.
      if (auto *enumTy = T->getAs<EnumType>()) {
        if (const auto *namespaceDecl = dyn_cast_or_null<clang::NamespaceDecl>(
                enumTy->getDecl()->getClangDecl())) {
          if (namespaceDecl->isInline()) {
            if (auto parent = enumTy->getParent())
              printParentType(parent);
            return;
          }
        }
      }
    }

    PrintOptions::OverrideScope scope(Options);
    OVERRIDE_PRINT_OPTION(scope, SynthesizeSugarOnTypes, false);

    if (auto sugarType = dyn_cast<SyntaxSugarType>(T.getPointer()))
      T = sugarType->getImplementationType();

    TypePrinter(Printer, Options).printWithParensIfNotSimple(T);
    Printer << ".";
  }

  void visitEnumType(EnumType *T, NonRecursivePrintOptions nrOptions) {
    printQualifiedType(T);
  }

  void visitStructType(StructType *T, NonRecursivePrintOptions nrOptions) {
    printQualifiedType(T);
  }

  void visitClassType(ClassType *T, NonRecursivePrintOptions nrOptions) {
    printQualifiedType(T);
  }

  void visitAnyMetatypeType(AnyMetatypeType *T,
                            NonRecursivePrintOptions nrOptions) {
    if (T->hasRepresentation()) {
      switch (T->getRepresentation()) {
      case MetatypeRepresentation::Thin:  Printer << "@thin ";  break;
      case MetatypeRepresentation::Thick: Printer << "@thick "; break;
      case MetatypeRepresentation::ObjC:  Printer << "@objc_metatype "; break;
      }
    }

    Type instanceType = T->getInstanceType();
    if (T->is<ExistentialMetatypeType>()) {
      Printer << "any ";

      // FIXME: We need to replace nested existential metatypes so that
      // we don't print duplicate 'any'. This will be unnecessary once
      // ExistentialMetatypeType is split into ExistentialType(MetatypeType).
      printWithParensIfNotSimple(instanceType.transformRec([](Type t) -> std::optional<Type> {
        if (auto existential = t->getAs<ExistentialMetatypeType>())
          return MetatypeType::get(existential->getInstanceType());

        return std::nullopt;
      }));
    } else {
      assert(T->is<MetatypeType>());
      if (instanceType->is<ExistentialType>()) {
        // The 'any' keyword is needed to distinguish between existential
        // metatypes and singleton metatypes. However, 'any' usually isn't
        // printed for Any and AnyObject, because it's unnecessary to write
        // 'any' with these specific constraints. Force printing with 'any'
        // for the existential instance type in this case.
        instanceType->getAs<ExistentialType>()->forcePrintWithAny([&](Type ty) {
          printWithParensIfNotSimple(ty);
        });
      } else {
        printWithParensIfNotSimple(instanceType);
      }
    }

    Printer << ".Type";
  }

  void visitModuleType(ModuleType *T, NonRecursivePrintOptions nrOptions) {
    Printer << "module<";
    // Should print the module real name in case module aliasing is
    // used (see -module-alias), since that's the actual binary name.
    Printer.printModuleRef(T->getModule(), T->getModule()->getRealName());
    Printer << ">";
  }

  void visitDynamicSelfType(DynamicSelfType *T,
                            NonRecursivePrintOptions nrOptions) {
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

  void printFunctionExtInfo(AnyFunctionType *fnType) {
    if (!fnType->hasExtInfo()) {
      Printer << "@_NO_EXTINFO ";
      return;
    }
    auto &ctx = fnType->getASTContext();
    auto info = fnType->getExtInfo();
    if (Options.SkipAttributes)
      return;

    if (!Options.excludeAttrKind(TypeAttrKind::Differentiable)) {
      switch (info.getDifferentiabilityKind()) {
      case DifferentiabilityKind::Normal:
        Printer << "@differentiable ";
        break;
      case DifferentiabilityKind::Linear:
        Printer << "@differentiable(_linear) ";
        break;
      case DifferentiabilityKind::Forward:
        Printer << "@differentiable(_forward) ";
        break;
      case DifferentiabilityKind::Reverse:
        Printer << "@differentiable(reverse) ";
        break;
      case DifferentiabilityKind::NonDifferentiable:
        break;
      }
    }

    auto isolation = info.getIsolation();
    switch (isolation.getKind()) {
    case FunctionTypeIsolation::Kind::NonIsolated:
    case FunctionTypeIsolation::Kind::Parameter:
      break;

    case FunctionTypeIsolation::Kind::GlobalActor:
      Printer << "@";
      visit(isolation.getGlobalActorType());
      Printer << " ";
      break;

    case FunctionTypeIsolation::Kind::Erased:
      if (!Options.SuppressIsolatedAny)
        Printer << "@isolated(any) ";
      break;

    case FunctionTypeIsolation::Kind::NonIsolatedCaller:
      Printer << "nonisolated(nonsending) ";
      break;
    }

    if (!Options.excludeAttrKind(TypeAttrKind::Sendable) && info.isSendable()) {
      Printer.printSimpleAttr("@Sendable") << " ";
    }

    SmallString<64> buf;
    switch (Options.PrintFunctionRepresentationAttrs) {
    case PrintOptions::FunctionRepresentationMode::None:
      return;
    case PrintOptions::FunctionRepresentationMode::Full:
    case PrintOptions::FunctionRepresentationMode::NameOnly:
      if (Options.excludeAttrKind(TypeAttrKind::Convention) ||
          info.getSILRepresentation() == SILFunctionType::Representation::Thick)
        return;

      bool printClangType = Options.PrintFunctionRepresentationAttrs ==
                            PrintOptions::FunctionRepresentationMode::Full;
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
        if (printClangType && fnType->hasNonDerivableClangType())
          printCType(ctx, Printer, info);
        break;
      case SILFunctionType::Representation::CFunctionPointer:
        Printer << "c";
        if (printClangType && fnType->hasNonDerivableClangType())
          printCType(ctx, Printer, info);
        break;
      case SILFunctionType::Representation::Method:
        Printer << "method";
        break;
      case SILFunctionType::Representation::CXXMethod:
        Printer << "cxx_method";
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
      case SILFunctionType::Representation::KeyPathAccessorGetter:
        Printer << "keypath_accessor_getter";
        break;
      case SILFunctionType::Representation::KeyPathAccessorSetter:
        Printer << "keypath_accessor_setter";
        break;
      case SILFunctionType::Representation::KeyPathAccessorEquals:
        Printer << "keypath_accessor_equals";
        break;
      case SILFunctionType::Representation::KeyPathAccessorHash:
        Printer << "keypath_accessor_hash";
        break;
      }
      Printer << ")";
      Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
      Printer << " ";
    }
  }

  void printFunctionExtInfo(SILFunctionType *fnType) {
    auto &Ctx = fnType->getASTContext();
    auto info = fnType->getExtInfo();
    auto witnessMethodConformance =
        fnType->getWitnessMethodConformanceOrInvalid();

    if (Options.SkipAttributes)
      return;

    if (!Options.excludeAttrKind(TypeAttrKind::Differentiable)) {
      switch (info.getDifferentiabilityKind()) {
      case DifferentiabilityKind::Normal:
        Printer << "@differentiable ";
        break;
      case DifferentiabilityKind::Linear:
        Printer << "@differentiable(_linear) ";
        break;
      case DifferentiabilityKind::Forward:
        Printer << "@differentiable(_forward) ";
        break;
      case DifferentiabilityKind::Reverse:
        Printer << "@differentiable(reverse) ";
        break;
      case DifferentiabilityKind::NonDifferentiable:
        break;
      }
    }

    SmallString<64> buf;
    switch (Options.PrintFunctionRepresentationAttrs) {
    case PrintOptions::FunctionRepresentationMode::None:
      break;
    case PrintOptions::FunctionRepresentationMode::NameOnly:
    case PrintOptions::FunctionRepresentationMode::Full:
      if (Options.excludeAttrKind(TypeAttrKind::Convention) ||
          info.getRepresentation() == SILFunctionType::Representation::Thick)
        break;

      bool printClangType = Options.PrintFunctionRepresentationAttrs ==
                            PrintOptions::FunctionRepresentationMode::Full;
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
        if (printClangType && fnType->hasNonDerivableClangType())
          printCType(Ctx, Printer, info);
        break;
      case SILFunctionType::Representation::CFunctionPointer:
        Printer << "c";
        if (printClangType && fnType->hasNonDerivableClangType())
          printCType(Ctx, Printer, info);
        break;
      case SILFunctionType::Representation::Method:
        Printer << "method";
        break;
      case SILFunctionType::Representation::CXXMethod:
        Printer << "cxx_method";
        break;
      case SILFunctionType::Representation::ObjCMethod:
        Printer << "objc_method";
        break;
      case SILFunctionType::Representation::WitnessMethod:
        Printer << "witness_method: ";
        printTypeDeclName(
            witnessMethodConformance.getProtocol()->getDeclaredType()
                ->castTo<ProtocolType>());
        break;
      case SILFunctionType::Representation::Closure:
        Printer << "closure";
        break;
      case SILFunctionType::Representation::KeyPathAccessorGetter:
        Printer << "keypath_accessor_getter";
        break;
      case SILFunctionType::Representation::KeyPathAccessorSetter:
        Printer << "keypath_accessor_setter";
        break;
      case SILFunctionType::Representation::KeyPathAccessorEquals:
        Printer << "keypath_accessor_equals";
        break;
      case SILFunctionType::Representation::KeyPathAccessorHash:
        Printer << "keypath_accessor_hash";
        break;
      }
      Printer << ")";
      Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
      Printer << " ";
    }

    if (info.hasErasedIsolation()) {
      Printer.callPrintStructurePre(PrintStructureKind::BuiltinAttribute);
      Printer.printAttrName("@isolated");
      Printer << "(any)";
      Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
      Printer << " ";
    }
    if (info.isUnimplementable()) {
      Printer.printSimpleAttr("@unimplementable") << " ";
    }
    if (info.isPseudogeneric()) {
      Printer.printSimpleAttr("@pseudogeneric") << " ";
    }
    if (info.isNoEscape()) {
      Printer.printSimpleAttr("@noescape") << " ";
    }
    if (info.isSendable()) {
      Printer.printSimpleAttr("@Sendable") << " ";
    }
    if (info.isAsync()) {
      Printer.printSimpleAttr("@async") << " ";
    }
  }

  void visitAnyFunctionTypeParams(
      ArrayRef<AnyFunctionType::Param> Params, bool printLabels,
      ArrayRef<LifetimeDependenceInfo> lifetimeDependencies) {
    Printer << "(";

    for (unsigned i = 0, e = Params.size(); i != e; ++i) {
      if (i)
        Printer << ", ";
      const AnyFunctionType::Param &Param = Params[i];

      Printer.callPrintStructurePre(PrintStructureKind::FunctionParameter);
      SWIFT_DEFER {
        Printer.printStructurePost(PrintStructureKind::FunctionParameter);
      };

      if ((Options.AlwaysTryPrintParameterLabels || printLabels) &&
          Param.hasLabel()) {
        // Label printing was requested and we have an external label. Print it
        // and omit the internal label.
        Printer.printName(Param.getLabel(),
                          PrintNameContext::FunctionParameterExternal);
        Printer << ": ";
      } else if (Options.AlwaysTryPrintParameterLabels &&
                 Param.hasInternalLabel() &&
                 !Param.getInternalLabel().hasDollarPrefix()) {
        // We didn't have an external parameter label but were requested to
        // always try and print parameter labels.
        // If the internal label is a valid internal parameter label (does not
        // start with '$'), print the internal label. If we have neither an
        // external nor a printable internal label, only print the type.
        Printer << "_ ";
        Printer.printName(Param.getInternalLabel(),
                          PrintNameContext::FunctionParameterLocal);
        Printer << ": ";
      }

      Printer.printLifetimeDependenceAt(lifetimeDependencies, i);

      auto type = Param.getPlainType();
      if (Param.isVariadic()) {
        visit(type);
        Printer << "...";
      } else {
        printParameterFlags(Printer, Options, nullptr, Param.getParameterFlags(),
                            isEscaping(type));
        visit(type);
      }
    }

    Printer << ")";
  }

  void visitFunctionType(FunctionType *T,
                         NonRecursivePrintOptions nrOptions) {
    Printer.callPrintStructurePre(PrintStructureKind::FunctionType);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::FunctionType);
    };

    printFunctionExtInfo(T);

    // If we're stripping argument labels from types, do it when printing.
    visitAnyFunctionTypeParams(T->getParams(), /*printLabels*/ false,
                               T->getLifetimeDependencies());

    if (T->hasExtInfo()) {
      if (T->isAsync()) {
        Printer << " ";
        Printer.printKeyword("async", Options);
      }

      if (T->isThrowing()) {
        Printer << " " << tok::kw_throws;

        if (auto thrownError = T->getThrownError()) {
          Printer << "(";
          thrownError->print(Printer, Options);
          Printer << ")";
        }
      }
    }

    Printer << " -> ";

    if (!Options.SuppressSendingArgsAndResults && T->hasExtInfo() &&
        T->hasSendingResult()) {
      Printer.printKeyword("sending ", Options);
    }

    Printer.printLifetimeDependence(T->getLifetimeDependenceForResult());

    Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
    T->getResult().print(Printer, Options);
    Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
  }

  void printGenericSignature(GenericSignature genericSig, unsigned flags) {
    PrintAST(Printer, Options).printGenericSignature(genericSig, flags);
  }

  void printSubstitutions(SubstitutionMap subs) {
    Printer << " <";
    interleave(subs.getReplacementTypes(),
               [&](Type type) {
                 visit(type);
               }, [&]{
                 Printer << ", ";
               });
    Printer << ">";
  }

  void visitGenericFunctionType(GenericFunctionType *T,
                                NonRecursivePrintOptions nrOptions) {
    Printer.callPrintStructurePre(PrintStructureKind::FunctionType);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::FunctionType);
    };

    printFunctionExtInfo(T);
    printGenericSignature(T->getGenericSignature(),
                          PrintAST::PrintParams |
                          PrintAST::defaultGenericRequirementFlags(Options));
    Printer << " ";

    visitAnyFunctionTypeParams(T->getParams(), /*printLabels*/ true,
                               T->getLifetimeDependencies());

    if (T->hasExtInfo()) {
      if (T->isAsync()) {
        Printer << " ";
        Printer.printKeyword("async", Options);
      }

      if (T->isThrowing()) {
        Printer << " " << tok::kw_throws;

        if (auto thrownError = T->getThrownError()) {
          Printer << "(";
          thrownError->print(Printer, Options);
          Printer << ")";
        }
      }
   }

    Printer << " -> ";

    Printer.printLifetimeDependenceAt(T->getLifetimeDependencies(),
                                      T->getParams().size());

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
    case SILCoroutineKind::YieldOnce2:
      Printer << "@yield_once_2 ";
      return;
    case SILCoroutineKind::YieldMany:
      Printer << "@yield_many ";
      return;
    }
    llvm_unreachable("bad convention");
  }

  void printSILAsyncAttr(bool isAsync) {
    if (isAsync) {
      Printer << "@async ";
    }
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
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_InoutAliasable:
    case ParameterConvention::Indirect_In_CXX:
    case ParameterConvention::Indirect_In_Guaranteed:
      llvm_unreachable("callee convention cannot be indirect");
    case ParameterConvention::Pack_Guaranteed:
    case ParameterConvention::Pack_Owned:
    case ParameterConvention::Pack_Inout:
      llvm_unreachable("callee convention cannot be a pack");
    }
    llvm_unreachable("bad convention");
  }

  void visitSILFunctionType(SILFunctionType *T,
                            NonRecursivePrintOptions nrOptions) {
    printSILCoroutineKind(T->getCoroutineKind());
    printFunctionExtInfo(T);
    printCalleeConvention(T->getCalleeConvention());

    if (GenericSignature sig = T->getInvocationGenericSignature()) {
      printGenericSignature(sig,
                            PrintAST::PrintParams |
                            PrintAST::defaultGenericRequirementFlags(Options));
      Printer << " ";
    }

    // If this is a substituted function type, then its generic signature is
    // independent of the enclosing context, and defines the parameters active
    // in the interface params and results. Unsubstituted types use the existing
    // environment, which may be a sil decl's generic environment.
    //
    // Yeah, this is fiddly. In the end, we probably want all decls to have
    // substituted types in terms of a generic signature declared on the decl,
    // which would make this logic more uniform.

    {
      PrintOptions::OverrideScope scope(Options);

      if (auto substitutions = T->getPatternSubstitutions()) {
        OVERRIDE_PRINT_OPTION(scope, GenericSig, nullptr);

        GenericSignature sig = substitutions.getGenericSignature();

        // The substituted signature is printed without inverse requirement
        // desugaring, but also we drop conformances to Copyable and Escapable
        // when constructing it.
        Printer << "@substituted ";
        printGenericSignature(sig,
                              PrintAST::PrintParams |
                              PrintAST::PrintRequirements);
        Printer << " ";
      }

      Printer << "(";
      bool first = true;
      unsigned paramIndex = 0;
      for (auto param : T->getParameters()) {
        Printer.printSeparator(first, ", ");
        param.print(Printer, Options,
                    T->getLifetimeDependenceFor(paramIndex));
        paramIndex++;
      }
      Printer << ") -> ";

      Printer.printLifetimeDependence(T->getLifetimeDependenceForResult());

      bool parenthesizeResults = mustParenthesizeResults(T);
      if (parenthesizeResults)
        Printer << "(";

      first = true;

      for (auto yield : T->getYields()) {
        Printer.printSeparator(first, ", ");
        Printer << "@yields ";
        // TODO: Fix lifetime dependence printing here
        yield.print(Printer, Options);
      }

      for (auto result : T->getResults()) {
        Printer.printSeparator(first, ", ");
        result.print(Printer, Options);
      }

      if (T->hasErrorResult()) {
        Printer.printSeparator(first, ", ");
        if (T->getErrorResult().getConvention() == ResultConvention::Owned)
          Printer << "@error ";
        else if (T->getErrorResult().getConvention() == ResultConvention::Indirect)
          Printer << "@error_indirect ";
        else if (T->getErrorResult().getConvention() == ResultConvention::Unowned)
          Printer << "@error_unowned ";
        else {
          assert(false && "Should have error, error_indirect, or error_unowned");
        }
        T->getErrorResult().getInterfaceType().print(Printer, Options);
      }

      if (parenthesizeResults)
        Printer << ")";
    }

    // Both the pattern and invocation substitution types are always in
    // terms of the outer environment.  But this wouldn't necessarily be
    // true with higher-rank polymorphism.
    if (auto substitutions = T->getPatternSubstitutions()) {
      Printer << " for";
      printSubstitutions(substitutions);
    }
    if (auto substitutions = T->getInvocationSubstitutions()) {
      Printer << " for";
      printSubstitutions(substitutions);
    }
  }

  static bool mustParenthesizeResults(SILFunctionType *T) {
    // If we don't have exactly one result, we must parenthesize.
    unsigned totalResults =
      T->getNumYields() + T->getNumResults() + unsigned(T->hasErrorResult());
    if (totalResults != 1)
      return true;

    // If we have substitutions, we must parenthesize if the single
    // result is a function type.
    if (!T->hasPatternSubstitutions() && !T->hasInvocationSubstitutions())
      return false;
    if (T->getNumResults() == 1)
      return isa<SILFunctionType>(T->getResults()[0].getInterfaceType());
    if (T->getNumYields() == 1)
      return isa<SILFunctionType>(T->getYields()[0].getInterfaceType());
    return isa<SILFunctionType>(T->getErrorResult().getInterfaceType());
  }

  void visitSILBlockStorageType(SILBlockStorageType *T,
                                NonRecursivePrintOptions nrOptions) {
    Printer << "@block_storage ";
    printWithParensIfNotSimple(T->getCaptureType());
  }

  void visitSILBoxType(SILBoxType *T, NonRecursivePrintOptions nrOptions) {
    // Print attributes.
    if (T->getLayout()->capturesGenericEnvironment()) {
      Printer << "@captures_generics ";
    }
    
    {
      // A box layout has its own independent generic environment. Don't try
      // to print it with the environment's generic params.
      PrintOptions::OverrideScope scope(Options);
      OVERRIDE_PRINT_OPTION(scope, GenericSig, nullptr);

      if (auto sig = T->getLayout()->getGenericSignature()) {
        printGenericSignature(sig,
                        PrintAST::PrintParams |
                        PrintAST::defaultGenericRequirementFlags(Options));
        Printer << " ";
      }
      Printer << "{";
      interleave(T->getLayout()->getFields(),
                 [&](const SILField &field) {
                   Printer <<
                     (field.isMutable() ? " var " : " let ");
                   visit(field.getLoweredType());
                 },
                 [&]{
                   Printer << ",";
                 });
      Printer << " }";
    }

    // The arguments to the layout, if any, do come from the outer environment.
    if (auto subMap = T->getSubstitutions()) {
      printSubstitutions(subMap);
    }
  }

  void visitSILMoveOnlyWrappedType(SILMoveOnlyWrappedType *T,
                                   NonRecursivePrintOptions nrOptions) {
    Printer << "@moveOnly ";
    printWithParensIfNotSimple(T->getInnerType());
  }

  void visitArraySliceType(ArraySliceType *T,
                           NonRecursivePrintOptions nrOptions) {
    if (Options.AlwaysDesugarArraySliceTypes) {
      visit(T->getDesugaredType());
    } else {
      Printer << "[";
      visit(T->getBaseType());
      Printer << "]";
    }
  }

  void visitInlineArrayType(InlineArrayType *T,
                            NonRecursivePrintOptions nrOptions) {
    if (Options.AlwaysDesugarInlineArrayTypes) {
      visit(T->getDesugaredType());
    } else {
      Printer << "[";
      visit(T->getCountType());
      Printer << " of ";
      visit(T->getElementType());
      Printer << "]";
    }
  }

  void visitDictionaryType(DictionaryType *T,
                           NonRecursivePrintOptions nrOptions) {
    if (Options.AlwaysDesugarDictionaryTypes) {
      visit(T->getDesugaredType());
    } else {
      Printer << "[";
      visit(T->getKeyType());
      Printer << " : ";
      visit(T->getValueType());
      Printer << "]";
    }
  }

  void visitOptionalType(OptionalType *T,
                         NonRecursivePrintOptions nrOptions) {
    if (Options.AlwaysDesugarOptionalTypes) {
      visit(T->getDesugaredType(), nrOptions);
      return;
    } else {
      printWithParensIfNotSimple(T->getBaseType());
      if (nrOptions & NonRecursivePrintOption::ImplicitlyUnwrappedOptional)
        Printer << "!";
      else
        Printer << "?";
    }
  }

  void visitVariadicSequenceType(VariadicSequenceType *T,
                                 NonRecursivePrintOptions nrOptions) {
    if (Options.PrintForSIL) {
      Printer << "[";
      visit(T->getBaseType());
      Printer << "]";
    } else {
      visit(T->getBaseType());
      Printer << "...";
    }
  }

  void visitProtocolType(ProtocolType *T,
                         NonRecursivePrintOptions nrOptions) {
    printQualifiedType(T);
  }

  void visitProtocolCompositionType(ProtocolCompositionType *T,
                                    NonRecursivePrintOptions nrOptions) {
    interleave(T->getMembers(), [&](Type Ty) { visit(Ty); },
        [&] { Printer << " & "; });

    bool printed = !T->getMembers().empty();
    auto printSpecial = [&](llvm::StringRef str, bool tilde=false) {
      if (printed)
        Printer << " & ";

      if (tilde)
        Printer << "~";

      Printer << str;
      printed = true;
    };

    if (T->hasExplicitAnyObject())
      printSpecial("AnyObject");

    for (auto ip : T->getInverses())
      printSpecial(getProtocolName(getKnownProtocolKind(ip)), true);

    if (!printed)
      Printer.printKeyword("Any", Options);
  }

  void visitParameterizedProtocolType(ParameterizedProtocolType *T,
                                      NonRecursivePrintOptions nrOptions) {
    visit(T->getBaseType());
    Printer << "<";
    interleave(T->getArgs(), [&](Type Ty) { visit(Ty); },
               [&] { Printer << ", "; });
    Printer << ">";
  }

  void visitExistentialType(ExistentialType *T,
                            NonRecursivePrintOptions nrOptions) {
    if (T->shouldPrintWithAny())
      Printer << "any ";

    // FIXME: The desugared type is used here only to support
    // existential types with protocol typealiases in Swift
    // interfaces. Verifying that the underlying type of a
    // protocol typealias is a constriant type is fundamentally
    // circular, so the desugared type should be written in source.
    if (Options.DesugarExistentialConstraint && !T->isAnyObject()) {
      visit(T->getConstraintType()->getDesugaredType());
    } else {
      visit(T->getConstraintType());
    }
  }

  void visitBuiltinTupleType(BuiltinTupleType *T,
                             NonRecursivePrintOptions nrOptions) {
    printQualifiedType(T);
  }

  void visitLValueType(LValueType *T, NonRecursivePrintOptions nrOptions) {
    Printer << "@lvalue ";
    visit(T->getObjectType());
  }

  void visitInOutType(InOutType *T, NonRecursivePrintOptions nrOptions) {
    Printer << tok::kw_inout << " ";
    // Apply the non-recursive options to the object type of `inout`.
    // This might seem a little weird, but it's convenient for the uncommon
    // cases that we do actually print InOutType.
    visit(T->getObjectType(), nrOptions);
  }

  void visitExistentialArchetypeType(ExistentialArchetypeType *T,
                                     NonRecursivePrintOptions nrOptions) {
    if (Options.PrintForSIL) {
      auto *env = T->getGenericEnvironment();

      Printer << "@opened(\"" << env->getOpenedExistentialUUID() << "\", ";
      auto existentialTy = env->maybeApplyOuterContextSubstitutions(
          env->getOpenedExistentialType());
      visit(existentialTy);
      Printer << ") ";

      llvm::DenseMap<CanType, Identifier> newAlternativeTypeNames;

      auto interfaceTy = T->getInterfaceType();
      auto selfTy = interfaceTy->getRootGenericParam();
      auto &ctx = selfTy->getASTContext();
      newAlternativeTypeNames[selfTy->getCanonicalType()] = ctx.Id_Self;

      PrintOptions::OverrideScope scope(Options);
      OVERRIDE_PRINT_OPTION(scope, AlternativeTypeNames, &newAlternativeTypeNames);
      visit(interfaceTy);
    } else {
      visit(T->getExistentialType());
    }
  }

  static Type findPackForElementArchetype(ElementArchetypeType *T) {
    // The type in @pack_element is looked up in the generic params
    // of the identified open_pack_element instruction.  The param list
    // is long gone, but the sugar survives in the type parameters of
    // the generic signature of the contextual substitution map in the
    // opened element environment.
    auto env = T->getGenericEnvironment();
    auto subs = env->getOuterSubstitutions();
    auto sig = subs.getGenericSignature();
    auto params = sig.getGenericParams();

    auto elementShapeClass = env->getOpenedElementShapeClass();

    // The element archetypes are at a depth one past the max depth
    // of the base signature.
    unsigned elementDepth = sig.getNextDepth();

    // Transform the archetype's interface type to be based on the
    // corresponding non-canonical type parameter.
    auto interfaceType = T->getInterfaceType();
    return interfaceType.subst([&](SubstitutableType *type) -> Type {
      // Don't transform types that aren't element type parameters.
      auto *elementParam = type->getAs<GenericTypeParamType>();
      if (!elementParam || elementParam->getDepth() != elementDepth)
        return Type();

      // Loop through the type parameters looking for the type parameter
      // pack at the appropriate index.  We only expect to actually do
      // this once for each type, so it's fine to do it in the callback.
      unsigned nextIndex = 0;
      for (auto *genericParam : params) {
        if (!genericParam->isParameterPack())
          continue;

        if (!sig->haveSameShape(genericParam, elementShapeClass))
          continue;

        if (nextIndex == elementParam->getIndex())
          return genericParam;
        nextIndex++;
      }
      llvm_unreachable("ran out of type parameters");
      return Type();
    }, LookUpConformanceInModule());
  }

  void visitElementArchetypeType(ElementArchetypeType *T,
                                 NonRecursivePrintOptions nrOptions) {
    if (Options.PrintForSIL) {
      Printer << "@pack_element(\"" << T->getOpenedElementID() << "\") ";
      auto packTy = findPackForElementArchetype(T);
      visit(packTy);
    } else {
      visit(T->getInterfaceType());
    }
  }

  void printDependentMember(DependentMemberType *T) {
    if (auto *const Assoc = T->getAssocType()) {
      if (Options.ProtocolQualifiedDependentMemberTypes) {
        Printer << "[";
        Printer.printName(Assoc->getProtocol()->getName());
        Printer << "]";
      }
      Printer.printTypeRef(T, Assoc, T->getName());
    } else {
      Printer.printName(T->getName());
    }
  }

  void printEach() {
    Printer << "each ";
  }

  void printLet() {
    Printer << "let ";
  }

  void printArchetypeCommon(Type interfaceTy, GenericEnvironment *env) {
    if (auto *paramTy = interfaceTy->getAs<GenericTypeParamType>()) {
      if (Options.AlternativeTypeNames) {
        auto archetypeTy = env->mapTypeIntoContext(paramTy)->getAs<GenericTypeParamType>();
        if (archetypeTy) {
          auto found = Options.AlternativeTypeNames->find(CanType(archetypeTy));
          if (found != Options.AlternativeTypeNames->end()) {
            if (paramTy->isParameterPack()) printEach();
            if (paramTy->isValue()) printLet();
            Printer << found->second.str();
            return;
          }
        }
      }

      visit(paramTy);
      return;
    }

    auto *memberTy = interfaceTy->castTo<DependentMemberType>();
    if (auto *paramTy = memberTy->getBase()->getAs<GenericTypeParamType>())
      printParentType(env->mapTypeIntoContext(paramTy));
    else {
      printArchetypeCommon(memberTy->getBase(), env);
      Printer << ".";
    }

    printDependentMember(memberTy);
  }

  void visitPrimaryArchetypeType(PrimaryArchetypeType *T,
                                 NonRecursivePrintOptions nrOptions) {
    printArchetypeCommon(T->getInterfaceType(), T->getGenericEnvironment());
  }

  void visitOpaqueTypeArchetypeType(OpaqueTypeArchetypeType *T,
                                    NonRecursivePrintOptions nrOptions) {
    auto interfaceTy = T->getInterfaceType();
    auto *paramTy = interfaceTy->getAs<GenericTypeParamType>();

    if (!paramTy) {
      assert(interfaceTy->is<DependentMemberType>());
      printArchetypeCommon(interfaceTy, T->getGenericEnvironment());
      return;
    }

    // Try to print a named opaque type.
    auto printNamedOpaque = [&] {
      unsigned ordinal = paramTy->getIndex();
      if (auto genericParam = T->getDecl()->getExplicitGenericParam(ordinal)) {
        visit(genericParam->getDeclaredInterfaceType());
        return true;
      }

      return false;
    };

    OpaqueTypeDecl *decl = T->getDecl();
    auto *namingDecl = decl->getNamingDecl();
    auto genericSig = namingDecl->getInnermostDeclContext()
          ->getGenericSignatureOfContext();

    switch (Options.OpaqueReturnTypePrinting) {
    case PrintOptions::OpaqueReturnTypePrintingMode::WithOpaqueKeyword:
      if (printNamedOpaque())
        return;

      Printer.printKeyword("some", Options, /*Suffix=*/" ");
      LLVM_FALLTHROUGH;
    case PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword: {
      if (printNamedOpaque())
        return;

      auto constraint = T->getExistentialType();
      if (auto existential = constraint->getAs<ExistentialType>())
        constraint = existential->getConstraintType();

      // Opaque archetype substitutions are always canonical, so re-sugar the
      // constraint type using the owning declaration's generic parameter names.
      if (genericSig)
        constraint = genericSig->getSugaredType(constraint);

      visit(constraint);
      return;
    }
    case PrintOptions::OpaqueReturnTypePrintingMode::StableReference: {
      // Print the source of the opaque return type as a mangled name.
      // We'll use type reconstruction while parsing the attribute to
      // turn this back into a reference to the naming decl for the opaque
      // type.
      Printer << "@_opaqueReturnTypeOf(";

      Printer.printEscapedStringLiteral(
                                   decl->getOpaqueReturnTypeIdentifier().str());

      Printer << ", " << paramTy->getIndex();

      // The identifier after the closing parenthesis is irrelevant and can be
      // anything. It just needs to be there for the @_opaqueReturnTypeOf
      // attribute to apply to, but the attribute alone references the opaque
      // type.
      Printer << ") __";

      if (genericSig) {
        printGenericArgs(decl->getASTContext(),
                         genericSig.getGenericParams(),
                         T->getSubstitutions().getReplacementTypes());
      }
      return;
    }
    case PrintOptions::OpaqueReturnTypePrintingMode::Description: {
      // TODO(opaque): present opaque types with user-facing syntax. we should
      // probably print this as `some P` and record the fact that we printed that
      // so that diagnostics can add followup notes.
      Printer << "(return type of " << namingDecl->printRef();
      Printer << ')';
      if (genericSig) {
        printGenericArgs(decl->getASTContext(),
                         genericSig.getGenericParams(),
                         T->getSubstitutions().getReplacementTypes());
      }
      return;
    }
    }
  }

  void visitPackArchetypeType(PackArchetypeType *T,
                              NonRecursivePrintOptions nrOptions) {
    printArchetypeCommon(T->getInterfaceType(), T->getGenericEnvironment());
  }

  void visitGenericTypeParamType(GenericTypeParamType *T,
                                 NonRecursivePrintOptions nrOptions) {
    auto printPrefix = [&]{
      if (T->isParameterPack()) printEach();
    };

    if (T->isCanonical()) {
      // If we have an alternate name for this type, use it.
      if (Options.AlternativeTypeNames) {
        auto found = Options.AlternativeTypeNames->find(T->getCanonicalType());
        if (found != Options.AlternativeTypeNames->end()) {
          printPrefix();
          Printer << found->second.str();
          return;
        }
      }

      // When printing SIL types, use a generic signature to map them from
      // canonical types to sugared types.
      if (Options.GenericSig)
        T = Options.GenericSig->getSugaredType(T);
    }

    // Print opaque types as "some ..."
    if (auto *decl =T->getOpaqueDecl()) {
      // For SIL, we print opaque parameter types as canonical types, and parse
      // them that way too (because they're printed in this way in the SIL
      // generic parameter list).
      if (Options.PrintInSILBody) {
        Printer.printName(cast<GenericTypeParamType>(T->getCanonicalType())->getName());
        return;
      }

      // If we have and should print based on the type representation, do so.
      if (auto opaqueRepr = decl->getOpaqueTypeRepr()) {
        if (willUseTypeReprPrinting(opaqueRepr, Type(), Options)) {
          printPrefix();
          opaqueRepr->print(Printer, Options);
          return;
        }
      }

      // Print based on the type.
      Printer.printKeyword("some", Options, /*Suffix=*/" ");
      auto archetypeType = decl->getDeclContext()->mapTypeIntoContext(
          decl->getDeclaredInterfaceType())->castTo<ArchetypeType>();
      auto constraintType = archetypeType->getExistentialType();
      if (auto *existentialType = constraintType->getAs<ExistentialType>())
        constraintType = existentialType->getConstraintType();

      constraintType->print(Printer, Options);
      return;
    }

    printPrefix();

    const auto Name = T->getName();
    if (Name.empty()) {
      Printer << "<anonymous>";
    } else if (auto *Decl = T->getDecl()) {
      Printer.printTypeRef(T, Decl, Name);
    } else {
      Printer.printName(Name);
    }
  }

  void visitDependentMemberType(DependentMemberType *T,
                                NonRecursivePrintOptions nrOptions) {
    printParentType(T->getBase());
    printDependentMember(T);
  }

#define REF_STORAGE(Name, name, ...) \
  void visit##Name##StorageType(Name##StorageType *T, \
                                NonRecursivePrintOptions nrOptions) { \
    if (Options.PrintStorageRepresentationAttrs) \
      Printer << "@sil_" #name " "; \
    visit(T->getReferentType(), nrOptions); \
  }
#include "swift/AST/ReferenceStorage.def"

  void visitTypeVariableType(TypeVariableType *T,
                             NonRecursivePrintOptions nrOptions) {
    if (Options.PrintTypesForDebugging) {
      Printer << "$T" << T->getID();
      return;
    }

    Printer << "_";
  }

  void visitIntegerType(IntegerType *T, NonRecursivePrintOptions nrOptions) {
    if (T->isNegative()) {
      Printer << "-";
    }

    Printer << T->getDigitsText();
  }
};
} // unnamed namespace

void Type::print(raw_ostream &OS, const PrintOptions &PO,
                 NonRecursivePrintOptions nrOptions) const {
  StreamPrinter Printer(OS);
  print(Printer, PO, nrOptions);
}
void Type::print(ASTPrinter &Printer, const PrintOptions &PO,
                 NonRecursivePrintOptions nrOptions) const {
  if (isNull()) {
    if (!PO.AllowNullTypes) {
      // Use report_fatal_error instead of assert to trap in release builds too.
      llvm::report_fatal_error("Cannot pretty-print a null type");
    }
    Printer << "<null>";
    return;
  }
  TypePrinter(Printer, PO).visit(*this, nrOptions);
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
  // TODO: Handle lifetime dependence printing here
  TypePrinter(Printer, PO)
      .visitAnyFunctionTypeParams(Params,
                                  /*printLabels*/ true, {});
}

std::string
AnyFunctionType::getParamListAsString(ArrayRef<AnyFunctionType::Param> Params,
                                      const PrintOptions &PO) {
  SmallString<16> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  AnyFunctionType::printParams(Params, OS);
  return std::string(OS.str());
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
  Printer << getName(PO.PrintInternalLayoutName);
  switch (getKind()) {
  case LayoutConstraintKind::UnknownLayout:
  case LayoutConstraintKind::RefCountedObject:
  case LayoutConstraintKind::NativeRefCountedObject:
  case LayoutConstraintKind::Class:
  case LayoutConstraintKind::NativeClass:
  case LayoutConstraintKind::Trivial:
  case LayoutConstraintKind::BridgeObject:
    return; // non-parameterized cases
  case LayoutConstraintKind::TrivialOfAtMostSize:
  case LayoutConstraintKind::TrivialOfExactSize:
  case LayoutConstraintKind::TrivialStride:
    Printer << "(";
    Printer << SizeInBits;
    if (Alignment)
      Printer << ", " << Alignment;
    Printer << ")";
    break;
  }
}

void GenericSignatureImpl::print(raw_ostream &OS,
                                 const PrintOptions &PO) const {
  GenericSignature(const_cast<GenericSignatureImpl *>(this)).print(OS, PO);
}
void GenericSignatureImpl::print(ASTPrinter &Printer,
                                 const PrintOptions &PO) const {
  GenericSignature(const_cast<GenericSignatureImpl *>(this)).print(Printer, PO);
}

void GenericSignature::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}

void GenericSignature::print(ASTPrinter &Printer,
                             const PrintOptions &Opts) const {
  if (isNull()) {
    Printer << "<null>";
    return;
  }

  auto flags = PrintAST::PrintParams | PrintAST::PrintRequirements;
  if (Opts.PrintInverseRequirements)
    flags |= PrintAST::PrintInverseRequirements;
  PrintAST(Printer, Opts).printGenericSignature(*this, flags);
}

void RequirementSignature::print(ProtocolDecl *owner,
                                 raw_ostream &OS,
                                 const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(owner, Printer, Opts);
}

void RequirementSignature::print(ProtocolDecl *owner,
                                 ASTPrinter &Printer,
                                 const PrintOptions &Opts) const {
  auto flags = PrintAST::PrintParams | PrintAST::PrintRequirements;
  if (Opts.PrintInverseRequirements)
    flags |= PrintAST::PrintInverseRequirements;
  PrintAST(Printer, Opts).printRequirementSignature(owner, *this, flags, nullptr);
}

void Requirement::print(raw_ostream &os, const PrintOptions &opts) const {
  StreamPrinter printer(os);
  PrintAST(printer, opts).printRequirement(*this);
}

void Requirement::print(ASTPrinter &printer, const PrintOptions &opts) const {
  PrintAST(printer, opts).printRequirement(*this);
}

void InverseRequirement::print(raw_ostream &os,
                          const PrintOptions &opts,
                          bool forInherited) const {
  StreamPrinter printer(os);
  PrintAST(printer, opts).printRequirement(*this, forInherited);
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

StringRef swift::getStringForParameterConvention(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Indirect_In: return "@in ";
  case ParameterConvention::Indirect_In_Guaranteed:  return "@in_guaranteed ";
  case ParameterConvention::Indirect_Inout: return "@inout ";
  case ParameterConvention::Indirect_InoutAliasable: return "@inout_aliasable ";
  case ParameterConvention::Indirect_In_CXX: return "@in_cxx ";
  case ParameterConvention::Direct_Owned: return "@owned ";
  case ParameterConvention::Direct_Unowned: return "";
  case ParameterConvention::Direct_Guaranteed: return "@guaranteed ";
  case ParameterConvention::Pack_Guaranteed: return "@pack_guaranteed ";
  case ParameterConvention::Pack_Owned: return "@pack_owned ";
  case ParameterConvention::Pack_Inout: return "@pack_inout ";
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

void SILParameterInfo::print(
    raw_ostream &OS, const PrintOptions &Opts,
    std::optional<LifetimeDependenceInfo> lifetimeDependence) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts, lifetimeDependence);
}

void SILParameterInfo::print(
    ASTPrinter &Printer, const PrintOptions &Opts,
    std::optional<LifetimeDependenceInfo> lifetimeDependence) const {
  auto options = getOptions();

  if (options.contains(SILParameterInfo::NotDifferentiable)) {
    options -= SILParameterInfo::NotDifferentiable;
    Printer << "@noDerivative ";
  }

  if (options.contains(SILParameterInfo::Sending)) {
    options -= SILParameterInfo::Sending;
    Printer << "@sil_sending ";
  }

  if (options.contains(SILParameterInfo::Isolated)) {
    options -= SILParameterInfo::Isolated;
    Printer << "@sil_isolated ";
  }
      
  if (options.contains(SILParameterInfo::Const)) {
    options -= SILParameterInfo::Const;
    Printer << "@const ";
  }

  if (lifetimeDependence) {
    Printer.printLifetimeDependence(*lifetimeDependence);
  }

  if (options.contains(SILParameterInfo::ImplicitLeading)) {
    options -= SILParameterInfo::ImplicitLeading;
    Printer << "@sil_implicit_leading_param ";
  }

  // If we did not handle a case in Options, this code was not updated
  // appropriately.
  assert(!bool(options) && "Code not updated for introduced option");

  Printer << getStringForParameterConvention(getConvention());
  getInterfaceType().print(Printer, Opts);
}

static StringRef getStringForResultConvention(ResultConvention conv) {
  switch (conv) {
  case ResultConvention::Indirect: return "@out ";
  case ResultConvention::Owned: return "@owned ";
  case ResultConvention::Unowned: return "";
  case ResultConvention::UnownedInnerPointer: return "@unowned_inner_pointer ";
  case ResultConvention::Autoreleased: return "@autoreleased ";
  case ResultConvention::Pack: return "@pack_out ";
  }
  llvm_unreachable("bad result convention");
}

void SILResultInfo::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}

void SILResultInfo::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  auto options = getOptions();

  if (options.contains(SILResultInfo::NotDifferentiable)) {
    options -= SILResultInfo::NotDifferentiable;
    Printer << "@noDerivative ";
  }

  if (options.contains(SILResultInfo::IsSending)) {
    options -= SILResultInfo::IsSending;
    Printer << "@sil_sending ";
  }

  assert(!bool(options) && "ResultInfo has option that was not handled?!");

  Printer << getStringForResultConvention(getConvention());
  getInterfaceType().print(Printer, Opts);
}

std::string Type::getString(const PrintOptions &PO,
                            NonRecursivePrintOptions nrOptions) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO, nrOptions);
  return OS.str();
}

std::string TypeBase::getString(const PrintOptions &PO,
                                NonRecursivePrintOptions nrOptions) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO, nrOptions);
  return OS.str();
}

std::string Type::getStringAsComponent(const PrintOptions &PO,
                                       NonRecursivePrintOptions nrOptions) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);

  if (getPointer()->hasSimpleTypeRepr()) {
    print(OS, PO, nrOptions);
  } else {
    OS << "(";
    print(OS, PO, nrOptions);
    OS << ")";
  }

  return OS.str();
}

std::string TypeBase::getStringAsComponent(const PrintOptions &PO,
                                           NonRecursivePrintOptions nrOptions) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);

  if (hasSimpleTypeRepr()) {
    print(OS, PO, nrOptions);
  } else {
    OS << "(";
    print(OS, PO, nrOptions);
    OS << ")";
  }

  return OS.str();
}

void TypeBase::print() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void TypeBase::print(raw_ostream &OS, const PrintOptions &PO,
                     NonRecursivePrintOptions nrOptions) const {
  Type(const_cast<TypeBase *>(this)).print(OS, PO, nrOptions);
}
void TypeBase::print(ASTPrinter &Printer, const PrintOptions &PO,
                     NonRecursivePrintOptions nrOptions) const {
  Type(const_cast<TypeBase *>(this)).print(Printer, PO, nrOptions);
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
          .printGenericSignature(
            genericSig,
            PrintAST::PrintParams |
            PrintAST::defaultGenericRequirementFlags(PO));
      os << ' ';
    }
  }

  getType()->print(os, PO);
  os << ": ";

  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto normal = cast<NormalProtocolConformance>(this);
    os << normal->getProtocol()->getName()
       << " module " << normal->getDeclContext()->getParentModule()->getRealName();
    break;
  }
  case ProtocolConformanceKind::Self: {
    auto self = cast<SelfProtocolConformance>(this);
    os << self->getProtocol()->getName()
       << " module " << self->getDeclContext()->getParentModule()->getRealName();
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
  case ProtocolConformanceKind::Builtin: {
    auto builtin = cast<BuiltinProtocolConformance>(this);
    os << builtin->getProtocol()->getName()
       << " type " << builtin->getType();
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
/// For a protocol, don't consult getInherited() at all. Instead, rebuild
/// the inherited types from getInheritedProtocols(), getSuperclass(), and
/// the inverse requirement transform.
///
/// FIXME: This seems generally useful and should be moved elsewhere.
static void getSyntacticInheritanceClause(const ProtocolDecl *proto,
                                          llvm::SmallVectorImpl<InheritedEntry> &Results) {
  auto &ctx = proto->getASTContext();

  auto genericSig = proto->getGenericSignature();
  if (auto superclassTy = genericSig->getSuperclassBound(
        proto->getSelfInterfaceType())) {
    Results.emplace_back(TypeLoc::withoutLoc(superclassTy),
                         ProtocolConformanceOptions());
  }

  InvertibleProtocolSet inverses = InvertibleProtocolSet::allKnown();

  for (auto *inherited : proto->getInheritedProtocols()) {
    if (auto ip = inherited->getInvertibleProtocolKind()) {
      inverses.remove(*ip);
      continue;
    }

    for (auto ip : InvertibleProtocolSet::allKnown()) {
      auto *proto = ctx.getProtocol(getKnownProtocolKind(ip));
      if (inherited->inheritsFrom(proto))
        inverses.remove(ip);
    }

    Results.emplace_back(TypeLoc::withoutLoc(inherited->getDeclaredInterfaceType()),
                      ProtocolConformanceOptions());
  }

  for (auto ip : inverses) {
    InvertibleProtocolSet singleton;
    singleton.insert(ip);

    auto inverseTy = ProtocolCompositionType::get(
        ctx, ArrayRef<Type>(), singleton,
        /*hasExplicitAnyObject=*/false);
    Results.emplace_back(TypeLoc::withoutLoc(inverseTy),
                         ProtocolConformanceOptions());
  }
}

void
swift::getInheritedForPrinting(
    const Decl *decl, const PrintOptions &options,
    llvm::SmallVectorImpl<InheritedEntry> &Results) {
  if (auto *proto = dyn_cast<ProtocolDecl>(decl)) {
    getSyntacticInheritanceClause(proto, Results);
    return;
  }

  InheritedTypes inherited = InheritedTypes(decl);

  // Collect explicit inherited types.
  for (auto i : inherited.getIndices()) {
    if (auto ty = inherited.getResolvedType(i)) {
      // Preserve inverses separately, because the `foundUnprintable` logic
      // doesn't handle compositions with a mix of printable and unprintable
      // types! That's handled later by `InheritedProtocolCollector`.
      //
      // Generally speaking, `getInheritedForPrinting` needs to be
      // querying `InheritedProtocolCollector` to find out what protocols it
      // should print in the inheritance clause, to reduce code duplication
      // in the printer.
      InvertibleProtocolSet printableInverses;

      bool foundUnprintable = ty.findIf([&](Type subTy) {
        {
          // We canonicalize the composition to ensure no inverses are missed.
          auto subCanTy = subTy->getCanonicalType();
          if (auto PCT = subCanTy->getAs<ProtocolCompositionType>()) {
            printableInverses.insertAll(PCT->getInverses());
          }
        }
        if (auto aliasTy = dyn_cast<TypeAliasType>(subTy.getPointer()))
          return !options.shouldPrint(aliasTy->getDecl());
        if (auto NTD = subTy->getAnyNominal()) {
          if (!options.shouldPrint(NTD))
            return true;
        }
        return false;
      });

      // Preserve any inverses that appeared in the unprintable type.
      if (foundUnprintable) {
        if (!printableInverses.empty()) {
          auto inversesTy = ProtocolCompositionType::get(decl->getASTContext(),
                                                         /*members=*/{},
                                                         printableInverses,
                                                         /*anyObject=*/false);
          Results.push_back(InheritedEntry(TypeLoc::withoutLoc(inversesTy)));
        }
        continue;
      }
    }

    if (options.SuppressConformanceSuppression &&
        inherited.getEntry(i).isSuppressed()) {
      continue;
    }

    Results.push_back(inherited.getEntry(i));
  }

  // Collect synthesized conformances.
  llvm::SetVector<ProtocolDecl *> protocols;
  llvm::TinyPtrVector<ProtocolDecl *> uncheckedProtocols;
  for (auto attr : decl->getAttrs().getAttributes<SynthesizedProtocolAttr>()) {
    if (auto *proto = attr->getProtocol()) {
      // FIXME: Reconstitute inverses here
      if (proto->getInvertibleProtocolKind())
        continue;

      // The SerialExecutor conformance is only synthesized on the root
      // actor class, so we can just test resilience immediately.
      if (proto->isSpecificProtocol(KnownProtocolKind::SerialExecutor) &&
          cast<ClassDecl>(decl)->isResilient())
        continue;
      if (proto->getKnownProtocolKind() &&
          *proto->getKnownProtocolKind() == KnownProtocolKind::RawRepresentable &&
          isa<EnumDecl>(decl) &&
          cast<EnumDecl>(decl)->hasRawType())
        continue;
      protocols.insert(proto);
      if (attr->isUnchecked())
        uncheckedProtocols.push_back(proto);
    }
  }

  for (size_t i = 0; i < protocols.size(); i++) {
    auto proto = protocols[i];
    bool isUnchecked = llvm::is_contained(uncheckedProtocols, proto);

    if (!options.shouldPrint(proto)) {
      // If private stdlib protocols are skipped and this is a private stdlib
      // protocol, see if any of its inherited protocols are public. Those
      // protocols can affect the user-visible behavior of the declaration, and
      // should be printed.
      if (options.SkipPrivateSystemDecls &&
          proto->isPrivateSystemDecl(!options.SkipUnderscoredSystemProtocols)) {
        auto inheritedProtocols = proto->getInheritedProtocols();
        protocols.insert(inheritedProtocols.begin(), inheritedProtocols.end());
        if (isUnchecked)
          copy(inheritedProtocols, std::back_inserter(uncheckedProtocols));
      }
      continue;
    }

    // FIXME: Reconstitute inverses here
    if (proto->getInvertibleProtocolKind())
      continue;

    ProtocolConformanceOptions options;
    if (isUnchecked)
      options |= ProtocolConformanceFlags::Unchecked;
    Results.push_back({TypeLoc::withoutLoc(proto->getDeclaredInterfaceType()),
                       options});
  }
}

//===----------------------------------------------------------------------===//
//  Generic param list printing.
//===----------------------------------------------------------------------===//

void RequirementRepr::print(raw_ostream &out) const {
  StreamPrinter printer(out);
  print(printer);
}

void RequirementRepr::print(ASTPrinter &out) const {
  auto printLayoutConstraint =
      [&](const LayoutConstraintLoc &LayoutConstraintLoc) {
        LayoutConstraintLoc.getLayoutConstraint()->print(out, PrintOptions());
      };

  switch (getKind()) {
  case RequirementReprKind::LayoutConstraint:
    if (auto *repr = getSubjectRepr()) {
      repr->print(out, PrintOptions());
    }
    out << " : ";
    printLayoutConstraint(getLayoutConstraintLoc());
    break;

  case RequirementReprKind::TypeConstraint:
    if (auto *repr = getSubjectRepr()) {
      repr->print(out, PrintOptions());
    }
    out << " : ";
    if (auto *repr = getConstraintRepr()) {
      repr->print(out, PrintOptions());
    }
    break;

  case RequirementReprKind::SameType:
    if (auto *repr = getFirstTypeRepr()) {
      repr->print(out, PrintOptions());
    }
    out << " == ";
    if (auto *repr = getSecondTypeRepr()) {
      repr->print(out, PrintOptions());
    }
    break;
  }
}

void GenericParamList::print(raw_ostream &out, const PrintOptions &PO) const {
  StreamPrinter printer(out);
  print(printer, PO);
}

static void printTrailingRequirements(ASTPrinter &Printer,
                                      ArrayRef<RequirementRepr> Reqs,
                                      bool printWhereKeyword) {
  if (Reqs.empty())
    return;

  if (printWhereKeyword)
    Printer << " where ";
  interleave(
      Reqs,
      [&](const RequirementRepr &req) {
        Printer.callPrintStructurePre(PrintStructureKind::GenericRequirement);
        req.print(Printer);
        Printer.printStructurePost(PrintStructureKind::GenericRequirement);
      },
      [&] { Printer << ", "; });
}

void GenericParamList::print(ASTPrinter &Printer,
                             const PrintOptions &PO) const {
  Printer << '<';
  interleave(
      *this,
      [&](const GenericTypeParamDecl *P) {
        Printer << P->getName();
        if (!P->getInherited().empty()) {
          Printer << " : ";

          auto loc = P->getInherited().getEntry(0);
          if (willUseTypeReprPrinting(loc, nullptr, PO)) {
            loc.getTypeRepr()->print(Printer, PO);
          } else {
            loc.getType()->print(Printer, PO);
          }
        }
      },
      [&] { Printer << ", "; });

  printTrailingRequirements(Printer, getRequirements(),
                            /*printWhereKeyword*/ true);
  Printer << '>';
}

void TrailingWhereClause::print(llvm::raw_ostream &OS,
                                bool printWhereKeyword) const {
  StreamPrinter Printer(OS);
  printTrailingRequirements(Printer, getRequirements(), printWhereKeyword);
}
