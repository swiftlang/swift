//===--- ImportDecl.cpp - Import Clang Declarations -----------------------===//
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
///
/// This file infers and attaches macros to imported decls based on their attributes.
///
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Import.h"
#include "swift/AST/InternalMacro.h"
#include "swift/AST/MacroDeclaration.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangImporterRequests.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Module.h"
#include "clang/Sema/Overload.h"
#include "llvm-c/Types.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <optional>

using namespace swift;
using namespace importer;

#define DEBUG_TYPE "safe-interop-wrappers"

#define DLOG(x) LLVM_DEBUG(LogIndentTracker::indent(DBGS) << x)

#ifndef NDEBUG
#define DBGS llvm::dbgs() << "[swiftify:" << __LINE__ << "] "
#define DUMP(x) DLOG(""); x->dump(llvm::errs())
#define DLOG_SCOPE(x) DLOG(x); LogIndentTracker Scope
#else
#define DLOG_SCOPE(x) do {} while(false);
#endif

namespace {
#ifndef NDEBUG
struct LogIndentTracker {
  static thread_local uint8_t LogIndent;
  static llvm::raw_ostream &indent(llvm::raw_ostream &out) {
    for (uint8_t i = 0; i < LogIndent; i++)
      out << "| ";
    return out;
  }

  LogIndentTracker() {
    LogIndent++;
  }
  ~LogIndentTracker() {
    LogIndent--;
  }
};
thread_local uint8_t LogIndentTracker::LogIndent = 0;
#endif

ValueDecl *getKnownSingleDecl(ASTContext &SwiftContext, StringRef DeclName) {
  SmallVector<ValueDecl *, 1> decls;
  SwiftContext.lookupInSwiftModule(DeclName, decls);
  ASSERT(decls.size() < 2);
  if (decls.size() != 1) return nullptr;
  return decls[0];
}

static bool isStdSpanType(clang::QualType clangType) {
  const auto *decl = clangType->getAsTagDecl();
  return decl && decl->isInStdNamespace() && decl->getName() == "span";
}

/// Escape an imported C identifier for use in generated Swift source, wrapping
/// Swift keywords (e.g. a C parameter named `guard`) in backticks.
static std::string escapeSwiftIdentifier(Identifier name) {
  std::string result;
  llvm::raw_string_ostream os(result);
  printIdentifierEscapingIfNeeded(name.str(), os);
  return result;
}

static void
printSwiftIdentifier(Identifier name, llvm::raw_ostream &os,
                     PrintNameContext context = PrintNameContext::Normal) {
  printIdentifierEscapingIfNeeded(name.str(), os, context);
}

// Walks a clang `Expr` tree that appears as a `__counted_by` /
// `__sized_by` count expression and emits an equivalent Swift expression.
// `Visit(expr)` returns true on success and the result can be retrieved via
// `str()`.
struct SwiftCountExprEmitter
    : clang::ConstStmtVisitor<SwiftCountExprEmitter, bool> {
  const clang::ASTContext &ctx;
  llvm::SmallString<128> result;
  llvm::raw_svector_ostream out;

  explicit SwiftCountExprEmitter(const clang::ASTContext &ctx)
      : ctx(ctx), out(result) {}

  StringRef str() const { return result; }

  bool VisitDeclRefExpr(const clang::DeclRefExpr *e) {
    const clang::DeclarationName name = e->getDecl()->getDeclName();
    if (name.getNameKind() != clang::DeclarationName::Identifier ||
        name.isEmpty()) {
      DLOG("Unsupported decl name in count expr\n");
      return false;
    }

    printIdentifierEscapingIfNeeded(e->getDecl()->getName(), out);
    return true;
  }

  bool VisitIntegerLiteral(const clang::IntegerLiteral *IL) {
    const auto *bt = IL->getType()->getAs<clang::BuiltinType>();
    if (!bt)
      return false;
    bool isSigned = IL->getType()->isSignedIntegerType();
    llvm::SmallString<20> valueStr;
    IL->getValue().toString(valueStr, /*Radix=*/10, isSigned);
    std::optional<StringRef> swiftName = getBuiltinTypeSwiftName(bt);
    if (!swiftName) {
      DLOG("Unsupported integer literal type\n");
      return false;
    }
    out << *swiftName << '(' << valueStr << ')';
    return true;
  }

  bool VisitImplicitCastExpr(const clang::ImplicitCastExpr *c) {
    return visitCastImpl(c);
  }

  bool VisitCStyleCastExpr(const clang::CStyleCastExpr *c) {
    return visitCastImpl(c);
  }

  bool VisitParenExpr(const clang::ParenExpr *p) {
    out << '(';
    if (!Visit(p->getSubExpr()))
      return false;
    out << ')';
    return true;
  }

  bool VisitUnaryOperator(const clang::UnaryOperator *unop) {
    char op;
    switch (unop->getOpcode()) {
#define UNOP(variant, c)                                                       \
  case clang::variant:                                                         \
    op = c;                                                                    \
    break
      UNOP(UO_Plus, '+');
      UNOP(UO_Minus, '-');
      UNOP(UO_Not, '~');
#undef UNOP
    default:
      DLOG("Unsupported unary operator\n");
      return false;
    }
    out << op;
    return Visit(unop->getSubExpr());
  }

  bool VisitBinaryOperator(const clang::BinaryOperator *binop) {
    StringRef op;
    switch (binop->getOpcode()) {
#define BINOP(variant, string)                                                 \
  case clang::variant:                                                         \
    op = " " string " ";                                                       \
    break
      BINOP(BO_Add, "+");
      BINOP(BO_Sub, "-");
      BINOP(BO_Mul, "*");
      BINOP(BO_Div, "/");
      BINOP(BO_Rem, "%");
      BINOP(BO_Shl, "<<");
      BINOP(BO_Shr, ">>");
      BINOP(BO_And, "&");
      BINOP(BO_Or, "|");
      BINOP(BO_Xor, "^");
#undef BINOP
    default:
      DLOG("Unsupported binary operator\n");
      return false;
    }
    // Always parenthesize binary operations: Swift and C disagree on the
    // relative precedence of `<<`/`>>` vs `+`/`-` and `&` vs `+`/`-`, so
    // unparenthesized output could change meaning across languages.
    out << '(';
    if (!Visit(binop->getLHS()))
      return false;
    out << op;
    if (!Visit(binop->getRHS()))
      return false;
    out << ')';
    return true;
  }

  bool VisitStmt(const clang::Stmt *) {
    DLOG("Ignoring count parameter with unsupported expression\n");
    return false;
  }

private:
  bool visitCastImpl(const clang::CastExpr *c) {
    ASSERT(isa<clang::CStyleCastExpr>(c) || isa<clang::ImplicitCastExpr>(c));
    using CK = clang::CastKind;
    switch (c->getCastKind()) {
    case CK::CK_LValueToRValue:
    case CK::CK_NoOp:
    case CK::CK_ArrayToPointerDecay:
    case CK::CK_FunctionToPointerDecay:
      return Visit(c->getSubExpr());
    case CK::CK_IntegralCast:
    case CK::CK_BooleanToSignedIntegral:
    case CK::CK_IntegralToBoolean:
    case CK::CK_IntegralToFloating:
    case CK::CK_FloatingToIntegral:
    case CK::CK_FloatingCast: {
      std::optional<StringRef> swiftName =
          getBuiltinTypeSwiftName(c->getType());
      if (!swiftName) {
        DLOG("Unsupported cast destination type\n");
        return false;
      }
      bool isExplicitCast = isa<clang::CStyleCastExpr>(c);
      out << *swiftName << '(';
      // Implicit casts get plain T(x) casts: trap instead of truncate
      // Explicit casts mirror C's truncation on narrowing integer conversions
      if (isExplicitCast && c->getCastKind() == CK::CK_IntegralCast)
        out << "truncatingIfNeeded: ";
      if (!Visit(c->getSubExpr()))
        return false;
      out << ')';
      return true;
    }
    default:
      DLOG("Unsupported cast kind\n");
      return false;
    }
  }
};

static Type ConcretePointeeType(Type swiftType) {
  Type nonnullType = swiftType->lookThroughSingleOptionalType();
  PointerTypeKind PTK;
  Type PointeeTy = nonnullType->getAnyPointerElementType(PTK);
  if (PointeeTy &&
      (PTK == PTK_UnsafePointer || PTK == PTK_UnsafeMutablePointer))
    return PointeeTy;
  return Type();
}

// Don't try to transform any Swift types that _SwiftifyImport doesn't know how
// to handle.
static bool
SwiftifiableSizedByPointerType(const clang::ASTContext &ctx, Type swiftType,
                               const clang::CountAttributedType *CAT) {
  Type nonnullType = swiftType->lookThroughSingleOptionalType();
  if (nonnullType->isOpaquePointer())
    return true;
  PointerTypeKind PTK;
  if (!nonnullType->getAnyPointerElementType(PTK)) {
    DLOG("Ignoring sized_by on non-pointer type\n");
    return false;
  }
  if (PTK == PTK_UnsafeRawPointer || PTK == PTK_UnsafeMutableRawPointer)
    return true;
  if (PTK != PTK_UnsafePointer && PTK != PTK_UnsafeMutablePointer) {
    DLOG("Ignoring sized_by on Autoreleasing pointer\n");
    CONDITIONAL_ASSERT(PTK == PTK_AutoreleasingUnsafeMutablePointer);
    return false;
  }
  // We have a pointer to a type with a size. Verify that it is char-sized.
  auto PtrT = CAT->getAs<clang::PointerType>();
  auto PointeeT = PtrT->getPointeeType();
  bool isByteSized = ctx.getTypeSizeInChars(PointeeT).isOne();
  if (!isByteSized)
    DLOG("Ignoring sized_by on non-byte-sized pointer\n");
  return isByteSized;
}

struct SwiftifyInfoPrinter {
  static const ssize_t SELF_PARAM_INDEX = -2;
  static const ssize_t RETURN_VALUE_INDEX = -1;
  clang::ASTContext &ctx;
  ASTContext &SwiftContext;
  llvm::raw_svector_ostream &out;
  MacroDecl &SwiftifyImportDecl;
  bool firstParam = true;
  llvm::StringMap<std::string> &typeMapping;
  bool &DiagnosedMissingNullableAsEmptySpanParam;
  bool hasNullableCountedBy = false;

protected:
  SwiftifyInfoPrinter(clang::ASTContext &ctx, ASTContext &SwiftContext,
                      llvm::raw_svector_ostream &out,
                      MacroDecl &SwiftifyImportDecl,
                      llvm::StringMap<std::string> &typeMapping,
                      bool &DiagnosedMissingNullableAsEmptySpanParam)
      : ctx(ctx), SwiftContext(SwiftContext), out(out),
        SwiftifyImportDecl(SwiftifyImportDecl), typeMapping(typeMapping),
        DiagnosedMissingNullableAsEmptySpanParam(
            DiagnosedMissingNullableAsEmptySpanParam) {}

public:
  void printTypeMapping() {
    printSeparator();
    out << "typeMappings: [";
    if (typeMapping.empty()) {
      out << ":]";
      return;
    }
    llvm::interleaveComma(typeMapping, out, [&](const auto &entry) {
      out << '"' << entry.getKey() << "\" : \"" << entry.getValue() << '"';
    });
    out << "]";
  }

  void printAvailability() {
    if (!hasMacroParameter("spanAvailability"))
      return;

    ValueDecl *D = getKnownSingleDecl(SwiftContext, "Span");
    const SemanticAvailableAttributes availabilityAttrs =
        D->getSemanticAvailableAttrs(/*includingInactive=*/true);
    if (availabilityAttrs.empty())
      return; // don't print availability when targeting embedded

    printSeparator();
    out << "spanAvailability: ";
    out << "\"";
    llvm::SaveAndRestore<bool> hasAvailbilitySeparatorRestore(firstParam, true);
    for (auto attr : availabilityAttrs) {
      auto platform = attr.getPlatform();
      if (!platform) continue;
      auto introducedOpt = attr.getIntroduced();
      if (!introducedOpt.has_value()) continue;
      printSeparator();
      out << prettyPlatformString(*platform) << " " << introducedOpt.value();
    }
    out << "\"";
  }

protected:
  bool hasMacroParameter(StringRef ParamName) const {
    for (auto *Param : *SwiftifyImportDecl.parameterList)
      if (Param->getArgumentName().str() == ParamName)
        return true;
    return false;
  }

  void printSeparator() {
    if (!firstParam) {
      out << ", ";
    } else {
      firstParam = false;
    }
  }
};

struct SwiftifyInfoFunctionPrinter : public SwiftifyInfoPrinter {
  SwiftifyInfoFunctionPrinter(clang::ASTContext &ctx, ASTContext &SwiftContext,
                              llvm::raw_svector_ostream &out,
                              MacroDecl &SwiftifyImportDecl,
                              llvm::StringMap<std::string> &typeMapping,
                              bool &DiagnosedMissingNullableAsEmptySpanParam)
      : SwiftifyInfoPrinter(ctx, SwiftContext, out, SwiftifyImportDecl,
                            typeMapping,
                            DiagnosedMissingNullableAsEmptySpanParam) {}

  bool printCountedBy(const clang::CountAttributedType *CAT, Type swiftType,
                      ssize_t pointerIndex, bool isImplicitlyUnwrapped) {
    // Step 1: check if we support this attribute
    bool isSizedBy = CAT->isCountInBytes();
    if (isSizedBy ? !SwiftifiableSizedByPointerType(ctx, swiftType, CAT)
                  : ConcretePointeeType(swiftType).isNull())
      return false;
    SwiftCountExprEmitter emitter(ctx);
    if (!emitter.Visit(CAT->getCountExpr()))
      return false;

    // Step 2: print - any early exit must occur before this point
    printSeparator();
    out << ".";
    if (isSizedBy)
      out << "sizedBy";
    else
      out << "countedBy";
    if (CAT->isOrNull() && hasOrNullSupport())
      out << "OrNull";
    out << "(pointer: ";
    printParamOrReturn(pointerIndex);
    out << ", ";
    out << (isSizedBy ? "size" : "count");
    out << ": \"" << emitter.str() << "\")";
    if (!CAT->isOrNull() && swiftType->isOptional() && !isImplicitlyUnwrapped)
      hasNullableCountedBy = true;
    return true;
  }

  void printNonEscaping(int idx) {
    printSeparator();
    out << ".nonescaping(pointer: ";
    printParamOrReturn(idx);
    out << ")";
  }

  void printLifetimeboundReturn(int idx, bool borrow) {
    printSeparator();
    out << ".lifetimeDependence(dependsOn: ";
    printParamOrReturn(idx);
    out << ", pointer: .return, type: ";
    out << (borrow ? ".borrow" : ".copy");
    out << ")";
  }

  bool registerStdSpanTypeMapping(Type swiftType, const clang::QualType clangType) {
    if (isStdSpanType(clangType)) {
      typeMapping.try_emplace(swiftType->getString(),
                              swiftType->getDesugaredType()->getString());
      return true;
    }
    return false;
  }

  void printNullableAsEmptySpan() {
    if (!hasMacroParameter("nullableAsEmptySpan")) {
      if (DiagnosedMissingNullableAsEmptySpanParam ||
          // Don't warn when it has no impact on the result.
          !hasNullableCountedBy)
        return;
      DiagnosedMissingNullableAsEmptySpanParam = true;
      SwiftContext.Diags.diagnose(
          SourceLoc(), diag::swiftify_nullable_as_empty_span_param_missing);
      return;
    }
    printSeparator();
    out << "nullableAsEmptySpan: true";
  }

private:
  void printParamOrReturn(ssize_t pointerIndex) {
    if (pointerIndex == SELF_PARAM_INDEX)
      out << ".self";
    else if (pointerIndex == RETURN_VALUE_INDEX)
      out << ".return";
    else
      out << ".param(" << pointerIndex + 1 << ")";
  }

  std::optional<bool> hasOrNullSupportCached = std::nullopt;
  bool hasOrNullSupport() {
    if (hasOrNullSupportCached.has_value())
      return hasOrNullSupportCached.value();

    auto *D = getKnownSingleDecl(SwiftContext, "_SwiftifyInfo");
    auto *Enum = dyn_cast_or_null<EnumDecl>(D);
    if (!Enum)
      return false;
    for (auto *Element :
         Enum->lookupDirect(SwiftContext.getIdentifier("countedByOrNull"))) {
      if (isa<EnumElementDecl>(Element)) {
        hasOrNullSupportCached = true;
        return true;
      }
    }
    hasOrNullSupportCached = false;
    return false;
  }
};

// Searches for template instantiations that are not behind type aliases.
// FIXME: make sure the generated code compiles for template
// instantiations that are not behind type aliases.
struct UnaliasedInstantiationVisitor
    : clang::RecursiveASTVisitor<UnaliasedInstantiationVisitor> {
  bool hasUnaliasedInstantiation = false;

  bool TraverseTypedefType(const clang::TypedefType *, bool TraverseQualifier) {
    return true;
  }

  bool
  VisitTemplateSpecializationType(const clang::TemplateSpecializationType *) {
    hasUnaliasedInstantiation = true;
    DLOG("Signature contains raw template, skipping\n");
    return false;
  }

  bool VisitRecordType(const clang::RecordType *RT) {
    if (isa_and_nonnull<clang::ClassTemplateSpecializationDecl>(
            RT->getDecl())) {
      hasUnaliasedInstantiation = true;
      DLOG("Signature contains raw template, skipping\n");
      return false;
    }
    return true;
  }

  static bool checkTemplates(clang::QualType clangType, bool hasLifetime,
                             bool isStdSpan) {
    if (hasLifetime && isStdSpan) {
      // std::span is transformed to Swift Span, so the std::span template
      // instantiation won't show up in the macro expansion's signature. The
      // element type still needs to be checked.
      auto getTemplateArg = [](clang::QualType Ty) {
        const auto *STTPT = Ty->getAs<clang::SubstTemplateTypeParmType>();
        if (!STTPT)
          return clang::QualType();
        const auto *RT =
            dyn_cast<clang::RecordType>(STTPT->getReplacementType());
        if (!RT)
          return clang::QualType();
        const auto *CD =
            dyn_cast<clang::ClassTemplateSpecializationDecl>(RT->getDecl());
        if (!CD)
          return clang::QualType();
        auto Args = CD->getTemplateArgs().asArray();
        return Args[0].getAsType();
      };
      if (const auto *TST =
              clangType->getAs<clang::TemplateSpecializationType>())
        clangType = TST->template_arguments()[0].getAsType();
      else if (clang::QualType ArgTy = getTemplateArg(clangType);
               !ArgTy.isNull()) {
        clangType = ArgTy;
      } else {
        assert(0 && "unknown std::span representation");
        return true;
      }
    }
    UnaliasedInstantiationVisitor checker;
    checker.TraverseType(clangType);
    return checker.hasUnaliasedInstantiation;
  }
};

static const clang::Decl *getTemplateInstantiation(const clang::Decl *D) {
  if (auto FuncD = dyn_cast<clang::FunctionDecl>(D)) {
    return FuncD->getTemplateInstantiationPattern();
  }
  if (auto RecordD = dyn_cast<clang::CXXRecordDecl>(D)) {
    return RecordD->getTemplateInstantiationPattern();
  }
  if (auto EnumD = dyn_cast<clang::EnumDecl>(D)) {
    return EnumD->getTemplateInstantiationPattern();
  }
  if (auto VarD = dyn_cast<clang::VarDecl>(D)) {
    return VarD->getTemplateInstantiationPattern();
  }
  return nullptr;
}

static clang::Module *getOwningModule(const clang::Decl *ClangDecl) {
  std::optional<clang::Module *> M;
  if (const auto *Instance = getTemplateInstantiation(ClangDecl)) {
    M = importer::getClangSubmoduleForDecl(Instance, true);
  } else {
    M = importer::getClangSubmoduleForDecl(ClangDecl, true);
  }
  if (M) {
    // the inner value can be null, so flatten it
    return M.value();
  }
  return nullptr;
}

struct ForwardDeclaredConcreteTypeVisitor : public TypeWalker {
  bool hasForwardDeclaredConcreteType = false;
  const clang::Module *Owner;

  explicit ForwardDeclaredConcreteTypeVisitor(const clang::Module *Owner)
      : Owner(Owner){};

  Action walkToTypePre(Type ty) override {
    DLOG("Walking type:\n");
    LLVM_DEBUG(DUMP(ty));

    auto *Nom = ty->getAnyNominal();
    if (!Nom) {
      return Action::Continue;
    }

    const clang::Decl *ClangDecl = Nom->getClangDecl();
    if (!ClangDecl) {
      return Action::Continue;
    }

    auto TD = dyn_cast<clang::TagDecl>(ClangDecl);
    if (!TD) {
      return Action::Continue;
    }

    const clang::Module *M = getOwningModule(ClangDecl);
    if (!M) {
      DLOG("Concrete type is in bridging header, which is always imported\n");
      return Action::Continue;
    }

    if (!Owner) {
      hasForwardDeclaredConcreteType = true;
      DLOG("Imported signature contains concrete type not available in bridging header, skipping\n");
      if (const clang::TagDecl *Def = TD->getDefinition())
        LLVM_DEBUG(DUMP(Def));
      return Action::Stop;
    }
    if (!Owner->isModuleVisible(M)) {
      hasForwardDeclaredConcreteType = true;
      DLOG("Imported signature contains concrete type not available in clang module, skipping\n");
      if (const clang::TagDecl *Def = TD->getDefinition())
        LLVM_DEBUG(DUMP(Def));
      return Action::Stop;
    }

    return Action::Continue;
  }

  bool IsIncompatibleImport(Type SwiftTy, clang::QualType ClangTy) {
    DLOG_SCOPE("Checking compatibility of type: " << ClangTy << "\n");
    SwiftTy.walk(*this);
    return hasForwardDeclaredConcreteType;
  }
};

// until CountAttributedType::getAttributeName lands in our LLVM branch
static StringRef getAttributeName(const clang::CountAttributedType *CAT) {
  switch (CAT->getKind()) {
    case clang::CountAttributedType::CountedBy:
      return "__counted_by";
    case clang::CountAttributedType::CountedByOrNull:
      return "__counted_by_or_null";
    case clang::CountAttributedType::SizedBy:
      return "__sized_by";
    case clang::CountAttributedType::SizedByOrNull:
      return "__sized_by_or_null";
    case clang::CountAttributedType::EndedBy:
      llvm_unreachable("CountAttributedType cannot be ended_by");
  }
}

static bool wouldBeIllegalInitializer(const AbstractFunctionDecl *MappedDecl) {
  if (!isa<ConstructorDecl>(MappedDecl))
    return false;
  const auto *Parent = MappedDecl->getParent();
  if (const auto *Ext = dyn_cast<ExtensionDecl>(Parent)) {
    Parent = Ext->getExtendedNominal();
  }
  const auto *ParentClass = dyn_cast<ClassDecl>(Parent);
  if (!ParentClass)
    return false;

  return ParentClass->getForeignClassKind() != ClassDecl::ForeignKind::Normal;
}

template<typename T>
static bool getImplicitObjectParamAnnotation(const clang::ObjCMethodDecl* D) {
    return false; // Only C++ methods have implicit params
}

static bool shouldSkipModule(ModuleDecl *M) {
  if (M->isClangBridgingHeaderImportModule()) {
    DLOG("is from bridging header (or C++ namespace)\n");
    return false;
  }

  if (M->getImplicitImportInfo().StdlibKind != ImplicitStdlibKind::Stdlib) {
    DLOG("module " << M->getNameStr() << " does not import stdlib\n");
    return true;
  }

  return false;
}
} // namespace

template<typename T>
static bool swiftifyImpl(ClangImporter::Implementation &Self,
                         SwiftifyInfoFunctionPrinter &printer,
                         const AbstractFunctionDecl *MappedDecl,
                         const T *ClangDecl) {
  DLOG_SCOPE("Checking '" << *ClangDecl << "' for bounds and lifetime info\n");

  if (hasSwiftAttribute(ClangDecl, {"no_safe_wrapper"})) {
    DLOG("skipping function with no_safe_wrapper\n");
    return false;
  }

  if (shouldSkipModule(MappedDecl->getParentModule()))
    return false;

  // FIXME: for private macro generated functions we do not serialize the
  // SILFunction's body anywhere triggering assertions.
  if (ClangDecl->getAccess() == clang::AS_protected ||
      ClangDecl->getAccess() == clang::AS_private)
    return false;

  if (ClangDecl->isImplicit()) {
    DLOG("implicit functions lack lifetime and bounds info\n");
    return false;
  }

  const clang::Module *OwningModule = getOwningModule(ClangDecl);
  bool IsInBridgingHeader = MappedDecl->getModuleContext()->isClangBridgingHeaderImportModule();
  ASSERT(OwningModule || IsInBridgingHeader);
  ForwardDeclaredConcreteTypeVisitor CheckForwardDecls(OwningModule);

  if (wouldBeIllegalInitializer(MappedDecl)) {
    DLOG("illegal initializer\n");
    return false;
  }

  // We only attach the macro if it will produce an overload. Any __counted_by
  // will produce an overload, since UnsafeBufferPointer is still an improvement
  // over UnsafePointer, but std::span will only produce an overload if it also
  // has lifetime information, since std::span already contains bounds info.
  bool attachMacro = false;
  {

    auto isNonEscapable = [&Self](clang::QualType ty) {
      // We only care whether it's _known_ ~Escapable, because it affects
      // lifetime info requirements.
      return evaluateOrDefault(Self.SwiftContext.evaluator,
                               ClangTypeEscapability({ty.getTypePtr(), &Self}),
                               CxxEscapability::Escapable) ==
             CxxEscapability::NonEscapable;
    };

    auto dependsOnClass = [](const ParamDecl *fromParam) {
      return fromParam->getInterfaceType()->isAnyClassReferenceType();
    };
    clang::QualType clangReturnTy = ClangDecl->getReturnType();
    bool returnIsStdSpan = isStdSpanType(clangReturnTy);
    auto *CAT = clangReturnTy->getAs<clang::CountAttributedType>();
    bool returnHasBoundsInfo = returnIsStdSpan || CAT != nullptr;
    bool returnValueIsNonEscapable = isNonEscapable(clangReturnTy);
    bool returnValueCanBeNonEscapable = returnValueIsNonEscapable || returnHasBoundsInfo;
    bool returnHasLifetimeInfo = false;
    if (getImplicitObjectParamAnnotation<clang::LifetimeBoundAttr>(ClangDecl)) {
      DLOG("Found lifetimebound attribute on implicit 'this'\n");
      if (Self.SwiftContext.LangOpts.hasFeature(Feature::SafeInteropWrappers)) {
        if (!dependsOnClass(
                MappedDecl->getImplicitSelfDecl(/*createIfNeeded*/ true))) {
          if (returnValueCanBeNonEscapable) {
            printer.printLifetimeboundReturn(
                SwiftifyInfoPrinter::SELF_PARAM_INDEX, true);
            returnHasLifetimeInfo = true;
          } else {
            DLOG("lifetimebound ignored because return value is escapable");
          }
        } else {
          DLOG("lifetimebound ignored because it depends on class with "
               "refcount\n");
        }
      } else {
        DLOG("lifetimebound not yet supported by stable feature-set - "
             "skipping\n");
        return false;
      }
    }

    bool isClangInstanceMethod =
        (isa<clang::CXXMethodDecl>(ClangDecl) &&
         !isa<clang::CXXConstructorDecl>(ClangDecl) &&
         cast<clang::CXXMethodDecl>(ClangDecl)->isInstance()) ||
        (isa<clang::ObjCMethodDecl>(ClangDecl) &&
         cast<clang::ObjCMethodDecl>(ClangDecl)->isInstanceMethod());

    size_t swiftNumParams = MappedDecl->getParameters()->size();
    if (MappedDecl->isInstanceMember() && !isClangInstanceMethod) {
      ASSERT(MappedDecl->isImportAsInstanceMember());
      swiftNumParams += 1;
    }
    if (ClangDecl->param_size() != swiftNumParams) {
      DLOG("mismatching parameter lists");
      assert(
          ClangDecl->isVariadic() ||
          MappedDecl->getForeignErrorConvention().has_value() ||
          MappedDecl->getForeignAsyncConvention().has_value() ||
          (swiftNumParams == 1 &&
           MappedDecl->getParameters()->get(0)->getInterfaceType()->isVoid()));
      return false;
    }

    size_t selfParamIndex = MappedDecl->isImportAsInstanceMember()
                                ? MappedDecl->getSelfIndex()
                                : ClangDecl->param_size();
    for (auto [index, clangParam] : llvm::enumerate(ClangDecl->parameters())) {
      clang::QualType clangParamTy = clangParam->getType();
      DLOG_SCOPE("Checking parameter '" << *clangParam << "' with type '"
                                        << clangParamTy << "'\n");
      int mappedIndex = index < selfParamIndex ? index :
        index > selfParamIndex ? index - 1 :
        SwiftifyInfoPrinter::SELF_PARAM_INDEX;
      const ParamDecl *swiftParam = nullptr;
      if (mappedIndex == SwiftifyInfoPrinter::SELF_PARAM_INDEX) {
        swiftParam = MappedDecl->getImplicitSelfDecl(/*createIfNeeded*/true);
      } else {
        swiftParam = MappedDecl->getParameters()->get(mappedIndex);
      }
      ASSERT(swiftParam);
      Type swiftParamTy = swiftParam->getInterfaceType();

      if (CheckForwardDecls.IsIncompatibleImport(swiftParamTy, clangParamTy))
        return false;

      bool paramHasBoundsInfo = false;
      auto *CAT = clangParamTy->getAs<clang::CountAttributedType>();
      if (CAT && mappedIndex == SwiftifyInfoPrinter::SELF_PARAM_INDEX) {
        Self.diagnose(HeaderLoc(clangParam->getLocation()),
                 diag::warn_clang_ignored_bounds_on_self, getAttributeName(CAT));
        auto swiftName = ClangDecl->template getAttr<clang::SwiftNameAttr>();
        ASSERT(swiftName &&
               "free function mapped to instance method without swift_name??");
        Self.diagnose(HeaderLoc(swiftName->getLocation()),
                 diag::note_swift_name_instance_method);
      } else if (CAT && printer.printCountedBy(
                            CAT, swiftParamTy, mappedIndex,
                            swiftParam->isImplicitlyUnwrappedOptional())) {
        DLOG("Found bounds info '" << clangParamTy << "'\n");
        attachMacro = paramHasBoundsInfo = true;
      }
      bool paramIsStdSpan =
          printer.registerStdSpanTypeMapping(swiftParamTy, clangParamTy);
      paramHasBoundsInfo |= paramIsStdSpan;

      bool paramHasLifetimeInfo = false;
      if (clangParam->template hasAttr<clang::NoEscapeAttr>()) {
        DLOG("Found noescape attribute\n");
        printer.printNonEscaping(mappedIndex);
        paramHasLifetimeInfo = true;
      }
      if (clangParam->template hasAttr<clang::LifetimeBoundAttr>()) {
        if (Self.SwiftContext.LangOpts.hasFeature(
                Feature::SafeInteropWrappers)) {
          DLOG("Found lifetimebound attribute\n");
          if (!dependsOnClass(swiftParam)) {
            if (returnValueCanBeNonEscapable) {
              // If this parameter has bounds info we will tranform it into a
              // Span, so then it will no longer be Escapable.
              bool willBeEscapable =
                  !isNonEscapable(clangParamTy) &&
                  (!paramHasBoundsInfo ||
                   mappedIndex == SwiftifyInfoPrinter::SELF_PARAM_INDEX);
              printer.printLifetimeboundReturn(mappedIndex, willBeEscapable);
              paramHasLifetimeInfo = true;
              returnHasLifetimeInfo = true;
            } else {
              DLOG("lifetimebound ignored because return value is escapable\n");
            }
          } else {
            DLOG("lifetimebound ignored because it depends on class with "
                 "refcount\n");
          }
        } else {
          DLOG("lifetimebound not yet supported by stable feature-set - skipping\n");
          return false;
        }
      }
      if (UnaliasedInstantiationVisitor::checkTemplates(
              clangParamTy, paramHasLifetimeInfo, paramIsStdSpan)) {
        return false;
      }
      if (paramIsStdSpan && paramHasLifetimeInfo) {
        DLOG("Found both std::span and lifetime info\n");
        attachMacro = true;
      }
    }
    if (!returnHasLifetimeInfo && returnValueIsNonEscapable) {
      DLOG("~Escapable return value without lifetime info\n");
      return false;
    }

    if (UnaliasedInstantiationVisitor::checkTemplates(
            clangReturnTy, returnHasLifetimeInfo, returnIsStdSpan)) {
      return false;
    }
    if (returnIsStdSpan && returnHasLifetimeInfo) {
      DLOG("Found both std::span and lifetime info for return value\n");
      attachMacro = true;
    }

    Type swiftReturnTy;
    if (const auto *funcDecl = dyn_cast<FuncDecl>(MappedDecl))
      swiftReturnTy = funcDecl->getResultInterfaceType();
    else if (const auto *ctorDecl = dyn_cast<ConstructorDecl>(MappedDecl))
      swiftReturnTy = ctorDecl->getResultInterfaceType();
    else
      ABORT("Unexpected AbstractFunctionDecl subclass.");

    if (CheckForwardDecls.IsIncompatibleImport(swiftReturnTy, clangReturnTy))
      return false;
    (void)printer.registerStdSpanTypeMapping(
        swiftReturnTy, clangReturnTy);
    if (CAT && printer.printCountedBy(
                   CAT, swiftReturnTy, SwiftifyInfoPrinter::RETURN_VALUE_INDEX,
                   MappedDecl->isImplicitlyUnwrappedOptional())) {
      DLOG("Found bounds info '" << clang::QualType(CAT, 0)
                                 << "' on return value\n");
      attachMacro = true;
    }
  }
  return attachMacro;
}

static bool diagnoseMissingMacroPlugin(ASTContext &SwiftContext,
                                       StringRef MacroName,
                                       Decl *MappedDecl) {
  ExternalMacroDefinitionRequest request{
      &SwiftContext, SwiftContext.getIdentifier("SwiftMacros"),
      SwiftContext.getIdentifier(MacroName)};
  auto externalDef =
      evaluateOrDefault(SwiftContext.evaluator, request,
                        ExternalMacroDefinition::error("failed request"));
  if (externalDef.isError()) {
    auto &diags = SwiftContext.Diags;
    auto didSuppressWarnings = diags.getSuppressWarnings();
    // We are highly likely parsing a textual interface, where warnings are
    // silenced. Make sure this warning gets emitted anyways.
    diags.setSuppressWarnings(false);
    SWIFT_DEFER { diags.setSuppressWarnings(didSuppressWarnings); };
    diags.diagnose(MappedDecl, diag::macro_on_import_not_loadable, MacroName);
    return true;
  }

  return false;
}

void ClangImporter::Implementation::swiftify(AbstractFunctionDecl *MappedDecl) {
  if (SwiftContext.LangOpts.DisableSafeInteropWrappers)
    return;
  const clang::Decl *ClangDecl = MappedDecl->getClangDecl();

  if (ClangDecl && ClangDecl->isImplicit()) {
    if (auto *F = dyn_cast<FuncDecl>(MappedDecl)) {
      if (const FuncDecl *Orig = getOriginalForVirtualThunk(F)) {
        DLOG("Remapping virtual thunk to original clang decl\n");
        ClangDecl = Orig->getClangDecl();
      }
    }
  }

  auto ClangFuncDecl = dyn_cast_or_null<clang::FunctionDecl>(ClangDecl);
  auto ClangObjCMethodDecl = dyn_cast_or_null<clang::ObjCMethodDecl>(ClangDecl);
  if (!ClangFuncDecl && !ClangObjCMethodDecl)
    return;
  ASSERT(!ClangFuncDecl || !ClangObjCMethodDecl);

  if (isa<ProtocolDecl>(MappedDecl->getParent()))
    return;

  MacroDecl *SwiftifyImportDecl = dyn_cast_or_null<MacroDecl>(getKnownSingleDecl(SwiftContext, "_SwiftifyImport"));
  if (!SwiftifyImportDecl) {
    DLOG("_SwiftifyImport macro not found\n");
    return;
  }

  // A method that overrides a virtual method needs no wrapper of its own if it
  // inherits one: the base class wrapper calls the virtual method, so it already
  // dispatches to this override, and a second wrapper here would only add an
  // overload that cannot be resolved against the inherited one.
  //
  // The wrapper is only inherited when the C++ base class is imported as the
  // Swift superclass. Reached any other way - through a value type base, or a
  // base that is not the primary one - the base class wrapper is cloned rather
  // than inherited and cannot be called, so this override does need its own. Ask
  // for the primary superclass in Clang terms rather than looking at the Swift
  // superclass, which is not necessarily set up yet while importing a member.
  if (auto *CxxMethod = dyn_cast<clang::CXXMethodDecl>(ClangDecl)) {
    auto primarySuperclassOf = [&](const clang::CXXRecordDecl *Record) {
      return evaluateOrDefault(SwiftContext.evaluator,
                               ForeignReferenceTypeInfoRequest({Record}), {})
          .getPrimarySuperclass();
    };
    if (CxxMethod->size_overridden_methods() > 0) {
      llvm::SmallPtrSet<const clang::CXXRecordDecl *, 16> SuperClasses;
      for (auto *Super = primarySuperclassOf(CxxMethod->getParent()); Super;
           Super = primarySuperclassOf(Super)) {
        SuperClasses.insert(Super->getCanonicalDecl());
      }
      if (SuperClasses.size() > 0) {
        for (auto *Overridden : CxxMethod->overridden_methods()) {
          if (SuperClasses.count(Overridden->getParent()->getCanonicalDecl())) {
            // FIXME: We should still generate a safe wrapper if the superclass
            // is missing one, or if this one would produce a different
            // signature.
            DLOG("Inherits the wrapper of an overridden virtual method, which "
                 "dispatches here\n");
            return;
          }
        }
      }
    }
  }

  // For projects adopting SafeInteropWrappers we preserve the original
  // Optional-propagating signature unless they opt-in to the new one.
  const bool LegacyOptionalRequested =
      SwiftContext.LangOpts.hasFeature(Feature::SafeInteropWrappers) &&
      !SwiftContext.LangOpts.hasFeature(
          Feature::SafeInteropWrappersNullAsEmptySpan);

  llvm::SmallString<128> MacroString;
  {
    llvm::raw_svector_ostream out(MacroString);
    out << "@_SwiftifyImport(";

    llvm::StringMap<std::string> typeMapping;
    SwiftifyInfoFunctionPrinter printer(
        getClangASTContext(), SwiftContext, out, *SwiftifyImportDecl,
        typeMapping, DiagnosedMissingNullableAsEmptySpanParam);
    bool foundInfo = ClangFuncDecl ?
      swiftifyImpl(*this, printer, MappedDecl, ClangFuncDecl) :
      swiftifyImpl(*this, printer, MappedDecl, ClangObjCMethodDecl);
    if (!foundInfo) {
      DLOG("No relevant bounds or lifetime info found\n");
      return;
    }
    printer.printAvailability();
    printer.printTypeMapping();
    if (!LegacyOptionalRequested) {
      printer.printNullableAsEmptySpan();
    }
    out << ")";
  }

  if (diagnoseMissingMacroPlugin(SwiftContext, "_SwiftifyImport", MappedDecl))
    return;

  DLOG("Attaching safe interop macro: " << MacroString << "\n");
  if (const clang::RawComment *raw =
          getClangASTContext().getRawCommentForAnyRedecl(ClangDecl)) {
    // swift::RawDocCommentAttr doesn't contain its text directly, but instead
    // references the source range of the parsed comment. Instead of creating
    // a new source file just to parse the doc comment, we can add the
    // comment to the macro invocation attribute, which the macro has access
    // to. Waiting until we know that the macro will be attached before
    // emitting the comment to the string, despite the comment occurring
    // first, avoids copying a bunch of potentially long comments for nodes
    // that don't end up with wrappers.
    auto commentString =
        raw->getRawText(getClangASTContext().getSourceManager());
    importNontrivialAttribute(MappedDecl,
                              (commentString + "\n" + MacroString).str());
  } else {
    importNontrivialAttribute(MappedDecl, MacroString);
  }
}

namespace {
using namespace swift;

struct UnswiftifyParamInfo {
  /// Swift source for the bound expression (an element count for
  /// `__counted_by`, a byte size for `__sized_by`).
  std::string boundsExprSource;
  /// Has a `__counted_by` / `__sized_by` annotation.
  bool hasBoundsAnnotation = false;
  /// The bound is a byte size rather than an element count.
  bool sizedBy = false;
  /// Has the `_or_null` variant.
  bool orNull = false;
  /// Construct a (Mutable)(Raw)Span rather than a buffer pointer.
  bool nonescaping = false;
  /// Referenced purely as another parameter's bound, so dropped from the
  /// forwarding call.
  bool isElidedBoundsParam = false;
};

/// If \p expr (ignoring implicit casts) is a reference to one of \p clangFD's
/// parameters, return that parameter's index. Any parameter identified by this
/// function will be dropped from the safe signature.
std::optional<unsigned>
getReferencedParamIndex(const clang::Expr *expr,
                        const clang::FunctionDecl *clangFD) {
  expr = expr->IgnoreImpCasts();
  const auto *declRef = dyn_cast<clang::DeclRefExpr>(expr);
  if (!declRef)
    return std::nullopt;
  const auto *param = dyn_cast<clang::ParmVarDecl>(declRef->getDecl());
  if (!param)
    return std::nullopt;
  // The count expression's `DeclRefExpr` may name the parameter of a different
  // redeclaration (prototype vs. definition) than `clangFD`. Parameter indices
  // are stable across redeclarations, so verify the owner is a redeclaration of
  // `clangFD` and use that redecl's index.
  const auto *owner = dyn_cast<clang::FunctionDecl>(param->getDeclContext());
  if (!owner || owner->getCanonicalDecl() != clangFD->getCanonicalDecl())
    return std::nullopt;
  return param->getFunctionScopeIndex();
}

/// The classification of a (possibly optional) pointer type.
struct PointerClassification {
  Type pointee;   // Element type for typed pointers; null for raw/non-pointer.
  bool isMutable; // True for `UnsafeMutable*Pointer` families.
};

/// Peel an optional (`?`/`!`) pointer type and classify the underlying pointer.
PointerClassification classifyPointerType(Type ty) {
  if (Type object = ty->getOptionalObjectType())
    ty = object;
  PointerTypeKind ptk;
  Type pointee = ty->getAnyPointerElementType(ptk);
  if (!pointee)
    return {Type(), /*isMutable=*/false};
  bool isMutable = false;
  switch (ptk) {
  case PTK_UnsafeMutablePointer:
  case PTK_AutoreleasingUnsafeMutablePointer:
  case PTK_UnsafeMutableRawPointer:
    isMutable = true;
    break;
  case PTK_UnsafePointer:
  case PTK_UnsafeRawPointer:
    isMutable = false;
    break;
  }
  // Raw pointers report the empty tuple as their "element"; treat that as
  // having no pointee for the purposes of `Span<Pointee>()`.
  if (pointee->isVoid())
    return {Type(), isMutable};
  return {pointee, isMutable};
}

/// The buffer/span type the swiftify transform maps an annotated pointer
/// to, and the information necessary to construct one.
struct SafeBufferSpelling {
  StringRef name;       // "Span", "UnsafeBufferPointer", ...
  StringRef initLabel;  // "start" or "_unsafeStart".
  StringRef countLabel; // "count" or "byteCount".
  Type pointee;         // empty if RawSpan/RawBufferPointer
  /// The pointer is an `OpaquePointer`, which the raw buffer initializers do
  /// not accept directly; it has to be converted to an `UnsafeRawPointer`.
  bool castToRawPointer = false;

  /// Select the type for a parameter based on its bounds flags and mutability.
  static SafeBufferSpelling forParam(const UnswiftifyParamInfo &info,
                                     Type pty) {
    auto [pointee, isMutable] = classifyPointerType(pty);
    bool span = info.nonescaping;
    bool raw = info.sizedBy;

    // Each (span, raw) combination maps to a distinct type family; within a
    // family, mutability picks the concrete spelling.
    StringRef name;
    if (span && raw)
      name = isMutable ? "MutableRawSpan" : "RawSpan";
    else if (span)
      name = isMutable ? "MutableSpan" : "Span";
    else if (raw)
      name = isMutable ? "UnsafeMutableRawBufferPointer"
                       : "UnsafeRawBufferPointer";
    else
      name = isMutable ? "UnsafeMutableBufferPointer" : "UnsafeBufferPointer";

    StringRef initLabel = span ? "_unsafeStart" : "start";
    // Only a RawSpan deals in absolute bytes.
    StringRef countLabel = (span && raw) ? "byteCount" : "count";

    Type unwrapped = pty;
    if (Type object = unwrapped->getOptionalObjectType())
      unwrapped = object;
    bool isOpaque = raw && unwrapped->getAnyNominal() ==
                               pty->getASTContext().getOpaquePointerDecl();
    return {name, initLabel, countLabel, raw ? Type() : pointee, isOpaque};
  }

  /// Emit the initializer expression producing the safe value from a pointer
  /// and an already-`Int`-typed count expression, e.g.
  /// `unsafe Span(_unsafeStart: x, count: _count0)`.
  void printInitializer(llvm::raw_ostream &os, const llvm::Twine &ptrExpr,
                        const llvm::Twine &countExpr) const {
    os << "unsafe " << name << "(" << initLabel << ": ";
    // `UnsafeRawPointer.init?(_: OpaquePointer?)` preserves optionality.
    if (castToRawPointer)
      os << "UnsafeRawPointer(" << ptrExpr << ")";
    else
      os << ptrExpr;
    os << ", " << countLabel << ": " << countExpr << ")";
  }

  void printTypeName(llvm::raw_ostream &os) const {
    os << name;
    if (pointee)
      os << "<" << pointee << ">";
  }
};

/// One argument of the forwarding call the generated peer makes to the safe
/// original: its argument label, the Swift source spelling to pass, whether it
/// is passed `inout`, and whether its type is unsafe (so the call needs
/// `unsafe`).
struct CallArg {
  Identifier label;
  std::string name;
  bool isInout;
  /// The forwarded value has an unsafe type (a pointer / buffer pointer), so
  /// the forwarding call must be marked `unsafe`.
  bool isUnsafe;

  CallArg(Identifier label, std::string name, bool isInout,
          bool isUnsafe = false)
      : label(label), name(std::move(name)), isInout(isInout),
        isUnsafe(isUnsafe) {}
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os, const CallArg &arg) {
  if (!arg.label.empty())
    os << arg.label.str() << ": ";
  if (arg.isInout)
    os << "&";
  os << arg.name;
  return os;
}

/// Emit a pre-call `let`/`var` binding that constructs the bufferpointer/span
/// for a single annotated unsafe pointer parameter, returning the name of the
/// local variable to pass at the call site.
std::string emitSafeValueBinding(const UnswiftifyParamInfo &info,
                                 const ParamDecl *targetParam, Type safeType,
                                 std::string bindingName, std::string countName,
                                 bool isInout, llvm::raw_ostream &os) {
  DLOG_SCOPE("Binding safe buffer\n");
  std::string pointerName = escapeSwiftIdentifier(targetParam->getName());
  Type pty = targetParam->getInterfaceType();
  StringRef bindKeyword = isInout ? "var" : "let";

  SafeBufferSpelling typeInfo = SafeBufferSpelling::forParam(info, pty);

  bool isOptionalPointer = !pty->getOptionalObjectType().isNull();

  // The count comes straight from the C caller. Bind it as an `Int` and
  // validate it (`>= 0`) before constructing any buffer: an UnsafeBufferPointer
  // with a negative count is UB.
  //
  // For a `_or_null` pointer, however, a null pointer carries no buffer and its
  // count is not meaningful (the C contract permits it to be negative). Clamp
  // the count to 0 in that case so the null branch constructs an empty
  // collection and the precondition only constrains the non-null case.
  os << "    let " << countName << " = ";
  if (info.orNull && isOptionalPointer) {
    os << "(";
    if (pty->isUnsafe())
      os << "unsafe ";
    os << pointerName << " != nil) ? Int(" << info.boundsExprSource
       << ") : 0\n";
    os << "    precondition(" << countName
      << " >= 0, \"non-null buffer with negative count\")\n";
  } else {
    os << "Int(" << info.boundsExprSource << ")\n";
    os << "    precondition(" << countName
       << " >= 0, \"buffer with negative count\")\n";
    if (isOptionalPointer)
      os << "    precondition(unsafe " << pointerName
         << " != nil || " << countName
         << " == 0, \"null buffer with non-zero count\")\n";
  }
  StringRef countExpr = countName;

  bool safeTypeIsOptional =
      safeType && !safeType->getOptionalObjectType().isNull();
  bool generateSpan = info.nonescaping;

  // Bind argument to a local variable even when not strictly necessary.
  // This simplifies generating the forwarding call.
  os << "    " << bindKeyword << " " << bindingName;
  if (isOptionalPointer && safeTypeIsOptional) {
    // Nullable pointer, Optional safe value: a null pointer maps to `nil`.
    if (generateSpan) {
      os << ": ";
      typeInfo.printTypeName(os);
      os << "? = if unsafe " << pointerName << " != nil { ";
      typeInfo.printInitializer(os, pointerName + "!", countExpr);
      os << " } else { nil }\n";
    } else {
      os << " = unsafe " << pointerName << ".map { ";
      typeInfo.printInitializer(os, "$0", countExpr);
      os << " }\n";
    }
  } else if (isOptionalPointer && generateSpan) {
    // Nullable pointer, non-Optional Span: a null pointer maps to an empty span.
    os << " = if unsafe " << pointerName << " != nil { ";
    typeInfo.printInitializer(os, pointerName + "!", countExpr);
    os << " } else { ";
    typeInfo.printTypeName(os);
    os << "() }\n";
  } else {
    // The remaining cases need no nil-guard: either the pointer is
    // non-nullable, or it is a nullable buffer pointer whose own initializer
    // accepts a null base (mapping null to an empty buffer).
    os << " = ";
    typeInfo.printInitializer(os, pointerName, countExpr);
    os << "\n";
  }

  return bindingName;
}

/// Derive per-parameter bounds/nonescaping info by walking \p clangFD into
/// \p infos, and flag the parameters referenced purely as another parameter's
/// count (which are dropped from the forwarding call). \p infos is indexed by
/// clang parameter position (`clangFD->getNumParams()` entries).
void deriveParamInfo(const clang::FunctionDecl *clangFD,
                     llvm::SmallVectorImpl<UnswiftifyParamInfo> &infos) {
  DLOG_SCOPE("Deriving parameter info\n");
  infos.assign(clangFD->getNumParams(), UnswiftifyParamInfo());
  clang::ASTContext &clangCtx = clangFD->getASTContext();
  for (auto [index, clangParam] : llvm::enumerate(clangFD->parameters())) {
    UnswiftifyParamInfo &info = infos[index];
    clang::QualType clangParamTy = clangParam->getType();
    if (const auto *CAT = clangParamTy->getAs<clang::CountAttributedType>()) {
      SwiftCountExprEmitter emitter(clangCtx);
      if (emitter.Visit(CAT->getCountExpr())) {
        info.hasBoundsAnnotation = true;
        info.sizedBy = CAT->isCountInBytes();
        info.orNull = CAT->isOrNull();
        info.boundsExprSource = emitter.str().str();
        if (auto countIndex =
                getReferencedParamIndex(CAT->getCountExpr(), clangFD);
            countIndex && *countIndex < infos.size())
          infos[*countIndex].isElidedBoundsParam = true;
      }
    }
    if (clangParam->hasAttr<clang::NoEscapeAttr>())
      info.nonescaping = true;
  }
}

/// Emit the generated peer's `@c @implementation func NAME(PARAMS) -> RET {`
/// header, inheriting access from the safe original \p safeDecl and taking the
/// signature \p targetParams of the imported C entry point.
void emitPeerSignature(llvm::raw_ostream &os,
                       const AbstractFunctionDecl *safeDecl,
                       const AbstractFunctionDecl *importedAFD,
                       const ParameterList *targetParams, Type resultTy) {
  DLOG_SCOPE("Emitting peer signature\n");
  os << "@c @implementation\n";
  if (StringRef access = getAccessLevelSpelling(safeDecl->getFormalAccess());
      !access.empty())
    os << access << " ";
  os << "func ";
  printSwiftIdentifier(importedAFD->getBaseIdentifier(), os);
  os << "(";
  llvm::interleaveComma(*targetParams, os, [&os](auto *param) {
    os << "_ ";
    printSwiftIdentifier(param->getName(), os,
                         PrintNameContext::FunctionParameterLocal);
    os << ": " << param->getInterfaceType();
  });
  os << ")";
  if (resultTy)
    os << " -> " << resultTy;
  os << " {\n";
}

/// Emit the pre-call bindings that reconstruct each safe value (Span,
/// UnsafeBufferPointer, ...) from the unsafe pointer/count pair, collecting the
/// arguments to forward into \p callArgs. Returns false if the safe
/// original's parameters cannot be reconciled with the imported unsafe
/// signature for some reason (this shouldn't happen).
bool emitArgumentBindings(llvm::raw_ostream &os,
                          const ParameterList *targetParams,
                          const ParameterList *sourceParams,
                          ArrayRef<UnswiftifyParamInfo> infos,
                          llvm::SmallVectorImpl<CallArg> &callArgs) {
  DLOG_SCOPE("Emitting argument bindings\n");
  llvm::StringSet<> reservedNames;
  for (auto *param : *targetParams)
    reservedNames.insert(param->getName().str());
  auto uniqueLocalName = [&](const llvm::Twine &base) -> std::string {
    std::string candidate = base.str();
    // It's quite unlikely that we clash with one of the human written parameter
    // names, so just appending '_' until we no longer clash should be fine.
    while (!reservedNames.insert(candidate).second)
      candidate += "_";
    return candidate;
  };
  auto assertExpr = [](bool pred) -> bool {
    assert(pred && "mismatching signature");
    return pred;
  };

  // Dense index for the locals we actually emit. It advances only when a
  // binding is written. N.B: the generated `_safeArgN`/`_countN` names have no
  // gaps (plainly-forwarded parameters bind no local).
  unsigned localIdx = 0;
  unsigned callArgIdx = 0;
  // Walk the unsafe parameters in order (skipping elided count parameters),
  // construct the appropriate safe(r) buffer type for each annotated pointer
  // parameter, and collect the variable name to forward.
  for (auto [index, param] : llvm::enumerate(*targetParams)) {
    DLOG_SCOPE("Emitting '" << param->getParameterName() << "'\n");
    const UnswiftifyParamInfo &info = infos[index];
    if (info.isElidedBoundsParam)
      continue;
    if (!assertExpr(callArgIdx <= sourceParams->size()))
      return false;
    const ParamDecl *sourceParam = sourceParams->get(callArgIdx);
    bool isInout = sourceParam->isInOut();
    Identifier label = sourceParam->getArgumentName();
    if (info.hasBoundsAnnotation) {
      Type srcType = sourceParam->getInterfaceType();
      callArgs.emplace_back(
          label,
          emitSafeValueBinding(
              info, param, srcType,
              uniqueLocalName("_safeArg" + llvm::Twine(localIdx)),
              uniqueLocalName("_count" + llvm::Twine(localIdx)), isInout, os),
          isInout, /*isUnsafe=*/!info.nonescaping);
      ++localIdx;
    } else {
      assert(!isInout); // plain C params should never be 'inout'
      callArgs.emplace_back(label, escapeSwiftIdentifier(param->getName()),
                            false, param->getInterfaceType()->isUnsafe());
    }
    ++callArgIdx;
  }

  return assertExpr(sourceParams->size() == callArgs.size());
}

void emitForwardingCall(llvm::raw_ostream &os, ArrayRef<CallArg> callArgs,
                        const AbstractFunctionDecl *safeDecl,
                        const AbstractFunctionDecl *importedAFD,
                        Type resultTy) {
  bool forwardsUnsafe =
      safeDecl->getExplicitSafety() == ExplicitSafety::Unsafe ||
      (resultTy && resultTy->isUnsafe()) ||
      llvm::any_of(callArgs, [](const CallArg &arg) { return arg.isUnsafe; });
  os << "    " << (resultTy ? "return " : "")
     << (forwardsUnsafe ? "unsafe " : "");
  printSwiftIdentifier(importedAFD->getBaseIdentifier(), os);
  os << "(";
  llvm::interleaveComma(callArgs, os);
  os << ")\n}";
}

/// The compiler-internal implementation of the `_Unswiftify` macro. Given an
/// `@c @implementation` Swift function, it synthesizes a C-callable
/// (`@c @implementation`) peer with the unsafe signature of the imported C
/// entry point, forwarding to the safe original by constructing safe(r) values
/// (Span, UnsafeBufferPointer, ...) from each unsafe pointer/count pair.
class UnswiftifyMacro : public InternalMacro {
public:
  std::string expandAttached(ASTContext &ctx, Decl *attachedTo) const override;
};

#ifndef NDEBUG
static StringRef tryGetName(const Decl *D, SmallVectorImpl<char> &scratch) {
  if (!D)
    return "<null>";
  if (const auto *VD = dyn_cast<ValueDecl>(D)) {
    DeclName Name = VD->getName();
    if (Name.isSpecial())
      return "<special-name>";
    return VD->getName().getString(scratch);
  }
  return "<not-a-named-decl>";
}
#endif

std::string UnswiftifyMacro::expandAttached(ASTContext &ctx,
                                            Decl *attachedTo) const {
#ifndef NDEBUG
  SmallVector<char, 0> scratch;
#endif
  DLOG_SCOPE("Expanding @_Unswiftify for " << tryGetName(attachedTo, scratch) << "\n");
  // Every bail-out below means we committed to synthesizing a C entry point
  // (attachUnswiftifyMacroIfNeeded attached this macro) but then could not.
  // Returning an empty expansion would silently drop the C symbol and surface
  // only as a link error, so each failure should emit some error message.
  auto cannotLower = [&](Decl *at) -> std::string {
    if (auto *vd = dyn_cast_or_null<ValueDecl>(at))
      ctx.Diags.diagnose(vd->getLoc(), diag::implementation_safe_cannot_lower,
                         vd);
    return "";
  };

  auto *safeDecl = dyn_cast<AbstractFunctionDecl>(attachedTo);
  if (!safeDecl) {
    DLOG("Safe decl is not a function\n");
    return cannotLower(attachedTo);
  }

  // Fetch the unsafe signature directly from the safe overload: its imported
  // clang counterpart is the unsafe C entry point we must expose.
  auto *importedAFD = dyn_cast_or_null<AbstractFunctionDecl>(
      safeDecl->getImplementedObjCDecl());
  if (!importedAFD) {
    DLOG("Cannot find original C function\n");
    return cannotLower(safeDecl);
  }
  const auto *clangFD =
      dyn_cast_or_null<clang::FunctionDecl>(importedAFD->getClangDecl());
  if (!clangFD) {
    DLOG("Cannot find clang decl\n");
    return cannotLower(safeDecl);
  }

  auto *importedFD = dyn_cast<FuncDecl>(importedAFD);
  if (!importedFD) {
    DLOG("Imported decl is not a normal function\n");
    return cannotLower(safeDecl);
  }

  auto *targetParams = importedAFD->getParameters();
  auto *sourceParams = safeDecl->getParameters();

  // The forwarding loop below indexes `infos` (clang parameter domain) by the
  // imported Swift parameter position. The signatures always align for a plain
  // top level C function, but a `swift_name`/`swift_error` remapping could make
  // them diverge in the future when ObjC or C++ support is added.
  if (targetParams->size() != clangFD->getNumParams()) {
    DLOG("Mismatching parameter lists between Swift and clang decls\n");
    return cannotLower(safeDecl);
  }

  llvm::SmallVector<UnswiftifyParamInfo, 16> infos;
  deriveParamInfo(clangFD, infos);

  Type resultTy = importedFD->getResultInterfaceType();
  if (resultTy && resultTy->isVoid())
    resultTy = Type();

  // Emit directly into the final buffer, in source order.
  std::string result;
  llvm::raw_string_ostream os(result);
  emitPeerSignature(os, safeDecl, importedAFD, targetParams, resultTy);
  llvm::SmallVector<CallArg, 16> callArgs;
  if (!emitArgumentBindings(os, targetParams, sourceParams, infos, callArgs)) {
    DLOG("Could not bind arguments\n");
    return cannotLower(safeDecl);
  }
  emitForwardingCall(os, callArgs, safeDecl, importedAFD, resultTy);
  return result;
}

} // namespace

static constexpr llvm::StringLiteral UnswiftifyMacroName = "_Unswiftify";

MacroDecl *ClangImporter::Implementation::createUnswiftifyMacroDecl(
    DeclName introducedName) {
  ASTContext &ctx = SwiftContext;
  DeclContext *dc = ctx.getStdlibModule();
  if (!dc)
    dc = ctx.MainModule;

  auto *macro = new (ctx) MacroDecl(
      /*macroLoc=*/SourceLoc(),
      DeclName(ctx.getIdentifier(UnswiftifyMacroName)),
      /*nameLoc=*/SourceLoc(), /*genericParams=*/nullptr,
      /*parameterList=*/nullptr, /*arrowLoc=*/SourceLoc(),
      /*resultType=*/nullptr, /*definition=*/nullptr, dc);
  macro->setImplicit();

  // `@attached(peer, names: named(<introducedName>))`: the peer is an unsafe
  // overload of the safe original whose compound name matches the C interface
  // (e.g. `foo(_:_:)` taking pointer and count), which may differ in arity from
  // the safe original (e.g. `foo(_:)` taking a Span).
  // This is important because @implementation processing does a compound name
  // lookup, which won't trigger expansion of the macro based on the attached
  // decl's compound name since it doesn't match (the base name is the same).
  // Simply declaring `overloaded` wouldn't work - I'm not entirely convinced
  // that isn't just because of a macro expansion bug, but this works.
  MacroIntroducedDeclName named =
      MacroIntroducedDeclName::getNamed(introducedName);
  auto *roleAttr = MacroRoleAttr::create(
      ctx, /*atLoc=*/SourceLoc(), /*range=*/SourceRange(),
      MacroSyntax::Attached,
      /*lParenLoc=*/SourceLoc(), MacroRole::Peer, named,
      /*conformances=*/{}, /*rParenLoc=*/SourceLoc(), /*implicit=*/true);
  macro->getAttrs().add(roleAttr);

  static UnswiftifyMacro unswiftifyMacro;
  macro->setDefinition(MacroDefinition::forInternal(&unswiftifyMacro));

  return macro;
}

void ClangImporter::Implementation::attachUnswiftifyForSafeImplementation(
    AbstractFunctionDecl *safeSwiftDecl, DeclName introducedPeerName) {
  // `_Unswiftify` is never declared in source. Synthesize the internal macro
  // decl, then let the generic AST plumbing attach it and register its
  // introduced names.
  MacroDecl *macro = createUnswiftifyMacroDecl(introducedPeerName);
  safeSwiftDecl->attachInternalMacro(macro, MacroRole::Peer);

  DLOG("Attached @_Unswiftify to '" << safeSwiftDecl->getNameStr() << "'\n");
}
