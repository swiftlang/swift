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
#include "swift/AST/Import.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangImporterRequests.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Module.h"
#include "clang/Sema/Overload.h"
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
    out << e->getDecl()->getName();
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
      auto introducedOpt = attr.getIntroduced();
      if (!introducedOpt.has_value()) continue;
      printSeparator();
      out << prettyPlatformString(attr.getPlatform()) << " " << introducedOpt.value();
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

  bool TraverseTypedefType(const clang::TypedefType *) { return true; }

  bool
  VisitTemplateSpecializationType(const clang::TemplateSpecializationType *) {
    hasUnaliasedInstantiation = true;
    DLOG("Signature contains raw template, skipping\n");
    return false;
  }

  static bool checkTemplates(clang::QualType clangType, bool hasLifetime,
                             bool isStdSpan) {
    if (hasLifetime && isStdSpan) {
      // std::span is transformed to Swift Span, so the std::span template
      // instantiation won't show up in the macro expansion's signature. The
      // element type still needs to be checked.
      const auto *TST = clangType->getAs<clang::TemplateSpecializationType>();
      ASSERT(TST && "std::span is not specialized?");
      clangType = TST->template_arguments()[0].getAsType();
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

static size_t getNumParams(const clang::FunctionDecl* D) {
    return D->getNumParams();
}

static bool shouldSkipModule(ModuleDecl *M) {
  if (M->getName().str() == CLANG_HEADER_MODULE_NAME) {
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
  bool IsInBridgingHeader = MappedDecl->getModuleContext()->getName().str() == CLANG_HEADER_MODULE_NAME;
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
    if (getNumParams(ClangDecl) != swiftNumParams) {
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
                                : getNumParams(ClangDecl);
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

    if (!attachMacro && CAT == nullptr)
      // The return type is not imported eagerly (unlike parameter types). Exit
      // early to avoid unnecessarily importing types we might not need.
      return false;

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
  auto ClangDecl = dyn_cast_or_null<clang::FunctionDecl>(MappedDecl->getClangDecl());
  if (!ClangDecl)
    return;

  MacroDecl *SwiftifyImportDecl = dyn_cast_or_null<MacroDecl>(getKnownSingleDecl(SwiftContext, "_SwiftifyImport"));
  if (!SwiftifyImportDecl) {
    DLOG("_SwiftifyImport macro not found\n");
    return;
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
    if (!swiftifyImpl(*this, printer, MappedDecl, ClangDecl)) {
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
  if (clang::RawComment *raw =
          getClangASTContext().getRawCommentForDeclNoCache(ClangDecl)) {
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

