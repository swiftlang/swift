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
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Module.h"

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

struct SwiftifyInfoPrinter {
  static const ssize_t SELF_PARAM_INDEX = -2;
  static const ssize_t RETURN_VALUE_INDEX = -1;
  clang::ASTContext &ctx;
  ASTContext &SwiftContext;
  llvm::raw_svector_ostream &out;
  MacroDecl &SwiftifyImportDecl;
  bool firstParam = true;
  llvm::StringMap<std::string> &typeMapping;

protected:
  SwiftifyInfoPrinter(clang::ASTContext &ctx, ASTContext &SwiftContext,
                      llvm::raw_svector_ostream &out,
                      MacroDecl &SwiftifyImportDecl,
                      llvm::StringMap<std::string> &typeMapping)
      : ctx(ctx), SwiftContext(SwiftContext), out(out),
        SwiftifyImportDecl(SwiftifyImportDecl), typeMapping(typeMapping) {}

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
    printSeparator();
    out << "spanAvailability: ";
    printAvailabilityOfType("Span");
  }
private:
  bool hasMacroParameter(StringRef ParamName) {
    for (auto *Param : *SwiftifyImportDecl.parameterList)
      if (Param->getArgumentName().str() == ParamName)
        return true;
    return false;
  }

  void printAvailabilityOfType(StringRef Name) {
    ValueDecl *D = getKnownSingleDecl(SwiftContext, Name);
    out << "\"";
    llvm::SaveAndRestore<bool> hasAvailbilitySeparatorRestore(firstParam, true);
    for (auto attr : D->getSemanticAvailableAttrs(/*includingInactive=*/true)) {
      auto introducedOpt = attr.getIntroduced();
      if (!introducedOpt.has_value()) continue;
      printSeparator();
      out << prettyPlatformString(attr.getPlatform()) << " " << introducedOpt.value();
    }
    out << "\"";
  }

protected:
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
                      llvm::StringMap<std::string> &typeMapping)
      : SwiftifyInfoPrinter(ctx, SwiftContext, out, SwiftifyImportDecl, typeMapping) {}

  void printCountedBy(const clang::CountAttributedType *CAT,
                      ssize_t pointerIndex) {
    printSeparator();
    clang::Expr *countExpr = CAT->getCountExpr();
    bool isSizedBy = CAT->isCountInBytes();
    out << ".";
    if (isSizedBy)
      out << "sizedBy";
    else
      out << "countedBy";
    out << "(pointer: ";
    printParamOrReturn(pointerIndex);
    out << ", ";
    if (isSizedBy)
      out << "size";
    else
      out << "count";
    out << ": \"";
    countExpr->printPretty(
        out, {}, {ctx.getLangOpts()}); // TODO: map clang::Expr to Swift Expr
    out << "\")";
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
    const auto *decl = clangType->getAsTagDecl();
    if (decl && decl->isInStdNamespace() && decl->getName() == "span") {
      typeMapping.insert(std::make_pair(
          swiftType->getString(), swiftType->getDesugaredType()->getString()));
      return true;
    }
    return false;
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
};

struct CountedByExpressionValidator
    : clang::ConstStmtVisitor<CountedByExpressionValidator, bool> {
  bool VisitDeclRefExpr(const clang::DeclRefExpr *e) { return true; }

  bool VisitIntegerLiteral(const clang::IntegerLiteral *IL) {
    switch (IL->getType()->castAs<clang::BuiltinType>()->getKind()) {
    case clang::BuiltinType::Char_S:
    case clang::BuiltinType::Char_U:
    case clang::BuiltinType::UChar:
    case clang::BuiltinType::SChar:
    case clang::BuiltinType::Short:
    case clang::BuiltinType::UShort:
    case clang::BuiltinType::UInt:
    case clang::BuiltinType::Long:
    case clang::BuiltinType::ULong:
    case clang::BuiltinType::LongLong:
    case clang::BuiltinType::ULongLong:
      DLOG("Ignoring count parameter with non-portable integer literal\n");
      return false;
    default:
      return true;
    }
  }

  bool VisitImplicitCastExpr(const clang::ImplicitCastExpr *c) {
    return this->Visit(c->getSubExpr());
  }
  bool VisitParenExpr(const clang::ParenExpr *p) {
    return this->Visit(p->getSubExpr());
  }

#define SUPPORTED_UNOP(UNOP) \
  bool VisitUnary ## UNOP(const clang::UnaryOperator *unop) { \
    return this->Visit(unop->getSubExpr()); \
  }
  SUPPORTED_UNOP(Plus)
  SUPPORTED_UNOP(Minus)
  SUPPORTED_UNOP(Not)
#undef SUPPORTED_UNOP

#define SUPPORTED_BINOP(BINOP) \
  bool VisitBin ## BINOP(const clang::BinaryOperator *binop) { \
    return this->Visit(binop->getLHS()) && this->Visit(binop->getRHS()); \
  }
  SUPPORTED_BINOP(Add)
  SUPPORTED_BINOP(Sub)
  SUPPORTED_BINOP(Div)
  SUPPORTED_BINOP(Mul)
  SUPPORTED_BINOP(Rem)
  SUPPORTED_BINOP(Shl)
  SUPPORTED_BINOP(Shr)
  SUPPORTED_BINOP(And)
  SUPPORTED_BINOP(Xor)
  SUPPORTED_BINOP(Or)
#undef SUPPORTED_BINOP

  bool VisitStmt(const clang::Stmt *) {
    DLOG("Ignoring count parameter with unsupported expression\n");
    return false;
  }
};


static Type ConcretePointeeType(Type swiftType) {
  Type nonnullType = swiftType->lookThroughSingleOptionalType();
  PointerTypeKind PTK;
  Type PointeeTy = nonnullType->getAnyPointerElementType(PTK);
  if (PointeeTy && (PTK == PTK_UnsafePointer || PTK == PTK_UnsafeMutablePointer))
    return PointeeTy;
  return Type();
}

// Don't try to transform any Swift types that _SwiftifyImport doesn't know how
// to handle.
static bool SwiftifiableSizedByPointerType(const clang::ASTContext &ctx,
                                           Type swiftType,
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
static bool SwiftifiableCAT(const clang::ASTContext &ctx,
                            const clang::CountAttributedType *CAT,
                            Type swiftType) {
  return CAT && CountedByExpressionValidator().Visit(CAT->getCountExpr()) &&
    (CAT->isCountInBytes() ?
       SwiftifiableSizedByPointerType(ctx, swiftType, CAT)
     : !ConcretePointeeType(swiftType).isNull());
}

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

  ForwardDeclaredConcreteTypeVisitor(const clang::Module *Owner) : Owner(Owner) {};

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

    const clang::TagDecl *Def = TD->getDefinition();
    ASSERT(Def && "concrete type without type definition?");
    const clang::Module *M = getOwningModule(ClangDecl);

    if (!M) {
      DLOG("Concrete type is in bridging header, which is always imported\n");
      return Action::Continue;
    }

    if (!Owner) {
      hasForwardDeclaredConcreteType = true;
      DLOG("Imported signature contains concrete type not available in bridging header, skipping\n");
      LLVM_DEBUG(DUMP(Def));
      return Action::Stop;
    }
    if (!Owner->isModuleVisible(M)) {
      hasForwardDeclaredConcreteType = true;
      DLOG("Imported signature contains concrete type not available in clang module, skipping\n");
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

static size_t getNumParams(const clang::ObjCMethodDecl* D) {
    return D->param_size();
}
static size_t getNumParams(const clang::FunctionDecl* D) {
    return D->getNumParams();
}
} // namespace

template<typename T>
static bool swiftifyImpl(ClangImporter::Implementation &Self,
                         SwiftifyInfoFunctionPrinter &printer,
                         const AbstractFunctionDecl *MappedDecl,
                         const T *ClangDecl) {
  DLOG_SCOPE("Checking '" << *ClangDecl << "' for bounds and lifetime info\n");

  // FIXME: for private macro generated functions we do not serialize the
  // SILFunction's body anywhere triggering assertions.
  if (ClangDecl->getAccess() == clang::AS_protected ||
      ClangDecl->getAccess() == clang::AS_private)
    return false;

  if (ClangDecl->isImplicit()) {
    DLOG("implicit functions lack lifetime and bounds info\n");
    return false;
  }

  clang::ASTContext &clangASTContext = Self.getClangASTContext();

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
    Type swiftReturnTy;
    if (const auto *funcDecl = dyn_cast<FuncDecl>(MappedDecl))
      swiftReturnTy = funcDecl->getResultInterfaceType();
    else if (const auto *ctorDecl = dyn_cast<ConstructorDecl>(MappedDecl))
      swiftReturnTy = ctorDecl->getResultInterfaceType();
    else
      ABORT("Unexpected AbstractFunctionDecl subclass.");
    clang::QualType clangReturnTy = ClangDecl->getReturnType();

    if (CheckForwardDecls.IsIncompatibleImport(swiftReturnTy, clangReturnTy))
      return false;

    auto isNonEscapable = [&Self](clang::QualType ty) {
      // We only care whether it's _known_ ~Escapable, because it affects
      // lifetime info requirements.
      return evaluateOrDefault(Self.SwiftContext.evaluator,
                               ClangTypeEscapability({ty.getTypePtr(), &Self}),
                               CxxEscapability::Escapable) ==
             CxxEscapability::NonEscapable;
    };

    bool returnIsStdSpan = printer.registerStdSpanTypeMapping(
        swiftReturnTy, clangReturnTy);
    auto *CAT = clangReturnTy->getAs<clang::CountAttributedType>();
    if (SwiftifiableCAT(clangASTContext, CAT, swiftReturnTy)) {
      printer.printCountedBy(CAT, SwiftifyInfoPrinter::RETURN_VALUE_INDEX);
      DLOG("Found bounds info '" << clang::QualType(CAT, 0) << "' on return value\n");
      attachMacro = true;
    }
    auto dependsOnClass = [](const ParamDecl *fromParam) {
      return fromParam->getInterfaceType()->isAnyClassReferenceType();
    };
    bool returnHasLifetimeInfo = false;
    if (getImplicitObjectParamAnnotation<clang::LifetimeBoundAttr>(ClangDecl)) {
      DLOG("Found lifetimebound attribute on implicit 'this'\n");
      if (!dependsOnClass(
              MappedDecl->getImplicitSelfDecl(/*createIfNeeded*/ true))) {
        printer.printLifetimeboundReturn(SwiftifyInfoPrinter::SELF_PARAM_INDEX,
                                         true);
        returnHasLifetimeInfo = true;
      } else {
        DLOG("lifetimebound ignored because it depends on class with refcount\n");
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
      assert(ClangDecl->isVariadic() ||
             MappedDecl->getForeignErrorConvention().has_value() ||
             MappedDecl->getForeignAsyncConvention().has_value());
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
      } else if (SwiftifiableCAT(clangASTContext, CAT, swiftParamTy)) {
        printer.printCountedBy(CAT, mappedIndex);
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
        DLOG("Found lifetimebound attribute\n");
        if (!dependsOnClass(swiftParam)) {
          // If this parameter has bounds info we will tranform it into a Span,
          // so then it will no longer be Escapable.
          bool willBeEscapable =
              !isNonEscapable(clangParamTy) &&
              (!paramHasBoundsInfo ||
               mappedIndex == SwiftifyInfoPrinter::SELF_PARAM_INDEX);
          printer.printLifetimeboundReturn(mappedIndex, willBeEscapable);
          paramHasLifetimeInfo = true;
          returnHasLifetimeInfo = true;
        } else {
          DLOG("lifetimebound ignored because it depends on class with refcount\n");
        }
      }
      if (paramIsStdSpan && paramHasLifetimeInfo) {
        DLOG("Found both std::span and lifetime info\n");
        attachMacro = true;
      }
    }
    if (returnIsStdSpan && returnHasLifetimeInfo) {
      DLOG("Found both std::span and lifetime info for return value\n");
      attachMacro = true;
    }
  }
  return attachMacro;
}

class SwiftifyProtocolInfoPrinter : public SwiftifyInfoPrinter {
private:
  ClangImporter::Implementation &ImporterImpl;

public:
  SwiftifyProtocolInfoPrinter(ClangImporter::Implementation &ImporterImpl,
                              clang::ASTContext &ctx, ASTContext &SwiftContext,
                              llvm::raw_svector_ostream &out,
                              MacroDecl &SwiftifyImportDecl,
                              llvm::StringMap<std::string> &typeMapping)
      : SwiftifyInfoPrinter(ctx, SwiftContext, out, SwiftifyImportDecl, typeMapping),
        ImporterImpl(ImporterImpl) {}

  bool printMethod(const FuncDecl *Method) {
    auto ClangDecl = dyn_cast_or_null<clang::ObjCMethodDecl>(Method->getClangDecl());
    if (!ClangDecl)
      return false;

    llvm::SmallString<128> paramInfoString;
    llvm::raw_svector_ostream tmpOut(paramInfoString);

    SwiftifyInfoFunctionPrinter methodPrinter(ctx, SwiftContext, tmpOut,
                                              SwiftifyImportDecl, typeMapping);
    bool hadAttributes = swiftifyImpl(ImporterImpl, methodPrinter, Method, ClangDecl);
    if (hadAttributes) {
      printSeparator();
      out << ".method(signature: \"";
      printMethodSignature(Method);
      out << "\", paramInfo: [" << paramInfoString << "])";
    }
    return hadAttributes;
  }

private:
  void printMethodSignature(const FuncDecl *Method) {
    auto options =
        PrintOptions::printForDiagnostics(AccessLevel::Private, true);
    for (const auto *attr : Method->getAttrs()) {
      options.ExcludeAttrList.push_back(attr->getKind());
    }
    StreamPrinter printer(out);
    Method->print(printer, options);
  }
};

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
  if (!SwiftContext.LangOpts.hasFeature(Feature::SafeInteropWrappers))
    return;
  auto ClangDecl = dyn_cast_or_null<clang::FunctionDecl>(MappedDecl->getClangDecl());
  if (!ClangDecl)
    return;

  DLOG_SCOPE(
      "Checking '" << *ClangDecl << "' (imported as '"
                   << MappedDecl->getName().getBaseName().userFacingName()
                   << "')\n");

  if (shouldSkipModule(MappedDecl->getParentModule()))
    return;

  {
    UnaliasedInstantiationVisitor visitor;
    visitor.TraverseType(ClangDecl->getType());
    if (visitor.hasUnaliasedInstantiation)
      return;
  }

  MacroDecl *SwiftifyImportDecl = dyn_cast_or_null<MacroDecl>(getKnownSingleDecl(SwiftContext, "_SwiftifyImport"));
  if (!SwiftifyImportDecl) {
    DLOG("_SwiftifyImport macro not found\n");
    return;
  }

  llvm::SmallString<128> MacroString;
  {
    llvm::raw_svector_ostream out(MacroString);
    out << "@_SwiftifyImport(";

    llvm::StringMap<std::string> typeMapping;
    SwiftifyInfoFunctionPrinter printer(getClangASTContext(), SwiftContext, out,
                                        *SwiftifyImportDecl, typeMapping);
    if (!swiftifyImpl(*this, printer, MappedDecl, ClangDecl)) {
      DLOG("No relevant bounds or lifetime info found\n");
      return;
    }
    printer.printAvailability();
    printer.printTypeMapping();
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

