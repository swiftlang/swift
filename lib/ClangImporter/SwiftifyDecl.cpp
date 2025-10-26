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
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/ParameterList.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/Type.h"

using namespace swift;
using namespace importer;

#define DEBUG_TYPE "safe-interop-wrappers"
#define DLOG(x) LLVM_DEBUG(llvm::dbgs() << x)

namespace {
ValueDecl *getKnownSingleDecl(ASTContext &SwiftContext, StringRef DeclName) {
  SmallVector<ValueDecl *, 1> decls;
  SwiftContext.lookupInSwiftModule(DeclName, decls);
  assert(decls.size() < 2);
  if (decls.size() != 1) return nullptr;
  return decls[0];
}

class SwiftifyInfoPrinter {
public:
  static const ssize_t SELF_PARAM_INDEX = -2;
  static const ssize_t RETURN_VALUE_INDEX = -1;
  clang::ASTContext &ctx;
  ASTContext &SwiftContext;
  llvm::raw_ostream &out;
  MacroDecl &SwiftifyImportDecl;
  bool firstParam = true;
  llvm::StringMap<std::string> typeMapping;

  SwiftifyInfoPrinter(clang::ASTContext &ctx, ASTContext &SwiftContext,
                      llvm::raw_ostream &out, MacroDecl &SwiftifyImportDecl)
      : ctx(ctx), SwiftContext(SwiftContext), out(out),
        SwiftifyImportDecl(SwiftifyImportDecl) {
    out << "(";
  }
  ~SwiftifyInfoPrinter() { out << ")"; }

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

  void printSeparator() {
    if (!firstParam) {
      out << ", ";
    } else {
      firstParam = false;
    }
  }

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
      // These integer literals are printed with a suffix that isn't valid Swift
      // syntax
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

  bool VisitStmt(const clang::Stmt *) { return false; }
};


// Don't try to transform any Swift types that _SwiftifyImport doesn't know how
// to handle.
static bool SwiftifiableCountedByPointerType(Type swiftType) {
  Type nonnullType = swiftType->lookThroughSingleOptionalType();
  PointerTypeKind PTK;
  return nonnullType->getAnyPointerElementType(PTK) &&
    (PTK == PTK_UnsafePointer || PTK == PTK_UnsafeMutablePointer);
}
static bool SwiftifiableSizedByPointerType(const clang::ASTContext &ctx,
                                           Type swiftType,
                                           const clang::CountAttributedType *CAT) {
  Type nonnullType = swiftType->lookThroughSingleOptionalType();
  if (nonnullType->isOpaquePointer())
    return true;
  PointerTypeKind PTK;
  if (!nonnullType->getAnyPointerElementType(PTK))
    return false;
  if (PTK == PTK_UnsafeRawPointer || PTK == PTK_UnsafeMutableRawPointer)
    return true;
  if (PTK != PTK_UnsafePointer && PTK != PTK_UnsafeMutablePointer)
    return false;
  // We have a pointer to a type with a size. Verify that it is char-sized.
  auto PtrT = CAT->getAs<clang::PointerType>();
  auto PointeeT = PtrT->getPointeeType();
  return ctx.getTypeSizeInChars(PointeeT).isOne();
}
static bool SwiftifiableCAT(const clang::ASTContext &ctx,
                            const clang::CountAttributedType *CAT,
                            Type swiftType) {
  return CAT && CountedByExpressionValidator().Visit(CAT->getCountExpr()) &&
    (CAT->isCountInBytes() ?
       SwiftifiableSizedByPointerType(ctx, swiftType, CAT)
     : SwiftifiableCountedByPointerType(swiftType));
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
    return false;
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
} // namespace

static bool swiftifyImpl(ClangImporter::Implementation &Self,
                         SwiftifyInfoPrinter &printer,
                         const AbstractFunctionDecl *MappedDecl,
                         const clang::FunctionDecl *ClangDecl) {
  DLOG("Checking " << *ClangDecl << " for bounds and lifetime info\n");

  // FIXME: for private macro generated functions we do not serialize the
  // SILFunction's body anywhere triggering assertions.
  if (ClangDecl->getAccess() == clang::AS_protected ||
      ClangDecl->getAccess() == clang::AS_private)
    return false;

  {
    UnaliasedInstantiationVisitor visitor;
    visitor.TraverseType(ClangDecl->getType());
    if (visitor.hasUnaliasedInstantiation)
      return false;
  }

  clang::ASTContext &clangASTContext = Self.getClangASTContext();

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
    bool returnIsStdSpan = printer.registerStdSpanTypeMapping(
        swiftReturnTy, ClangDecl->getReturnType());
    auto *CAT = ClangDecl->getReturnType()->getAs<clang::CountAttributedType>();
    if (SwiftifiableCAT(clangASTContext, CAT, swiftReturnTy)) {
      printer.printCountedBy(CAT, SwiftifyInfoPrinter::RETURN_VALUE_INDEX);
      DLOG("  Found bounds info '" << clang::QualType(CAT, 0) << "' on return value\n");
      attachMacro = true;
    }
    auto dependsOnClass = [](const ParamDecl *fromParam) {
      return fromParam->getInterfaceType()->isAnyClassReferenceType();
    };
    bool returnHasLifetimeInfo = false;
    if (getImplicitObjectParamAnnotation<clang::LifetimeBoundAttr>(ClangDecl)) {
      DLOG("  Found lifetimebound attribute on implicit 'this'\n");
      if (!dependsOnClass(
              MappedDecl->getImplicitSelfDecl(/*createIfNeeded*/ true))) {
        printer.printLifetimeboundReturn(SwiftifyInfoPrinter::SELF_PARAM_INDEX,
                                         true);
        returnHasLifetimeInfo = true;
      }
    }

    bool isClangInstanceMethod =
        isa<clang::CXXMethodDecl>(ClangDecl) &&
        !isa<clang::CXXConstructorDecl>(ClangDecl) &&
        cast<clang::CXXMethodDecl>(ClangDecl)->isInstance();
    size_t swiftNumParams = MappedDecl->getParameters()->size() -
                            (ClangDecl->isVariadic() ? 1 : 0);
    ASSERT((MappedDecl->isImportAsInstanceMember() == isClangInstanceMethod) ==
           (ClangDecl->getNumParams() == swiftNumParams));

    size_t selfParamIndex = MappedDecl->isImportAsInstanceMember()
                                ? MappedDecl->getSelfIndex()
                                : ClangDecl->getNumParams();
    for (auto [index, clangParam] : llvm::enumerate(ClangDecl->parameters())) {
      auto clangParamTy = clangParam->getType();
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
      bool paramHasBoundsInfo = false;
      auto *CAT = clangParamTy->getAs<clang::CountAttributedType>();
      if (CAT && mappedIndex == SwiftifyInfoPrinter::SELF_PARAM_INDEX) {
        Self.diagnose(HeaderLoc(clangParam->getLocation()),
                      diag::warn_clang_ignored_bounds_on_self,
                      getAttributeName(CAT));
        auto swiftName = ClangDecl->getAttr<clang::SwiftNameAttr>();
        ASSERT(swiftName &&
               "free function mapped to instance method without swift_name??");
        Self.diagnose(HeaderLoc(swiftName->getLocation()),
                      diag::note_swift_name_instance_method);
      } else if (SwiftifiableCAT(clangASTContext, CAT, swiftParamTy)) {
        printer.printCountedBy(CAT, mappedIndex);
        DLOG("  Found bounds info '" << clangParamTy << "' on parameter '"
                                     << *clangParam << "'\n");
        attachMacro = paramHasBoundsInfo = true;
      }
      bool paramIsStdSpan =
          printer.registerStdSpanTypeMapping(swiftParamTy, clangParamTy);
      paramHasBoundsInfo |= paramIsStdSpan;

      bool paramHasLifetimeInfo = false;
      if (clangParam->hasAttr<clang::NoEscapeAttr>()) {
        DLOG("  Found noescape attribute on parameter '" << *clangParam << "'\n");
        printer.printNonEscaping(mappedIndex);
        paramHasLifetimeInfo = true;
      }
      if (clangParam->template hasAttr<clang::LifetimeBoundAttr>()) {
        DLOG("  Found lifetimebound attribute on parameter '" << *clangParam
                                                              << "'\n");
        if (!dependsOnClass(swiftParam)) {
          // If this parameter has bounds info we will tranform it into a Span,
          // so then it will no longer be Escapable.
          bool willBeEscapable =
              swiftParamTy->isEscapable() &&
              (!paramHasBoundsInfo ||
               mappedIndex == SwiftifyInfoPrinter::SELF_PARAM_INDEX);
          printer.printLifetimeboundReturn(mappedIndex, willBeEscapable);
          paramHasLifetimeInfo = true;
          returnHasLifetimeInfo = true;
        }
      }
      if (paramIsStdSpan && paramHasLifetimeInfo) {
        DLOG("  Found both std::span and lifetime info for parameter '"
             << *clangParam << "'\n");
        attachMacro = true;
      }
    }
    if (returnIsStdSpan && returnHasLifetimeInfo) {
      DLOG("  Found both std::span and lifetime info for return value\n");
      attachMacro = true;
    }
  }
  return attachMacro;
}

void ClangImporter::Implementation::swiftify(AbstractFunctionDecl *MappedDecl) {
  if (!SwiftContext.LangOpts.hasFeature(Feature::SafeInteropWrappers) ||
      SwiftContext.ClangImporterOpts.DisableSafeInteropWrappers)
    return;
  auto ClangDecl = dyn_cast_or_null<clang::FunctionDecl>(MappedDecl->getClangDecl());
  if (!ClangDecl)
    return;

  MacroDecl *SwiftifyImportDecl = dyn_cast_or_null<MacroDecl>(getKnownSingleDecl(SwiftContext, "_SwiftifyImport"));
  if (!SwiftifyImportDecl)
    return;

  llvm::SmallString<128> MacroString;
  {
    llvm::raw_svector_ostream out(MacroString);
    out << "@_SwiftifyImport";

    SwiftifyInfoPrinter printer(getClangASTContext(), SwiftContext, out, *SwiftifyImportDecl);
    if (!swiftifyImpl(*this, printer, MappedDecl, ClangDecl))
      return;
    printer.printAvailability();
    printer.printTypeMapping();
  }

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

