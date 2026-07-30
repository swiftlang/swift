#include "ClangDerivedConformances.h"
#include "ImporterImpl.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

bool importer::hasImportReferenceAttr(const clang::RecordDecl *decl) {
  return hasSwiftAttribute(decl, {"import_reference"});
}

bool importer::hasImportAsOpaquePointerAttr(const clang::RecordDecl *decl) {
  return decl->hasAttrs() && llvm::any_of(decl->getAttrs(), [](auto *attr) {
           if (auto swiftAttr = dyn_cast<clang::SwiftAttrAttr>(attr))
             return swiftAttr->getAttribute() == "import_opaque_pointer";
           return false;
         });
}

//===----------------------------------------------------------------------===//
// Direct view analysis
//===----------------------------------------------------------------------===//

namespace {

/// Whether \p type is "self-contained" for the purpose of direct-view
/// inference: it is escapable (SWIFT_ESCAPABLE), a foreign reference type
/// (SWIFT_SHARED_REFERENCE / SWIFT_IMMORTAL_REFERENCE), or a record explicitly
/// annotated SWIFT_SELF_CONTAINED (import_owned). A record explicitly marked
/// unsafe is never self-contained, which also excludes
/// SWIFT_UNSAFE_REFERENCE.
bool isSelfContainedForDirectView(const clang::Type *type, Evaluator &eval) {
  type = type->getUnqualifiedDesugaredType();

  // A function (pointer) refers to code, and a pointer to member is an offset
  // rather than an address, so neither can dangle.
  if (type->isFunctionPointerType() || type->isFunctionType() ||
      type->isMemberPointerType())
    return true;

  if (const auto *recordType = type->getAs<clang::RecordType>()) {
    auto *definition = recordType->getDecl()->getDefinition();
    if (!definition)
      return false;
    // An explicitly unsafe type is never self-contained, so its unsafety is
    // not silently dropped by an enclosing view.
    if (importer::hasSwiftAttribute(definition, {"unsafe"}))
      return false;
    // Reference types are managed by Swift, so a pointer to one does not
    // introduce a lifetime dependency. Use the request rather than a raw
    // attribute lookup, so that reference-ness inherited from a base class is
    // taken into account.
    if (evaluateOrDefault(eval, ForeignReferenceTypeInfoRequest({definition}),
                          ForeignReferenceTypeInfo())
            .isReference())
      return true;
    if (importer::hasOwnedValueAttr(definition))
      return true;
  }

  return evaluateOrDefault(eval, ClangTypeEscapability({type, nullptr}),
                           CxxEscapability::Unknown) ==
         CxxEscapability::Escapable;
}

bool isDirectViewTypeImpl(const clang::Type *type, Evaluator &eval,
                          llvm::SmallDenseSet<const clang::Decl *, 4> &seen) {
  type = type->getUnqualifiedDesugaredType();

  // (A) A pointer or reference is a direct view if its pointee is
  // self-contained. Block and ObjC-object pointers are not "pointers into a
  // buffer of self-contained objects", so they are deliberately not matched
  // here.
  if (type->isPointerType() || type->isReferenceType()) {
    clang::QualType pointee = type->getPointeeType();
    // We do not know what is stored at the pointed-to memory, so a `void *`
    // cannot be a pointer into a buffer of self-contained objects.
    if (pointee->isFunctionType() || pointee->isVoidType())
      return false;
    return isSelfContainedForDirectView(pointee.getTypePtr(), eval);
  }

  // (B) A record is a direct view if every field and base is either
  // self-contained or itself a direct view. A record with no indirection at
  // all is treated as having a single level of indirection, so that a
  // non-escapable marker type (`struct SWIFT_NONESCAPABLE Token { long id; };`)
  // stays a direct view: it holds no pointer, so nothing in it can dangle.
  if (const auto *recordType = type->getAs<clang::RecordType>()) {
    auto *recordDecl = recordType->getDecl()->getDefinition();
    if (!recordDecl)
      return false;
    if (importer::hasSwiftAttribute(recordDecl, {"unsafe"}))
      return false;
    if (!seen.insert(recordDecl).second)
      return true;

    auto isSelfContainedOrDirectView = [&](clang::QualType t) {
      const clang::Type *ty = t.getTypePtr();
      return isSelfContainedForDirectView(ty, eval) ||
             isDirectViewTypeImpl(ty, eval, seen);
    };

    if (const auto *cxxRecordDecl =
            dyn_cast<clang::CXXRecordDecl>(recordDecl)) {
      for (auto base : cxxRecordDecl->bases())
        if (!isSelfContainedOrDirectView(base.getType()))
          return false;
    }
    for (auto *field : recordDecl->fields())
      if (!isSelfContainedOrDirectView(field->getType()))
        return false;
    return true;
  }

  // (C) Anything else is not itself a direct view.
  return false;
}

} // end anonymous namespace

bool importer::isDirectViewType(const clang::Type *type, Evaluator &eval) {
  llvm::SmallDenseSet<const clang::Decl *, 4> seen;
  return isDirectViewTypeImpl(type, eval, seen);
}

bool importer::isDirectViewType(const clang::Decl *decl, ASTContext &swiftCtx) {
  if (const auto *typeDecl = dyn_cast<clang::TypeDecl>(decl)) {
    clang::QualType type = typeDecl->getASTContext().getTypeDeclType(typeDecl);
    return isDirectViewType(type.getTypePtr(), swiftCtx.evaluator);
  }
  return false;
}

namespace {
class ForeignReferenceTypeChecker {
  /// We are checking this to determine whether it is a foreign reference type.
  const clang::CXXRecordDecl *checkedDecl;

  /// Used for emitting diagnostics.
  ClangImporter::Implementation *Impl = nullptr;

  /// Whether we encountered a non-record base during the base traversal.
  bool hasNonRecordBase = false;

  /// Base classes that are marked as FRTs. Populated by \c visitBases().
  llvm::SmallVector<const clang::CXXRecordDecl *, 1> FRTBases;

  /// Virtual bases, which we only need to visit once.
  llvm::SmallPtrSet<const clang::CXXRecordDecl *, 1> virtualBases;

  /// Recursively visits the bases of \p decl to accumulate FRT information.
  ///
  /// If exactly one base of \p decl leads to an annotated FRT base class, then
  /// this function returns a pointer to that direct base. Returns \c nullptr
  /// otherwise.
  const clang::CXXRecordDecl *visitBases(const clang::CXXRecordDecl *decl) {
    if (!decl->hasDefinition())
      // Without a definition, there's no inheritance info to check.
      return nullptr;

    const clang::CXXRecordDecl *singleFRTSuperclass = nullptr;
    bool multipleFRTSuperclasses = false;
    for (auto declBase : decl->bases()) {
      auto *base = declBase.getType()->getAsCXXRecordDecl();
      if (!base) {
        // It is possible to encounter `clang::TemplateSpecializationType`s.
        // In such cases, report this as invalid and continue past it.
        hasNonRecordBase = true;
        continue;
      }

      ASSERT(base->hasDefinition() && "base record should be complete");
      base = base->getDefinition();
      ASSERT(!base->isDependentContext() && "base should not be dependent");

      if (!declBase.isVirtual() || virtualBases.insert(base).second) {
        bool baseIsFRT;
        if (importer::hasImportReferenceAttr(base)) {
          FRTBases.push_back(base);
          baseIsFRT = true;
        } else {
          baseIsFRT = static_cast<bool>(visitBases(base));
        }

        if (baseIsFRT && !declBase.isVirtual() &&
            declBase.getAccessSpecifier() ==
                clang::AccessSpecifier::AS_public) {
          if (singleFRTSuperclass)
            multipleFRTSuperclasses = true;
          else
            singleFRTSuperclass = base;
        }
      }
    }

    return multipleFRTSuperclasses ? nullptr : singleFRTSuperclass;
  }

  /// Whether \p base is non-null and has an offset of zero from \c checkedDecl.
  bool isPresentAndAtOffsetZero(const clang::CXXRecordDecl *base) const {
    ASSERT(checkedDecl);
    if (base == nullptr)
      return false;
    auto &clangCtx = checkedDecl->getASTContext();
    auto &layout = clangCtx.getASTRecordLayout(checkedDecl);
    return layout.getBaseClassOffset(base).isZero();
  }

public:
  ForeignReferenceTypeChecker(const clang::CXXRecordDecl *checkedDecl)
      : checkedDecl{checkedDecl} {}

  ForeignReferenceTypeChecker &&
  withDiagnostics(ClangImporter::Implementation &ImplRef) && {
    Impl = &ImplRef;
    return std::move(*this);
  }

  ForeignReferenceTypeInfo check() && {
    ASSERT(checkedDecl && "ForeignReferenceTypeInfo should only be used once");
    SWIFT_DEFER { checkedDecl = nullptr; };

    if (importer::hasImportReferenceAttr(checkedDecl)) {
      // checkedDecl is explicitly annotated as a foreign reference type.
      // Do not let it have a primarySuperclass, to prevent upcasting past the
      // annotation boundary in the class hierarchy.
      return ForeignReferenceTypeInfo::Shared(checkedDecl,
                                              /*primarySuperclass=*/nullptr);
    }

    const clang::CXXRecordDecl *uniqueDirectFRTBase = visitBases(checkedDecl);
    if (FRTBases.empty()) {
      // Neither checkedDecl nor any of its base classes are annotated as
      // a reference type, so checkedDecl is a value type.
      ASSERT(uniqueDirectFRTBase == nullptr &&
             "there should be no superclass if there are no FRT bases");
      return ForeignReferenceTypeInfo::Value();
    }

    // The primary FRT superclass is the unique direct FRT base of checkedDecl,
    // but only if it is at offset 0 (so a pointer bitcast suffices for upcast).
    auto *primarySuperclass = isPresentAndAtOffsetZero(uniqueDirectFRTBase)
                                  ? uniqueDirectFRTBase
                                  : nullptr;

    const clang::CXXRecordDecl *FRTBase = nullptr;
    bool seenShared = false, seenMultipleShared = false, seenImmortal = false;

    for (auto *base : FRTBases) {
      if (importer::hasAnyImmortalAttr(base)) {
        seenImmortal = true;
      } else if (!FRTBase) {
        FRTBase = base;
        seenShared = true;
      } else {
        seenMultipleShared = true;
      }
    }

    // If there are no shared references, FRTBase is the first immortal base.
    if (!FRTBase) {
      ASSERT(seenImmortal && "should have encountered immortal FRTBase");
      FRTBase = FRTBases.front();
    }

    if (seenMultipleShared || (seenShared && seenImmortal)) {
      // checkedDecl is an invalid FRT, either because it has multiple shared
      // FRT bases (ambiguous retain/release ops), or because it has mixed
      // ancestry between shared and immortal bases.
      if (Impl)
        Impl->diagnose(HeaderLoc{checkedDecl->getLocation()},
                       diag::cant_infer_frt_in_cxx_inheritance, checkedDecl);

      // decl inherits from FRT base, so we should treat it as a reference
      // type, albeit an invalid one (due to ambiguity).
      //
      // return ForeignReferenceTypeInfo::Shared(FRTBase, nullptr,
      //                                         /*isValid=*/false);
      //
      // However, to honor the existing behavior, (for now) we will report
      // that this is an (invalid) value type.
      return ForeignReferenceTypeInfo::Value(/*isValid=*/false);
    }
    return ForeignReferenceTypeInfo::Shared(FRTBase, primarySuperclass);
  }
};
} // namespace

void swift::simple_display(llvm::raw_ostream &out,
                           const ForeignReferenceTypeInfoDescriptor &desc) {
  out << "Checking foreign reference type info for '";
  if (desc.decl->getIdentifier())
    out << desc.decl->getName();
  else if (desc.decl->isAnonymousStructOrUnion())
    out << "(anonymous record)";
  else
    out << "(unnamed record)";
  out << "'\n";
}

SourceLoc
swift::extractNearestSourceLoc(const ForeignReferenceTypeInfoDescriptor &desc) {
  return SourceLoc();
}

ForeignReferenceTypeInfo ForeignReferenceTypeInfoRequest::evaluate(
    Evaluator &evaluator, ForeignReferenceTypeInfoDescriptor desc) const {
  auto *decl = desc.decl;

  if (auto *cxxDecl = dyn_cast<clang::CXXRecordDecl>(decl))
    return ForeignReferenceTypeChecker(cxxDecl).check();

  // If this isn't a C++ record, then there's no inheritance (nor any of the
  // associated complications) to worry about. Just look for ref attributes.

  if (importer::hasImportReferenceAttr(decl))
    return ForeignReferenceTypeInfo::Shared(decl,
                                            /*primarySuperclass=*/nullptr);

  return ForeignReferenceTypeInfo::Value();
}

bool importer::diagnoseForeignReferenceType(
    const clang::CXXRecordDecl *decl, ClangImporter::Implementation &Impl) {

  // First, evaluate as a request. This does not emit diagnostics, but caches
  // the result for future requests. This ensures that we perform the checkFRT()
  // routine at most once for valid decls.
  auto info = evaluateOrDefault(Impl.SwiftContext.evaluator,
                                ForeignReferenceTypeInfoRequest({decl}), {});
  if (info.isValid())
    return true;

  // If the result was invalid, we need to run the underlying check again, but
  // this time with ClangImporter::Implemention in order to emit diagnostics.
  // This slow path does redundant work but only for invalid decls.
  auto infoAgain =
      ForeignReferenceTypeChecker(decl).withDiagnostics(Impl).check();
  // FIXME: this appears to be non-deterministic in some configurations
  // ASSERT(!infoAgain.isValid() && "FRT check should be deterministic");
  (void)infoAgain;
  return false;
}

static const clang::RecordDecl *
getReturnTypeAsRecordDeclPtr(const clang::NamedDecl *ND) {
  clang::QualType retTy;

  if (auto *CD = dyn_cast<clang::CXXConstructorDecl>(ND))
    retTy = CD->getParent()->getTypeForDecl()->getCanonicalTypeUnqualified();
  else if (auto *FD = dyn_cast<clang::FunctionDecl>(ND))
    retTy = FD->getReturnType();
  else if (auto *MD = dyn_cast<clang::ObjCMethodDecl>(ND))
    retTy = MD->getReturnType();
  else
    return nullptr;

  if (!retTy->isPointerOrReferenceType())
    return nullptr;
  // N.B. We can't use QualType::just getPointeeCXXRecordDecl here because we
  // also need to account for ObjC interop, where FRTs are clang::RecordDecls.
  return retTy->getPointeeType()->getAsRecordDecl();
}

static void diagnoseMissingReturnsRetained(ClangImporter::Implementation &Impl,
                                           const ValueDecl *func,
                                           SourceLoc callSiteLoc) {
  auto &ctx = Impl.SwiftContext;
  auto *clangFunc = cast<clang::NamedDecl>(func->getClangDecl());

  if (!isa<clang::FunctionDecl, clang::ObjCMethodDecl>(clangFunc))
    // Ownership attrs are not yet supported for non-(functions|ObjCMethods),
    // in particular clang::BlockDecls and clang::VarDecls of function/block
    // pointers, so we exclude them from these diagnostics.
    //
    // Furthermore, we do not diagnose clang::FunctionTemplateDecls here;
    // instead, we need to diagnose calls to their specializations.
    return;
  
  if (const auto *methodDecl = dyn_cast<clang::CXXMethodDecl>(clangFunc)) {
    ASSERT((!isa<clang::CXXDeductionGuideDecl, clang::CXXDestructorDecl>(
               clangFunc)) &&
           "C++ deduction guides and destructors can't be called in Swift");

    if (methodDecl->isOverloadedOperator())
      return; // Ownership attrs are not yet supported for overloaded operators

    if (!methodDecl->isUserProvided())
      return; // Implicit methods shouldn't be diagnosed because users can't
              // annotate them
  }

  auto attrInfo = importer::ReturnOwnershipInfo(clangFunc);
  if (attrInfo.hasRetainAttr())
    return; // function is annotated, so it can't be missing

  auto *recordDecl = getReturnTypeAsRecordDeclPtr(clangFunc);
  if (!recordDecl)
    return; // Not returning a pointer to a clang::RecordDecl

  auto info =
      evaluateOrDefault(Impl.SwiftContext.evaluator,
                        ForeignReferenceTypeInfoRequest({recordDecl}), {});
  if (!info.isReference() || importer::hasAnyImmortalAttr(recordDecl))
    return; // recordDecl is not a shared reference type

  if (importer::matchSwiftAttr<bool>(
          info.getDecl(), {{"returned_as_unretained_by_default", true}}))
    return;

  // If we reached here, then we have a call to an unannotated, Clang-imported
  // function that returns a pointer to a shared reference type that doesn't
  // have a default return ownership convention. Emit diagnostics.

  ctx.Diags.diagnose(callSiteLoc, diag::unannotated_cxx_func_returning_frt,
                     func);

  Impl.diagnose(HeaderLoc{clangFunc->getLocation()},
                diag::unannotated_cxx_func_returning_frt_suggestion, func);
}

void ClangImporter::checkCalledClangFunction(const ValueDecl *func,
                                             SourceLoc callSiteLoc) {
  diagnoseMissingReturnsRetained(Impl, func, callSiteLoc);
}

std::optional<ResultConvention>
swift::importer::getOwnershipOfReturnedFRT(const clang::NamedDecl *decl) {

  auto attrInfo = importer::ReturnOwnershipInfo(decl);
  if (attrInfo.hasReturnsUnretained)
    return ResultConvention::Unowned;

  if (attrInfo.hasReturnsRetained)
    return ResultConvention::Owned;

  if (auto *recordDecl = getReturnTypeAsRecordDeclPtr(decl)) {
    if (auto convention = importer::matchSwiftAttr<ResultConvention>(
            recordDecl,
            {{"returned_as_unretained_by_default", ResultConvention::Unowned}}))
      return convention.value();

    // FIXME: this is only here to preserve legacy behavior; we really shouldn't
    //        consider returned_as_unretained_by_default annotations on anything
    //        other than the "canonical" FRT base (the one whose retain/release
    //        methods we use)
    if (auto *cxxRecordDecl = dyn_cast<clang::CXXRecordDecl>(recordDecl);
        cxxRecordDecl && cxxRecordDecl->hasDefinition()) {
      auto hasAttr = false;
      cxxRecordDecl->forallBases([&hasAttr](auto *base) {
        hasAttr =
            hasAttr || importer::matchSwiftAttr<bool>(
                           base, {{"returned_as_unretained_by_default", true}});
        return true;
      });
      if (hasAttr)
        return ResultConvention::Unowned;
    }
  }

  return std::nullopt;
}

//===----------------------------------------------------------------------===//
// Foreign reference type retain/release operations
//===----------------------------------------------------------------------===//

namespace {
/// The result of resolving a single retain or release operation for a foreign
/// reference type from its `retain:`/`release:` swift_attr attribute.
struct ResolvedRefCountOperation {
  enum Kind {
    /// The type has no `retain:`/`release:` attribute for this operation.
    NoAttribute,
    /// The type has more than one attribute for this operation.
    TooManyAttributes,
    /// The operation is `immortal`.
    Immortal,
    /// The named operation could not be found.
    NotFound,
    /// The named operation resolves ambiguously.
    TooManyFound,
    /// The derived/base type definitions are not reachable, so a forwarding
    /// operation could not be synthesized.
    Unreachable,
    /// A unique, valid operation was found; \c operation is set.
    FoundOperation,
  } kind;
  ValueDecl *operation = nullptr;
  StringRef name = {};
};

enum class RetainReleaseOperationKind {
  notAfunction,
  notAnInstanceFunction,
  invalidReturnType,
  invalidParameters,
  valid
};
} // namespace

/// Check whether \p operation is a valid retain/release operation for the
/// foreign reference type \p classDecl (\p isRetainOperation selects which).
static RetainReleaseOperationKind checkRetainReleaseOperationValidity(
    const ClassDecl *classDecl, ValueDecl *operation, bool isRetainOperation) {
  auto operationFn = dyn_cast<FuncDecl>(operation);
  if (!operationFn)
    return RetainReleaseOperationKind::notAfunction;

  if (operationFn->isStatic())
    return RetainReleaseOperationKind::notAnInstanceFunction;

  if (operationFn->isInstanceMember()) {
    if (operationFn->getParameters()->size() != 0)
      return RetainReleaseOperationKind::invalidParameters;
  } else {
    if (operationFn->getParameters()->size() != 1)
      return RetainReleaseOperationKind::invalidParameters;
  }

  Type paramType;
  NominalTypeDecl *paramDecl = nullptr;
  if (!operationFn->isInstanceMember()) {
    paramType = operationFn->getParameters()
                    ->get(0)
                    ->getInterfaceType()
                    ->lookThroughSingleOptionalType();

    paramDecl = paramType->getAnyNominal();
  } else {
    paramDecl = cast<NominalTypeDecl>(operationFn->getParent());
    paramType = paramDecl->getDeclaredInterfaceType();
  }

  // The return type should be void (for release functions), or void
  // or the parameter type (for retain functions).
  auto resultInterfaceType = operationFn->getResultInterfaceType();
  if (!resultInterfaceType->isVoid() && !resultInterfaceType->isUInt() &&
      !resultInterfaceType->isUInt8() && !resultInterfaceType->isUInt16() &&
      !resultInterfaceType->isUInt32() && !resultInterfaceType->isUInt64() &&
      !resultInterfaceType->isInt() && !resultInterfaceType->isInt8() &&
      !resultInterfaceType->isInt16() && !resultInterfaceType->isInt32() &&
      !resultInterfaceType->isInt64()) {
    if (!isRetainOperation ||
        !resultInterfaceType->lookThroughSingleOptionalType()->isEqual(
            paramType))
      return RetainReleaseOperationKind::invalidReturnType;
  }

  // The parameter of the retain/release function should be pointer to the
  // same FRT or a base FRT.
  if (paramDecl != classDecl) {
    if (auto cxxDecl =
            dyn_cast<clang::CXXRecordDecl>(classDecl->getClangDecl())) {
      if (const clang::Decl *paramClangDecl = paramDecl->getClangDecl()) {
        if (const auto *paramTypeDecl =
                dyn_cast<clang::CXXRecordDecl>(paramClangDecl)) {
          if (cxxDecl->isDerivedFrom(paramTypeDecl)) {
            return RetainReleaseOperationKind::valid;
          }
        }
      }
    }
    return RetainReleaseOperationKind::invalidParameters;
  }

  return RetainReleaseOperationKind::valid;
}

/// Resolve the retain (\p isRetain) or release operation for the foreign
/// reference type \p swiftDecl by inspecting its `retain:`/`release:`
/// swift_attr attributes. This does not emit any diagnostics.
static ResolvedRefCountOperation
resolveRefCountOperation(const ClassDecl *swiftDecl, bool isRetain,
                         ClangImporter::Implementation &Impl) {
  StringRef operationStr = isRetain ? "retain:" : "release:";

  auto decl = cast<clang::RecordDecl>(swiftDecl->getClangDecl());
  if (!decl->hasAttrs())
    return {ResolvedRefCountOperation::NoAttribute};

  llvm::SmallVector<const clang::SwiftAttrAttr *, 1> retainReleaseAttrs;
  for (auto *attr : decl->getAttrs()) {
    if (auto swiftAttr = dyn_cast<clang::SwiftAttrAttr>(attr)) {
      if (swiftAttr->getAttribute().starts_with(operationStr))
        retainReleaseAttrs.push_back(swiftAttr);
    }
  }

  if (retainReleaseAttrs.empty())
    return {ResolvedRefCountOperation::NoAttribute};

  if (retainReleaseAttrs.size() > 1)
    return {ResolvedRefCountOperation::TooManyAttributes};

  auto name = retainReleaseAttrs.front()->getAttribute().drop_front(
      operationStr.size());

  if (name == "immortal")
    return {ResolvedRefCountOperation::Immortal, nullptr, name};

  auto results =
      importer::getValueDeclsForName(const_cast<ClassDecl *>(swiftDecl), name);

  TinyPtrVector<ValueDecl *> validResults;
  if (results.size() > 1) {
    // If we have ambiguous retain/release operations, try to disambiguate.
    for (auto *candidate : results) {
      if (checkRetainReleaseOperationValidity(swiftDecl, candidate, isRetain) ==
          RetainReleaseOperationKind::valid)
        validResults.push_back(candidate);
    }
  } else if (results.size() == 1) {
    validResults.push_back(results.front());
  }

  if (validResults.size() == 1)
    return {ResolvedRefCountOperation::FoundOperation, validResults.front(),
            name};

  if (validResults.empty())
    return {ResolvedRefCountOperation::NotFound, nullptr, name};

  return {ResolvedRefCountOperation::TooManyFound, nullptr, name};
}

/// Emit diagnostics for a single (retain or release) operation of a foreign
/// reference type. \p sel selects between retain (false) and release (true) in
/// the diagnostic wording, matching the existing diagnostics.
static void diagnoseRefCountOperation(ClangImporter::Implementation &Impl,
                                      HeaderLoc loc,
                                      const clang::RecordDecl *decl,
                                      ClassDecl *classDecl, bool isRetain,
                                      const ResolvedRefCountOperation &op) {
  bool sel = !isRetain;
  switch (op.kind) {
  case ResolvedRefCountOperation::NoAttribute:
    Impl.diagnose(loc, diag::reference_type_must_have_retain_release_attr, sel,
                  decl->getNameAsString());
    return;
  case ResolvedRefCountOperation::TooManyAttributes:
    Impl.diagnose(loc, diag::too_many_reference_type_retain_release_attr, sel,
                  decl->getNameAsString());
    return;
  case ResolvedRefCountOperation::NotFound:
    Impl.diagnose(loc, diag::foreign_reference_types_cannot_find_retain_release,
                  sel, op.name, decl->getNameAsString());
    if (!Impl.SwiftContext.LangOpts
             .DisableExperimentalClangImporterDiagnostics) {
      Impl.diagnoseTopLevelValue(
          DeclName(Impl.SwiftContext.getIdentifier(op.name)));
    }
    return;
  case ResolvedRefCountOperation::TooManyFound:
    Impl.diagnose(loc, diag::too_many_reference_type_retain_release_operations,
                  sel, op.name, decl->getNameAsString());
    return;
  case ResolvedRefCountOperation::FoundOperation: {
    auto operationKind =
        checkRetainReleaseOperationValidity(classDecl, op.operation, isRetain);
    switch (operationKind) {
    case RetainReleaseOperationKind::notAfunction:
      Impl.diagnose(
          loc, diag::foreign_reference_types_retain_release_not_a_function_decl,
          sel, op.name);
      break;
    case RetainReleaseOperationKind::notAnInstanceFunction:
      Impl.diagnose(
          loc,
          diag::foreign_reference_types_retain_release_not_an_instance_function,
          sel, op.name);
      break;
    case RetainReleaseOperationKind::invalidReturnType:
      if (isRetain)
        Impl.diagnose(
            loc,
            diag::foreign_reference_types_retain_non_void_or_self_return_type,
            op.name);
      else
        Impl.diagnose(
            loc, diag::foreign_reference_types_release_non_void_return_type,
            op.name);
      break;
    case RetainReleaseOperationKind::invalidParameters:
      Impl.diagnose(loc, diag::foreign_reference_types_invalid_retain_release,
                    sel, op.name, classDecl->getNameStr());
      break;
    case RetainReleaseOperationKind::valid:
      break;
    }
    return;
  }
  case ResolvedRefCountOperation::Immortal:
  case ResolvedRefCountOperation::Unreachable:
    // Nothing to diagnose here (unreachable is diagnosed separately).
    return;
  }
}

/// Emit all diagnostics for the retain/release operations of the foreign
/// reference type \p classDecl (Clang record \p decl).
static void
diagnoseRefCountOperations(ClangImporter::Implementation &Impl,
                           const clang::RecordDecl *decl, ClassDecl *classDecl,
                           const ResolvedRefCountOperation &retainOp,
                           const ResolvedRefCountOperation &releaseOp) {
  if (retainOp.kind == ResolvedRefCountOperation::Unreachable ||
      releaseOp.kind == ResolvedRefCountOperation::Unreachable) {
    Impl.diagnose(HeaderLoc(decl->getLocation()),
                  diag::foreign_reference_type_unreachable,
                  classDecl->getNameStr());
    return;
  }

  HeaderLoc loc(decl->getLocation());
  diagnoseRefCountOperation(Impl, loc, decl, classDecl, /*isRetain=*/true,
                            retainOp);
  diagnoseRefCountOperation(Impl, loc, decl, classDecl, /*isRetain=*/false,
                            releaseOp);
}

/// Normalize a resolved retain/release operation \p op to the concrete Clang
/// function that IRGen should emit a call to. This mirrors the normalization
/// that IRGen previously performed on the operation ValueDecl.
static const clang::FunctionDecl *
getEmittedClangCallee(ValueDecl *op, ClangImporter::Implementation &Impl) {
  if (!op)
    return nullptr;
  auto *loader = Impl.SwiftContext.getClangModuleLoader();
  if (loader->getOriginalForClonedMember(op))
    op = loader->getCalledBaseCxxMethod(op);
  if (!op || !op->getClangDecl())
    return nullptr;
  return dyn_cast<clang::FunctionDecl>(op->getClangDecl());
}

/// Return the underlying Clang function of a resolved retain/release operation
/// \p op, looking through cloned members to the original. Used as the target
/// that a synthesized forwarding method should call.
static const clang::FunctionDecl *
getUnderlyingClangFn(ValueDecl *op, ClangImporter::Implementation &Impl) {
  if (!op)
    return nullptr;
  if (auto *original = Impl.getOriginalForClonedMember(op))
    op = original;
  return dyn_cast_or_null<clang::FunctionDecl>(op->getClangDecl());
}

/// Clone the retain:/release: swift_attr attributes from the FRT base \p from
/// onto the derived FRT \p to. Used for immortal foreign reference types, which
/// have no retain/release functions to synthesize forwarding methods for.
static void cloneRefCountingAttributes(clang::CXXRecordDecl *to,
                                       const clang::CXXRecordDecl *from,
                                       clang::ASTContext &ctx) {
  if (!from->hasAttr<clang::SwiftAttrAttr>())
    return;
  for (auto attr : from->getAttrs()) {
    if (auto swiftAttr = dyn_cast<clang::SwiftAttrAttr>(attr)) {
      if (swiftAttr->getAttribute().starts_with("release:") ||
          swiftAttr->getAttribute().starts_with("retain:"))
        to->addAttr(
            clang::SwiftAttrAttr::Create(ctx, swiftAttr->getAttribute()));
    }
  }
}

/// Synthesize forwarding retain/release methods on the derived FRT \p decl that
/// call the base FRT's \p baseRetainFn / \p baseReleaseFn, and import them as
/// members. Returns the synthesized methods, or {nullptr, nullptr} on failure;
/// sets \p isUnreachable if the derived or base type has no reachable
/// definition.
static std::pair<const clang::CXXMethodDecl *, const clang::CXXMethodDecl *>
synthesizeInheritedRefCountOperations(ClassDecl *decl,
                                      clang::CXXRecordDecl *clangDecl,
                                      const clang::FunctionDecl *baseRetainFn,
                                      const clang::FunctionDecl *baseReleaseFn,
                                      const clang::CXXRecordDecl *baseClangDecl,
                                      ClangImporter::Implementation &Impl,
                                      bool &isUnreachable) {
  isUnreachable = false;
  auto &context = Impl.SwiftContext;
  auto &clangCtx = Impl.getClangASTContext();

  if (!baseRetainFn || !baseReleaseFn)
    return {nullptr, nullptr};

  auto &clangSema = Impl.getClangSema();
  {
    clang::Sema::SFINAETrap trap(clangSema);
    // The derived FRT and its FRT base must both have reachable definitions so
    // that we can synthesize expressions that implicitly cast from one to the
    // other (which requires knowing their layout).
    if (!clangSema.hasReachableDefinition(
            const_cast<clang::CXXRecordDecl *>(clangDecl)) ||
        !clangSema.hasReachableDefinition(
            const_cast<clang::CXXRecordDecl *>(baseClangDecl))) {
      isUnreachable = true;
      return {nullptr, nullptr};
    }
  }

  // Synthesize forwarding function.

  clang::QualType methodType = clangCtx.getFunctionType(
      clangCtx.VoidTy, {}, clang::FunctionProtoType::ExtProtoInfo{});

  auto generateLifetimeOperation =
      [&](const clang::FunctionDecl *fd) -> const clang::CXXMethodDecl * {
    auto loc = fd->getLocation();
    auto &ident = clangCtx.Idents.get("__synthesized_lifetimeAccessor_" +
                                      fd->getNameAsString());
    clang::DeclarationName methodName(&ident);
    auto method = clang::CXXMethodDecl::Create(
        clangCtx, clangDecl, fd->getSourceRange().getBegin(),
        clang::DeclarationNameInfo(methodName, clang::SourceLocation()),
        methodType, clangCtx.getTrivialTypeSourceInfo(methodType),
        clang::SC_None,
        /*usesFPIntrin=*/false, /*isInline=*/true,
        clang::ConstexprSpecKind::Unspecified, fd->getSourceRange().getEnd());
    method->setImplicit();
    method->setImplicitlyInline();
    method->setAccess(clang::AccessSpecifier::AS_public);
    method->addAttr(clang::NoDebugAttr::CreateImplicit(clangCtx));

    clang::Expr *argExpr =
        clang::CXXThisExpr::Create(clangCtx, clang::SourceLocation(),
                                   method->getThisType(), /*IsImplicit=*/false);

    if (auto calledMethod = dyn_cast<clang::CXXMethodDecl>(fd)) {
      if (calledMethod->isStatic())
        return nullptr;
      auto memberExpr = clangSema.BuildMemberExpr(
          argExpr, /*isArrow=*/true, loc, clang::NestedNameSpecifierLoc(),
          clang::SourceLocation(),
          const_cast<clang::CXXMethodDecl *>(calledMethod),
          clang::DeclAccessPair::make(
              const_cast<clang::CXXMethodDecl *>(calledMethod),
              clang::AS_public),
          /*HadMultipleCandidates=*/false, calledMethod->getNameInfo(),
          clangCtx.BoundMemberTy, clang::VK_PRValue, clang::OK_Ordinary);
      auto memberCall =
          clangSema.BuildCallExpr(nullptr, memberExpr, clang::SourceLocation(),
                                  {}, clang::SourceLocation());
      ASSERT(memberCall.isUsable());
      method->setBody(clang::CompoundStmt::Create(
          clangCtx, {memberCall.get()}, clang::FPOptionsOverride(), loc, loc));
    } else {
      clang::Expr *fnExpr = clang::DeclRefExpr::Create(
          clangCtx, clang::NestedNameSpecifierLoc(), clang::SourceLocation(),
          const_cast<clang::FunctionDecl *>(fd),
          /*RefersToEnclosingVariableOrCapture=*/false, loc, fd->getType(),
          clang::VK_LValue);
      auto call =
          clangSema.BuildCallExpr(nullptr, fnExpr, clang::SourceLocation(),
                                  {argExpr}, clang::SourceLocation());
      method->setBody(clang::CompoundStmt::Create(
          clangCtx, {call.get()}, clang::FPOptionsOverride(), loc, loc));
    }
    return method;
  };

  auto synthesizedRetain = generateLifetimeOperation(baseRetainFn);
  auto synthesizedRelease = generateLifetimeOperation(baseReleaseFn);
  if (!synthesizedRetain || !synthesizedRelease)
    return {nullptr, nullptr};

  // Add attributes to class.
  clangDecl->addAttr(clang::SwiftAttrAttr::Create(
      clangCtx,
      context.AllocateCopy("retain:." + synthesizedRetain->getNameAsString())));
  clangDecl->addAttr(clang::SwiftAttrAttr::Create(
      clangCtx, context.AllocateCopy("release:." +
                                     synthesizedRelease->getNameAsString())));

  // Update the Swift type
  auto importRefCountOp = [&](const clang::CXXMethodDecl *op) {
    auto importedOp =
        cast<ValueDecl>(context.getClangModuleLoader()->importDeclDirectly(op));
    Impl.markMemberSynthesizedPerType(importedOp);
    decl->addMember(importedOp);
    decl->addMemberToLookupTable(importedOp);
  };
  importRefCountOp(synthesizedRetain);
  importRefCountOp(synthesizedRelease);

  return {synthesizedRetain, synthesizedRelease};
}

void importer::checkRetainReleaseFunctions(
    ClassDecl *classDecl, const clang::RecordDecl *clangDecl,
    ClangImporter::Implementation &Impl) {
  // Determine which type carries the retain/release annotations. For an FRT
  // that inherits its reference-counting operations from a base FRT, the
  // annotations live on the base; otherwise they live on this type.
  const ClassDecl *annotatedDecl = classDecl;
  const clang::CXXRecordDecl *cxxDecl =
      dyn_cast<clang::CXXRecordDecl>(clangDecl);
  const clang::CXXRecordDecl *baseClangDecl = nullptr;

  if (cxxDecl) {
    auto frtInfo =
        evaluateOrDefault(Impl.SwiftContext.evaluator,
                          ForeignReferenceTypeInfoRequest({cxxDecl}), {});
    baseClangDecl = dyn_cast_or_null<clang::CXXRecordDecl>(frtInfo.getDecl());
    if (baseClangDecl && baseClangDecl != cxxDecl) {
      annotatedDecl =
          cast<ClassDecl>(Impl.importDecl(baseClangDecl, Impl.CurrentVersion));
    } else {
      baseClangDecl = nullptr;
    }
  }

  bool isInherited = baseClangDecl != nullptr;

  // Resolve the retain/release operations from the annotated type's attributes.
  auto retainOp =
      resolveRefCountOperation(annotatedDecl, /*isRetain=*/true, Impl);
  auto releaseOp =
      resolveRefCountOperation(annotatedDecl, /*isRetain=*/false, Impl);

  bool isImmortal = retainOp.kind == ResolvedRefCountOperation::Immortal ||
                    releaseOp.kind == ResolvedRefCountOperation::Immortal;

  // The concrete Clang callees to record for this type. Null means immortal /
  // no custom reference counting.
  const clang::FunctionDecl *retainFn = nullptr;
  const clang::FunctionDecl *releaseFn = nullptr;
  bool recordOperations = false;

  if (isInherited) {
    if (isImmortal) {
      // Immortal base: clone the retain:/release: attributes onto this type.
      cloneRefCountingAttributes(const_cast<clang::CXXRecordDecl *>(cxxDecl),
                                 baseClangDecl, Impl.getClangASTContext());
    } else if (retainOp.kind == ResolvedRefCountOperation::FoundOperation &&
               releaseOp.kind == ResolvedRefCountOperation::FoundOperation) {
      // Shared base: synthesize forwarding retain/release methods on this type
      // that call the base's operations (performing the pointer adjustment).
      auto *baseRetainFn = getUnderlyingClangFn(retainOp.operation, Impl);
      auto *baseReleaseFn = getUnderlyingClangFn(releaseOp.operation, Impl);
      bool isUnreachable = false;
      auto synthesized = synthesizeInheritedRefCountOperations(
          classDecl, const_cast<clang::CXXRecordDecl *>(cxxDecl), baseRetainFn,
          baseReleaseFn, baseClangDecl, Impl, isUnreachable);
      if (isUnreachable) {
        retainOp.kind = releaseOp.kind = ResolvedRefCountOperation::Unreachable;
      } else if (synthesized.first && synthesized.second) {
        retainFn = synthesized.first;
        releaseFn = synthesized.second;
        recordOperations = true;
      }
    }
    // Diagnose using the (possibly updated) base operations.
    diagnoseRefCountOperations(Impl, clangDecl, classDecl, retainOp, releaseOp);
  } else {
    diagnoseRefCountOperations(Impl, clangDecl, classDecl, retainOp, releaseOp);
    if (retainOp.kind == ResolvedRefCountOperation::FoundOperation &&
        releaseOp.kind == ResolvedRefCountOperation::FoundOperation) {
      retainFn = getEmittedClangCallee(retainOp.operation, Impl);
      releaseFn = getEmittedClangCallee(releaseOp.operation, Impl);
      recordOperations = retainFn && releaseFn;
    }
  }

  if (isImmortal) {
    // Immortal FRTs are valid reference types with no custom reference
    // counting; record them with null operations.
    Impl.setForeignReferenceTypeOperations(clangDecl, /*retain=*/nullptr,
                                           /*release=*/nullptr);
  } else if (recordOperations) {
    Impl.setForeignReferenceTypeOperations(clangDecl, retainFn, releaseFn);
  }
}

//===----------------------------------------------------------------------===//
// Unsafe projection ("__fooUnsafe") analysis
//===----------------------------------------------------------------------===//

/// Is \a type a pointer or reference to a foreign reference type?
static bool clangTypeIsForeignReference(const clang::QualType type,
                                        ASTContext &ctx) {
  if (!type->isPointerOrReferenceType())
    return false;
  auto *pointee = type->getPointeeType().getCanonicalType()->getAsRecordDecl();
  if (!pointee)
    return false;
  auto info = evaluateOrDefault(ctx.evaluator,
                                ForeignReferenceTypeInfoRequest({pointee}), {});
  return info.isReference();
}

static bool hasCustomCopyOrMoveConstructor(const clang::CXXRecordDecl *decl) {
  return decl->hasUserDeclaredCopyConstructor() ||
         decl->hasUserDeclaredMoveConstructor();
}

bool importer::isSwiftClassType(const clang::CXXRecordDecl *decl) {
  // Swift type must be annotated with external_source_symbol attribute.
  auto essAttr = decl->getAttr<clang::ExternalSourceSymbolAttr>();
  if (!essAttr || essAttr->getLanguage() != "Swift" ||
      essAttr->getDefinedIn().empty() || essAttr->getUSR().empty())
    return false;

  // Ensure that the baseclass is swift::RefCountedClass.
  auto baseDecl = decl->getDefinition();
  if (!baseDecl)
    return false;
  do {
    if (baseDecl->getNumBases() != 1)
      return false;
    auto baseClassSpecifier = *baseDecl->bases_begin();
    auto Ty = baseClassSpecifier.getType();
    auto nextBaseDecl = Ty->getAsCXXRecordDecl();
    if (!nextBaseDecl)
      return false;
    baseDecl = nextBaseDecl->getDefinition();
    if (!baseDecl)
      return false;
  } while (baseDecl->getName() != "RefCountedClass");

  return true;
}

static bool anySubobjectsSelfContained(const clang::CXXRecordDecl *decl) {
  // std::pair and std::tuple might have copy and move constructors, or base
  // classes with copy and move constructors, but they are not self-contained
  // types, e.g. `std::pair<UnsafeType, T>`.
  if (decl->isInStdNamespace() &&
      (decl->getName() == "pair" || decl->getName() == "tuple"))
    return false;

  if (!decl->getDefinition())
    return false;

  if (hasCustomCopyOrMoveConstructor(decl) || importer::hasOwnedValueAttr(decl))
    return true;

  auto checkType = [](clang::QualType t) {
    if (auto recordType = dyn_cast<clang::RecordType>(t.getCanonicalType())) {
      if (auto cxxRecord =
              dyn_cast<clang::CXXRecordDecl>(recordType->getDecl())) {
        return anySubobjectsSelfContained(cxxRecord);
      }
    }

    return false;
  };

  for (auto field : decl->fields()) {
    if (checkType(field->getType()))
      return true;
  }

  for (auto base : decl->bases()) {
    if (checkType(base.getType()))
      return true;
  }

  return false;
}

bool importer::shouldRenameCXXMethodAsUnsafe(const clang::CXXMethodDecl *method,
                                             ASTContext &ctx) {
  // The user explicitly explicitly acknowledged this method's unsafety
  // and asked us to import it as is anyway. No renaming needed.
  if (hasUnsafeAPIAttr(method))
    return false;

  // If it's a static method, it cannot project anything. It's fine.
  if (method->isOverloadedOperator() || method->isStatic() ||
      isa<clang::CXXConstructorDecl>(method))
    return false;

  // begin and end methods likely return an iterator, so they're unsafe.
  // This is required so that automatic the conformance to RAC works properly.
  if (method->getNameAsString() == "begin" ||
      method->getNameAsString() == "end")
    return true;

  if (clangTypeIsForeignReference(method->getReturnType(), ctx))
    return false;

  auto parentQualType =
      method->getParent()->getTypeForDecl()->getCanonicalTypeUnqualified();

  bool parentIsSelfContained =
      !clangTypeIsForeignReference(parentQualType, ctx) &&
      anySubobjectsSelfContained(method->getParent());

  // If it returns a pointer or reference from an owned parent, that's a
  // projection (unsafe).
  if (method->getReturnType()->isPointerType() ||
      method->getReturnType()->isReferenceType())
    return parentIsSelfContained;

  // Check if it's one of the known unsafe methods we currently
  // mark as safe by default.
  if (isUnsafeStdMethod(method))
    return true;

  // Try to figure out the semantics of the return type. If it's a
  // pointer/iterator, it's unsafe.
  if (auto returnType = dyn_cast<clang::RecordType>(
          method->getReturnType().getCanonicalType())) {
    if (auto cxxRecordReturnType =
            dyn_cast<clang::CXXRecordDecl>(returnType->getDecl())) {
      if (isSwiftClassType(cxxRecordReturnType))
        return false;

      if (hasIteratorAPIAttr(cxxRecordReturnType) ||
          hasIteratorCategory(cxxRecordReturnType))
        return true;

      // Mark this as safe to help our diganostics down the road.
      if (!cxxRecordReturnType->getDefinition()) {
        return false;
      }

      // A projection of a view type (such as a string_view) from a self
      // contained parent is a proejction (unsafe).
      if (!anySubobjectsSelfContained(cxxRecordReturnType) &&
          isViewType(cxxRecordReturnType)) {
        return parentIsSelfContained;
      }
    }
  }

  // Otherwise, it's safe.
  return false;
}
