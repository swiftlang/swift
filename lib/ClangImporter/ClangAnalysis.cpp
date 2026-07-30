#include "ImporterImpl.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Specifiers.h"
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
