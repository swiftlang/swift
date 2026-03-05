#include "ImporterImpl.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

bool importer::hasImportReferenceAttr(const clang::RecordDecl *decl) {
  return decl->hasAttrs() && llvm::any_of(decl->getAttrs(), [](auto *attr) {
           if (auto swiftAttr = dyn_cast<clang::SwiftAttrAttr>(attr))
             return swiftAttr->getAttribute() == "import_reference" ||
                    // TODO: Remove this once libSwift hosttools no longer
                    // requires it.
                    swiftAttr->getAttribute() == "import_as_ref";
           return false;
         });
}

/// This routine determines whether \a decl is a foreign reference type, and
/// whether its foreign reference type attributes are valid. This determinition
/// considers attributes annotated on both \a decl itself as well as its
/// transitive bases.
///
/// When \a Impl is non-null, this function will use it to emit diagnostics for
/// invalid attributes; when \a Impl is null, those diagnostics are not emitted
/// (but the returned ForeignReferenceTypeInfo will still indicate \a decl has
/// invalid attributes). This function is designed this way to keep the site
/// where diagnostics are emitted close to the logic that leads to them.
static ForeignReferenceTypeInfo
checkForeignReferenceType(const clang::CXXRecordDecl *decl,
                          ClangImporter::Implementation *Impl) {
  // This was explicitly annotated. Just trust the annotation.
  if (importer::hasImportReferenceAttr(decl))
    return ForeignReferenceTypeInfo::Shared(decl);

  if (!decl->hasDefinition())
    // If this doesn't have a definition, there's no inheritance info to check.
    return ForeignReferenceTypeInfo::Value();

  // Keep track of base classes that are marked as FRTs, with set semantics
  // because we cannot have multiple copies of the same base FRT.
  llvm::SmallSetVector<const clang::CXXRecordDecl *, 1> FRTBases;

  // Keep track of virtual bases, which we only need to visit once.
  llvm::SmallPtrSet<const clang::CXXRecordDecl *, 1> virtualBases;

  // Bases we have yet to visit. Each base class instance should appear
  // in this container exactly once.
  llvm::SmallVector<const clang::CXXRecordDecl *, 4> unvisitedBases;

  // N.B. our definition of "base" also includes the origin class itself,
  //      i.e., decl
  unvisitedBases.push_back(decl);

  bool multipleFRTs = false;

  while (!unvisitedBases.empty()) {
    auto *rec = unvisitedBases.pop_back_val();

    if (importer::hasImportReferenceAttr(rec)) {
      if (!FRTBases.insert(rec)) {
        // The same base FRT appears multiple times in the class hierarchy
        // (i.e., due to diamond inheritance), which is not valid. Bail.
        multipleFRTs = true;
        break;
      }
    }

    for (auto recBase : rec->bases()) {
      auto *base = recBase.getType()->getAsCXXRecordDecl();
      if (!base)
        // It is possible to encounter clang::TemplateSpecializationType.
        // In such cases, just bail and report this as an invalid value type
        return ForeignReferenceTypeInfo::Value(/*isValid=*/false);

      ASSERT(base->hasDefinition() && "base record should be complete");
      base = base->getDefinition();

      ASSERT(!base->isDependentContext() && "base should not be dependent");

      if (recBase.isVirtual()) {
        if (virtualBases.insert(base).second)
          // First time seeing this virtual base. Visit it later.
          unvisitedBases.push_back(base);
      } else {
        // Visit this non-virtual base class later.
        unvisitedBases.push_back(base);
      }
    }
  }

  if (FRTBases.empty())
    // Did not encounter any FRTs in the class hierarchy
    return ForeignReferenceTypeInfo::Value();

  // This is the first FRT we encountered during our base traversal,
  // so we shall use it as our "canonical" FRT base.
  auto *baseFRT = FRTBases.front();

  if (multipleFRTs) {
    // The same FRT base appears multiple times in the class hierarchy
    if (Impl)
      Impl->diagnose(HeaderLoc{decl->getLocation()},
                     diag::cant_infer_frt_in_cxx_inheritance, decl);

    // decl inherits from FRT base, so we should treat it as a reference
    // type, albeit an invalid one (due to ambiguity).
    //
    // return ForeignReferenceTypeInfo::Shared(baseFRT, /*isValid=*/false);
    //
    // However, in honor the existing behavior, (for now) we will report that
    // this is an (invalid) value type.
    return ForeignReferenceTypeInfo::Value(/*isValid=*/false);
  }

  constexpr StringRef retainPrefix = "retain:", releasePrefix = "release:";
  std::optional<StringRef> retain = std::nullopt, release = std::nullopt;
  bool multipleOps = false;

  for (auto *base : FRTBases) {
    for (auto *attr : base->getAttrs()) {
      auto *swiftAttr = llvm::dyn_cast<clang::SwiftAttrAttr>(attr);
      if (!swiftAttr)
        continue;
      auto attrStr = swiftAttr->getAttribute();
      if (attrStr.consume_front(retainPrefix)) {
        if (retain.has_value() && retain.value() != attrStr) {
          multipleOps = true;
          break;
        }
        retain = attrStr;
      } else if (attrStr.consume_front(releasePrefix)) {
        if (release.has_value() && release.value() != attrStr) {
          multipleOps = true;
          break;
        }
        release = attrStr;
      }
    }

    if (multipleOps)
      break;
  }

  if (multipleOps) {
    if (Impl)
      Impl->diagnose(HeaderLoc{decl->getLocation()},
                     diag::cant_infer_frt_in_cxx_inheritance, decl);

    // decl inherits from FRT base, so we should treat it as a reference
    // type, albeit an invalid one (due to ambiguity).
    //
    // return ForeignReferenceTypeInfo::Shared(baseFRT, /*isValid=*/false);
    //
    // However, in honor the existing behavior, (for now) we will report that
    // this is an (invalid) value type.
    return ForeignReferenceTypeInfo::Value(/*isValid=*/false);
  }

  return ForeignReferenceTypeInfo::Shared(baseFRT);
}

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
    return checkForeignReferenceType(cxxDecl, nullptr);

  // If this isn't a C++ record, then there's no inheritance (nor any of the
  // associated complications) to worry about. Just look for ref attributes.

  if (importer::hasImportReferenceAttr(decl))
    return ForeignReferenceTypeInfo::Shared(decl);

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
  auto infoAgain = checkForeignReferenceType(decl, &Impl);
  ASSERT(!infoAgain.isValid() && "FRT check validity should be deterministic");
  return false;
}
