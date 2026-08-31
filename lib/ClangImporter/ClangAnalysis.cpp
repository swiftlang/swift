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
#include <algorithm>

using namespace swift;

bool importer::hasImportReferenceAttr(const clang::RecordDecl *decl) {
  return hasSwiftAttribute(decl, {"import_reference"});
}

bool importer::hasSwiftAttributeOnAnyRedecl(const clang::RecordDecl *decl,
                                            ArrayRef<StringRef> attrs) {
  return llvm::any_of(decl->redecls(), [&](const clang::Decl *redecl) {
    return hasSwiftAttribute(redecl, attrs);
  });
}

bool importer::isForeignReferenceRecord(const clang::RecordDecl *decl,
                                        Evaluator &eval) {
  // Only ask the request once there is a definition. It is cached per
  // declaration, so asking about a class template specialization that has not
  // been instantiated yet would cache "not a reference" for good, even though
  // instantiating it may reveal a reference base.
  if (decl->getDefinition())
    return evaluateOrDefault(eval, ForeignReferenceTypeInfoRequest({decl}),
                             ForeignReferenceTypeInfo())
        .isReference();

  // Without one, a direct annotation is all there is to go on.
  return llvm::any_of(decl->redecls(), [](const clang::Decl *redecl) {
    return hasImportReferenceAttr(cast<clang::RecordDecl>(redecl));
  });
}

bool importer::hasImportAsOpaquePointerAttr(const clang::RecordDecl *decl) {
  return llvm::any_of(decl->specific_attrs<clang::SwiftAttrAttr>(),
                      [](const clang::SwiftAttrAttr *swiftAttr) {
                        return swiftAttr->getAttribute() ==
                               "import_opaque_pointer";
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
    auto *recordDecl = recordType->getDecl();
    auto *definition = recordDecl->getDefinition();
    // An explicitly unsafe type is never self-contained, so its unsafety is
    // not silently dropped by an enclosing view. The annotation may sit on any
    // declaration of the type, so consider the whole chain.
    if (importer::hasSwiftAttributeOnAnyRedecl(recordDecl, {"unsafe"}))
      return false;
    // Reference types are managed by Swift, so a pointer to one does not
    // introduce a lifetime dependency. This holds for a reference type that has
    // only been declared, too.
    if (importer::isForeignReferenceRecord(recordDecl, eval))
      return true;
    if (!definition)
      return false;
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
/// The retain:/release: attributes written directly on a record.
struct RetainReleaseInfo {
  RetainReleaseInfo(const clang::RecordDecl *decl) : decl(decl) {
    // Only the first swift_attr is propagated to a redeclaration, so
    // retain:/release: are often missing from the declaration at hand even
    // though the type is annotated. Read them from the declaration that spells
    // them, and from that one alone, so inherited copies are not counted twice.
    auto spellsRetainRelease = [](const clang::RecordDecl *record) {
      return llvm::any_of(record->specific_attrs<clang::SwiftAttrAttr>(),
                          [](const clang::SwiftAttrAttr *attr) {
                            return attr->getAttribute().starts_with("retain:") ||
                                   attr->getAttribute().starts_with("release:");
                          });
    };
    const clang::RecordDecl *carrier = decl;
    if (!spellsRetainRelease(carrier)) {
      for (auto *redecl : decl->redecls()) {
        auto *record = cast<clang::RecordDecl>(redecl);
        if (spellsRetainRelease(record)) {
          carrier = record;
          break;
        }
      }
    }

    for (auto *attr : carrier->specific_attrs<clang::SwiftAttrAttr>()) {
      StringRef attrStr = attr->getAttribute();
      if (attrStr.consume_front("retain:")) {
        retainAttrs.push_back(attr);
        retainName = attrStr;
      } else if (attrStr.consume_front("release:")) {
        releaseAttrs.push_back(attr);
        releaseName = attrStr;
      }
    }
  }

  /// The name of the last retain: operation (empty if none).
  StringRef getRetain() const { return retainName; }

  /// The name of the last release: operation (empty if none).
  StringRef getRelease() const { return releaseName; }

  bool isImmortal() const {
    return isValid() && retainName == "immortal" && releaseName == "immortal";
  }

  bool hasMixedImmortality() const {
    bool retainImmortal = retainName == "immortal";
    bool releaseImmortal = releaseName == "immortal";
    return retainImmortal != releaseImmortal;
  }

  bool isValid() const {
    return retainAttrs.size() == 1 && releaseAttrs.size() == 1 &&
           !hasMixedImmortality();
  }

  /// Emit "retain and release functions specified on / inherited from" note.
  void
  noteRetainReleaseOrigin(ClangImporter::Implementation &Impl,
                          clang::SourceLocation loc,
                          std::optional<bool> isRelease = std::nullopt) const {
    if (loc.isValid()) {
      unsigned sel = 2 /* retain and release functions */;
      if (isRelease.has_value())
        sel = isRelease.value() ? 1 /* release function */
                                : 0 /* retain function */;
      Impl.diagnose(HeaderLoc(loc), diag::retain_release_function_origin, sel,
                    /*isInherited=*/false, decl);
    }
    // Otherwise: no usable attribute location and not inherited -> omit.
  }

  /// Diagnose malformed retain:/release: attributes via \p Impl (if non-null).
  /// Returns whether the annotations are structurally well-formed.
  bool checkShape(ClangImporter::Implementation *Impl) const {
    HeaderLoc loc(decl->getLocation());
    auto checkOp = [&](bool isRelease) {
      auto &attrs = isRelease ? releaseAttrs : retainAttrs;
      auto &name = isRelease ? releaseName : retainName;
      if (attrs.size() != 1) {
        if (Impl) {
          Impl->diagnose(loc,
                         diag::reference_type_exactly_one_retain_release_attr,
                         isRelease, decl);
          for (auto *attr : attrs)
            noteRetainReleaseOrigin(*Impl, attr->getLocation(), isRelease);
        }
        return false;
      }
      if (name.empty()) {
        if (Impl) {
          Impl->diagnose(loc, diag::reference_type_empty_retain_release_name,
                         isRelease, decl);
          noteRetainReleaseOrigin(*Impl, attrs[0]->getLocation(), isRelease);
        }
        return false;
      }
      return true;
    };
    bool retainOk = checkOp(/*isRelease=*/false);
    bool releaseOk = checkOp(/*isRelease=*/true);
    if (!retainOk || !releaseOk)
      return false;

    if (hasMixedImmortality()) {
      if (Impl) {
        Impl->diagnose(loc, diag::reference_type_mixed_immortal_marker, decl);
        noteRetainReleaseOrigin(*Impl, retainAttrs[0]->getLocation());
      }
      return false;
    }
    return true;
  }

private:
  const clang::RecordDecl *decl;
  StringRef retainName, releaseName;
  llvm::SmallVector<const clang::SwiftAttrAttr *, 1> retainAttrs, releaseAttrs;
};

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
      auto rrInfo = RetainReleaseInfo(checkedDecl);
      if (rrInfo.isImmortal())
        return ForeignReferenceTypeInfo::Immortal(checkedDecl,
                                                  /*primarySuperclass=*/nullptr,
                                                  /*isValid=*/rrInfo.isValid());
      else
        return ForeignReferenceTypeInfo::Shared(checkedDecl,
                                                /*primarySuperclass=*/nullptr,
                                                /*isValid=*/rrInfo.isValid());
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
    bool seenShared = false, seenMultipleShared = false, seenImmortal = false,
         seenInvalidOps = false;

    for (auto *base : FRTBases) {
      auto rrInfo = RetainReleaseInfo(base);
      seenInvalidOps |= !rrInfo.isValid();
      if (rrInfo.isImmortal()) {
        seenImmortal = true;
      } else {
        if (!FRTBase) {
          FRTBase = base;
          seenShared = true;
        } else {
          seenMultipleShared = true;
        }
      }
    }

    // If there are no shared references, FRTBase is the first immortal base.
    if (!FRTBase) {
      ASSERT(seenImmortal && "should have encountered immortal FRTBase");
      FRTBase = FRTBases.front();
    }

    if (seenMultipleShared || (seenShared && seenImmortal) || seenInvalidOps) {
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

    if (seenImmortal)
      return ForeignReferenceTypeInfo::Immortal(FRTBase, primarySuperclass);

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

  // A swift_attr propagates to later redeclarations only, so an earlier
  // declaration does not see the annotation, and the retain/release parameters
  // of SWIFT_SHARED_REFERENCE declare the type before the annotated declaration
  // is reached. Answer for the declaration that carries the information: the
  // definition when there is one, since reference-ness can also be inherited
  // from a base class, and otherwise the declaration spelling the annotation.
  // Clients can then use the request without walking the chain themselves.
  if (auto *definition = decl->getDefinition()) {
    decl = definition;
  } else if (!importer::hasImportReferenceAttr(decl)) {
    for (auto *redecl : decl->redecls()) {
      auto *record = cast<clang::RecordDecl>(redecl);
      if (importer::hasImportReferenceAttr(record)) {
        decl = record;
        break;
      }
    }
  }

  if (auto *cxxDecl = dyn_cast<clang::CXXRecordDecl>(decl))
    return ForeignReferenceTypeChecker(cxxDecl).check();

  // If this isn't a C++ record, then there's no inheritance (nor any of the
  // associated complications) to worry about. Just look for ref attributes.

  if (importer::hasImportReferenceAttr(decl)) {
    auto rrInfo = RetainReleaseInfo(decl);
    if (rrInfo.isImmortal())
      return ForeignReferenceTypeInfo::Immortal(decl,
                                                /*primarySuperclass=*/nullptr,
                                                /*isValid=*/rrInfo.isValid());
    else
      return ForeignReferenceTypeInfo::Shared(decl,
                                              /*primarySuperclass=*/nullptr,
                                              /*isValid=*/rrInfo.isValid());
  }

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
  if (!info.isReference() || info.isImmortal())
    return; // recordDecl is not a shared reference type

  if (importer::matchSwiftAttr<bool>(
          info.getDecl(), {{"returned_as_unretained_by_default", true}}))
    return;

  // If this returns OSObject or one of its subclasses, rely on libkern's
  // ownership rules.
  if (importer::getLibkernOwnershipOfReturnedFRT(clangFunc, ctx))
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

static bool isOSObject(const clang::CXXRecordDecl *record) {
  return record && record->getIdentifier() && record->getName() == "OSObject" &&
         record->getDeclContext()->getRedeclContext()->isTranslationUnit();
}

static bool isOSIterator(const clang::CXXRecordDecl *record) {
  return record && record->getIdentifier() &&
         record->getName() == "OSIterator" &&
         record->getDeclContext()->getRedeclContext()->isTranslationUnit();
}

LibkernSubclass ClangImporter::Implementation::getLibkernSubclass(
    const clang::CXXRecordDecl *record) {
  if (!record || !record->hasDefinition())
    return LibkernSubclass::None;

  record = record->getDefinition();
  auto it = libkernSubclasses.find(record);
  if (it != libkernSubclasses.end())
    return it->second;

  // OSIterator is the strongest answer there is, so no base can change it.
  if (isOSIterator(record)) {
    libkernSubclasses[record] = LibkernSubclass::OSIterator;
    return LibkernSubclass::OSIterator;
  }

  auto result =
      isOSObject(record) ? LibkernSubclass::OSObject : LibkernSubclass::None;

  for (const auto &base : record->bases()) {
    auto baseSubclass =
        getLibkernSubclass(base.getType()->getAsCXXRecordDecl());
    result = std::max(result, baseSubclass);
    if (result == LibkernSubclass::OSIterator)
      break;
  }

  libkernSubclasses[record] = result;
  return result;
}

std::optional<ResultConvention>
swift::importer::getOwnershipOfReturnedFRT(const clang::NamedDecl *decl,
                                           ASTContext &ctx) {

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

    if (auto convention = getLibkernOwnershipOfReturnedFRT(decl, ctx))
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

std::optional<ResultConvention>
swift::importer::getLibkernOwnershipOfReturnedFRT(const clang::NamedDecl *decl,
                                                  ASTContext &ctx) {
  if (!ctx.LangOpts.hasFeature(Feature::LibkernOwnershipConventions))
    return std::nullopt;

  auto *func = dyn_cast<clang::FunctionDecl>(decl);
  if (!func)
    return std::nullopt;

  auto *recordDecl = getReturnTypeAsRecordDeclPtr(func);
  if (!recordDecl)
    return std::nullopt;

  auto *importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
  auto libkernSubclass = importer->getLibkernSubclass(recordDecl);
  if (libkernSubclass == LibkernSubclass::None)
    return std::nullopt;

  if (!func->getIdentifier())
    return std::nullopt;

  auto consumeSynthesizedPrefixes = [](StringRef &funcName) -> bool {
    bool consumed = false;
    while (funcName.consume_front("__synthesizedVirtualCall_") ||
           funcName.consume_front("__synthesizedBaseCall_"))
      consumed = true;
    return consumed;
  };

  StringRef funcName = func->getName();
  // If this is a synthesized thunk, consume the prefix we added.
  if (func->isImplicit()) {
    if (!consumeSynthesizedPrefixes(funcName))
      return std::nullopt;
    if (funcName.starts_with("operator"))
      return std::nullopt;
  }

  // Strip leading underscores.
  funcName = funcName.substr(funcName.find_first_not_of('_'));

  if (funcName == "safeMetaCast" || funcName == "requiredMetaCast" ||
      funcName == "metaCast")
    return ResultConvention::Unowned;

  if (funcName.ends_with("Matching"))
    return std::nullopt;

  if ((!funcName.starts_with("get") && !funcName.starts_with("Get")) ||
      libkernSubclass == LibkernSubclass::OSIterator)
    return ResultConvention::Owned;

  return ResultConvention::Unowned;
}

//===----------------------------------------------------------------------===//
// Foreign reference type retain/release operations
//===----------------------------------------------------------------------===//

/// Whether \p op is a valid retain/release operation for the foreign reference
/// type \p classDecl (\p isRetain selects which). If \p Impl is non-null, the
/// specific problem is diagnosed at \p loc; otherwise the check is silent (used
/// to disambiguate an overloaded operation name).
static bool checkRefCountOperation(const ClassDecl *classDecl, ValueDecl *op,
                                   bool isRetain, StringRef name,
                                   ClangImporter::Implementation *Impl,
                                   HeaderLoc loc) {
  auto diagnose = [&Impl, &loc](auto diag, auto &&...args) {
    if (Impl)
      Impl->diagnose(loc, diag, std::forward<decltype(args)>(args)...);
  };

  auto *fn = dyn_cast<FuncDecl>(op);
  if (!fn) {
    diagnose(diag::foreign_reference_types_retain_release_not_a_function_decl,
             !isRetain, name);
    return false;
  }

  if (fn->isStatic()) {
    diagnose(
        diag::foreign_reference_types_retain_release_not_an_instance_function,
        !isRetain, name);
    return false;
  }

  // Instance operations take no parameters; free operations take one.
  if (fn->getParameters()->size() != (fn->isInstanceMember() ? 0 : 1)) {
    diagnose(diag::foreign_reference_retain_release_param_type, !isRetain, name,
             classDecl->getNameStr());
    return false;
  }

  Type paramType;
  NominalTypeDecl *paramDecl;
  if (fn->isInstanceMember()) {
    paramDecl = cast<NominalTypeDecl>(fn->getParent());
    paramType = paramDecl->getDeclaredInterfaceType();
  } else {
    paramType = fn->getParameters()
                    ->get(0)
                    ->getInterfaceType()
                    ->lookThroughSingleOptionalType();
    paramDecl = paramType->getAnyNominal();
  }

  // The return type must be void or an integer
  auto resultTy = fn->getResultInterfaceType();
  bool validReturn =
      resultTy->isVoid() || resultTy->isUInt() || resultTy->isUInt8() ||
      resultTy->isUInt16() || resultTy->isUInt32() || resultTy->isUInt64() ||
      resultTy->isInt() || resultTy->isInt8() || resultTy->isInt16() ||
      resultTy->isInt32() || resultTy->isInt64();
  // A retain may also return the parameter (self) type
  if (isRetain && !validReturn)
    validReturn = resultTy->lookThroughSingleOptionalType()->isEqual(paramType);
  if (!validReturn) {
    diagnose(diag::foreign_reference_retain_release_return_type, !isRetain,
             name);
    return false;
  }

  // The operation must take the FRT as its parameter
  if (paramDecl != classDecl) {
    auto *cxxDecl = dyn_cast<clang::CXXRecordDecl>(classDecl->getClangDecl());
    auto *paramCxxDecl = dyn_cast_or_null<clang::CXXRecordDecl>(
        paramDecl ? paramDecl->getClangDecl() : nullptr);
    if (cxxDecl && paramCxxDecl && cxxDecl->isDerivedFrom(paramCxxDecl)) {
      // The parameter may also be one of the FRT's bases
    } else {
      diagnose(diag::foreign_reference_retain_release_param_type, !isRetain,
               name, classDecl->getNameStr());
      return false;
    }
  }
  return true;
}

/// Resolve and diagnose the retain (\p isRetain) or release operation for the
/// foreign reference type \p classDecl, based on the annotation read from
/// \p annotatedDecl. Returns null on any (semantic) error.
///
/// This performs semantic checking only: the caller guarantees, via
/// \c RetainReleaseInfo::checkShape, that \p annotatedDecl has exactly one
/// \c retain: and one \c release: attribute, and (via the FRT request) that
/// neither is immortal.
static ValueDecl *
resolveRefCountOperation(const ClassDecl *classDecl,
                         const ClassDecl *annotatedDecl, StringRef name,
                         bool isRetain, ClangImporter::Implementation &Impl) {
  auto *record = cast<clang::RecordDecl>(classDecl->getClangDecl());
  HeaderLoc loc(record->getLocation());

  ASSERT(!name.empty() &&
         "structural check should guarantee a retain/release attr");
  ASSERT(name != "immortal" && "immortal FRTs are handled before resolution");

  auto results = importer::getValueDeclsForName(
      const_cast<ClassDecl *>(annotatedDecl), name);

  // Pick the operation, silently disambiguating an overloaded name.
  ValueDecl *op = nullptr;
  if (results.size() == 1) {
    op = results.front();
  } else {
    for (auto *candidate : results) {
      if (!checkRefCountOperation(classDecl, candidate, isRetain, name,
                                  /*Impl=*/nullptr, loc))
        continue;
      if (op) {
        Impl.diagnose(loc,
                      diag::too_many_reference_type_retain_release_operations,
                      !isRetain, name, record);
        return nullptr;
      }
      op = candidate;
    }
  }

  if (!op) {
    Impl.diagnose(loc, diag::foreign_reference_types_cannot_find_retain_release,
                  !isRetain, name, record);
    if (!Impl.SwiftContext.LangOpts.DisableExperimentalClangImporterDiagnostics)
      Impl.diagnoseTopLevelValue(
          DeclName(Impl.SwiftContext.getIdentifier(name)));
    return nullptr;
  }

  checkRefCountOperation(classDecl, op, isRetain, name, &Impl, loc);
  return op;
}

/// Synthesize an inline C++ method on \p clangDecl that forwards to \p baseFn.
/// Returns the synthesized method, or nullptr on failure.
///
/// Unlike SwiftDeclSynthesizer::synthesizeCXXForwardingMethod, this function
/// does not use Sema::SynthesizedFunctionScope (which is not re-entrant), so
/// it is safe to use during importing.
static const clang::CXXMethodDecl *
synthesizeForwardingRefCountMethod(clang::CXXRecordDecl *clangDecl,
                                   const clang::FunctionDecl *baseFn,
                                   ClangImporter::Implementation &Impl) {
  if (!baseFn)
    return nullptr;
  auto &clangCtx = Impl.getClangASTContext();
  auto &clangSema = Impl.getClangSema();

  clang::QualType methodType = clangCtx.getFunctionType(
      clangCtx.VoidTy, {}, clang::FunctionProtoType::ExtProtoInfo{});

  auto loc = baseFn->getLocation();
  auto &ident = clangCtx.Idents.get("__synthesized_lifetimeAccessor_" +
                                    baseFn->getNameAsString());
  clang::DeclarationName methodName(&ident);
  auto method = clang::CXXMethodDecl::Create(
      clangCtx, clangDecl, baseFn->getSourceRange().getBegin(),
      clang::DeclarationNameInfo(methodName, clang::SourceLocation()),
      methodType, clangCtx.getTrivialTypeSourceInfo(methodType), clang::SC_None,
      /*usesFPIntrin=*/false, /*isInline=*/true,
      clang::ConstexprSpecKind::Unspecified, baseFn->getSourceRange().getEnd());
  method->setImplicit();
  method->setImplicitlyInline();
  method->setAccess(clang::AccessSpecifier::AS_public);
  method->addAttr(clang::NoDebugAttr::CreateImplicit(clangCtx));

  clang::Expr *argExpr =
      clang::CXXThisExpr::Create(clangCtx, clang::SourceLocation(),
                                 method->getThisType(), /*IsImplicit=*/false);

  if (auto calledMethod = dyn_cast<clang::CXXMethodDecl>(baseFn)) {
    if (calledMethod->isStatic())
      return nullptr;
    auto memberExpr = clangSema.BuildMemberExpr(
        argExpr, /*isArrow=*/true, loc, clang::NestedNameSpecifierLoc(),
        clang::SourceLocation(),
        const_cast<clang::CXXMethodDecl *>(calledMethod),
        clang::DeclAccessPair::make(
            const_cast<clang::CXXMethodDecl *>(calledMethod), clang::AS_public),
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
        const_cast<clang::FunctionDecl *>(baseFn),
        /*RefersToEnclosingVariableOrCapture=*/false, loc, baseFn->getType(),
        clang::VK_LValue);
    auto call =
        clangSema.BuildCallExpr(nullptr, fnExpr, clang::SourceLocation(),
                                {argExpr}, clang::SourceLocation());
    method->setBody(clang::CompoundStmt::Create(
        clangCtx, {call.get()}, clang::FPOptionsOverride(), loc, loc));
  }
  return method;
}

/// Synthesize forwarding retain/release methods on the derived FRT \p decl that
/// performs the derived-to-base adjustment on behalf of Swift. This function
/// assumes both the derived and base types have reachable definitions.
static std::pair<const clang::CXXMethodDecl *, const clang::CXXMethodDecl *>
synthesizeInheritedRefCountOperations(ClassDecl *decl,
                                      clang::CXXRecordDecl *clangDecl,
                                      const clang::FunctionDecl *baseRetainFn,
                                      const clang::FunctionDecl *baseReleaseFn,
                                      ClangImporter::Implementation &Impl) {
  auto &context = Impl.SwiftContext;
  auto &clangCtx = Impl.getClangASTContext();

  auto synthesizedRetain =
      synthesizeForwardingRefCountMethod(clangDecl, baseRetainFn, Impl);
  auto synthesizedRelease =
      synthesizeForwardingRefCountMethod(clangDecl, baseReleaseFn, Impl);
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
    ClassDecl *classDecl, ClangImporter::Implementation &Impl) {
  auto *recordDecl = cast<clang::RecordDecl>(classDecl->getClangDecl());
  auto *cxxRecordDecl = dyn_cast<clang::CXXRecordDecl>(recordDecl);
  auto frtInfo =
      evaluateOrDefault(Impl.SwiftContext.evaluator,
                        ForeignReferenceTypeInfoRequest({recordDecl}), {});

  // Where classDecl inherits its FRT annotations. When this is null, classDecl
  // (i.e., recordDecl) is directly annotated itself (i.e., does not inherit).
  auto *baseCxxRecordDecl =
      dyn_cast_or_null<clang::CXXRecordDecl>(frtInfo.getDecl());

  // Imported class of whatever was annotated.
  // FIXME: should not be necessary to keep track of this, it is confusing.
  const ClassDecl *annotatedClassDecl = classDecl;

  // Compare canonical decls: frtInfo may name a different redeclaration of
  // recordDecl, which is not inheritance.
  if (cxxRecordDecl && baseCxxRecordDecl &&
      baseCxxRecordDecl->getCanonicalDecl() != cxxRecordDecl->getCanonicalDecl())
    annotatedClassDecl = cast<ClassDecl>(
        Impl.importDecl(baseCxxRecordDecl, Impl.CurrentVersion));
  else
    baseCxxRecordDecl = nullptr;

  // Check that the record carrying the retain:/release: attributes has exactly
  // one of each. Only diagnose when those attributes are on this type, to avoid
  // repeating diagnostics once per derived type.
  //
  // frtInfo names the declaration the annotation was found on, which for a
  // forward-declared type need not be the one classDecl was imported from.
  const clang::RecordDecl *annotatedDecl =
      baseCxxRecordDecl ? baseCxxRecordDecl
                        : (frtInfo.getDecl() ? frtInfo.getDecl() : recordDecl);

  auto rrInfo = RetainReleaseInfo(annotatedDecl);
  if (!rrInfo.checkShape(/*Impl=*/baseCxxRecordDecl ? nullptr : &Impl))
    return;

  if (!frtInfo.isValid())
    // If this FRT is invalid for any other reason, do not resolve or synthesize
    // retain/release operations and just bail.
    return;

  // Immortal FRTs have no custom reference counting, so there is nothing to
  // resolve or synthesize.
  if (frtInfo.isImmortal()) {
    Impl.setForeignReferenceTypeOperations(recordDecl, /*retain=*/nullptr,
                                           /*release=*/nullptr);
    return;
  }

  auto isReachable = [&Impl](const clang::CXXRecordDecl *Decl) -> bool {
    if (!Decl->getDefinition())
      return false;
    clang::Sema::SFINAETrap trap(Impl.getClangSema());
    return Impl.getClangSema().hasReachableDefinition(
        const_cast<clang::CXXRecordDecl *>(Decl));
  };

  // An FRT by inheritance must have a reachable definition; check that here.
  // (A directly-annotated FRT may be forward-declared.)
  if (baseCxxRecordDecl &&
      (!isReachable(cxxRecordDecl) || !isReachable(baseCxxRecordDecl))) {
    Impl.diagnose(HeaderLoc(recordDecl->getLocation()),
                  diag::foreign_reference_type_unreachable,
                  classDecl->getNameStr());
    return;
  }

  // Resolve (and semantically diagnose) the retain/release operations.
  ValueDecl *retainOp = resolveRefCountOperation(classDecl, annotatedClassDecl,
                                                 rrInfo.getRetain(),
                                                 /*isRetain=*/true, Impl);
  ValueDecl *releaseOp = resolveRefCountOperation(classDecl, annotatedClassDecl,
                                                  rrInfo.getRelease(),
                                                  /*isRetain=*/false, Impl);

  const clang::FunctionDecl *retainFn = nullptr;
  const clang::FunctionDecl *releaseFn = nullptr;

  // Look through a cloned (inherited) member to the original base method,
  // without forcing any synthesis (getCalledBaseCxxMethod would call
  // getBody()).
  auto baseClangFn = [&](ValueDecl *op, bool *cloned = nullptr) {
    if (auto *original = Impl.getOriginalForClonedMember(op)) {
      op = original;
      if (cloned)
        *cloned = true;
    } else {
      if (cloned)
        *cloned = false;
    }
    return dyn_cast_or_null<clang::FunctionDecl>(op->getClangDecl());
  };

  if (baseCxxRecordDecl && retainOp && releaseOp) {
    // FRT annotation was inherited: always synthesize forwarding methods that
    // call the base's operations, performing the derived-to-base adjustment.
    std::tie(retainFn, releaseFn) = synthesizeInheritedRefCountOperations(
        classDecl, const_cast<clang::CXXRecordDecl *>(cxxRecordDecl),
        baseClangFn(retainOp), baseClangFn(releaseOp), Impl);
  } else if (!baseCxxRecordDecl && retainOp && releaseOp) {
    // FRT annotation appears directly on clangDecl/cxxDecl.
    bool retainCloned = false, releaseCloned = false;
    retainFn = baseClangFn(retainOp, &retainCloned);
    releaseFn = baseClangFn(releaseOp, &releaseCloned);

    // Even if cxxDecl was itself directly annotated, its retain/release may
    // still be inherited from some (reachable) base. If so, synthesize
    // forwarding retain/release methods as well.
    if (cxxRecordDecl && (retainCloned || releaseCloned) &&
        isReachable(cxxRecordDecl)) {
      auto *cxxDeclMut = const_cast<clang::CXXRecordDecl *>(cxxRecordDecl);
      if (retainCloned) {
        if (auto *retainThunk =
                synthesizeForwardingRefCountMethod(cxxDeclMut, retainFn, Impl))
          retainFn = retainThunk;
      }
      if (releaseCloned) {
        if (auto *releaseThunk =
                synthesizeForwardingRefCountMethod(cxxDeclMut, releaseFn, Impl))
          releaseFn = releaseThunk;
      }
    }
  }
  Impl.setForeignReferenceTypeOperations(recordDecl, retainFn, releaseFn);
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

/// Whether the C++ standard library overlay in stdlib/public/Cxx already
/// provides a safe Swift API named after \p method:
///
/// - 'basic_string::append' vs 'std.string.append(_:)', which differs only in
///   its result type and so ambiguates every discarded-result call;
/// - 'set::insert' etc. vs 'CxxUniqueSet.insert(_:)', a protocol extension
///   member that a concrete C++ 'insert' would shadow outright;
/// - 'optional::value' vs the 'CxxOptional.value' property.
static bool overlayProvidesSafeWrapperNamed(const clang::CXXMethodDecl *method) {
  if (!method->getParent()->isInStdNamespace() || !method->getIdentifier())
    return false;

  return llvm::StringSwitch<bool>(method->getName())
      .Cases({"append", "insert", "value"}, true)
      .Default(false);
}

bool importer::keepsNameWhenImportedAsUnsafe(const clang::CXXMethodDecl *method,
                                             ASTContext &ctx) {
  return ctx.LangOpts.hasFeature(
             Feature::ImportUnsafeCxxMethodsAsAlwaysUnsafe) &&
         shouldRenameCXXMethodAsUnsafe(method, ctx) &&
         !overlayProvidesSafeWrapperNamed(method);
}
