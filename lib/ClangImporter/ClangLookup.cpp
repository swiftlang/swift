//===--- ClangLookup.cpp - Lookup in entities imported from Clang ---------===//
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
// This file contains facilities for name lookup in entites imported from Clang
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "SwiftDeclSynthesizer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Mangle.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Index/IndexingAction.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/iterator_range.h"
#include <utility>

using namespace swift;

namespace {
/// Collects name lookup results into the given tiny vector, for use in the
/// various ClangImporter lookup routines.
///
/// Validates that the name we looked up matches the resulting imported name.
class CollectLookupResults {
  /// Match by base name, since that is what MemberLookupTable is keyed on for
  /// laziness (i.e., see type of MemberLookupTable::isLazilyComplete).
  DeclBaseName name;
  TinyPtrVector<ValueDecl *> &result;

public:
  CollectLookupResults(DeclBaseName name, TinyPtrVector<ValueDecl *> &result)
      : name(name), result(result) {}

  void add(ValueDecl *imported) {
    if (imported->getBaseName() == name)
      result.push_back(imported);

    // Expand any macros introduced by the Clang importer.
    imported->visitAuxiliaryDecls([&](Decl *decl) {
      auto valueDecl = dyn_cast<ValueDecl>(decl);
      if (!valueDecl)
        return;

      // Bail out if the auxiliary decl was not produced by a macro.
      auto module = decl->getDeclContext()->getParentModule();
      auto *sf = module->getSourceFileContainingLocation(decl->getLoc());
      if (!sf || sf->Kind != SourceFileKind::MacroExpansion)
        return;

      // Only produce results that match the requested name.
      if (valueDecl->getBaseName() != name)
        return;

      result.push_back(valueDecl);
    });
  }
};
} // anonymous namespace

static SmallVector<SwiftLookupTable::SingleEntry, 4>
lookupInClassTemplateSpecialization(
    ASTContext &ctx, const clang::ClassTemplateSpecializationDecl *clangDecl,
    DeclName name) {
  // TODO: we could make this faster if we can cache class templates in the
  // lookup table as well.
  // Import all the names to figure out which ones we're looking for.
  SmallVector<SwiftLookupTable::SingleEntry, 4> found;
  for (auto member : clangDecl->decls()) {
    auto *namedDecl = dyn_cast<clang::NamedDecl>(member);
    if (!namedDecl)
      continue;

    auto memberName = ctx.getClangModuleLoader()->importName(namedDecl);
    if (!memberName)
      continue;

    // Use the base names here because *sometimes* our input name won't have
    // any arguments.
    if (name.getBaseName().compare(memberName.getBaseName()) == 0)
      found.push_back(namedDecl);
  }

  return found;
}

static SmallVector<SwiftLookupTable::SingleEntry, 4>
lookupInClangDecl(Decl *swiftDecl, DeclName name) {
  auto &ctx = swiftDecl->getASTContext();
  auto *whereDecl = swiftDecl->getClangDecl();
  ASSERT(whereDecl && "should only look up decls imported from Clang");

  // Class templates aren't in the lookup table.
  if (auto *spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(whereDecl))
    return lookupInClassTemplateSpecialization(ctx, spec, name);

  auto *clangModule =
      importer::getClangOwningModule(whereDecl, whereDecl->getASTContext());
  auto *lookupTable = ctx.getClangModuleLoader()->findLookupTable(clangModule);

  auto foundDecls = lookupTable->lookup(SerializedSwiftName(name.getBaseName()),
                                        EffectiveClangContext());

  // lookup() just gives us all decls in the module of the given name.
  // Make sure that `clangDecl` is the parent of all the members we found.
  SmallVector<SwiftLookupTable::SingleEntry, 4> result;
  for (auto entry : foundDecls) {
    auto *found = entry.dyn_cast<clang::NamedDecl *>();
    if (!found)
      continue; // What we found wasn't a NamedDecl

    auto *foundCtx = found->getDeclContext();
    if (auto *foundCtxDecl = dyn_cast<clang::Decl>(foundCtx)) {
      // Context of found decl is also a decl; compare canonical decl of each
      auto *whereCanonical = whereDecl->getCanonicalDecl();

      if (foundCtxDecl->getCanonicalDecl() == whereCanonical) {
        result.push_back(entry);
      } else if (auto *ED = dyn_cast<clang::EnumDecl>(foundCtxDecl);
                 ED && !ED->isScoped() && isa<clang::EnumConstantDecl>(found)) {
        // Enum constants can be found in the parent context of the enum decl.
        auto *enumCtx = dyn_cast<clang::Decl>(foundCtxDecl->getDeclContext());
        if (enumCtx && enumCtx->getCanonicalDecl() == whereCanonical)
          result.push_back(entry);
      }
    } else {
      // Context of found decl is not a decl; compare contexts directly
      if (foundCtx == cast<clang::DeclContext>(whereDecl))
        result.push_back(entry);
    }
  }

  return result;
}

TinyPtrVector<ValueDecl *> CXXNamespaceMemberLookup::evaluate(
    Evaluator &evaluator, CXXNamespaceMemberLookupDescriptor desc) const {
  DeclName name = desc.name;
  auto &ctx = desc.namespaceDecl->getASTContext();

  auto *theNamespace =
      cast<clang::NamespaceDecl>(desc.namespaceDecl->getClangDecl())
          ->getCanonicalDecl();

  TinyPtrVector<ValueDecl *> result;

  auto *lookupTable = ctx.getClangModuleLoader()->findLookupTable(nullptr);
  auto foundDecls = lookupTable->lookup(SerializedSwiftName(name.getBaseName()),
                                        EffectiveClangContext());

  CollectLookupResults collector(name.getBaseName(), result);
  llvm::SmallPtrSet<clang::NamedDecl *, 8> seenDecls;
  for (auto entry : foundDecls) {
    auto *foundDecl = entry.dyn_cast<clang::NamedDecl *>();
    if (!foundDecl)
      continue; // What we found wasn't a NamedDecl

    auto *foundCtx = foundDecl->getDeclContext();

    if (auto *ED = dyn_cast<clang::EnumDecl>(foundCtx);
        ED && !ED->isScoped() && isa<clang::EnumConstantDecl>(foundDecl)) {
      // enum constants are found in the enum decl's parent
      foundCtx = ED->getDeclContext();
    }

    auto found = false;
    while (auto *foundNamespace =
               dyn_cast_or_null<clang::NamespaceDecl>(foundCtx)) {
      // Compare theNamespace with the namespace enclosing the found decl,
      // as well as any outer namespaces if it is inline.
      if (foundNamespace->getCanonicalDecl() == theNamespace) {
        found = true;
        break;
      }

      if (!foundNamespace->isInline())
        break;

      foundCtx = foundNamespace->getParent();
    }

    if (!found)
      continue;

    if (!seenDecls.insert(foundDecl).second)
      continue; // We've already seen this; a re-declaration?
    if (auto *import =
            ctx.getClangModuleLoader()->importDeclDirectly(foundDecl))
      collector.add(cast<ValueDecl>(import));
  }
  return result;
}

llvm::SmallVector<ValueDecl *, 16> CXXNamespaceMemberEnumeration::evaluate(
    Evaluator &evaluator, CXXNamespaceMemberEnumerationDescriptor desc) const {
  auto *namespaceEnum = desc.namespaceEnum;
  auto *namespaceDecl =
      cast<clang::NamespaceDecl>(namespaceEnum->getClangDecl());
  auto &ctx = namespaceEnum->getASTContext();
  auto *clangModuleLoader = ctx.getClangModuleLoader();

  llvm::SmallVector<ValueDecl *, 16> result;

  // Keep track of which names we've seen and which decls we've imported.
  // When we lookup a member by name, we import all of them at once.
  llvm::SmallSet<DeclName, 16> seenNames;
  llvm::SmallPtrSet<Decl *, 16> seenImports;

  auto importMember = [&](const clang::NamedDecl *nd) {
    auto name = clangModuleLoader->importName(nd);
    if (!name || !seenNames.insert(name).second)
      return;

    auto importedMembers = evaluateOrDefault(
        ctx.evaluator,
        CXXNamespaceMemberLookup({cast<EnumDecl>(namespaceEnum), name}), {});

    for (auto *imported : importedMembers) {
      if (seenImports.insert(imported).second)
        result.push_back(imported);
    }
  };

  // If this is non-null, we will only import members from that module
  const clang::Module *owningModule = nullptr;
  if (!desc.includeOtherModules && namespaceDecl->getOwningModule())
    owningModule = namespaceDecl->getOwningModule()->getTopLevelModule();

  auto importSpecializations = [&](const clang::ClassTemplateDecl *tmpl) {
    if (!desc.includeSpecializations)
      return;

    // Add all specializations to a worklist so we don't accidentally mutate
    // the list of decls we're iterating over.
    llvm::SmallPtrSet<const clang::ClassTemplateSpecializationDecl *, 16>
        specWorklist;
    specWorklist.insert_range(tmpl->specializations());

    for (auto *spec : specWorklist) {
      auto *imported = clangModuleLoader->importDeclDirectly(spec);
      if (imported && seenImports.insert(imported).second)
        result.push_back(cast<ValueDecl>(imported));
    }
  };

  auto Redecls =
      llvm::SmallVector<clang::NamespaceDecl *, 2>(namespaceDecl->redecls());
  std::stable_sort(Redecls.begin(), Redecls.end(), [&](auto *LHS, auto *RHS) {
    // Sort according to module name, if any (a namespace redeclaration will not
    // have an owning Clang module if it is declared in a bridging header).
    if (!LHS->getOwningModule() || !RHS->getOwningModule())
      return (bool)LHS->getOwningModule() < (bool)RHS->getOwningModule();
    return LHS->getOwningModule()->Name < RHS->getOwningModule()->Name;
  });

  for (auto *redecl : Redecls) {
    // Skip namespace declarations that come from other top-level modules
    // if there's such a requirement (i.e., if owningModule is non-null)
    if (owningModule && redecl->getOwningModule() &&
        owningModule != redecl->getOwningModule()->getTopLevelModule())
      continue;

    for (auto *member : redecl->decls()) {
      if (auto *classTemplate = dyn_cast<clang::ClassTemplateDecl>(member)) {
        importSpecializations(classTemplate);
      }

      if (auto *nd = dyn_cast<clang::NamedDecl>(member)) {
        importMember(nd);
      }

      // Unscoped enums have their enumerators present in the parent namespace.
      if (auto *ed = dyn_cast<clang::EnumDecl>(member); ed && !ed->isScoped()) {
        for (const auto *ecd : ed->enumerators())
          importMember(ecd);
      }
    }
  }
  return result;
}

TinyPtrVector<ValueDecl *> ClangRecordMemberLookup::evaluate(
    Evaluator &evaluator, ClangRecordMemberLookupDescriptor desc) const {
  NominalTypeDecl *recordDecl = desc.recordDecl;
  NominalTypeDecl *inheritingDecl = desc.inheritingDecl;
  DeclName name = desc.name;
  ClangInheritanceInfo inheritance = desc.inheritance;

  auto &ctx = recordDecl->getASTContext();
  auto &Importer = *static_cast<ClangImporter *>(ctx.getClangModuleLoader());

  // Whether to skip non-public members. Feature::ImportNonPublicCxxMembers says
  // to import all non-public members by default; if that is disabled, we only
  // import non-public members annotated with SWIFT_PRIVATE_FILEID (since those
  // are the only classes that need non-public members.)
  auto *cxxRecordDecl =
      dyn_cast<clang::CXXRecordDecl>(inheritingDecl->getClangDecl());
  auto skipIfNonPublic =
      !ctx.LangOpts.hasFeature(Feature::ImportNonPublicCxxMembers) &&
      cxxRecordDecl && importer::getPrivateFileIDAttrs(cxxRecordDecl).empty();

  auto directResults = lookupInClangDecl(recordDecl, name);

  // The set of declarations we found.
  TinyPtrVector<ValueDecl *> result;
  CollectLookupResults collector(name.getBaseName(), result);

  // Find the results that are actually a member of "recordDecl".
  ClangModuleLoader *clangModuleLoader = ctx.getClangModuleLoader();
  for (auto foundEntry : directResults) {
    auto found = cast<clang::NamedDecl *>(foundEntry);
    if (dyn_cast<clang::Decl>(found->getDeclContext()) !=
        recordDecl->getClangDecl())
      continue;

    // We should not import 'found' if the following are all true:
    //
    // -  Feature::ImportNonPublicCxxMembers is not enabled
    // -  'found' is not a member of a SWIFT_PRIVATE_FILEID-annotated class
    // -  'found' is a non-public member.
    // -  'found' is not a non-inherited FieldDecl; we must import private
    //    fields because they may affect implicit conformances that iterate
    //    through all of a struct's fields, e.g., Sendable (#76892).
    //
    // Note that we can skip inherited FieldDecls because implicit conformances
    // handle those separately.
    //
    // The first two conditions are captured by skipIfNonPublic. The next two
    // are conveyed by the following:
    auto nonPublic = found->getAccess() == clang::AS_private ||
                     found->getAccess() == clang::AS_protected;
    auto noninheritedField = !inheritance && isa<clang::FieldDecl>(found);
    if (skipIfNonPublic && nonPublic && !noninheritedField)
      continue;

    // Don't import constructors on foreign reference types.
    if (isa<clang::CXXConstructorDecl>(found) && isa<ClassDecl>(recordDecl))
      continue;

    auto imported = clangModuleLoader->importDeclDirectly(found);
    if (!imported)
      continue;

    // If this member is found due to inheritance, clone it from the base class
    // by synthesizing getters and setters.
    if (inheritance) {
      imported = clangModuleLoader->importBaseMemberDecl(
          cast<ValueDecl>(imported), inheritingDecl, inheritance);
      if (!imported)
        continue;
    }

    collector.add(cast<ValueDecl>(imported));
  }

  if (inheritance) {
    // For inherited members, add members that are synthesized eagerly, such as
    // subscripts. This is not necessary for non-inherited members because those
    // should already be in the lookup table.
    for (auto member :
         cast<NominalTypeDecl>(recordDecl)->getCurrentMembersWithoutLoading()) {
      auto namedMember = dyn_cast<ValueDecl>(member);
      if (!namedMember || !namedMember->hasName() ||
          namedMember->getName().getBaseName() != name ||
          clangModuleLoader->isMemberSynthesizedPerType(namedMember) ||
          clangModuleLoader->getOriginalForClonedMember(namedMember))
        continue;

      auto *imported = clangModuleLoader->importBaseMemberDecl(
          namedMember, inheritingDecl, inheritance);
      if (!imported)
        continue;

      collector.add(imported);
    }
  }

  // If this is a C++ record, look through any base classes.
  const clang::CXXRecordDecl *cxxRecord;
  if ((cxxRecord =
           dyn_cast<clang::CXXRecordDecl>(recordDecl->getClangDecl())) &&
      cxxRecord->isCompleteDefinition()) {
    // Capture the arity of already found members in the
    // current record, to avoid adding ambiguous members
    // from base classes.
    llvm::SmallSet<DeclName, 4> foundMethodNames;
    for (const auto *valueDecl : result)
      foundMethodNames.insert(valueDecl->getName());

    for (auto base : cxxRecord->bases()) {
      if (skipIfNonPublic && base.getAccessSpecifier() != clang::AS_public)
        continue;

      clang::QualType baseType = base.getType();
      if (auto spectType =
              dyn_cast<clang::TemplateSpecializationType>(baseType))
        baseType = spectType->desugar();
      if (!isa<clang::RecordType>(baseType.getCanonicalType()))
        continue;

      auto *baseRecord = baseType->getAs<clang::RecordType>()->getDecl();

      if (importer::isSymbolicCircularBase(cxxRecord, baseRecord))
        // Skip circular bases to avoid unbounded recursion
        continue;

      if (auto import = clangModuleLoader->importDeclDirectly(baseRecord)) {
        // If we are looking up the base class, go no further. We will have
        // already found it during the other lookup.
        if (cast<ValueDecl>(import)->getName() == name)
          continue;

        auto baseInheritance = ClangInheritanceInfo(inheritance, base);

        // Add Clang members that are imported lazily.
        auto baseResults = evaluateOrDefault(
            ctx.evaluator,
            ClangRecordMemberLookup({cast<NominalTypeDecl>(import), name,
                                     inheritingDecl, baseInheritance}),
            {});

        for (auto foundInBase : baseResults) {
          // Do not add duplicate entry with the same DeclName,
          // as that would cause an ambiguous lookup.
          if (foundMethodNames.count(foundInBase->getName()))
            continue;

          collector.add(foundInBase);
        }
      }
    }
  }

  if (result.empty() && !inheritance) {
    if (name.isSimpleName("pointee")) {
      if (auto *pointee = Importer.Impl.lookupAndImportPointee(inheritingDecl))
        result.push_back(pointee);
    } else if (name.getBaseName() == "successor" &&
               name.getArgumentNames().size() == 0) {
      if (auto *succ = Importer.Impl.lookupAndImportSuccessor(inheritingDecl))
        result.push_back(succ);
    }
  }

  return result;
}

/// Perform a qualified lookup for \a name in \a Record, suppressing diagnostics
/// and returning std::nullopt if the lookup was unsuccessful.
static std::optional<clang::LookupResult>
lookupCXXMember(clang::Sema &Sema, clang::DeclarationName name,
                const clang::CXXRecordDecl *Record) {
  auto R = clang::LookupResult(Sema, name, clang::SourceLocation(),
                               clang::Sema::LookupMemberName);
  R.suppressDiagnostics();
  // NOTE: this will not find free-standing operator decl, i.e. not a member
  Sema.LookupQualifiedName(R, const_cast<clang::CXXRecordDecl *>(Record));

  switch (R.getResultKind()) {
  case clang::LookupResultKind::Found:
  case clang::LookupResultKind::FoundOverloaded:
    return R;
  default:
    return std::nullopt; // Lookup didn't yield any results
  }
}

namespace {
/// Convenience wrapper around clang::LookupResult::iterator that yields
/// std::pair<NamedDecl *, AccessSpecifier> rather than just NamedDecl *.
struct ResultIterator
    : llvm::iterator_adaptor_base<
          ResultIterator, clang::LookupResult::iterator,
          clang::LookupResult::iterator::iterator_category,
          std::pair<const clang::NamedDecl *, clang::AccessSpecifier>> {
  explicit ResultIterator(const clang::LookupResult::iterator &it) { I = it; }
  value_type operator*() const {
    return std::make_pair(I.getDecl(), I.getAccess());
  }
};
} // namespace

/// Creates an iterable range of method decl-access pairs for the results found
/// in \a R, originating from a qualified lookup in \a Origin.
///
/// This helper for the lookupAndImport* functions encapsulates casting the
/// decls to clang::CXXMethodDecl *s and "looking through" using decls. It also
/// ensures the access is set to clang::AS_none if the method is not inherited.
static auto filterMethodOverloads(clang::LookupResult &R,
                                  const clang::CXXRecordDecl *Origin) {
  return llvm::make_filter_range(
      llvm::map_range(
          llvm::make_range(ResultIterator{R.begin()}, ResultIterator{R.end()}),
          [=](std::pair<const clang::NamedDecl *, clang::AccessSpecifier> da) {
            auto [nd, access] = da;

            if (auto *usd = dyn_cast<clang::UsingShadowDecl>(nd))
              nd = usd->getTargetDecl();

            if (nd->getDeclContext() == Origin)
              access = clang::AS_none; // not inherited

            auto *md = dyn_cast<clang::CXXMethodDecl>(nd);
            return std::make_pair(md, access);
          }),
      [](std::pair<const clang::CXXMethodDecl *, clang::AccessSpecifier> da) {
        return da.first;
      });
}

/// Imports a C++ \a method to a Swift \a struct as a non-inherited member when
/// \a access is AS_none, or an inherited member with effective \a access
/// otherwise. Marks the method as unavailable with \a unavailabilityMsg.
///
/// Helper for the lookupAndImport* functions.
static FuncDecl *importUnavailableMethod(ClangImporter::Implementation &Impl,
                                         const clang::CXXMethodDecl *method,
                                         clang::AccessSpecifier access,
                                         NominalTypeDecl *Struct,
                                         StringRef unavailabilityMsg) {
  auto *func =
      dyn_cast_or_null<FuncDecl>(Impl.importDecl(method, Impl.CurrentVersion));
  if (!func)
    return nullptr;
  if (auto inheritance = ClangInheritanceInfo(access))
    func = dyn_cast_or_null<FuncDecl>(
        Impl.importBaseMemberDecl(func, Struct, inheritance));
  if (!func)
    return nullptr;
  Impl.markUnavailable(func, unavailabilityMsg);
  return func;
}

VarDecl *
ClangImporter::Implementation::lookupAndImportPointee(NominalTypeDecl *Struct) {
  const auto *CXXRecord =
      dyn_cast<clang::CXXRecordDecl>(Struct->getClangDecl());

  if (!CXXRecord)
    return nullptr;

  if (auto [it, inserted] = importedPointeeCache.try_emplace(Struct, nullptr);
      !inserted)
    return it->second;

  // From this point onward, if we encounter some error and return, future calls
  // to this function will pick up the nullptr we cached using try_emplace. Note
  // that this may silently suppress unintentional cycles.

  auto &Ctx = getClangASTContext();
  auto &Sema = getClangSema();

  auto name = Ctx.DeclarationNames.getCXXOperatorName(
      clang::OverloadedOperatorKind::OO_Star);
  auto R = lookupCXXMember(Sema, name, CXXRecord);
  if (!R.has_value())
    return nullptr;

  auto overloads = filterMethodOverloads(R.value(), CXXRecord);

  const clang::CXXMethodDecl *CXXGetter = nullptr, *CXXSetter = nullptr;
  clang::AccessSpecifier CXXGetterAccess, CXXSetterAccess;

  for (auto [method, access] : overloads) {
    if (method->isStatic() || method->isVolatile() ||
        method->getMinRequiredArguments() != 0 ||
        method->getRefQualifier() == clang::RQ_RValue)
      continue;

    // A setter is anything that returns a mutable reference; anything else is
    // a getter.
    auto retTy = method->getReturnType();
    bool isSetter =
        retTy->isReferenceType() && !retTy->getPointeeType().isConstQualified();

    if (isSetter) {
      if (!CXXSetter) {
        CXXSetter = method;
        CXXSetterAccess = access;
      } else if (!CXXSetter->isConst() && method->isConst()) {
        // Previously found a non-const setter, but prefer a const setter
        CXXSetter = method;
        CXXSetterAccess = access;
      }
    }

    // NOTE: a setter can also be used as a getter, so we don't check !isSetter
    if (!CXXGetter) {
      CXXGetter = method;
      CXXGetterAccess = access;
    } else if (!CXXGetter->isConst() && method->isConst()) {
      // Previously found a non-const getter, but prefer a const getter
      CXXGetter = method;
      CXXGetterAccess = access;
    }
  }

  if (!CXXGetter && !CXXSetter)
    return nullptr;

  FuncDecl *getter = nullptr, *setter = nullptr;

  if (CXXSetter) {
    setter = importUnavailableMethod(*this, CXXSetter, CXXSetterAccess, Struct,
                                     "use .pointee property");
    if (!setter)
      return nullptr;
  }

  if (CXXGetter == CXXSetter) {
    getter = setter;
  } else if (CXXGetter) {
    getter = importUnavailableMethod(*this, CXXGetter, CXXGetterAccess, Struct,
                                     "use .pointee property");
    if (!getter)
      return nullptr;
  }

  SwiftDeclSynthesizer synth{*this};
  auto *pointee = synth.makeDereferencedPointeeProperty(getter, setter);
  if (!pointee)
    return nullptr;

  importAttributes(CXXGetter ? CXXGetter : CXXSetter, pointee);

  Struct->addMember(pointee);
  importedPointeeCache[Struct] = pointee;
  return pointee;
}

FuncDecl *ClangImporter::Implementation::lookupAndImportSuccessor(
    NominalTypeDecl *Struct) {
  const auto *CXXRecord =
      dyn_cast<clang::CXXRecordDecl>(Struct->getClangDecl());

  if (!CXXRecord || !isa<StructDecl>(Struct))
    // Do not synthesize successor() if this is not a C++ record, or if it is
    // a foreign reference type (successor() needs to copy values of this type),
    // which would be a ClassDecl rather than a StructDecl.
    return nullptr;

  if (auto [it, inserted] = importedSuccessorCache.try_emplace(Struct, nullptr);
      !inserted)
    return it->second;

  // From this point onward, if we encounter some error and return, future calls
  // to this function will pick up the nullptr we cached using try_emplace. Note
  // that this may silently suppress unintentional cycles.

  auto &Ctx = getClangASTContext();
  auto &Sema = getClangSema();

  auto name = Ctx.DeclarationNames.getCXXOperatorName(
      clang::OverloadedOperatorKind::OO_PlusPlus);
  auto R = lookupCXXMember(Sema, name, CXXRecord);
  if (!R.has_value())
    return nullptr;

  auto overloads = filterMethodOverloads(R.value(), CXXRecord);

  const clang::CXXMethodDecl *CXXMethod = nullptr;
  clang::AccessSpecifier CXXMethodAccess;

  for (auto [method, access] : overloads) {
    if (method->isStatic() || method->isVolatile() ||
        method->getRefQualifier() == clang::RQ_RValue ||
        method->getMinRequiredArguments() != 0)
      continue;

    // FIXME: we currently only support non-const overloads of operator++,
    // so skip const overloads for now. Synthesizing a nonmutating .successor()
    // from a const overload should be possible but currently causes a crash.

    if (method->isConst())
      continue;

    CXXMethod = method;
    CXXMethodAccess = access;
  }

  if (!CXXMethod)
    return nullptr;

  auto *incr = importUnavailableMethod(*this, CXXMethod, CXXMethodAccess,
                                       Struct, "use .pointee property");
  if (!incr)
    return nullptr;

  SwiftDeclSynthesizer synth{*this};
  auto *succ = synth.makeSuccessorFunc(incr);
  if (!succ)
    return nullptr;

  importAttributes(CXXMethod, succ);

  Struct->addMember(succ);
  importedSuccessorCache[Struct] = succ;
  return succ;
}
