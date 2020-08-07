//===--- NameLookupRequests.cpp - Name Lookup Requests --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/Subsystems.h"

using namespace swift;

namespace swift {
// Implement the name lookup type zone.
#define SWIFT_TYPEID_ZONE NameLookup
#define SWIFT_TYPEID_HEADER "swift/AST/NameLookupTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
}

//----------------------------------------------------------------------------//
// Referenced inherited decls computation.
//----------------------------------------------------------------------------//

SourceLoc InheritedDeclsReferencedRequest::getNearestLoc() const {
  const auto &storage = getStorage();
  auto &typeLoc = getInheritedTypeLocAtIndex(std::get<0>(storage),
                                             std::get<1>(storage));
  return typeLoc.getLoc();
}

//----------------------------------------------------------------------------//
// Superclass declaration computation.
//----------------------------------------------------------------------------//
Optional<ClassDecl *> SuperclassDeclRequest::getCachedResult() const {
  auto nominalDecl = std::get<0>(getStorage());

  if (auto *classDecl = dyn_cast<ClassDecl>(nominalDecl))
    if (classDecl->LazySemanticInfo.SuperclassDecl.getInt())
      return classDecl->LazySemanticInfo.SuperclassDecl.getPointer();

  if (auto *protocolDecl = dyn_cast<ProtocolDecl>(nominalDecl))
    if (protocolDecl->LazySemanticInfo.SuperclassDecl.getInt())
      return protocolDecl->LazySemanticInfo.SuperclassDecl.getPointer();

  return None;
}

void SuperclassDeclRequest::cacheResult(ClassDecl *value) const {
  auto nominalDecl = std::get<0>(getStorage());

  if (auto *classDecl = dyn_cast<ClassDecl>(nominalDecl))
    classDecl->LazySemanticInfo.SuperclassDecl.setPointerAndInt(value, true);

  if (auto *protocolDecl = dyn_cast<ProtocolDecl>(nominalDecl))
    protocolDecl->LazySemanticInfo.SuperclassDecl.setPointerAndInt(value, true);
}

//----------------------------------------------------------------------------//
// InheritedProtocolsRequest computation.
//----------------------------------------------------------------------------//

Optional<ArrayRef<ProtocolDecl *>>
InheritedProtocolsRequest::getCachedResult() const {
  auto proto = std::get<0>(getStorage());
  if (!proto->areInheritedProtocolsValid())
    return None;

  return proto->InheritedProtocols;
}

void InheritedProtocolsRequest::cacheResult(ArrayRef<ProtocolDecl *> PDs) const {
  auto proto = std::get<0>(getStorage());
  proto->InheritedProtocols = PDs;
  proto->setInheritedProtocolsValid();
}

evaluator::DependencySource InheritedProtocolsRequest::readDependencySource(
    const evaluator::DependencyRecorder &e) const {
  auto *PD = std::get<0>(getStorage());
  // Ignore context changes for protocols outside our module. This
  // prevents transitive cascading edges when e.g. our private
  // type conforms to Hashable which itself looks up Equatable during
  // qualified lookup.
  if (!PD->getParentSourceFile())
    return { nullptr, e.getActiveSourceScope() };
  return {
    e.getActiveDependencySourceOrNull(),
    evaluator::getScopeForAccessLevel(PD->getFormalAccess())
  };
}

void InheritedProtocolsRequest::writeDependencySink(
    evaluator::DependencyCollector &tracker,
    ArrayRef<ProtocolDecl *> PDs) const {
  for (auto *parentProto : PDs) {
    tracker.addPotentialMember(parentProto);
  }
}

//----------------------------------------------------------------------------//
// Missing designated initializers computation
//----------------------------------------------------------------------------//

Optional<bool> HasMissingDesignatedInitializersRequest::getCachedResult() const {
  auto classDecl = std::get<0>(getStorage());
  return classDecl->getCachedHasMissingDesignatedInitializers();
}

void HasMissingDesignatedInitializersRequest::cacheResult(bool result) const {
  auto classDecl = std::get<0>(getStorage());
  classDecl->setHasMissingDesignatedInitializers(result);
}

bool
HasMissingDesignatedInitializersRequest::evaluate(Evaluator &evaluator,
                                           ClassDecl *subject) const {
  // Short-circuit and check for the attribute here.
  if (subject->getAttrs().hasAttribute<HasMissingDesignatedInitializersAttr>())
    return true;

  AccessScope scope =
    subject->getFormalAccessScope(/*useDC*/nullptr,
                                  /*treatUsableFromInlineAsPublic*/true);
  // This flag only makes sense for public types that will be written in the
  // module.
  if (!scope.isPublic())
    return false;

  auto constructors = subject->lookupDirect(DeclBaseName::createConstructor());
  return llvm::any_of(constructors, [&](ValueDecl *decl) {
    auto init = cast<ConstructorDecl>(decl);
    if (!init->isDesignatedInit())
      return false;
    AccessScope scope =
        init->getFormalAccessScope(/*useDC*/nullptr,
                                   /*treatUsableFromInlineAsPublic*/true);
    return !scope.isPublic();
  });
}

//----------------------------------------------------------------------------//
// Extended nominal computation.
//----------------------------------------------------------------------------//

Optional<NominalTypeDecl *> ExtendedNominalRequest::getCachedResult() const {
  // Note: if we fail to compute any nominal declaration, it's considered
  // a cache miss. This allows us to recompute the extended nominal types
  // during extension binding.
  // This recomputation is also what allows you to extend types defined inside
  // other extensions, regardless of source file order. See \c bindExtensions(),
  // which uses a worklist algorithm that attempts to bind everything until
  // fixed point.
  auto ext = std::get<0>(getStorage());
  if (!ext->hasBeenBound() || !ext->getExtendedNominal())
    return None;
  return ext->getExtendedNominal();
}

void ExtendedNominalRequest::cacheResult(NominalTypeDecl *value) const {
  auto ext = std::get<0>(getStorage());
  ext->setExtendedNominal(value);
}

void ExtendedNominalRequest::writeDependencySink(
    evaluator::DependencyCollector &tracker, NominalTypeDecl *value) const {
  if (!value)
    return;

  // Ensure this extension comes from a source file.
  auto *SF = std::get<0>(getStorage())->getParentSourceFile();
  if (!SF)
    return;
  if (SF != tracker.getRecorder().getActiveDependencySourceOrNull())
    return;
  tracker.addPotentialMember(value);
}

//----------------------------------------------------------------------------//
// Destructor computation.
//----------------------------------------------------------------------------//

Optional<DestructorDecl *> GetDestructorRequest::getCachedResult() const {
  auto *classDecl = std::get<0>(getStorage());
  auto results = classDecl->lookupDirect(DeclBaseName::createDestructor());
  if (results.empty())
    return None;

  return cast<DestructorDecl>(results.front());
}

void GetDestructorRequest::cacheResult(DestructorDecl *value) const {
  auto *classDecl = std::get<0>(getStorage());
  classDecl->addMember(value);
}

evaluator::DependencySource GetDestructorRequest::readDependencySource(
    const evaluator::DependencyRecorder &eval) const {
  // Looking up the deinitializer currently always occurs in a private
  // scope because it is impossible to reference 'deinit' in user code, and a
  // valid 'deinit' declaration cannot occur outside of the
  // definition of a type.
  return {
    eval.getActiveDependencySourceOrNull(),
    evaluator::DependencyScope::Private
  };
}

//----------------------------------------------------------------------------//
// GenericParamListRequest computation.
//----------------------------------------------------------------------------//

Optional<GenericParamList *> GenericParamListRequest::getCachedResult() const {
  auto *decl = std::get<0>(getStorage());
  if (auto *params = decl->GenericParamsAndBit.getPointer())
    return params;

  if (decl->GenericParamsAndBit.getInt())
    return nullptr;

  return None;
}

void GenericParamListRequest::cacheResult(GenericParamList *params) const {
  auto *context = std::get<0>(getStorage());
  if (params)
    params->setDeclContext(context);

  context->GenericParamsAndBit.setPointerAndInt(params, true);
}

//----------------------------------------------------------------------------//
// UnqualifiedLookupRequest computation.
//----------------------------------------------------------------------------//

void swift::simple_display(llvm::raw_ostream &out,
                           const UnqualifiedLookupDescriptor &desc) {
  out << "looking up ";
  simple_display(out, desc.Name);
  out << " from ";
  simple_display(out, desc.DC);
  out << " with options ";
  simple_display(out, desc.Options);
}

SourceLoc
swift::extractNearestSourceLoc(const UnqualifiedLookupDescriptor &desc) {
  return extractNearestSourceLoc(desc.DC);
}

//----------------------------------------------------------------------------//
// DirectLookupRequest computation.
//----------------------------------------------------------------------------//

void swift::simple_display(llvm::raw_ostream &out,
                           const DirectLookupDescriptor &desc) {
  out << "directly looking up ";
  simple_display(out, desc.Name);
  out << " on ";
  simple_display(out, desc.DC);
  out << " with options ";
  simple_display(out, desc.Options);
}

SourceLoc swift::extractNearestSourceLoc(const DirectLookupDescriptor &desc) {
  return extractNearestSourceLoc(desc.DC);
}

//----------------------------------------------------------------------------//
// LookupOperatorRequest computation.
//----------------------------------------------------------------------------//

OperatorLookupDescriptor OperatorLookupDescriptor::forDC(const DeclContext *DC,
                                                         Identifier name) {
  auto *moduleDC = DC->getModuleScopeContext();
  if (auto *file = dyn_cast<FileUnit>(moduleDC)) {
    return OperatorLookupDescriptor::forFile(file, name);
  } else {
    auto *mod = cast<ModuleDecl>(moduleDC->getAsDecl());
    return OperatorLookupDescriptor::forModule(mod, name);
  }
}

ArrayRef<FileUnit *> OperatorLookupDescriptor::getFiles() const {
  if (auto *module = getModule())
    return module->getFiles();

  // Return an ArrayRef pointing to the FileUnit in the union.
  return llvm::makeArrayRef(*fileOrModule.getAddrOfPtr1());
}

void swift::simple_display(llvm::raw_ostream &out,
                           const OperatorLookupDescriptor &desc) {
  out << "looking up operator ";
  simple_display(out, desc.name);
  out << " in ";
  simple_display(out, desc.fileOrModule);
}

SourceLoc swift::extractNearestSourceLoc(const OperatorLookupDescriptor &desc) {
  return extractNearestSourceLoc(desc.fileOrModule);
}

void DirectLookupRequest::writeDependencySink(
    evaluator::DependencyCollector &tracker,
    TinyPtrVector<ValueDecl *> result) const {
  auto &desc = std::get<0>(getStorage());
  tracker.addUsedMember(desc.DC, desc.Name.getBaseName());
}

//----------------------------------------------------------------------------//
// LookupInModuleRequest computation.
//----------------------------------------------------------------------------//

void LookupInModuleRequest::writeDependencySink(
    evaluator::DependencyCollector &reqTracker, QualifiedLookupResult l) const {
  auto *module = std::get<0>(getStorage());
  auto member = std::get<1>(getStorage());
  auto *DC = std::get<4>(getStorage());

  // Decline to record lookups outside our module.
  if (!DC->getParentSourceFile() ||
      module->getParentModule() != DC->getParentModule()) {
    return;
  }
  reqTracker.addTopLevelName(member.getBaseName());
}

//----------------------------------------------------------------------------//
// LookupConformanceInModuleRequest computation.
//----------------------------------------------------------------------------//

void swift::simple_display(llvm::raw_ostream &out,
                           const LookupConformanceDescriptor &desc) {
  out << "looking up conformance to ";
  simple_display(out, desc.PD);
  out << " for ";
  out << desc.Ty.getString();
  out << " in ";
  simple_display(out, desc.Mod);
}

void AnyObjectLookupRequest::writeDependencySink(
    evaluator::DependencyCollector &reqTracker, QualifiedLookupResult l) const {
  auto member = std::get<1>(getStorage());
  reqTracker.addDynamicLookupName(member.getBaseName());
}

SourceLoc
swift::extractNearestSourceLoc(const LookupConformanceDescriptor &desc) {
  return SourceLoc();
}

//----------------------------------------------------------------------------//
// LookupInModuleRequest computation.
//----------------------------------------------------------------------------//

evaluator::DependencySource ModuleQualifiedLookupRequest::readDependencySource(
    const evaluator::DependencyRecorder &eval) const {
  auto *DC = std::get<0>(getStorage());
  auto options = std::get<3>(getStorage());

  // FIXME(Evaluator Incremental Dependencies): This is an artifact of the
  // current scheme and should be removed. There are very few callers that are
  // accurately passing the right known dependencies mask.
  const bool knownPrivate =
      (options & NL_KnownDependencyMask) == NL_KnownNonCascadingDependency;
  const bool fromPrivateDC =
      DC->isCascadingContextForLookup(/*functionsAreNonCascading=*/false);

  auto scope = evaluator::DependencyScope::Cascading;
  if (knownPrivate || fromPrivateDC)
    scope = evaluator::DependencyScope::Private;
  return { DC->getParentSourceFile(), scope };
}

void ModuleQualifiedLookupRequest::writeDependencySink(
    evaluator::DependencyCollector &reqTracker, QualifiedLookupResult l) const {
  auto *DC = std::get<0>(getStorage());
  auto *module = std::get<1>(getStorage());
  auto member = std::get<2>(getStorage());

  // Decline to record lookups outside our module.
  if (!DC->getParentSourceFile() ||
      module != DC->getModuleScopeContext()->getParentModule()) {
    return;
  }
  reqTracker.addTopLevelName(member.getBaseName());
}

//----------------------------------------------------------------------------//
// LookupConformanceInModuleRequest computation.
//----------------------------------------------------------------------------//

void LookupConformanceInModuleRequest::writeDependencySink(
    evaluator::DependencyCollector &reqTracker,
    ProtocolConformanceRef lookupResult) const {
  if (lookupResult.isInvalid() || !lookupResult.isConcrete())
    return;

  auto &desc = std::get<0>(getStorage());
  auto *Adoptee = desc.Ty->getAnyNominal();
  if (!Adoptee)
    return;

  auto *source = reqTracker.getRecorder().getActiveDependencySourceOrNull();
  if (!source)
    return;

  // Decline to record conformances defined outside of the active module.
  auto *conformance = lookupResult.getConcrete();
  if (source->getParentModule() !=
      conformance->getDeclContext()->getParentModule())
    return;
  reqTracker.addPotentialMember(Adoptee);
}

//----------------------------------------------------------------------------//
// UnqualifiedLookupRequest computation.
//----------------------------------------------------------------------------//

evaluator::DependencySource UnqualifiedLookupRequest::readDependencySource(
    const evaluator::DependencyRecorder &) const {
  auto &desc = std::get<0>(getStorage());
  // FIXME(Evaluator Incremental Dependencies): This maintains compatibility
  // with the existing scheme, but the existing scheme is totally ad-hoc. We
  // should remove this flag and ensure that non-cascading qualified lookups
  // occur in the right contexts instead.
  auto scope = evaluator::DependencyScope::Cascading;
  if (desc.Options.contains(UnqualifiedLookupFlags::KnownPrivate)) {
    scope = evaluator::DependencyScope::Private;
  }
  return {desc.DC->getParentSourceFile(), scope};
}

void UnqualifiedLookupRequest::writeDependencySink(
    evaluator::DependencyCollector &track, LookupResult res) const {
  auto &desc = std::get<0>(getStorage());
  track.addTopLevelName(desc.Name.getBaseName());
}

//----------------------------------------------------------------------------//
// QualifiedLookupRequest computation.
//----------------------------------------------------------------------------//

evaluator::DependencySource QualifiedLookupRequest::readDependencySource(
    const evaluator::DependencyRecorder &) const {
  auto *dc = std::get<0>(getStorage());
  auto opts = std::get<3>(getStorage());
  // FIXME(Evaluator Incremental Dependencies): This is an artifact of the
  // current scheme and should be removed. There are very few callers that are
  // accurately passing the right known dependencies mask.
  const bool cascades =
      dc->isCascadingContextForLookup(/*functionsAreNonCascading*/ false);
  const bool knownPrivate =
      (opts & NL_KnownDependencyMask) == NL_KnownNonCascadingDependency;
  auto scope = evaluator::DependencyScope::Cascading;
  if (!cascades || knownPrivate)
    scope = evaluator::DependencyScope::Private;
  return {
    dyn_cast<SourceFile>(dc->getModuleScopeContext()),
    scope
  };
}

// Define request evaluation functions for each of the name lookup requests.
static AbstractRequestFunction *nameLookupRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/AST/NameLookupTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerNameLookupRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::NameLookup,
                                     nameLookupRequestFunctions);
}
