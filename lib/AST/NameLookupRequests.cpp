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
#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"

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

//----------------------------------------------------------------------------//
// GenericParamListRequest computation.
//----------------------------------------------------------------------------//

Optional<GenericParamList *> GenericParamListRequest::getCachedResult() const {
  auto *decl = std::get<0>(getStorage());
  if (!decl->GenericParamsAndBit.getInt()) {
    return None;
  }
  return decl->GenericParamsAndBit.getPointer();
}

void GenericParamListRequest::cacheResult(GenericParamList *params) const {
  auto *context = std::get<0>(getStorage());
  if (params) {
    for (auto param : *params)
      param->setDeclContext(context);
  }
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
