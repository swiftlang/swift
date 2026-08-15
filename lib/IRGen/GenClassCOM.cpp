//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ConstantBuilder.h"
#include "GenClass.h"
#include "GenProto.h"
#include "Explosion.h"
#include "IRGenMangler.h"
#include "IRGenModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "swift/ABI/Metadata.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/SIL/SILWitnessVisitor.h"
#include "swift/shims/_SwiftCOMShims.h"

using namespace llvm;
using namespace swift;
using namespace swift::irgen;

namespace {
auto GEP = [](llvm::Type *Ty, Constant *C, ArrayRef<Constant *> Indicies) {
  return ConstantExpr::getInBoundsGetElementPtr(Ty, C, Indicies);
};


struct COMInterfaceMapEntry {
  ProtocolDecl *Interface;
  unsigned ProjectionIndex;
};

class COMVTableRequirementVisitor final
    : public SILWitnessVisitor<COMVTableRequirementVisitor> {
  SmallVector<std::optional<SILDeclRef>, 8> Requirements;

public:
  void addProtocolConformanceDescriptor() { }
  void addOutOfLineBaseProtocol(ProtocolDecl *) { }
  void addAssociatedType(AssociatedTypeDecl *) { }
  void addAssociatedConformance(AssociatedConformance) { }

  void addMethod(SILDeclRef requirement) {
    Requirements.push_back(requirement);
  }

  void addPlaceholder(MissingMemberDecl *) {
    Requirements.push_back(std::nullopt);
  }

  ArrayRef<std::optional<SILDeclRef>> getRequirements() const {
    return Requirements;
  }
};

SmallVector<COMInterfaceMapEntry, 8>
getCOMInterfaceMapEntries(const COMDeclInfo *info) {
  SmallVector<COMInterfaceMapEntry, 8> entries;

  auto record = [&](ProtocolDecl *interface, unsigned index) {
    if (llvm::none_of(entries, [&](const COMInterfaceMapEntry &entry) {
          return entry.Interface == interface;
        }))
      entries.push_back({interface, index});
  };

  auto slots = info->getInterfaceSlots();
  for (auto [index, slot] : llvm::enumerate(slots)) {
    auto *hierarchy = slot->getCOMInterfaceHierarchy();
    ASSERT(hierarchy && !hierarchy->isInvalid());
    for (auto *interface : hierarchy->getABIChain())
      record(interface, index);
  }

  // The active object model may supply an identity interface which is not
  // written in the source interface hierarchy. It uses the primary user address
  // point, or the compiler-managed Swift identity address point for an
  // implementation with no user interface. This entry is independent of the
  // three common ABI slots present in every interface vtable.
  if (auto *root = info->getRootInterface())
    record(root, slots.size() == 1 ? 0 : 1);

  return entries;
}

/// Emit one class-wide interface map shared by all of its native interface
/// vtables. The layout is:
///
///   uint32_t count;
///   uint32_t reserved;
///   struct {
///     RelativePointer<ProtocolDescriptor> descriptor;
///     uint32_t index;
///   } entries[count];
///
/// Each entry is eight bytes on every target.
Constant *getOrEmitCOMInterfaceMap(IRGenModule &IGM, ClassDecl *CD) {
  if (auto iter = IGM.COMInterfaceMaps.find(CD);
      iter != IGM.COMInterfaceMaps.end())
    return iter->second;

  const COMDeclInfo *info = CD->getCOMDeclInfo();
  ASSERT(info && info->isImplementation());

  IRGenMangler decorator(IGM.Context);
  std::string label =
      decorator.mangleNominalTypeDescriptor(CD) + ".com.interface_map";

  auto entries = getCOMInterfaceMapEntries(info);

  ConstantInitBuilder Builder(IGM);
  auto S = Builder.beginStruct();
  S.setPacked(true);
  S.addInt32(entries.size()); // count
  S.addInt32(0);              // reserved

  for (auto entry : entries) {
    S.addRelativeAddress(IGM.getConstantReferenceForProtocolDescriptor(entry.Interface));
    S.addInt32(entry.ProjectionIndex);
  }

  auto *GV = S.finishAndCreateGlobal(label, Alignment(8), /*constant=*/true,
                                     GlobalValue::PrivateLinkage);

  auto *result = cast<GlobalVariable>(GV);
  IGM.COMInterfaceMaps[CD] = result;
  return result;
}

static_assert(sizeof(_SwiftCOMInterfaceMapHeader) == 2 * sizeof(uint32_t),
              "unexpected COM interface-map header layout");

static_assert(offsetof(_SwiftCOMInterfaceMapHeader, count) == 0);
static_assert(offsetof(_SwiftCOMInterfaceMapHeader, reserved) == sizeof(uint32_t));

static_assert(sizeof(_SwiftCOMInterfaceMapEntry) == 2 * sizeof(uint32_t),
              "unexpected COM interface-map entry layout");

static_assert(offsetof(_SwiftCOMInterfaceMapEntry, descriptor) == 0);
static_assert(offsetof(_SwiftCOMInterfaceMapEntry, index) == sizeof(uint32_t));

/// Return the direct C entry point supplied by the COM module.
Constant *getCOMRuntimeFunction(IRGenModule &IGM, StringRef identifier) {
  auto &Context = IGM.Context;
  auto *Module = Context.getLoadedModule(Context.Id_COM);
  if (!Module &&
      Context.MainModule && Context.MainModule->getName() == Context.Id_COM)
    Module = Context.MainModule;
  ASSERT(Module && "COM interop requires the COM supplemental module");

  Identifier name = Context.getIdentifier(identifier);
  auto request = COMRuntimeEntryRequest{Module, name};
  FuncDecl *FD = evaluateOrDefault(Context.evaluator, request, nullptr);
  if (!FD)
    return ConstantPointerNull::get(IGM.Int8PtrTy);

  auto entry = SILDeclRef(FD).asForeign();
  auto *SF = IGM.getSILModule().lookUpFunction(entry);
  ASSERT(SF && "COM runtime function was not materialized by SILGen");
  ASSERT(SF->getLoweredFunctionType()->getRepresentation() ==
             SILFunctionTypeRepresentation::CFunctionPointer &&
         "COM runtime function has the wrong calling convention");

  return IGM.getAddrOfSILFunction(SF, NotForDefinition);
}

Constant *getOrCreateCOMMethodEntry(IRGenModule &IGM, ClassDecl *CD,
                                    SILDeclRef requirement) {
  ProtocolDecl *PD =
      dyn_cast<ProtocolDecl>(requirement.getDecl()->getDeclContext());
  ASSERT(PD && PD->isCOMInterface());

  auto conformance = lookupConformance(CD->getDeclaredInterfaceType(), PD);
  ASSERT(!conformance.isInvalid() && conformance.isConcrete());

  auto concrete = conformance.getConcrete();
  if (auto *inherited = dyn_cast<InheritedProtocolConformance>(concrete))
    concrete = inherited->getInheritedConformance();

  IRGenMangler decorator(IGM.Context);
  std::string name =
      (decorator.mangleWitnessThunk(concrete,
                                    requirement.getDecl()) + Twine(".com.entry"))
          .str();

  auto *entry = IGM.getSILModule().lookUpFunction(name);
  ASSERT(entry && !entry->empty() &&
         "native COM conformance must provide a method entry");
  return IGM.getAddrOfSILFunction(entry, NotForDefinition);
}

Constant *getOrEmitCOMVTable(IRGenModule &IGM, ClassDecl *CD, Constant *map,
                             ProtocolDecl *PD, unsigned index) {
  auto key = std::make_pair(CD, PD);
  if (auto iter = IGM.COMVTables.find(key); iter != IGM.COMVTables.end())
    return iter->second;

  auto slot = [&](Constant *constant) {
    return ConstantExpr::getBitCast(constant, IGM.Int8PtrTy);
  };

  IRGenMangler decorator(IGM.Context);
  std::string label =
      (decorator.mangleNominalTypeDescriptor(CD)
          + Twine(".com.vtable.")
          + decorator.mangleProtocolDescriptor(PD)).str();

  int adjustment = (index + 1) * IGM.getPointerSize().getValue();

  auto *hierarchy = PD->getCOMInterfaceHierarchy();
  ASSERT(hierarchy && !hierarchy->isInvalid());

  COMVTableRequirementVisitor visitor;
  for (auto *interface : hierarchy->getABIChain())
    visitor.visitProtocolDecl(interface);

  bool aggregated = false;
  if (auto *PD = IGM.Context.getProtocol(KnownProtocolKind::COMAggregatable))
    aggregated =
        !lookupConformance(CD->getDeclaredInterfaceType(), PD).isInvalid();

  ConstantInitBuilder Builder(IGM);
  auto S = Builder.beginStruct();
  S.add(slot(map));
  S.add(ConstantInt::get(IGM.IntPtrTy, adjustment));

  for (StringRef Function : {
          aggregated ? "AggregatedQueryInterface" : "QueryInterface",
          aggregated ? "AggregatedAddRef" : "AddRef",
          aggregated ? "AggregatedRelease" : "Release",
       })
    S.add(slot(getCOMRuntimeFunction(IGM, Function)));

  for (auto requirement : visitor.getRequirements())
    S.add(requirement ? slot(getOrCreateCOMMethodEntry(IGM, CD, *requirement))
                      : ConstantPointerNull::get(IGM.Int8PtrTy));

  auto *GV =
      S.finishAndCreateGlobal(label, IGM.getPointerAlignment(), /*constant=*/true,
                              GlobalValue::PrivateLinkage);
  auto *result = cast<GlobalVariable>(GV);
  IGM.addUsedGlobal(result);

  Constant *indices[] = {
    ConstantInt::get(IGM.Int32Ty, 0),
    ConstantInt::get(IGM.Int32Ty, 2),
  };

  auto *lpVtbl = GEP(result->getValueType(), result, indices);
  auto *vtable = ConstantExpr::getBitCast(lpVtbl, IGM.Int8PtrTy);
  IGM.COMVTables[key] = vtable;
  return vtable;
}
}

Constant *
irgen::getOrCreateCOMObjectPrefixTemplate(IRGenModule &IGM, ClassDecl *CD) {
  auto *info = CD->getCOMDeclInfo();
  if (!info || !info->isImplementation())
    return nullptr;

  auto interfaces = info->getInterfaceSlots();
  if (interfaces.empty())
    return nullptr;

  auto label =
      IRGenMangler(IGM.Context).mangleNominalTypeDescriptor(CD) + ".com.prefix";
  if (auto *GV = IGM.getModule()->getNamedGlobal(label))
    return GV;

  Constant *map = getOrEmitCOMInterfaceMap(IGM, CD);

  SmallVector<Constant *, 4> words;
  words.reserve(interfaces.size());

  // Projection zero is closest to the native address point. The allocator
  // copies from the allocation base, so emit the words in reverse order.
  for (size_t index = interfaces.size(); index != 0; --index)
    words.push_back(getOrEmitCOMVTable(IGM, CD, map,
                                       interfaces[index - 1], index - 1));

  auto *PrefixTy = ArrayType::get(IGM.Int8PtrTy, words.size());
  auto *initializer = ConstantArray::get(PrefixTy, words);
  auto *GV = new GlobalVariable(*IGM.getModule(), PrefixTy, /*isConstant=*/true,
                                GlobalValue::PrivateLinkage, initializer, label);
  GV->setAlignment(Align(IGM.getPointerAlignment().getValue()));
  return GV;
}

namespace {
std::optional<unsigned>
getCOMProjectionIndex(const COMDeclInfo *info, ProtocolDecl *PD) {
  for (auto entry : getCOMInterfaceMapEntries(info))
    if (entry.Interface == PD)
      return entry.ProjectionIndex;
  return std::nullopt;
}
}

llvm::Value *
irgen::emitCOMInterfaceProjection(IRGenFunction &IGF, llvm::Value *value,
                                  CanType Ty, ProtocolDecl *PD,
                                  ProtocolConformanceRef conformance) {
  // An opened COM existential is already at its selected interface address
  // point. Refinement shares that physical projection with every base in the
  // same ABI chain.
  if (Ty->is<ExistentialArchetypeType>() || Ty->isExistentialType())
    return value;

  auto *CD = Ty.getClassOrBoundGenericClass();
  ASSERT(CD && "only native classes can implement COM interfaces");

  auto *info = CD->getCOMDeclInfo();
  ASSERT(info && info->isImplementation());

  auto index = getCOMProjectionIndex(info, PD);
  ASSERT(index &&
         "COM implementation is missing the requested interface projection");

  int64_t distance =
      static_cast<int64_t>((*index + 1) * IGF.IGM.getPointerSize().getValue());
  return IGF.Builder.CreateInBoundsGEP(IGF.IGM.Int8Ty, value,
                                       ConstantInt::getSigned(IGF.IGM.IntPtrTy,
                                                              -distance),
                                       "com.interface");
}
