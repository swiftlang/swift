//===--- GenProto.cpp - Swift IR Generation for Protocols -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for protocols in Swift.
//
//  Protocols serve two masters: generic algorithms and existential
//  types.  In either case, the size and structure of a type is opaque
//  to the code manipulating a value.  Local values of the type must
//  be stored in fixed-size buffers (which can overflow to use heap
//  allocation), and basic operations on the type must be dynamically
//  delegated to a collection of information that "witnesses" the
//  truth that a particular type implements the protocol.
//
//  In the comments throughout this file, three type names are used:
//    'B' is the type of a fixed-size buffer
//    'T' is the type which implements a protocol
//    'W' is the type of a witness to the protocol
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILWitnessVisitor.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"

#include "CallEmission.h"
#include "EnumPayload.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "Fulfillment.h"
#include "GenArchetype.h"
#include "GenClass.h"
#include "GenEnum.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenOpaque.h"
#include "GenPoly.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "MetadataPath.h"
#include "NecessaryBindings.h"
#include "ProtocolInfo.h"
#include "TypeInfo.h"

#include "GenProto.h"

using namespace swift;
using namespace irgen;

static bool shouldSetName(IRGenModule &IGM, llvm::Value *value, CanType type) {
  // If value names are globally disabled, honor that.
  if (!IGM.EnableValueNames) return false;

  // Suppress value names for values with opened existentials.
  if (type->hasOpenedExistential()) return false;

  // If the value already has a name, honor that.
  if (value->hasName()) return false;

  // Only do this for local values.
  return (isa<llvm::Instruction>(value) || isa<llvm::Argument>(value));
}

void irgen::setTypeMetadataName(IRGenModule &IGM, llvm::Value *metadata,
                                CanType type) {
  if (!shouldSetName(IGM, metadata, type)) return;

  SmallString<128> name; {
    llvm::raw_svector_ostream out(name);
    type.print(out);
  }
  metadata->setName(type->getString());
}

void irgen::setProtocolWitnessTableName(IRGenModule &IGM, llvm::Value *wtable,
                                        CanType type,
                                        ProtocolDecl *requirement) {
  if (!shouldSetName(IGM, wtable, type)) return;

  SmallString<128> name; {
    llvm::raw_svector_ostream out(name);
    type.print(out);
    out << '.' << requirement->getNameStr();
  }
  wtable->setName(name);
}

namespace {
  /// A concrete witness table, together with its known layout.
  class WitnessTable {
    llvm::Value *Table;
    const ProtocolInfo &Info;
  public:
    WitnessTable(llvm::Value *wtable, const ProtocolInfo &info)
      : Table(wtable), Info(info) {}

    llvm::Value *getTable() const { return Table; }
    const ProtocolInfo &getInfo() const { return Info; }
  };

  /// A class which lays out a witness table in the abstract.
  class WitnessTableLayout : public SILWitnessVisitor<WitnessTableLayout> {
    unsigned NumWitnesses = 0;
    SmallVector<WitnessTableEntry, 16> Entries;

    WitnessIndex getNextIndex() {
      return WitnessIndex(NumWitnesses++, /*isPrefix=*/false);
    }

  public:
    /// The next witness is an out-of-line base protocol.
    void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
      Entries.push_back(
             WitnessTableEntry::forOutOfLineBase(baseProto, getNextIndex()));
    }

    void addMethod(FuncDecl *func) {
      Entries.push_back(WitnessTableEntry::forFunction(func, getNextIndex()));
    }

    void addConstructor(ConstructorDecl *ctor) {
      Entries.push_back(WitnessTableEntry::forFunction(ctor, getNextIndex()));
    }

    void addAssociatedType(AssociatedTypeDecl *ty,
                           ArrayRef<ProtocolDecl *> protos) {
      // An associated type takes up a spot for the type metadata and for the
      // witnesses to all its conformances.
      Entries.push_back(
                      WitnessTableEntry::forAssociatedType(ty, getNextIndex()));
      for (auto *proto : protos)
        if (Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
          ++NumWitnesses;
    }

    unsigned getNumWitnesses() const { return NumWitnesses; }
    ArrayRef<WitnessTableEntry> getEntries() const { return Entries; }
  };

  /// A path through a protocol hierarchy.
  class ProtocolPath {
    IRGenModule &IGM;

    /// The destination protocol.
    ProtocolDecl *Dest;

    /// The path from the selected origin down to the destination
    /// protocol.
    SmallVector<WitnessIndex, 8> ReversePath;

    /// The origin index to use.
    unsigned OriginIndex;

    /// The best path length we found.
    unsigned BestPathLength;

  public:
    /// Find a path from the given set of origins to the destination
    /// protocol.
    ///
    /// T needs to provide a couple of member functions:
    ///   ProtocolDecl *getProtocol() const;
    ///   const ProtocolInfo &getInfo() const;
    template <class T>
    ProtocolPath(IRGenModule &IGM, ArrayRef<T> origins, ProtocolDecl *dest)
      : IGM(IGM), Dest(dest), BestPathLength(~0U) {

      // Consider each of the origins in turn, breaking out if any of
      // them yields a zero-length path.
      for (unsigned i = 0, e = origins.size(); i != e; ++i) {
        auto &origin = origins[i];
        if (considerOrigin(origin.getProtocol(), origin.getInfo(), i))
          break;
      }

      // Sanity check that we actually found a path at all.
      assert(BestPathLength != ~0U);
      assert(BestPathLength == ReversePath.size());
    }

    /// Returns the index of the origin protocol we chose.
    unsigned getOriginIndex() const { return OriginIndex; }

    /// Apply the path to the given witness table.
    llvm::Value *apply(IRGenFunction &IGF, llvm::Value *wtable) const {
      for (unsigned i = ReversePath.size(); i != 0; --i) {
        wtable = emitInvariantLoadOfOpaqueWitness(IGF, wtable,
                                                  ReversePath[i-1]);
        wtable = IGF.Builder.CreateBitCast(wtable, IGF.IGM.WitnessTablePtrTy);
      }
      return wtable;
    }

  private:
    /// Consider paths starting from a new origin protocol.
    /// Returns true if there's no point in considering other origins.
    bool considerOrigin(ProtocolDecl *origin, const ProtocolInfo &originInfo,
                        unsigned originIndex) {
      assert(BestPathLength != 0);

      // If the origin *is* the destination, we can stop here.
      if (origin == Dest) {
        OriginIndex = originIndex;
        BestPathLength = 0;
        ReversePath.clear();
        return true;
      }

      // Otherwise, if the origin gives rise to a better path, that's
      // also cool.
      if (findBetterPath(origin, originInfo, 0)) {
        OriginIndex = originIndex;
        return BestPathLength == 0;
      }

      return false;
    }

    /// Consider paths starting at the given protocol.
    bool findBetterPath(ProtocolDecl *proto, const ProtocolInfo &protoInfo,
                        unsigned lengthSoFar) {
      assert(lengthSoFar < BestPathLength);
      assert(proto != Dest);

      // Keep track of whether we found a better path than the
      // previous best.
      bool foundBetter = false;
      for (auto base : proto->getInheritedProtocols(nullptr)) {
        // ObjC protocols do not have witnesses.
        if (!Lowering::TypeConverter::protocolRequiresWitnessTable(base))
          continue;

        auto &baseEntry = protoInfo.getWitnessEntry(base);
        assert(baseEntry.isBase());

        // Compute the length down to this base.
        unsigned lengthToBase = lengthSoFar;
        if (baseEntry.isOutOfLineBase()) {
          lengthToBase++;

          // Don't consider this path if we reach a length that can't
          // possibly be better than the best so far.
          if (lengthToBase == BestPathLength) continue;
        }
        assert(lengthToBase < BestPathLength);

        // If this base *is* the destination, go ahead and start
        // building the path into ReversePath.
        if (base == Dest) {
          // Reset the collected best-path information.
          BestPathLength = lengthToBase;
          ReversePath.clear();

        // Otherwise, if there isn't a better path through this base,
        // don't accumulate anything in the path.
        } else if (!findBetterPath(base, IGM.getProtocolInfo(base),
                                   lengthToBase)) {
          continue;
        }

        // Okay, we've found a better path, and ReversePath contains a
        // path leading from base to Dest.
        assert(BestPathLength >= lengthToBase);
        foundBetter = true;

        // Add the link from proto to base if necessary.
        if (baseEntry.isOutOfLineBase()) {
          ReversePath.push_back(baseEntry.getOutOfLineBaseIndex());

        // If it isn't necessary, then we might be able to
        // short-circuit considering the bases of this protocol.
        } else {
          if (lengthSoFar == BestPathLength)
            return true;
        }
      }

      return foundBetter;
    }
  };

} // end anonymous namespace

/// Is there anything about the given conformance that requires witness
/// tables to be dependently-generated?
static bool isDependentConformance(IRGenModule &IGM,
                             const NormalProtocolConformance *conformance,
                                   ResilienceExpansion expansion) {
  // If the conforming type isn't dependent, this is never true.
  if (!conformance->getDeclContext()->isGenericContext())
    return false;

  // Check whether any of the associated types are dependent.
  if (conformance->forEachTypeWitness(nullptr,
        [&](AssociatedTypeDecl *requirement, const Substitution &sub,
            TypeDecl *explicitDecl) -> bool {
          // RESILIENCE: this could be an opaque conformance
          return sub.getReplacement()->hasArchetype();
         })) {
    return true;
  }

  // Check whether any of the associated types are dependent.
  for (auto &entry : conformance->getInheritedConformances()) {
    if (isDependentConformance(IGM, entry.second->getRootNormalConformance(),
                               expansion)) {
      return true;
    }
  }

  return false;
}

/// Detail about how an object conforms to a protocol.
class irgen::ConformanceInfo {
  friend class ProtocolInfo;
public:
  virtual ~ConformanceInfo() {}
  virtual llvm::Value *getTable(IRGenFunction &IGF,
                                CanType conformingType,
                               llvm::Value **conformingMetadataCache) const = 0;
  /// Try to get this table as a constant pointer.  This might just
  /// not be supportable at all.
  virtual llvm::Constant *tryGetConstantTable(IRGenModule &IGM,
                                              CanType conformingType) const = 0;
};

static llvm::Value *
emitWitnessTableAccessorCall(IRGenFunction &IGF,
                             const NormalProtocolConformance *conformance,
                             CanType conformingType,
                             llvm::Value **srcMetadataCache) {
  auto accessor =
    IGF.IGM.getAddrOfWitnessTableAccessFunction(conformance, NotForDefinition);

  // If the conformance is generic, the accessor takes the metatype
  // as an argument.
  llvm::CallInst *call;
  if (conformance->getDeclContext()->isGenericContext()) {
    // Emit the source metadata if we haven't yet.
    if (!*srcMetadataCache) {
      *srcMetadataCache = IGF.emitTypeMetadataRef(conformingType);
    }
    call = IGF.Builder.CreateCall(accessor, {*srcMetadataCache});
  } else {
    call = IGF.Builder.CreateCall(accessor, {});
  }

  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotAccessMemory();
  call->setDoesNotThrow();

  return call;
}

/// Fetch the lazy access function for the given conformance of the
/// given type.
static llvm::Function *
getWitnessTableLazyAccessFunction(IRGenModule &IGM,
                                  const NormalProtocolConformance *conformance,
                                  CanType conformingType) {
  assert(!conformingType->hasArchetype());
  llvm::Function *accessor =
    IGM.getAddrOfWitnessTableLazyAccessFunction(conformance, conformingType,
                                                ForDefinition);

  // If we're not supposed to define the accessor, or if we already
  // have defined it, just return the pointer.
  if (!accessor->empty())
    return accessor;

  // Okay, define the accessor.
  auto cacheVariable = cast<llvm::GlobalVariable>(
    IGM.getAddrOfWitnessTableLazyCacheVariable(conformance, conformingType,
                                               ForDefinition));
  emitLazyCacheAccessFunction(IGM, accessor, cacheVariable,
                              [&](IRGenFunction &IGF) -> llvm::Value* {
    llvm::Value *conformingMetadataCache = nullptr;
    return emitWitnessTableAccessorCall(IGF, conformance, conformingType,
                                        &conformingMetadataCache);
  });

  return accessor;
}

namespace {

/// Conformance info for a witness table that can be directly generated.
class DirectConformanceInfo : public ConformanceInfo {
  friend class ProtocolInfo;

  const NormalProtocolConformance *RootConformance;
public:
  DirectConformanceInfo(const NormalProtocolConformance *C)
    : RootConformance(C) {}

  llvm::Value *getTable(IRGenFunction &IGF, CanType conformingType,
                        llvm::Value **conformingMetadataCache) const override {
    return IGF.IGM.getAddrOfWitnessTable(RootConformance);
  }

  llvm::Constant *tryGetConstantTable(IRGenModule &IGM,
                                      CanType conformingType) const override {
    return IGM.getAddrOfWitnessTable(RootConformance);
  }
};

/// Conformance info for a witness table that is (or may be) dependent.
class AccessorConformanceInfo : public ConformanceInfo {
  friend class ProtocolInfo;

  const NormalProtocolConformance *Conformance;
public:
  AccessorConformanceInfo(const NormalProtocolConformance *C)
    : Conformance(C) {}

  llvm::Value *getTable(IRGenFunction &IGF, CanType type,
                        llvm::Value **typeMetadataCache) const override {
    // If the conformance isn't generic, or we're looking up a dependent
    // type, we don't want to / can't cache the result.
    if (!Conformance->getDeclContext()->isGenericContext() ||
        type->hasArchetype()) {
      return emitWitnessTableAccessorCall(IGF, Conformance, type,
                                          typeMetadataCache);
    }

    // Otherwise, call a lazy-cache function.
    auto accessor =
      getWitnessTableLazyAccessFunction(IGF.IGM, Conformance, type);
    llvm::CallInst *call = IGF.Builder.CreateCall(accessor, {});
    call->setCallingConv(IGF.IGM.RuntimeCC);
    call->setDoesNotAccessMemory();
    call->setDoesNotThrow();

    return call;
  }

  llvm::Constant *tryGetConstantTable(IRGenModule &IGM,
                                      CanType conformingType) const override {
    return nullptr;
  }
};

  /// A class which lays out a specific conformance to a protocol.
  class WitnessTableBuilder : public SILWitnessVisitor<WitnessTableBuilder> {
    IRGenModule &IGM;
    SmallVectorImpl<llvm::Constant*> &Table;
    CanType ConcreteType;
    const NormalProtocolConformance &Conformance;
    ArrayRef<SILWitnessTable::Entry> SILEntries;
    const ProtocolInfo &PI;
    Optional<FulfillmentMap> Fulfillments;
    SmallVector<std::pair<size_t, const ConformanceInfo *>, 4>
      SpecializedBaseConformances;
    // Metadata caches are stored at negative offsets.
    unsigned NextCacheIndex = 0;
    bool RequiresSpecialization = false;

  public:
    WitnessTableBuilder(IRGenModule &IGM,
                        SmallVectorImpl<llvm::Constant*> &table,
                        SILWitnessTable *SILWT)
      : IGM(IGM), Table(table),
        ConcreteType(SILWT->getConformance()->getType()->getCanonicalType()),
        Conformance(*SILWT->getConformance()),
        SILEntries(SILWT->getEntries()),
        PI(IGM.getProtocolInfo(SILWT->getConformance()->getProtocol()))
    {
      // TODO: in conditional conformances, allocate space for the assumed
      // conformances here.
    }

    /// The top-level entry point.
    void build();

    /// Create the access function.
    void buildAccessFunction(llvm::Constant *wtable);

    /// A base protocol is witnessed by a pointer to the conformance
    /// of this type to that protocol.
    void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
#ifndef NDEBUG
      auto &entry = SILEntries.front();
      assert(entry.getKind() == SILWitnessTable::BaseProtocol
             && "sil witness table does not match protocol");
      assert(entry.getBaseProtocolWitness().Requirement == baseProto
             && "sil witness table does not match protocol");
      auto piEntry = PI.getWitnessEntry(baseProto);
      assert(piEntry.getOutOfLineBaseIndex().getValue() == Table.size()
             && "offset doesn't match ProtocolInfo layout");
#endif
      
      SILEntries = SILEntries.slice(1);

      // TODO: Use the witness entry instead of falling through here.

      // Look for a protocol type info.
      const ProtocolInfo &basePI = IGM.getProtocolInfo(baseProto);
      const ProtocolConformance *astConf
        = Conformance.getInheritedConformance(baseProto);
      const ConformanceInfo &conf =
        basePI.getConformance(IGM, baseProto, astConf);

      // If we can emit the base witness table as a constant, do so.
      llvm::Constant *baseWitness = conf.tryGetConstantTable(IGM, ConcreteType);
      if (baseWitness) {
        Table.push_back(baseWitness);
        return;
      }

      // Otherwise, we'll need to derive it at instantiation time.
      RequiresSpecialization = true;
      SpecializedBaseConformances.push_back({Table.size(), &conf});
      Table.push_back(llvm::ConstantPointerNull::get(IGM.WitnessTablePtrTy));
    }

    void addMethodFromSILWitnessTable(AbstractFunctionDecl *iface) {
      auto &entry = SILEntries.front();
      SILEntries = SILEntries.slice(1);

      // Handle missing optional requirements.
      if (entry.getKind() == SILWitnessTable::MissingOptional) {
        Table.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
        return;
      }

#ifndef NDEBUG
      assert(entry.getKind() == SILWitnessTable::Method
             && "sil witness table does not match protocol");
      assert(entry.getMethodWitness().Requirement.getDecl() == iface
             && "sil witness table does not match protocol");
      auto piEntry = PI.getWitnessEntry(iface);
      assert(piEntry.getFunctionIndex().getValue() == Table.size()
             && "offset doesn't match ProtocolInfo layout");
#endif

      SILFunction *Func = entry.getMethodWitness().Witness;
      llvm::Constant *witness = nullptr;
      if (Func) {
        witness = IGM.getAddrOfSILFunction(Func, NotForDefinition);
      } else {
        // The method is removed by dead method elimination.
        // It should be never called. We add a pointer to an error function.
        witness = IGM.getDeletedMethodErrorFn();
      }
      Table.push_back(witness);
      return;
    }

    void addMethod(FuncDecl *iface) {
      return addMethodFromSILWitnessTable(iface);
    }

    void addConstructor(ConstructorDecl *iface) {
      return addMethodFromSILWitnessTable(iface);
    }

    void addAssociatedType(AssociatedTypeDecl *requirement,
                           ArrayRef<ProtocolDecl *> protos) {
#ifndef NDEBUG
      auto &entry = SILEntries.front();
      assert(entry.getKind() == SILWitnessTable::AssociatedType
             && "sil witness table does not match protocol");
      assert(entry.getAssociatedTypeWitness().Requirement == requirement
             && "sil witness table does not match protocol");
      auto piEntry = PI.getWitnessEntry(requirement);
      assert(piEntry.getAssociatedTypeIndex().getValue() == Table.size()
             && "offset doesn't match ProtocolInfo layout");
#endif

      SILEntries = SILEntries.slice(1);

      const Substitution &sub =
        Conformance.getTypeWitness(requirement, nullptr);
      assert(protos.size() == sub.getConformances().size());

      // This type will be expressed in terms of the archetypes
      // of the conforming context.
      CanType associate = sub.getReplacement()->getCanonicalType();
      assert(!associate->hasTypeParameter());

      llvm::Constant *metadataAccessFunction =
        getAssociatedTypeMetadataAccessFunction(requirement, associate);
      Table.push_back(metadataAccessFunction);

      // FIXME: Add static witness tables for type conformances.
      for (auto index : indices(protos)) {
        ProtocolDecl *protocol = protos[index];
        auto associatedConformance = sub.getConformances()[index];

        if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
          continue;

#ifndef NDEBUG
        auto &entry = SILEntries.front();
        (void)entry;
        assert(entry.getKind() == SILWitnessTable::AssociatedTypeProtocol
               && "sil witness table does not match protocol");
        auto associatedWitness = entry.getAssociatedTypeProtocolWitness();
        assert(associatedWitness.Requirement == requirement
               && "sil witness table does not match protocol");
        assert(associatedWitness.Protocol == protocol
               && "sil witness table does not match protocol");
#endif

        SILEntries = SILEntries.slice(1);

        llvm::Constant *wtableAccessFunction = 
          getAssociatedTypeWitnessTableAccessFunction(requirement, associate,
                                            protocol, associatedConformance);
        Table.push_back(wtableAccessFunction);
      }
    }

  private:
    llvm::Constant *buildInstantiationFunction();

    llvm::Constant *
    getAssociatedTypeMetadataAccessFunction(AssociatedTypeDecl *requirement,
                                            CanType associatedType);

    llvm::Constant *
    getAssociatedTypeWitnessTableAccessFunction(AssociatedTypeDecl *requirement,
                                                CanType associatedType,
                                                ProtocolDecl *protocol,
                                        ProtocolConformanceRef conformance);

    void emitReturnOfCheckedLoadFromCache(IRGenFunction &IGF,
                                          Address destTable,
                                          llvm::Value *selfMetadata,
                                    llvm::function_ref<llvm::Value*()> body);

    void bindArchetypes(IRGenFunction &IGF, llvm::Value *selfMetadata);

    /// Allocate another word of private data storage in the conformance table.
    unsigned getNextCacheIndex() {
      RequiresSpecialization = true;
      return NextCacheIndex++;
    }

    const FulfillmentMap &getFulfillmentMap() {
      if (Fulfillments) return *Fulfillments;

      Fulfillments.emplace();
      if (ConcreteType->hasArchetype()) {
        struct Callback : FulfillmentMap::InterestingKeysCallback {
          bool isInterestingType(CanType type) const override {
            return isa<ArchetypeType>(type);
          }
          bool hasInterestingType(CanType type) const override {
            return type->hasArchetype();
          }
          bool hasLimitedInterestingConformances(CanType type) const override {
            return false;
          }
          GenericSignature::ConformsToArray
          getInterestingConformances(CanType type) const override {
            llvm_unreachable("no limits");
          }
        } callback;
        Fulfillments->searchTypeMetadata(*IGM.SILMod->getSwiftModule(),
                                         ConcreteType, IsExact,
                                         /*sourceIndex*/ 0, MetadataPath(),
                                         callback);
      }
      return *Fulfillments;
    }
  };
}

/// Build the witness table.
void WitnessTableBuilder::build() {
  visitProtocolDecl(Conformance.getProtocol());

  // Go through and convert all the entries to i8*.
  // TODO: the IR would be more legible if we made a struct instead.
  for (auto &entry : Table) {
    entry = llvm::ConstantExpr::getBitCast(entry, IGM.Int8PtrTy);
  }
}

/// Return the address of a function which will return the type metadata
/// for an associated type.
llvm::Constant *WitnessTableBuilder::
getAssociatedTypeMetadataAccessFunction(AssociatedTypeDecl *requirement,
                                        CanType associatedType) {
  // If the associated type is non-dependent, we can use an ordinary
  // metadata access function.  We'll just end up passing extra arguments.
  if (!associatedType->hasArchetype()) {
    return getOrCreateTypeMetadataAccessFunction(IGM, associatedType);
  }

  // Otherwise, emit an access function.
  llvm::Function *accessor =
    IGM.getAddrOfAssociatedTypeMetadataAccessFunction(&Conformance,
                                                      requirement);

  IRGenFunction IGF(IGM, accessor);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, accessor);

  Explosion parameters = IGF.collectParameters();

  llvm::Value *self = parameters.claimNext();
  setTypeMetadataName(IGM, self, ConcreteType);

  Address destTable(parameters.claimNext(), IGM.getPointerAlignment());
  setProtocolWitnessTableName(IGM, destTable.getAddress(), ConcreteType,
                              requirement->getProtocol());

  // If the associated type is directly fulfillable from the type,
  // we don't need a cache entry.
  // TODO: maybe we should have a cache entry anyway if the fulfillment
  // is expensive.
  if (auto fulfillment =
        getFulfillmentMap().getTypeMetadata(associatedType)) {
    llvm::Value *metadata =
      fulfillment->Path.followFromTypeMetadata(IGF, ConcreteType, self,
                                               /*cache*/ nullptr);
    IGF.Builder.CreateRet(metadata);
    return accessor;
  }

  // Otherwise, we need a cache entry.
  emitReturnOfCheckedLoadFromCache(IGF, destTable, self,
                                   [&]() -> llvm::Value* {
    return IGF.emitTypeMetadataRef(associatedType);
  });

  return accessor;
}

/// Return a function which will return a particular witness table
/// conformance.  The function will be passed the metadata for which
/// the conformance is being requested; it may ignore this (perhaps
/// implicitly by taking no arguments).
static llvm::Constant *
getOrCreateWitnessTableAccessFunction(IRGenModule &IGM, CanType type,
                                      ProtocolConformance *conformance) {
  assert(!type->hasArchetype() && "cannot do this for dependent type");

  // We always emit an access function for conformances, and in principle
  // it is always possible to just use that here directly.  However,
  // if it's dependent, doing so won't allow us to cache the result.
  // For the specific use case of an associated type conformance, we could
  // use a cache in the witness table; but that wastes space per conformance
  // and won't let us re-use the cache with other non-dependent uses in
  // the module.  Therefore, in this case, we use the address of the lazy-cache
  // function.
  //
  // FIXME: we will need to pass additional parameters if the target
  // conformance is conditional.
  auto rootConformance = conformance->getRootNormalConformance();
  if (rootConformance->getDeclContext()->isGenericContext()) {
    return getWitnessTableLazyAccessFunction(IGM, rootConformance, type);
  } else {
    return IGM.getAddrOfWitnessTableAccessFunction(
                                    conformance->getRootNormalConformance(),
                                                   NotForDefinition);
  }
}

llvm::Constant *WitnessTableBuilder::
getAssociatedTypeWitnessTableAccessFunction(AssociatedTypeDecl *requirement,
                                            CanType associatedType,
                                            ProtocolDecl *associatedProtocol,
                                ProtocolConformanceRef associatedConformance) {
  if (!associatedType->hasArchetype()) {
    assert(associatedConformance.isConcrete() &&
           "no concrete conformance for non-dependent type");
    return getOrCreateWitnessTableAccessFunction(IGM, associatedType,
                                          associatedConformance.getConcrete());
  }

  // Otherwise, emit an access function.
  llvm::Function *accessor =
    IGM.getAddrOfAssociatedTypeWitnessTableAccessFunction(&Conformance,
                                                          requirement,
                                                          associatedProtocol);

  IRGenFunction IGF(IGM, accessor);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, accessor);

  Explosion parameters = IGF.collectParameters();

  llvm::Value *associatedTypeMetadata = parameters.claimNext();
  if (IGM.EnableValueNames)
    associatedTypeMetadata->setName(Twine(ConcreteType->getString())
                                      + "." + requirement->getNameStr());

  llvm::Value *self = parameters.claimNext();
  setTypeMetadataName(IGM, self, ConcreteType);

  Address destTable(parameters.claimNext(), IGM.getPointerAlignment());
  setProtocolWitnessTableName(IGM, destTable.getAddress(), ConcreteType,
                              requirement->getProtocol());

  const ConformanceInfo *conformanceI = nullptr;
  if (associatedConformance.isConcrete()) {
    const ProtocolInfo &protocolI = IGM.getProtocolInfo(associatedProtocol);
    conformanceI =
      &protocolI.getConformance(IGM, associatedProtocol,
                                associatedConformance.getConcrete());

    // If we can emit a constant table, do so.
    // In principle, any time we can do this, we should try to re-use this
    // function for other conformances.  But that should typically already
    // be covered by the !hasArchetype() check above.
    if (auto constantTable =
          conformanceI->tryGetConstantTable(IGM, associatedType)) {
      IGF.Builder.CreateRet(constantTable);
      return accessor;
    }
  }

  // If the witness table is directly fulfillable from the type,
  // we don't need a cache entry.
  // TODO: maybe we should have a cache entry anyway if the fulfillment
  // is expensive.
  if (auto fulfillment =
        getFulfillmentMap().getWitnessTable(associatedType,
                                            associatedProtocol)) {
    llvm::Value *wtable =
      fulfillment->Path.followFromTypeMetadata(IGF, ConcreteType, self,
                                               /*cache*/ nullptr);
    IGF.Builder.CreateRet(wtable);
    return accessor;
  }

  assert(conformanceI && "no conformance information, but also couldn't "
         "fulfill witness table contextually");

  // Otherwise, we need a cache entry.
  emitReturnOfCheckedLoadFromCache(IGF, destTable, self,
                                   [&]() -> llvm::Value* {
    return conformanceI->getTable(IGF, associatedType, &associatedTypeMetadata);
  });

  return accessor;
}

void WitnessTableBuilder::
emitReturnOfCheckedLoadFromCache(IRGenFunction &IGF, Address destTable,
                                 llvm::Value *selfMetadata,
                                 llvm::function_ref<llvm::Value*()> body) {
  // Allocate a new cache slot and drill down to it.
  int cacheIndex = -1 - getNextCacheIndex();
  Address cache = IGF.Builder.CreateConstArrayGEP(destTable, cacheIndex,
                                                  IGM.getPointerSize());

  llvm::Type *expectedTy = IGF.CurFn->getReturnType();
  cache = IGF.Builder.CreateBitCast(cache, expectedTy->getPointerTo());

  // Load and check whether it was null.
  auto cachedResult = IGF.Builder.CreateLoad(cache);
  // FIXME: cachedResult->setOrdering(Consume);
  auto cacheIsEmpty = IGF.Builder.CreateIsNull(cachedResult);
  llvm::BasicBlock *fetchBB = IGF.createBasicBlock("fetch");
  llvm::BasicBlock *contBB = IGF.createBasicBlock("cont");
  llvm::BasicBlock *entryBB = IGF.Builder.GetInsertBlock();
  IGF.Builder.CreateCondBr(cacheIsEmpty, fetchBB, contBB);

  // Create a phi in the continuation block and use the loaded value if
  // we branched directly here.  Note that we arrange blocks so that we
  // fall through into this.
  IGF.Builder.emitBlock(contBB);
  auto result = IGF.Builder.CreatePHI(expectedTy, 2);
  result->addIncoming(cachedResult, entryBB);
  IGF.Builder.CreateRet(result);

  // In the fetch block, bind the archetypes and evaluate the body.
  IGF.Builder.emitBlock(fetchBB);
  bindArchetypes(IGF, selfMetadata);

  llvm::Value *fetchedResult = body();

  // Store the fetched result back to the cache.
  // We need to transitively ensure that any stores initializing the result
  // that are visible to us are visible to callers.
  IGF.Builder.CreateStore(fetchedResult, cache)->setOrdering(llvm::Release);

  auto fetchedResultBB = IGF.Builder.GetInsertBlock();
  IGF.Builder.CreateBr(contBB);
  result->addIncoming(fetchedResult, fetchedResultBB);
}

/// Within a metadata or witness-table accessor on this conformance, bind
/// the type metadata and witness tables for all the associated types.
void WitnessTableBuilder::bindArchetypes(IRGenFunction &IGF,
                                         llvm::Value *selfMetadata) {
  auto generics =
    Conformance.getDeclContext()->getGenericParamsOfContext();
  if (!generics) return;

  MetadataPath::Map<llvm::Value*> cache;

  auto &fulfillments = getFulfillmentMap();

  for (auto archetype : generics->getAllArchetypes()) {
    // FIXME: be lazier.

    // Find the type metadata for the archetype.
    //
    // All of the primary archetypes will be fulfilled by the concrete
    // type; otherwise they'd be free.  Everything else we should be able
    // to derive from some parent archetype and its known conformances.
    llvm::Value *archetypeMetadata;
    if (auto fulfillment =
          fulfillments.getTypeMetadata(CanType(archetype))) {
      archetypeMetadata =
        fulfillment->Path.followFromTypeMetadata(IGF, ConcreteType,
                                                 selfMetadata, &cache);
    } else {
      assert(!archetype->isPrimary() && "free type param in conformance?");

      // getAllArchetypes is in dependency order, so the parent archetype
      // should always be mapped.
      auto parentArchetype = CanArchetypeType(archetype->getParent());
      archetypeMetadata =
        emitAssociatedTypeMetadataRef(IGF, parentArchetype,
                                      archetype->getAssocType());
    }

    // Find the witness tables for the archetype.
    //
    // Archetype conformances in a type context can be classified into
    // three buckets:
    //
    //   - They can be inherent to the extended type, e.g. Dictionary's
    //     requirement that its keys be Equatable.  These should always
    //     be fulfillable from the concrete type metadata.
    //
    //   - If the archetype is an associated type, they can be inherent
    //     to that associated type's requirements.  These should always
    //     be available from the associated type's parent conformance.
    //
    //   - Otherwise, the conformance must be a free requirement on the
    //     extension; that is, this must be a conditional conformance.
    //     We don't support this yet, but when we do they'll have to
    //     be stored in the private section of the witness table.
    SmallVector<llvm::Value*, 4> archetypeWitnessTables;
    for (auto protocol : archetype->getConformsTo()) {
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;

      llvm::Value *wtable;
      if (auto fulfillment =
            fulfillments.getWitnessTable(CanType(archetype), protocol)) {
        wtable =
          fulfillment->Path.followFromTypeMetadata(IGF, ConcreteType,
                                                   selfMetadata, &cache);
      } else {
        assert(!archetype->isPrimary() && "conditional conformance?");
        auto parentArchetype = CanArchetypeType(archetype->getParent());
        wtable = emitAssociatedTypeWitnessTableRef(IGF, parentArchetype,
                                                archetype->getAssocType(),
                                                   archetypeMetadata,
                                                   protocol);
      }
      archetypeWitnessTables.push_back(wtable);
    }

    IGF.bindArchetype(archetype, archetypeMetadata, archetypeWitnessTables);
  }
}

/// Emit the access function for this witness table.
void WitnessTableBuilder::buildAccessFunction(llvm::Constant *wtable) {
  llvm::Function *fn =
    IGM.getAddrOfWitnessTableAccessFunction(&Conformance, ForDefinition);

  IRGenFunction IGF(IGM, fn);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, fn);

  wtable = llvm::ConstantExpr::getBitCast(wtable, IGM.WitnessTablePtrTy);

  // If specialization isn't required, just return immediately.
  // TODO: allow dynamic specialization?
  if (!RequiresSpecialization) {
    IGF.Builder.CreateRet(wtable);
    return;
  }

  // The target metadata is the first argument.
  assert(Conformance.getDeclContext()->isGenericContext());
  Explosion params = IGF.collectParameters();
  llvm::Value *metadata = params.claimNext();

  // Okay, we need a cache.  Build the cache structure.
  //  struct GenericWitnessTable {
  //    /// The size of the witness table in words.
  //    uint16_t WitnessTableSizeInWords;
  //
  //    /// The amount to copy from the pattern in words.  The rest is zeroed.
  //    uint16_t WitnessTableSizeInWordsToCopy;
  //
  //    /// The protocol.
  //    RelativeIndirectablePointer<ProtocolDescriptor> Protocol;
  //
  //    /// The pattern.
  //    RelativeDirectPointer<WitnessTable> WitnessTable;
  //
  //    /// The instantiation function, which is called after the template is copied.
  //    RelativeDirectPointer<void(WitnessTable *, const Metadata *)> Instantiator;
  //
  //    void *PrivateData[swift::NumGenericMetadataPrivateDataWords];
  //  };

  // First, create the global.  We have to build this in two phases because
  // it contains relative pointers.
  auto cache = cast<llvm::GlobalVariable>(
    IGM.getAddrOfGenericWitnessTableCache(&Conformance, ForDefinition));

  // We need an instantiation function if the base conformance
  // is non-dependent.
  // TODO: the conformance might be conditional.
  llvm::Constant *instantiationFn;
  llvm::Value *instantiationArgs =
    llvm::ConstantPointerNull::get(IGM.Int8PtrPtrTy);
  if (SpecializedBaseConformances.empty()) {
    instantiationFn = llvm::ConstantInt::get(IGM.RelativeAddressTy, 0);    
  } else {
    llvm::Constant *fn = buildInstantiationFunction();
    instantiationFn = IGM.emitDirectRelativeReference(fn, cache, { 4 });
  }

  auto descriptorRef = IGM.getAddrOfLLVMVariableOrGOTEquivalent(
                LinkEntity::forProtocolDescriptor(Conformance.getProtocol()),
                IGM.getPointerAlignment(), IGM.ProtocolDescriptorStructTy);

  // Fill in the global.
  auto cacheTy = cast<llvm::StructType>(cache->getValueType());
  llvm::Constant *cacheData[] = {
    // WitnessTableSizeInWords
    llvm::ConstantInt::get(IGM.Int16Ty, Table.size()),
    // WitnessTablePrivateSizeInWords
    llvm::ConstantInt::get(IGM.Int16Ty, NextCacheIndex),
    // RelativeIndirectablePointer<ProtocolDescriptor>
    IGM.emitRelativeReference(descriptorRef, cache, { 2 }),
    // RelativePointer<WitnessTable>
    IGM.emitDirectRelativeReference(wtable, cache, { 3 }),
    // Instantiation function
    instantiationFn,
    // Private data
    llvm::Constant::getNullValue(cacheTy->getStructElementType(5))
  };
  cache->setInitializer(llvm::ConstantStruct::get(cacheTy, cacheData));

  auto call = IGF.Builder.CreateCall(IGM.getGetGenericWitnessTableFn(),
                                     { cache, metadata, instantiationArgs });
  call->setCallingConv(IGM.RuntimeCC);
  call->setDoesNotThrow();

  IGF.Builder.CreateRet(call);
}

llvm::Constant *WitnessTableBuilder::buildInstantiationFunction() {
  llvm::Function *fn =
    IGM.getAddrOfGenericWitnessTableInstantiationFunction(&Conformance);
  IRGenFunction IGF(IGM, fn);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, fn);

  // Break out the parameters.
  Explosion params = IGF.collectParameters();
  Address wtable(params.claimNext(), IGM.getPointerAlignment());
  llvm::Value *metadata = params.claimNext();
  llvm::Value *instantiationArgs = params.claimNext();
  (void) instantiationArgs; // unused for now

  // TODO: store any required conditional-conformance information
  // in the private data.

  // Initialize all the specialized base conformances.
  for (auto &base : SpecializedBaseConformances) {
    // Ask the ConformanceInfo to emit the wtable.
    // TODO: we may need to bind extra information in the IGF in order
    // to make conditional conformances work.
    llvm::Value *baseWTable =
      base.second->getTable(IGF, ConcreteType, &metadata);
    baseWTable = IGF.Builder.CreateBitCast(baseWTable, IGM.Int8PtrTy);

    // Store that to the appropriate slot in the new witness table.
    Address slot = IGF.Builder.CreateConstArrayGEP(wtable, base.first,
                                                   IGM.getPointerSize());
    IGF.Builder.CreateStore(baseWTable, slot);
  }

  IGF.Builder.CreateRetVoid();
  return fn;
}

/// Do a memoized witness-table layout for a protocol.
const ProtocolInfo &IRGenModule::getProtocolInfo(ProtocolDecl *protocol) {
  return Types.getProtocolInfo(protocol);
}

/// Do a memoized witness-table layout for a protocol.
const ProtocolInfo &TypeConverter::getProtocolInfo(ProtocolDecl *protocol) {
  // Check whether we've already translated this protocol.
  auto it = Protocols.find(protocol);
  if (it != Protocols.end()) return *it->second;

  // If not, lay out the protocol's witness table, if it needs one.
  WitnessTableLayout layout;
  if (Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
    layout.visitProtocolDecl(protocol);

  // Create a ProtocolInfo object from the layout.
  ProtocolInfo *info = ProtocolInfo::create(layout.getNumWitnesses(),
                                            layout.getEntries());
  info->NextConverted = FirstProtocol;
  FirstProtocol = info;

  // Memoize.
  Protocols.insert(std::make_pair(protocol, info));

  // Done.
  return *info;
}

/// Allocate a new ProtocolInfo.
ProtocolInfo *ProtocolInfo::create(unsigned numWitnesses,
                                   ArrayRef<WitnessTableEntry> table) {
  size_t bufferSize = totalSizeToAlloc<WitnessTableEntry>(table.size());
  void *buffer = ::operator new(bufferSize);
  return new(buffer) ProtocolInfo(numWitnesses, table);
}

ProtocolInfo::~ProtocolInfo() {
  for (auto &conf : Conformances) {
    delete conf.second;
  }
}

/// Find the conformance information for a protocol.
const ConformanceInfo &
ProtocolInfo::getConformance(IRGenModule &IGM, ProtocolDecl *protocol,
                             const ProtocolConformance *conformance) const {
  assert(conformance->getProtocol() == protocol &&
         "conformance is for wrong protocol");

  // Drill down to the root normal conformance.
  auto normalConformance = conformance->getRootNormalConformance();

  // Check whether we've already cached this.
  auto it = Conformances.find(normalConformance);
  if (it != Conformances.end()) return *it->second;

  ConformanceInfo *info;

  // If the conformance is dependent in any way, we need to unique it.
  // TODO: maybe this should apply whenever it's out of the module?
  // TODO: actually enable this
  if (isDependentConformance(IGM, normalConformance,
                             ResilienceExpansion::Maximal)) {
    info = new AccessorConformanceInfo(normalConformance);

  // Otherwise, we can use a direct-referencing conformance.
  } else {
    info = new DirectConformanceInfo(normalConformance);
  }

  Conformances.insert({normalConformance, info});
  return *info;
}

void IRGenModule::emitSILWitnessTable(SILWitnessTable *wt) {
  // Don't emit a witness table if it is a declaration.
  if (wt->isDeclaration())
    return;

  bool mustEmitDefinition = !isAvailableExternally(wt->getLinkage());

  // Don't emit a witness table that is available externally if we are emitting
  // code for the JIT. We do not do any optimization for the JIT and it has
  // problems with external symbols that get merged with non-external symbols.
  if (Opts.UseJIT && !mustEmitDefinition)
    return;

  // Build the witnesses.
  SmallVector<llvm::Constant*, 32> witnesses;
  WitnessTableBuilder wtableBuilder(*this, witnesses, wt);
  wtableBuilder.build();
  
  assert(getProtocolInfo(wt->getConformance()->getProtocol())
           .getNumWitnesses() == witnesses.size()
         && "witness table size doesn't match ProtocolInfo");

  // Produce the initializer value.
  auto tableTy = llvm::ArrayType::get(FunctionPtrTy, witnesses.size());
  auto initializer = llvm::ConstantArray::get(tableTy, witnesses);

  auto global = cast<llvm::GlobalVariable>(
                         getAddrOfWitnessTable(wt->getConformance(), tableTy));
  global->setConstant(true);
  global->setInitializer(initializer);
  global->setAlignment(getWitnessTableAlignment().getValue());

  // FIXME: resilience; this should use the conformance's publishing scope.
  if (mustEmitDefinition) {
    wtableBuilder.buildAccessFunction(global);
  }

  // Build the conformance record, if it lives in this TU.
  if (!mustEmitDefinition)
    return;

  addProtocolConformanceRecord(wt->getConformance());
}


/// True if a function's signature in LLVM carries polymorphic parameters.
/// Generic functions and protocol witnesses carry polymorphic parameters.
bool irgen::hasPolymorphicParameters(CanSILFunctionType ty) {
  switch (ty->getRepresentation()) {
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
    // Should never be polymorphic.
    assert(!ty->isPolymorphic() && "polymorphic C function?!");
    return false;

  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::ObjCMethod:
    return ty->isPolymorphic();

  case SILFunctionTypeRepresentation::WitnessMethod:
    // Always carries polymorphic parameters for the Self type.
    return true;
  }
}

namespace {

  /// A class for computing how to pass arguments to a polymorphic
  /// function.  The subclasses of this are the places which need to
  /// be updated if the convention changes.
  class PolymorphicConvention {
  public:
    enum class SourceKind {
      /// Metadata is derived from a source class pointer.
      ClassPointer,

      /// Metadata is derived from a type metadata pointer.
      Metadata,

      /// Metadata is derived from the origin type parameter.
      GenericLValueMetadata,

      /// Metadata is obtained directly from the from a Self metadata
      /// parameter passed via the WitnessMethod convention.
      SelfMetadata,

      /// Metadata is derived from the Self witness table parameter
      /// passed via the WitnessMethod convention.
      SelfWitnessTable,
    };

    static bool requiresSourceIndex(SourceKind kind) {
      return (kind == SourceKind::ClassPointer ||
              kind == SourceKind::Metadata ||
              kind == SourceKind::GenericLValueMetadata);
    }

    enum : unsigned { InvalidSourceIndex = ~0U };

    class Source {
      /// The kind of source this is.
      SourceKind Kind;

      /// The parameter index, for ClassPointer and Metadata sources.
      unsigned Index;

    public:
      CanType Type;

      Source(SourceKind kind, unsigned index, CanType type)
         : Kind(kind), Index(index), Type(type) {
        assert(index != InvalidSourceIndex || !requiresSourceIndex(kind));
      }

      SourceKind getKind() const { return Kind; }
      unsigned getParamIndex() const {
        assert(requiresSourceIndex(getKind()));
        return Index;
      }
    };

  protected:
    ModuleDecl &M;
    CanSILFunctionType FnType;

    /// This is the canonical "mangling" signature of the function type, which
    /// is minimized in a way such that getAllDependentTypes() excludes
    /// types with equality constraints to concrete types.
    CanGenericSignature Generics;

    std::vector<Source> Sources;

    FulfillmentMap Fulfillments;

    GenericSignature::ConformsToArray getConformsTo(Type t) {
      return Generics->getConformsTo(t, M);
    }

  public:
    PolymorphicConvention(CanSILFunctionType fnType, Module &M)
        : M(M), FnType(fnType) {
      initGenerics();

      auto params = fnType->getParameters();
      unsigned selfIndex = ~0U;

      auto rep = fnType->getRepresentation();

      if (rep == SILFunctionTypeRepresentation::WitnessMethod) {
        // Protocol witnesses always derive all polymorphic parameter
        // information from the Self argument. We also *cannot* consider other
        // arguments; doing so would potentially make the signature
        // incompatible with other witnesses for the same method.
        selfIndex = params.size() - 1;
        considerWitnessSelf(params[selfIndex], selfIndex);
      } else if (rep == SILFunctionTypeRepresentation::ObjCMethod) {
        // Objective-C thunks for generic methods also always derive all
        // polymorphic parameter information from the Self argument.
        selfIndex = params.size() - 1;
        considerObjCGenericSelf(params[selfIndex], selfIndex);
      } else {
        // We don't need to pass anything extra as long as all of the
        // archetypes (and their requirements) are producible from
        // arguments.

        // Consider 'self' first.
        if (fnType->hasSelfParam()) {
          selfIndex = params.size() - 1;
          considerParameter(params[selfIndex], selfIndex, true);
        }

        // Now consider the rest of the parameters.
        for (auto index : indices(params)) {
          if (index != selfIndex)
            considerParameter(params[index], index, false);
        }
      }
    }

    ArrayRef<Source> getSources() const { return Sources; }

    GenericSignatureWitnessIterator getAllDependentTypes() const {
      return Generics ? Generics->getAllDependentTypes()
                      : GenericSignatureWitnessIterator::emptyRange();
    }

  private:
    void initGenerics() {
      assert(hasPolymorphicParameters(FnType));

      // The canonical mangling signature removes dependent types that are
      // equal to concrete types, but isn't necessarily parallel with
      // substitutions.
      Generics = FnType->getGenericSignature();
    }

    void considerNewTypeSource(SourceKind kind, unsigned paramIndex,
                               CanType type, IsExact_t isExact) {
      if (!Fulfillments.isInterestingTypeForFulfillments(type)) return;

      // Prospectively add a source.
      Sources.emplace_back(kind, paramIndex, type);

      // Consider the source.
      if (!considerType(type, isExact, Sources.size() - 1, MetadataPath())) {
        // If it wasn't used in any fulfillments, remove it.
        Sources.pop_back();
      }
    }

    bool considerType(CanType type, IsExact_t isExact,
                      unsigned sourceIndex, MetadataPath &&path) {
      struct Callback : FulfillmentMap::InterestingKeysCallback {
        PolymorphicConvention &Self;
        Callback(PolymorphicConvention &self) : Self(self) {}

        bool isInterestingType(CanType type) const override {
          return type->isTypeParameter();
        }
        bool hasInterestingType(CanType type) const override {
          return type->hasTypeParameter();
        }
        bool hasLimitedInterestingConformances(CanType type) const override {
          return true;
        }
        GenericSignature::ConformsToArray
        getInterestingConformances(CanType type) const override {
          return Self.getConformsTo(type);
        }
      } callbacks(*this);
      return Fulfillments.searchTypeMetadata(M, type, isExact, sourceIndex,
                                             std::move(path), callbacks);
    }

    /// Testify to generic parameters in the Self type of a protocol
    /// witness method.
    void considerWitnessSelf(SILParameterInfo param, unsigned paramIndex) {
      // If this is a static method, get the instance type.
      CanType selfTy = param.getType();
      if (auto metaTy = dyn_cast<AnyMetatypeType>(selfTy))
        selfTy = metaTy.getInstanceType();

      // First, bind type metadata for Self.
      Sources.emplace_back(SourceKind::SelfMetadata, InvalidSourceIndex,
                           selfTy);

      if (auto paramTy = dyn_cast<GenericTypeParamType>(selfTy)) {
        // Don't pass in witness tables for associated types of Self.
        addImpossibleFulfillments(paramTy);

        // The Self type is abstract, so we must pass in a witness table.
        addSelfMetadataFulfillment(paramTy);

        // Look at the witness table for the conformance.
        Sources.emplace_back(SourceKind::SelfWitnessTable, InvalidSourceIndex,
                             selfTy);
        addSelfWitnessTableFulfillment(paramTy);
      } else {
        // If the Self type is concrete, we have a witness thunk with a
        // fully substituted Self type. The witness table parameter is not
        // used.
        considerType(selfTy, IsInexact, Sources.size() - 1, MetadataPath());
      }
    }

    /// Testify to generic parameters in the Self type of an @objc
    /// generic or protocol method.
    void considerObjCGenericSelf(SILParameterInfo param, unsigned paramIndex) {
      // If this is a static method, get the instance type.
      CanType selfTy = param.getType();
      if (auto metaTy = dyn_cast<AnyMetatypeType>(selfTy))
        selfTy = metaTy.getInstanceType();

      // Bind type metadata for Self.
      Sources.emplace_back(SourceKind::ClassPointer, paramIndex,
                           selfTy);

      if (auto paramTy = dyn_cast<GenericTypeParamType>(selfTy))
        addSelfMetadataFulfillment(paramTy);
      else
        considerType(selfTy, IsInexact, Sources.size() - 1, MetadataPath());
    }

    void considerParameter(SILParameterInfo param, unsigned paramIndex,
                           bool isSelfParameter) {
      auto type = param.getType();
      switch (param.getConvention()) {
      // Indirect parameters do give us a value we can use, but right now
      // we don't bother, for no good reason. But if this is 'self',
      // consider passing an extra metatype.
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_InoutAliasable:
        if (!isSelfParameter) return;
        if (type->getNominalOrBoundGenericNominal()) {
          considerNewTypeSource(SourceKind::GenericLValueMetadata,
                                paramIndex, type, IsExact);
        }
        return;

      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Direct_Deallocating:
        // Classes are sources of metadata.
        if (type->getClassOrBoundGenericClass()) {
          considerNewTypeSource(SourceKind::ClassPointer, paramIndex, type,
                                IsInexact);
          return;
        }

        // Thick metatypes are sources of metadata.
        if (auto metatypeTy = dyn_cast<MetatypeType>(type)) {
          if (metatypeTy->getRepresentation() != MetatypeRepresentation::Thick)
            return;

          CanType objTy = metatypeTy.getInstanceType();
          considerNewTypeSource(SourceKind::Metadata, paramIndex, objTy,
                                IsInexact);
          return;
        }

        return;
      }
      llvm_unreachable("bad parameter convention");
    }

    /// We're binding an archetype for a protocol witness, and we're only
    /// passing in metadata for Self, so make sure we don't try passing in
    /// secondary archetypes too, since that would break the calling
    /// convention.
    ///
    /// This works when calling concrete witnesses because there the
    /// associated types are always known; for default implementations,
    /// we will need to do some work still to implement default
    /// implementations for protocols with associated types.
    void addImpossibleFulfillments(CanGenericTypeParamType arg) {
      for (auto depTy : getAllDependentTypes()) {
        // Is this a dependent member?
        auto depMemTy = dyn_cast<DependentMemberType>(CanType(depTy));
        if (!depMemTy)
          continue;

        // Is it rooted in the protocol Self type?
        CanType rootTy;
        do {
          rootTy = depMemTy.getBase();
        } while ((depMemTy = dyn_cast<DependentMemberType>(rootTy)));

        auto rootParamTy = dyn_cast<GenericTypeParamType>(rootTy);
        if (!rootParamTy)
          continue;

        // If so, suppress providing metadata for the type by making up a bogus
        // fulfillment.
        if (rootParamTy == arg) {
          MetadataPath path;
          path.addImpossibleComponent();
          unsigned source = Sources.size() - 1;
          Fulfillments.addFulfillment({depTy, nullptr}, source,
                                      MetadataPath(path));
          for (auto protocol : getConformsTo(depTy)) {
            Fulfillments.addFulfillment({depTy, protocol}, source,
                                        MetadataPath(path));
          }
        }
      }
    }

    void addSelfMetadataFulfillment(CanGenericTypeParamType arg) {
      assert(arg->getDepth() == 0 && arg->getIndex() == 0);

      unsigned source = Sources.size() - 1;
      Fulfillments.addFulfillment({arg, nullptr}, source, MetadataPath());
    }

    void addSelfWitnessTableFulfillment(CanGenericTypeParamType arg) {
      assert(arg->getDepth() == 0 && arg->getIndex() == 0);

      unsigned source = Sources.size() - 1;
      auto protos = getConformsTo(arg);
      assert(protos.size() == 1);
      for (auto protocol : protos) {
        //considerWitnessTable(arg, protocol, Sources.size() - 1, MetadataPath());
        Fulfillments.addFulfillment({arg, protocol}, source, MetadataPath());
      }
    }
  };

  /// A class for binding type parameters of a generic function.
  class EmitPolymorphicParameters : public PolymorphicConvention {
    IRGenFunction &IGF;
    SILFunction &Fn;

    struct SourceValue {
      llvm::Value *Value = nullptr;
      MetadataPath::Map<llvm::Value*> Cache;
    };

    std::vector<SourceValue> SourceValues;

  public:
    EmitPolymorphicParameters(IRGenFunction &IGF,
                              SILFunction &Fn)
      : PolymorphicConvention(Fn.getLoweredFunctionType(),
                              *IGF.IGM.SILMod->getSwiftModule()),
        IGF(IGF), Fn(Fn) {}

    void emit(Explosion &in, WitnessMetadata *witnessMetadata,
              const GetParameterFn &getParameter);

  private:
    // Emit metadata bindings after the source, if any, has been bound.
    void emitWithSourcesBound(Explosion &in);

    CanType getTypeInContext(CanType type) const {
      return Fn.mapTypeIntoContext(type)->getCanonicalType();
    }

    CanType getArgTypeInContext(unsigned paramIndex) const {
      return getTypeInContext(FnType->getParameters()[paramIndex].getType());
    }

    /// Emit the source value for parameters.
    llvm::Value *emitSourceForParameters(const Source &source,
                                         Explosion &in,
                                         WitnessMetadata *witnessMetadata,
                                         const GetParameterFn &getParameter) {
      switch (source.getKind()) {
      case SourceKind::Metadata:
        return getParameter(source.getParamIndex());

      case SourceKind::ClassPointer: {
        unsigned paramIndex = source.getParamIndex();
        llvm::Value *instanceRef = getParameter(paramIndex);
        SILType instanceType =
          SILType::getPrimitiveObjectType(getArgTypeInContext(paramIndex));
        return emitDynamicTypeOfHeapObject(IGF, instanceRef, instanceType);
      }

      case SourceKind::GenericLValueMetadata: {
        CanType argTy = getArgTypeInContext(source.getParamIndex());

        llvm::Value *metatype = in.claimNext();
        setTypeMetadataName(IGF.IGM, metatype, argTy);

        // Mark this as the cached metatype for the l-value's object type.
        IGF.setUnscopedLocalTypeData(argTy, LocalTypeDataKind::forTypeMetadata(),
                                     metatype);
        return metatype;
      }

      case SourceKind::SelfMetadata: {
        assert(witnessMetadata && "no metadata for witness method");
        llvm::Value *metadata = witnessMetadata->SelfMetadata;
        assert(metadata && "no Self metadata for witness method");
        
        // Mark this as the cached metatype for Self.
        CanType argTy = getArgTypeInContext(FnType->getParameters().size() - 1);
        setTypeMetadataName(IGF.IGM, metadata, argTy);
        IGF.setUnscopedLocalTypeData(argTy,
                               LocalTypeDataKind::forTypeMetadata(), metadata);
        return metadata;
      }

      case SourceKind::SelfWitnessTable: {
        assert(witnessMetadata && "no metadata for witness method");
        llvm::Value *wtable = witnessMetadata->SelfWitnessTable;
        assert(wtable && "no Self witness table for witness method");
        
        // Mark this as the cached witness table for Self.
        CanType argTy = getArgTypeInContext(FnType->getParameters().size() - 1);

        if (auto archetypeTy = dyn_cast<ArchetypeType>(argTy)) {
          auto protos = archetypeTy->getConformsTo();
          assert(protos.size() == 1);
          auto *protocol = protos[0];

          setProtocolWitnessTableName(IGF.IGM, wtable, argTy, protocol);
        }

        return wtable;
      }
      }
      llvm_unreachable("bad source kind!");
    }

    /// Produce the metadata value for the given depth, using the
    /// given cache.
    llvm::Value *getMetadataForFulfillment(const Fulfillment &fulfillment) {
      unsigned sourceIndex = fulfillment.SourceIndex;
      auto &source = getSources()[sourceIndex];
      auto &sourceValue = SourceValues[sourceIndex];

      CanType sourceType = getTypeInContext(source.Type);
      return fulfillment.Path.followFromTypeMetadata(IGF, sourceType,
                                                     sourceValue.Value,
                                                     &sourceValue.Cache);
    }
  };
};

/// Emit a polymorphic parameters clause, binding all the metadata necessary.
void EmitPolymorphicParameters::emit(Explosion &in,
                                     WitnessMetadata *witnessMetadata,
                                     const GetParameterFn &getParameter) {
  SourceValues.reserve(getSources().size());
  for (const Source &source : getSources()) {
    llvm::Value *value =
      emitSourceForParameters(source, in, witnessMetadata, getParameter);
    SourceValues.emplace_back();
    SourceValues.back().Value = value;
  }

  emitWithSourcesBound(in);
}

void
EmitPolymorphicParameters::emitWithSourcesBound(Explosion &in) {
  for (auto ncDepTy : getAllDependentTypes()) {
    CanType depTy = ncDepTy->getCanonicalType();

    // Get the corresponding context archetype.
    auto contextTy = getTypeInContext(depTy)->getAs<ArchetypeType>();
    assert(contextTy);

    // Derive the appropriate metadata reference.
    llvm::Value *metadata;

    // If the reference is fulfilled by the source, go for it.
    if (auto fulfillment = Fulfillments.getTypeMetadata(depTy)) {
      metadata = getMetadataForFulfillment(*fulfillment);

    // Otherwise, it's just next in line.
    } else {
      metadata = in.claimNext();
    }

    // Collect all the witness tables.
    SmallVector<llvm::Value *, 8> wtables;
    assert(contextTy->getConformsTo() == makeArrayRef(getConformsTo(depTy)));
    for (auto protocol : contextTy->getConformsTo()) {
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;

      llvm::Value *wtable;

      // If the protocol witness table is fulfilled by the source, go for it.
      if (auto fulfillment = Fulfillments.getWitnessTable(depTy, protocol)) {
        wtable = getMetadataForFulfillment(*fulfillment);

      // Otherwise, it's just next in line.
      } else {
        wtable = in.claimNext();
      }
      wtables.push_back(wtable);
    }
    IGF.bindArchetype(contextTy, metadata, wtables);
  }
}

llvm::Value *
MetadataPath::followFromTypeMetadata(IRGenFunction &IGF,
                                     CanType sourceType,
                                     llvm::Value *source,
                                     Map<llvm::Value*> *cache) const {
  LocalTypeDataKey key = {
    sourceType,
    LocalTypeDataKind::forTypeMetadata()
  };
  return follow(IGF, key, source, Path.begin(), Path.end(), cache);
}

llvm::Value *
MetadataPath::followFromWitnessTable(IRGenFunction &IGF,
                                     CanType conformingType,
                                     ProtocolConformanceRef conformance,
                                     llvm::Value *source,
                                     Map<llvm::Value*> *cache) const {
  LocalTypeDataKey key = {
    conformingType,
    LocalTypeDataKind::forProtocolWitnessTable(conformance)
  };
  return follow(IGF, key, source, Path.begin(), Path.end(), cache);
}

/// Follow this metadata path.
///
/// \param sourceKey - A description of the source value.  Not necessarily
///   an appropriate caching key.
/// \param cache - If given, this cache will be used to short-circuit
///   the lookup; otherwise, the global (but dominance-sensitive) cache
///   in the IRGenFunction will be used.  This caching system is somewhat
///   more efficient than what IGF provides, but it's less general, and it
///   should probably be removed.
llvm::Value *MetadataPath::follow(IRGenFunction &IGF,
                                  LocalTypeDataKey sourceKey,
                                  llvm::Value *source,
                                  iterator begin, iterator end,
                                  Map<llvm::Value*> *cache) {
  assert(source && "no source metadata value!");

  // The invariant is that this iterator starts a path from source and
  // that sourceKey is correctly describes it.
  iterator i = begin;

  // Before we begin emitting code to generate the actual path, try to find
  // the latest point in the path that we've cached a value for.

  // If the caller gave us a cache to use, check that.  This lookup is very
  // efficient and doesn't even require us to parse the prefix.
  if (cache) {
    auto result = cache->findPrefix(begin, end);
    if (result.first) {
      source = *result.first;

      // If that was the end, there's no more work to do; don't bother
      // adjusting the source key.
      if (result.second == end)
        return source;

      // Advance the source key past the cached prefix.
      while (i != result.second) {
        Component component = *i++;
        (void) followComponent(IGF, sourceKey, /*source*/ nullptr, component);
      }
    }

  // Otherwise, make a pass over the path looking for available concrete
  // entries in the IGF's local type data cache.
  } else {
    auto skipI = i;
    LocalTypeDataKey skipKey = sourceKey;
    while (skipI != end) {
      Component component = *skipI++;
      (void) followComponent(IGF, skipKey, /*source*/ nullptr, component);

      // Check the cache for a concrete value.  We don't want an abstract
      // entry because, if one exists, we'll just end up here again
      // recursively.
      if (auto skipSource =
            IGF.tryGetConcreteLocalTypeData(skipKey.getCachingKey())) {
        // If we found one, advance the info for the source to the current
        // point in the path, then continue the search.
        sourceKey = skipKey;
        source = skipSource;
        i = skipI;
      }
    }
  }

  // Drill in on the actual source value.
  while (i != end) {
    auto component = *i++;
    source = followComponent(IGF, sourceKey, source, component);

    // If we have a cache, remember this in the cache at the next position.
    if (cache) {
      cache->insertNew(begin, i, source);

    // Otherwise, insert it into the global cache.
    } else {
      IGF.setScopedLocalTypeData(sourceKey, source);
    }
  }

  return source;
}

/// Drill down on a single stage of component.
///
/// sourceType and sourceDecl will be adjusted to refer to the new
/// component.  Source can be null, in which case this will be the only
/// thing done.
llvm::Value *MetadataPath::followComponent(IRGenFunction &IGF,
                                           LocalTypeDataKey &sourceKey,
                                           llvm::Value *source,
                                           Component component) {
  switch (component.getKind()) {
  case Component::Kind::NominalTypeArgument: {
    assert(sourceKey.Kind == LocalTypeDataKind::forTypeMetadata());
    auto generic = cast<BoundGenericType>(sourceKey.Type);
    auto index = component.getPrimaryIndex();

    auto subs = generic->getSubstitutions(IGF.IGM.SILMod->getSwiftModule(),
                                          nullptr);
    sourceKey.Type = subs[index].getReplacement()->getCanonicalType();

    if (source) {
      source = emitArgumentMetadataRef(IGF, generic->getDecl(), index, source);
      setTypeMetadataName(IGF.IGM, source, sourceKey.Type);
    }
    return source;
  }

  /// Generic type argument protocol conformance.
  case Component::Kind::NominalTypeArgumentConformance: {
    assert(sourceKey.Kind == LocalTypeDataKind::forTypeMetadata());
    auto generic = cast<BoundGenericType>(sourceKey.Type);
    auto argIndex = component.getPrimaryIndex();
    auto confIndex = component.getSecondaryIndex();

    auto subs = generic->getSubstitutions(IGF.IGM.SILMod->getSwiftModule(),
                                          nullptr);
    auto conformance = subs[argIndex].getConformances()[confIndex];
    sourceKey.Type = subs[argIndex].getReplacement()->getCanonicalType();
    sourceKey.Kind = LocalTypeDataKind::forProtocolWitnessTable(conformance);

    if (source) {
      auto protocol = conformance.getRequirement();
      source = emitArgumentWitnessTableRef(IGF, generic->getDecl(), argIndex,
                                           protocol, source);
      setProtocolWitnessTableName(IGF.IGM, source, sourceKey.Type, protocol);
    }
    return source;
  }

  case Component::Kind::NominalParent: {
    assert(sourceKey.Kind == LocalTypeDataKind::forTypeMetadata());
    NominalTypeDecl *nominalDecl;
    if (auto nominal = dyn_cast<NominalType>(sourceKey.Type)) {
      nominalDecl = nominal->getDecl();
      sourceKey.Type = nominal.getParent();
    } else {
      auto generic = cast<BoundGenericType>(sourceKey.Type);
      nominalDecl = generic->getDecl();
      sourceKey.Type = generic.getParent();
    }

    if (source) {
      source = emitParentMetadataRef(IGF, nominalDecl, source);
      setTypeMetadataName(IGF.IGM, source, sourceKey.Type);
    }
    return source;
  }

  case Component::Kind::InheritedProtocol: {
    auto conformance = sourceKey.Kind.getProtocolConformance();
    auto protocol = conformance.getRequirement();
    auto inheritedProtocol =
      protocol->getInheritedProtocols(nullptr)[component.getPrimaryIndex()];

    sourceKey.Kind =
      LocalTypeDataKind::forAbstractProtocolWitnessTable(inheritedProtocol);
    if (conformance.isConcrete()) {
      auto inheritedConformance =
        conformance.getConcrete()->getInheritedConformance(inheritedProtocol);
      if (inheritedConformance) {
        sourceKey.Kind = LocalTypeDataKind::forConcreteProtocolWitnessTable(
                                                          inheritedConformance);
      }
    }

    if (source) {
      auto &pi = IGF.IGM.getProtocolInfo(protocol);
      auto &entry = pi.getWitnessEntry(inheritedProtocol);
      assert(entry.isOutOfLineBase());
      source = emitInvariantLoadOfOpaqueWitness(IGF, source,
                                                entry.getOutOfLineBaseIndex());
      source = IGF.Builder.CreateBitCast(source, IGF.IGM.WitnessTablePtrTy);
      setProtocolWitnessTableName(IGF.IGM, source, sourceKey.Type,
                                  inheritedProtocol);
    }
    return source;
  }

  case Component::Kind::Impossible:
    llvm_unreachable("following an impossible path!");

  } 
  llvm_unreachable("bad metadata path component");
}

/// Collect any required metadata for a witness method from the end of
/// the given parameter list.
void irgen::collectTrailingWitnessMetadata(IRGenFunction &IGF,
                                           SILFunction &fn,
                                           Explosion &params,
                                           WitnessMetadata &witnessMetadata) {
  assert(fn.getLoweredFunctionType()->getRepresentation()
           == SILFunctionTypeRepresentation::WitnessMethod);

  llvm::Value *wtable = params.takeLast();
  assert(wtable->getType() == IGF.IGM.WitnessTablePtrTy &&
         "parameter signature mismatch: witness metadata didn't "
         "end in witness table?");
  wtable->setName("SelfWitnessTable");
  witnessMetadata.SelfWitnessTable = wtable;

  llvm::Value *metatype = params.takeLast();
  assert(metatype->getType() == IGF.IGM.TypeMetadataPtrTy &&
         "parameter signature mismatch: witness metadata didn't "
         "end in metatype?");
  metatype->setName("Self");
  witnessMetadata.SelfMetadata = metatype;
}

/// Perform all the bindings necessary to emit the given declaration.
void irgen::emitPolymorphicParameters(IRGenFunction &IGF,
                                      SILFunction &Fn,
                                      Explosion &in,
                                      WitnessMetadata *witnessMetadata,
                                      const GetParameterFn &getParameter) {
  EmitPolymorphicParameters(IGF, Fn).emit(in, witnessMetadata, getParameter);
}

namespace {
  /// A CRTP class for finding the archetypes we need to bind in order
  /// to perform value operations on the given type.
  struct FindArchetypesForValueOperations
    : CanTypeVisitor<FindArchetypesForValueOperations>
  {
    NecessaryBindings &Bindings;
  public:
    FindArchetypesForValueOperations(NecessaryBindings &bindings)
      : Bindings(bindings) {}

    // We're collecting archetypes.
    void visitArchetypeType(CanArchetypeType type) {
      Bindings.addArchetype(type);
    }

    // We need to walk into tuples.
    void visitTupleType(CanTupleType tuple) {
      for (auto eltType : tuple.getElementTypes()) {
        visit(eltType);
      }
    }

    // Walk into on-stack block storage.
    void visitSILBlockStorageType(CanSILBlockStorageType t) {
      visit(t->getCaptureType());
    }

    // We do not need to walk into any of these types, because their
    // value operations do not depend on the specifics of their
    // sub-structure (or they have none).
    void visitAnyFunctionType(CanAnyFunctionType fn) {}
    void visitSILFunctionType(CanSILFunctionType fn) {}
    void visitBuiltinType(CanBuiltinType type) {}
    void visitAnyMetatypeType(CanAnyMetatypeType type) {}
    void visitModuleType(CanModuleType type) {}
    void visitDynamicSelfType(CanDynamicSelfType type) {}
    void visitProtocolCompositionType(CanProtocolCompositionType type) {}
    void visitReferenceStorageType(CanReferenceStorageType type) {}
    void visitSILBoxType(CanSILBoxType t) {}

    // L-values are impossible.
    void visitLValueType(CanLValueType type) {
      llvm_unreachable("cannot store l-value type directly");
    }
    void visitInOutType(CanInOutType type) {
      llvm_unreachable("cannot store inout type directly");
    }

    // Bind archetypes from the parent of nominal types.
    void visitNominalType(CanNominalType type) {
      if (auto parent = CanType(type->getParent()))
        visit(parent);
    }
    // Bind archetypes from bound generic types and their parents.
    void visitBoundGenericType(CanBoundGenericType type) {
      if (auto parent = CanType(type->getParent()))
        visit(parent);
      for (auto arg : type->getGenericArgs())
        visit(CanType(arg));
    }

    // FIXME: Will need to bind the archetype that this eventually refers to.
    void visitGenericTypeParamType(CanGenericTypeParamType type) { }

    // FIXME: Will need to bind the archetype that this eventually refers to.
    void visitDependentMemberType(CanDependentMemberType type) { }
  };
}

NecessaryBindings
NecessaryBindings::forFunctionInvocations(IRGenModule &IGM,
                                          CanSILFunctionType origType,
                                          CanSILFunctionType substType,
                                          ArrayRef<Substitution> subs) {
  NecessaryBindings bindings;
  // Collect bindings required by the polymorphic parameters to the function.
  for (auto &sub : subs) {
    sub.getReplacement().findIf([&](Type t) -> bool {
      if (auto archetype = dyn_cast<ArchetypeType>(t->getCanonicalType())) {
        bindings.addArchetype(archetype);
      }
      return false;
    });
  }
  return bindings;
}

NecessaryBindings
NecessaryBindings::forValueOperations(IRGenModule &IGM, CanType type) {
  NecessaryBindings bindings;
  FindArchetypesForValueOperations(bindings).visit(type);
  return bindings;
}

Size NecessaryBindings::getBufferSize(IRGenModule &IGM) const {
  unsigned numPointers = 0;

  // We need one pointer for each archetype and witness table.
  for (auto type : Types) {
    numPointers++;
    for (auto proto : type->getConformsTo())
      if (Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
        numPointers++;
  }

  return IGM.getPointerSize() * numPointers;
}

void NecessaryBindings::restore(IRGenFunction &IGF, Address buffer) const {
  if (Types.empty()) return;

  // Cast the buffer to %type**.
  auto metatypePtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
  buffer = IGF.Builder.CreateBitCast(buffer, metatypePtrPtrTy);

  for (unsigned archetypeI = 0, e = Types.size(), metadataI = 0;
       archetypeI != e; ++archetypeI) {
    auto archetype = Types[archetypeI];

    // GEP to the appropriate slot.
    Address slot = buffer;
    if (metadataI) slot = IGF.Builder.CreateConstArrayGEP(slot, metadataI,
                                                  IGF.IGM.getPointerSize());
    ++metadataI;

    // Load the archetype's metatype.
    llvm::Value *metatype = IGF.Builder.CreateLoad(slot);

    // Load the witness tables for the archetype's protocol constraints.
    SmallVector<llvm::Value*, 4> witnesses;
    for (unsigned protocolI : indices(archetype->getConformsTo())) {
      auto protocol = archetype->getConformsTo()[protocolI];
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;
      Address witnessSlot = IGF.Builder.CreateConstArrayGEP(buffer, metadataI,
                                                      IGF.IGM.getPointerSize());
      witnessSlot = IGF.Builder.CreateBitCast(witnessSlot,
                                    IGF.IGM.WitnessTablePtrTy->getPointerTo());
      ++metadataI;
      llvm::Value *witness = IGF.Builder.CreateLoad(witnessSlot);
      witnesses.push_back(witness);
    }

    IGF.bindArchetype(archetype, metatype, witnesses);
  }
}

void NecessaryBindings::save(IRGenFunction &IGF, Address buffer) const {
  if (Types.empty()) return;

  // Cast the buffer to %type**.
  auto metatypePtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
  buffer = IGF.Builder.CreateBitCast(buffer, metatypePtrPtrTy);

  for (unsigned typeI = 0, typeE = Types.size(),
                metadataI = 0; typeI != typeE; ++typeI) {
    auto archetype = Types[typeI];

    // GEP to the appropriate slot.
    Address slot = buffer;
    if (metadataI) slot = IGF.Builder.CreateConstArrayGEP(slot, metadataI,
                                                  IGF.IGM.getPointerSize());
    ++metadataI;

    // Find the metatype for the appropriate archetype and store it in
    // the slot.
    llvm::Value *metatype = IGF.getLocalTypeData(CanType(archetype),
                                          LocalTypeDataKind::forTypeMetadata());
    IGF.Builder.CreateStore(metatype, slot);

    // Find the witness tables for the archetype's protocol constraints and
    // store them in the slot.
    for (auto protocol : archetype->getConformsTo()) {
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;
      Address witnessSlot = IGF.Builder.CreateConstArrayGEP(buffer, metadataI,
                                                      IGF.IGM.getPointerSize());
      witnessSlot = IGF.Builder.CreateBitCast(witnessSlot,
                                    IGF.IGM.WitnessTablePtrTy->getPointerTo());
      ++metadataI;
      llvm::Value *witness =
        IGF.getLocalTypeData(CanType(archetype),
                 LocalTypeDataKind::forAbstractProtocolWitnessTable(protocol));
      IGF.Builder.CreateStore(witness, witnessSlot);
    }
  }
}

void NecessaryBindings::addArchetype(CanArchetypeType type) {
  if (Types.insert(type))
    // Collect the associated archetypes.
    for (auto nested : type->getNestedTypes())
      if (auto assocArchetype = nested.second.getAsArchetype())
        addArchetype(CanArchetypeType(assocArchetype));
}

llvm::Value *irgen::emitImpliedWitnessTableRef(IRGenFunction &IGF,
                                               ArrayRef<ProtocolEntry> entries,
                                               ProtocolDecl *target,
                                     const GetWitnessTableFn &getWitnessTable) {
  ProtocolPath path(IGF.IGM, entries, target);
  auto wtable = getWitnessTable(path.getOriginIndex());
  wtable = path.apply(IGF, wtable);
  return wtable;
}

/// Emit a protocol witness table for a conformance.
llvm::Value *irgen::emitWitnessTableRef(IRGenFunction &IGF,
                                        CanType srcType,
                                        llvm::Value **srcMetadataCache,
                                        ProtocolDecl *proto,
                                        const ProtocolInfo &protoI,
                                        ProtocolConformanceRef conformance) {
  assert(Lowering::TypeConverter::protocolRequiresWitnessTable(proto)
         && "protocol does not have witness tables?!");

  // If we don't have concrete conformance information, the type must be
  // an archetype and the conformance must be via one of the protocol
  // requirements of the archetype. Look at what's locally bound.
  if (conformance.isAbstract()) {
    auto archetype = cast<ArchetypeType>(srcType);
    return emitArchetypeWitnessTableRef(IGF, archetype, proto);
  }

  // All other source types should be concrete enough that we have
  // conformance info for them.  However, that conformance info might be
  // more concrete than we're expecting.
  // TODO: make a best effort to devirtualize, maybe?
  auto concreteConformance = conformance.getConcrete();
  if (concreteConformance->getProtocol() != proto) {
    concreteConformance = concreteConformance->getInheritedConformance(proto);
  }
  auto &conformanceI =
    protoI.getConformance(IGF.IGM, proto, concreteConformance);
  return conformanceI.getTable(IGF, srcType, srcMetadataCache);
}

/// Emit the witness table references required for the given type
/// substitution.
void irgen::emitWitnessTableRefs(IRGenFunction &IGF,
                                 const Substitution &sub,
                                 llvm::Value **metadataCache,
                                 SmallVectorImpl<llvm::Value*> &out) {
  auto conformances = sub.getConformances();

  // We don't need to do anything if we have no protocols to conform to.
  if (conformances.empty()) return;

  // Look at the replacement type.
  CanType replType = sub.getReplacement()->getCanonicalType();

  for (auto &conformance : conformances) {
    auto *proto = conformance.getRequirement();
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
      continue;

    auto wtable = emitWitnessTableRef(IGF, replType, metadataCache,
                                      proto, IGF.IGM.getProtocolInfo(proto),
                                      conformance);

    out.push_back(wtable);
  }
}

namespace {
  class EmitPolymorphicArguments : public PolymorphicConvention {
    IRGenFunction &IGF;
  public:
    EmitPolymorphicArguments(IRGenFunction &IGF,
                             CanSILFunctionType polyFn)
      : PolymorphicConvention(polyFn, *IGF.IGM.SILMod->getSwiftModule()),
        IGF(IGF) {}

    void emit(CanType substInputType, ArrayRef<Substitution> subs,
              WitnessMetadata *witnessMetadata, Explosion &out);

  private:
    void emitEarlySources(CanType substInputType, Explosion &out) {
      for (auto &source : getSources()) {
        switch (source.getKind()) {
        // Already accounted for in the parameters.
        case SourceKind::ClassPointer:
        case SourceKind::Metadata:
          continue;

        // Needs a special argument.
        case SourceKind::GenericLValueMetadata: {
          out.add(IGF.emitTypeMetadataRef(substInputType));
          continue;
        }

        // Witness 'Self' arguments are added as a special case in
        // EmitPolymorphicArguments::emit.
        case SourceKind::SelfMetadata:
        case SourceKind::SelfWitnessTable:
          continue;
        }
        llvm_unreachable("bad source kind!");
      }
    }
  };
}

/// Pass all the arguments necessary for the given function.
void irgen::emitPolymorphicArguments(IRGenFunction &IGF,
                                     CanSILFunctionType origFnType,
                                     CanSILFunctionType substFnType,
                                     ArrayRef<Substitution> subs,
                                     WitnessMetadata *witnessMetadata,
                                     Explosion &out) {
  // Grab the apparent 'self' type.  If there isn't a 'self' type,
  // we're not going to try to access this anyway.
  CanType substInputType;
  if (!substFnType->getParameters().empty()) {
    auto selfParam = substFnType->getParameters().back();
    substInputType = selfParam.getType();
    // If the parameter is a direct metatype parameter, this is a static method
    // of the instance type. We can assume this because:
    // - metatypes cannot directly conform to protocols
    // - even if they could, they would conform as a value type 'self' and thus
    //   be passed indirectly as an @in or @inout parameter.
    if (auto meta = dyn_cast<MetatypeType>(substInputType)) {
      if (!selfParam.isIndirect())
        substInputType = meta.getInstanceType();
    }
  }

  EmitPolymorphicArguments(IGF, origFnType).emit(substInputType, subs,
                                                 witnessMetadata, out);
}

void EmitPolymorphicArguments::emit(CanType substInputType,
                                    ArrayRef<Substitution> subs,
                                    WitnessMetadata *witnessMetadata,
                                    Explosion &out) {
  // Add all the early sources.
  emitEarlySources(substInputType, out);

  // For now, treat all archetypes independently.
  // FIXME: Later, we'll want to emit only the minimal set of archetypes,
  // because non-primary archetypes (which correspond to associated types)
  // will have their witness tables embedded in the witness table corresponding
  // to their parent.
  for (auto ncDepTy : getAllDependentTypes()) {
    CanType depTy = ncDepTy->getCanonicalType();

    // The substitutions should be in the same order.
    const Substitution &sub = subs.front();
    subs = subs.slice(1);

    CanType argType = sub.getReplacement()->getCanonicalType();

    // If same-type constraints have eliminated the genericity of this
    // parameter, it doesn't need an independent metadata parameter.
    if (Generics->isConcreteType(depTy, M))
      continue;

    llvm::Value *argMetadata = nullptr;

    // Add the metadata reference unless it's fulfilled.
    if (!Fulfillments.getTypeMetadata(depTy)) {
      argMetadata = IGF.emitTypeMetadataRef(argType);
      out.add(argMetadata);
    }

    // Nothing else to do if there aren't any protocols to witness.
    auto protocols = getConformsTo(depTy);
    auto conformances = sub.getConformances();
    assert(!conformances.size() || protocols.size() == conformances.size());

    if (protocols.empty()) continue;

    // Add witness tables for each of the required protocols.
    for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
      auto protocol = protocols[i];

      // Skip this if the protocol doesn't require a witness table.
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;

      // Skip this if it's fulfilled by the source.
      if (Fulfillments.getWitnessTable(depTy, protocol))
        continue;

      auto wtable = emitWitnessTableRef(IGF, argType, &argMetadata,
                                        protocol,
                                        IGF.IGM.getProtocolInfo(protocol),
                                        conformances[i]);
      out.add(wtable);
    }
  }
  assert(subs.empty()
         && "did not use all substitutions?!");

  // For a witness call, add the Self argument metadata arguments last.
  for (auto &source : getSources()) {
    switch (source.getKind()) {
    case SourceKind::Metadata:
    case SourceKind::ClassPointer:
      // Already accounted for in the arguments.
      continue;

    case SourceKind::GenericLValueMetadata:
      // Added in the early phase.
      continue;

    case SourceKind::SelfMetadata: {
      assert(witnessMetadata && "no metadata structure for witness method");
      auto self = IGF.emitTypeMetadataRef(substInputType);
      witnessMetadata->SelfMetadata = self;
      continue;
    }

    case SourceKind::SelfWitnessTable: {
      // Added later.
      continue;
    }
    }
    llvm_unreachable("bad source kind");
  }
}

namespace {
  /// A class for expanding a polymorphic signature.
  class ExpandPolymorphicSignature : public PolymorphicConvention {
    IRGenModule &IGM;
  public:
    ExpandPolymorphicSignature(IRGenModule &IGM, CanSILFunctionType fn)
      : PolymorphicConvention(fn, *IGM.SILMod->getSwiftModule()), IGM(IGM) {}

    void expand(SmallVectorImpl<llvm::Type*> &out) {
      for (auto &source : getSources())
        addEarlySource(source, out);

      for (auto ncDepTy : getAllDependentTypes()) {
        CanType depTy = ncDepTy->getCanonicalType();

        // Only emit parameters for independent parameters that haven't been
        // constrained to concrete types.
        if (Generics->isConcreteType(depTy, M))
          continue;

        // Pass the type argument if not fulfilled.
        if (!Fulfillments.getTypeMetadata(depTy)) {
          out.push_back(IGM.TypeMetadataPtrTy);
        }

        // Pass each signature requirement that needs a witness table
        // separately (unless fulfilled).
        for (auto protocol : getConformsTo(depTy)) {
          if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
            continue;

          if (!Fulfillments.getWitnessTable(depTy, protocol)) {
            out.push_back(IGM.WitnessTablePtrTy);
          }
        }
      }
    }

  private:
    /// Add signature elements for the source metadata.
    void addEarlySource(const Source &source,
                        SmallVectorImpl<llvm::Type*> &out) {
      switch (source.getKind()) {
      case SourceKind::ClassPointer: return; // already accounted for
      case SourceKind::Metadata: return; // already accounted for
      case SourceKind::GenericLValueMetadata:
        return out.push_back(IGM.TypeMetadataPtrTy);
      case SourceKind::SelfMetadata:
      case SourceKind::SelfWitnessTable:
        return; // handled as a special case in expand()
      }
      llvm_unreachable("bad source kind");
    }
  };
}

/// Given a generic signature, add the argument types required in order to call it.
void irgen::expandPolymorphicSignature(IRGenModule &IGM,
                                       CanSILFunctionType polyFn,
                                       SmallVectorImpl<llvm::Type*> &out) {
  ExpandPolymorphicSignature(IGM, polyFn).expand(out);
}

void irgen::expandTrailingWitnessSignature(IRGenModule &IGM,
                                           CanSILFunctionType polyFn,
                                           SmallVectorImpl<llvm::Type*> &out) {
  assert(polyFn->getRepresentation()
          == SILFunctionTypeRepresentation::WitnessMethod);

  assert(getTrailingWitnessSignatureLength(IGM, polyFn) == 2);

  // A witness method always provides Self.
  out.push_back(IGM.TypeMetadataPtrTy);

  // A witness method always provides the witness table for Self.
  out.push_back(IGM.WitnessTablePtrTy);
}

void
irgen::emitWitnessMethodValue(IRGenFunction &IGF,
                              CanType baseTy,
                              llvm::Value **baseMetadataCache,
                              SILDeclRef member,
                              ProtocolConformanceRef conformance,
                              Explosion &out) {
  auto fn = cast<AbstractFunctionDecl>(member.getDecl());

  // The protocol we're calling on.
  ProtocolDecl *fnProto = cast<ProtocolDecl>(fn->getDeclContext());

  // Find the witness table.
  // FIXME conformance for concrete type
  auto &fnProtoInfo = IGF.IGM.getProtocolInfo(fnProto);
  llvm::Value *wtable = emitWitnessTableRef(IGF, baseTy, baseMetadataCache,
                                            fnProto,
                                            fnProtoInfo,
                                            conformance);

  // Find the witness we're interested in.
  auto index = fnProtoInfo.getWitnessEntry(fn).getFunctionIndex();
  llvm::Value *witness = emitInvariantLoadOfOpaqueWitness(IGF, wtable, index);
  
  // Cast the witness pointer to i8*.
  witness = IGF.Builder.CreateBitCast(witness, IGF.IGM.Int8PtrTy);
  
  // Build the value.
  out.add(witness);
  out.add(wtable);
}

llvm::FunctionType *IRGenModule::getAssociatedTypeMetadataAccessFunctionTy() {
  if (AssociatedTypeMetadataAccessFunctionTy)
    return AssociatedTypeMetadataAccessFunctionTy;

  auto accessorTy = llvm::FunctionType::get(TypeMetadataPtrTy,
                                            { TypeMetadataPtrTy,
                                              WitnessTablePtrTy },
                                            /*varargs*/ false);
  AssociatedTypeMetadataAccessFunctionTy = accessorTy;
  return accessorTy;
}

llvm::Value *irgen::emitAssociatedTypeMetadataRef(IRGenFunction &IGF,
                                                  llvm::Value *parentMetadata,
                                                  llvm::Value *wtable,
                                          AssociatedTypeDecl *associatedType) {
  auto &pi = IGF.IGM.getProtocolInfo(associatedType->getProtocol());
  auto index = pi.getWitnessEntry(associatedType).getAssociatedTypeIndex();
  llvm::Value *witness = emitInvariantLoadOfOpaqueWitness(IGF, wtable, index);

  // Cast the witness to the appropriate function type.
  auto witnessTy = IGF.IGM.getAssociatedTypeMetadataAccessFunctionTy();
  witness = IGF.Builder.CreateBitCast(witness, witnessTy->getPointerTo());

  // Call the accessor.
  auto call = IGF.Builder.CreateCall(witness, { parentMetadata, wtable });
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.RuntimeCC);

  return call;
}

llvm::FunctionType *
IRGenModule::getAssociatedTypeWitnessTableAccessFunctionTy() {
  if (AssociatedTypeWitnessTableAccessFunctionTy)
    return AssociatedTypeWitnessTableAccessFunctionTy;

  // The associated type metadata is passed first so that this function is
  // CC-compatible with a conformance's witness table access function.
  auto accessorTy = llvm::FunctionType::get(WitnessTablePtrTy,
                                            { TypeMetadataPtrTy,
                                              TypeMetadataPtrTy,
                                              WitnessTablePtrTy },
                                            /*varargs*/ false);
  AssociatedTypeWitnessTableAccessFunctionTy = accessorTy;
  return accessorTy;
}

llvm::Value *
irgen::emitAssociatedTypeWitnessTableRef(IRGenFunction &IGF,
                                         llvm::Value *parentMetadata,
                                         llvm::Value *wtable,
                                         AssociatedTypeDecl *associatedType,
                                         llvm::Value *associatedTypeMetadata,
                                         ProtocolDecl *associatedProtocol) {
  auto &pi = IGF.IGM.getProtocolInfo(associatedType->getProtocol());
  auto index = pi.getWitnessEntry(associatedType)
                 .getAssociatedTypeWitnessTableIndex(associatedProtocol);
  llvm::Value *witness = emitInvariantLoadOfOpaqueWitness(IGF, wtable, index);

  // Cast the witness to the appropriate function type.
  auto witnessTy = IGF.IGM.getAssociatedTypeWitnessTableAccessFunctionTy();
  witness = IGF.Builder.CreateBitCast(witness, witnessTy->getPointerTo());

  // Call the accessor.
  auto call = IGF.Builder.CreateCall(witness,
                            { associatedTypeMetadata, parentMetadata, wtable });
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.RuntimeCC);

  return call;
}
