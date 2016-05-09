//===--- ReflectionContext.h - Swift Type Reflection Context ----*- C++ -*-===//
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
// Implements the context for reflection of values in the address space of a
// remote process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_REFLECTIONCONTEXT_H
#define SWIFT_REFLECTION_REFLECTIONCONTEXT_H

#include "swift/Remote/MemoryReader.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"

#include <iostream>
#include <set>
#include <vector>
#include <unordered_map>

namespace swift {
namespace reflection {

using swift::remote::MemoryReader;
using swift::remote::RemoteAddress;

template <typename Runtime>
class ReflectionContext
       : public remote::MetadataReader<Runtime, TypeRefBuilder> {
  using super = remote::MetadataReader<Runtime, TypeRefBuilder>;

  std::unordered_map<typename super::StoredPointer, const TypeInfo *> Cache;

public:
  using super::getBuilder;
  using super::readIsaMask;
  using super::readTypeFromMetadata;
  using super::readParentFromMetadata;
  using super::readGenericArgFromMetadata;
  using super::readMetadataFromInstance;
  using typename super::StoredPointer;

  explicit ReflectionContext(std::shared_ptr<MemoryReader> reader)
    : super(std::move(reader)) {}

  ReflectionContext(const ReflectionContext &other) = delete;
  ReflectionContext &operator=(const ReflectionContext &other) = delete;

  MemoryReader &getReader() {
    return *this->Reader;
  }

  void dumpAllSections(std::ostream &OS) {
    getBuilder().dumpAllSections();
  }

  void addReflectionInfo(ReflectionInfo I) {
    getBuilder().addReflectionInfo(I);
  }

  /// Return a description of the layout of a class instance with the given
  /// metadata as its isa pointer.
  const TypeInfo *getMetadataTypeInfo(StoredPointer MetadataAddress) {
    // See if we cached the layout already
    auto found = Cache.find(MetadataAddress);
    if (found != Cache.end())
      return found->second;

    auto &TC = getBuilder().getTypeConverter();

    const TypeInfo *TI = nullptr;

    auto TR = readTypeFromMetadata(MetadataAddress);
    auto kind = this->readKindFromMetadata(MetadataAddress);
    if (TR != nullptr && kind.first) {
      switch (kind.second) {
      case MetadataKind::Class: {
        // Figure out where the stored properties of this class begin
        // by looking at the size of the superclass
        bool valid;
        unsigned size, align;
        auto super =
            this->readSuperClassFromClassMetadata(MetadataAddress);
        if (super) {
          std::tie(valid, size, align) =
              this->readInstanceSizeAndAlignmentFromClassMetadata(super);

          // Perform layout
          if (valid)
            TI = TC.getClassInstanceTypeInfo(TR, size, align);
        }
        break;
      }
      default:
        break;
      }
    }

    // Cache the result for future lookups
    Cache[MetadataAddress] = TI;
    return TI;
  }

  /// Return a description of the layout of a class instance with the given
  /// metadata as its isa pointer.
  const TypeInfo *getInstanceTypeInfo(StoredPointer ObjectAddress) {
    auto MetadataAddress = readMetadataFromInstance(ObjectAddress);
    if (!MetadataAddress.first)
      return nullptr;

    auto kind = this->readKindFromMetadata(MetadataAddress.second);
    if (!kind.first)
      return nullptr;

    switch (kind.second) {
    case MetadataKind::Class:
      return getMetadataTypeInfo(MetadataAddress.second);

    case MetadataKind::HeapLocalVariable: {
      auto CDAddr = this->readCaptureDescriptorFromMetadata(MetadataAddress.second);
      if (!CDAddr.first)
        return nullptr;

      auto *CD = getBuilder().getCaptureDescriptor(CDAddr.second);
      if (CD == nullptr)
        return nullptr;

      auto Info = getBuilder().getClosureContextInfo(*CD);

      return getClosureContextInfo(ObjectAddress, Info);
    }

    case MetadataKind::HeapGenericLocalVariable:
      // SIL @box type
      return nullptr;

    case MetadataKind::ErrorObject:
      // ErrorProtocol boxed existential on non-Objective C runtime target
      return nullptr;

    default:
      return nullptr;
    }
  }

  bool
  projectExistential(RemoteAddress ExistentialAddress,
                     const TypeRef *ExistentialTR,
                     const TypeRef **OutInstanceTR,
                     RemoteAddress *OutInstanceAddress) {
    if (ExistentialTR == nullptr)
      return false;

    auto ExistentialTI = getTypeInfo(ExistentialTR);
    if (ExistentialTI == nullptr)
      return false;

    auto ExistentialRecordTI = dyn_cast<const RecordTypeInfo>(ExistentialTI);
    if (ExistentialRecordTI == nullptr)
      return false;

    switch (ExistentialRecordTI->getRecordKind()) {
    // Class existentials have trivial layout.
    // It is itself the pointer to the instance followed by the witness tables.
    case RecordKind::ClassExistential:
      *OutInstanceTR = getBuilder().getTypeConverter().getUnknownObjectTypeRef();
      *OutInstanceAddress = ExistentialAddress;
      return true;

    // Other existentials have two cases:
    // If the value fits in three words, it starts at the beginning of the
    // container. If it doesn't, the first word is a pointer to a heap box.
    case RecordKind::Existential: {
      auto Fields = ExistentialRecordTI->getFields();
      auto ExistentialMetadataField = std::find_if(Fields.begin(), Fields.end(),
                                   [](const FieldInfo &FI) -> bool {
        return FI.Name.compare("metadata") == 0;
      });
      if (ExistentialMetadataField == Fields.end())
        return false;

      // Get the metadata pointer for the contained instance type.
      // This is equivalent to:
      // auto PointerArray = reinterpret_cast<uintptr_t*>(ExistentialAddress);
      // uintptr_t MetadataAddress = PointerArray[Offset];
      auto MetadataAddressAddress
        = RemoteAddress(ExistentialAddress.getAddressData() +
                        ExistentialMetadataField->Offset);

      StoredPointer MetadataAddress = 0;
      if (!getReader().readInteger(MetadataAddressAddress, &MetadataAddress))
        return false;

      auto InstanceTR = readTypeFromMetadata(MetadataAddress);
      if (!InstanceTR)
        return false;

      *OutInstanceTR = InstanceTR;

      auto InstanceTI = getTypeInfo(InstanceTR);
      if (!InstanceTI)
        return false;

      if (InstanceTI->getSize() <= ExistentialMetadataField->Offset) {
        // The value fits in the existential container, so it starts at the
        // start of the container.
        *OutInstanceAddress = ExistentialAddress;
      } else {
        // Otherwise it's in a box somewhere off in the heap. The first word
        // of the container has the address to that box.
        StoredPointer BoxAddress = 0;

        if (!getReader().readInteger(ExistentialAddress, &BoxAddress))
          return false;

        *OutInstanceAddress = RemoteAddress(BoxAddress);
      }
      return true;
    }
    default:
      return false;
    }
  }

  /// Return a description of the layout of a value with the given type.
  const TypeInfo *getTypeInfo(const TypeRef *TR) {
    return getBuilder().getTypeConverter().getTypeInfo(TR);
  }

private:
  const TypeInfo *getClosureContextInfo(StoredPointer Context,
                                        const ClosureContextInfo &Info) {
    RecordTypeInfoBuilder Builder(getBuilder().getTypeConverter(),
                                  RecordKind::ClosureContext);

    auto Metadata = readMetadataFromInstance(Context);
    if (!Metadata.first)
      return nullptr;

    // Calculate the offset of the first capture.
    // See GenHeap.cpp, buildPrivateMetadata().
    auto OffsetToFirstCapture =
        this->readOffsetToFirstCaptureFromMetadata(Metadata.second);
    if (!OffsetToFirstCapture.first)
      return nullptr;

    // Initialize the builder.
    Builder.addField(OffsetToFirstCapture.second, sizeof(StoredPointer));

    // FIXME: should be unordered_set but I'm too lazy to write a hash
    // functor
    std::set<std::pair<const TypeRef *, const MetadataSource *>> Done;
    GenericArgumentMap Subs;

    ArrayRef<const TypeRef *> CaptureTypes = Info.CaptureTypes;

    // Closure context element layout depends on the layout of the
    // captured types, but captured types might depend on element
    // layout (of previous elements). Use an iterative approach to
    // solve the problem.
    while (!CaptureTypes.empty()) {
      const TypeRef *OrigCaptureTR = CaptureTypes[0];
      const TypeRef *SubstCaptureTR = nullptr;

      // If we have enough substitutions to make this captured value's
      // type concrete, or we know it's size anyway (because it is a
      // class reference or metatype, for example), go ahead and add
      // it to the layout.
      if (OrigCaptureTR->isConcreteAfterSubstitutions(Subs))
        SubstCaptureTR = OrigCaptureTR->subst(getBuilder(), Subs);
      else if (getBuilder().getTypeConverter().hasFixedSize(OrigCaptureTR))
        SubstCaptureTR = OrigCaptureTR;

      if (SubstCaptureTR != nullptr) {
        Builder.addField("", SubstCaptureTR);
        if (Builder.isInvalid())
          return nullptr;

        // Try the next capture type.
        CaptureTypes = CaptureTypes.slice(1);
        continue;
      }

      // Ok, we do not have enough substitutions yet. Perhaps we have
      // enough elements figured out that we can pick off some
      // metadata sources though, and use those to derive some new
      // substitutions.
      bool Progress = false;
      for (auto Source : Info.MetadataSources) {
        // Don't read a source more than once.
        if (Done.count(Source))
          continue;

        // If we don't have enough information to read this source
        // (because it is fulfilled by metadata from a capture at
        // at unknown offset), keep going.
        if (!isMetadataSourceReady(Source.second, Builder))
          continue;

        auto Metadata = readMetadataSource(Context, Source.second, Builder);
        if (!Metadata.first)
          return nullptr;

        auto *SubstTR = readTypeFromMetadata(Metadata.second);
        if (SubstTR == nullptr)
          return nullptr;

        if (!TypeRef::deriveSubstitutions(Subs, Source.first, SubstTR))
          return nullptr;

        Done.insert(Source);
        Progress = true;
      }

      // If we failed to make any forward progress above, we're stuck
      // and cannot close out this layout.
      if (!Progress)
        return nullptr;
    }

    // Ok, we have a complete picture now.
    return Builder.build();
  }

  /// Checks if we have enough information to read the given metadata
  /// source.
  ///
  /// \param Builder Used to obtain offsets of elements known so far.
  bool isMetadataSourceReady(const MetadataSource *MS,
                             const RecordTypeInfoBuilder &Builder) {
    switch (MS->getKind()) {
    case MetadataSourceKind::ClosureBinding:
      return true;
    case MetadataSourceKind::ReferenceCapture: {
      unsigned Index = cast<ReferenceCaptureMetadataSource>(MS)->getIndex();
      return Index < Builder.getNumFields();
    }
    case MetadataSourceKind::MetadataCapture: {
      unsigned Index = cast<MetadataCaptureMetadataSource>(MS)->getIndex();
      return Index < Builder.getNumFields();
    }
    case MetadataSourceKind::GenericArgument: {
      auto Base = cast<GenericArgumentMetadataSource>(MS)->getSource();
      return isMetadataSourceReady(Base, Builder);
    }
    case MetadataSourceKind::Parent: {
      auto Base = cast<ParentMetadataSource>(MS)->getChild();
      return isMetadataSourceReady(Base, Builder);
    }
    case MetadataSourceKind::Self:
    case MetadataSourceKind::SelfWitnessTable:
      return true;
    }
  }

  /// Read metadata for a captured generic type from a closure context.
  ///
  /// \param Context The closure context in the remote process.
  ///
  /// \param MS The metadata source, which must be "ready" as per the
  /// above.
  ///
  /// \param Builder Used to obtain offsets of elements known so far.
  std::pair<bool, StoredPointer>
  readMetadataSource(StoredPointer Context,
                     const MetadataSource *MS,
                     const RecordTypeInfoBuilder &Builder) {
    switch (MS->getKind()) {
    case MetadataSourceKind::ClosureBinding: {
      unsigned Index = cast<ClosureBindingMetadataSource>(MS)->getIndex();

      // Skip the context's isa pointer.
      //
      // Metadata bindings are stored consecutively at the beginning of the
      // closure context.
      unsigned Offset = ((sizeof(StoredPointer) == 4 ? 12 : 16) +
                         sizeof(StoredPointer) * Index);

      StoredPointer MetadataAddress;
      if (!getReader().readInteger(RemoteAddress(Context + Offset),
                                   &MetadataAddress))
        break;

      return std::make_pair(true, MetadataAddress);
    }
    case MetadataSourceKind::ReferenceCapture: {
      unsigned Index = cast<ReferenceCaptureMetadataSource>(MS)->getIndex();

      // We should already have enough type information to know the offset
      // of this capture in the context.
      unsigned CaptureOffset = Builder.getFieldOffset(Index);

      StoredPointer CaptureAddress;
      if (!getReader().readInteger(RemoteAddress(Context + CaptureOffset),
                                   &CaptureAddress))
        break;

      // Read the requested capture's isa pointer.
      return readMetadataFromInstance(CaptureAddress);
    }
    case MetadataSourceKind::MetadataCapture: {
      unsigned Index = cast<MetadataCaptureMetadataSource>(MS)->getIndex();

      // We should already have enough type information to know the offset
      // of this capture in the context.
      unsigned CaptureOffset = Builder.getFieldOffset(Index);

      StoredPointer CaptureAddress;
      if (!getReader().readInteger(RemoteAddress(Context + CaptureOffset),
                                   &CaptureAddress))
        break;

      return std::make_pair(true, CaptureAddress);
    }
    case MetadataSourceKind::GenericArgument: {
      auto *GAMS = cast<GenericArgumentMetadataSource>(MS);
      auto Base = readMetadataSource(Context, GAMS->getSource(), Builder);
      if (!Base.first)
        break;

      unsigned Index = GAMS->getIndex();
      auto Arg = readGenericArgFromMetadata(Base.second, Index);
      if (!Arg.first)
        break;

      return Arg;
    }
    case MetadataSourceKind::Parent: {
      auto Base = readMetadataSource(Context,
          cast<ParentMetadataSource>(MS)->getChild(),
          Builder);
      if (!Base.first)
        break;

      auto Parent = readParentFromMetadata(Base.second);
      if (!Parent.first)
        break;

      return Parent;
    }
    case MetadataSourceKind::Self:
    case MetadataSourceKind::SelfWitnessTable:
      break;
    }

    return std::make_pair(false, StoredPointer(0));
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H
