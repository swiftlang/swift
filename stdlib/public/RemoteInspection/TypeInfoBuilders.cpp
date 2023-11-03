//===--- TypeInfoBuilders.cpp - Swift Type Reference Builder --------------===//
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
//
// Implements utilities for building TypeInfos.
//
//===----------------------------------------------------------------------===//

#if SWIFT_ENABLE_REFLECTION

#include "swift/RemoteInspection/TypeInfoBuilders.h"

using namespace swift;
using namespace reflection;

bool RecordTypeInfoBuilder::isInvalid() const { return Invalid; }

unsigned RecordTypeInfoBuilder::addField(unsigned fieldSize,
                                         unsigned fieldAlignment,
                                         unsigned numExtraInhabitants,
                                         bool bitwiseTakable) {

  assert(fieldAlignment > 0);

  // Align the current size appropriately
  Size = ((Size + fieldAlignment - 1) & ~(fieldAlignment - 1));

  // Record the offset
  unsigned offset = Size;

  // Update the aggregate size
  Size += fieldSize;

  // Update the aggregate alignment
  Alignment = std::max(Alignment, fieldAlignment);

  // The aggregate is bitwise takable if all elements are.
  BitwiseTakable &= bitwiseTakable;

  switch (Kind) {
  // The extra inhabitants of a struct or tuple are the same as the extra
  // inhabitants of the field that has the most.
  // Opaque existentials pick up the extra inhabitants of their type metadata
  // field.
  case RecordKind::Struct:
  case RecordKind::OpaqueExistential:
  case RecordKind::Tuple:
    NumExtraInhabitants = std::max(NumExtraInhabitants, numExtraInhabitants);
    break;

  // For other kinds of records, we only use the extra inhabitants of the
  // first field.
  case RecordKind::ClassExistential:
  case RecordKind::ClassInstance:
  case RecordKind::ClosureContext:
  case RecordKind::ErrorExistential:
  case RecordKind::ExistentialMetatype:
  case RecordKind::Invalid:
  case RecordKind::ThickFunction:
    if (Empty) {
      NumExtraInhabitants = numExtraInhabitants;
    }
    break;
  }
  Empty = false;

  return offset;
}

// Add a field of a record type, such as a struct.
void RecordTypeInfoBuilder::addField(
    const std::string &Name, const TypeRef *TR,
    remote::TypeInfoProvider *ExternalTypeInfo) {
  const TypeInfo *TI = TC.getTypeInfo(TR, ExternalTypeInfo);
  if (TI == nullptr) {
    DEBUG_LOG(fprintf(stderr, "No TypeInfo for field type: "); TR->dump());
    Invalid = true;
    return;
  }

  unsigned offset =
      addField(TI->getSize(), TI->getAlignment(), TI->getNumExtraInhabitants(),
               TI->isBitwiseTakable());
  Fields.push_back({Name, offset, -1, TR, *TI});
}

const RecordTypeInfo *RecordTypeInfoBuilder::build() {
  if (Invalid)
    return nullptr;

  // Calculate the stride
  unsigned Stride = ((Size + Alignment - 1) & ~(Alignment - 1));
  if (Stride == 0)
    Stride = 1;

  return TC.makeTypeInfo<RecordTypeInfo>(Size, Alignment, Stride,
                                         NumExtraInhabitants, BitwiseTakable,
                                         Kind, Fields);
}

unsigned RecordTypeInfoBuilder::getNumFields() const { return Fields.size(); }

unsigned RecordTypeInfoBuilder::getFieldOffset(unsigned Index) const {
  return Fields[Index].Offset;
}

const TypeRef *EnumTypeInfoBuilder::getCaseTypeRef(FieldTypeInfo Case) {
  // An indirect case is like a payload case with an argument type
  // of Builtin.NativeObject.
  if (Case.Indirect)
    return TC.getNativeObjectTypeRef();

  return Case.TR;
}

void EnumTypeInfoBuilder::addCase(const std::string &Name) {
  // FieldInfo's TI field is a reference, so give it a reference to a value
  // that stays alive forever.
  static TypeInfo emptyTI;
  Cases.push_back({Name, /*offset=*/0, /*value=*/-1, nullptr, emptyTI});
}

void EnumTypeInfoBuilder::addCase(const std::string &Name, const TypeRef *TR,
                                  const TypeInfo *TI) {
  if (TI == nullptr) {
    DEBUG_LOG(fprintf(stderr, "No TypeInfo for case type: "); TR->dump());
    Invalid = true;
    static TypeInfo emptyTI;
    Cases.push_back({Name, /*offset=*/0, /*value=*/-1, TR, emptyTI});
  } else {
    Size = std::max(Size, TI->getSize());
    Alignment = std::max(Alignment, TI->getAlignment());
    BitwiseTakable &= TI->isBitwiseTakable();
    Cases.push_back({Name, /*offset=*/0, /*value=*/-1, TR, *TI});
  }
}

// Recursively populate the spare bit mask for this single type
static bool populateSpareBitsMask(const TypeInfo *TI, BitMask &mask,
                                  uint64_t mpePointerSpareBits);

// Recursively populate the spare bit mask for this collection of
// record fields or enum cases.
static bool populateSpareBitsMask(const std::vector<FieldInfo> &Fields,
                                  BitMask &mask, uint64_t mpePointerSpareBits) {
  for (auto Field : Fields) {
    if (Field.TR != 0) {
      BitMask submask(Field.TI.getSize());
      if (!populateSpareBitsMask(&Field.TI, submask, mpePointerSpareBits)) {
        return false;
      }
      mask.andMask(submask, Field.Offset);
    }
  }
  return true;
}

// General recursive type walk to combine spare bit info from nested structures.
static bool populateSpareBitsMask(const TypeInfo *TI, BitMask &mask,
                                  uint64_t mpePointerSpareBits) {
  switch (TI->getKind()) {
  case TypeInfoKind::Reference: {
    if (TI->getSize() == 8) {
      mask.andMask(mpePointerSpareBits, 0);
    } else /* TI->getSize() == 4 */ {
      uint32_t pointerMask = (uint32_t)mpePointerSpareBits;
      mask.andMask(pointerMask, 0);
    }
    break;
  }
  case TypeInfoKind::Enum: {
    auto EnumTI = reinterpret_cast<const EnumTypeInfo *>(TI);
    // Remove bits used by the payloads
    if (!populateSpareBitsMask(EnumTI->getCases(), mask, mpePointerSpareBits)) {
      return false;
    }
    // TODO: Remove bits needed to discriminate payloads.
    // Until then, return false for any type with an enum in it so we
    // won't claim to support something we don't.
    return false;
    break;
  }
  case TypeInfoKind::Record: {
    auto RecordTI = dyn_cast<RecordTypeInfo>(TI);
    if (!populateSpareBitsMask(RecordTI->getFields(), mask,
                               mpePointerSpareBits)) {
      return false;
    }
    break;
  }
  default: {
    mask.makeZero();
    break;
  }
  }
  return true;
}

const TypeInfo *
EnumTypeInfoBuilder::build(const TypeRef *TR, RemoteRef<FieldDescriptor> FD,
                           remote::TypeInfoProvider *ExternalTypeInfo) {
  // Count various categories of cases:
  unsigned NonPayloadCases = 0;             // `case a`
  unsigned NonGenericEmptyPayloadCases = 0; // `case a(Void)` or `case b(Never)`
  unsigned NonGenericNonEmptyPayloadCases =
      0; // `case a(Int)` or `case d([Int?])`
  unsigned GenericPayloadCases =
      0; // `case a(T)` or `case a([String : (Int, T)])`

  // For a single-payload enum, this is the only payload
  const TypeRef *LastPayloadCaseTR = nullptr;

  std::vector<FieldTypeInfo> Fields;
  if (!TC.getBuilder().getFieldTypeRefs(TR, FD, ExternalTypeInfo, Fields)) {
    Invalid = true;
    return nullptr;
  }

  for (auto Case : Fields) {
    if (Case.TR == nullptr) {
      ++NonPayloadCases;
      addCase(Case.Name);
    } else {
      auto *CaseTR = getCaseTypeRef(Case);
      assert(CaseTR != nullptr);
      auto *CaseTI = TC.getTypeInfo(CaseTR, ExternalTypeInfo);
      if (CaseTI == nullptr) {
        // We don't have typeinfo; something is very broken.
        Invalid = true;
        return nullptr;
      } else if (Case.Generic) {
        ++GenericPayloadCases;
        LastPayloadCaseTR = CaseTR;
      } else if (CaseTI->getSize() == 0) {
        ++NonGenericEmptyPayloadCases;
      } else {
        ++NonGenericNonEmptyPayloadCases;
        LastPayloadCaseTR = CaseTR;
      }
      addCase(Case.Name, CaseTR, CaseTI);
    }
  }
  // For determining a layout strategy, cases w/ empty payload are treated the
  // same as cases with no payload, and generic cases are always considered
  // non-empty.
  unsigned EffectiveNoPayloadCases =
      NonPayloadCases + NonGenericEmptyPayloadCases;
  unsigned EffectivePayloadCases =
      GenericPayloadCases + NonGenericNonEmptyPayloadCases;

  if (Cases.empty()) {
    return TC.makeTypeInfo<EmptyEnumTypeInfo>(Cases);
  }

  // `Kind` is used when dumping data, so it reflects how the enum was
  // declared in source; the various *TypeInfo classes mentioned below reflect
  // the in-memory layout, which may be different because non-generic cases
  // with zero-sized payloads get treated for layout purposes as non-payload
  // cases.
  EnumKind Kind;
  switch (GenericPayloadCases + NonGenericEmptyPayloadCases +
          NonGenericNonEmptyPayloadCases) {
  case 0:
    Kind = EnumKind::NoPayloadEnum;
    break;
  case 1:
    Kind = EnumKind::SinglePayloadEnum;
    break;
  default:
    Kind = EnumKind::MultiPayloadEnum;
    break;
  }

  if (Cases.size() == 1) {
    if (EffectivePayloadCases == 0) {
      // Zero-sized enum with only one empty case
      return TC.makeTypeInfo<TrivialEnumTypeInfo>(Kind, Cases);
    } else {
      // Enum that has only one payload case is represented as that case
      return TC.getTypeInfo(LastPayloadCaseTR, ExternalTypeInfo);
    }
  }

  if (EffectivePayloadCases == 0) {
    // Enum with no non-empty payloads.  (It may
    // formally be a single-payload or multi-payload enum,
    // but all the actual payloads have zero size.)

    // Represent it as a 1-, 2-, or 4-byte integer
    unsigned Size, NumExtraInhabitants;
    if (EffectiveNoPayloadCases < 256) {
      Size = 1;
      NumExtraInhabitants = 256 - EffectiveNoPayloadCases;
    } else if (EffectiveNoPayloadCases < 65536) {
      Size = 2;
      NumExtraInhabitants = 65536 - EffectiveNoPayloadCases;
    } else {
      Size = 4;
      NumExtraInhabitants =
          std::numeric_limits<uint32_t>::max() - EffectiveNoPayloadCases + 1;
    }
    if (NonGenericEmptyPayloadCases > 0) {
      // This enum uses no-payload layout, but the source actually does
      // have payloads (they're just all zero-sized).
      // If this is really a single-payload or multi-payload enum, we
      // formally take extra inhabitants from the first payload, which is
      // zero sized in this case.
      NumExtraInhabitants = 0;
    }
    if (NumExtraInhabitants > ValueWitnessFlags::MaxNumExtraInhabitants) {
      NumExtraInhabitants = ValueWitnessFlags::MaxNumExtraInhabitants;
    }
    return TC.makeTypeInfo<NoPayloadEnumTypeInfo>(
        /* Size */ Size, /* Alignment */ Size, /* Stride */ Size,
        NumExtraInhabitants, Kind, Cases);
  }

  if (EffectivePayloadCases == 1) {
    // SinglePayloadEnumImplStrategy

    // This is a true single-payload enum with
    // a single non-zero-sized payload, or an MPE
    // with a single payload that is not statically empty.
    // It also has at least one non-payload (or empty) case.

    auto *CaseTR = LastPayloadCaseTR;
    auto *CaseTI = TC.getTypeInfo(CaseTR, ExternalTypeInfo);
    if (CaseTR == nullptr || CaseTI == nullptr) {
      return nullptr;
    }
    // Below logic should match the runtime function
    // swift_initEnumMetadataSinglePayload().
    auto PayloadExtraInhabitants = CaseTI->getNumExtraInhabitants();
    if (PayloadExtraInhabitants >= EffectiveNoPayloadCases) {
      // Extra inhabitants can encode all no-payload cases.
      NumExtraInhabitants = PayloadExtraInhabitants - EffectiveNoPayloadCases;
    } else {
      // Not enough extra inhabitants for all cases. We have to add an
      // extra tag field.
      NumExtraInhabitants = 0;
      auto tagCounts = getEnumTagCounts(Size, EffectiveNoPayloadCases,
                                        /*payloadCases=*/1);
      Size += tagCounts.numTagBytes;
      Alignment = std::max(Alignment, tagCounts.numTagBytes);
    }
    unsigned Stride = ((Size + Alignment - 1) & ~(Alignment - 1));
    return TC.makeTypeInfo<SinglePayloadEnumTypeInfo>(
        Size, Alignment, Stride, NumExtraInhabitants, BitwiseTakable, Kind,
        Cases);
  }

  //
  // Multi-Payload Enum strategies
  //
  // We now know this is a multi-payload enum with at least one non-zero-sized
  // payload case.
  //

  // Do we have a fixed layout?
  // TODO: Test whether a missing FixedDescriptor is actually relevant.
  auto FixedDescriptor = TC.getBuilder().getBuiltinTypeInfo(TR);
  if (!FixedDescriptor || GenericPayloadCases > 0) {
    // This is a "dynamic multi-payload enum".  For example,
    // this occurs with:
    // ```
    // class ClassWithEnum<T> {
    //   enum E {
    //   case t(T)
    //   case u(Int)
    //   }
    //   var e: E?
    // }
    // ```
    auto tagCounts =
        getEnumTagCounts(Size, EffectiveNoPayloadCases, EffectivePayloadCases);
    Size += tagCounts.numTagBytes;
    if (tagCounts.numTagBytes >= 4) {
      NumExtraInhabitants = ValueWitnessFlags::MaxNumExtraInhabitants;
    } else {
      NumExtraInhabitants =
          (1 << (tagCounts.numTagBytes * 8)) - tagCounts.numTags;
      if (NumExtraInhabitants > ValueWitnessFlags::MaxNumExtraInhabitants) {
        NumExtraInhabitants = ValueWitnessFlags::MaxNumExtraInhabitants;
      }
    }
    unsigned Stride = ((Size + Alignment - 1) & ~(Alignment - 1));
    if (Stride == 0)
      Stride = 1;
    return TC.makeTypeInfo<TaggedMultiPayloadEnumTypeInfo>(
        Size, Alignment, Stride, NumExtraInhabitants, BitwiseTakable, Cases,
        EffectivePayloadCases);
  }

  // This is a multi-payload enum that:
  //  * Has no generic cases
  //  * Has at least two cases with non-zero payload size
  //  * Has a descriptor stored as BuiltinTypeInfo
  Size = FixedDescriptor->Size;
  Alignment = FixedDescriptor->getAlignment();
  NumExtraInhabitants = FixedDescriptor->NumExtraInhabitants;
  BitwiseTakable = FixedDescriptor->isBitwiseTakable();
  unsigned Stride = ((Size + Alignment - 1) & ~(Alignment - 1));
  if (Stride == 0)
    Stride = 1;
  auto PayloadSize = EnumTypeInfo::getPayloadSizeForCases(Cases);

  // If there's a multi-payload enum descriptor, then we
  // have spare bits information from the compiler.

  // Uncomment the following line to dump the MPE section every time we come
  // through here...
  // TC.getBuilder().dumpMultiPayloadEnumSection(std::cerr); // DEBUG helper

  auto MPEDescriptor = TC.getBuilder().getMultiPayloadEnumInfo(TR);
  if (MPEDescriptor && MPEDescriptor->usesPayloadSpareBits()) {
    auto PayloadSpareBitMaskByteCount =
        MPEDescriptor->getPayloadSpareBitMaskByteCount();
    auto PayloadSpareBitMaskByteOffset =
        MPEDescriptor->getPayloadSpareBitMaskByteOffset();
    auto SpareBitMask = MPEDescriptor->getPayloadSpareBits();
    BitMask spareBitsMask(PayloadSize, SpareBitMask,
                          PayloadSpareBitMaskByteCount,
                          PayloadSpareBitMaskByteOffset);

    if (spareBitsMask.isZero()) {
      // If there are no spare bits, use the "simple" tag-only implementation.
      return TC.makeTypeInfo<TaggedMultiPayloadEnumTypeInfo>(
          Size, Alignment, Stride, NumExtraInhabitants, BitwiseTakable, Cases,
          EffectivePayloadCases);
    }

#if 0 // TODO: This should be !defined(NDEBUG)
      // DEBUG verification that compiler mask and locally-computed
      // mask are the same (whenever both are available).
      BitMask locallyComputedSpareBitsMask(PayloadSize);
      auto mpePointerSpareBits = TC.getBuilder().getMultiPayloadEnumPointerMask();
      auto locallyComputedSpareBitsMaskIsValid
        = populateSpareBitsMask(Cases, locallyComputedSpareBitsMask, mpePointerSpareBits);
      // If the local computation were always correct, we could:
      // assert(locallyComputedSpareBitsMaskIsValid);
      if (locallyComputedSpareBitsMaskIsValid) {
        // Whenever the compiler and local computation both produce
        // data, they should agree.
        // TODO: Make this true, then change `#if 0` above
        assert(locallyComputedSpareBitsMask == spareBitsMask);
      }
#endif

    // Use compiler-provided spare bit information
    return TC.makeTypeInfo<MultiPayloadEnumTypeInfo>(
        Size, Alignment, Stride, NumExtraInhabitants, BitwiseTakable, Cases,
        spareBitsMask, EffectivePayloadCases);
  }

  // Either there was no compiler data or it didn't make sense
  // (existed but claimed to have no mask).
  // Try computing the mask ourselves: This is less robust, but necessary to
  // support images from older compilers.
  BitMask spareBitsMask(PayloadSize);
  auto mpePointerSpareBits = TC.getBuilder().getMultiPayloadEnumPointerMask();
  auto validSpareBitsMask =
      populateSpareBitsMask(Cases, spareBitsMask, mpePointerSpareBits);
  // For DEBUGGING, disable fallback to local computation to
  // make missing compiler data more obvious:
  // validSpareBitsMask = false;
  if (!validSpareBitsMask) {
    // If we couldn't correctly determine the spare bits mask,
    // return a TI that will always fail when asked for XIs or value.
    return TC.makeTypeInfo<UnsupportedEnumTypeInfo>(
        Size, Alignment, Stride, NumExtraInhabitants, BitwiseTakable,
        EnumKind::MultiPayloadEnum, Cases);
  } else if (spareBitsMask.isZero()) {
    // Simple case that does not use spare bits
    // This is correct as long as our local spare bits calculation
    // above only returns an empty mask when the mask is really empty,
    return TC.makeTypeInfo<TaggedMultiPayloadEnumTypeInfo>(
        Size, Alignment, Stride, NumExtraInhabitants, BitwiseTakable, Cases,
        EffectivePayloadCases);
  } else {
    // General case can mix spare bits and extra discriminator
    // It obviously relies on having an accurate spare bit mask.
    return TC.makeTypeInfo<MultiPayloadEnumTypeInfo>(
        Size, Alignment, Stride, NumExtraInhabitants, BitwiseTakable, Cases,
        spareBitsMask, EffectivePayloadCases);
  }
}

bool ExistentialTypeInfoBuilder::isSingleError() const {
  // If we changed representation, it means we added a
  // superclass constraint or an AnyObject member.
  if (Representation != ExistentialTypeRepresentation::Opaque)
    return false;

  if (Protocols.size() != 1)
    return false;

  if (Superclass)
    return false;

  for (auto *P : Protocols) {
    if (auto *NTD = dyn_cast<NominalTypeRef>(P))
      if (NTD->isErrorProtocol())
        return true;
  }
  return false;
}

void ExistentialTypeInfoBuilder::examineProtocols() {
  if (isSingleError()) {
    Representation = ExistentialTypeRepresentation::Error;
    // No extra witness table for protocol<Error>
    return;
  }

  for (auto *P : Protocols) {
    auto *NTD = dyn_cast<NominalTypeRef>(P);
    auto *OP = dyn_cast<ObjCProtocolTypeRef>(P);
    if (!NTD && !OP) {
      DEBUG_LOG(fprintf(stderr, "Bad protocol: "); P->dump())
      Invalid = true;
      continue;
    }

    // Don't look up field info for imported Objective-C protocols.
    if (OP) {
      ObjC = true;
      continue;
    }

    auto FD = TC.getBuilder().getFieldTypeInfo(P);
    if (FD == nullptr) {
      DEBUG_LOG(fprintf(stderr, "No field descriptor: "); P->dump())
      Invalid = true;
      continue;
    }

    switch (FD->Kind) {
    case FieldDescriptorKind::ObjCProtocol:
      // Objective-C protocols do not have any witness tables.
      ObjC = true;
      continue;
    case FieldDescriptorKind::ClassProtocol:
      Representation = ExistentialTypeRepresentation::Class;
      ++WitnessTableCount;

      if (auto *Superclass = TC.getBuilder().lookupSuperclass(P)) {
        // ObjC class info should be available in the metadata, so it's safe
        // to not pass an external provider here. This helps preserving the
        // layering.
        auto *SuperclassTI = TC.getTypeInfo(Superclass, nullptr);
        if (SuperclassTI == nullptr) {
          DEBUG_LOG(fprintf(stderr, "No TypeInfo for superclass: ");
                    Superclass->dump());
          Invalid = true;
          continue;
        }

        if (!isa<ReferenceTypeInfo>(SuperclassTI)) {
          DEBUG_LOG(fprintf(stderr, "Superclass not a reference type: ");
                    SuperclassTI->dump());
          Invalid = true;
          continue;
        }

        if (cast<ReferenceTypeInfo>(SuperclassTI)->getReferenceCounting() ==
            ReferenceCounting::Native) {
          Refcounting = ReferenceCounting::Native;
        }
      }

      continue;
    case FieldDescriptorKind::Protocol:
      ++WitnessTableCount;
      continue;
    case FieldDescriptorKind::ObjCClass:
    case FieldDescriptorKind::Struct:
    case FieldDescriptorKind::Enum:
    case FieldDescriptorKind::MultiPayloadEnum:
    case FieldDescriptorKind::Class:
      Invalid = true;
      continue;
    }
  }
}

void ExistentialTypeInfoBuilder::addProtocol(const TypeRef *P) {
  Protocols.push_back(P);
}

void ExistentialTypeInfoBuilder::addProtocolComposition(
    const ProtocolCompositionTypeRef *PC) {
  for (auto *T : PC->getProtocols()) {
    addProtocol(T);
  }

  if (PC->hasExplicitAnyObject())
    addAnyObject();

  if (auto *T = PC->getSuperclass()) {
    // Anything else should either be a superclass constraint, or
    // we have an invalid typeref.
    if (!isa<NominalTypeRef>(T) && !isa<BoundGenericTypeRef>(T) &&
        !isa<ObjCClassTypeRef>(T)) {
      DEBUG_LOG(fprintf(stderr, "Bad existential member: "); T->dump())
      Invalid = true;
      return;
    }

    // Don't look up field info for imported Objective-C classes.
    if (isa<ObjCClassTypeRef>(T)) {
      addAnyObject();
      return;
    }

    const auto &FD = TC.getBuilder().getFieldTypeInfo(T);
    if (FD == nullptr) {
      DEBUG_LOG(fprintf(stderr, "No field descriptor: "); T->dump())
      Invalid = true;
      return;
    }

    // We have a valid superclass constraint. It only affects
    // lowering by class-constraining the entire existential.
    switch (FD->Kind) {
    case FieldDescriptorKind::Class:
      Refcounting = ReferenceCounting::Native;
      SWIFT_FALLTHROUGH;

    case FieldDescriptorKind::ObjCClass:
      addAnyObject();
      break;

    default:
      DEBUG_LOG(fprintf(stderr, "Bad existential member: "); T->dump())
      Invalid = true;
      return;
    }
  }
}

void ExistentialTypeInfoBuilder::addAnyObject() {
  Representation = ExistentialTypeRepresentation::Class;
}

void ExistentialTypeInfoBuilder::markInvalid() { Invalid = true; }

const TypeInfo *
ExistentialTypeInfoBuilder::build(remote::TypeInfoProvider *ExternalTypeInfo) {
  examineProtocols();

  if (Invalid)
    return nullptr;

  if (ObjC) {
    if (WitnessTableCount > 0) {
      DEBUG_LOG(fprintf(stderr, "@objc existential with witness tables\n"));
      return nullptr;
    }

    return TC.getReferenceTypeInfo(ReferenceKind::Strong, Refcounting);
  }

  RecordKind Kind;
  switch (Representation) {
  case ExistentialTypeRepresentation::Class:
    Kind = RecordKind::ClassExistential;
    break;
  case ExistentialTypeRepresentation::Opaque:
    Kind = RecordKind::OpaqueExistential;
    break;
  case ExistentialTypeRepresentation::Error:
    Kind = RecordKind::ErrorExistential;
    break;
  }

  RecordTypeInfoBuilder builder(TC, Kind);

  switch (Representation) {
  case ExistentialTypeRepresentation::Class:
    // Class existentials consist of a single retainable pointer
    // followed by witness tables.
    if (Refcounting == ReferenceCounting::Unknown)
      builder.addField("object", TC.getUnknownObjectTypeRef(),
                       ExternalTypeInfo);
    else
      builder.addField("object", TC.getNativeObjectTypeRef(), ExternalTypeInfo);
    break;
  case ExistentialTypeRepresentation::Opaque: {
    auto *TI = TC.getTypeInfo(TC.getRawPointerTypeRef(), ExternalTypeInfo);
    if (TI == nullptr) {
      DEBUG_LOG(fprintf(stderr, "No TypeInfo for RawPointer\n"));
      return nullptr;
    }

    // Non-class existentials consist of a three-word buffer,
    // value metadata, and finally zero or more witness tables.
    // The buffer is always bitwise takable, since non-bitwise
    // takable payloads are stored out of line.
    builder.addField(TI->getSize() * 3, TI->getAlignment(),
                     /*numExtraInhabitants=*/0,
                     /*bitwiseTakable=*/true);
    builder.addField("metadata", TC.getAnyMetatypeTypeRef(), ExternalTypeInfo);
    break;
  }
  case ExistentialTypeRepresentation::Error:
    builder.addField("error", TC.getUnknownObjectTypeRef(), ExternalTypeInfo);
    break;
  }

  for (unsigned i = 0; i < WitnessTableCount; ++i)
    builder.addField("wtable", TC.getRawPointerTypeRef(), ExternalTypeInfo);

  return builder.build();
}

const TypeInfo *ExistentialTypeInfoBuilder::buildMetatype(
    remote::TypeInfoProvider *ExternalTypeInfo) {
  examineProtocols();

  if (Invalid)
    return nullptr;

  if (ObjC) {
    if (WitnessTableCount > 0) {
      DEBUG_LOG(fprintf(stderr, "@objc existential with witness tables\n"));
      return nullptr;
    }

    return TC.getAnyMetatypeTypeInfo();
  }

  RecordTypeInfoBuilder builder(TC, RecordKind::ExistentialMetatype);

  builder.addField("metadata", TC.getAnyMetatypeTypeRef(), ExternalTypeInfo);
  for (unsigned i = 0; i < WitnessTableCount; ++i)
    builder.addField("wtable", TC.getRawPointerTypeRef(), ExternalTypeInfo);

  return builder.build();
}
#endif
