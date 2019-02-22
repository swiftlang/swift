//===--- TypeRefBuilder.cpp - Swift Type Reference Builder ----------------===//
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
// Implements utilities for constructing TypeRefs and looking up field and
// enum case types.
//
//===----------------------------------------------------------------------===//

#include "swift/Reflection/TypeRefBuilder.h"

#include "swift/Demangling/Demangle.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Remote/MetadataReader.h"

using namespace swift;
using namespace reflection;

uint64_t
TypeRefBuilder::getRemoteAddrOfTypeRefPointer(const void *pointer) {
  // Find what type ref section the pointer resides in, if any.
  const ReflectionInfo *containingInfo = nullptr;
  for (auto &info : ReflectionInfos) {
    auto start = (uintptr_t)info.TypeReference.Metadata.startAddress();
    auto size = (uintptr_t)info.TypeReference.Metadata.size();
    if (start <= (uintptr_t)pointer && (uintptr_t)pointer < start + size) {
       containingInfo = &info;
       break;
    }
  }
  
  if (!containingInfo)
    return 0;
  
  return (uintptr_t)pointer
    + containingInfo->RemoteStartAddress
    - containingInfo->LocalStartAddress
    + containingInfo->TypeReference.SectionOffset;
}

TypeRefBuilder::TypeRefBuilder() : TC(*this) {}

/// Normalize a mangled name so it can be matched with string equality.
static std::string normalizeReflectionName(Demangler &dem, StringRef reflectionName) {
  reflectionName = dropSwiftManglingPrefix(reflectionName);
  
  // Remangle the reflection name to resolve symbolic references.
  if (auto node = dem.demangleType(reflectionName)) {
    return mangleNode(node);
  }

  // Fall back to the raw string.
  return reflectionName;
}

/// Determine whether the given reflection protocol name matches.
static bool reflectionNameMatches(Demangler &dem,
                                  StringRef reflectionName,
                                  StringRef searchName) {
  auto normalized = normalizeReflectionName(dem, reflectionName);
  return searchName.equals(normalized);
}

const TypeRef * TypeRefBuilder::
lookupTypeWitness(const std::string &MangledTypeName,
                  const std::string &Member,
                  const StringRef Protocol) {
  TypeRefID key;
  key.addString(MangledTypeName);
  key.addString(Member);
  key.addString(Protocol);
  auto found = AssociatedTypeCache.find(key);
  if (found != AssociatedTypeCache.end())
    return found->second;

  // Cache missed - we need to look through all of the assocty sections
  // for all images that we've been notified about.
  for (auto &Info : ReflectionInfos) {
    uintptr_t TypeRefOffset = Info.AssociatedType.SectionOffset
                            - Info.TypeReference.SectionOffset;
    uintptr_t NameOffset = Info.AssociatedType.SectionOffset
                         - Info.ReflectionString.SectionOffset;
    for (const auto &AssocTyDescriptor : Info.AssociatedType.Metadata) {
      if (!reflectionNameMatches(Dem,
                 AssocTyDescriptor.getMangledConformingTypeName(TypeRefOffset),
                 MangledTypeName))
        continue;

      if (!reflectionNameMatches(Dem,
                 AssocTyDescriptor.getMangledProtocolTypeName(TypeRefOffset),
                 Protocol))
        continue;

      for (auto &AssocTy : AssocTyDescriptor) {
        if (Member.compare(AssocTy.getName(NameOffset)) != 0)
          continue;

        auto SubstitutedTypeName =
            AssocTy.getMangledSubstitutedTypeName(TypeRefOffset);
        auto Demangled = Dem.demangleType(SubstitutedTypeName);
        auto *TypeWitness = swift::Demangle::decodeMangledType(*this, Demangled);

        AssociatedTypeCache.insert(std::make_pair(key, TypeWitness));
        return TypeWitness;
      }
    }
  }
  return nullptr;
}

const TypeRef * TypeRefBuilder::
lookupSuperclass(const TypeRef *TR) {
  const auto &FD = getFieldTypeInfo(TR);
  if (FD.first == nullptr)
    return nullptr;

  if (!FD.first->hasSuperclass())
    return nullptr;

  auto TypeRefOffset = FD.second->Field.SectionOffset
                     - FD.second->TypeReference.SectionOffset;
  auto Demangled = Dem.demangleType(FD.first->getSuperclass(TypeRefOffset));
  auto Unsubstituted = swift::Demangle::decodeMangledType(*this, Demangled);
  if (!Unsubstituted)
    return nullptr;

  auto SubstMap = TR->getSubstMap();
  if (!SubstMap)
    return nullptr;
  return Unsubstituted->subst(*this, *SubstMap);
}

std::pair<const FieldDescriptor *, const ReflectionInfo *>
TypeRefBuilder::getFieldTypeInfo(const TypeRef *TR) {
  std::string MangledName;
  if (auto N = dyn_cast<NominalTypeRef>(TR))
    MangledName = N->getMangledName();
  else if (auto BG = dyn_cast<BoundGenericTypeRef>(TR))
    MangledName = BG->getMangledName();
  else
    return {};

  // Try the cache.
  auto Found = FieldTypeInfoCache.find(MangledName);
  if (Found != FieldTypeInfoCache.end())
    return Found->second;

  // On failure, fill out the cache with everything we know about.
  std::vector<std::pair<std::string, const TypeRef *>> Fields;
  for (auto &Info : ReflectionInfos) {
    uintptr_t TypeRefOffset = Info.Field.SectionOffset
                            - Info.TypeReference.SectionOffset;
    for (auto &FD : Info.Field.Metadata) {
      if (!FD.hasMangledTypeName())
        continue;
      auto CandidateMangledName = FD.getMangledTypeName(TypeRefOffset);
      auto NormalizedName = normalizeReflectionName(Dem, CandidateMangledName);
      FieldTypeInfoCache[NormalizedName] = {&FD, &Info};
      Dem.clear();
    }
  }

  // We've filled the cache with everything we know about now. Try the cache again.
  Found = FieldTypeInfoCache.find(MangledName);
  if (Found != FieldTypeInfoCache.end())
    return Found->second;

  return {nullptr, 0};
}

bool TypeRefBuilder::getFieldTypeRefs(
    const TypeRef *TR,
    const std::pair<const FieldDescriptor *, const ReflectionInfo *> &FD,
    std::vector<FieldTypeInfo> &Fields) {
  if (FD.first == nullptr)
    return false;

  auto Subs = TR->getSubstMap();
  if (!Subs)
    return false;

  for (auto &Field : *FD.first) {
    auto TypeRefOffset = FD.second->Field.SectionOffset
                       - FD.second->TypeReference.SectionOffset;
    auto FieldOffset = FD.second->Field.SectionOffset
                     - FD.second->ReflectionString.SectionOffset;
    auto FieldName = Field.getFieldName(FieldOffset);

    // Empty cases of enums do not have a type
    if (FD.first->isEnum() && !Field.hasMangledTypeName()) {
      Fields.push_back(FieldTypeInfo::forEmptyCase(FieldName));
      continue;
    }

    auto Demangled = Dem.demangleType(Field.getMangledTypeName(TypeRefOffset));
    auto Unsubstituted = swift::Demangle::decodeMangledType(*this, Demangled);
    if (!Unsubstituted)
      return false;

    auto Substituted = Unsubstituted->subst(*this, *Subs);

    if (FD.first->isEnum() && Field.isIndirectCase()) {
      Fields.push_back(FieldTypeInfo::forIndirectCase(FieldName, Substituted));
      continue;
    }

    Fields.push_back(FieldTypeInfo::forField(FieldName, Substituted));
  }
  return true;
}

const BuiltinTypeDescriptor *
TypeRefBuilder::getBuiltinTypeInfo(const TypeRef *TR) {
  std::string MangledName;
  if (auto B = dyn_cast<BuiltinTypeRef>(TR))
    MangledName = B->getMangledName();
  else if (auto N = dyn_cast<NominalTypeRef>(TR))
    MangledName = N->getMangledName();
  else if (auto B = dyn_cast<BoundGenericTypeRef>(TR))
    MangledName = B->getMangledName();
  else
    return nullptr;

  for (auto Info : ReflectionInfos) {
    uintptr_t TypeRefOffset = Info.Builtin.SectionOffset
                            - Info.TypeReference.SectionOffset;
    for (auto &BuiltinTypeDescriptor : Info.Builtin.Metadata) {
      assert(BuiltinTypeDescriptor.Size > 0);
      assert(BuiltinTypeDescriptor.getAlignment() > 0);
      assert(BuiltinTypeDescriptor.Stride > 0);
      if (!BuiltinTypeDescriptor.hasMangledTypeName())
        continue;
      auto CandidateMangledName =
          BuiltinTypeDescriptor.getMangledTypeName(TypeRefOffset);
      if (!reflectionNameMatches(Dem, CandidateMangledName, MangledName))
        continue;
      return &BuiltinTypeDescriptor;
    }
  }

  return nullptr;
}

const CaptureDescriptor *
TypeRefBuilder::getCaptureDescriptor(uintptr_t RemoteAddress) {
  for (auto Info : ReflectionInfos) {
    for (auto &CD : Info.Capture.Metadata) {
      auto OtherAddr = (reinterpret_cast<uintptr_t>(&CD) -
                        Info.LocalStartAddress + Info.RemoteStartAddress);
      if (OtherAddr == RemoteAddress)
        return &CD;
    }
  }

  return nullptr;
}

/// Get the unsubstituted capture types for a closure context.
ClosureContextInfo
TypeRefBuilder::getClosureContextInfo(const CaptureDescriptor &CD,
                                      uintptr_t TypeRefOffset) {
  ClosureContextInfo Info;

  for (auto i = CD.capture_begin(), e = CD.capture_end(); i != e; ++i) {
    const TypeRef *TR = nullptr;
    if (i->hasMangledTypeName()) {
      auto MangledName = i->getMangledTypeName(TypeRefOffset);
      auto DemangleTree = Dem.demangleType(MangledName);
      TR = swift::Demangle::decodeMangledType(*this, DemangleTree);
    }
    Info.CaptureTypes.push_back(TR);
  }

  for (auto i = CD.source_begin(), e = CD.source_end(); i != e; ++i) {
    const TypeRef *TR = nullptr;
    if (i->hasMangledTypeName()) {
      auto MangledName = i->getMangledTypeName(TypeRefOffset);
      auto DemangleTree = Dem.demangleType(MangledName);
      TR = swift::Demangle::decodeMangledType(*this, DemangleTree);
    }

    const MetadataSource *MS = nullptr;
    if (i->hasMangledMetadataSource()) {
      auto MangledMetadataSource = i->getMangledMetadataSource(TypeRefOffset);
      MS = MetadataSource::decode(MSB, MangledMetadataSource);
    }

    Info.MetadataSources.push_back({TR, MS});
  }

  Info.NumBindings = CD.NumBindings;

  return Info;
}

///
/// Dumping reflection metadata
///

void
TypeRefBuilder::dumpTypeRef(StringRef MangledName,
                            std::ostream &OS, bool printTypeName) {
  auto DemangleTree = Dem.demangleType(MangledName);
  auto TypeName = nodeToString(DemangleTree);
  OS << TypeName << '\n';
  auto TR = swift::Demangle::decodeMangledType(*this, DemangleTree);
  if (!TR) {
    OS << "!!! Invalid typeref: "
       << std::string(MangledName.begin(), MangledName.end())
       << '\n';
    return;
  }
  TR->dump(OS);
  OS << '\n';
}

void TypeRefBuilder::dumpFieldSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    uintptr_t TypeRefOffset = sections.Field.SectionOffset
                            - sections.TypeReference.SectionOffset;
    uintptr_t NameOffset = sections.Field.SectionOffset
                           - sections.ReflectionString.SectionOffset;
    for (const auto &descriptor : sections.Field.Metadata) {
      auto TypeDemangling = Dem.demangleType(
         dropSwiftManglingPrefix(descriptor.getMangledTypeName(TypeRefOffset)));
      auto TypeName = nodeToString(TypeDemangling);
      OS << TypeName << '\n';
      for (size_t i = 0; i < TypeName.size(); ++i)
        OS << '-';
      OS << '\n';
      for (auto &field : descriptor) {
        OS << std::string(field.getFieldName(NameOffset).begin(),
                          field.getFieldName(NameOffset).end());
        if (field.hasMangledTypeName()) {
          OS << ": ";
          dumpTypeRef(field.getMangledTypeName(TypeRefOffset), OS);
        } else {
          OS << "\n\n";
        }
      }
    }
  }
}

void TypeRefBuilder::dumpAssociatedTypeSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    uintptr_t TypeRefOffset = sections.AssociatedType.SectionOffset
                            - sections.TypeReference.SectionOffset;
    uintptr_t NameOffset = sections.AssociatedType.SectionOffset
                           - sections.ReflectionString.SectionOffset;
    for (const auto &descriptor : sections.AssociatedType.Metadata) {
      auto conformingTypeNode = Dem.demangleType(
          descriptor.getMangledConformingTypeName(TypeRefOffset));
      auto conformingTypeName = nodeToString(conformingTypeNode);
      auto protocolNode = Dem.demangleType(dropSwiftManglingPrefix(
                         descriptor.getMangledProtocolTypeName(TypeRefOffset)));
      auto protocolName = nodeToString(protocolNode);

      OS << "- " << conformingTypeName << " : " << protocolName;
      OS << '\n';

      for (const auto &associatedType : descriptor) {
        std::string name = associatedType.getName(NameOffset);
        OS << "typealias " << name << " = ";
        dumpTypeRef(
               associatedType.getMangledSubstitutedTypeName(TypeRefOffset), OS);
      }
    }
  }
}

void TypeRefBuilder::dumpBuiltinTypeSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    uintptr_t TypeRefOffset = sections.Builtin.SectionOffset
                            - sections.TypeReference.SectionOffset;
    for (const auto &descriptor : sections.Builtin.Metadata) {
      auto typeName =
          Demangle::demangleTypeAsString(
                                  descriptor.getMangledTypeName(TypeRefOffset));

      OS << "\n- " << typeName << ":\n";
      OS << "Size: " << descriptor.Size << "\n";
      OS << "Alignment: " << descriptor.getAlignment() << "\n";
      OS << "Stride: " << descriptor.Stride << "\n";
      OS << "NumExtraInhabitants: " << descriptor.NumExtraInhabitants << "\n";
      OS << "BitwiseTakable: " << descriptor.isBitwiseTakable() << "\n";
    }
  }
}

void ClosureContextInfo::dump() const {
  dump(std::cerr);
}

void ClosureContextInfo::dump(std::ostream &OS) const {
  OS << "- Capture types:\n";
  for (auto *TR : CaptureTypes) {
    if (TR == nullptr)
      OS << "!!! Invalid typeref\n";
    else
      TR->dump(OS);
  }
  OS << "- Metadata sources:\n";
  for (auto MS : MetadataSources) {
    if (MS.first == nullptr)
      OS << "!!! Invalid typeref\n";
    else
      MS.first->dump(OS);
    if (MS.second == nullptr)
      OS << "!!! Invalid metadata source\n";
    else
      MS.second->dump(OS);
  }
  OS << "\n";
}

void TypeRefBuilder::dumpCaptureSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    uintptr_t TypeRefOffset = sections.Capture.SectionOffset
                            - sections.TypeReference.SectionOffset;
    for (const auto &descriptor : sections.Capture.Metadata) {
      auto info = getClosureContextInfo(descriptor, TypeRefOffset);
      info.dump(OS);
    }
  }
}

void TypeRefBuilder::dumpAllSections(std::ostream &OS) {
  OS << "FIELDS:\n";
  OS << "=======\n";
  dumpFieldSection(OS);
  OS << '\n';
  OS << "ASSOCIATED TYPES:\n";
  OS << "=================\n";
  dumpAssociatedTypeSection(OS);
  OS << '\n';
  OS << "BUILTIN TYPES:\n";
  OS << "==============\n";
  dumpBuiltinTypeSection(OS);
  OS << '\n';
  OS << "CAPTURE DESCRIPTORS:\n";
  OS << "====================\n";
  dumpCaptureSection(OS);
  OS << '\n';
}
