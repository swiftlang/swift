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

RemoteRef<char> TypeRefBuilder::readTypeRef(uint64_t remoteAddr) {
  // The remote address should point into one of the TypeRef or
  // ReflectionString references we already read out of the images.
  RemoteRef<char> foundTypeRef;
  RemoteRef<void> limitAddress;
  for (auto &info : ReflectionInfos) {
    if (info.TypeReference.containsRemoteAddress(remoteAddr, 1)) {
      foundTypeRef = info.TypeReference.getRemoteRef<char>(remoteAddr);
      limitAddress = info.TypeReference.endAddress();
      goto found_type_ref;
    }
    if (info.ReflectionString.containsRemoteAddress(remoteAddr, 1)) {
      foundTypeRef = info.ReflectionString.getRemoteRef<char>(remoteAddr);
      limitAddress = info.ReflectionString.endAddress();
      goto found_type_ref;
    }
  }
  // TODO: Try using MetadataReader to read the string here?
  
  // Invalid type ref pointer.
  return nullptr;

found_type_ref:
  // Make sure there's a valid mangled string within the bounds of the
  // section.
  for (auto i = foundTypeRef;
       i.getAddressData() < limitAddress.getAddressData(); ) {
    auto c = *i.getLocalBuffer();
    if (c == '\0')
      goto valid_type_ref;
      
    if (c >= '\1' && c <= '\x17')
      i = i.atByteOffset(5);
    else if (c >= '\x18' && c <= '\x1F') {
      i = i.atByteOffset(PointerSize + 1);
    } else {
      i = i.atByteOffset(1);
    }
  }
  
  // Unterminated string.
  return nullptr;
  
valid_type_ref:
  // Look past the $s prefix if the string has one.
  auto localStr = foundTypeRef.getLocalBuffer();
  if (localStr[0] == '$' && localStr[1] == 's') {
    foundTypeRef = foundTypeRef.atByteOffset(2);
  }
  
  return foundTypeRef;
}

/// Load and normalize a mangled name so it can be matched with string equality.
std::string
TypeRefBuilder::normalizeReflectionName(RemoteRef<char> reflectionName) {
  // Remangle the reflection name to resolve symbolic references.
  if (auto node = demangleTypeRef(reflectionName)) {
    return mangleNode(node);
  }

  // Fall back to the raw string.
  return getTypeRefString(reflectionName);
}

/// Determine whether the given reflection protocol name matches.
bool
TypeRefBuilder::reflectionNameMatches(RemoteRef<char> reflectionName,
                                      StringRef searchName) {
  auto normalized = normalizeReflectionName(reflectionName);
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
    for (auto AssocTyDescriptor : Info.AssociatedType) {
      if (!reflectionNameMatches(
          readTypeRef(AssocTyDescriptor, AssocTyDescriptor->ConformingTypeName),
          MangledTypeName))
        continue;

      if (!reflectionNameMatches(
            readTypeRef(AssocTyDescriptor, AssocTyDescriptor->ProtocolTypeName),
            Protocol))
        continue;

      for (auto &AssocTyRef : *AssocTyDescriptor.getLocalBuffer()) {
        auto AssocTy = AssocTyDescriptor.getField(AssocTyRef);
        if (Member.compare(
                    getTypeRefString(readTypeRef(AssocTy, AssocTy->Name))) != 0)
          continue;

        auto SubstitutedTypeName = readTypeRef(AssocTy,
                                               AssocTy->SubstitutedTypeName);
        auto Demangled = demangleTypeRef(SubstitutedTypeName);
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
  if (FD == nullptr)
    return nullptr;

  if (!FD->hasSuperclass())
    return nullptr;

  auto Demangled = demangleTypeRef(readTypeRef(FD, FD->Superclass));
  auto Unsubstituted = swift::Demangle::decodeMangledType(*this, Demangled);
  if (!Unsubstituted)
    return nullptr;

  auto SubstMap = TR->getSubstMap();
  if (!SubstMap)
    return nullptr;
  return Unsubstituted->subst(*this, *SubstMap);
}

RemoteRef<FieldDescriptor>
TypeRefBuilder::getFieldTypeInfo(const TypeRef *TR) {
  std::string MangledName;
  if (auto N = dyn_cast<NominalTypeRef>(TR))
    MangledName = N->getMangledName();
  else if (auto BG = dyn_cast<BoundGenericTypeRef>(TR))
    MangledName = BG->getMangledName();
  else
    return nullptr;

  // Try the cache.
  auto Found = FieldTypeInfoCache.find(MangledName);
  if (Found != FieldTypeInfoCache.end())
    return Found->second;

  // On failure, fill out the cache with everything we know about.
  std::vector<std::pair<std::string, const TypeRef *>> Fields;
  for (auto &Info : ReflectionInfos) {
    for (auto FD : Info.Field) {
      if (!FD->hasMangledTypeName())
        continue;
      auto CandidateMangledName = readTypeRef(FD, FD->MangledTypeName);
      auto NormalizedName = normalizeReflectionName(CandidateMangledName);
      FieldTypeInfoCache[NormalizedName] = FD;
      Dem.clear();
    }
  }

  // We've filled the cache with everything we know about now. Try the cache again.
  Found = FieldTypeInfoCache.find(MangledName);
  if (Found != FieldTypeInfoCache.end())
    return Found->second;

  return nullptr;
}

bool TypeRefBuilder::getFieldTypeRefs(
    const TypeRef *TR,
    RemoteRef<FieldDescriptor> FD,
    std::vector<FieldTypeInfo> &Fields) {
  if (FD == nullptr)
    return false;

  auto Subs = TR->getSubstMap();
  if (!Subs)
    return false;

  for (auto &FieldRef : *FD.getLocalBuffer()) {
    auto Field = FD.getField(FieldRef);
    
    auto FieldName = getTypeRefString(readTypeRef(Field, Field->FieldName));

    // Empty cases of enums do not have a type
    if (FD->isEnum() && !Field->hasMangledTypeName()) {
      Fields.push_back(FieldTypeInfo::forEmptyCase(FieldName));
      continue;
    }

    auto Demangled = demangleTypeRef(readTypeRef(Field,Field->MangledTypeName));
    auto Unsubstituted = swift::Demangle::decodeMangledType(*this, Demangled);
    if (!Unsubstituted)
      return false;

    auto Substituted = Unsubstituted->subst(*this, *Subs);

    if (FD->isEnum() && Field->isIndirectCase()) {
      Fields.push_back(FieldTypeInfo::forIndirectCase(FieldName, Substituted));
      continue;
    }

    Fields.push_back(FieldTypeInfo::forField(FieldName, Substituted));
  }
  return true;
}

RemoteRef<BuiltinTypeDescriptor>
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
    for (auto BuiltinTypeDescriptor : Info.Builtin) {
      assert(BuiltinTypeDescriptor->Size > 0);
      assert(BuiltinTypeDescriptor->getAlignment() > 0);
      assert(BuiltinTypeDescriptor->Stride > 0);
      if (!BuiltinTypeDescriptor->hasMangledTypeName())
        continue;
      auto CandidateMangledName =
        readTypeRef(BuiltinTypeDescriptor, BuiltinTypeDescriptor->TypeName);
      if (!reflectionNameMatches(CandidateMangledName, MangledName))
        continue;
      return BuiltinTypeDescriptor;
    }
  }

  return nullptr;
}

RemoteRef<CaptureDescriptor>
TypeRefBuilder::getCaptureDescriptor(uint64_t RemoteAddress) {
  for (auto Info : ReflectionInfos) {
    for (auto CD : Info.Capture) {
      if (RemoteAddress == CD.getAddressData()) {
        return CD;
      }
    }
  }

  return nullptr;
}

/// Get the unsubstituted capture types for a closure context.
ClosureContextInfo
TypeRefBuilder::getClosureContextInfo(RemoteRef<CaptureDescriptor> CD) {
  ClosureContextInfo Info;

  for (auto i = CD->capture_begin(), e = CD->capture_end(); i != e; ++i) {
    const TypeRef *TR = nullptr;
    auto CR = CD.getField(*i);
    
    if (CR->hasMangledTypeName()) {
      auto MangledName = readTypeRef(CR, CR->MangledTypeName);
      auto DemangleTree = demangleTypeRef(MangledName);
      TR = swift::Demangle::decodeMangledType(*this, DemangleTree);
    }
    Info.CaptureTypes.push_back(TR);
  }

  for (auto i = CD->source_begin(), e = CD->source_end(); i != e; ++i) {
    const TypeRef *TR = nullptr;
    auto MSR = CD.getField(*i);
    
    if (MSR->hasMangledTypeName()) {
      auto MangledName = readTypeRef(MSR, MSR->MangledTypeName);
      auto DemangleTree = demangleTypeRef(MangledName);
      TR = swift::Demangle::decodeMangledType(*this, DemangleTree);
    }

    const MetadataSource *MS = nullptr;
    if (MSR->hasMangledMetadataSource()) {
      auto MangledMetadataSource =
        getTypeRefString(readTypeRef(MSR, MSR->MangledMetadataSource));
      MS = MetadataSource::decode(MSB, MangledMetadataSource);
    }

    Info.MetadataSources.push_back({TR, MS});
  }

  Info.NumBindings = CD->NumBindings;

  return Info;
}

///
/// Dumping reflection metadata
///

void
TypeRefBuilder::dumpTypeRef(RemoteRef<char> MangledName,
                            FILE *file, bool printTypeName) {
  auto DemangleTree = demangleTypeRef(MangledName);
  auto TypeName = nodeToString(DemangleTree);
  fprintf(file, "%s\n", TypeName.c_str());
  auto TR = swift::Demangle::decodeMangledType(*this, DemangleTree);
  if (!TR) {
    auto str = getTypeRefString(MangledName);
    fprintf(file, "!!! Invalid typeref: %s\n", std::string(str.begin(), str.end()).c_str());
    return;
  }
  TR->dump(file);
  fprintf(file, "\n");
}

void TypeRefBuilder::dumpFieldSection(FILE *file) {
  for (const auto &sections : ReflectionInfos) {
    for (auto descriptor : sections.Field) {
      auto TypeDemangling =
        demangleTypeRef(readTypeRef(descriptor, descriptor->MangledTypeName));
      auto TypeName = nodeToString(TypeDemangling);
      fprintf(file, "%s\n", TypeName.c_str());
      for (size_t i = 0; i < TypeName.size(); ++i)
        fprintf(file, "-");
      fprintf(file, "\n");
      for (auto &fieldRef : *descriptor.getLocalBuffer()) {
        auto field = descriptor.getField(fieldRef);
        auto fieldName = getTypeRefString(readTypeRef(field, field->FieldName));
        fprintf(file, "%*s", (int)fieldName.size(), fieldName.data());
        if (field->hasMangledTypeName()) {
          fprintf(file, ": ");
          dumpTypeRef(readTypeRef(field, field->MangledTypeName), file);
        } else {
          fprintf(file, "\n\n");
        }
      }
    }
  }
}

void TypeRefBuilder::dumpAssociatedTypeSection(FILE *file) {
  for (const auto &sections : ReflectionInfos) {
    for (auto descriptor : sections.AssociatedType) {
      auto conformingTypeNode = demangleTypeRef(
                       readTypeRef(descriptor, descriptor->ConformingTypeName));
      auto conformingTypeName = nodeToString(conformingTypeNode);
      auto protocolNode = demangleTypeRef(
                         readTypeRef(descriptor, descriptor->ProtocolTypeName));
      auto protocolName = nodeToString(protocolNode);

      fprintf(file, "- %s : %s", conformingTypeName.c_str(), protocolName.c_str());
      fprintf(file, "\n");

      for (const auto &associatedTypeRef : *descriptor.getLocalBuffer()) {
        auto associatedType = descriptor.getField(associatedTypeRef);
        
        std::string name = getTypeRefString(
                            readTypeRef(associatedType, associatedType->Name));
        fprintf(file, "typealias %s = ", name.c_str());
        dumpTypeRef(
          readTypeRef(associatedType, associatedType->SubstitutedTypeName), file);
      }
    }
  }
}

void TypeRefBuilder::dumpBuiltinTypeSection(FILE *file) {
  for (const auto &sections : ReflectionInfos) {
    for (auto descriptor : sections.Builtin) {
      auto typeNode = demangleTypeRef(readTypeRef(descriptor,
                                                  descriptor->TypeName));
      auto typeName = nodeToString(typeNode);
      
      fprintf(file, "\n- %s:\n", typeName.c_str());
      fprintf(file, "Size: %u\n", descriptor->Size);
      fprintf(file, "Alignment: %u:\n", descriptor->getAlignment());
      fprintf(file, "Stride: %u:\n", descriptor->Stride);
      fprintf(file, "NumExtraInhabitants: %u:\n", descriptor->NumExtraInhabitants);
      fprintf(file, "BitwiseTakable: %d:\n", descriptor->isBitwiseTakable());
    }
  }
}

void ClosureContextInfo::dump() const {
  dump(stderr);
}

void ClosureContextInfo::dump(FILE *file) const {
  fprintf(file, "- Capture types:\n");
  for (auto *TR : CaptureTypes) {
    if (TR == nullptr)
      fprintf(file, "!!! Invalid typeref\n");
    else
      TR->dump(file);
  }
  fprintf(file, "- Metadata sources:\n");
  for (auto MS : MetadataSources) {
    if (MS.first == nullptr)
      fprintf(file, "!!! Invalid typeref\n");
    else
      MS.first->dump(file);
    if (MS.second == nullptr)
      fprintf(file, "!!! Invalid metadata source\n");
    else
      MS.second->dump(file);
  }
  fprintf(file, "\n");
}

void TypeRefBuilder::dumpCaptureSection(FILE *file) {
  for (const auto &sections : ReflectionInfos) {
    for (const auto &descriptor : sections.Capture) {
      auto info = getClosureContextInfo(descriptor);
      info.dump(file);
    }
  }
}

void TypeRefBuilder::dumpAllSections(FILE *file) {
  fprintf(file, "FIELDS:\n");
  fprintf(file, "=======\n");
  dumpFieldSection(file);
  fprintf(file, "\n");
  fprintf(file, "ASSOCIATED TYPES:\n");
  fprintf(file, "=================\n");
  dumpAssociatedTypeSection(file);
  fprintf(file, "\n");
  fprintf(file, "BUILTIN TYPES:\n");
  fprintf(file, "==============\n");
  dumpBuiltinTypeSection(file);
  fprintf(file, "\n");
  fprintf(file, "CAPTURE DESCRIPTORS:\n");
  fprintf(file, "====================\n");
  dumpCaptureSection(file);
  fprintf(file, "\n");
}
