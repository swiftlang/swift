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
  fputs("invalid type ref pointer\n", stderr);
  abort();

found_type_ref:
  // Make sure there's a valid mangled string within the bounds of the
  // section.
  for (auto i = foundTypeRef;
       i.getAddressData() < limitAddress.getAddressData(); ) {
    auto c = *i.getLocalBuffer();
    if (c == '\0')
      goto valid_type_ref;
      
    if (c >= '\1' && c <= '\x17')
      i = i.atByteOffset(4);
    else if (c >= '\x18' && c <= '\x1F') {
      i = i.atByteOffset(PointerSize);
    } else {
      i = i.atByteOffset(1);
    }
  }
  
  fputs("unterminated type ref\n", stderr);
  abort();
  
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
                            std::ostream &OS, bool printTypeName) {
  auto DemangleTree = demangleTypeRef(MangledName);
  auto TypeName = nodeToString(DemangleTree);
  OS << TypeName << '\n';
  auto TR = swift::Demangle::decodeMangledType(*this, DemangleTree);
  if (!TR) {
    auto str = getTypeRefString(MangledName);
    OS << "!!! Invalid typeref: "
       << std::string(str.begin(), str.end())
       << '\n';
    return;
  }
  TR->dump(OS);
  OS << '\n';
}

void TypeRefBuilder::dumpFieldSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    for (auto descriptor : sections.Field) {
      auto TypeDemangling =
        demangleTypeRef(readTypeRef(descriptor, descriptor->MangledTypeName));
      auto TypeName = nodeToString(TypeDemangling);
      OS << TypeName << '\n';
      for (size_t i = 0; i < TypeName.size(); ++i)
        OS << '-';
      OS << '\n';
      for (auto &fieldRef : *descriptor.getLocalBuffer()) {
        auto field = descriptor.getField(fieldRef);
        auto fieldName = getTypeRefString(readTypeRef(field, field->FieldName));
        OS << std::string(fieldName.begin(), fieldName.end());
        if (field->hasMangledTypeName()) {
          OS << ": ";
          dumpTypeRef(readTypeRef(field, field->MangledTypeName), OS);
        } else {
          OS << "\n\n";
        }
      }
    }
  }
}

void TypeRefBuilder::dumpAssociatedTypeSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    for (auto descriptor : sections.AssociatedType) {
      auto conformingTypeNode = demangleTypeRef(
                       readTypeRef(descriptor, descriptor->ConformingTypeName));
      auto conformingTypeName = nodeToString(conformingTypeNode);
      auto protocolNode = demangleTypeRef(
                         readTypeRef(descriptor, descriptor->ProtocolTypeName));
      auto protocolName = nodeToString(protocolNode);

      OS << "- " << conformingTypeName << " : " << protocolName;
      OS << '\n';

      for (const auto &associatedTypeRef : *descriptor.getLocalBuffer()) {
        auto associatedType = descriptor.getField(associatedTypeRef);
        
        std::string name = getTypeRefString(
                            readTypeRef(associatedType, associatedType->Name));
        OS << "typealias " << name << " = ";
        dumpTypeRef(
          readTypeRef(associatedType, associatedType->SubstitutedTypeName), OS);
      }
    }
  }
}

void TypeRefBuilder::dumpBuiltinTypeSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    for (auto descriptor : sections.Builtin) {
      auto typeNode = demangleTypeRef(readTypeRef(descriptor,
                                                  descriptor->TypeName));
      auto typeName = nodeToString(typeNode);
      
      OS << "\n- " << typeName << ":\n";
      OS << "Size: " << descriptor->Size << "\n";
      OS << "Alignment: " << descriptor->getAlignment() << "\n";
      OS << "Stride: " << descriptor->Stride << "\n";
      OS << "NumExtraInhabitants: " << descriptor->NumExtraInhabitants << "\n";
      OS << "BitwiseTakable: " << descriptor->isBitwiseTakable() << "\n";
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
    for (const auto &descriptor : sections.Capture) {
      auto info = getClosureContextInfo(descriptor);
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
