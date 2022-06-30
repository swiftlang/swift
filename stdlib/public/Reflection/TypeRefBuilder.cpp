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

#if SWIFT_ENABLE_REFLECTION

#include "swift/Reflection/TypeRefBuilder.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Remote/MetadataReader.h"
#include <iomanip>
#include <iostream>
#include <sstream>

using namespace swift;
using namespace reflection;
using ReadBytesResult = swift::remote::MemoryReader::ReadBytesResult;

TypeRefBuilder::BuiltType
TypeRefBuilder::decodeMangledType(Node *node, bool forRequirement) {
  return swift::Demangle::decodeMangledType(*this, node, forRequirement)
      .getType();
}

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
llvm::Optional<std::string>
TypeRefBuilder::normalizeReflectionName(RemoteRef<char> reflectionName) {
  // Remangle the reflection name to resolve symbolic references.
  if (auto node = demangleTypeRef(reflectionName,
                                  /*useOpaqueTypeSymbolicReferences*/ false)) {
    switch (node->getKind()) {
    case Node::Kind::TypeSymbolicReference:
    case Node::Kind::ProtocolSymbolicReference:
    case Node::Kind::OpaqueTypeDescriptorSymbolicReference:
      // Symbolic references cannot be mangled, return a failure.
      return {};
    default:
      auto mangling = mangleNode(node);
      clearNodeFactory();
      if (!mangling.isSuccess()) {
        return {};
      }
      return std::move(mangling.result());
    }
  }

  // Fall back to the raw string.
  return getTypeRefString(reflectionName).str();
}

/// Determine whether the given reflection protocol name matches.
bool
TypeRefBuilder::reflectionNameMatches(RemoteRef<char> reflectionName,
                                      StringRef searchName) {
  auto normalized = normalizeReflectionName(reflectionName);
  if (!normalized)
    return false;
  return searchName.equals(*normalized);
}

const TypeRef * TypeRefBuilder::
lookupTypeWitness(const std::string &MangledTypeName,
                  const std::string &Member,
                  const StringRef Protocol) {
  TypeRefID key;
  key.addString(MangledTypeName);
  key.addString(Member);
  key.addString(Protocol.str());
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
                getTypeRefString(readTypeRef(AssocTy, AssocTy->Name)).str()) !=
            0)
          continue;

        auto SubstitutedTypeName = readTypeRef(AssocTy,
                                               AssocTy->SubstitutedTypeName);
        auto Demangled = demangleTypeRef(SubstitutedTypeName);
        auto *TypeWitness = decodeMangledType(Demangled);
        clearNodeFactory();

        AssociatedTypeCache.insert(std::make_pair(key, TypeWitness));
        return TypeWitness;
      }
    }
  }
  return nullptr;
}

const TypeRef *TypeRefBuilder::lookupSuperclass(const TypeRef *TR) {
  const auto &FD = getFieldTypeInfo(TR);
  if (FD == nullptr)
    return nullptr;

  if (!FD->hasSuperclass())
    return nullptr;

  auto Demangled = demangleTypeRef(readTypeRef(FD, FD->Superclass));
  auto Unsubstituted = decodeMangledType(Demangled);
  clearNodeFactory();
  if (!Unsubstituted)
    return nullptr;

  auto SubstMap = TR->getSubstMap();
  if (!SubstMap)
    return nullptr;
  return Unsubstituted->subst(*this, *SubstMap);
}

static llvm::Optional<StringRef> FindOutermostModuleName(NodePointer Node) {
  if (!Node)
    return {};
  // Breadth first search until we find the module name so we find the outermost
  // one.
  llvm::SmallVector<NodePointer, 8> Queue;
  Queue.push_back(Node);
  // Instead of removing items from the front of the queue we just iterate over
  // them.
  for (size_t i = 0; i < Queue.size(); ++i) {
    NodePointer Current = Queue[i];
    if (Current->getKind() == Node::Kind::Module) {
      if (Current->hasText())
        return Current->getText();
      else
        return {};
    }
    for (auto Child : *Current)
      Queue.push_back(Child);
  }
  return {};
}

void TypeRefBuilder::populateFieldTypeInfoCacheWithReflectionAtIndex(
    size_t Index) {
  if (ProcessedReflectionInfoIndexes.contains(Index))
    return;

  const auto &Info = ReflectionInfos[Index];
  for (auto FD : Info.Field) {
    if (!FD->hasMangledTypeName())
      continue;
    auto CandidateMangledName = readTypeRef(FD, FD->MangledTypeName);
    if (auto NormalizedName = normalizeReflectionName(CandidateMangledName)) {
      FieldTypeInfoCache[std::move(*NormalizedName)] = FD;
    }
  }

  ProcessedReflectionInfoIndexes.insert(Index);
}

llvm::Optional<RemoteRef<FieldDescriptor>>
TypeRefBuilder::findFieldDescriptorAtIndex(size_t Index,
                                           const std::string &MangledName) {
  populateFieldTypeInfoCacheWithReflectionAtIndex(Index);
  auto Found = FieldTypeInfoCache.find(MangledName);
  if (Found != FieldTypeInfoCache.end()) {
    return Found->second;
  }
  return llvm::None;
}

RemoteRef<FieldDescriptor> TypeRefBuilder::getFieldTypeInfo(const TypeRef *TR) {
  const std::string *MangledName;
  NodePointer Node;
  Demangler Dem;
  if (auto N = dyn_cast<NominalTypeRef>(TR)) {
    Node = N->getDemangling(Dem);
    MangledName = &N->getMangledName();
  } else if (auto BG = dyn_cast<BoundGenericTypeRef>(TR)) {
    Node = BG->getDemangling(Dem);
    MangledName = &BG->getMangledName();
  } else
    return nullptr;

  // Try the cache.
  auto Found = FieldTypeInfoCache.find(*MangledName);
  if (Found != FieldTypeInfoCache.end())
    return Found->second;

  // Heuristic: find the outermost Module node available, and try to parse the
  // ReflectionInfos with a matching name first.
  auto ModuleName = FindOutermostModuleName(Node);
  // If we couldn't find a module name or the type is imported (__C module) we
  // don't any useful information on which image to look for the type.
  if (ModuleName && ModuleName != llvm::StringRef("__C"))
    for (size_t i = 0; i < ReflectionInfos.size(); ++i)
      if (llvm::is_contained(ReflectionInfos[i].PotentialModuleNames,
                             ModuleName))
        if (auto FD = findFieldDescriptorAtIndex(i, *MangledName))
          return *FD;

  // On failure, fill out the cache, ReflectionInfo by ReflectionInfo,
  // until we find the field descriptor we're looking for.
  for (size_t i = 0; i < ReflectionInfos.size(); ++i)
    if (auto FD = findFieldDescriptorAtIndex(i, *MangledName))
      return *FD;

  return nullptr;
}

bool TypeRefBuilder::getFieldTypeRefs(
    const TypeRef *TR, RemoteRef<FieldDescriptor> FD,
    remote::TypeInfoProvider *ExternalTypeInfo,
    std::vector<FieldTypeInfo> &Fields) {
  if (FD == nullptr)
    return false;

  auto Subs = TR->getSubstMap();
  if (!Subs)
    return false;

  int FieldValue = -1;
  for (auto &FieldRef : *FD.getLocalBuffer()) {
    auto Field = FD.getField(FieldRef);
    
    auto FieldName = getTypeRefString(readTypeRef(Field, Field->FieldName));
    FieldValue += 1;

    // Empty cases of enums do not have a type
    if (FD->isEnum() && !Field->hasMangledTypeName()) {
      Fields.push_back(FieldTypeInfo::forEmptyCase(FieldName.str(), FieldValue));
      continue;
    }

    auto Demangled = demangleTypeRef(readTypeRef(Field,Field->MangledTypeName));
    auto Unsubstituted = decodeMangledType(Demangled);
    clearNodeFactory();
    if (!Unsubstituted)
      return false;

    auto Substituted = Unsubstituted->subst(*this, *Subs);

    if (FD->isEnum() && Field->isIndirectCase()) {
      Fields.push_back(FieldTypeInfo::forIndirectCase(FieldName.str(), FieldValue, Substituted));
      continue;
    }

    Fields.push_back(FieldTypeInfo::forField(FieldName.str(), FieldValue, Substituted));
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
      if (BuiltinTypeDescriptor->Stride <= 0)
        continue;
      if (!BuiltinTypeDescriptor->hasMangledTypeName())
        continue;

      auto Alignment = BuiltinTypeDescriptor->getAlignment();
      if (Alignment <= 0)
        continue;
      // Reject any alignment that's not a power of two.
      if (Alignment & (Alignment - 1))
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

RemoteRef<MultiPayloadEnumDescriptor>
TypeRefBuilder::getMultiPayloadEnumInfo(const TypeRef *TR) {
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
    for (auto MultiPayloadEnumDescriptor : Info.MultiPayloadEnum) {

      // Assert that descriptor size is sane...
      assert(MultiPayloadEnumDescriptor->getContentsSizeInWords() >= 1);
      // We're limited to 64k of spare bits mask...
      assert(MultiPayloadEnumDescriptor->getContentsSizeInWords() < 16384);
      assert(MultiPayloadEnumDescriptor->getSizeInBytes() ==
             4 + MultiPayloadEnumDescriptor->getContentsSizeInWords() * 4);
      // Must have a non-empty spare bits mask iff spare bits are used...
      assert(MultiPayloadEnumDescriptor->usesPayloadSpareBits()
             == (MultiPayloadEnumDescriptor->getPayloadSpareBitMaskByteCount() != 0));
      // BitMask must fit within the advertised size...
      if (MultiPayloadEnumDescriptor->usesPayloadSpareBits()) {
        assert(MultiPayloadEnumDescriptor->getContentsSizeInWords()
               >= 2 + (MultiPayloadEnumDescriptor->getPayloadSpareBitMaskByteCount() + 3) / 4);
      }

      auto CandidateMangledName =
        readTypeRef(MultiPayloadEnumDescriptor, MultiPayloadEnumDescriptor->TypeName);
      if (!reflectionNameMatches(CandidateMangledName, MangledName))
        continue;
      return MultiPayloadEnumDescriptor;
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
      TR = decodeMangledType(DemangleTree);
      clearNodeFactory();
    }
    Info.CaptureTypes.push_back(TR);
  }

  for (auto i = CD->source_begin(), e = CD->source_end(); i != e; ++i) {
    const TypeRef *TR = nullptr;
    auto MSR = CD.getField(*i);
    
    if (MSR->hasMangledTypeName()) {
      auto MangledName = readTypeRef(MSR, MSR->MangledTypeName);
      auto DemangleTree = demangleTypeRef(MangledName);
      TR = decodeMangledType(DemangleTree);
      clearNodeFactory();
    }

    const MetadataSource *MS = nullptr;
    if (MSR->hasMangledMetadataSource()) {
      auto MangledMetadataSource =
        getTypeRefString(readTypeRef(MSR, MSR->MangledMetadataSource));
      MS = MetadataSource::decode(MSB, MangledMetadataSource.str());
    }

    Info.MetadataSources.push_back({TR, MS});
  }

  Info.NumBindings = CD->NumBindings;

  return Info;
}

///
/// Dumping reflection metadata
///

void TypeRefBuilder::dumpTypeRef(RemoteRef<char> MangledName,
                                 std::ostream &stream, bool printTypeName) {
  auto DemangleTree = demangleTypeRef(MangledName);
  auto TypeName = nodeToString(DemangleTree);
  stream << TypeName << "\n";
  auto Result = swift::Demangle::decodeMangledType(*this, DemangleTree);
  clearNodeFactory();
  if (Result.isError()) {
    auto *Error = Result.getError();
    char *ErrorStr = Error->copyErrorString();
    auto str = getTypeRefString(MangledName);
    stream << "!!! Invalid typeref: " << str.str() << " - " << ErrorStr << "\n";
    Error->freeErrorString(ErrorStr);
    return;
  }
  auto TR = Result.getType();
  TR->dump(stream);
  stream << "\n";
}

FieldTypeCollectionResult TypeRefBuilder::collectFieldTypes(
    llvm::Optional<std::string> forMangledTypeName) {
  FieldTypeCollectionResult result;
  for (const auto &sections : ReflectionInfos) {
    for (auto descriptor : sections.Field) {
      auto typeRef = readTypeRef(descriptor, descriptor->MangledTypeName);
      auto typeName = nodeToString(demangleTypeRef(typeRef));
      auto optionalMangledTypeName = normalizeReflectionName(typeRef);
      clearNodeFactory();
      if (optionalMangledTypeName.hasValue()) {
        auto mangledTypeName =
          optionalMangledTypeName.getValue();
        if (forMangledTypeName.hasValue()) {
          if (mangledTypeName != forMangledTypeName.getValue())
            continue;
        }

        std::vector<PropertyTypeInfo> properties;
        std::vector<EnumCaseInfo> enumCases;
        for (auto &fieldRef : *descriptor.getLocalBuffer()) {
          auto field = descriptor.getField(fieldRef);
          auto fieldName = getTypeRefString(readTypeRef(field, field->FieldName));
          if (field->hasMangledTypeName()) {
            std::string mangledFieldTypeName =
                std::string(field->MangledTypeName);
            auto fieldTypeRef = readTypeRef(field, field->MangledTypeName);
            auto optionalMangledfieldTypeName =
                normalizeReflectionName(fieldTypeRef);
            if (optionalMangledfieldTypeName.hasValue()) {
              mangledFieldTypeName = optionalMangledfieldTypeName.getValue();
            }
            auto fieldTypeDemangleTree = demangleTypeRef(fieldTypeRef);
            auto fieldTypeName = nodeToString(fieldTypeDemangleTree);
            std::stringstream OS;
            dumpTypeRef(fieldTypeRef, OS);
            properties.emplace_back(PropertyTypeInfo{fieldName.str(),
                                                     mangledFieldTypeName,
                                                     fieldTypeName, OS.str()});
          } else {
            enumCases.emplace_back(EnumCaseInfo{fieldName.str()});
          }
        }
        result.FieldInfos.emplace_back(FieldMetadata{
            mangledTypeName, typeName, properties, enumCases});
      }
    }
  }

  return result;
}

void TypeRefBuilder::dumpFieldSection(std::ostream &stream) {
  auto fieldInfoCollectionResult =
      collectFieldTypes(llvm::Optional<std::string>());
  for (const auto &info : fieldInfoCollectionResult.FieldInfos) {
    stream << info.FullyQualifiedName << "\n";
    for (size_t i = 0; i < info.FullyQualifiedName.size(); ++i)
      stream << "-";
    stream << "\n";
    for (const auto &field : info.Properties) {
      stream << field.Label;
      stream << ": ";
      stream << field.TypeDiagnosticPrintName;
    }
    for (const auto &field : info.EnumCases) {
      stream << field.Label;
      stream << "\n\n";
    }
  }
}

AssociatedTypeCollectionResult TypeRefBuilder::collectAssociatedTypes(
    llvm::Optional<std::string> forMangledTypeName) {
  AssociatedTypeCollectionResult result;
  for (const auto &sections : ReflectionInfos) {
    for (auto descriptor : sections.AssociatedType) {
      auto typeRef = readTypeRef(descriptor, descriptor->ConformingTypeName);
      auto typeName = nodeToString(demangleTypeRef(typeRef));
      auto optionalMangledTypeName = normalizeReflectionName(typeRef);
      auto protocolNode = demangleTypeRef(
          readTypeRef(descriptor, descriptor->ProtocolTypeName));
      auto protocolName = nodeToString(protocolNode);
      clearNodeFactory();
      if (optionalMangledTypeName.hasValue()) {
        auto mangledTypeName =
            optionalMangledTypeName.getValue();
        if (forMangledTypeName.hasValue()) {
          if (mangledTypeName != forMangledTypeName.getValue())
            continue;
        }
        std::vector<AssociatedType> associatedTypes;
        for (const auto &associatedTypeRef : *descriptor.getLocalBuffer()) {
          auto associatedType = descriptor.getField(associatedTypeRef);
          std::string typealiasTypeName =
              getTypeRefString(
                  readTypeRef(associatedType, associatedType->Name))
                  .str();

          std::string mangledSubstitutedTypeName =
              std::string(associatedType->SubstitutedTypeName);
          auto substitutedTypeRef =
              readTypeRef(associatedType, associatedType->SubstitutedTypeName);
          auto optionalMangledSubstitutedTypeName =
              normalizeReflectionName(substitutedTypeRef);
          if (optionalMangledSubstitutedTypeName.hasValue()) {
            mangledSubstitutedTypeName = optionalMangledSubstitutedTypeName.getValue();
          }
          auto substitutedDemangleTree = demangleTypeRef(substitutedTypeRef);
          auto substitutedTypeName = nodeToString(substitutedDemangleTree);
          std::stringstream OS;
          dumpTypeRef(substitutedTypeRef, OS);
          associatedTypes.emplace_back(
              AssociatedType{typealiasTypeName, mangledSubstitutedTypeName,
                             substitutedTypeName, OS.str()});
        }
        result.AssociatedTypeInfos.emplace_back(AssociatedTypeInfo{
            mangledTypeName, typeName, protocolName, associatedTypes});
      }
    }
  }
  return result;
}

void TypeRefBuilder::dumpAssociatedTypeSection(std::ostream &stream) {
  auto associatedTypeCollectionResult =
      collectAssociatedTypes(llvm::Optional<std::string>());
  for (const auto &info : associatedTypeCollectionResult.AssociatedTypeInfos) {
    stream << "- " << info.FullyQualifiedName << " : "
           << info.ProtocolFullyQualifiedName << "\n";
    for (const auto &typeAlias : info.AssociatedTypes) {
      stream << "typealias " << typeAlias.TypeAliasName << " = "
             << typeAlias.SubstitutedTypeFullyQualifiedName << "\n";
      stream << typeAlias.SubstitutedTypeDiagnosticPrintName;
    }
    stream << "\n";
  }
}

void TypeRefBuilder::dumpBuiltinTypeSection(std::ostream &stream) {
  for (const auto &sections : ReflectionInfos) {
    for (auto descriptor : sections.Builtin) {
      auto typeNode =
          demangleTypeRef(readTypeRef(descriptor, descriptor->TypeName));
      auto typeName = nodeToString(typeNode);
      clearNodeFactory();

      stream << "\n- " << typeName << ":\n";
      stream << "Size: " << descriptor->Size << "\n";
      stream << "Alignment: " << descriptor->getAlignment() << ":\n";
      stream << "Stride: " << descriptor->Stride << ":\n";
      stream << "NumExtraInhabitants: " << descriptor->NumExtraInhabitants
             << ":\n";
      stream << "BitwiseTakable: " << descriptor->isBitwiseTakable() << ":\n";
    }
  }
}

void ClosureContextInfo::dump() const { dump(std::cerr); }

void ClosureContextInfo::dump(std::ostream &stream) const {
  stream << "- Capture types:\n";
  for (auto *TR : CaptureTypes) {
    if (TR == nullptr)
      stream << "!!! Invalid typeref\n";
    else
      TR->dump(stream);
  }
  stream << "- Metadata sources:\n";
  for (auto MS : MetadataSources) {
    if (MS.first == nullptr)
      stream << "!!! Invalid typeref\n";
    else
      MS.first->dump(stream);
    if (MS.second == nullptr)
      stream << "!!! Invalid metadata source\n";
    else
      MS.second->dump(stream);
  }
  stream << "\n";
}

void TypeRefBuilder::dumpCaptureSection(std::ostream &stream) {
  for (const auto &sections : ReflectionInfos) {
    for (const auto descriptor : sections.Capture) {
      auto info = getClosureContextInfo(descriptor);
      info.dump(stream);
    }
  }
}

void TypeRefBuilder::dumpMultiPayloadEnumSection(std::ostream &stream) {
  for (const auto &sections : ReflectionInfos) {
    for (const auto descriptor : sections.MultiPayloadEnum) {
      auto typeNode =
          demangleTypeRef(readTypeRef(descriptor, descriptor->TypeName));
      auto typeName = nodeToString(typeNode);
      clearNodeFactory();

      stream << "\n- " << typeName << ":\n";
      stream << "  Descriptor Size: " << descriptor->getSizeInBytes() << "\n";
      stream << "  Flags: " << std::hex << descriptor->getFlags() << std::dec;
      if (descriptor->usesPayloadSpareBits()) {
        stream << " usesPayloadSpareBits";
      }
      stream << "\n";
      auto maskBytes = descriptor->getPayloadSpareBitMaskByteCount();
      auto maskOffset = descriptor->getPayloadSpareBitMaskByteOffset();
      if (maskBytes > 0) {
        if (maskOffset > 0) {
          stream << "  Spare bit mask: (offset " << maskOffset << " bytes) 0x";
        } else {
          stream << "  Spare bit mask: 0x";
        }
        const uint8_t *p = descriptor->getPayloadSpareBits();
        for (unsigned i = 0; i < maskBytes; i++) {
          stream << std::hex << std::setw(2) << std::setfill('0') << p[i];
        }
        stream << std::dec << "\n";
      }
      stream << "\n";
    }
  }
}

#endif
