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

TypeRefBuilder::TypeRefBuilder() : TC(*this) {}

const TypeRef * TypeRefBuilder::
lookupTypeWitness(const std::string &MangledTypeName,
                  const std::string &Member,
                  const TypeRef *Protocol) {
  TypeRefID key;
  key.addString(MangledTypeName);
  key.addString(Member);
  key.addPointer(Protocol);
  auto found = AssociatedTypeCache.find(key);
  if (found != AssociatedTypeCache.end())
    return found->second;

  // Cache missed - we need to look through all of the assocty sections
  // for all images that we've been notified about.
  for (auto &Info : ReflectionInfos) {
    for (const auto &AssocTyDescriptor : Info.assocty) {
      std::string ConformingTypeName(AssocTyDescriptor.ConformingTypeName);
      if (ConformingTypeName.compare(MangledTypeName) != 0)
        continue;

      std::string ProtocolMangledName(AssocTyDescriptor.ProtocolTypeName);
      Demangle::Demangler Dem;
      auto DemangledProto = Dem.demangleType(ProtocolMangledName);
      auto TR = swift::remote::decodeMangledType(*this, DemangledProto);

      if (Protocol != TR)
        continue;

      for (auto &AssocTy : AssocTyDescriptor) {
        if (Member.compare(AssocTy.getName()) != 0)
          continue;

        auto SubstitutedTypeName = AssocTy.getMangledSubstitutedTypeName();
        auto Demangled = Dem.demangleType(SubstitutedTypeName);
        auto *TypeWitness = swift::remote::decodeMangledType(*this, Demangled);

        AssociatedTypeCache.insert(std::make_pair(key, TypeWitness));
        return TypeWitness;
      }
    }
  }
  return nullptr;
}

const TypeRef * TypeRefBuilder::
lookupSuperclass(const std::string &MangledTypeName) {
  // Superclasses are recorded as a special associated type named 'super'
  // on the 'AnyObject' protocol.
  return lookupTypeWitness(MangledTypeName, "super",
                           ProtocolTypeRef::create(*this, "s9AnyObject_p"));
}

const TypeRef * TypeRefBuilder::
lookupSuperclass(const TypeRef *TR) {
  const TypeRef *Superclass = nullptr;

  if (auto *Nominal = dyn_cast<NominalTypeRef>(TR)) {
    Superclass = lookupSuperclass(Nominal->getMangledName());
  } else {
    auto BG = cast<BoundGenericTypeRef>(TR);
    Superclass = lookupSuperclass(BG->getMangledName());
  }

  if (Superclass == nullptr)
    return nullptr;

  return Superclass->subst(*this, TR->getSubstMap());
}

const FieldDescriptor *
TypeRefBuilder::getFieldTypeInfo(const TypeRef *TR) {
  std::string MangledName;
  if (auto N = dyn_cast<NominalTypeRef>(TR))
    MangledName = N->getMangledName();
  else if (auto BG = dyn_cast<BoundGenericTypeRef>(TR))
    MangledName = BG->getMangledName();
  else if (auto P = dyn_cast<ProtocolTypeRef>(TR))
    MangledName = P->getMangledName();
  else
    return {};

  std::vector<std::pair<std::string, const TypeRef *>> Fields;
  for (auto Info : ReflectionInfos) {
    for (auto &FD : Info.fieldmd) {
      if (!FD.hasMangledTypeName())
        continue;
      auto CandidateMangledName = FD.getMangledTypeName();
      if (MangledName.compare(CandidateMangledName) != 0)
        continue;
      return &FD;
    }
  }

  return nullptr;
}

std::vector<FieldTypeInfo>
TypeRefBuilder::getFieldTypeRefs(const TypeRef *TR, const FieldDescriptor *FD) {
  if (FD == nullptr)
    return {};

  auto Subs = TR->getSubstMap();

  Demangle::Demangler Dem;
  std::vector<FieldTypeInfo> Fields;
  for (auto &Field : *FD) {
    auto FieldName = Field.getFieldName();

    // Empty cases of enums do not have a type
    if (FD->isEnum() && !Field.hasMangledTypeName()) {
      Fields.push_back(FieldTypeInfo::forEmptyCase(FieldName));
      continue;
    }

    auto Demangled = Dem.demangleType(Field.getMangledTypeName());
    auto Unsubstituted = swift::remote::decodeMangledType(*this, Demangled);
    if (!Unsubstituted)
      return {};

    auto Substituted = Unsubstituted->subst(*this, Subs);

    if (FD->isEnum() && Field.isIndirectCase()) {
      Fields.push_back(FieldTypeInfo::forIndirectCase(FieldName, Substituted));
      continue;
    }

    Fields.push_back(FieldTypeInfo::forField(FieldName, Substituted));
  }
  return Fields;
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
    for (auto &BuiltinTypeDescriptor : Info.builtin) {
      assert(BuiltinTypeDescriptor.Size > 0);
      assert(BuiltinTypeDescriptor.Alignment > 0);
      assert(BuiltinTypeDescriptor.Stride > 0);
      if (!BuiltinTypeDescriptor.hasMangledTypeName())
        continue;
      auto CandidateMangledName = BuiltinTypeDescriptor.getMangledTypeName();
      if (MangledName.compare(CandidateMangledName) != 0)
        continue;
      return &BuiltinTypeDescriptor;
    }
  }

  return nullptr;
}

const CaptureDescriptor *
TypeRefBuilder::getCaptureDescriptor(uintptr_t RemoteAddress) {
  for (auto Info : ReflectionInfos) {
    for (auto &CD : Info.capture) {
      auto OtherAddr = ((uintptr_t) &CD -
                        Info.LocalStartAddress +
                        Info.RemoteStartAddress);
      if (OtherAddr == RemoteAddress)
        return &CD;
    }
  }

  return nullptr;
}

/// Get the unsubstituted capture types for a closure context.
ClosureContextInfo
TypeRefBuilder::getClosureContextInfo(const CaptureDescriptor &CD) {
  ClosureContextInfo Info;

  Demangle::Demangler Dem;
  for (auto i = CD.capture_begin(), e = CD.capture_end(); i != e; ++i) {
    const TypeRef *TR = nullptr;
    if (i->hasMangledTypeName()) {
      auto MangledName = i->getMangledTypeName();
      auto DemangleTree = Dem.demangleType(MangledName);
      TR = swift::remote::decodeMangledType(*this, DemangleTree);
    }
    Info.CaptureTypes.push_back(TR);
  }

  for (auto i = CD.source_begin(), e = CD.source_end(); i != e; ++i) {
    const TypeRef *TR = nullptr;
    if (i->hasMangledTypeName()) {
      auto MangledName = i->getMangledTypeName();
      auto DemangleTree = Dem.demangleType(MangledName);
      TR = swift::remote::decodeMangledType(*this, DemangleTree);
    }

    const MetadataSource *MS = nullptr;
    if (i->hasMangledMetadataSource()) {
      auto MangledMetadataSource = i->getMangledMetadataSource();
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
TypeRefBuilder::dumpTypeRef(const std::string &MangledName,
                            std::ostream &OS, bool printTypeName) {
  auto TypeName = Demangle::demangleTypeAsString(MangledName);
  OS << TypeName << '\n';

  Demangle::Demangler Dem;
  auto DemangleTree = Dem.demangleType(MangledName);
  auto TR = swift::remote::decodeMangledType(*this, DemangleTree);
  if (!TR) {
    OS << "!!! Invalid typeref: " << MangledName << '\n';
    return;
  }
  TR->dump(OS);
  OS << '\n';
}

void TypeRefBuilder::dumpFieldSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    for (const auto &descriptor : sections.fieldmd) {
      auto TypeName
        = Demangle::demangleTypeAsString(descriptor.getMangledTypeName());
      OS << TypeName << '\n';
      for (size_t i = 0; i < TypeName.size(); ++i)
        OS << '-';
      OS << '\n';
      for (auto &field : descriptor) {
        OS << field.getFieldName();
        if (field.hasMangledTypeName()) {
          OS << ": ";
          dumpTypeRef(field.getMangledTypeName(), OS);
        } else {
          OS << "\n\n";
        }
      }
    }
  }
}

void TypeRefBuilder::dumpAssociatedTypeSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    for (const auto &descriptor : sections.assocty) {
      auto conformingTypeName = Demangle::demangleTypeAsString(
        descriptor.getMangledConformingTypeName());
      auto protocolName = Demangle::demangleTypeAsString(
        descriptor.getMangledProtocolTypeName());

      OS << "- " << conformingTypeName << " : " << protocolName;
      OS << '\n';

      for (const auto &associatedType : descriptor) {
        OS << "typealias " << associatedType.getName() << " = ";
        dumpTypeRef(associatedType.getMangledSubstitutedTypeName(), OS);
      }
    }
  }
}

void TypeRefBuilder::dumpBuiltinTypeSection(std::ostream &OS) {
  for (const auto &sections : ReflectionInfos) {
    for (const auto &descriptor : sections.builtin) {
      auto typeName = Demangle::demangleTypeAsString(
        descriptor.getMangledTypeName());

      OS << "\n- " << typeName << ":\n";
      OS << "Size: " << descriptor.Size << "\n";
      OS << "Alignment: " << descriptor.Alignment << "\n";
      OS << "Stride: " << descriptor.Stride << "\n";
      OS << "NumExtraInhabitants: " << descriptor.NumExtraInhabitants << "\n";
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
    for (const auto &descriptor : sections.capture) {
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
