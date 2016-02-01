//===--- MetadataLookup.cpp - Swift Language Type Name Lookup -------------===//
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
// Implementations of runtime functions for looking up a type by name.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/StringExtras.h"
#include "Private.h"

#if defined(__APPLE__) && defined(__MACH__)
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#elif defined(__ELF__)
#include <elf.h>
#include <link.h>
#endif

#include <dlfcn.h>
#include <mutex>

using namespace swift;
using namespace Demangle;

#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>
#include <objc/message.h>
#include <objc/objc.h>
#endif

#if defined(__APPLE__) && defined(__MACH__)
#define SWIFT_TYPE_METADATA_SECTION "__swift2_types"
#elif defined(__ELF__)
#define SWIFT_TYPE_METADATA_SECTION ".swift2_type_metadata_start"
#endif

// Type Metadata Cache.

namespace {
  struct TypeMetadataSection {
    const TypeMetadataRecord *Begin, *End;
    const TypeMetadataRecord *begin() const {
      return Begin;
    }
    const TypeMetadataRecord *end() const {
      return End;
    }
  };

  struct TypeMetadataCacheEntry {
  private:
    std::string Name;
    const Metadata *Metadata;

  public:
    TypeMetadataCacheEntry(const llvm::StringRef name,
                           const struct Metadata *metadata) {
      Name = name.str();
      Metadata = metadata;
    }

    bool matches(llvm::StringRef aName) {
      return aName.equals(Name);
    }

    const struct Metadata *getMetadata(void) {
      return Metadata;
    }
  };
}

static void _initializeCallbacksToInspectDylib();

struct TypeMetadataState {
  ConcurrentMap<size_t, TypeMetadataCacheEntry> Cache;
  std::vector<TypeMetadataSection> SectionsToScan;
  pthread_mutex_t SectionsToScanLock;

  TypeMetadataState() {
    SectionsToScan.reserve(16);
    pthread_mutex_init(&SectionsToScanLock, nullptr);
    _initializeCallbacksToInspectDylib();
  }
};

static Lazy<TypeMetadataState> TypeMetadataRecords;

static void
_registerTypeMetadataRecords(TypeMetadataState &T,
                             const TypeMetadataRecord *begin,
                             const TypeMetadataRecord *end) {
  pthread_mutex_lock(&T.SectionsToScanLock);
  T.SectionsToScan.push_back(TypeMetadataSection{begin, end});
  pthread_mutex_unlock(&T.SectionsToScanLock);
}

static void _addImageTypeMetadataRecordsBlock(const uint8_t *records,
                                              size_t recordsSize) {
  assert(recordsSize % sizeof(TypeMetadataRecord) == 0
         && "weird-sized type metadata section?!");

  // If we have a section, enqueue the type metadata for lookup.
  auto recordsBegin
    = reinterpret_cast<const TypeMetadataRecord*>(records);
  auto recordsEnd
    = reinterpret_cast<const TypeMetadataRecord*>
                                            (records + recordsSize);

  // type metadata cache should always be sufficiently initialized by this point.
  _registerTypeMetadataRecords(TypeMetadataRecords.unsafeGetAlreadyInitialized(),
                               recordsBegin, recordsEnd);
}

#if defined(__APPLE__) && defined(__MACH__)
static void _addImageTypeMetadataRecords(const mach_header *mh,
                                         intptr_t vmaddr_slide) {
#ifdef __LP64__
  using mach_header_platform = mach_header_64;
  assert(mh->magic == MH_MAGIC_64 && "loaded non-64-bit image?!");
#else
  using mach_header_platform = mach_header;
#endif

  // Look for a __swift2_types section.
  unsigned long recordsSize;
  const uint8_t *records =
    getsectiondata(reinterpret_cast<const mach_header_platform *>(mh),
                   SEG_TEXT, SWIFT_TYPE_METADATA_SECTION,
                   &recordsSize);

  if (!records)
    return;

  _addImageTypeMetadataRecordsBlock(records, recordsSize);
}
#elif defined(__ELF__)
static int _addImageTypeMetadataRecords(struct dl_phdr_info *info,
                                        size_t size, void * /*data*/) {
  void *handle;
  if (!info->dlpi_name || info->dlpi_name[0] == '\0') {
    handle = dlopen(nullptr, RTLD_LAZY);
  } else
    handle = dlopen(info->dlpi_name, RTLD_LAZY | RTLD_NOLOAD);
  auto records = reinterpret_cast<const uint8_t*>(
      dlsym(handle, SWIFT_TYPE_METADATA_SECTION));

  if (!records) {
    // if there are no type metadata records, don't hold this handle open.
    dlclose(handle);
    return 0;
  }

  // Extract the size of the type metadata block from the head of the section
  auto recordsSize = *reinterpret_cast<const uint64_t*>(records);
  records += sizeof(recordsSize);

  _addImageTypeMetadataRecordsBlock(records, recordsSize);

  dlclose(handle);
  return 0;
}
#endif

static void _initializeCallbacksToInspectDylib() {
#if defined(__APPLE__) && defined(__MACH__)
  // Install our dyld callback.
  // Dyld will invoke this on our behalf for all images that have already
  // been loaded.
  _dyld_register_func_for_add_image(_addImageTypeMetadataRecords);
#elif defined(__ELF__)
  // Search the loaded dls. Unlike the above, this only searches the already
  // loaded ones.
  // FIXME: Find a way to have this continue to happen after.
  // rdar://problem/19045112
  dl_iterate_phdr(_addImageTypeMetadataRecords, nullptr);
#else
# error No known mechanism to inspect dynamic libraries on this platform.
#endif
}

void
swift::swift_registerTypeMetadataRecords(const TypeMetadataRecord *begin,
                                         const TypeMetadataRecord *end) {
  auto &T = TypeMetadataRecords.get();
  _registerTypeMetadataRecords(T, begin, end);
}

// copied from ProtocolConformanceRecord::getCanonicalTypeMetadata()
const Metadata *TypeMetadataRecord::getCanonicalTypeMetadata() const {
  switch (getTypeKind()) {
  case TypeMetadataRecordKind::UniqueDirectType:
    return getDirectType();
  case TypeMetadataRecordKind::NonuniqueDirectType:
    return swift_getForeignTypeMetadata((ForeignTypeMetadata *)getDirectType());
  case TypeMetadataRecordKind::UniqueDirectClass:
    if (auto *ClassMetadata =
          static_cast<const struct ClassMetadata *>(getDirectType()))
      return swift_getObjCClassMetadata(ClassMetadata);
    else
      return nullptr;
  default:
    return nullptr;
  }
}

static const Metadata *
findMetadataForNode(const NodePointer node);

// returns the type metadata if typeNodeName matches the metadata's name
static const Metadata *
matchMetadataTypeNode(const llvm::StringRef typeNodeName,
                      const NodePointer typeNode,
                      const Metadata *metadata,
                      const NominalTypeDescriptor *ntd) {
  if (metadata != nullptr) {
    assert(ntd == nullptr);
    ntd = metadata->getNominalTypeDescriptor();
  }

  if (ntd->Name.get() != typeNodeName)
    return nullptr;

  // Instantiate resilient types.
  if (metadata == nullptr &&
      ntd->getGenericMetadataPattern() &&
      !ntd->GenericParams.hasGenericParams()) {
    return swift_getResilientMetadata(ntd->getGenericMetadataPattern());
  }

  return metadata;
}

// returns the generic type metadata for type named by boundGenericNode
static const Metadata *
matchMetadataGenericNode(const llvm::StringRef typeNodeName,
                         const NodePointer boundGenericNode,
                         const Metadata *unused __attribute__((unused)),
                         const NominalTypeDescriptor *ntd) {
  if (boundGenericNode->getNumChildren() != 2)
    return nullptr;

  auto templateTypeNode = boundGenericNode->getChild(0);
  auto typeListNode = boundGenericNode->getChild(1);

  if (templateTypeNode->getKind() != Node::Kind::Type ||
      typeListNode->getKind() != Node::Kind::TypeList)
    return nullptr;

  // Validate that we have one or more generic parameters (the zero parameter
  // case is handled by matchMetadataTypeNode) and that the number of primary
  // archetypes matches the number of child nodes in the demangled type.
  //
  // TODO: support secondary archetypes, where NumParams != NumPrimaryParams;
  // not sure if there is enough information available at runtime to do this.
  auto &genericParams = ntd->GenericParams;
  if (!genericParams.hasGenericParams() ||
      genericParams.NumPrimaryParams != typeListNode->getNumChildren() ||
      genericParams.NumParams != genericParams.NumPrimaryParams)
    return nullptr;

  auto templateName = mangleNode(templateTypeNode); // XXX cache this
  if (ntd->Name.get() != templateName)
    return nullptr;

  auto pattern = ntd->getGenericMetadataPattern();
  void **arguments = new void *[pattern->NumKeyArguments];

  for (size_t i = 0, witnessIndex = 0;
       i < genericParams.NumPrimaryParams; i++) {
    auto param = genericParams.getParameterAt(i);
    auto metadata = findMetadataForNode(typeListNode->getChild(i));
    if (metadata == nullptr) {
      // unknown type
      delete arguments;
      return nullptr;
    }

    for (size_t j = 0; j < param->NumWitnessTables; j++, witnessIndex++) {
      auto &protocol = param->Protocols[j];
      auto wt = swift_conformsToProtocol(metadata, protocol);
      if (wt == nullptr) {
        // does not conform to associated type requirement, bail
        delete arguments;
        return nullptr;
      }

      arguments[genericParams.NumPrimaryParams + witnessIndex] =
        const_cast<void *>(reinterpret_cast<const void *>(wt));
    }

    arguments[i] = const_cast<void *>(reinterpret_cast<const void *>(metadata));
  }

  auto metadata = swift_getGenericMetadata(pattern, arguments);

  delete arguments;
  return metadata;
}

static const Metadata *
matchMetadataNode(const llvm::StringRef typeNodeName,
                  const NodePointer typeNode,
                  const Metadata *metadata,
                  const NominalTypeDescriptor *ntd) {
  if (typeNode->getKind() != Node::Kind::Type || !typeNode->hasChildren())
    return nullptr;

  auto childNode = typeNode->getChild(0);
  const Metadata *foundMetadata = nullptr;

  switch (childNode->getKind()) {
  case Node::Kind::BoundGenericClass:
  case Node::Kind::BoundGenericEnum:
  case Node::Kind::BoundGenericStructure:
    if (ntd != nullptr)
      foundMetadata = matchMetadataGenericNode(typeNodeName, childNode,
                                               metadata, ntd);
    break;
  default:
    foundMetadata = matchMetadataTypeNode(typeNodeName, childNode,
                                          metadata, ntd);
    break;
  }

  return foundMetadata;
}

// caller must acquire lock
static const Metadata *
iterateTypeMetadata(
  const llvm::StringRef nameRef,
  const NodePointer node,
  llvm::function_ref<const Metadata *(llvm::StringRef,
                                      const NodePointer,
                                      const Metadata *,
                                      const NominalTypeDescriptor *)> match) {
  auto &T = TypeMetadataRecords.get();

  unsigned sectionIdx = 0;
  unsigned endSectionIdx = T.SectionsToScan.size();
  const Metadata *foundMetadata = nullptr;

  for (; sectionIdx < endSectionIdx; ++sectionIdx) {
    auto &section = T.SectionsToScan[sectionIdx];
    for (const auto &record : section) {
      if (auto metadata = record.getCanonicalTypeMetadata())
        foundMetadata = match(nameRef, node, metadata, nullptr);
      else if (auto ntd = record.getNominalTypeDescriptor())
        foundMetadata = match(nameRef, node, nullptr, ntd);

      if (foundMetadata != nullptr)
        return foundMetadata;
    }
  }

  return nullptr;
}

static const Metadata *
findMetadataForNode(const NodePointer node) {
  const Metadata *foundMetadata;

  auto name = mangleNode(node);
  auto nameRef = llvm::StringRef(name);

  foundMetadata = iterateTypeMetadata(nameRef, node, matchMetadataNode);
  if (foundMetadata == nullptr)
    foundMetadata = _iterateConformances(nameRef, node, matchMetadataNode);

  return foundMetadata;
}

static const Metadata *
_typeByMangledName(const llvm::StringRef typeName) {
  const Metadata *foundMetadata = nullptr;
  auto &T = TypeMetadataRecords.get();
  size_t hash = llvm::HashString(typeName);

  ConcurrentList<TypeMetadataCacheEntry> &Bucket = T.Cache.findOrAllocateNode(hash);

  // Check name to type metadata cache
  for (auto &Entry : Bucket) {
    if (Entry.matches(typeName))
      return Entry.getMetadata();
  }

  auto root = demangleTypeAsNode(typeName.data(), typeName.size());
  if (root == nullptr)
    return nullptr;

  pthread_mutex_lock(&T.SectionsToScanLock);
  foundMetadata = findMetadataForNode(root);
  pthread_mutex_unlock(&T.SectionsToScanLock);

  if (foundMetadata != nullptr)
    Bucket.push_front(TypeMetadataCacheEntry(typeName, foundMetadata));

#if SWIFT_OBJC_INTEROP
  // Check for ObjC class
  // FIXME does this have any value? any ObjC class with a Swift name
  // should already be registered as a Swift type.
  if (foundMetadata == nullptr) {
    std::string prefixedName("_Tt" + typeName.str());
    foundMetadata = reinterpret_cast<ClassMetadata *>
      (objc_lookUpClass(prefixedName.c_str()));
  }
#endif

  return foundMetadata;
}

/// Return the type metadata for a given mangled name, used in the
/// implementation of _typeByName(). The human readable name returned
/// by swift_getTypeName() is non-unique, so we used mangled names
/// internally.
extern "C"
const Metadata *
swift_getTypeByMangledName(const char *typeName, size_t typeNameLength) {
  llvm::StringRef name(typeName, typeNameLength);
  return _typeByMangledName(name);
}
