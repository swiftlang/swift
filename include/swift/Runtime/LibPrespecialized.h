//===--- LibPrespecialized.h - Interface for prespecializations -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Interface for interacting with prespecialized metadata library.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LIB_PRESPECIALIZED_H
#define SWIFT_LIB_PRESPECIALIZED_H

#include "PrebuiltStringMap.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/TargetLayout.h"
#include "swift/Demangling/Demangler.h"

#define LIB_PRESPECIALIZED_TOP_LEVEL_SYMBOL_NAME "_swift_prespecializationsData"

namespace swift {

template <typename Runtime>
struct LibPrespecializedData {
  uint32_t majorVersion;
  uint32_t minorVersion;

  TargetPointer<Runtime, const void> metadataMap;
  TargetPointer<Runtime, const void> disabledProcessesTable;
  TargetPointer<Runtime, const void> pointerKeyedMetadataMap;

  typename Runtime::StoredSize optionFlags;

  TargetPointer<Runtime, const void> descriptorMap;

  // Existing fields are above, add new fields below this point.

  // The major/minor version numbers for this version of the struct.
  static constexpr uint32_t currentMajorVersion = 1;
  static constexpr uint32_t currentMinorVersion = 4;

  // Version numbers where various fields were introduced.
  static constexpr uint32_t minorVersionWithDisabledProcessesTable = 2;
  static constexpr uint32_t minorVersionWithPointerKeyedMetadataMap = 3;
  static constexpr uint32_t minorVersionWithOptionFlags = 3;
  static constexpr uint32_t minorVersionWithDescriptorMap = 4;

  // Option flags values.
  enum : typename Runtime::StoredSize {
    // When this flag is set, the runtime should default to using the
    // pointer-keyed table. When not set, default to using the name-keyed table.
    OptionFlagDefaultToPointerKeyedMap = 1ULL << 0,

    // When this flag is set, the runtime should default to using the descriptor
    // map. When not set, default to turning off the descriptor map.
    OptionFlagDescriptorMapDefaultOn = 1ULL << 1,

    // When this flag is set, descriptorMap is not comprehensive, meaning that
    // a negative lookup result is not a definitive failure.
    OptionFlagDescriptorMapNotComprehensive = 1ULL << 2,
  };

  // Helpers for safely retrieving various fields. Helpers return 0 or NULL if
  // the version number indicates that the field is not present.

  typename Runtime::StoredSize getOptionFlags() const {
    if (minorVersion < minorVersionWithOptionFlags)
      return 0;
    return optionFlags;
  }

  static bool stringIsNull(const char *str) { return str == nullptr; }

  using MetadataMap = PrebuiltStringMap<const char *, Metadata *, stringIsNull>;

  const MetadataMap *getMetadataMap() const {
    return reinterpret_cast<const MetadataMap *>(metadataMap);
  }

  const char *const *getDisabledProcessesTable() const {
    if (minorVersion < minorVersionWithDisabledProcessesTable)
      return nullptr;
    return reinterpret_cast<const char *const *>(disabledProcessesTable);
  }

  const void *getPointerKeyedMetadataMap() const {
    if (minorVersion < minorVersionWithPointerKeyedMetadataMap)
      return nullptr;
    return pointerKeyedMetadataMap;
  }

  using DescriptorMap =
      PrebuiltAuxDataImplicitStringMap<TargetPointer<Runtime, const void>,
                                       uint16_t>;

  const DescriptorMap *getDescriptorMap() const {
    if (minorVersion < minorVersionWithDescriptorMap)
      return nullptr;
    return reinterpret_cast<const DescriptorMap *>(descriptorMap);
  }
};

enum class LibPrespecializedLookupResult {
  // We found something.
  Found,

  // We didn't find anything, and we know it's not in the shared cache.
  DefinitiveNotFound,

  // We didn't find anything, but we couldn't rule out the shared cache. Caller
  // must do a full search.
  NonDefinitiveNotFound,
};

const LibPrespecializedData<InProcess> *getLibPrespecializedData();

Metadata *getLibPrespecializedMetadata(const TypeContextDescriptor *description,
                                       const void *const *arguments);
void libPrespecializedImageLoaded();

std::pair<LibPrespecializedLookupResult, const TypeContextDescriptor *>
getLibPrespecializedTypeDescriptor(Demangle::NodePointer node);

/// Given the demangling referring to a particular descriptor, build the
/// canonical simplified version of the demangling that's used for the keys in
/// the descriptorMap. We copy across Extension and Module nodes. Type nodes are
/// all normalized to be OtherNominalType to allow for the runtime allowing
/// type kind mismatches on imported C types in certain cases. Other nodes are
/// skipped.
///
/// The runtime always searches through duplicates in the table, and uses its
/// own matching on all candidates, so the simplified demangling is allowed to
/// be simplified to the point of having different descriptors sometimes produce
/// the same demangling.
static inline Demangle::NodePointer
buildSimplifiedDescriptorDemangling(Demangle::NodePointer node,
                                    Demangle::Demangler &dem) {
  // The node that will be returned to the caller.
  Demangle::NodePointer result = nullptr;

  // The bottommost node in the result that we've generated. Additional nodes
  // are added as children to this one.
  Demangle::NodePointer resultBottom = nullptr;

  // The current node that we're iterating over in the input node tree.
  Demangle::NodePointer current = node;

  using Kind = Demangle::Node::Kind;

  // Helper to add a new node to the result. This sets `result` to the node if
  // it hasn't already been set (indicating this is the topmost node), and adds
  // the node as a child to `resultBottom` otherwise. `resultBottom` is updated
  // to point to the new node.
  auto addNode = [&](Demangle::NodePointer newNode) {
    if (!result) {
      result = newNode;
    } else {
      if (resultBottom->getKind() == Kind::Extension) {
        resultBottom->addChild(newNode, dem);
      } else {
        // Shift the Identifier down, insert before it.
        resultBottom->addChild(resultBottom->getFirstChild(), dem);
        resultBottom->replaceChild(0, newNode);
      }
    }
    resultBottom = newNode;
  };

  // Walk down the input node tree.
  while (current) {
    switch (current->getKind()) {
    case Kind::Extension: {
      // Extensions are copied across. The new extension node has the module
      // from the original, and the second child will be added as we traverse
      // the next node in the tree.
      auto copy = dem.createNode(Kind::Extension);
      auto module = current->getChild(0);
      if (module == nullptr || module->getKind() != Kind::Module)
        return nullptr;
      copy->addChild(module, dem);
      addNode(copy);
      current = current->getChild(1);
      break;
    }
    case Kind::Module: {
      // Module contents are always in the form we want, so we can incorporate
      // this node verbatim and terminate the walk.
      addNode(current);
      current = nullptr;
      break;
    }
    case Kind::Protocol: {
      // Bring Protocol nodes across verbatim, there's no fuzzy matching.
      addNode(current);
      current = nullptr;
      break;
    }
    case Kind::OpaqueType:
    case Kind::Class:
    case Kind::Structure:
    case Kind::Enum:
    case Kind::TypeAlias:
    case Kind::OtherNominalType: {
      // Type nodes are copied across with the kind always set to
      // OtherNominalType.
      auto copy = dem.createNode(Kind::OtherNominalType);
      auto identifier = current->getChild(1);
      if (identifier == nullptr || identifier->getKind() != Kind::Identifier)
        return nullptr;
      copy->addChild(identifier, dem);
      addNode(copy);
      current = current->getChild(0);
      break;
    }

    default:
      // If we don't know about this node, continue the walk with its first
      // child.
      current = current->getFirstChild();
      break;
    }
  }

  return result;
}

} // namespace swift

// Validate the prespecialized metadata map by building each entry dynamically
// and comparing. This should be called before any metadata is built for other
// purposes, as any prespecialized entries that have already been cached will
// not be rebuilt, so the validation will be comparing the prespecialized
// metadata with itself.
//
// On return, outValidated is set to the total number of metadata records that
// were validated (which is the total number in the table), and outFailed is set
// to the number that failed validation.
SWIFT_RUNTIME_EXPORT
void _swift_validatePrespecializedMetadata();

#endif // SWIFT_LIB_PRESPECIALIZED_H
