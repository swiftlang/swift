//===------------ DependencyScanImpl.cpp - Swift Compiler -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementation of the dependency scanning C API
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVMInitialize.h"
#include "swift/StaticMirror/BinaryScanningTool.h"
#include "swift/StaticMirror/BinaryScanImpl.h"
#include "swift/DependencyScan/StringUtils.h"

// FIXME: Code duplication with StringUtils.cpp
namespace swift {
namespace c_string_utils {

swiftscan_string_ref_t create_null() {
  swiftscan_string_ref_t str;
  str.data = nullptr;
  str.length = 0;
  return str;
}

swiftscan_string_ref_t create_clone(const char *string) {
  if (!string)
    return create_null();

  if (string[0] == '\0')
    return create_null();

  swiftscan_string_ref_t str;
  str.data = strdup(string);
  str.length = strlen(string);
  return str;
}

} // namespace c_string_utils
} // namespace swift

using namespace swift::static_mirror;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(BinaryScanningTool, swift_static_mirror_t)

//=== Private Cleanup Functions -------------------------------------------===//

/// Free the given string.
void swift_static_mirror_string_dispose(swift_static_mirror_string_ref_t string) {
  if (string.data)
    free(const_cast<void *>(string.data));
}

//=== Static Mirror Scan Functions ---------------------------------------===//
swift_static_mirror_t
swift_static_mirror_create(int num_binaries, const char **binary_paths,
                               const char *arch) {
  //INITIALIZE_LLVM();
  std::vector<std::string> inputBinaryPaths;
  for (unsigned SI = 0, SE = num_binaries; SI < SE; ++SI)
    inputBinaryPaths.push_back(binary_paths[SI]);

  return wrap(new BinaryScanningTool(inputBinaryPaths, arch));
}

void swift_static_mirror_dispose(
    swift_static_mirror_t c_static_mirror) {
  delete unwrap(c_static_mirror);
}

swift_static_mirror_conformances_set_t *
swift_static_mirror_conformances_set_create(
    swift_static_mirror_t static_mirror, int num_protocols,
    const char **protocol_names) {
  std::vector<std::string> protocols;
  for (unsigned SI = 0, SE = num_protocols; SI < SE; ++SI)
    protocols.push_back(protocol_names[SI]);
  BinaryScanningTool *scanTool = unwrap(static_mirror);
  auto scanResult = scanTool->collectConformances(protocols);

  // Bridge to the C interface
  swift_static_mirror_conformances_set_t *conformanceSet =
        new swift_static_mirror_conformances_set_t;
  conformanceSet->count = scanResult.Conformances.size();
  conformanceSet->conformances =
      new swift_static_mirror_conformance_info_t[conformanceSet->count];

  size_t idx = 0;
  for (auto &conformance : scanResult.Conformances) {
    swift_static_mirror_conformance_info_s *conformanceInfo = new swift_static_mirror_conformance_info_s;
    conformanceSet->conformances[idx] = conformanceInfo;
    conformanceInfo->type_name = swift::c_string_utils::create_clone(conformance.TypeName.c_str());
    conformanceInfo->mangled_type_name = swift::c_string_utils::create_clone(conformance.MangledTypeName.c_str());
    conformanceInfo->protocol_name = swift::c_string_utils::create_clone(conformance.ProtocolName.c_str());
    idx += 1;
  }
  return conformanceSet;
}

swift_static_mirror_string_ref_t
swift_static_mirror_conformance_info_get_type_name(swift_static_mirror_conformance_info_t info) {
  return info->type_name;
}

swift_static_mirror_string_ref_t
swift_static_mirror_conformance_info_get_protocol_name(swift_static_mirror_conformance_info_t info) {
  return info->protocol_name;
}

swift_static_mirror_string_ref_t
swift_static_mirror_conformance_info_get_mangled_type_name(swift_static_mirror_conformance_info_t info) {
  return info->mangled_type_name;
}

void swift_static_mirror_conformance_info_dispose(
    swift_static_mirror_conformance_info_t info) {
  swift_static_mirror_conformance_info_s *info_impl = info;
  swift_static_mirror_string_dispose(info_impl->type_name);
  swift_static_mirror_string_dispose(info_impl->mangled_type_name);
  swift_static_mirror_string_dispose(info_impl->protocol_name);
}

void swift_static_mirror_conformances_set_dispose(
    swift_static_mirror_conformances_set_t *set) {
  for (size_t i = 0; i < set->count; ++i) {
    swift_static_mirror_conformance_info_dispose(set->conformances[i]);
  }
  delete[] set->conformances;
  delete set;
}

static swift_static_mirror_associated_type_info_set_t *
convertAssociatedTypeQueryResult(
    const swift::reflection::AssociatedTypeCollectionResult &scanResult) {
  swift_static_mirror_associated_type_info_set_t *result =
      new swift_static_mirror_associated_type_info_set_t;
  result->count = scanResult.AssociatedTypeInfos.size();
  result->associated_type_infos = new swift_static_mirror_associated_type_info_t
      [scanResult.AssociatedTypeInfos.size()];

  int associatedTypeInfoIndex = 0;
  for (const auto &assocTypeInfo : scanResult.AssociatedTypeInfos) {
    swift_static_mirror_associated_type_info_s *info =
        new swift_static_mirror_associated_type_info_s;
    info->mangled_type_name = swift::c_string_utils::create_clone(
        assocTypeInfo.MangledTypeName.c_str());
    info->type_alias_set = new swift_static_mirror_type_alias_set_t;
    info->type_alias_set->count = assocTypeInfo.AssociatedTypes.size();
    info->type_alias_set->type_aliases =
        new swift_static_mirror_type_alias_t[assocTypeInfo.AssociatedTypes
                                                      .size()];
    int typealiasIndex = 0;
    for (const auto &typeAliasInfo : assocTypeInfo.AssociatedTypes) {
      swift_static_mirror_type_alias_s *typealiasDetails =
          new swift_static_mirror_type_alias_s;
      typealiasDetails->type_alias_name = swift::c_string_utils::create_clone(
          typeAliasInfo.TypeAliasName.c_str());
      typealiasDetails->substituted_type_name =
          swift::c_string_utils::create_clone(
              typeAliasInfo.SubstitutedTypeFullyQualifiedName.c_str());
      typealiasDetails->substituted_type_mangled_name =
          swift::c_string_utils::create_clone(
              typeAliasInfo.SubstitutedTypeMangledName.c_str());
      info->type_alias_set->type_aliases[typealiasIndex] =
          typealiasDetails;
      typealiasIndex += 1;
    }
    result->associated_type_infos[associatedTypeInfoIndex] = info;
    associatedTypeInfoIndex += 1;
  }
  return result;
}

swift_static_mirror_associated_type_info_set_t *
swift_static_mirror_associated_type_info_set_create(
    swift_static_mirror_t static_mirror, const char *forTypeName) {
  BinaryScanningTool *scanTool = unwrap(static_mirror);
  auto scanResult = scanTool->collectAssociatedTypes(forTypeName);
  return convertAssociatedTypeQueryResult(scanResult);
}

/// Identify and collect associated types of all discovered types.
swift_static_mirror_associated_type_info_set_t *
swift_static_mirror_all_associated_type_info_set_create(
    swift_static_mirror_t static_mirror) {
  BinaryScanningTool *scanTool = unwrap(static_mirror);
  auto scanResult = scanTool->collectAllAssociatedTypes();
  return convertAssociatedTypeQueryResult(scanResult);
}

// swift_static_mirror_associated_type query methods
swift_static_mirror_string_ref_t
swift_static_mirror_type_alias_get_type_alias_name(
    swift_static_mirror_type_alias_t type_alias) {
  return type_alias->type_alias_name;
}
swift_static_mirror_string_ref_t
swift_static_mirror_type_alias_get_substituted_type_name(
    swift_static_mirror_type_alias_t type_alias) {
  return type_alias->substituted_type_name;
}
swift_static_mirror_string_ref_t
swift_static_mirror_type_alias_get_substituted_type_mangled_name(
    swift_static_mirror_type_alias_t type_alias) {
  return type_alias->substituted_type_mangled_name;
}

// swift_static_mirror_associated_type_info query methods
swift_static_mirror_string_ref_t
swift_static_mirror_associated_type_info_get_mangled_type_name(
    swift_static_mirror_associated_type_info_t associated_type_info) {
  return associated_type_info->mangled_type_name;
}
swift_static_mirror_type_alias_set_t*
swift_static_mirror_associated_type_info_get_type_alias_set(
    swift_static_mirror_associated_type_info_t associated_type_info) {
  return associated_type_info->type_alias_set;
}

void swift_static_mirror_type_alias_dispose(swift_static_mirror_type_alias_t type_alias) {
  swift_static_mirror_string_dispose(type_alias->substituted_type_mangled_name);
  swift_static_mirror_string_dispose(type_alias->substituted_type_name);
  swift_static_mirror_string_dispose(type_alias->type_alias_name);
  delete type_alias;
}

void swift_static_mirror_type_alias_set_dispose(swift_static_mirror_type_alias_set_t *type_alias_set) {
  for (size_t i = 0; i < type_alias_set->count; ++i) {
    swift_static_mirror_type_alias_dispose(type_alias_set->type_aliases[i]);
  }
  delete[] type_alias_set->type_aliases;
  delete type_alias_set;
}

void swift_static_mirror_associated_type_info_dispose(swift_static_mirror_associated_type_info_t associated_type_info) {
  swift_static_mirror_string_dispose(associated_type_info->mangled_type_name);
  swift_static_mirror_type_alias_set_dispose(associated_type_info->type_alias_set);
}

void swift_static_mirror_associated_type_info_set_dispose(swift_static_mirror_associated_type_info_set_t *associated_type_info_set) {
  for (size_t i = 0; i < associated_type_info_set->count; ++i) {
    swift_static_mirror_associated_type_info_dispose(associated_type_info_set->associated_type_infos[i]);
  }
  delete[] associated_type_info_set->associated_type_infos;
  delete associated_type_info_set;
}
