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
//#include "swift/Option/Options.h"

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
