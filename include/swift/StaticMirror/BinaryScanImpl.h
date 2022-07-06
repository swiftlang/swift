//===---------------- BinaryScanImpl.h - Swift Compiler ------------------===//
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
// Implementation details of the binary scanning C API
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_C_BINARY_SCAN_IMPL_H
#define SWIFT_C_BINARY_SCAN_IMPL_H

#include "swift-c/StaticMirror/BinaryScan.h"

namespace swift {
namespace static_mirror {
class BinaryScanningTool;
}
} // namespace swift

struct swift_static_mirror_conformance_info_s {
  swift_static_mirror_string_ref_t type_name;
  swift_static_mirror_string_ref_t mangled_type_name;
  swift_static_mirror_string_ref_t protocol_name;
};

struct swift_static_mirror_type_alias_s {
  swift_static_mirror_string_ref_t type_alias_name;
  swift_static_mirror_string_ref_t substituted_type_name;
  swift_static_mirror_string_ref_t substituted_type_mangled_name;
  swift_static_mirror_string_set_t *opaque_requirements_set;
};

struct swift_static_mirror_associated_type_info_s {
  swift_static_mirror_string_ref_t mangled_type_name;
  swift_static_mirror_type_alias_set_t *type_alias_set;
};

struct swift_static_mirror_enum_case_info_s {
  swift_static_mirror_string_ref_t label;
};

struct swift_static_mirror_property_info_s {
  swift_static_mirror_string_ref_t label;
  swift_static_mirror_string_ref_t type_name;
  swift_static_mirror_string_ref_t mangled_type_name;
};

struct swift_static_mirror_field_info_s {
  swift_static_mirror_string_ref_t mangled_type_name;
  swift_static_mirror_property_info_set_t *property_set;
  swift_static_mirror_enum_case_info_set_t *enum_case_set;
};

#endif // SWIFT_C_BINARY_SCAN_IMPL_H
