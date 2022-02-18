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

#endif // SWIFT_C_BINARY_SCAN_IMPL_H
