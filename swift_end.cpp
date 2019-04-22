//===--- SwiftRT-ELF.cpp --------------------------------------------------===//
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

#include <cstddef>

// We synthesize the start/stop symbols ourselves.
#define DECLARE_SWIFT_SECTION(name)                                                          \
  __attribute__((__section__(#name),__visibility__("hidden"),__aligned__(1))) const void* __stop_##name = (void*)0xfacefeed; \

extern "C" {
DECLARE_SWIFT_SECTION(swift5_protocols)
DECLARE_SWIFT_SECTION(swift5_protocol_conformances)
DECLARE_SWIFT_SECTION(swift5_type_metadata)

DECLARE_SWIFT_SECTION(swift5_typeref)
DECLARE_SWIFT_SECTION(swift5_reflstr)
DECLARE_SWIFT_SECTION(swift5_fieldmd)
DECLARE_SWIFT_SECTION(swift5_assocty)
DECLARE_SWIFT_SECTION(swift5_replace)
}
