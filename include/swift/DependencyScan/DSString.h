//===-- swift-c/SCString.h - Managed C String Utility Functions ---*- C -*-===//
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

#include "swift-c/DependencyScan/DSString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include <string>
#include <vector>

//=== Private Utility Functions--------------------------------------------===//
namespace swift {
namespace dependencies {
/// Create an empty "" string
ds_string_t create_empty();

/// Create null string
ds_string_t create_null();

/// Create a c_string_t object from a nul-terminated C string.  New
/// c_string_t may contain a pointer to \p String.
///
/// \p String should not be changed by the caller afterwards.
ds_string_t create_ref(const char *string);

/// Create a c_string_t object from a nul-terminated C string.  New
/// c_string_t will contain a copy of \p String.
ds_string_t create_dup(const char *string);

// std::string is already intended to be used as backing storage for CXString.
// Instead, call \c create_ref(string.c_str()).
ds_string_t create_ref(std::string string) = delete;

ds_string_set_t *create_set(const std::vector<std::string> &strings);
}
}
