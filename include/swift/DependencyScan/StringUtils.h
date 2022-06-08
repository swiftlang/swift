//===----- StringUtils.h - Managed C String Utility Functions -----*- C -*-===//
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

#include "swift-c/CommonString/CommonString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include <string>
#include <vector>

//=== Private Utility Functions--------------------------------------------===//
namespace swift {
namespace c_string_utils {

/// Create null string
swiftscan_string_ref_t create_null();

/// Create a \c swiftscan_string_ref_t object from a nul-terminated C string.  New
/// \c swiftscan_string_ref_t will contain a copy of \p string.
swiftscan_string_ref_t create_clone(const char *string);

/// Create an array of \c swiftscan_string_ref_t objects from a vector of C++ strings using the
/// create_clone routine.
swiftscan_string_set_t *create_set(const std::vector<std::string> &strings);

/// Create an array of swiftscan_string_ref_t objects from an array of C strings using the
/// create_clone routine.
swiftscan_string_set_t *create_set(int count, const char **strings);

/// Create an empty array of swiftscan_string_ref_t objects
swiftscan_string_set_t *create_empty_set();

/// Retrieve the character data associated with the given string.
const char *get_C_string(swiftscan_string_ref_t string);
}
}
