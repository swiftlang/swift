//===--- TypeID.h - Simple Type Identification ------------------*- C++ -*-===//
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
//  This file defines the TypeID template, which provides a numeric
//  encoding of (static) type information for use as a simple replacement
//  for run-time type information.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_TYPEID_H
#define SWIFT_BASIC_TYPEID_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <cstdint>
#include <string>
#include <vector>

namespace swift {

enum class Zone : uint8_t {
  C                       = 0,
  AST                     = 1,
  AccessControl           = 11,
  IDETypes                = 136,
  IDE                     = 137,
  IDETypeChecking         = 97,
  NameLookup              = 9,
  Parse                   = 8,
  TypeChecker             = 10,
  // N.B. This is not a formal zone and exists solely to support the unit tests.
  ArithmeticEvaluator     = 255,
};

static_assert(std::is_same<std::underlying_type<Zone>::type, uint8_t>::value,
              "underlying type is no longer uint8_t!");

/// Form a unique 64-bit integer value describing the type `T`.
///
/// This template needs to be specialized for every type that can
/// participate in this kind of run-time type information, e.g., so
/// that it can be stored in \c AnyValue.
template<typename T>
struct TypeID;

/// Template whose specializations provide the set of type IDs within a
/// given zone.
template<Zone Zone> struct TypeIDZoneTypes;

/// Form a type ID given a zone and type value.
constexpr uint64_t formTypeID(uint8_t zone, uint8_t type) {
  return (uint64_t(zone) << 8) | uint64_t(type);
}

// Define the C type zone (zone 0).
#define SWIFT_TYPEID_ZONE C
#define SWIFT_TYPEID_HEADER "swift/Basic/CTypeIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"

} // end namespace swift

#endif // SWIFT_BASIC_TYPEID_H
