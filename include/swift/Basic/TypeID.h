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

#include <cstdint>
#include <vector>

namespace swift {

/// Form a unique 64-bit integer value describing the type `T`.
///
/// This template needs to be specialized for every type that can
/// participate in this kind of run-time type information, e.g., so
/// that it can be stored in \c AnyValue.
template<typename T>
struct TypeID;

/// Each type "zone" can contain 256 separate types for use in typeid,
/// which should be enumerated.
struct TypeIdZones {
  enum ZoneValues : uint8_t {
    /// C/C++ language and standard library types.
    C = 0,

    /// A zone used only for unit testing.
    UnitTests = 1,
  };
};

/// Form a type ID given a zone and type value.
constexpr uint64_t formTypeID(uint8_t zone, uint8_t type) {
  return (uint64_t(zone) << 8) | uint64_t(type);
}

/// Assign the given type a particular value in the zone.
#define SWIFT_TYPEID(Zone, Type, Value)                           \
template<> struct TypeID<Type> {                                        \
  static const uint64_t value = formTypeID(TypeIdZones::Zone, Value);   \
}

/// Assign the given single-param template a particular value in the zone.
#define SWIFT_TYPEID_TEMPLATE1(Zone, Template, Value, Param1, Arg1)       \
template<Param1> struct TypeID<Template<Arg1>> {                          \
 static const uint64_t templateID = formTypeID(TypeIdZones::Zone, Value); \
                                                                          \
public:                                                                   \
  static const uint64_t value = (TypeID<Arg1>::value << 16) | templateID; \
}

// C types.
SWIFT_TYPEID(C, unsigned char, 1);
SWIFT_TYPEID(C, signed char, 2);
SWIFT_TYPEID(C, char, 3);
SWIFT_TYPEID(C, short, 4);
SWIFT_TYPEID(C, unsigned short, 5);
SWIFT_TYPEID(C, int, 6);
SWIFT_TYPEID(C, unsigned int, 7);
SWIFT_TYPEID(C, long, 8);
SWIFT_TYPEID(C, unsigned long, 9);
SWIFT_TYPEID(C, long long, 10);
SWIFT_TYPEID(C, unsigned long long, 11);
SWIFT_TYPEID(C, float, 12);
SWIFT_TYPEID(C, double, 13);
SWIFT_TYPEID(C, bool, 14);
SWIFT_TYPEID(C, decltype(nullptr), 15);
SWIFT_TYPEID(C, void, 16);

// C++ standard library types.
SWIFT_TYPEID_TEMPLATE1(C, std::vector, 100, typename T, T);

} // end namespace swift

#endif // SWIFT_BASIC_TYPEID_H
