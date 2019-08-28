//===--- DefineTypeIDZone.h - Define a TypeID Zone --------------*- C++ -*-===//
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
//  This file should be #included to define the TypeIDs for a given zone.
//  Two macros should be #define'd before inclusion, and will be #undef'd at
//  the end of this file:
//
//    SWIFT_TYPEID_ZONE: The ID number of the Zone being defined, which must
//    be unique. 0 is reserved for basic C and LLVM types; 255 is reserved
//    for test cases.
//
//    SWIFT_TYPEID_HEADER: A (quoted) name of the header to be
//    included to define the types in the zone.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPEID_ZONE
#  error Must define the value of the TypeID zone with the given name.
#endif

#ifndef SWIFT_TYPEID_HEADER
#  error Must define the TypeID header name with SWIFT_TYPEID_HEADER
#endif

// Define a TypeID where the type name and internal name are the same.
#define SWIFT_TYPEID(Type) SWIFT_TYPEID_NAMED(Type, Type)
#define SWIFT_REQUEST(Zone, Type) SWIFT_TYPEID_NAMED(Type, Type)

// First pass: put all of the names into an enum so we get values for them.
template<> struct TypeIDZoneTypes<Zone::SWIFT_TYPEID_ZONE> {
  enum Types : uint8_t {
#define SWIFT_TYPEID_NAMED(Type, Name) Name,
#define SWIFT_TYPEID_TEMPLATE1_NAMED(Template, Name, Param1, Arg1) Name,
#include SWIFT_TYPEID_HEADER
#undef SWIFT_TYPEID_NAMED
#undef SWIFT_TYPEID_TEMPLATE1_NAMED
  };
};

// Second pass: create specializations of TypeID for these types.
#define SWIFT_TYPEID_NAMED(Type, Name)                       \
template<> struct TypeID<Type> {                             \
  static const uint8_t zoneID =                              \
    static_cast<uint8_t>(Zone::SWIFT_TYPEID_ZONE);           \
  static const uint8_t localID =                             \
    TypeIDZoneTypes<Zone::SWIFT_TYPEID_ZONE>::Name;          \
                                                             \
  static const uint64_t value = formTypeID(zoneID, localID); \
                                                             \
  static llvm::StringRef getName() { return #Name; }         \
};

#define SWIFT_TYPEID_TEMPLATE1_NAMED(Template, Name, Param1, Arg1)    \
template<Param1> struct TypeID<Template<Arg1>> {                      \
private:                                                              \
  static const uint64_t templateID =                                  \
    formTypeID(static_cast<uint8_t>(Zone::SWIFT_TYPEID_ZONE),         \
               TypeIDZoneTypes<Zone::SWIFT_TYPEID_ZONE>::Name);       \
                                                                      \
public:                                                               \
  static const uint64_t value =                                       \
    (TypeID<Arg1>::value << 16) | templateID;                         \
                                                                      \
  static std::string getName() {                                      \
    return std::string(#Name) + "<" + TypeID<Arg1>::getName() + ">";  \
  }                                                                   \
};                                                                    \
                                                                      \
template<Param1> const uint64_t TypeID<Template<Arg1>>::value;

#include SWIFT_TYPEID_HEADER

#undef SWIFT_REQUEST

#undef SWIFT_TYPEID_NAMED
#undef SWIFT_TYPEID_TEMPLATE1_NAMED

#undef SWIFT_TYPEID
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
