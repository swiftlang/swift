//===--- ImplementTypeIDZone.h - Implement a TypeID Zone --------*- C++ -*-===//
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
//  This file should be #included to implement the TypeIDs for a given zone
//  in a C++ file.
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
#define SWIFT_REQUEST(Zone, Type, Sig, Caching, LocOptions) SWIFT_TYPEID_NAMED(Type, Type)
#define SWIFT_TYPEID(Type) SWIFT_TYPEID_NAMED(Type, Type)

// Out-of-line definitions.
#define SWIFT_TYPEID_NAMED(Type, Name)            \
  const uint64_t TypeID<Type>::value;

#define SWIFT_TYPEID_TEMPLATE1_NAMED(Template, Name, Param1, Arg1)

#include SWIFT_TYPEID_HEADER

#undef SWIFT_REQUEST

#undef SWIFT_TYPEID_NAMED
#undef SWIFT_TYPEID_TEMPLATE1_NAMED

#undef SWIFT_TYPEID
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
