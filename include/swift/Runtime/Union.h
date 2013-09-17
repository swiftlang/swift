//===--- Union.h - Runtime declarations for unions -------------*- C++ -*--===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift runtime functions in support of unions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_UNION_H
#define SWIFT_RUNTIME_UNION_H

namespace swift {
  
struct OpaqueValue;
struct ValueWitnessTable;
struct Metadata;
  
/// \brief Initialize the value witness table for a generic, single-payload
///        union instance.
///
/// \param vwtable - pointer to the instantiated but uninitialized value
///                  witness table for the union.
/// \param payload - type metadata for the payload case of the union.
/// \param emptyCases - the number of empty cases in the union.
extern "C" void swift_initUnionValueWitnessTableSinglePayload(
                                                    ValueWitnessTable *vwtable,
                                                    const Metadata *payload,
                                                    unsigned emptyCases);

/// \brief Return an integer value representing which case of a single-payload
///        union is inhabited.
///
/// \param value - pointer to the union value.
/// \param payload - type metadata for the payload case of the union.
/// \param emptyCases - the number of empty cases in the union.
///
/// \returns -1 if the payload case is inhabited. If an empty case is inhabited,
///          returns a value greater than or equal to zero and less than
///          emptyCases.
extern "C" int swift_getUnionCaseSinglePayload(const OpaqueValue *value,
                                               const Metadata *payload,
                                               unsigned emptyCases);

/// \brief Store the tag value for the given case.
///
/// \param value - pointer to the union value. If the case being initialized is
///                the payload case (-1), then the payload should be
///                initialized.
/// \param payload - type metadata for the payload case of the union.
/// \param whichCase - unique value identifying the case. -1 for the payload
///                    case, or a value greater than or equal to zero and less
///                    than emptyCases for an empty case.
/// \param emptyCases - the number of empty cases in the union.
extern "C" void swift_storeUnionTagSinglePayload(OpaqueValue *value,
                                                 const Metadata *payload,
                                                 int whichCase,
                                                 unsigned emptyCases);

}

#endif