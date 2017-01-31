//===--- Enum.h - Runtime declarations for enums ----------------*- C++ -*-===//
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
// Swift runtime functions in support of enums.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_ENUM_H
#define SWIFT_RUNTIME_ENUM_H

#include "swift/Runtime/Config.h"

namespace swift {
  
struct OpaqueValue;
struct ValueWitnessTable;
  
struct InProcess;

template <typename Runtime> struct TargetMetadata;
using Metadata = TargetMetadata<InProcess>;

template <typename Runtime> struct TargetEnumMetadata;
using EnumMetadata = TargetEnumMetadata<InProcess>;
struct TypeLayout;

/// \brief Initialize the value witness table for a generic, single-payload
///        enum instance.
///
/// \param vwtable - pointer to the instantiated but uninitialized value
///                  witness table for the enum.
/// \param payload - type metadata for the payload case of the enum.
/// \param emptyCases - the number of empty cases in the enum.
SWIFT_RUNTIME_EXPORT
void swift_initEnumValueWitnessTableSinglePayload(ValueWitnessTable *vwtable,
                                                  const TypeLayout *payload,
                                                  unsigned emptyCases);

/// \brief Faster variant of the above which avoids digging into the enum type
/// metadata when the caller already has the payload information handy.
///
/// \param value - pointer to the enum value.
/// \param payload - type metadata for the payload case of the enum.
/// \param emptyCases - the number of empty cases in the enum.
///
/// \returns -1 if the payload case is inhabited. If an empty case is inhabited,
///          returns a value greater than or equal to zero and less than
///          emptyCases.
SWIFT_RT_ENTRY_VISIBILITY
int swift_getEnumCaseSinglePayload(const OpaqueValue *value,
                                   const Metadata *payload,
                                   unsigned emptyCases)
  SWIFT_CC(RegisterPreservingCC);



/// \brief Store the tag value for the given case into a single-payload enum,
///        whose associated payload (if any) has already been initialized.
///
/// \param value - pointer to the enum value. If the case being initialized is
///                the payload case (-1), then the payload should be
///                initialized.
/// \param payload - type metadata for the payload case of the enum.
/// \param whichCase - unique value identifying the case. -1 for the payload
///                    case, or a value greater than or equal to zero and less
///                    than emptyCases for an empty case.
/// \param emptyCases - the number of empty cases in the enum.
SWIFT_RT_ENTRY_VISIBILITY
void swift_storeEnumTagSinglePayload(OpaqueValue *value,
                                     const Metadata *payload,
                                     int whichCase,
                                     unsigned emptyCases)
  SWIFT_CC(RegisterPreservingCC);

/// \brief Initialize the value witness table for a generic, multi-payload
///        enum instance.
SWIFT_RUNTIME_EXPORT
void swift_initEnumMetadataMultiPayload(ValueWitnessTable *vwtable,
                                        EnumMetadata *enumType,
                                        unsigned numPayloads,
                                        const TypeLayout * const *payloadTypes);

/// \brief Return an integer value representing which case of a multi-payload
///        enum is inhabited.
///
/// \param value - pointer to the enum value.
/// \param enumType - type metadata for the enum.
///
/// \returns The index of the enum case.
SWIFT_RUNTIME_EXPORT
unsigned swift_getEnumCaseMultiPayload(const OpaqueValue *value,
                                       const EnumMetadata *enumType);
  
/// \brief Store the tag value for the given case into a multi-payload enum,
///        whose associated payload (if any) has already been initialized.
SWIFT_RUNTIME_EXPORT
void swift_storeEnumTagMultiPayload(OpaqueValue *value,
                                    const EnumMetadata *enumType,
                                    unsigned whichCase);

}

#endif
