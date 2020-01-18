//===--- ProtocolConformance.cpp - Swift protocol conformance checking ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Checking and caching of Swift protocol conformances.
//
// This implementation is intended to be backward-deployed into Swift 5.1
// runtimes.
//
//===----------------------------------------------------------------------===//

#include "Overrides.h"
#include "../../public/runtime/Private.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/BuiltinProtocolConformances.h"
#include "swift/Runtime/Casting.h"
#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#include <objc/runtime.h>

using namespace swift;

extern const ProtocolDescriptor
PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING);

static bool tupleConformsToProtocol(const Metadata *type,
                                    const ProtocolDescriptor *protocol) {
  auto tuple = cast<TupleTypeMetadata>(type);

  // At the moment, tuples can only conform to Equatable, so reject all other
  // protocols.
  if (protocol != &PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING))
    return false;

  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);
    if (!swift51override_conformsToProtocol(elt.Type, protocol,
                                            swift_conformsToProtocol))
      return false;
  }

  return true;
}

static const WitnessTable *getTupleConformanceWitnessTable(
                                           const ProtocolDescriptor *protocol) {
  if (protocol == &PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING)) {
    return reinterpret_cast<const WitnessTable *>(
      &BUILTIN_PROTOCOL_WITNESS_TABLE_SYM(VARIADIC_TUPLE_MANGLING,
                                          SWIFT_EQUATABLE_MANGLING));
  }

  return nullptr;
}

const WitnessTable *
swift::swift51override_conformsToProtocol(const Metadata *type,
  const ProtocolDescriptor *protocol,
  ConformsToProtocol_t *original_conformsToProtocol)
{
  auto result = original_conformsToProtocol(type, protocol);
  if (result)
    return result;

  // Swift 5.2 introduces tuple Equatable conformance, so ensure that Swift 5.1
  // runtime can handle this as well.
  if (auto tuple = dyn_cast<TupleTypeMetadata>(type)) {
    if (!tupleConformsToProtocol(type, protocol))
      return nullptr;

    return getTupleConformanceWitnessTable(protocol);
  }

  return nullptr;
}
