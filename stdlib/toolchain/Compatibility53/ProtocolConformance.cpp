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
// Checking of Swift protocol conformances.
//
// This implementation is intended to be backward-deployed into Swift 5.3 and
// later runtimes.
//
//===----------------------------------------------------------------------===//

#include "Overrides.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/BuiltinProtocolConformances.h"
#include <dlfcn.h>
#include <mach-o/dyld.h>

using namespace swift;

static const ProtocolDescriptor *getEquatableDescriptor() {
  auto descriptor = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<const ProtocolDescriptor *>(
                     dlsym(RTLD_DEFAULT, "$sSQMp")));
  return descriptor;
}

static const ProtocolDescriptor *getComparableDescriptor() {
  auto descriptor = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<const ProtocolDescriptor *>(
                     dlsym(RTLD_DEFAULT, "$sSLMp")));
  return descriptor;
}

static const WitnessTable *conformsToProtocol(const Metadata *type,
                                        const ProtocolDescriptor *protocol) {
  using Fn = const WitnessTable *(const Metadata *, const ProtocolDescriptor *);
  auto func = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<Fn *>(
      dlsym(RTLD_DEFAULT, "swift_conformsToProtocol")));
  return func(type, protocol);
}

static bool tupleConformsToProtocol(const Metadata *type,
                                    const ProtocolDescriptor *protocol) {
  auto tuple = cast<TupleTypeMetadata>(type);

  // At the moment, tuples can only conform to Equatable and Comparable, so
  // reject all other protocols.
  if (protocol != getEquatableDescriptor() &&
      protocol != getComparableDescriptor())
    return false;

  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);
    if (!conformsToProtocol(elt.Type, protocol))
      return false;
  }

  return true;
}

extern const WitnessTable _swift_tupleEquatable_wt;
extern const WitnessTable _swift_tupleComparable_wt;

static const WitnessTable *getTupleConformanceWitnessTable(
                                           const ProtocolDescriptor *protocol) {
  if (protocol == getEquatableDescriptor())
    return &_swift_tupleEquatable_wt;

  if (protocol == getComparableDescriptor())
    return &_swift_tupleComparable_wt;

  return nullptr;
}

const WitnessTable *
swift::swift53override_conformsToProtocol(const Metadata *type,
  const ProtocolDescriptor *protocol,
  ConformsToProtocol_t *original_conformsToProtocol)
{
  // Swift 5.4 introduces tuple Equatable conformance, so ensure that Swift 5.3
  // and later runtimes can handle this as well.
  if (auto tuple = dyn_cast<TupleTypeMetadata>(type)) {
    if (!tupleConformsToProtocol(type, protocol))
      return nullptr;

    return getTupleConformanceWitnessTable(protocol);
  }

  auto result = original_conformsToProtocol(type, protocol);
  if (result)
    return result;

  return nullptr;
}
