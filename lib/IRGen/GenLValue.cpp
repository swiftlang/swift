//===--- GenLValue.cpp - IR Generation for Operations on L-Values ---------===//
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
//  This file implements IR generation for store and, conceivably,
//  compound store operations on l-values.
//
//===----------------------------------------------------------------------===//

#include "GenType.h"
#include "IRGenFunction.h"
#include "Explosion.h"
#include "LValue.h"

using namespace swift;
using namespace irgen;

void PathComponent::_anchor() {}
void LogicalPathComponent::_anchor() {}
void PhysicalPathComponent::_anchor() {}

namespace {
  /// A path component with a fixed address.
  class FixedAddress : public PhysicalPathComponent {
    Address Addr;

  public:
    FixedAddress(Address addr)
      : PhysicalPathComponent(sizeof(FixedAddress)), Addr(addr) {}

    Address offset(IRGenFunction &IGF, Address base) const {
      assert(!base.isValid());
      return Addr;
    }
  };
}

/// Create an l-value which resolves exactly to the given address.
LValue IRGenFunction::emitAddressLValue(Address address) {
  LValue lvalue;
  lvalue.add<FixedAddress>(address);
  return lvalue;
}

/// Load this l-value to create an exploded r-value.
void IRGenFunction::emitExplodedLoad(const LValue &lvalue,
                                     const TypeInfo &type,
                                     Explosion &explosion) {
  Address address;

  for (auto i = lvalue.begin(), e = lvalue.end(); i != e; ) {
    const PathComponent &component = *i++;

    // If this is a physical component, just compute it relative to the
    // previous component.  The address is initialized on the first pass,
    // but that's okay, because the first component should never care.
    if (component.isPhysical()) {
      address = component.asPhysical().offset(*this, address);
      continue;
    }

    // If this is the last component, load it and return that as the result.
    if (i == e)
      return component.asLogical().loadExplosion(*this, address, explosion);

    // Otherwise, load and materialize the result into memory.
    address = component.asLogical().loadAndMaterialize(*this, address);
  }

  return type.load(*this, address, explosion);
}

/// Perform a store into the given path, given the base of the first
/// component.
static void emitAssignRecursive(IRGenFunction &IGF, Address base,
                                const TypeInfo &finalType,
                                Explosion &finalValue,
                                LValue::const_iterator pathStart,
                                LValue::const_iterator pathEnd) {
  // Drill into any physical components.
  while (true) {
    assert(pathStart != pathEnd);

    const PathComponent &component = *pathStart;
    if (component.isLogical()) break;
    base = component.asPhysical().offset(IGF, base);

    // If we reach the end, do an assignment and we're done.
    if (++pathStart == pathEnd) {
      return finalType.assign(IGF, finalValue, base);
    }
  }

  // Okay, we have a logical component.
  assert(pathStart != pathEnd);
  const LogicalPathComponent &component = pathStart->asLogical();
  ++pathStart;
  
  // If this is the final component, just do a logical store.
  if (pathStart == pathEnd) {
    return component.storeExplosion(IGF, finalValue, base);
  }

  // Otherwise, load and materialize into a temporary.
  Address temp = component.loadAndMaterialize(IGF, base);

  // Recursively perform the store.
  emitAssignRecursive(IGF, temp, finalType, finalValue, pathStart, pathEnd);

  // Store the temporary back.
  component.storeMaterialized(IGF, temp, base);
}
                           

void IRGenFunction::emitAssign(Explosion &rvalue, const LValue &lvalue,
                              const TypeInfo &type) {
  emitAssignRecursive(*this, Address(), type, rvalue,
                      lvalue.begin(), lvalue.end());
}

/// Given an l-value which is known to be physical, load from it.
Address IRGenFunction::emitAddressForPhysicalLValue(const LValue &lvalue) {
  Address address;
  for (auto &component : lvalue) {
    address = component.asPhysical().offset(*this, address);
  }
  return address;
}

void IRGenFunction::emitLValueAsScalar(const LValue &lvalue,
                                       Explosion &explosion) {
  Address address;
  for (auto &component : lvalue) {
    if (component.isLogical())
      address = component.asLogical().loadAndMaterialize(*this, address);
    else
      address = component.asPhysical().offset(*this, address);
  }

  // FIXME: writebacks
  // FIXME: rematerialize if inadequate alignment
  explosion.add(address.getAddress());
}

void IRGenFunction::emitAssign(Expr *rhs, const LValue &lhs,
                                   const TypeInfo &type) {
  // We don't expect that l-value emission is generally going to admit
  // maximally-exploded calls.
  Explosion explosion(ExplosionKind::Minimal);
  emitExplodedRValue(rhs, explosion);
  emitAssign(explosion, lhs, type);
}
