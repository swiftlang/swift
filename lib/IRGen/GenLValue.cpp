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
#include "RValue.h"

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
  lvalue.push<FixedAddress>(address);
  return lvalue;
}

/// Load this l-value to create an exploded r-value.
void IRGenFunction::emitExplodedLoad(const LValue &lvalue,
                                     const TypeInfo &type,
                                     Explosion &explosion) {
  // Find the addresses of the components of the stack so that we can
  // efficiently walk backwards.  This is basically just flattening
  // the recursion.  Maybe our stack should be optimized to make this
  // more reasonable.
  SmallVector<LValue::const_iterator, 16> components;
  for (auto i = lvalue.begin(), e = lvalue.end(); i != e; ++i) {
    components.push_back(i);
  }

  Address address;

  // Walk backwards through the stack.
  for (auto i = components.rbegin(), e = components.rend(); i != e; ++i) {
    const PathComponent &component = **i;

    // If this is a physical component, just compute it relative to the
    // previous component.  The address is initialized on the first pass,
    // but that's okay, because the first component should never care.
    if (component.isPhysical()) {
      address = component.asPhysical().offset(*this, address);
      continue;
    }

    // If this is the last component, load it and return that as the result.
    if (i + 1 == e)
      return component.asLogical().loadExplosion(*this, address, explosion);

    // Otherwise, load and materialize the result into memory.
    address = component.asLogical().loadAndMaterialize(*this, address);
  }

  return type.loadExplosion(*this, address, explosion);
}

typedef std::reverse_iterator<LValue::const_iterator*> path_iterator;

/// Perform a store into the given path, given the base of the first
/// component.
static void emitStoreRecursive(IRGenFunction &IGF, Address base,
                               const TypeInfo &finalType,
                               Explosion &finalValue,
                               path_iterator pathStart,
                               path_iterator pathEnd) {
  // Drill into any physical components.
  while (true) {
    assert(pathStart != pathEnd);

    const PathComponent &component = **pathStart;
    if (component.isLogical()) break;
    base = component.asPhysical().offset(IGF, base);

    // If we reach the end, do a simple store and we're done.
    if (++pathStart == pathEnd) {
      return finalType.storeExplosion(IGF, finalValue, base);
    }
  }

  // Okay, we have a logical component.
  assert(pathStart != pathEnd);
  const LogicalPathComponent &component = (*pathStart)->asLogical();
  
  // If this is the final component, just do a logical store.
  if (pathStart + 1 == pathEnd) {
    return component.storeExplosion(IGF, finalValue, base);
  }

  // Otherwise, load and materialize into a temporary.
  Address temp = component.loadAndMaterialize(IGF, base);

  // Recursively perform the store.
  emitStoreRecursive(IGF, temp, finalType, finalValue, pathStart + 1, pathEnd);

  // Store the temporary back.
  component.storeMaterialized(IGF, temp, base);
}
                           

void IRGenFunction::emitStore(Explosion &rvalue, const LValue &lvalue,
                              const TypeInfo &type) {

  // Find the addresses of the components of the stack so that we can
  // efficiently walk backwards.  This is basically just flattening
  // the recursion.  Maybe our stack should be optimized to make this
  // more reasonable.
  SmallVector<LValue::const_iterator, 16> components;
  for (auto i = lvalue.begin(), e = lvalue.end(); i != e; ++i) {
    components.push_back(i);
  }

  emitStoreRecursive(*this, Address(), type, rvalue,
                     components.rbegin(), components.rend());
}

/// Given an l-value which is known to be physical, load from it.
Address IRGenFunction::emitAddressForPhysicalLValue(const LValue &lvalue) {
  SmallVector<LValue::const_iterator, 16> components;
  for (auto i = lvalue.begin(), e = lvalue.end(); i != e; ++i) {
    components.push_back(i);
  }

  Address address;
  for (auto i = components.rbegin(), e = components.rend(); i != e; ++i) {
    address = (*i)->asPhysical().offset(*this, address);
  }
  return address;
}

void IRGenFunction::emitLValueAsScalar(const LValue &lvalue,
                                       Explosion &explosion) {
  SmallVector<LValue::const_iterator, 16> components;
  for (auto i = lvalue.begin(), e = lvalue.end(); i != e; ++i) {
    components.push_back(i);
  }

  Address address;
  for (auto i = components.rbegin(), e = components.rend(); i != e; ++i) {
    if ((*i)->isLogical())
      address = (*i)->asLogical().loadAndMaterialize(*this, address);
    else
      address = (*i)->asPhysical().offset(*this, address);
  }

  // FIXME: writebacks
  // FIXME: rematerialize if inadequate alignment
  explosion.add(address.getAddress());
}

void IRGenFunction::emitAssignment(Expr *rhs, const LValue &lhs,
                                   const TypeInfo &type) {
  // We don't expect that l-value emission is generally going to admit
  // maximally-exploded calls.
  Explosion explosion(ExplosionKind::Minimal);
  emitExplodedRValue(rhs, explosion);
  emitStore(explosion, lhs, type);
}
