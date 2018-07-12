//===--- Existential.h - Existential related Analyses. -------*- C++ //-*-===//
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

#ifndef SWIFT_SILOPTIMIZER_UTILS_EXISTENTIAL_H
#define SWIFT_SILOPTIMIZER_UTILS_EXISTENTIAL_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

/// Find InitExistential from global_addr and copy_addr.
SILValue findInitExistentialFromGlobalAddrAndCopyAddr(GlobalAddrInst *GAI,
                                                      CopyAddrInst *CAI);

/// Find InitExistential from global_addr and an apply argument.
SILValue findInitExistentialFromGlobalAddrAndApply(GlobalAddrInst *GAI,
                                                   ApplySite Apply, int ArgIdx);

/// Returns the address of an object with which the stack location \p ASI is
/// initialized. This is either a init_existential_addr or the destination of a
/// copy_addr. Returns a null value if the address does not dominate the
/// alloc_stack user \p ASIUser.
/// If the value is copied from another stack location, \p isCopied is set to
/// true.
SILValue getAddressOfStackInit(AllocStackInst *ASI, SILInstruction *ASIUser,
                               bool &isCopied);

/// Find the init_existential, which could be used to determine a concrete
/// type of the value used by \p openedUse.
/// If the value is copied from another stack location, \p isCopied is set to
/// true.
///
/// FIXME: replace all uses of this with ConcreteExistentialInfo.
SILInstruction *findInitExistential(Operand &openedUse,
                                    ArchetypeType *&OpenedArchetype,
                                    SILValue &OpenedArchetypeDef,
                                    bool &isCopied);

/// Record conformance and concrete type info derived from an init_existential
/// value that is reopened before it's use. This is useful for finding the
/// concrete type of an apply's self argument. For example, an important pattern
/// for a class existential is:
///
/// %e = init_existential_ref %c : $C : $C, $P & Q
/// %o = open_existential_ref %e : $P & Q to $@opened("PQ") P & Q
/// %r = apply %f<@opened("PQ") P & Q>(%o)
///   : $@convention(method) <τ_0_0 where τ_0_0 : P, τ_0_0 : Q>
///     (@guaranteed τ_0_0) -> @owned τ_0_0
struct ConcreteExistentialInfo {
  // The opened type passed as self. `$@opened("PQ")` above.
  // This is also the replacement type of the method's Self type.
  ArchetypeType *OpenedArchetype = nullptr;
  // The definition of the OpenedArchetype.
  SILValue OpenedArchetypeDef;
  // True if the openedValue is copied from another stack location
  bool isCopied;
  // The init_existential instruction that produces the opened existential.
  SILInstruction *InitExistential = nullptr;
  // The existential type of the self argument before it is opened,
  // produced by an init_existential.
  CanType ExistentialType;
  // The concrete type of self from the init_existential. `$C` above.
  CanType ConcreteType;
  // When ConcreteType is itself an opened existential, record the type
  // definition. May be nullptr for a valid AppliedConcreteType.
  SingleValueInstruction *ConcreteTypeDef = nullptr;
  // The Substitution map derived from the init_existential.
  // This maps a single generic parameter to the replacement ConcreteType
  // and includes the full list of existential conformances.
  // signature: <P & Q>, replacement: $C : conformances: [$P, $Q]
  SubstitutionMap ExistentialSubs;
  // The value of concrete type used to initialize the existential. `%c` above.
  SILValue ConcreteValue;

  // Search for a recognized pattern in which the given value is an opened
  // existential that was previously initialized to a concrete type.
  // Constructs a valid ConcreteExistentialInfo object if successfull.
  ConcreteExistentialInfo(Operand &openedUse);

  ConcreteExistentialInfo(ConcreteExistentialInfo &) = delete;

  bool isValid() const {
    return OpenedArchetype && OpenedArchetypeDef && InitExistential
           && ConcreteType && !ExistentialSubs.empty() && ConcreteValue;
  }

  // Do a conformance lookup on ConcreteType with the given requirement, P. If P
  // is satisfiable based on the existential's conformance, return the new
  // conformance on P. Otherwise return None.
  Optional<ProtocolConformanceRef>
  lookupExistentialConformance(ProtocolDecl *P) const {
    CanType selfTy = P->getSelfInterfaceType()->getCanonicalType();
    return ExistentialSubs.lookupConformance(selfTy, P);
  }
};

} // end namespace swift

#endif
