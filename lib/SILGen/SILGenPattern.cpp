//===--- SILGenPattern.cpp - Pattern matching codegen ---------------------===//
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

#define DEBUG_TYPE "switch-silgen"
#include "SILGen.h"
#include "Scope.h"
#include "Cleanup.h"
#include "Initialization.h"
#include "RValue.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FormattedStream.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

/// Shallow-dump a pattern node one level deep for debug purposes.
static void dumpPattern(const Pattern *p, llvm::raw_ostream &os) {
  if (!p) {
    // We use null to represent a synthetic wildcard.
    os << '_';
    return;
  }
  p = p->getSemanticsProvidingPattern();
  switch (p->getKind()) {
  case PatternKind::Any:
    os << '_';
    return;
  case PatternKind::Expr:
    os << "<expr>";
    return;
  case PatternKind::Named:
    os << "var " << cast<NamedPattern>(p)->getBoundName();
    return;
  case PatternKind::Tuple: {
    unsigned numFields = cast<TuplePattern>(p)->getNumFields();
    if (numFields == 0)
      os << "()";
    else if (numFields == 1)
      os << "(_)";
    else {
      os << '(';
      for (unsigned i = 0; i < numFields - 1; ++i)
        os << ',';
      os << ')';
    }
    return;
  }
  case PatternKind::Isa:
    os << "is ";
    cast<IsaPattern>(p)->getCastTypeLoc().getType()->print(os);
    break;
  case PatternKind::NominalType: {
    auto np = cast<NominalTypePattern>(p);
    np->getType()->print(os);
    os << '(';
    interleave(np->getElements(),
               [&](const NominalTypePattern::Element &elt) {
                 os << elt.getProperty()->getName() << ":";
               },
               [&]{ os << ", "; });
    os << ')';
    return;
  }
  case PatternKind::EnumElement: {
    auto eep = cast<EnumElementPattern>(p);
    os << '.' << eep->getName();
    return;
  }
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    llvm_unreachable("not semantic");
  }
}

static void dumpDepth(unsigned depth, llvm::raw_ostream &os) {
  for (unsigned d = 0; d < depth; ++d)
    os << "| ";
}

/// True if a pattern is a wildcard, meaning it matches any value. '_' and
/// variable patterns are wildcards. We also consider ExprPatterns to be
/// wildcards; we test the match expression as a guard outside of the normal
/// pattern clause matrix. When destructuring wildcard patterns, we also use
/// nullptr to represent newly-constructed wildcards.
static bool isWildcardPattern(const Pattern *p) {
  if (!p)
    return true;

  switch (p->getKind()) {
  // Simple wildcards.
  case PatternKind::Any:
  case PatternKind::Expr:
  case PatternKind::Named:
    return true;
  
  // Non-wildcards.
  case PatternKind::Tuple:
  case PatternKind::Isa:
  case PatternKind::NominalType:
  case PatternKind::EnumElement:
    return false;
  
  // Recur into simple wrapping patterns.
  case PatternKind::Paren:
    return isWildcardPattern(cast<ParenPattern>(p)->getSubPattern());
  case PatternKind::Typed:
    return isWildcardPattern(cast<TypedPattern>(p)->getSubPattern());
  case PatternKind::Var:
    return isWildcardPattern(cast<VarPattern>(p)->getSubPattern());
  }
}

/// True if a pattern is a simple variable binding.
static bool isBindingPattern(const Pattern *p) {
  return p && isa<NamedPattern>(p->getSemanticsProvidingPattern());
}

namespace {
  /// A pair representing a specialization of a clause matrix.
  struct SpecializingPattern {
    /// The pattern node representing the specialization.
    const Pattern *pattern;
    /// The number of rows to skip in the specialization.
    unsigned row;
  };

  /// Typedef for the vector of basic blocks and destructured values produced by
  /// emitDispatchAndDestructure.
  using DispatchedPatternVector
    = std::vector<std::pair<SILBasicBlock*, std::vector<SILValue>>>;
}

/// Load a computed property from a reference type.
static SILValue emitGetComputedPropertyFromRefTypeRValue(SILGenFunction &gen,
                                                         SILLocation loc,
                                                         SILValue aggregate,
                                                         VarDecl *property,
                                                         CanType propTy) {
  // FIXME: class, class archetype, and class protocol type properties
  llvm_unreachable("not implemented");
}

/// Load a stored property from a reference type (in other words a class).
static SILValue emitGetStoredPropertyFromRefTypeRValue(SILGenFunction &gen,
                                                       SILLocation loc,
                                                       SILValue aggregate,
                                                       VarDecl *property,
                                                       CanType propTy) {
  assert(aggregate.getType().getClassOrBoundGenericClass());
  auto &propTL = gen.getTypeLowering(propTy);
  SILValue addr = gen.B.createRefElementAddr(loc, aggregate, property,
                                     propTL.getLoweredType().getAddressType());
  return gen.emitLoad(loc, addr, propTL, SGFContext(), IsNotTake).forward(gen);
}

/// Load a computed property from a value type.
static SILValue emitGetComputedPropertyFromValueTypeRValue(SILGenFunction &gen,
                                                           SILLocation loc,
                                                           SILValue aggregate,
                                                           VarDecl *property,
                                                           CanType propTy) {
  // FIXME: struct, archetype, and protocol type properties
  llvm_unreachable("not implemented");
}

/// Load a stored property from a value type (in other words a struct).
static SILValue emitGetStoredPropertyFromValueTypeRValue(SILGenFunction &gen,
                                                         SILLocation loc,
                                                         SILValue aggregate,
                                                         VarDecl *property,
                                                         CanType propTy) {
  assert(aggregate.getType().getStructOrBoundGenericStruct());
  auto &propTL = gen.getTypeLowering(propTy);
  if (aggregate.getType().isAddress()) {
    // Load from an address-only struct.
    SILValue addr = gen.B.createStructElementAddr(loc, aggregate, property,
                                      propTL.getLoweredType().getAddressType());
    return gen.emitLoad(loc, addr, propTL, SGFContext(), IsNotTake)
      .forward(gen);
  } else {
    // Extract from a loadable struct.
    SILValue field = gen.B.createStructExtract(loc, aggregate, property,
                                               propTL.getLoweredType());
    // FIXME: Make unowned field strong.
    return gen.B.createCopyValue(loc, field);
  }
}

/// Load a property from a struct or class rvalue as an independent rvalue.
/// Does not consume the source aggregate.
static SILValue emitGetPropertyFromRValue(SILGenFunction &gen,
                                          SILLocation loc,
                                          SILValue aggregate,
                                          VarDecl *property,
                                          CanType propTy) {
  if (aggregate.getType().hasReferenceSemantics()) {
    if (property->isComputed())
      return emitGetComputedPropertyFromRefTypeRValue(gen, loc, aggregate,
                                                      property, propTy);
    return emitGetStoredPropertyFromRefTypeRValue(gen, loc, aggregate,
                                                  property, propTy);
  }
  
  if (property->isComputed())
    return emitGetComputedPropertyFromValueTypeRValue(gen, loc, aggregate,
                                                      property, propTy);
  return emitGetStoredPropertyFromValueTypeRValue(gen, loc, aggregate,
                                                  property, propTy);
}

/// Emit a conditional branch testing if a value matches one of the given
/// pattern nodes.
/// In the case branch for each pattern, destructure the value.
/// On return, the insertion point is cleared.
/// The parts of the value used for dispatch are conceptually consumed, that is,
/// it should be correct to destroy only the destructured values on each branch
/// in order to completely destroy the original subject value.
///
/// \returns null if the set of pattern nodes match every possible value of the
/// type, or else the "default" basic block for the dispatch that will be
/// branched to if no patterns match.
static SILBasicBlock *emitDispatchAndDestructure(SILGenFunction &gen,
                                        ArrayRef<SpecializingPattern> patterns,
                                        SILValue v,
                                        DispatchedPatternVector &dispatches,
                                        SwitchStmt *stmt) {
  assert(!patterns.empty() && "no patterns to dispatch on?!");
  
  const Pattern *headPattern = patterns[0].pattern;
  PatternKind kind = headPattern->getSemanticsProvidingPattern()->getKind();
  CanType type = headPattern->getType()->getCanonicalType();
  
  switch (kind) {
  case PatternKind::Any:
  case PatternKind::Named:
  case PatternKind::Expr:
    llvm_unreachable("wildcards shouldn't get here");

  case PatternKind::Tuple: {
    // Tuples are irrefutable; destructure without branching.
    assert(patterns.size() == 1 && "pattern orthogonal to tuple?!");
    auto *tp = cast<TuplePattern>(headPattern);
    RegularLocation Loc(const_cast<TuplePattern*>(tp));

    std::vector<SILValue> destructured;

    auto tupleTy = tp->getType()->castTo<TupleType>();
    auto tupleSILTy = gen.getLoweredType(tupleTy);
    if (tupleSILTy.isAddressOnly(gen.F.getModule())) {
      for (unsigned i = 0, e = tupleTy->getFields().size(); i < e; ++i) {
        SILType fieldTy = gen.getLoweredType(tupleTy->getElementType(i));
        SILValue member = gen.B.createTupleElementAddr(Loc,
                                             v, i, fieldTy.getAddressType());
        if (!fieldTy.isAddressOnly(gen.F.getModule()))
          member = gen.B.createLoad(Loc, member);
        destructured.push_back(member);
      }
    } else {
      for (unsigned i = 0, e = tupleTy->getFields().size(); i < e; ++i) {
        auto fieldType = tupleTy->getElementType(i);
        SILType fieldTy = gen.getLoweredLoadableType(fieldType);
        SILValue member = gen.B.createTupleExtract(Loc, v, i, fieldTy);
        destructured.push_back(member);
      }
    }
    
    dispatches.emplace_back(gen.B.getInsertionBB(), std::move(destructured));
    gen.B.clearInsertionPoint();
    return nullptr;
  }
      
  case PatternKind::Isa: {
    auto &origTL = gen.getTypeLowering(headPattern->getType());
    auto castTLs
      = map<SmallVector<const TypeLowering *,4>>(patterns,
          [&](const SpecializingPattern &p) {
            return &gen.getTypeLowering(
              cast<IsaPattern>(p.pattern)->getCastTypeLoc().getType());
          });
    
    
    /// Emit an abstraction change if any of the casts will require it.
    SILValue vAbstract
      = gen.emitCheckedCastAbstractionChange(const_cast<Pattern*>(headPattern),
                                             v, origTL, castTLs);
    
    /// Emit all of the 'is' checks.
    unsigned i = 0;
    for (SpecializingPattern p : patterns) {
      auto *ip = cast<IsaPattern>(p.pattern);
      RegularLocation Loc(const_cast<IsaPattern*>(ip));

      std::vector<SILValue> destructured;
      
      // Perform a conditional cast branch.
      SILBasicBlock *trueBB, *falseBB;
      std::tie(trueBB, falseBB) = gen.emitCheckedCastBranch(Loc,
                                        v, vAbstract,
                                        origTL, *castTLs[i],
                                        ip->getCastKind());
      
      // On the true branch, we can get the cast value from the BB argument.
      SILValue cast = trueBB->bbarg_begin()[0];
      // If the cast result is loadable and we cast a value address, load it.
      if (cast.getType().isAddress()
          && !cast.getType().isAddressOnly(gen.F.getModule())) {
        gen.B.setInsertionPoint(trueBB);
        cast = gen.B.createLoad(Loc, cast);
        gen.B.clearInsertionPoint();
      }
      destructured.push_back(cast);
      
      // FIXME: If we cast from an opaque existential, we'll continue using its
      // contained value, but we need to deallocate the existential husk.
      
      // Code matching the pattern goes into the "true" block.
      dispatches.emplace_back(trueBB, std::move(destructured));
      
      // Dispatch continues on the "false" block.
      gen.B.emitBlock(falseBB);
      
      ++i;
    }

    // The current block is now the "default" block.
    SILBasicBlock *defaultBB = gen.B.getInsertionBB();
    gen.B.clearInsertionPoint();
    return defaultBB;
  }
      
  case PatternKind::EnumElement: {
    /// We'll want to know if we matched every case of the enum to see if we
    /// need a default block.
    ///
    /// FIXME: If the enum is resilient, then we always need a default block.
    llvm::DenseSet<EnumElementDecl*> unmatchedCases;
    type->getEnumOrBoundGenericEnum()->getAllElements(unmatchedCases);
    
    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 4> caseBBs;
    
    bool addressOnlyEnum = v.getType().isAddress();
    
    SILValue voidValue;
    
    SILBasicBlock *bb = gen.B.getInsertionBB();
    
    for (SpecializingPattern p : patterns) {
      auto *up = cast<EnumElementPattern>(p.pattern);
      RegularLocation Loc(const_cast<EnumElementPattern*>(up));

      EnumElementDecl *elt = up->getElementDecl();
      
      assert(unmatchedCases.count(elt)
             && "specializing same enum case twice?!");
      unmatchedCases.erase(elt);
      
      SILBasicBlock *caseBB = gen.createBasicBlock();
      
      // Create a BB argument to receive the enum case data if it has any.
      SILValue eltValue;
      if (elt->hasArgumentType()) {
        auto argSwiftTy = v.getType().getSwiftRValueType()
          ->getTypeOfMember(elt->getModuleContext(), elt, nullptr,
                            elt->getArgumentType());
        if (!argSwiftTy->isVoid()) {
          
          auto &argLowering = gen.getTypeLowering(argSwiftTy);
          SILType argTy = argLowering.getLoweredType();
          if (addressOnlyEnum)
            argTy = argTy.getAddressType();
          eltValue = new (gen.F.getModule()) SILArgument(argTy, caseBB);
          // Load a loadable data value from an address-only enum.
          if (addressOnlyEnum && argLowering.isLoadable()) {
            gen.B.setInsertionPoint(caseBB);
            eltValue = gen.B.createLoad(Loc, eltValue);
            gen.B.setInsertionPoint(bb);
          }
        }
      } else {
        // If the element pattern for a void enum element has a subpattern, it
        // will bind to a void value.
        if (!voidValue)
          voidValue = gen.emitEmptyTuple(Loc);
        eltValue = voidValue;
      }
      
      caseBBs.push_back({elt, caseBB});
      dispatches.emplace_back(caseBB, std::vector<SILValue>(1, eltValue));
    }
    
    SILBasicBlock *defaultBB = nullptr;
    
    // If we didn't cover every case, then we need a default block.
    if (!unmatchedCases.empty())
      defaultBB = gen.createBasicBlock();
    
    // Emit the switch instruction.
    if (addressOnlyEnum) {
      gen.B.createDestructiveSwitchEnumAddr(stmt, v,
                                             defaultBB, caseBBs);
    } else {
      gen.B.createSwitchEnum(stmt, v, defaultBB, caseBBs);
    }
    
    // Return the default BB.
    return defaultBB;
  }
      
  case PatternKind::NominalType: {
    // Nominal type patterns are irrefutable; destructure without branching.
    auto *np = cast<NominalTypePattern>(headPattern);
    RegularLocation loc(const_cast<NominalTypePattern*>(np));
    
    // Copy out the needed property values.
    std::vector<SILValue> destructured;
    
    for (auto &elt : np->getElements()) {
      destructured.push_back(emitGetPropertyFromRValue(gen, loc, v,
                           elt.getProperty(),
                           elt.getSubPattern()->getType()->getCanonicalType()));
    }
    
    // Carry the aggregate forward. It may be needed to match against other
    // pattern kinds.
    destructured.push_back(v);
    
    dispatches.emplace_back(gen.B.getInsertionBB(), std::move(destructured));
    gen.B.clearInsertionPoint();
    return nullptr;
  }
    
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    llvm_unreachable("pattern node is never semantic");
  }
}

/// Destructure a pattern parallel to the specializing pattern of a clause
/// matrix.
/// Destructured wildcards are represented with null Pattern* pointers.
/// p must be non-orthogonal to this pattern constructor.
static void destructurePattern(SILGenFunction &gen,
                               const Pattern *specializer,
                               const Pattern *p,
                               SmallVectorImpl<const Pattern *> &destructured) {
  specializer = specializer->getSemanticsProvidingPattern();
  assert(p && !isWildcardPattern(p) &&
         "wildcard patterns shouldn't be passed here");
  p = p->getSemanticsProvidingPattern();
  switch (specializer->getKind()) {
  case PatternKind::Any:
  case PatternKind::Named:
  case PatternKind::Expr:
    llvm_unreachable("shouldn't specialize on a wildcard pattern");

  case PatternKind::Tuple: {
    auto specializerTuple = cast<TuplePattern>(specializer);
    
    // Tuples should only match with other tuple patterns. Destructure into
    // the tuple fields.
    auto tp = cast<TuplePattern>(p);
    assert(specializerTuple->getFields().size() == tp->getFields().size()
           && "tuple patterns do not share shape");
    (void) specializerTuple;
    std::transform(tp->getFields().begin(), tp->getFields().end(),
                   std::back_inserter(destructured),
                   [&](const TuplePatternElt &e) -> const Pattern * {
                     return e.getPattern();
                   });
    return;
  }
      
  case PatternKind::Isa: {
    auto *specializerIsa = cast<IsaPattern>(specializer);
    auto *ip = cast<IsaPattern>(p);
    CanType newFromType
      = specializerIsa->getCastTypeLoc().getType()->getCanonicalType();
    CanType newToType = ip->getCastTypeLoc().getType()->getCanonicalType();
    
    // If a cast pattern casts to the same type, it reduces to a wildcard.
    // FIXME: 'is' patterns should have a subpattern, in which case they
    // destructure to that pattern in this case.
    if (newFromType == newToType) {
      destructured.push_back(nullptr);
      return;
    }
    
    // If the cast pattern casts to a superclass, it reduces to a wildcard.
    // FIXME: 'is' patterns should have a subpattern, in which case they
    // destructure to that pattern in this case. We'd need to arrange for the
    // value to be upcast.
    if (newToType->isSuperclassOf(newFromType, nullptr)) {
      destructured.push_back(nullptr);
      return;
    }

    // If a cast pattern is non-orthogonal and not to the same type, then
    // we have a cast to a superclass, subclass, or archetype. Produce a
    // new checked cast pattern from the destructured type.
    
    CheckedCastKind newKind;
    // Determine the new cast kind.
    bool fromArchetype = isa<ArchetypeType>(newFromType),
         toArchetype = isa<ArchetypeType>(newToType);
    if (fromArchetype && toArchetype) {
      newKind = CheckedCastKind::ArchetypeToArchetype;
    } else if (fromArchetype) {
      newKind = CheckedCastKind::ArchetypeToConcrete;
    } else if (toArchetype) {
      if (newFromType->isExistentialType()) {
        newKind = CheckedCastKind::ExistentialToArchetype;
      } else if (newFromType->isSuperclassOf(newToType, nullptr)) {
        newKind = CheckedCastKind::SuperToArchetype;
      } else {
        newKind = CheckedCastKind::ConcreteToArchetype;
      }
    } else {
      // We have a class-to-class downcast.
      assert(newFromType->getClassOrBoundGenericClass() &&
             newToType->getClassOrBoundGenericClass() &&
             "non-class, non-archetype cast patterns should be orthogonal!");
      newKind = CheckedCastKind::Downcast;
    }
    
    // Create the new cast pattern.
    auto *newIsa
      = new (gen.F.getASTContext()) IsaPattern(p->getLoc(),
                                               ip->getCastTypeLoc(),
                                               newKind);
    newIsa->setType(newFromType);
    
    destructured.push_back(newIsa);
    return;
  }
      
  case PatternKind::EnumElement: {
    auto *up = cast<EnumElementPattern>(p);
    
    // If the enum case has a value, but the pattern does not specify a
    // subpattern, then treat it like a wildcard.
    if (!up->hasSubPattern())
      destructured.push_back(nullptr);
    else
      destructured.push_back(up->getSubPattern());
    
    return;
  }
      
  case PatternKind::NominalType: {
    auto *specializerNP = cast<NominalTypePattern>(specializer);

    // If we're specializing another nominal type pattern, break out
    // its subpatterns.
    if (auto *np = dyn_cast<NominalTypePattern>(p)) {
      assert(np->getType()->isEqual(specializerNP->getType()));
      
      // Extract the property subpatterns in specializer order. If a property
      // isn't mentioned in the specializee, it's a wildcard.
      size_t base = destructured.size();
      // Extend the destructured vector with all wildcards.
      // The +1 is for the full aggregate itself, which we don't need to
      // match.
      destructured.append(specializerNP->getElements().size() + 1, nullptr);
      // Figure out the offsets for all the fields from the specializer.
      llvm::DenseMap<VarDecl*, size_t> offsets;
      size_t i = base;
      for (auto &specElt : specializerNP->getElements()) {
        offsets.insert({specElt.getProperty(), i});
        ++i;
      }
      
      // Drop in the subpatterns from the specializee.
      for (auto &elt : np->getElements()) {
        assert(offsets.count(elt.getProperty()));
        destructured[offsets[elt.getProperty()]] = elt.getSubPattern();
      }
    
      return;
    }
    
    // For any other kind of pattern, match it against the original aggregate,
    // which is placed after all of the extracted properties of the specializer.
    destructured.append(specializerNP->getElements().size(), nullptr);
    destructured.push_back(p);
    return;
  }
      
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    llvm_unreachable("not semantic");
  }
}

/// True if the 'sub' pattern node is subsumed by the 'super' node, that is,
/// all values 'sub' matches are also matched by 'super'.
bool isPatternSubsumed(const Pattern *sub, const Pattern *super) {
  // Wildcards subsume everything.
  if (!super) return true;
  if (isWildcardPattern(super)) return true;
  
  sub = sub->getSemanticsProvidingPattern();
  super = super->getSemanticsProvidingPattern();
  
  // A pattern always subsumes itself.
  if (sub == super) return true;
  
  switch (super->getKind()) {
  case PatternKind::Any:
  case PatternKind::Named:
  case PatternKind::Expr:
    // If super wasn't already handled as a wildcard above, then it can't
    // subsume a wildcard.
    return false;
      
  case PatternKind::Tuple: {
    // Tuples should only match wildcard or same-shaped tuple patterns, which
    // are exhaustive so always subsume other tuple patterns of the same type.
    auto *tsub = cast<TuplePattern>(sub);
    // Wildcard 'super' should have been handled above.
    auto *tsup = cast<TuplePattern>(super);
    
    assert(tsub->getType()->isEqual(tsup->getType()) &&
           "tuple patterns should match same type");
    assert(tsub->getFields().size() == tsup->getFields().size() &&
           "tuple patterns should have same shape");

    (void)tsub;
    (void)tsup;

    return true;
  }
  
  case PatternKind::Isa: {
    auto *isup = cast<IsaPattern>(super);
    auto *isub = dyn_cast<IsaPattern>(sub);
    
    // If the "sub" pattern isn't another cast, we can't subsume it.
    if (!isub)
      return false;
    
    Type subTy = isub->getCastTypeLoc().getType();
    Type supTy = isup->getCastTypeLoc().getType();
    
    // Casts to the same type subsume each other.
    if (subTy->isEqual(supTy))
      return true;

    // Superclass casts subsume casts to subclasses or archetypes bounded by the
    // superclass.
    if (supTy->isSuperclassOf(subTy, nullptr))
      return true;
    
    return false;
  }

  case PatternKind::EnumElement: {
    auto *usub = cast<EnumElementPattern>(sub);
    // Wildcard 'super' should have been handled above.
    auto *usup = cast<EnumElementPattern>(super);
    
    // EnumElements are subsumed by equivalent EnumElements.
    return usub->getElementDecl() == usup->getElementDecl();
  }
    
  case PatternKind::NominalType: {
    // A NominalType pattern subsumes any other pattern matching the same type.
    assert(sub->getType()->getCanonicalType()
             == super->getType()->getCanonicalType() &&
           "nominal type patterns should match same type");
    
    return true;
  }
    
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    llvm_unreachable("not semantic");
  }
}

/// Remove patterns in the given range that are subsumed by the given
/// specializing pattern, returning a combined pattern that covers all of them.
/// In most cases, the returned pattern is the same as the specializing pattern,
/// but e.g. nominal type patterns can be combined to match all properties
/// needed by the set of patterns.
static const Pattern *combineAndFilterSubsumedPatterns(
                                SmallVectorImpl<SpecializingPattern> &patterns,
                                unsigned beginIndex, unsigned endIndex,
                                ASTContext &C) {
  // For nominal type patterns, we want to combine all the following subsumed
  // nominal type patterns into one pattern with all of the necessary properties
  // to match all of the patterns together.
  auto begin = patterns.begin()+beginIndex, end = patterns.begin()+endIndex;
  auto foundSpec = std::find_if(begin, end,
      [](SpecializingPattern p) { return isa<NominalTypePattern>(p.pattern); });
  
  if (foundSpec != end) {
    auto specNP = cast<NominalTypePattern>(foundSpec->pattern);
    llvm::MapVector<VarDecl *, NominalTypePattern::Element> neededProperties;
    for (auto &elt : specNP->getElements()) {
      neededProperties.insert({elt.getProperty(), elt});
    }
    // An existing pattern with all of the needed properties, if any.
    const NominalTypePattern *superPattern = specNP;
    
    for (auto &sub : make_range(foundSpec + 1,
                                end)) {
      if (auto *subNP = dyn_cast<NominalTypePattern>(sub.pattern)) {
        // Count whether this pattern matches all the needed properties.
        unsigned propCount = neededProperties.size();
        for (auto &elt : subNP->getElements()) {
          if (neededProperties.count(elt.getProperty())) {
            --propCount;
          } else {
            // This patterns adds a property. Invalidate superPattern.
            neededProperties.insert({elt.getProperty(), elt});
            superPattern = nullptr;
          }
        }
        
        // Use this pattern as the new super-pattern if it has all the needed
        // properties.
        if (!superPattern && propCount == 0)
          superPattern = subNP;
      }
    }
    
    // If we didn't find an existing "super" pattern, we have to make one.
    if (!superPattern) {
      SmallVector<NominalTypePattern::Element, 4> superElts;
      for (auto &prop : neededProperties)
        superElts.push_back(prop.second);
      auto newPat = NominalTypePattern::create(specNP->getCastTypeLoc(),
                                        specNP->getLParenLoc(),
                                        superElts,
                                        specNP->getRParenLoc(), C);
      newPat->setType(specNP->getType());
      superPattern = newPat;
    }
    
    // Check that the superPattern is as super as we need it to be.
    assert([&]{
      for (auto prop : neededProperties) {
        for (auto &elt : superPattern->getElements())
          if (elt.getProperty() == prop.first)
            goto next;
        return false;
      next:;
      }
      return true;
    }() && "missing properties from subsuming nominal type pattern");

    // Subsume all of the patterns.
    patterns.erase(begin+1, end);
    return superPattern;
  }
  
  // Otherwise, specialize on the first pattern.
  // NB: Removes elements from 'patterns' mid-loop.
  const Pattern *specializer = patterns[beginIndex].pattern;
  for (unsigned i = beginIndex+1; i < endIndex; ++i) {
    while (isPatternSubsumed(patterns[i].pattern, specializer)) {
      patterns.erase(patterns.begin() + i);
      --endIndex;
      if (i >= endIndex)
        break;
    }
  }
  
  return specializer;
}

/// True if two pattern nodes are orthogonal, that is, they never both match
/// the same value.
static bool arePatternsOrthogonal(const Pattern *a, const Pattern *b) {
  // Wildcards are never orthogonal.
  if (!a) return false;
  if (!b) return false;
  if (isWildcardPattern(b)) return false;

  a = a->getSemanticsProvidingPattern();
  b = b->getSemanticsProvidingPattern();

  // A pattern is never orthogonal to itself.
  if (a == b) return false;

  switch (a->getKind()) {
  case PatternKind::Any:
  case PatternKind::Named:
  case PatternKind::Expr:
    return false;
      
  case PatternKind::Tuple: {
    // Tuples should only match wildcard or same-shaped tuple patterns, to
    // which they are never orthogonal.
    auto *ta = cast<TuplePattern>(a);
    auto *tb = cast<TuplePattern>(b);
    
    assert(ta->getType()->isEqual(tb->getType()) &&
           "tuple patterns should match same type");
    assert(ta->getFields().size() == tb->getFields().size() &&
           "tuple patterns should have same shape");

    (void)ta;
    (void)tb;

    return false;
  }
      
  case PatternKind::Isa: {
    auto *ia = cast<IsaPattern>(a);

    // 'is' is never orthogonal to a nominal type pattern.
    if (isa<NominalTypePattern>(b))
      return false;

    auto *ib = cast<IsaPattern>(b);
    if (!ib)
      return true;
    
    // Casts to the same type are parallel.
    Type aTy = ia->getCastTypeLoc().getType();
    Type bTy = ib->getCastTypeLoc().getType();
    
    if (aTy->isEqual(bTy))
      return false;
      
    ArchetypeType *aArchety = aTy->getAs<ArchetypeType>();
    ArchetypeType *bArchety = bTy->getAs<ArchetypeType>();
    if (aArchety && bArchety) {
      // Two archetypes are only conclusively orthogonal if they have unrelated
      // superclass constraints.
      if (aArchety->getSuperclass() && bArchety->getSuperclass())
        return !aArchety->getSuperclass()->isSuperclassOf(bArchety->getSuperclass(),
                                                         nullptr)
          && !bArchety->getSuperclass()->isSuperclassOf(bArchety->getSuperclass(),
                                                       nullptr);
      return false;
    }
    
    // An archetype cast is orthogonal if it's class-constrained and the other
    // type is not a class or a class unrelated to its superclass constraint.
    auto isOrthogonalToArchetype = [&](ArchetypeType *arch, Type ty) -> bool {
      if (arch->requiresClass()) {
        if (!ty->getClassOrBoundGenericClass())
          return true;
        if (Type sup = arch->getSuperclass())
          return !sup->isSuperclassOf(ty, nullptr);
      }
      return false;
    };

    if (aArchety)
      return isOrthogonalToArchetype(aArchety, bTy);
    if (bArchety)
      return isOrthogonalToArchetype(bArchety, aTy);
    
    // Class casts are orthogonal to non-class casts.
    bool aClass = aTy->getClassOrBoundGenericClass();
    bool bClass = bTy->getClassOrBoundGenericClass();
      
    if (!aClass || !bClass)
      return true;
      
    // Class casts are orthogonal to casts to a class without a subtype or
    // supertype relationship.
    return !aTy->isSuperclassOf(bTy, nullptr)
      && !bTy->isSuperclassOf(aTy, nullptr);
  }
      
  case PatternKind::EnumElement: {
    auto *ua = cast<EnumElementPattern>(a);
    auto *ub = cast<EnumElementPattern>(b);
    
    return ua->getElementDecl() != ub->getElementDecl();
  }
      
  case PatternKind::NominalType: {
    // Nominal types match all values of their type, so are never orthogonal.
    return false;
  }
      
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    llvm_unreachable("not semantic");
  }
}

namespace {
  
/// A CaseMap entry.
struct CaseBlock {
  /// The entry BB for the case.
  SILBasicBlock *entry = nullptr;
  /// The continuation BB for the case.
  SILBasicBlock *cont = nullptr;
  /// The scope of the case.
  CleanupsDepth cleanupsDepth = CleanupsDepth::invalid();
};
  
/// Map type used to associate CaseStmts to SILBasicBlocks. Filled in as
/// dispatch is resolved.
using CaseMap = llvm::MapVector<CaseStmt*, CaseBlock>;

/// Create the entry point BB for a case block, if necessary, and add it to the
/// CaseMap. Returns the entry point BB emitted for the block.
SILBasicBlock *createCaseBlock(SILGenFunction &gen, CaseMap &caseMap,
                             CaseStmt *caseBlock,
                             CleanupsDepth cleanupsDepth,
                             SILBasicBlock *contBB) {
  CaseBlock &dest = caseMap[caseBlock];
  
  // If the block was emitted, sanity check that it was emitted at the same
  // scope level we think it should be.
  if (dest.entry) {
    assert(cleanupsDepth == dest.cleanupsDepth
           && "divergent cleanup depths for case");
    assert(contBB == dest.cont
           && "divergent continuation BBs for case");
    return dest.entry;
  }
  
  // Set up the basic block for the case.
  dest.entry = gen.createBasicBlock();
  dest.cleanupsDepth = cleanupsDepth;
  dest.cont = contBB;
  
  return dest.entry;
}
  
/// A handle to a row in a clause matrix. Does not own memory; use of the
/// ClauseRow must be dominated by its originating ClauseMatrix.
class ClauseRow {
  friend class ClauseMatrix;
  
  /// The in-memory layout of a clause row prefix.
  struct Prefix {
    /// The CaseStmt corresponding to the patterns in the row.
    CaseStmt *caseBlock;
    /// The guard expression for the row, or null if there is no guard.
    Expr *guardExpr;
    /// The cleanup level of the case body.
    CleanupsDepth cleanupsDepth;
    /// The continuation BB for this case.
    SILBasicBlock *cont;
    /// The ExprPatterns within the pattern and their matching values.
    unsigned firstExprGuard, lastExprGuard;
  };
  
  Prefix *row;
  unsigned columnCount;
  ArrayRef<const ExprPattern*> exprGuards;
  
  // ClauseRows should only be vended by ClauseMatrix::operator[].
  ClauseRow(Prefix *row, unsigned columnCount,
            ArrayRef<const ExprPattern*> guards)
    : row(row), columnCount(columnCount), exprGuards(guards)
  {}
  
public:
  ClauseRow() = default;
  
  CaseStmt *getCaseBlock() const {
    return row->caseBlock;
  }
  Expr *getGuard() const {
    return row->guardExpr;
  }
  bool hasGuard() const {
    return row->guardExpr;
  }
  CleanupsDepth getCleanupsDepth() const {
    return row->cleanupsDepth;
  }
  void setCleanupsDepth(CleanupsDepth d) {
    row->cleanupsDepth = d;
  }
  
  SILBasicBlock *getContBB() const {
    return row->cont;
  }
  
  ArrayRef<const ExprPattern*> getExprGuards() const {
    return exprGuards;
  }
  
  /// Create the case block corresponding to this row if necessary. Returns the
  /// entry point BB emitted for the block.
  SILBasicBlock *createCaseBlock(SILGenFunction &gen, CaseMap &caseMap) const {
    return ::createCaseBlock(gen, caseMap,
                             getCaseBlock(), getCleanupsDepth(), getContBB());
  }
  
  ArrayRef<const Pattern *> getColumns() const {
    return {reinterpret_cast<const Pattern * const*>(row + 1), columnCount};
  }
  MutableArrayRef<const Pattern *> getColumns() {
    return {reinterpret_cast<const Pattern **>(row + 1), columnCount};
  }
  
  const Pattern * const *begin() const {
    return getColumns().begin();
  }
  const Pattern * const *end() const {
    return getColumns().end();
  }
  
  const Pattern **begin() {
    return getColumns().begin();
  }
  const Pattern **end() {
    return getColumns().end();
  }
  
  const Pattern *operator[](unsigned column) const {
    return getColumns()[column];
  }
  const Pattern *&operator[](unsigned column) {
    return getColumns()[column];
  }
  unsigned columns() const {
    return columnCount;
  }
};

/// Get a pattern as an ExprPattern, unwrapping semantically transparent
/// pattern nodes.
static const ExprPattern *getAsExprPattern(const Pattern *p) {
  if (!p) return nullptr;

  switch (p->getKind()) {
  case PatternKind::Expr:
    return cast<ExprPattern>(p);
  
  case PatternKind::Tuple:
  case PatternKind::Named:
  case PatternKind::Any:
  case PatternKind::Isa:
  case PatternKind::NominalType:
  case PatternKind::EnumElement:
    return nullptr;

  // Recur into simple wrapping patterns.
  case PatternKind::Paren:
    return getAsExprPattern(cast<ParenPattern>(p)->getSubPattern());
  case PatternKind::Typed:
    return getAsExprPattern(cast<TypedPattern>(p)->getSubPattern());
  case PatternKind::Var:
    return getAsExprPattern(cast<VarPattern>(p)->getSubPattern());
  }
}

/// A clause matrix. This matrix associates subpattern rows to their
/// corresponding guard expressions, and associates destination basic block
/// and columns to their associated subject value.
class ClauseMatrix {
  /// The capacity of the memory buffer in rows/columns.
  unsigned rowCapacity, columnCapacity;
  /// The inhabited part of the memory buffer.
  unsigned rowCount, columnCount;
  /// The memory buffer containing the matrix data.
  void *data;
  /// A list of ExprPatterns and the associated SILValue they test. Rows
  /// reference into slices of this vector to indicate which ExprPatterns form
  /// part of their guard.
  std::vector<const ExprPattern*> allExprGuards;
  
  // The memory layout of data is as follows:
  
  // - columnCapacity SILValues, for the occurrence vector. The first
  //   columnCount entries are populated.
  size_t getOccurrenceVectorStride() const {
    return columnCapacity * sizeof(SILValue);
  }
  MutableArrayRef<SILValue> getMutableOccurrences() {
    return {reinterpret_cast<SILValue*>(data), columnCount};
  }
  
  // - rowCapacity rows, each of which contains:
  //   - a ClauseRow::Prefix, followed by
  //   - columnCapacity Pattern*s, for the clause patterns.
  
  size_t getRowStride() const {
    return sizeof(ClauseRow::Prefix) + columnCapacity * sizeof(Pattern*);
  }
  ClauseRow::Prefix *getMutableRowPrefix(unsigned i) {
    assert(i < rowCount && "row access out of bounds");
    char *firstRow = reinterpret_cast<char*>(data) + getOccurrenceVectorStride();
    return reinterpret_cast<ClauseRow::Prefix*>(firstRow + getRowStride() * i);
  }
  
  size_t getDataSize() const {
    return getOccurrenceVectorStride() + getRowStride() * rowCapacity;
  }
  
  ClauseMatrix(const ClauseMatrix &) = delete;
  ClauseMatrix &operator=(const ClauseMatrix &) = delete;
public:
  /// Allocate a clause matrix over the given set of occurrences
  /// (actively matched values) and enough initial capacity for the
  /// given number of rows. The clause matrix will be initialized with zero rows
  /// and a column for every occurrence. Rows can be added using addRows.
  explicit ClauseMatrix(ArrayRef<SILValue> occurrences,
                        unsigned rowCapacity)
    : rowCapacity(rowCapacity), columnCapacity(occurrences.size()),
      rowCount(0), columnCount(columnCapacity)
  {
    // Allocate the data buffer.
    data = malloc(getDataSize());
    assert(data);
    
    // Initialize the occurrence vector.
    MutableArrayRef<SILValue> occurrenceBuf = getMutableOccurrences();
    for (unsigned i = 0, e = occurrences.size(); i < e; ++i) {
      occurrenceBuf[i] = occurrences[i];
    }
  }
  
  ~ClauseMatrix() {
    free(data);
  }
  
  ClauseMatrix(ClauseMatrix &&m)
    : rowCapacity(m.rowCapacity), columnCapacity(m.columnCapacity),
      rowCount(m.rowCount), columnCount(m.columnCount),
      data(m.data)
  {
    m.data = nullptr;
  }

  unsigned rows() const { return rowCount; }
  unsigned columns() const { return columnCount; }

  /// Append a row to the matrix.
  void addRow(CaseStmt *caseBlock, Expr *guardExpr,
              ArrayRef<const Pattern*> cols,
              CleanupsDepth scope,
              SILBasicBlock *contBB,
              ArrayRef<const ExprPattern *> parentExprGuards = {}) {
    assert(cols.size() == columnCount && "new row has wrong number of columns");

    // Grow storage if necessary.
    if (rowCount == rowCapacity) {
      rowCapacity *= 2;
      data = realloc(data, getDataSize());
      assert(data);
    }
    
    // Collect guards introduced by ExprPatterns in this row.
    unsigned exprGuardStart = allExprGuards.size();
    allExprGuards.insert(allExprGuards.end(),
                      parentExprGuards.begin(), parentExprGuards.end());
    for (unsigned i = 0, e = cols.size(); i < e; ++i) {
      auto *ep = getAsExprPattern(cols[i]);
      if (!ep)
        continue;
      if (std::find(parentExprGuards.begin(), parentExprGuards.end(), ep)
           == parentExprGuards.end())
        allExprGuards.push_back(ep);
    }
    unsigned exprGuardEnd = allExprGuards.size();
    
    // Initialize the next row.
    ClauseRow::Prefix *row = getMutableRowPrefix(rowCount++);
    ::new (row) ClauseRow::Prefix{caseBlock, guardExpr,
                                  scope, contBB,
                                  exprGuardStart, exprGuardEnd};
    MutableArrayRef<const Pattern*> columnsBuf{
      reinterpret_cast<const Pattern**>(row+1),
      columnCount
    };
    
    for (unsigned i = 0; i < columnCount; ++i)
      columnsBuf[i] = cols[i];
  }
  
  /// Remove a row from the matrix.
  void removeRow(unsigned r) {
    assert(r < rowCount && "removeRow out of bounds");
    if (r + 1 < rowCount)
      memmove(getMutableRowPrefix(r), getMutableRowPrefix(r+1),
              getRowStride() * (rowCount - (r+1)));
    --rowCount;
  }
  
  ArrayRef<SILValue> getOccurrences() const {
    return const_cast<ClauseMatrix*>(this)->getMutableOccurrences();
  }
  
  ClauseRow operator[](unsigned row) {
    auto *prefix = getMutableRowPrefix(row);
    return {getMutableRowPrefix(row), columnCount,
            {&allExprGuards[prefix->firstExprGuard],
             prefix->lastExprGuard - prefix->firstExprGuard}};
  }
  const ClauseRow operator[](unsigned row) const {
    return const_cast<ClauseMatrix&>(*this)[row];
  }
  
  /// Specialize this matrix's first column on a constructor, and emit
  /// variable bindings for the leaf pattern nodes exposed by specialization.
  ///
  /// Given an n-ary
  /// constructor form c(x1...xn), this performs the following row-wise
  /// transformation to create a new matrix:
  ///
  /// If the row is [ c(x1...xn)   p1 p2 ... -> Dest ],
  /// expand to     [ x1 x2 ... xn p1 p2 ... -> Dest ].
  ///
  /// If the row is [ _            p1 p2 ... -> Dest ],
  /// expand to     [ _  _  ... _  p1 p2 ... -> Dest ].
  ///
  /// If the row is [ c'(...)      p1 p2 ... -> Dest ] where c' != c,
  /// remove the column.
  ///
  /// The first skipRows rows are removed prior to the transformation.
  /// We treat ExprPatterns as wildcards with a guard.
  ClauseMatrix
  emitSpecialization(SILGenFunction &gen,
                     SpecializingPattern specializer,
                     ArrayRef<SILValue> specOccurrences,
                     CleanupsDepth specDepth, SILBasicBlock *specCont) const {
    assert(columnCount >= 1 && "can't specialize a matrix with no columns");
    assert(specializer.row < rowCount && "can't skip more rows than we have");
    
    unsigned specializedWidth = specOccurrences.size();
    
    // Gather the new occurrences with those of the remaining columns.
    SmallVector<SILValue, 4> newOccurrences(specOccurrences.begin(),
                                            specOccurrences.end());
    std::copy(getOccurrences().begin() + 1, getOccurrences().end(),
              std::back_inserter(newOccurrences));

    ClauseMatrix specialized{newOccurrences, rowCount - specializer.row};
    
    for (unsigned r = specializer.row; r < rowCount; ++r) {
      auto row = (*this)[r];
      auto columns = row.getColumns();
      // If the pattern in this row isn't a wildcard and matches an orthogonal
      // constructor, it is removed from the specialized matrix.
      if (arePatternsOrthogonal(specializer.pattern, columns[0]))
        continue;
      
      // Destructure matching constructors and wildcards.
      SmallVector<const Pattern*, 4> newPatterns;
      CleanupsDepth rowDepth = row.getCleanupsDepth();
      SILBasicBlock *rowCont = row.getContBB();
      if (isWildcardPattern(columns[0])) {
        // Wildcards explode into more wildcards. Since exploding a wildcard
        // won't expose more var bindings, we don't modify the scope from the
        // original row. (This scope must be the same as the scope in other
        // specializations that carry the row forward, since we emit the
        // case block only once.)
        for (unsigned i = 0; i < specializedWidth; ++i)
          newPatterns.push_back(nullptr);
      } else {
        // Non-wildcards destructure relative to the specializing pattern
        // constructor. If the specialization exposes variable bindings,
        // we rescope it to the specialized scope.
        destructurePattern(gen, specializer.pattern, columns[0], newPatterns);
        for (auto *newPattern : newPatterns)
          if (isBindingPattern(newPattern)) {
            rowDepth = CleanupsDepth::invalid();
            rowCont = specCont;
            break;
          }
      }
      assert(newPatterns.size() == specializedWidth &&
             "destructurePattern did not produce number of columns consistent "
             "with emitDispatchAndDestructure");
      
      // Forward the remaining pattern columns from the row.
      std::copy(columns.begin() + 1, columns.end(),
                std::back_inserter(newPatterns));
      
      // Append the new row.
      specialized.addRow(row.getCaseBlock(), row.getGuard(),
                         newPatterns,
                         rowDepth, rowCont,
                         row.getExprGuards());
    }
    RegularLocation loc(const_cast<Pattern *>(specializer.pattern));

    // Emit variable bindings from the newly specialized rows.
    for (unsigned i = 0; i < specializedWidth; ++i)
      specialized.emitVarsInColumn(gen, i);
    
    // Update the scope of specialized rows to include the new variables.
    for (unsigned r = 0, e = specialized.rows(); r < e; ++r)
      if (!specialized[r].getCleanupsDepth().isValid())
        specialized[r].setCleanupsDepth(gen.Cleanups.getCleanupsDepth());
    
    return specialized;
  }
  
  /// Emit pattern variable bindings, if any, to the value in a column of the
  /// matrix.
  void emitVarsInColumn(SILGenFunction &gen, unsigned column) {
    assert(column < columns() && "column out of bounds");
    if (rows() == 0) return;
    
    SILValue v = getOccurrences()[column];
    
    // Since only one row's variables will ever be available in any case block,
    // we can emit one box and alias all the column variables to it.
    // FIXME: If we end up supporting variables in case alternates, we will need
    // to handle that, perhaps by phi-ing the corresponding boxes into the case
    // block.
    VarDecl *emittedVar = nullptr;
    
    auto emitVar = [&](VarDecl *vd, SILLocation loc) {
      // If we already emitted a variable from another row, alias that variable.
      if (emittedVar) {
        gen.VarLocs[vd] = gen.VarLocs[emittedVar];
        return;
      }
      
      // Create and initialize the variable.
      InitializationPtr init = gen.emitLocalVariableWithCleanup(vd);
      ManagedValue(v, ManagedValue::Unmanaged).copyInto(gen, init->getAddress(),
                                                        loc);
      init->finishInitialization(gen);
      emittedVar = vd;
    };
    
    for (unsigned r = 0, e = rows(); r < e; ++r) {
      const Pattern *p = (*this)[r][column];
      if (!p)
        continue;
      p = p->getSemanticsProvidingPattern();
      switch (p->getKind()) {
      // Non-variables.
      case PatternKind::Any:
      case PatternKind::Tuple:
      case PatternKind::NominalType:
      case PatternKind::EnumElement:
      case PatternKind::Isa:
        continue;
          
      case PatternKind::Named:
        // Bind the named variable.
        emitVar(cast<NamedPattern>(p)->getDecl(),
                RegularLocation(const_cast<Pattern*>(p)));
        break;
          
      case PatternKind::Expr:
        // Bind the ExprPattern's implicit match var.
        // TODO: It'd be nice not to need the temp var for expr patterns.
        emitVar(cast<ExprPattern>(p)->getMatchVar(),
                RegularLocation(const_cast<Pattern*>(p)));
        break;

      case PatternKind::Paren:
      case PatternKind::Typed:
      case PatternKind::Var:
        llvm_unreachable("not semantic");
      }
    }
  }
  
  /// Transform this matrix into its default. The following row-wise
  /// transformation on the matrix is performed in-place:
  ///
  /// If the row is [ _ p1 p2 ... -> Dest ],
  /// reduce to       [ p1 p2 ... -> Dest ].
  ///
  /// If the row is [ c(...) p1 p2 ... -> Dest ] for any constructor c,
  /// remove the row.
  ///
  /// This collects all the wildcard rows not exclusively associated with any
  /// one specialized matrix.
  ///
  /// The first skipRows rows are removed prior to the transformation.
  /// We treat ExprPatterns as wildcards with a guard.
  ///
  /// This destroys the occurrence for the dropped row, because it is no
  /// longer needed for dispatch.
  void reduceToDefault(SILGenFunction &gen,
                       unsigned skipRows) {
    assert(columnCount >= 1 && "can't default a matrix with no columns");
    assert(skipRows < rowCount && "can't skip more rows than we have");

    // Destroy the first occurrence.
    SILValue droppedOccurrence = getOccurrences()[0];
    // FIXME: Cleanup location.
    gen.getTypeLowering(droppedOccurrence.getType())
      .emitDestroyRValue(gen.B, gen.CurrentSILLoc, droppedOccurrence);
    
    // Drop the first occurrence.
    memmove(getMutableOccurrences().begin(), getMutableOccurrences().begin()+1,
            sizeof(SILValue) * (columnCount - 1));
    
    // Discard the skipped rows.
    memmove(getMutableRowPrefix(0), getMutableRowPrefix(skipRows),
            getRowStride() * (rowCount - skipRows));
    rowCount -= skipRows;
    
    unsigned r = 0;
    while (r < rowCount) {
      // Filter specialized rows.
      if (!isWildcardPattern((*this)[r][0])) {
        removeRow(r);
        continue;
      }
      
      // Discard the head wildcard of wildcard rows.
      auto row = (*this)[r];
      memmove(row.begin(), row.begin()+1, sizeof(Pattern*) * (columnCount - 1));
      ++r;
    }
    
    // Shrink the column count.
    --columnCount;
  }
  
  /// Emit dispatch to a row's destination. If the row has no guard, the
  /// branch is unconditional, and this terminates the current block and
  /// returns true. Otherwise, it dispatches conditionally on the guard, leaves
  /// the insertion branch on the not-taken path, and returns false.
  bool emitDispatch(SILGenFunction &gen,
                    unsigned r,
                    CaseMap &caseMap) const {
    ClauseRow row = (*this)[r];
    
    // Create the destination case block.
    SILBasicBlock *caseBB = row.createCaseBlock(gen, caseMap);

    // Create new BBs for the guard branch.
    SILBasicBlock *trueBB = nullptr;
    SILBasicBlock *falseBB = nullptr;
    auto getFalseBB = [&] {
      if (!falseBB)
        falseBB = gen.createBasicBlock();
      return falseBB;
    };
    
    // TODO: We should emit every guard once and share code if it covers
    // multiple patterns.
    if (!row.exprGuards.empty()) {
      // Test ExprPatterns from the row in an "and" chain.
      for (const ExprPattern *ep : row.exprGuards) {
        // The last pattern test jumps to the guard.
        trueBB = gen.createBasicBlock();
        
        // Emit the match test.
        SILValue testBool;
        {
          Expr *ME = ep->getMatchExpr();
          FullExpr scope(gen.Cleanups, CleanupLocation(ME));
          testBool = gen.emitRValue(ME)
            .getUnmanagedSingleValue(gen, ME);
        }

        // If the test succeeds, we move on to the next test; otherwise, we
        // fail and move to the next pattern.
        gen.B.createCondBranch(row.getCaseBlock(), testBool,
                               trueBB, getFalseBB());
        gen.B.emitBlock(trueBB);
      }
    }
    
    if (row.hasGuard()) {
      SILValue guardBool;
      {
        Expr *G = row.getGuard();
        FullExpr scope(gen.Cleanups, CleanupLocation(G));
          
        // Emit the guard.
        // TODO: We should emit every guard once and share code if it covers
        // multiple patterns.
        guardBool = gen.emitRValue(G).getUnmanagedSingleValue(gen, G);
      }
      
      // Branch either to the row destination or the new BB.
      trueBB = gen.createBasicBlock();
      gen.B.createCondBranch(row.getCaseBlock(), guardBool,
                             trueBB, getFalseBB());
      gen.B.emitBlock(trueBB);
    }
    
    // In the true block, clean up the occurrences.
    // FIXME: We should share an occurrence cleanup block among all rows in the
    // same matrix.
    // FIXME: The location for the cleanup should be placed smarter.
    for (SILValue occurrence : getOccurrences()) {
      gen.getTypeLowering(occurrence.getType())
        .emitDestroyRValue(gen.B, CleanupLocation(row.getCaseBlock()),
                           occurrence);
    }
    
    // Jump to the case block, unwinding if necessary.
    gen.Cleanups.emitBranchAndCleanups(JumpDest{caseBB,
                                                row.getCleanupsDepth(),
                                                row.getCaseBlock()},
                                       gen.CurrentSILLoc);
    
    // Position the case block logically.
    gen.B.moveBlockToEnd(caseBB);
    
    // If we branched, continue codegen on the false branch.
    if (falseBB) {
      gen.B.emitBlock(falseBB);
      return false;
    }
    return true;
  }
  
  // Select the "necessary column", Maranget's term for the column most likely
  // to give an optimal decision tree, and swap it into the zero column of the
  // matrix.
  void chooseNecessaryColumn(unsigned depth) {
    // First of all, if we have zero or one columns, this is trivial.
    if (columns() < 2)
      return;
    
    // Use the "constructor prefix" heuristic from Maranget to pick the
    // necessary column. The column with the most pattern nodes prior to a
    // wildcard turns out to be a good and cheap-to-calculate heuristic for
    // generating an optimal decision tree.
    unsigned necessaryColumn = 0, longestConstructorPrefix = 0;
    for (unsigned c = 0, cend = columns(); c < cend; ++c) {
      unsigned constructorPrefix = 0;
      for (unsigned r = 0, rend = rows(); r < rend; ++r) {
        if (isWildcardPattern((*this)[r][c]))
          break;
        ++constructorPrefix;
      }
      
      if (constructorPrefix > longestConstructorPrefix) {
        longestConstructorPrefix = constructorPrefix;
        necessaryColumn = c;
      }
    }
    
    DEBUG(dumpDepth(depth, llvm::dbgs());
          llvm::dbgs() << "Choosing necessary column " << necessaryColumn
                       << '\n');
    
    // Swap the necessary column into the head position.
    if (necessaryColumn != 0) {
      std::swap(getMutableOccurrences()[0],
                getMutableOccurrences()[necessaryColumn]);
      for (unsigned r = 0, rend = rows(); r < rend; ++r)
        std::swap((*this)[r][0], (*this)[r][necessaryColumn]);
    }
  }
  
  void print(llvm::raw_ostream &os, unsigned depth = 0) const {
    // Tabulate the strings for each column, column-major.
    std::vector<std::vector<std::string>> patternStrings;
    std::vector<unsigned> columnSizes;
    patternStrings.resize(columns());
    columnSizes.resize(columns());
    
    llvm::formatted_raw_ostream fos(os);
    
    for (unsigned r = 0, rend = rows(); r < rend; ++r) {
      auto row = (*this)[r];
      for (unsigned c = 0, cend = columns(); c < cend; ++c) {
        patternStrings[c].push_back("");
        llvm::raw_string_ostream ss(patternStrings[c].back());
        dumpPattern(row[c], ss);
        ss.flush();

        columnSizes[c] = std::max(columnSizes[c],
                                  (unsigned)patternStrings[c].back().size());
      }
    }

    for (unsigned r = 0, rend = rows(); r < rend; ++r) {
      dumpDepth(depth, fos);
      fos << "[ ";
      for (unsigned c = 0, cend = columns(); c < cend; ++c) {
        unsigned start = fos.getColumn();
        fos << patternStrings[c][r];
        fos.PadToColumn(start + columnSizes[c] + 1);
      }
      fos << "]\n";
    }
    fos.flush();
  }
  
  void dump() const {
    return print(llvm::errs());
  }
};

} // end anonymous namespace

static void emitCaseBody(SILGenFunction &gen,
                         CaseStmt *caseStmt,
                         const CaseBlock &caseBlock) {
  assert(caseBlock.entry->empty() && "already emitted case body");
  assert(gen.getCleanupsDepth() == caseBlock.cleanupsDepth
         && "emitting case block in wrong scope");
  
  // Emit the case.
  gen.B.setInsertionPoint(caseBlock.entry);
  
  gen.visit(caseStmt->getBody());
  if (gen.B.hasValidInsertionPoint())
    gen.B.createBranch(caseStmt, caseBlock.cont);
}

/// Emit cases for a scope, identified by its continuation BB.
static void emitCasesForScope(SILGenFunction &gen,
                              SILBasicBlock *contBB,
                              const CaseMap &caseMap) {
  for (auto &cases : caseMap) {
    CaseStmt *caseStmt = cases.first;
    const CaseBlock &caseBlock = cases.second;
    
    if (caseBlock.cont == contBB)
      emitCaseBody(gen, caseStmt, caseBlock);
  }
}

/// Recursively emit a decision tree from the given pattern matrix.
static void emitDecisionTree(SILGenFunction &gen,
                             SwitchStmt *stmt,
                             ClauseMatrix &&clauses,
                             CaseMap &caseMap,
                             SILBasicBlock *contBB,
                             unsigned depth) {
recur:
  DEBUG(clauses.print(llvm::dbgs(), depth));
  
  // If there are no rows, then we fail. This will be a dataflow error if we
  // can reach here.
  if (clauses.rows() == 0) {
    gen.B.createUnreachable(stmt);
    return;
  }
  
  // If the first rows are all wildcards (or there are no columns), then
  // try to dispatch to them by testing their guards.
  // ExprPatterns are treated as wildcards with guards.
  unsigned r = 0, rows = clauses.rows();
  for (; r < rows; ++r) {
    ClauseRow row = clauses[r];
    bool wildcardRow = true;
    for (unsigned c = 0, cols = clauses.columns(); c < cols; ++c) {
      if (!isWildcardPattern(row[c])) {
        wildcardRow = false;
        break;
      }
    }
  
    if (!wildcardRow)
      break;
  
    // If the row has a guard, emit it, and try the next row if it fails.
    if (clauses.emitDispatch(gen, r, caseMap))
      // If the row is irrefutable, we're done.
      return;
  }
  
  // If there are no rows remaining, fail. This will be a dataflow error if we
  // can reach here.
  if (r >= rows) {
    gen.B.createUnreachable(stmt);
    return;
  }

  // If we get this far, we have at least one non-wildcard row with at least
  // one column.
  assert(clauses.rows() >= 1 && clauses.columns() >= 1 &&
         "empty clause matrices should have been handled above");

  // Specialize on the next necessary column to continue testing the match.
  clauses.chooseNecessaryColumn(depth);
  
  unsigned skipRows = r;
  
  // Collect the non-wildcard nodes from the column.
  SmallVector<SpecializingPattern, 4> specializers;
  unsigned firstWildcardRow = ~0U;
  for (; r < rows; ++r) {
    const Pattern *p = clauses[r][0];
    if (isWildcardPattern(p)) {
      firstWildcardRow = r;
      continue;
    }
    // If there was a preceding wildcard, we need to include in the
    // specialization. Otherwise, we can start the specialization at this row.
    unsigned specRow = firstWildcardRow == ~0U ? r : firstWildcardRow;
    specializers.push_back({clauses[r][0], specRow});
  }

  // Derive a set of orthogonal pattern nodes to specialize on.
  // We filter "specializers" on each pass, removing pattern nodes subsumed by
  // previous ones.
  for (unsigned i = 0; i < specializers.size(); ++i) {
    specializers[i].pattern
      = combineAndFilterSubsumedPatterns(specializers,
                                         i, specializers.size(),
                                         gen.getASTContext());
  }

  // If we have no specializations, recur into the default matrix immediately.
  if (specializers.empty()) {
    DEBUG(dumpDepth(depth, llvm::dbgs());
          llvm::dbgs() << "Reducing to default, at row " << skipRows << '\n');
    clauses.reduceToDefault(gen, skipRows);
    goto recur;
  }
  
  // Emit the dispatch table.
  DispatchedPatternVector dispatches;
  SILBasicBlock *defaultBB;
  {
    CleanupLocation contLoc(const_cast<Pattern*>(specializers[0].pattern));
    JumpDest contDest = JumpDest(contBB, gen.Cleanups.getCleanupsDepth(),
                                 contLoc);
    Scope dispatchScope(gen.Cleanups, contLoc);

    defaultBB
      = emitDispatchAndDestructure(gen, specializers,
                                   clauses.getOccurrences()[0],
                                   dispatches, stmt);
    assert(dispatches.size() == specializers.size() &&
           "dispatch table doesn't match specializing pattern set");
    
    // Emit each specialized branch.
    for (size_t i = 0, e = specializers.size(); i < e; ++i) {
      SILBasicBlock *bodyBB = dispatches[i].first;
      ArrayRef<SILValue> bodyOccurrences = dispatches[i].second;
      
      assert(!gen.B.hasValidInsertionPoint() && "dispatch did not close bb");
      gen.B.emitBlock(bodyBB);
      
      // Create a nested scope and cont bb to clean up var bindings exposed by
      // specializing the matrix.
      SILBasicBlock *innerContBB = gen.createBasicBlock();
      
      {
        Scope patternVarScope(gen.Cleanups,
                  CleanupLocation(const_cast<Pattern*>(specializers[i].pattern)));
        
        ClauseMatrix submatrix = clauses.emitSpecialization(gen, specializers[i],
                                                bodyOccurrences,
                                                gen.Cleanups.getCleanupsDepth(),
                                                innerContBB);
        DEBUG(dumpDepth(depth, llvm::dbgs());
              llvm::dbgs() << "Specializing on ";
              dumpPattern(specializers[i].pattern, llvm::dbgs());
              llvm::dbgs() << ", at row " << specializers[i].row << '\n');
        
        // Emit the submatrix into the true branch of the specialization.
        emitDecisionTree(gen, stmt, std::move(submatrix), caseMap, innerContBB,
                         depth + 1);
        assert(!gen.B.hasValidInsertionPoint()
               && "recursive emitDecisionTree did not terminate all its BBs");
        
        // Emit cases in this scope.
        emitCasesForScope(gen, innerContBB, caseMap);
        
        if (innerContBB->pred_empty()) {
          // If the continuation wasn't used, kill it.
          innerContBB->eraseFromParent();
          gen.B.clearInsertionPoint();
        } else {
          // Otherwise, emit scope cleanups into the continuation BB.
          gen.B.emitBlock(innerContBB);
        }
      }
      // Chain the inner continuation to the outer.
      if (gen.B.hasValidInsertionPoint())
        gen.Cleanups.emitBranchAndCleanups(contDest, stmt);
    }
  }

  // If the dispatch was exhaustive, then emitDispatchAndDestructure returns
  // null, and we're done.
  if (!defaultBB)
    return;
  
  // Otherwise, recur into the default matrix.
  assert(!gen.B.hasValidInsertionPoint() && "specialization did not close bb");
  
  gen.B.emitBlock(defaultBB, stmt);
  DEBUG(dumpDepth(depth, llvm::dbgs());
        llvm::dbgs() << "Reducing to default, at row " << skipRows << '\n');
  clauses.reduceToDefault(gen, skipRows);
  goto recur;
}

/// Context info used to emit FallthroughStmts.
/// Since fallthrough-able case blocks must not bind variables, they are always
/// emitted in the outermost scope of the switch.
class Lowering::SwitchContext {
public:
  // A reference to the active case-to-BB mapping.
  CaseMap &caseMap;
  // The cleanup scope of the outermost switch scope.
  CleanupsDepth outerScope;
  // The outermost continuation BB for the switch.
  SILBasicBlock *outerContBB;
};

void SILGenFunction::emitSwitchStmt(SwitchStmt *S) {
  DEBUG(llvm::dbgs() << "emitting switch stmt\n";
        S->print(llvm::dbgs());
        llvm::dbgs() << '\n');
  Scope OuterSwitchScope(Cleanups, CleanupLocation(S));
  
  // Emit the subject value. Dispatching will consume it.
  SILValue subject
    = emitRValue(S->getSubjectExpr()).forwardAsSingleValue(*this,
                                                           S->getSubjectExpr());
  
  // Prepare a case-to-bb mapping for fallthrough to consult.
  // The bbs for reachable cases will be filled in by emitDecisionTree below.
  CaseMap caseMap;

  // Create a scope to contain pattern variables.
  {
    Scope patternVarScope(Cleanups, CleanupLocation(S));

    // Create a continuation bb for life after the switch.
    SILBasicBlock *contBB = createBasicBlock();
    
    // Set up an initial clause matrix.
    ClauseMatrix clauses(subject, S->getCases().size());
    
    for (auto *caseBlock : S->getCases()) {
      caseMap[caseBlock] = {};
      for (auto *label : caseBlock->getCaseLabels())
        for (auto *pattern : label->getPatterns())
          clauses.addRow(caseBlock, label->getGuardExpr(), pattern,
                         CleanupsDepth::invalid(), contBB);
    }
  
    // Bind variable bindings from the topmost pattern nodes.
    clauses.emitVarsInColumn(*this, 0);
    
    CleanupsDepth cleanupsDepth = Cleanups.getCleanupsDepth();
    
    // Update the case scopes to include the bound variables.
    for (unsigned r = 0, e = clauses.rows(); r < e; ++r) {
      clauses[r].setCleanupsDepth(cleanupsDepth);
    }

    // Push context for fallthrough statements.
    SwitchContext context{caseMap, cleanupsDepth, contBB};
    SwitchStack.push_back(&context);

    // Emit the decision tree.
    emitDecisionTree(*this, S, std::move(clauses), caseMap, contBB, 0);
    assert(!B.hasValidInsertionPoint() &&
           "emitDecisionTree did not terminate all its BBs");
    
    // Emit cases for the outermost scope.
    emitCasesForScope(*this, contBB, caseMap);

    SwitchStack.pop_back();
    
    if (contBB->pred_empty()) {
      // If the continuation BB wasn't used, kill it.
      contBB->eraseFromParent();
      B.clearInsertionPoint();
    } else {
      // Otherwise, emit top-level cleanups into the continuation BB.
      B.emitBlock(contBB);
    }
  }
  
  assert([&] {
    for (auto &cases : caseMap)
      if (cases.second.entry && cases.second.entry->empty())
        return false && "Case not emitted";
    return true;
  }());
}

void SILGenFunction::emitSwitchFallthrough(FallthroughStmt *S) {
  assert(!SwitchStack.empty() && "fallthrough outside of switch?!");
  SwitchContext *context = SwitchStack.back();
  
  // Get the destination block.
  SILBasicBlock *dest = createCaseBlock(*this, context->caseMap,
                                      S->getFallthroughDest(),
                                      context->outerScope,
                                      context->outerContBB);
  
  // Jump to it.
  Cleanups.emitBranchAndCleanups(JumpDest{dest, context->outerScope,
                                          CleanupLocation(S)},
                                 S);
}
