//===--- SILConstants.cpp - SIL constant representation -------------------===//
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

#include "swift/SIL/SILConstants.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Demangling/Demangle.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/Support/TrailingObjects.h"
using namespace swift;

namespace swift {
llvm::cl::opt<unsigned>
    ConstExprLimit("constexpr-limit", llvm::cl::init(512),
                   llvm::cl::desc("Number of instructions interpreted in a"
                                  " constexpr function"));
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

//===----------------------------------------------------------------------===//
// SymbolicValue implementation
//===----------------------------------------------------------------------===//

void SymbolicValue::print(llvm::raw_ostream &os, unsigned indent) const {
  os.indent(indent);
  switch (representationKind) {
  case RK_Unknown: {
    os << "unknown(" << (int)getUnknownReason() << "): ";
    getUnknownNode()->dump();
    return;
  }
  case RK_Metatype:
    os << "metatype: ";
    getMetatypeValue()->print(os);
    os << "\n";
    return;
  case RK_Function: {
    auto fn = getFunctionValue();
    os << "fn: " << fn->getName() << ": ";
    os << Demangle::demangleSymbolAsString(fn->getName());
    os << "\n";
    return;
  }
  case RK_Integer:
  case RK_IntegerInline:
    os << "int: " << getIntegerValue() << "\n";
    return;
  case RK_Aggregate: {
    ArrayRef<SymbolicValue> elements = getAggregateValue();
    switch (elements.size()) {
    case 0:
      os << "agg: 0 elements []\n";
      return;
    case 1:
      os << "agg: 1 elt: ";
      elements[0].print(os, indent + 2);
      return;
    default:
      os << "agg: " << elements.size() << " elements [\n";
      for (auto elt : elements)
        elt.print(os, indent + 2);
      os.indent(indent) << "]\n";
      return;
    }
  }
  }
}

void SymbolicValue::dump() const { print(llvm::errs()); }

/// For constant values, return the classification of this value.  We have
/// multiple forms for efficiency, but provide a simpler interface to clients.
SymbolicValue::Kind SymbolicValue::getKind() const {
  switch (representationKind) {
  case RK_Unknown:
    return Unknown;
  case RK_Metatype:
    return Metatype;
  case RK_Function:
    return Function;
  case RK_Aggregate:
    return Aggregate;
  case RK_Integer:
  case RK_IntegerInline:
    return Integer;
  }
}

/// Clone this SymbolicValue into the specified ASTContext and return the new
/// version.  This only works for valid constants.
SymbolicValue
SymbolicValue::cloneInto(ASTContext &astContext) const {
  auto thisRK = representationKind;
  switch (thisRK) {
  case RK_Unknown:
  case RK_Metatype:
  case RK_Function:
    assert(0 && "cloning this representation kind is not supported");
  case RK_IntegerInline:
  case RK_Integer:
    return SymbolicValue::getInteger(getIntegerValue(), astContext);
  case RK_Aggregate: {
    auto elts = getAggregateValue();
    SmallVector<SymbolicValue, 4> results;
    results.reserve(elts.size());
    for (auto elt : elts)
      results.push_back(elt.cloneInto(astContext));
    return getAggregate(results, astContext);
  }
  }
}

//===----------------------------------------------------------------------===//
// Integers
//===----------------------------------------------------------------------===//

SymbolicValue SymbolicValue::getInteger(int64_t value, unsigned bitWidth) {
  SymbolicValue result;
  result.representationKind = RK_IntegerInline;
  result.value.integerInline = value;
  result.auxInfo.integerBitwidth = bitWidth;
  return result;
}

SymbolicValue SymbolicValue::getInteger(const APInt &value,
                                        ASTContext &astContext) {
  // In the common case, we can form an inline representation.
  unsigned numWords = value.getNumWords();
  if (numWords == 1)
    return getInteger(value.getRawData()[0], value.getBitWidth());

  // Copy the integers from the APInt into the bump pointer.
  auto *words = astContext.Allocate<uint64_t>(numWords).data();
  std::uninitialized_copy(value.getRawData(), value.getRawData() + numWords,
                          words);

  SymbolicValue result;
  result.representationKind = RK_Integer;
  result.value.integer = words;
  result.auxInfo.integerBitwidth = value.getBitWidth();
  return result;
}

APInt SymbolicValue::getIntegerValue() const {
  assert(getKind() == Integer);
  if (representationKind == RK_IntegerInline) {
    auto numBits = auxInfo.integerBitwidth;
    return APInt(numBits, value.integerInline);
  }

  assert(representationKind == RK_Integer);
  auto numBits = auxInfo.integerBitwidth;
  auto numWords =
      (numBits + APInt::APINT_BITS_PER_WORD - 1) / APInt::APINT_BITS_PER_WORD;
  return APInt(numBits, {value.integer, numWords});
}

unsigned SymbolicValue::getIntegerValueBitWidth() const {
  assert(getKind() == Integer);
  assert (representationKind == RK_IntegerInline ||
          representationKind == RK_Integer);
  return auxInfo.integerBitwidth;
}

//===----------------------------------------------------------------------===//
// Aggregates
//===----------------------------------------------------------------------===//

/// This returns a constant Symbolic value with the specified elements in it.
/// This assumes that the elements lifetime has been managed for this.
SymbolicValue SymbolicValue::getAggregate(ArrayRef<SymbolicValue> elements,
                                          ASTContext &astContext) {
  // Copy the elements into the bump pointer.
  auto *resultElts =
      astContext.Allocate<SymbolicValue>(elements.size()).data();
  std::uninitialized_copy(elements.begin(), elements.end(), resultElts);

  SymbolicValue result;
  result.representationKind = RK_Aggregate;
  result.value.aggregate = resultElts;
  result.auxInfo.aggregateNumElements = elements.size();
  return result;
}

ArrayRef<SymbolicValue> SymbolicValue::getAggregateValue() const {
  assert(getKind() == Aggregate);
  return ArrayRef<SymbolicValue>(value.aggregate, auxInfo.aggregateNumElements);
}

//===----------------------------------------------------------------------===//
// Unknown
//===----------------------------------------------------------------------===//

namespace swift {
/// When the value is Unknown, this contains information about the unfoldable
/// part of the computation.
struct alignas(SourceLoc) UnknownSymbolicValue final
    : private llvm::TrailingObjects<UnknownSymbolicValue, SourceLoc> {
  friend class llvm::TrailingObjects<UnknownSymbolicValue, SourceLoc>;

  /// The value that was unfoldable.
  SILNode *node;

  /// A more explanatory reason for the value being unknown.
  UnknownReason reason;

  /// The number of elements in the call stack.
  unsigned callStackSize;

  static UnknownSymbolicValue *create(SILNode *node, UnknownReason reason,
                                      ArrayRef<SourceLoc> elements,
                                      ASTContext &astContext) {
    auto byteSize =
        UnknownSymbolicValue::totalSizeToAlloc<SourceLoc>(elements.size());
    auto rawMem = astContext.Allocate(byteSize, alignof(UnknownSymbolicValue));

    // Placement-new the value inside the memory we just allocated.
    auto value = ::new (rawMem) UnknownSymbolicValue(
        node, reason, static_cast<unsigned>(elements.size()));
    std::uninitialized_copy(elements.begin(), elements.end(),
                            value->getTrailingObjects<SourceLoc>());
    return value;
  }

  ArrayRef<SourceLoc> getCallStack() const {
    return {getTrailingObjects<SourceLoc>(), callStackSize};
  }

  // This is used by the llvm::TrailingObjects base class.
  size_t numTrailingObjects(OverloadToken<SourceLoc>) const {
    return callStackSize;
  }

private:
  UnknownSymbolicValue() = delete;
  UnknownSymbolicValue(const UnknownSymbolicValue &) = delete;
  UnknownSymbolicValue(SILNode *node, UnknownReason reason,
                       unsigned callStackSize)
      : node(node), reason(reason), callStackSize(callStackSize) {}
};
} // namespace swift

SymbolicValue SymbolicValue::getUnknown(SILNode *node, UnknownReason reason,
                                        llvm::ArrayRef<SourceLoc> callStack,
                                        ASTContext &astContext) {
  assert(node && "node must be present");
  SymbolicValue result;
  result.representationKind = RK_Unknown;
  result.value.unknown =
      UnknownSymbolicValue::create(node, reason, callStack, astContext);
  return result;
}

ArrayRef<SourceLoc> SymbolicValue::getUnknownCallStack() const {
  assert(getKind() == Unknown);
  return value.unknown->getCallStack();
}

SILNode *SymbolicValue::getUnknownNode() const {
  assert(getKind() == Unknown);
  return value.unknown->node;
}

UnknownReason SymbolicValue::getUnknownReason() const {
  assert(getKind() == Unknown);
  return value.unknown->reason;
}

//===----------------------------------------------------------------------===//
// Higher level code
//===----------------------------------------------------------------------===//

/// The SIL location for operations we process are usually deep in the bowels
/// of inlined code from opaque libraries, which are all implementation details
/// to the user.  As such, walk the inlining location of the specified node to
/// return the first location *outside* opaque libraries.
static SILDebugLocation skipInternalLocations(SILDebugLocation loc) {
  auto ds = loc.getScope();

  if (!ds || loc.getLocation().getSourceLoc().isValid())
    return loc;

  // Zip through inlined call site information that came from the
  // implementation guts of the library.  We want to report the message inside
  // the user's code, not in the guts we inlined through.
  for (; auto ics = ds->InlinedCallSite; ds = ics) {
    // If we found a valid inlined-into location, then we are good.
    if (ds->Loc.getSourceLoc().isValid())
      return SILDebugLocation(ds->Loc, ds);
    if (SILFunction *F = ds->getInlinedFunction()) {
      if (F->getLocation().getSourceLoc().isValid())
        break;
    }
  }

  if (ds->Loc.getSourceLoc().isValid())
    return SILDebugLocation(ds->Loc, ds);

  return loc;
}

/// Dig through single element aggregates, return the ultimate thing inside of
/// it.  This is useful when dealing with integers and floats, because they
/// are often wrapped in single-element struct wrappers.
SymbolicValue SymbolicValue::lookThroughSingleElementAggregates() const {
  auto result = *this;
  while (1) {
    if (result.getKind() != Aggregate)
      return result;
    auto elts = result.getAggregateValue();
    if (elts.size() != 1)
      return result;
    result = elts[0];
  }
}

/// Emits an explanatory note if there is useful information to note or if there
/// is an interesting SourceLoc to point at.
/// Returns true if a diagnostic was emitted.
static bool emitNoteDiagnostic(SILInstruction *badInst, UnknownReason reason,
                               SILLocation fallbackLoc) {
  auto loc = skipInternalLocations(badInst->getDebugLocation()).getLocation();
  if (loc.isNull()) {
    // If we have important clarifying information, make sure to emit it.
    if (reason == UnknownReason::Default || fallbackLoc.isNull())
      return false;
    loc = fallbackLoc;
  }

  auto &ctx = badInst->getModule().getASTContext();
  auto sourceLoc = loc.getSourceLoc();
  switch (reason) {
  case UnknownReason::Default:
    diagnose(ctx, sourceLoc, diag::constexpr_unknown_reason_default)
        .highlight(loc.getSourceRange());
    break;
  case UnknownReason::TooManyInstructions:
    // TODO: Should pop up a level of the stack trace.
    diagnose(ctx, sourceLoc, diag::constexpr_too_many_instructions,
             ConstExprLimit)
        .highlight(loc.getSourceRange());
    break;
  case UnknownReason::Loop:
    diagnose(ctx, sourceLoc, diag::constexpr_loop)
        .highlight(loc.getSourceRange());
    break;
  case UnknownReason::Overflow:
    diagnose(ctx, sourceLoc, diag::constexpr_overflow)
        .highlight(loc.getSourceRange());
    break;
  case UnknownReason::Trap:
    diagnose(ctx, sourceLoc, diag::constexpr_trap)
        .highlight(loc.getSourceRange());
    break;
  }
  return true;
}

/// Given that this is an 'Unknown' value, emit diagnostic notes providing
/// context about what the problem is.
void SymbolicValue::emitUnknownDiagnosticNotes(SILLocation fallbackLoc) {
  auto badInst = dyn_cast<SILInstruction>(getUnknownNode());
  if (!badInst)
    return;

  bool emittedFirstNote = emitNoteDiagnostic(badInst, getUnknownReason(),
                                             fallbackLoc);

  auto sourceLoc = fallbackLoc.getSourceLoc();
  auto &module = badInst->getModule();
  if (sourceLoc.isInvalid()) {
    diagnose(module.getASTContext(), sourceLoc, diag::constexpr_not_evaluable);
    return;
  }
  for (auto &sourceLoc : llvm::reverse(getUnknownCallStack())) {
    // Skip unknown sources.
    if (!sourceLoc.isValid())
      continue;

    auto diag = emittedFirstNote ? diag::constexpr_called_from
                                 : diag::constexpr_not_evaluable;
    diagnose(module.getASTContext(), sourceLoc, diag);
    emittedFirstNote = true;
  }
}
