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

// SWIFT_ENABLE_TENSORFLOW

#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/Demangling/Demangle.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "llvm/Support/TrailingObjects.h"
using namespace swift;

template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

//===----------------------------------------------------------------------===//
// SymbolicValue implementation
//===----------------------------------------------------------------------===//

void SymbolicValue::print(llvm::raw_ostream &os, unsigned indent) const {
  os.indent(indent);
  switch (representationKind) {
  case RK_UninitMemory: os << "uninit\n"; return;
  case RK_Unknown: {
    std::pair<SILNode *, UnknownReason> unknown = getUnknownValue();
    os << "unknown(" << (int)unknown.second << "): ";
    unknown.first->dump();
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
  case RK_Inst:
    os << "inst: ";
    value.inst->dump();
    return;
  case RK_Integer:
    os << "int: " << getIntegerValue() << "\n";
    return;
  case RK_Float:
    os << "float: ";
    getFloatValue().print(os);
    os << "\n";
    return;
  case RK_String:
    os << "string: \"" << getStringValue() << "\"\n";
    return;
  case RK_Aggregate: {
    ArrayRef<SymbolicValue> elements = getAggregateValue();
    if (elements.empty()) {
      os << "agg: 0 elements []\n";
      return;
    } else if (elements.size() == 1) {
      os << "agg: 1 elt: ";
      elements[0].print(os, indent+2);
      return;
    }
    os << "agg: " << elements.size() << " element" << "s"[elements.size() == 1]
       << " [\n";
    for (auto elt : elements)
      elt.print(os, indent+2);
    os.indent(indent) << "]\n";
    return;
  }
  case RK_Enum: {
    auto *decl = getEnumValue();
    os << "enum: ";
    decl->print(os);
    return;
  }
  case RK_EnumWithPayload: {
    auto *decl = getEnumValue();
    os << "enum: ";
    decl->print(os);
    os <<", payload: ";
    getEnumPayloadValue().print(os, indent);
    return;
  }
  case RK_DirectAddress:
  case RK_DerivedAddress: {
    SmallVector<unsigned, 4> accessPath;
    SymbolicValueMemoryObject *memObject = getAddressValue(accessPath);
    os << "Address[" << memObject->getType() << "] ";
    interleave(accessPath.begin(), accessPath.end(),
               [&](unsigned idx) { os << idx; },
               [&]() { os << ", "; });
    os << "\n";
    break;
  }
  }
}

void SymbolicValue::dump() const {
  print(llvm::errs());
}

/// For constant values, return the classification of this value.  We have
/// multiple forms for efficiency, but provide a simpler interface to clients.
SymbolicValue::Kind SymbolicValue::getKind() const {
  switch (representationKind) {
  case RK_UninitMemory:
    return UninitMemory;
  case RK_Unknown:
    return Unknown;
  case RK_Metatype:
    return Metatype;
  case RK_Function:
    return Function;
  case RK_Aggregate:
    return Aggregate;
  case RK_Enum:
    return Enum;
  case RK_EnumWithPayload:
    return EnumWithPayload;
  case RK_Integer:
    return Integer;
  case RK_Float:
    return Float;
  case RK_String:
    return String;
  case RK_DirectAddress:
  case RK_DerivedAddress:
    return Address;
  case RK_Inst:
    auto *inst = value.inst;
    if (isa<IntegerLiteralInst>(inst))
      return Integer;
    if (isa<FloatLiteralInst>(inst))
      return Float;
    assert(isa<StringLiteralInst>(inst) && "Unknown ConstantInst kind");
    return String;
  }
}

/// Clone this SymbolicValue into the specified allocator and return the new
/// version.  This only works for valid constants.
SymbolicValue SymbolicValue::cloneInto(llvm::BumpPtrAllocator &allocator) const{
  switch (getKind()) {
  case Unknown:
  case UninitMemory:
  case Metatype:
  case Function:
  case Enum:
    // These have trivial inline storage, just return a copy.
    return *this;
  case Integer:
    return SymbolicValue::getInteger(getIntegerValue(), allocator);
  case Float:
    return SymbolicValue::getFloat(getFloatValue(), allocator);
  case String:
    return SymbolicValue::getString(getStringValue(), allocator);
  case Aggregate: {
    auto elts = getAggregateValue();
    SmallVector<SymbolicValue, 4> results;
    results.reserve(elts.size());
    for (auto elt : elts)
      results.push_back(elt.cloneInto(allocator));
    return getAggregate(results, allocator);
  }
  case EnumWithPayload:
    return getEnumWithPayload(getEnumValue(), getEnumPayloadValue(), allocator);
  case Address: {
    SmallVector<unsigned, 4> accessPath;
    auto *memObject = getAddressValue(accessPath);
    auto *newMemObject =
      SymbolicValueMemoryObject::create(memObject->getType(),
                                        memObject->getValue(), allocator);
    return getAddress(newMemObject, accessPath, allocator);
  }
  }
}

//===----------------------------------------------------------------------===//
// SymbolicValueMemoryObject implementation
//===----------------------------------------------------------------------===//

SymbolicValueMemoryObject *
SymbolicValueMemoryObject::create(Type type, SymbolicValue value,
                                  llvm::BumpPtrAllocator &allocator) {
  auto result = allocator.Allocate<SymbolicValueMemoryObject>();
  new (result) SymbolicValueMemoryObject(type, value);
  return result;
}



//===----------------------------------------------------------------------===//
// Integers
//===----------------------------------------------------------------------===//

namespace swift {
/// This is a representation of an integer value, stored as a trailing array
/// of words.  Elements of this value are bump-pointer allocated.
struct alignas(uint64_t) APIntSymbolicValue final
  : private llvm::TrailingObjects<APIntSymbolicValue, uint64_t> {
    friend class llvm::TrailingObjects<APIntSymbolicValue, uint64_t>;

  /// The number of words in the trailing array and # bits of the value.
  const unsigned numWords, numBits;

  static APIntSymbolicValue *create(unsigned numBits,
                                    ArrayRef<uint64_t> elements,
                                    llvm::BumpPtrAllocator &allocator) {
    auto byteSize =
      APIntSymbolicValue::totalSizeToAlloc<uint64_t>(elements.size());
    auto rawMem = allocator.Allocate(byteSize, alignof(APIntSymbolicValue));

    //  Placement initialize the APIntSymbolicValue.
    auto ilv = ::new (rawMem) APIntSymbolicValue(numBits, elements.size());
    std::uninitialized_copy(elements.begin(), elements.end(),
                            ilv->getTrailingObjects<uint64_t>());
    return ilv;
  }

  APInt getValue() const {
    return APInt(numBits, { getTrailingObjects<uint64_t>(), numWords });
  }

  // This is used by the llvm::TrailingObjects base class.
  size_t numTrailingObjects(OverloadToken<uint64_t>) const {
    return numWords;
  }
private:
  APIntSymbolicValue() = delete;
  APIntSymbolicValue(const APIntSymbolicValue &) = delete;
  APIntSymbolicValue(unsigned numBits, unsigned numWords)
    : numWords(numWords), numBits(numBits) {}
};
} // end namespace swift

SymbolicValue SymbolicValue::getInteger(const APInt &value,
                                        llvm::BumpPtrAllocator &allocator) {
  // TODO: Could store these inline in the union in the common case.
  auto intValue =
    APIntSymbolicValue::create(value.getBitWidth(),
                               { value.getRawData(), value.getNumWords()},
                               allocator);
  assert(intValue && "Integer value must be present");
  SymbolicValue result;
  result.representationKind = RK_Integer;
  result.value.integer = intValue;
  return result;
}

APInt SymbolicValue::getIntegerValue() const {
  assert(getKind() == Integer);
  if (representationKind == RK_Integer)
    return value.integer->getValue();

  assert(representationKind == RK_Inst);
  // TODO: Will eventually support the bump-pointer allocated folded int value.
  return cast<IntegerLiteralInst>(value.inst)->getValue();
}

//===----------------------------------------------------------------------===//
// Floats
//===----------------------------------------------------------------------===//

namespace swift {
/// This is a representation of a floating point value, stored as a trailing
/// array of words.  Elements of this value are bump-pointer allocated.
struct alignas(uint64_t) APFloatSymbolicValue final
  : private llvm::TrailingObjects<APFloatSymbolicValue, uint64_t> {
    friend class llvm::TrailingObjects<APFloatSymbolicValue, uint64_t>;

  const llvm::fltSemantics &semantics;

  static APFloatSymbolicValue *create(const llvm::fltSemantics &semantics,
                                     ArrayRef<uint64_t> elements,
                                     llvm::BumpPtrAllocator &allocator) {
    assert((APFloat::getSizeInBits(semantics)+63)/64 == elements.size());

    auto byteSize =
      APFloatSymbolicValue::totalSizeToAlloc<uint64_t>(elements.size());
    auto rawMem = allocator.Allocate(byteSize, alignof(APFloatSymbolicValue));

    //  Placement initialize the APFloatSymbolicValue.
    auto ilv = ::new (rawMem) APFloatSymbolicValue(semantics);
    std::uninitialized_copy(elements.begin(), elements.end(),
                            ilv->getTrailingObjects<uint64_t>());
    return ilv;
  }

  APFloat getValue() const {
    auto val = APInt(APFloat::getSizeInBits(semantics),
                     { getTrailingObjects<uint64_t>(),
                       numTrailingObjects(OverloadToken<uint64_t>())
                     });
    return APFloat(semantics, val);
  }

  // This is used by the llvm::TrailingObjects base class.
  size_t numTrailingObjects(OverloadToken<uint64_t>) const {
    return (APFloat::getSizeInBits(semantics)+63)/64;
  }
private:
  APFloatSymbolicValue() = delete;
  APFloatSymbolicValue(const APFloatSymbolicValue &) = delete;
  APFloatSymbolicValue(const llvm::fltSemantics &semantics)
    : semantics(semantics) {}
};
} // end namespace swift


SymbolicValue SymbolicValue::getFloat(const APFloat &value,
                                      llvm::BumpPtrAllocator &allocator) {
  APInt val = value.bitcastToAPInt();

  // TODO: Could store these inline in the union in the common case.
  auto fpValue =
    APFloatSymbolicValue::create(value.getSemantics(),
                                 { val.getRawData(), val.getNumWords()},
                                 allocator);
  assert(fpValue && "Floating point value must be present");
  SymbolicValue result;
  result.representationKind = RK_Float;
  result.value.floatingPoint = fpValue;
  return result;
}

APFloat SymbolicValue::getFloatValue() const {
  assert(getKind() == Float);

  if (representationKind == RK_Float)
    return value.floatingPoint->getValue();

  assert(representationKind == RK_Inst);
  return cast<FloatLiteralInst>(value.inst)->getValue();
}

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//

namespace swift {
/// This is a representation of an UTF-8 encoded string, stored as a trailing
/// array of bytes.  Elements of this value are bump-pointer allocated.
struct alignas(uint64_t) StringSymbolicValue final
  : private llvm::TrailingObjects<StringSymbolicValue, char> {
    friend class llvm::TrailingObjects<StringSymbolicValue, char>;

  /// The number of bytes in the trailing array.
  const unsigned numBytes;

  static StringSymbolicValue *create(const StringRef string,
                                     llvm::BumpPtrAllocator &allocator) {
    auto size = StringSymbolicValue::totalSizeToAlloc<char>(string.size());
    auto rawMem = allocator.Allocate(size, alignof(StringSymbolicValue));

    // Placement initialize the StringSymbolicValue.
    auto ilv = ::new (rawMem) StringSymbolicValue(string.size());
    std::uninitialized_copy(string.begin(), string.end(),
                            ilv->getTrailingObjects<char>());
    return ilv;
  }

  StringRef getValue() const {
    return {
      getTrailingObjects<char>(), numTrailingObjects(OverloadToken<char>())
    };
  }

  // This is used by the llvm::TrailingObjects base class.
  size_t numTrailingObjects(OverloadToken<char>) const {
    return numBytes;
  }
private:
  StringSymbolicValue() = delete;
  StringSymbolicValue(const StringSymbolicValue &) = delete;
  StringSymbolicValue(unsigned numBytes) :
    numBytes(numBytes) {}
};
} // end namespace swift

// Returns a SymbolicValue representing a UTF-8 encoded string.
SymbolicValue SymbolicValue::getString(const StringRef string,
                                       llvm::BumpPtrAllocator &allocator) {
  auto stringValue = StringSymbolicValue::create(string, allocator);
  assert(stringValue && "String value must be present");
  SymbolicValue result;
  result.representationKind = RK_String;
  result.value.string = stringValue;
  return result;
}

// Returns the UTF-8 encoded string underlying a SymbolicValue.
StringRef SymbolicValue::getStringValue() const {
  assert(getKind() == String);

  if (representationKind == RK_String)
    return value.string->getValue();

  assert(representationKind == RK_Inst);
  return cast<StringLiteralInst>(value.inst)->getValue();
}

//===----------------------------------------------------------------------===//
// Aggregates
//===----------------------------------------------------------------------===//

namespace swift {

/// This is the representation of a constant aggregate value.  It maintains
/// the elements as a trailing array of SymbolicValue's.  Note that single
/// element structs do not use this (as a performance optimization to reduce
/// allocations).
struct alignas(SymbolicValue) AggregateSymbolicValue final
: private llvm::TrailingObjects<AggregateSymbolicValue, SymbolicValue> {
  friend class llvm::TrailingObjects<AggregateSymbolicValue, SymbolicValue>;

  /// This is the number of elements in the aggregate.
  const unsigned numElements;

  static AggregateSymbolicValue *create(ArrayRef<SymbolicValue> elements,
                                       llvm::BumpPtrAllocator &allocator) {
    auto byteSize =
      AggregateSymbolicValue::totalSizeToAlloc<SymbolicValue>(elements.size());
    auto rawMem = allocator.Allocate(byteSize, alignof(AggregateSymbolicValue));

    //  Placement initialize the AggregateSymbolicValue.
    auto alv = ::new (rawMem) AggregateSymbolicValue(elements.size());
    std::uninitialized_copy(elements.begin(), elements.end(),
                            alv->getTrailingObjects<SymbolicValue>());
    return alv;
  }

  /// Return the element constants for this aggregate constant.  These are
  /// known to all be constants.
  ArrayRef<SymbolicValue> getElements() const {
    return { getTrailingObjects<SymbolicValue>(), numElements };
  }

  // This is used by the llvm::TrailingObjects base class.
  size_t numTrailingObjects(OverloadToken<SymbolicValue>) const {
    return numElements;
  }
private:
  AggregateSymbolicValue() = delete;
  AggregateSymbolicValue(const AggregateSymbolicValue &) = delete;
  AggregateSymbolicValue(unsigned numElements) : numElements(numElements) {}
};
} // end namespace swift


/// This returns a constant Symbolic value with the specified elements in it.
/// This assumes that the elements lifetime has been managed for this.
SymbolicValue SymbolicValue::getAggregate(ArrayRef<SymbolicValue> elements,
                                          llvm::BumpPtrAllocator &allocator) {
  auto aggregate = AggregateSymbolicValue::create(elements, allocator);
  assert(aggregate && "aggregate value must be present");
  SymbolicValue result;
  result.representationKind = RK_Aggregate;
  result.value.aggregate = aggregate;
  return result;
}

ArrayRef<SymbolicValue> SymbolicValue::getAggregateValue() const {
  assert(getKind() == Aggregate);
  return value.aggregate->getElements();
}

//===----------------------------------------------------------------------===//
// Enums
//===----------------------------------------------------------------------===//

namespace swift {

/// This is the representation of a constant enum value with payload.
struct EnumWithPayloadSymbolicValue final {
  /// The enum case.
  EnumElementDecl *enumDecl;
  SymbolicValue payload;

  EnumWithPayloadSymbolicValue(EnumElementDecl *decl, SymbolicValue payload)
    : enumDecl(decl), payload(payload) {
  }

private:
  EnumWithPayloadSymbolicValue() = delete;
  EnumWithPayloadSymbolicValue(const EnumWithPayloadSymbolicValue &) = delete;
};
} // end namespace swift

/// This returns a constant Symbolic value for the enum case in `decl` with a
/// payload.
SymbolicValue
SymbolicValue::getEnumWithPayload(EnumElementDecl *decl, SymbolicValue payload,
                                  llvm::BumpPtrAllocator &allocator) {
  assert(decl && payload.isConstant());
  auto rawMem = allocator.Allocate<EnumWithPayloadSymbolicValue>();
  auto enumVal = ::new (rawMem) EnumWithPayloadSymbolicValue(decl, payload);

  SymbolicValue result;
  result.representationKind = RK_EnumWithPayload;
  result.value.enumValWithPayload = enumVal;
  return result;
}

EnumElementDecl *SymbolicValue::getEnumValue() const {
  if (representationKind == RK_Enum)
    return value.enumVal;

  assert(representationKind == RK_EnumWithPayload);
  return value.enumValWithPayload->enumDecl;
}

SymbolicValue SymbolicValue::getEnumPayloadValue() const {
  assert(representationKind == RK_EnumWithPayload);
  return value.enumValWithPayload->payload;
}

//===----------------------------------------------------------------------===//
// Addresses
//===----------------------------------------------------------------------===//

namespace swift {

/// This is the representation of a derived address.  A derived address refers
/// to a memory object along with an access path that drills into it.
struct DerivedAddressValue final
: private llvm::TrailingObjects<DerivedAddressValue, unsigned> {
  friend class llvm::TrailingObjects<DerivedAddressValue, unsigned>;

  SymbolicValueMemoryObject *memoryObject;

  /// This is the number of indices in the derived address.
  const unsigned numElements;

  static DerivedAddressValue *create(SymbolicValueMemoryObject *memoryObject,
                                     ArrayRef<unsigned> elements,
                                     llvm::BumpPtrAllocator &allocator) {
    auto byteSize =
      DerivedAddressValue::totalSizeToAlloc<unsigned>(elements.size());
    auto rawMem = allocator.Allocate(byteSize, alignof(DerivedAddressValue));

    //  Placement initialize the object.
    auto dav = ::new (rawMem) DerivedAddressValue(memoryObject,
                                                  elements.size());
    std::uninitialized_copy(elements.begin(), elements.end(),
                            dav->getTrailingObjects<unsigned>());
    return dav;
  }

  /// Return the element constants for this aggregate constant.  These are
  /// known to all be constants.
  ArrayRef<unsigned> getElements() const {
    return { getTrailingObjects<unsigned>(), numElements };
  }

  // This is used by the llvm::TrailingObjects base class.
  size_t numTrailingObjects(OverloadToken<unsigned>) const {
    return numElements;
  }
private:
  DerivedAddressValue() = delete;
  DerivedAddressValue(const AggregateSymbolicValue &) = delete;
  DerivedAddressValue(SymbolicValueMemoryObject *memoryObject,
                      unsigned numElements)
   : memoryObject(memoryObject), numElements(numElements) {}
};
} // end namespace swift



/// Return a symbolic value that represents the address of a memory object
/// indexed by a path.
SymbolicValue SymbolicValue::
getAddress(SymbolicValueMemoryObject *memoryObject, ArrayRef<unsigned> indices,
           llvm::BumpPtrAllocator &allocator) {
  if (indices.empty())
    return getAddress(memoryObject);

  auto dav = DerivedAddressValue::create(memoryObject, indices, allocator);
  SymbolicValue result;
  result.representationKind = RK_DerivedAddress;
  result.value.derivedAddress = dav;
  return result;
}

/// Return the memory object of this reference along with any access path
/// indices involved.
SymbolicValueMemoryObject *SymbolicValue::
getAddressValue(SmallVectorImpl<unsigned> &accessPath) const {
  assert(getKind() == Address);

  accessPath.clear();
  if (representationKind == RK_DirectAddress)
    return value.directAddress;
  assert(representationKind == RK_DerivedAddress);

  auto *dav = value.derivedAddress;

  // The first entry is the object ID, the rest are indices in the accessPath.
  accessPath.assign(dav->getElements().begin(), dav->getElements().end());
  return dav->memoryObject;
}

/// Return just the memory object for an address value.
SymbolicValueMemoryObject *SymbolicValue::getAddressValueMemoryObject() const {
  if (representationKind == RK_DirectAddress)
    return value.directAddress;
  assert(representationKind == RK_DerivedAddress);
  return value.derivedAddress->memoryObject;
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
  // implementation guts of the tensor library.  We want to report the
  // message inside the user's code, not in the guts we inlined through.
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

/// Given that this is an 'Unknown' value, emit diagnostic notes providing
/// context about what the problem is.
void SymbolicValue::emitUnknownDiagnosticNotes(SILLocation fallbackLoc) {
  std::pair<SILNode *, UnknownReason> unknown = getUnknownValue();
  auto badInst = dyn_cast<SILInstruction>(unknown.first);
  if (!badInst) return;

  std::string error;
  switch (unknown.second) {
  case UnknownReason::Default:
    error = "could not fold operation";
    break;
  case UnknownReason::TooManyInstructions:
    // TODO: Should pop up a level of the stack trace.
    error = "expression is too large to evaluate at compile-time";
    break;
  case UnknownReason::Loop:
    error = "control flow loop found";
    break;
  case UnknownReason::Overflow:
    error = "integer overflow detected";
    break;
  case UnknownReason::Trap:
    error = "trap detected";
    break;
  }

  auto &module = badInst->getModule();

  auto loc = skipInternalLocations(badInst->getDebugLocation()).getLocation();
  if (loc.isNull()) {
    // If we have important clarifying information, make sure to emit it.
    if (unknown.second == UnknownReason::Default ||
        fallbackLoc.isNull())
      return;
    loc = fallbackLoc;
  }

  diagnose(module.getASTContext(), loc.getSourceLoc(),
           diag::tf_op_misuse_note, error)
    .highlight(loc.getSourceRange());
}
