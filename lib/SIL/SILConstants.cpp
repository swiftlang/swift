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
  case RK_IntegerInline:
    os << "int: " << getIntegerValue() << "\n";
    return;
  case RK_Float:
  case RK_Float32:
  case RK_Float64:
    os << "float: ";
    getFloatValue().print(os);
    os << "\n";
    return;
  case RK_String:
    os << "string: \"" << getStringValue() << "\"\n";
    return;
  case RK_Aggregate: {
    ArrayRef<SymbolicValue> elements = getAggregateValue();
    switch (elements.size()) {
    case 0:
      os << "agg: 0 elements []\n";
      return;
    case 1:
      os << "agg: 1 elt: ";
      elements[0].print(os, indent+2);
      return;
    default:
      os << "agg: " << elements.size() << " elements [\n";
      for (auto elt : elements)
        elt.print(os, indent+2);
      os.indent(indent) << "]\n";
      return;
    }
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
  case RK_Array:
  case RK_ArrayAddress: {
    CanType elementType;
    ArrayRef<SymbolicValue> elements = getArrayValue(elementType);
    os << "array<" << elementType << ">: " << elements.size();
    switch (elements.size()) {
    case 0:
      os << " elements []\n";
      return;
    case 1:
      os << " elt: ";
      elements[0].print(os, indent+2);
      return;
    default:
      os << " elements [\n";
      for (auto elt : elements)
        elt.print(os, indent+2);
      os.indent(indent) << "]\n";
      return;
    }
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
  case RK_IntegerInline:
    return Integer;
  case RK_Float:
  case RK_Float32:
  case RK_Float64:
    return Float;
  case RK_String:
    return String;
  case RK_DirectAddress:
  case RK_DerivedAddress:
    return Address;
  case RK_Array:
  case RK_ArrayAddress:
    return Array;
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
  auto thisRK = representationKind;
  // If this is an instruction kind, map onto the thing that will cause a copy.
  if (thisRK == RK_Inst) {
    auto *inst = value.inst;
    if (isa<IntegerLiteralInst>(inst))
      thisRK = RK_Integer;
    else if (isa<FloatLiteralInst>(inst))
      thisRK = RK_Float;
    else {
      assert(isa<StringLiteralInst>(inst) && "Unknown ConstantInst kind");
      thisRK = RK_String;
    }
  }

  switch (thisRK) {
  case RK_Inst: assert(0 && "should be remapped above");
  case RK_UninitMemory:
  case RK_Unknown:
  case RK_Metatype:
  case RK_Function:
  case RK_Enum:
  case RK_IntegerInline:
  case RK_Float32:
  case RK_Float64:
    // These have trivial inline storage, just return a copy.
    return *this;
  case RK_Integer:
    return SymbolicValue::getInteger(getIntegerValue(), allocator);
    case RK_Float:
    return SymbolicValue::getFloat(getFloatValue(), allocator);
  case RK_String:
    return SymbolicValue::getString(getStringValue(), allocator);
  case RK_Aggregate: {
    auto elts = getAggregateValue();
    SmallVector<SymbolicValue, 4> results;
    results.reserve(elts.size());
    for (auto elt : elts)
      results.push_back(elt.cloneInto(allocator));
    return getAggregate(results, allocator);
  }
  case RK_EnumWithPayload:
    return getEnumWithPayload(getEnumValue(), getEnumPayloadValue(), allocator);
  case RK_DirectAddress:
  case RK_DerivedAddress: {
    SmallVector<unsigned, 4> accessPath;
    auto *memObject = getAddressValue(accessPath);
    auto *newMemObject =
      SymbolicValueMemoryObject::create(memObject->getType(),
                                        memObject->getValue(), allocator);
    return getAddress(newMemObject, accessPath, allocator);
  }
  case RK_Array:
  case RK_ArrayAddress: {
    CanType elementType;
    auto elts = getArrayValue(elementType);
    SmallVector<SymbolicValue, 4> results;
    results.reserve(elts.size());
    for (auto elt : elts)
      results.push_back(elt.cloneInto(allocator));
    return getArray(results, elementType, allocator);
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

SymbolicValue SymbolicValue::getInteger(int64_t value, unsigned bitWidth) {
  SymbolicValue result;
  result.representationKind = RK_IntegerInline;
  result.value.integerInline = value;
  result.aux.integer_bitwidth = bitWidth;
  return result;
}


SymbolicValue SymbolicValue::getInteger(const APInt &value,
                                        llvm::BumpPtrAllocator &allocator) {
  // In the common case, we can form an inline representation.
  unsigned numWords = value.getNumWords();
  if (numWords == 1)
    return getInteger(value.getRawData()[0], value.getBitWidth());

  // Copy the integers from the APInt into the bump pointer.
  auto *words = allocator.Allocate<uint64_t>(numWords);
  std::uninitialized_copy(value.getRawData(), value.getRawData()+numWords,
                          words);

  SymbolicValue result;
  result.representationKind = RK_Integer;
  result.value.integer = words;
  result.aux.integer_bitwidth = value.getBitWidth();
  return result;
}

APInt SymbolicValue::getIntegerValue() const {
  assert(getKind() == Integer);
  if (representationKind == RK_IntegerInline) {
    auto numBits = aux.integer_bitwidth;
    return APInt(numBits, value.integerInline);
  }

  if (representationKind == RK_Integer) {
    auto numBits = aux.integer_bitwidth;
    auto numWords = (numBits+63)/64;
    return APInt(numBits, { value.integer, numWords });
  }

  assert(representationKind == RK_Inst);
  return cast<IntegerLiteralInst>(value.inst)->getValue();
}

unsigned SymbolicValue::getIntegerValueBitWidth() const {
  assert(getKind() == Integer);
  if (representationKind == RK_IntegerInline ||
      representationKind == RK_Integer)
    return aux.integer_bitwidth;

  assert(representationKind == RK_Inst);
  return cast<IntegerLiteralInst>(value.inst)->getValue().getBitWidth();
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
  // We have a lot of floats and doubles, store them with an inline
  // representation.
  auto &semantics = value.getSemantics();
  if (&semantics == &APFloat::IEEEsingle()) {
    SymbolicValue result;
    result.representationKind = RK_Float32;
    result.value.float32 = value.convertToFloat();
    return result;
  }
  if (&semantics == &APFloat::IEEEdouble()) {
    SymbolicValue result;
    result.representationKind = RK_Float64;
    result.value.float64 = value.convertToDouble();
    return result;
  }

  // Handle exotic formats with general support logic.
  APInt val = value.bitcastToAPInt();

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

  if (representationKind == RK_Float32)
    return APFloat(value.float32);
  if (representationKind == RK_Float64)
    return APFloat(value.float64);

  if (representationKind == RK_Float)
    return value.floatingPoint->getValue();

  assert(representationKind == RK_Inst);
  return cast<FloatLiteralInst>(value.inst)->getValue();
}

const llvm::fltSemantics *SymbolicValue::getFloatValueSemantics() const {
  assert(getKind() == Float);

  if (representationKind == RK_Float32)
    return &APFloat::IEEEsingle();
  if (representationKind == RK_Float64)
    return &APFloat::IEEEdouble();

  if (representationKind == RK_Float)
    return &value.floatingPoint->semantics;

  assert(representationKind == RK_Inst);
  return &getFloatValue().getSemantics();
}


//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//

// Returns a SymbolicValue representing a UTF-8 encoded string.
SymbolicValue SymbolicValue::getString(StringRef string,
                                       llvm::BumpPtrAllocator &allocator) {
  // TODO: Could have an inline representation for strings if thre was demand,
  // just store a char[8] as the storage.

  auto *resultPtr = allocator.Allocate<char>(string.size());
  std::uninitialized_copy(string.begin(), string.end(), resultPtr);

  SymbolicValue result;
  result.representationKind = RK_String;
  result.value.string = resultPtr;
  result.aux.string_numBytes = string.size();
  return result;
}

// Returns the UTF-8 encoded string underlying a SymbolicValue.
StringRef SymbolicValue::getStringValue() const {
  assert(getKind() == String);

  if (representationKind == RK_String)
    return StringRef(value.string, aux.string_numBytes);

  assert(representationKind == RK_Inst);
  return cast<StringLiteralInst>(value.inst)->getValue();
}

//===----------------------------------------------------------------------===//
// Aggregates
//===----------------------------------------------------------------------===//

/// This returns a constant Symbolic value with the specified elements in it.
/// This assumes that the elements lifetime has been managed for this.
SymbolicValue SymbolicValue::getAggregate(ArrayRef<SymbolicValue> elements,
                                          llvm::BumpPtrAllocator &allocator) {
  // Copy the integers from the APInt into the bump pointer.
  auto *resultElts = allocator.Allocate<SymbolicValue>(elements.size());
  std::uninitialized_copy(elements.begin(), elements.end(), resultElts);

  SymbolicValue result;
  result.representationKind = RK_Aggregate;
  result.value.aggregate = resultElts;
  result.aux.aggregate_numElements = elements.size();
  return result;
}

ArrayRef<SymbolicValue> SymbolicValue::getAggregateValue() const {
  assert(getKind() == Aggregate);
  return ArrayRef<SymbolicValue>(value.aggregate, aux.aggregate_numElements);
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
  DerivedAddressValue(const DerivedAddressValue &) = delete;
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
// Arrays
//===----------------------------------------------------------------------===//

namespace swift {

/// This is the representation of a derived address.  A derived address refers
/// to a memory object along with an access path that drills into it.
struct ArraySymbolicValue final
: private llvm::TrailingObjects<ArraySymbolicValue, SymbolicValue> {
  friend class llvm::TrailingObjects<ArraySymbolicValue, SymbolicValue>;

  const CanType elementType;

  /// This is the number of indices in the derived address.
  const unsigned numElements;

  static ArraySymbolicValue *create(ArrayRef<SymbolicValue> elements,
                                    CanType elementType,
                                    llvm::BumpPtrAllocator &allocator) {
    auto byteSize =
      ArraySymbolicValue::totalSizeToAlloc<SymbolicValue>(elements.size());
    auto rawMem = allocator.Allocate(byteSize, alignof(ArraySymbolicValue));

    //  Placement initialize the object.
    auto asv = ::new (rawMem) ArraySymbolicValue(elementType, elements.size());
    std::uninitialized_copy(elements.begin(), elements.end(),
                            asv->getTrailingObjects<SymbolicValue>());
    return asv;
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
  ArraySymbolicValue() = delete;
  ArraySymbolicValue(const ArraySymbolicValue &) = delete;
  ArraySymbolicValue(CanType elementType, unsigned numElements)
   : elementType(elementType), numElements(numElements) {}
};
} // end namespace swift

/// Produce an array of elements.
SymbolicValue SymbolicValue::getArray(ArrayRef<SymbolicValue> elements,
                                      CanType elementType,
                                      llvm::BumpPtrAllocator &allocator) {
  // TODO: Could compress the empty array representation if there were a reason
  // to.
  auto asv = ArraySymbolicValue::create(elements, elementType, allocator);
  SymbolicValue result;
  result.representationKind = RK_Array;
  result.value.array = asv;
  return result;
}

ArrayRef<SymbolicValue>
SymbolicValue::getArrayValue(CanType &elementType) const {
  assert(getKind() == Array);
  auto val = *this;
  if (representationKind == RK_ArrayAddress)
    val = value.arrayAddress->getValue();
  
  assert(val.representationKind == RK_Array);

  elementType = val.value.array->elementType;
  return val.value.array->getElements();
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
