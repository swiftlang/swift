//===--- TFUtilities.cpp - TensorFlow lowering utilities ------------------===//
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

#include "TFUtilities.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#ifdef SWIFT_ENABLE_TENSORFLOW
#ifdef CMAKE_INTDIR
#include "tensorflow/c/c_api.h"
#else
#include "tensorflow/c/c_api.h"
#endif
#endif

using namespace swift;
using namespace tf;

template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

static llvm::cl::opt<bool>
TFDumpIntermediates("tf-dump-intermediates", llvm::cl::init(false),
                    llvm::cl::desc("Dump intermediate results in TensorFlow passes"));

/// This returns true if we should dump out intermediate results to standard
/// out.  This is used for integration unit tests.
bool tf::shouldDumpIntermediates() {
  return TFDumpIntermediates;
}


/// If the specified type is the well-known TensorHandle<T> type, then return
/// "T".  If not, return a null type.
Type tf::isTensorHandle(Type ty) {
  if (auto bgct = ty->getAs<BoundGenericClassType>()) {
    if (bgct->getDecl()->getNameStr() == "TensorHandle") {
      assert(bgct->getGenericArgs().size() == 1 && "Expected one generic arg");
      return bgct->getGenericArgs()[0];
    }
  }
  return Type();
}

static bool is64(Type ty) {
  return ty->getASTContext().LangOpts.Target.isArch64Bit();
}

/// This function maps a Swift type (either a language type like Float or an
/// LLVM Builtin type like Builtin.f32) into the TensorFlow TF_DataType value.
///
/// This returns 0 (which is an invalid tensorflow type ID) on error.
///
unsigned tf::convertSwiftTypeToTF(Type ty) {
#ifdef SWIFT_ENABLE_TENSORFLOW
  // Handle wrappers like Float, which come up in TensorHandle<Float>
  if (auto *s = ty->getAs<StructType>()) {
    // Make sure the type is defined inside the Swift module.
    auto context = s->getDecl()->getDeclContext()->getParentModule();
    if (!context || context->getName().str() != "Swift")
      return 0;

    return llvm::StringSwitch<unsigned>(s->getDecl()->getNameStr())
      .Case("Bool", TF_BOOL)
      .Case("Int8", TF_INT8)
      .Case("UInt8", TF_UINT8)
      .Case("Int16", TF_INT16)
      .Case("UInt16", TF_UINT16)
      .Case("Int32", TF_INT32)
      .Case("UInt32", TF_UINT32)
      .Case("Int64", TF_INT64)
      .Case("UInt64", TF_UINT64)
      .Case("Int8", TF_INT8)
      .Case("UInt8", TF_UINT8)
      .Case("Float", TF_FLOAT)
      .Case("Double", TF_DOUBLE)
      .Case("Int", is64(s) ? TF_INT64 : TF_INT32)
      .Case("UInt", is64(s) ? TF_UINT64 : TF_UINT32)
      .Default(0);
  }

  // BuiltinIntegerType doesn't carry sign information, which TensorFlow needs,
  // so we can't rely on getting type information from the builtin types
  // themselves.  For now we'll just use signed types.
  if (auto *BII = ty->getAs<BuiltinIntegerType>()) {
    if (BII->getWidth().isPointerWidth())
      return is64(ty) ? TF_INT64 : TF_INT32;

    switch (BII->getFixedWidth()) {
    case 1: return TF_BOOL;
    case 8: return TF_INT8;
    case 16: return TF_INT16;
    case 32: return TF_INT32;
    case 64: return TF_INT64;
    }
  }

  if (auto *BIF = ty->getAs<BuiltinFloatType>()) {
    switch (BIF->getFPKind()) {
    case BuiltinFloatType::IEEE16: return TF_HALF;
    case BuiltinFloatType::IEEE32: return TF_FLOAT;
    case BuiltinFloatType::IEEE64: return TF_DOUBLE;
    case BuiltinFloatType::IEEE80:
    case BuiltinFloatType::IEEE128:
    case BuiltinFloatType::PPC128:
      return 0;
    }
  }
#endif
  return 0;
}

typedef std::pair<StringRef, SILTensorOpInfo::AttributeModifier> AttributeEntry;


/// Given a function name that might refer to a tensorflow op function, this
/// returns the op name and operand description and returns true.  If the
/// function name doesn't correspond to an op, this returns false.
static bool decodeTensorOpName(StringRef name, StringRef &opName,
                               StringRef &typeDescriptorStr,
                               SmallVectorImpl<AttributeEntry> &attributes) {
  // Op functions are expected to be of the form:
  //  __tfop_<OPNAME>,<OPERANDDESC>,<ATTRIBUTES>
  if (!name.startswith("__tfop_")) return false;
  name = name.substr(strlen("__tfop_"));

  auto pos = name.find(",");
  if (pos == StringRef::npos) return false;
  opName = name.substr(0, pos);
  name = name.substr(pos+strlen(","));

  pos = name.find(",");
  typeDescriptorStr = name.substr(0, pos);
  if (pos == StringRef::npos)
    return true;
  name = name.substr(pos);

  // Parse out any attribute names.
  while (!name.empty()) {
    assert(name[0] == ',');
    name = name.drop_front(1);

    pos = name.find(",");
    if (pos == StringRef::npos) pos = name.size();

    auto attrName = name.substr(0, pos);
    attributes.push_back({
      attrName, SILTensorOpInfo::AttributeModifier::Normal
    });
    name = name.substr(pos);
  }

  return true;
}


SILValue SILTensorOpInfo::getScalarOperand(SILValue v) const {
  // We have to handle two kinds of operands: SIL address operands and normal
  // values.
  if (!v->getType().isAddress()) {
    // If we have a normal operand, handle the form where a StructInst is
    // Swift stdlib type (e.g. Int/Float) wrapping an underlying LLVM value.
    if (auto *SI = dyn_cast<StructInst>(v))
      if (SI->getNumOperands() == 1)
        return SI->getOperand(0);

    return v;
  }

  // Because we're often coming from generic code, we frequently get a value
  // passed by-address.  Check for an alloc_stack with a single store to it and
  // consume the stored value.
  if (auto *ASI = dyn_cast<AllocStackInst>(v)) {
    if (auto *store = ASI->getSingleUserOfType<StoreInst>())
      return getScalarOperand(store->getSrc());
  }

  // Otherwise this is a by-address value that we can't handle:
  // FIXME: The proper way to deal with this is with a deabstraction pass,
  // which will guarantee generic specialization promotes the builtin operand
  // to never be an address.
  return SILValue();
}

/// If the specified value is a valid value for an attribute, return the
/// instruction that provides the value, otherwise null.
SingleValueInstruction *SILTensorOpInfo::getAttrOperand(SILValue v) const {
  // If the value is a string value, then we need to peel off all the SIL
  // instructions between the String struct value and the underlying
  // string_literal instruction.
  auto &ctx = inst->getFunction()->getASTContext();
  if (v->getType().getSwiftRValueType()->isEqual(
                                     ctx.getStringDecl()->getDeclaredType())) {
    auto str = v;
    // Strip off the specific set of instructions we expect to form the string
    // literal.
    while (1) {
      if (auto sli = dyn_cast<StringLiteralInst>(str))
        return sli->getEncoding() == StringLiteralInst::Encoding::UTF8
                ? sli : nullptr;

      if (auto si = dyn_cast<StructInst>(str)) {
        assert(si->getNumOperands() >= 1 &&
               "Expect String, UnsafeMutableRawPointer, and _StringCore types");
        str = si->getOperand(0);
        continue;
      }

      if (auto ei = dyn_cast<EnumInst>(str)) {
        assert(ei->getNumOperands() == 1 && "expect non-null optional");
        str = ei->getOperand();
        continue;
      }

      if (auto *ubc = dyn_cast<UncheckedBitwiseCastInst>(str)) {
        str = ubc->getOperand();
        continue;
      }

      // Look through the various operands that bit-mangle things into bridged
      // string representations.  This is gross, Swift should have higher level
      // operations for bridge values like this.
      if (auto *bi = dyn_cast<BuiltinInst>(str)) {
        switch (bi->getBuiltinInfo().ID) {
        case BuiltinValueKind::And:
        case BuiltinValueKind::Or:
        case BuiltinValueKind::ZExtOrBitCast:
        case BuiltinValueKind::PtrToInt:
          str = bi->getOperand(0);
          continue;
        default: break;
        }
      }

      // It is possible that we have a variable string, we want to reject it
      // as a non-constant value.
      return nullptr;
    }
  }

  // Handle cases that create a literal array.
  if (auto *si = dyn_cast<StructInst>(v)) {
    SmallVector<SingleValueInstruction*, 8> elements;
    Type elementType;
    if (decodeArrayElements(v, elements, elementType))
      return si;
  }

  // Simplify scalar operands in general.
  v = getScalarOperand(v);
  if (!v) return nullptr;

  // If we have an acceptable values for an attribute, return it.
  if (auto *fli = dyn_cast<FloatLiteralInst>(v))
    return fli;
  if (auto *ili = dyn_cast<IntegerLiteralInst>(v))
    return ili->getValue().getBitWidth() <= 64 ? ili : nullptr;
  if (auto *sli = dyn_cast<StringLiteralInst>(v))
    return sli->getEncoding() == StringLiteralInst::Encoding::UTF8
           ? sli : nullptr;
  if (auto *mti = dyn_cast<MetatypeInst>(v)) {
    auto ty = mti->getType().castTo<AnyMetatypeType>()->getInstanceType();
    if (convertSwiftTypeToTF(ty) != 0) return mti;
  }

  return nullptr;
}

/// Given a SILValue that may be an array, attempt to decode it into the
/// literal constant values that make up its elements.  If this fails or if
/// the value is not an array, this returns false.  Otherwise it decodes the
/// array and returns the element initializer in elements.
bool SILTensorOpInfo::
decodeArrayElements(SILValue value,
                    SmallVectorImpl<SingleValueInstruction*> &elements,
                    Type &elementType) const {
  auto bgst = value->getType().getAs<BoundGenericStructType>();
  if (!bgst ||
      bgst->getDecl() != inst->getModule().getASTContext().getArrayDecl())
    return false;
  elementType = bgst->getGenericArgs()[0];

  // Handle the standard patterns for array initialization.  'Value' is an
  // alloc_ref that is wrapped up in abstractions like this:
  //
  // %39 = alloc_ref [tail_elems $Int * %0 : $Builtin.Word] $_Contiguo....<Int>
  // %43 = unchecked_ref_cast %39 : $_ContiguousArrayStorage<Int> to ...
  // %44 = struct $_BridgeStorage<...> (%43 : $Builtin.BridgeObject)
  // %45 = struct $_ArrayBuffer<Int> (%44 : $_BridgeStorage<...>)
  // %46 = struct $Array<Int> (%45 : $_ArrayBuffer<Int>)
  //
  // Targets without ObjC bridging are slightly different, we handle both forms
  // here.
  AllocRefInst *allocRef = nullptr;
  while (!(allocRef = dyn_cast<AllocRefInst>(value))) {
    if (auto *si = dyn_cast<StructInst>(value)) {
      if (si->getNumOperands() != 1) return false;
      value = si->getOperand(0);
    } else if (auto *urci = dyn_cast<UncheckedRefCastInst>(value)) {
      value = urci->getOperand();
    } else if (auto *uci = dyn_cast<UpcastInst>(value)) {
      value = uci->getOperand();
    } else if (auto *globalValue = dyn_cast<GlobalValueInst>(value)) {
      // If we found a GlobalValueInst, then we're referring to an array that
      // got moved to being a static initializer.
      auto *init = dyn_cast_or_null<ObjectInst>(
              globalValue->getReferencedGlobal()->getStaticInitializerValue());
      if (!init) return false;

      // The initializer elements are the tail elements of the object_inst, see
      // if they are all decodable.
      for (auto elt : init->getTailElements()) {
        auto attrElt = getAttrOperand(elt);
        if (!attrElt) return false;
        elements.push_back(attrElt);
      }
      return true;
    } else if (auto *rptr = dyn_cast<RawPointerToRefInst>(value)) {
      // The empty array is specially recognized by the optimizer and
      // transformed into a well-known global produced by the standard library.
      // Uses of it look like this:
      //   %5 = global_addr @_swiftEmptyArrayStorage : $*_SwiftEmptyArrayStorage
      //   %6 = address_to_pointer %5 : $*_SwiftEmptyArrayStorage to $RawPointer
      //   %7 = raw_pointer_to_ref %6 : $RawPointer to $_EmptyArrayStorage
      //   %8 = unchecked_ref_cast %7 : $_EmptyArrayStorage to $BridgeObject
      auto a2p = dyn_cast<AddressToPointerInst>(rptr->getOperand());
      if (!a2p) return false;
      auto *ga = dyn_cast<GlobalAddrInst>(a2p->getOperand());

      elements.clear();
      return ga &&
             ga->getReferencedGlobal()->getName() == "_swiftEmptyArrayStorage";
    } else {
      return false;
    }
  }

  // The allocation must be of a constant number of elements.
  if (allocRef->getNumOperands() != 1 ||
      !isa<IntegerLiteralInst>(allocRef->getOperand(0)))
    return false;

  uint64_t numElements = cast<IntegerLiteralInst>(allocRef->getOperand(0))
                                            ->getValue().getLimitedValue();

  // Given the allocation, we then look for stores.  First there is going to be
  // an upcast to _ContiguousArrayStorageBase which is an internal
  // implementation detail that has the tail elements on it.  Then there will
  // be a ref_tail_addr, then indexed stores will hang off of it, like this:
  //
  // %40 = upcast %39 : $_ContiguousArrayStorage<Int> to $_ContiguousArra...
  // %47 = ref_tail_addr %40 : $_ContiguousArrayStorageBase, $Int
  // store %13 to %47 : $*Int
  // %49 = index_addr %47 : $*Int, %14 : $Builtin.Word
  // store %13 to %49 : $*Int
  auto *uci = allocRef->getSingleUserOfType<UpcastInst>();
  if (!uci) return false;
  auto *rti = uci->getSingleUserOfType<RefTailAddrInst>();
  if (!rti) return false;

  elements.resize(numElements);

  for (auto *use : rti->getUses()) {
    auto *user = use->getUser();

    uint64_t index = 0;
    if (auto *iai = dyn_cast<IndexAddrInst>(user)) {
      auto *ili = dyn_cast<IntegerLiteralInst>(iai->getOperand(1));
      if (!ili)
        return false;
      index = ili->getValue().getLimitedValue();
      if (auto *iaiUse = iai->getSingleUse())
        user = iaiUse->getUser();
      else
        return false;
    }

    // Check to see if we have a store to a valid index that hasn't been stored
    // to yet.
    auto *si = dyn_cast<StoreInst>(user);
    if (!si || index >= elements.size() || elements[index] != nullptr)
      return false;

    // If we got a store to a valid index, check to see if the stored value is
    // itself a valid constant.
    auto *elt = getAttrOperand(si->getOperand(0));
    if (!elt)
      return false;
    elements[index] = elt;

    // Track how many elements we see so we can know if we got them all.
    --numElements;
  }

  // Make sure that all of the elements were found.
  if (numElements != 0)
    return false;

  return true;
}

/// Analyze the specified SIL instruction and return a SILTensorOpInfo result if
/// the instruction is a valid tensor operation.  This is the way that
/// SILTensorOpInfo's are created.
Optional<SILTensorOpInfo>
SILTensorOpInfo::decode(SILInstruction *inst) {
  // Tuple extracts of tensor ops are considered to be themselves Tensor
  // operations, since they are part of the core representation of nodes that
  // produce multiple results.
  if (auto *ti = dyn_cast<TupleExtractInst>(inst))
    if (auto *ai = dyn_cast<BuiltinInst>(ti->getOperand()))
      return decode(ai);

  SILTensorOpInfo toiInfo(*inst);

  // Tensor operations are builtin instructions and apply instructions.
  if (auto *builtinInst = dyn_cast<BuiltinInst>(inst))
    if (toiInfo.decodeBuiltin(builtinInst))
      return toiInfo;

  // Operations which can conditionally run on the host or the accelerator are
  // modeled as well-known function calls.  If they satisfy the requirements
  // (e.g. that their parameters are constants we can analyze) then they get
  // promoted to notional "ops".
  if (auto *applyInst = dyn_cast<ApplyInst>(inst))
    if (auto *fnRef = dyn_cast<FunctionRefInst>(applyInst->getCallee())) {
      auto callee = fnRef->getReferencedFunction()->getName();
      if (callee == "__tf_tensor_from_units" &&
          toiInfo.decodeTensorFromUnits(applyInst))
        return toiInfo;
      if (callee == "__tf_tensor_from_units_1d" &&
          toiInfo.decodeTensorFromUnits1D(applyInst))
        return toiInfo;
    }

  return None;
}

/// The vast majority of interesting tensor operations are builtin instructions,
/// which come from the user-exposed #tfop() syntax.
bool SILTensorOpInfo::decodeBuiltin(BuiltinInst *inst) {
  builtinName = inst->getName().str();

  // If the name is valid, it isn't an op.
  StringRef typeDescriptorStr;
  if (!decodeTensorOpName(builtinName, opName, typeDescriptorStr,
                          attributes))
    return false;

  // This helper emits a diagnostic if the #tfop descriptor is malformed in a
  // way that prevents it from ever working.  Errors that are a result of a
  // client's misuse of the op is checked by checkAttributeConstants, because
  // the location information is far more important to get right there.
  auto diagInvalid = [&](std::string problem) {
    diagnose(inst->getModule().getASTContext(), inst->getLoc().getSourceLoc(),
             diag::tfop_incorrect_operandinfo,
             operandDescriptorStr, problem);
  };

  // The type descriptor has operand and result info separated by a colon.
  auto colonLoc = typeDescriptorStr.find(':');
  if (colonLoc == StringRef::npos) {
    diagInvalid("no colon in type descriptor");
    return false;
  }

  auto errInfo = decodeDescriptorString(typeDescriptorStr);
  if (errInfo.isError()) {
    diagInvalid(errInfo.message);
    return false;
  }

  // Validate that this instruction is ok.
  unsigned nextOperand = 0;
  auto getNextOperand = [&]() -> SILValue {
    // If we ran out of operands, something is wrong.
    if (nextOperand >= inst->getNumOperands()) {
      diagInvalid("expected more operands than the " +
                  llvm::utostr(inst->getNumOperands()-1) + " present");
      return SILValue();
    }
    return inst->getOperand(nextOperand++);
  };

  for (auto opInfo : operandDescriptors) {
    switch (opInfo) {
    case OpDescriptor::Tensor: {
      auto op = getNextOperand();
      if (!op) return false;  // diagnostic already emitted.
      if (!isTensorHandle(op->getType().getSwiftRValueType())) {
        diagInvalid("expected " +
                    llvm::utostr(nextOperand-2) + " to be a tensor");
        return false;
      }
      break;
    }
    case OpDescriptor::Scalar: {
      // This requires a scalar value.
      auto op = getNextOperand();
      if (!op) return false; // diagnostic already emitted.

      if (isTensorHandle(op->getType().getSwiftRValueType())) {
        diagInvalid("'s' operand requires a scalar value");
        return false;
      }

      assert(getScalarOperand(op) && "Invalid scalar operand");
      break;
    }
    }
  }

  // Attribute arguments come next.  We don't have to check their operands (they
  // get checked in a separate pass so we can diagnose errors better), but do
  // check to make sure that the attribute name doesn't have unsupported
  // suffixes.
  for (auto &attr : attributes) {
    auto op = getNextOperand();
    if (!op) return false; // diagnostic already emitted.

    // Figure out what the suffix is (if any) and reject invalid suffixes if
    // present.
    auto dollarLoc = attr.first.find('$');
    if (dollarLoc == StringRef::npos) continue;

    auto suffix = attr.first.drop_front(dollarLoc+1);
    AttributeModifier suffixKind;
    if (suffix == "tensor")
      suffixKind = AttributeModifier::Tensor;
    else if (suffix == "shape")
      suffixKind = AttributeModifier::Shape;
    else if (suffix == "dtype")
      suffixKind = AttributeModifier::DType;
    else if (suffix == "array")
      suffixKind = AttributeModifier::Array;
    else if (suffix == "elt")
      suffixKind = AttributeModifier::ArrayElement;
    else {
      diagInvalid("invalid attribute modifier '" + suffix.str() + "'");
      return false;
    }

    // Slice the suffix off of the attribute name and add the decoded version.
    attr = { attr.first.substr(0, dollarLoc), suffixKind };
  }

  // Diagnose when the type descriptor didn't specify the right number of args.
  if (nextOperand != inst->getNumOperands()) {
    diagInvalid("more arguments present than type descriptors specified");
    return false;
  }

  return true;
}

/// If all the operands to a call to __tf_tensor_from_units are constants, we
/// can promote this to a 'Const' node with an attached TF_Tensor attribute.
///
/// It takes a 1D array of units, a shape as a 1D array of integers, and a
/// metatype that corresponds to the Unit type.  This has been carefully set up
/// to align with what the Const op wants to see.
///
bool SILTensorOpInfo::decodeTensorFromUnits(ApplyInst *inst) {
  assert(inst->getNumOperands() == 3 &&
         isTensorHandle(inst->getType().getSwiftRValueType()) &&
         "Unexpected type signature for __tf_tensor_from_units");

  // If we can't analyze the operands as arrays of constants, give up.
  auto unitsArray = dyn_cast_or_null<StructInst>(getAttrOperand(1));
  auto shapeArray = dyn_cast_or_null<StructInst>(getAttrOperand(2));
  if (!unitsArray || !shapeArray)
    return false;

  // Otherwise, good news everyone!  We can treat this as a Const op.  The first
  // operand is the $tensor value, the second is the $shape for it, and the
  // third is the dtype for the result (which gets added later).
  opName = "Const";
  operandDescriptorStr = "";
  resultDescriptorStr = "t";
  builtinName = "__tfop_Const,:t";
  attributes.push_back({"value", AttributeModifier::Tensor });
  attributes.push_back({"value", AttributeModifier::Shape });
  return true;
}

/// If all the operands to a call to __tf_tensor_from_units_1d are constants,
/// we can promote this to a 'Const' node with an attached TF_Tensor attribute.
/// This is a specialized form of __tf_tensor_from_units, because the later is
/// defined in terms of a shape of "[units.count]" but the performance optimizer
/// is not reliably constant propagating this.  When we have a reliable
/// deabstraction pass we can re-evaluate this and hopefully eliminate it in
/// favor of library code in the TensorFlow module.
///
bool SILTensorOpInfo::decodeTensorFromUnits1D(ApplyInst *inst) {
  assert(inst->getNumOperands() == 2 &&
         isTensorHandle(inst->getType().getSwiftRValueType()) &&
         "Unexpected type signature for __tf_tensor_from_units_1d");

  // If we can't analyze the operands as arrays of constants, give up.
  auto unitsArray = dyn_cast_or_null<StructInst>(getAttrOperand(1));
  if (!unitsArray)
    return false;

  // Otherwise, good news everyone!  We can treat this as a Const op.
  opName = "Const";
  operandDescriptorStr = "";
  resultDescriptorStr = "t";
  builtinName = "__tfop_Const,:t";
  attributes.push_back({"value", AttributeModifier::Tensor });
  return true;
}



/// Return the string suffix for the specified attribute modifier.
const char *SILTensorOpInfo::
getAttributeModifierSuffix(AttributeModifier modifier) {
  switch (modifier) {
  case AttributeModifier::Normal: return "";
  case AttributeModifier::DType: return "$dtype";
  case AttributeModifier::Tensor: return "$tensor";
  case AttributeModifier::Shape: return "$shape";
  case AttributeModifier::Array: return "$array";
  case AttributeModifier::ArrayElement: return "$elt";
  }
}


/// Verify that any attribute operands are passed acceptable constants,
/// returning a non-empty error string to emit if that is not the case.
std::string SILTensorOpInfo::checkAttributeConstants() const {
  // Attribute arguments are always at the end.
  unsigned operandNumber = inst->getNumOperands()-attributes.size();

  // Attribute values require constant values.  If we don't have one then this
  // op is invalid and must be rejected.
  for (unsigned attrId = 0, e = attributes.size(); attrId != e; ) {
    auto attr = attributes[attrId++];
    auto operand = getAttrOperand(operandNumber++);
    if (!operand)
      return "attribute '" + attr.first.str() +"' requires a constant argument";

    // Check additional requirements imposed by attribute modifiers.
    switch (attr.second) {
    case AttributeModifier::Normal:  // No modifier.
      break;
    case AttributeModifier::DType:   // This integer value is a dtype.
      if (!isa<IntegerLiteralInst>(operand))
        return "attribute '" + attr.first.str()+"' requires a constant integer";
      break;
    case AttributeModifier::Tensor:
      // If this an integer or float, it should be turned into a TF_Tensor.
      if (isa<IntegerLiteralInst>(operand) ||
          isa<FloatLiteralInst>(operand))
        break;

      // Otherwise, if it is an array, it should be decodable and should be
      // followed by a shape.
      if (isa<StructInst>(operand)) {
        Type unitsElementType;
        SmallVector<SingleValueInstruction*, 16> units;
        if (!decodeArrayElements(operand, units, unitsElementType)) {
          return "attribute '" + attr.first.str() +
                 "' requires an array of constant values";
        }

        // The next operand must be a shape.
        if (attrId >= attributes.size() ||
            attr.first != attributes[attrId].first ||
            attributes[attrId].second != AttributeModifier::Shape) {
          // If we have a call to a well-known C function that will be promoted
          // to a tensor op, then we don't need a shape, it will be synthesized
          // later.
          if (isa<ApplyInst>(inst))
            break;

          return "tensor array attribute '" + attr.first.str() +
                 "' must be followed by a shape";
        }

        auto shapeOperand = getAttrOperand(operandNumber++);
        ++attrId;
        if (!shapeOperand || !isa<StructInst>(shapeOperand))
          return "attribute '" + attr.first.str() + "' has invalid shape";

        Type shapeElementType;
        SmallVector<SingleValueInstruction*, 4> shape;
        if (!decodeArrayElements(shapeOperand, shape, shapeElementType))
          return "attribute '" + attr.first.str() + "' has non-constant shape";

        // Verify we have the right number of units.
        uint64_t unitCount = 1;
        for (auto elt : shape)
          unitCount *= cast<IntegerLiteralInst>(elt)
                                                ->getValue().getLimitedValue();
        if (unitCount != units.size())
          return "tensor literal should have " + llvm::utostr(unitCount) +
                 " units for this shape, but has " + llvm::utostr(units.size());

        // If everything is ok, then we're good to go.
        break;
      }

      return "attribute '" + attr.first.str() +
        "' requires a constant integer or floating point constant";
    case AttributeModifier::Shape:
      // This array of integers is a shape specifier.
      return "shape modifier not supported yet";
    case AttributeModifier::Array:
    case AttributeModifier::ArrayElement:
      llvm_unreachable("$array and $elt shouldn't be written by users");
    }
  }

  // Otherwise everything is ok.
  return "";
}

/// Replace any indirect memory operands with direct references to the
/// scalars they reference.  This potentially replaces the builtin
/// instruction, so it returns the right one to use.
// TODO(clattner): Remove this when deabstraction exists.
SILInstruction *SILTensorOpInfo::canonicalizeOperands() {
  SmallVector<SILValue, 8> operands;

  std::string name = "__tfop_" + opName.str() + "," +
    operandDescriptorStr.str()+":"+resultDescriptorStr.str();

  // Handle normal operands.
  bool changed = false;
  unsigned nextOperand = 0;

  // If this is a well-known call we're promoting to a graph operations, always
  // canonicalize it.
  if (isa<ApplyInst>(inst)) {
    changed = true;
    nextOperand = 1;  // Skip the callee argument.
  }

  for (auto op: operandDescriptors) {
    switch (op) {
    case OpDescriptor::Tensor:
      operands.push_back(inst->getOperand(nextOperand++));
      break;
    case OpDescriptor::Scalar:
      auto operand = inst->getOperand(nextOperand++);
      auto scalar = getScalarOperand(operand);
      operands.push_back(scalar);
      changed |= operand != scalar;
      break;
    }
  }

  SILBuilder B(inst);

  // Handle attributes.
  for (auto attr : attributes) {
    auto operand = inst->getOperand(nextOperand++);
    auto attrOperand = getAttrOperand(operand)->getResults()[0];

    // If this is a normal operand, just add it.
    auto *si = dyn_cast<StructInst>(attrOperand);
    if (!si) {
      // Otherwise, this is a normal operand.
      operands.push_back(attrOperand);
      changed |= operand != attrOperand;
      name += ","+attr.first.str()+getAttributeModifierSuffix(attr.second);
      continue;
    }

    // Otherwise, this is an array attribute, so expand it out.
    SmallVector<SingleValueInstruction*, 8> elements;
    Type elementType;
    bool isArray = decodeArrayElements(attrOperand, elements, elementType);
    assert(isArray && "Unexpected attribute operand"); (void)isArray;

    // Add the first operand, which is the metatype for the element.  If it was
    // a 'Normal' operand, change it to an Array so we can distinguish it in the
    // case of an empty array.
    if (attr.second == AttributeModifier::Normal)
      attr.second = AttributeModifier::Array;
    name += ","+attr.first.str()+getAttributeModifierSuffix(attr.second);

    auto metatypeType =
      MetatypeType::get(elementType, MetatypeRepresentation::Thin)
         ->getCanonicalType();
    operands.push_back(B.createMetatype(inst->getLoc(),
                                SILType::getPrimitiveObjectType(metatypeType)));

    // Add all of the operands as explicit values.  If the instructions came
    // from an out of line array initializer, make sure to clone them over to
    // our function.
    for (auto elt : elements) {
      if (elt->getFunction() != inst->getFunction()) {
        // Make a copy of the instruction.  We can't even use the normal cloning
        // facilities here, because they don't support cloning across functions.
        if (auto *eltInt = dyn_cast<IntegerLiteralInst>(elt))
          elt = B.createIntegerLiteral(elt->getLoc(), eltInt->getType(),
                                       eltInt->getValue());
        else if (auto *eltFP = dyn_cast<FloatLiteralInst>(elt))
          elt = B.createFloatLiteral(elt->getLoc(), eltFP->getType(),
                                     eltFP->getValue());
        else
          llvm_unreachable("Unknown instruction to initialize array");
        elt->setDebugLocation(B.getSILDebugLocation(inst->getLoc()));
      }

      operands.push_back(elt);
      name += ",";
      name += getAttributeModifierSuffix(AttributeModifier::ArrayElement);
    }

    // Emit a release of the array, since we've dropped the consuming use of it.
    B.emitDestroyValueOperation(inst->getLoc(), attrOperand);
    changed = true;
  }
  assert(nextOperand == inst->getNumOperands() && "Unexpected operands?");

  // Handle special cases that arise from promoting functions into ops.  We
  // already know they are going to get rewritten.
  if (auto *applyInst = dyn_cast<ApplyInst>(inst)) {
    auto *fnRef = cast<FunctionRefInst>(applyInst->getCallee());
    auto callee = fnRef->getReferencedFunction()->getName();
    if (callee == "__tf_tensor_from_units") {
      // This takes a Tensor and a Shape operand, but needs a DType added.  The
      // dtype is the type of the Tensor elements, which we conveniently already
      // have available as the first operand.
      operands.push_back(operands[0]);
      name += ",dtype";
    } else if (callee == "__tf_tensor_from_units_1d") {
      // This takes a Tensor operand, but needs a Shape and a DType added.  At
      // this point, the operands list will have a metatype for the tensor as
      // the first operand then all the elements.
      uint64_t unitCount = operands.size()-1;

      // The shape needs a metatype to be well formed, but nothing actually
      // cares what it is.  Just re-push the metatype for the tensor elements,
      // even though it might be floating point or something else weird.
      operands.push_back(operands[0]);
      name += ",shape";
      name += getAttributeModifierSuffix(AttributeModifier::Shape);

      // The shape of a 1d tensor is just the count of elements.
      auto &ctx = inst->getFunction()->getASTContext();
      auto unitCountVal =
        B.createIntegerLiteral(inst->getLoc(),
                               SILType::getBuiltinIntegerType(64, ctx),
                               unitCount);
      operands.push_back(unitCountVal);
      name += ",";
      name += getAttributeModifierSuffix(AttributeModifier::ArrayElement);

      // This needs the dtype of the Tensor.
      operands.push_back(operands[0]);
      name += ",dtype";
    }
  }




  // If everything is already copasetic, just return our existing instruction.
  if (!changed)
    return inst;

  // Otherwise, rebuild a new builtin instruction with the simplified operands.
  auto newInst =
    B.createBuiltin(inst->getLoc(),
                    B.getASTContext().getIdentifier(name),
                    inst->getResults()[0]->getType(), /*no substitions*/{},
                    operands);
  newInst->setDebugLocation(inst->getDebugLocation());
  inst->replaceAllUsesPairwiseWith(newInst);
  inst->eraseFromParent();

  // Now that we have a new instruction, reparse it to make sure that our
  // internal state is all up to date, and that we built it correctly.
  auto newResult = decode(newInst);
  assert(newResult.hasValue() && "Misformed builting when canonicalizing");
  *this = newResult.getValue();
  return newInst;
}



/// The SIL location for operations we process are usually deep in the bowels
/// of the tensor library code, which are all implementation details to the
/// user.  As such, walk the inlining location of the specified node to return
/// the first location *outside* of the tensor implementation goop.
SILDebugLocation tf::skipInternalLocations(SILDebugLocation loc) {
  auto ds = loc.getScope();

  if (!ds) return loc;

  // If this location hasn't been inlined at all, just keep it unmodified.
  if (!ds->InlinedCallSite && loc.getLocation().getSourceLoc().isValid())
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

  if (!ds->Loc.isNull())
    return SILDebugLocation(ds->Loc, ds);

  return loc;
}

SILLocation tf::getUserSourceLocation(SILValue value) {
  if (auto *inst = dyn_cast<SILInstruction>((SILNode*)value))
    return getUserSourceLocation(inst);
  return getUserSourceLocation(value.getDebugLocation());
}

/// Get the user's source location for the specified instruction.  Because it
/// is an instruction, we can apply various heuristics to improve the
/// precision of the returned location information.
SILLocation tf::getUserSourceLocation(SILInstruction *inst) {
  // If we have a struct extract from a type like Int, Float, or Tensor of an
  // internal type like Builtin.i64 or TensorHandle, look through it to the
  // higher level type, which will have better source location information.
  //
  // The struct-extract came from the implementation of some operator in the
  // standard library like "+", and we want the source of the parameter.
  if (auto *sei = dyn_cast<StructExtractInst>(inst)) {
    auto outerType = sei->getType().getSwiftRValueType();
    if (outerType->is<BuiltinType>() ||
        isTensorHandle(outerType)) {
      return getUserSourceLocation(sei->getOperand());
    }
  }

  return getUserSourceLocation(inst->getDebugLocation());
}

