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

#define DEBUG_TYPE "TFUtilities"
#include "TFUtilities.h"
#include "TFConstExpr.h"
#include "TFDeviceSupport.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#ifdef SWIFT_ENABLE_TENSORFLOW
#include "tensorflow/c/c_api.h"
#endif

using namespace swift;
using namespace tf;
typedef SILTensorOpInfo::OperandClass OperandClass;

template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

static llvm::cl::opt<bool>
TFDumpIntermediates("tf-dump-intermediates", llvm::cl::init(false),
              llvm::cl::desc("Dump intermediate results in TensorFlow passes"));

// For Mac builds, default to saving logging files to /tmp since debugging in
// Xcode and the REPL is so challenging.
#if __APPLE__
#define DEFAULT_TO_TMP_LOGGING true
#else
#define DEFAULT_TO_TMP_LOGGING false
#endif

static llvm::cl::opt<bool>
TFDumpIntermediatesToTmp("tf-dump-intermediates-tmp",
                         llvm::cl::init(DEFAULT_TO_TMP_LOGGING),
                         llvm::cl::desc("Dump intermediate results in "
                                        "TensorFlow passes to files in /tmp"));

static raw_ostream &getTmpLoggingStream() {
  // If we are supposed to dump the intermediates into /tmp, set that up now.
  SmallString<64> resultPath;
  int resultFD = -1;
  auto error =
    llvm::sys::fs::createTemporaryFile("tf-dump-intermediates", "txt",
                                       resultFD, resultPath);

  // If we had an error, print something to stderr, but keep going.
  if (error) {
    llvm::errs() << "error opening -tf-dump-intermediates logging file '"
                 << resultPath.str() << "'!\n";
    return llvm::errs();
  }

  // This file never gets closed since the raw_ostream is immortal.
  return *new llvm::raw_fd_ostream(resultFD, /*shouldClose*/true,
                                   /*unbuffered*/true);
}

/// If the -tf-dump-intermediates flag has been passed, return a pointer to
/// the stream that we should print debug dump information to.  Otherwise,
/// return null.  This is used for integration unit tests and debugging.
llvm::raw_ostream *tf::getTFDumpIntermediateStream() {
  // If the -tf-dump-intermediates flag was passed, dump to stdout.
  if (TFDumpIntermediates)
    return &llvm::outs();

  if (!TFDumpIntermediatesToTmp)
    return nullptr;

  // Lazily initialize the logging stream.
  static llvm::raw_ostream &fileStream = getTmpLoggingStream();
  return &fileStream;
}

bool tf::isTensorHandle(SILType ty) {
  return (bool)isTensorHandle(ty.getASTType());
}

/// Determine whether the specified type is one of our well-known types, and
/// if so, which one it is.
TFValueKind tf::classifyTensorFlowValue(SILType ty) {
  return classifyTensorFlowValue(ty.getASTType());
}

/// Return true if the specified type is TensorHandle<T>, ResourceHandle, or
/// VariantHandle.
bool tf::isTensorFlowValue(SILType ty) {
  return (bool)isTensorFlowValue(ty.getASTType());
}

/// If the specified type conforms to the TensorProtocol protocol, return the
/// Scalar type for it.  Otherwise return a null type.
static Type conformsToTensorProtocol(Type ty, ModuleDecl *module) {
  auto nominal = ty->getAnyNominal();

  auto &ctx = ty->getASTContext();
  auto tensorProto = ctx.getProtocol(KnownProtocolKind::TensorProtocol);
  if (!tensorProto || !nominal) return Type();

  SmallVector<ProtocolConformance*, 2> conformances;
  nominal->lookupConformance(/*unused module*/nullptr, tensorProto,
                             conformances);
  if (conformances.size() != 1)
    return Type();

  auto scalarMembers =
    nominal->lookupDirect(DeclName(ctx.getIdentifier("Scalar")));
  if (scalarMembers.size() != 1)
    return Type();
  if (auto member = dyn_cast<TypeDecl>(scalarMembers[0]))
    return ty->getTypeOfMember(module, member,
                               member->getDeclaredInterfaceType());
  return Type();
}

/// If the specified type is a Swift.Array or some element type, then return the
/// element type.  Otherwise, return a null Type.
static Type getArrayElementType(Type ty) {
  if (auto bgst = ty->getAs<BoundGenericStructType>())
    if (bgst->getDecl() == bgst->getASTContext().getArrayDecl())
      return bgst->getGenericArgs()[0];
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
      .Case("BFloat16", TF_BFLOAT16)
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

/// `ty` must be a valid TensorFlow element type "T", like Builtin.Int32. Turn
/// it into a TensorHandle<T> type.
SILType tf::convertElementTypeToTensorValueType(Type ty, const ASTContext& ctx) {
  assert(isValidTensorFlowElementType(ty));
  auto decl = ctx.getTensorHandleDecl();
  auto tensorType = BoundGenericClassType::get(decl, /*parent*/ Type(), ty);

  return SILType::getPrimitiveObjectType(tensorType->getCanonicalType());
}

/// If the specified type is a TensorFlow value type, return it.  Otherwise, it
/// must be a primitive type T.  In that case, wrap it to form TensorHandle<T>.
SILType tf::convertElementTypeToTensorValueType(SILType ty) {
  // If this is already TensorHandle<T>, return it.
  if (isTensorFlowValue(ty))
    return ty;

  return convertElementTypeToTensorValueType(ty.getASTType(), ty.getASTContext());
}

/// Looks up a function in the current module. If it exists, returns it.
/// Otherwise, attempt to link it from imported modules. Returns null if such
/// function name does not exist.
SILFunction *tf::lookupOrLinkFunction(StringRef name, SILModule &module) {
  auto *fn = module.lookUpFunction(name);
  if (!fn)
    fn = module.findFunction(name, SILLinkage::PublicExternal);
  assert(fn);
  if (fn->isExternalDeclaration()) {
    bool loaded = module.loadFunction(fn);
    assert(loaded);
    (void)loaded;
    // linkFunction() can return false if this function has already been
    // deserialized and its body available in `module`.
    module.linkFunction(fn);
  }
  assert(!fn->isExternalDeclaration());
  assert(fn->isDefinition());
  return fn;
}

/// Looks up members by `name` in the context of `typeDecl`, `proto` and
/// `module`, and populates `results`.
static void lookupProtocolRequiredMembers(
    NominalTypeDecl *typeDecl, ProtocolDecl *proto, DeclName name,
    ModuleDecl *module, SmallVectorImpl<ValueDecl *> &results) {
  // Make sure the given type conforms to the given protocol.
  SmallVector<ProtocolConformance *, 2> conformances;
  auto type = typeDecl->getDeclaredInterfaceType();
  typeDecl->lookupConformance(module, proto, conformances);
  assert(!conformances.empty() && "Type doesn't conform to the protocol?");
  // Look up nominal type candidates and protocol requirement candidates.
  SmallVector<ValueDecl *, 2> lookupResults;
  typeDecl->lookupQualified(
    type, name, NLOptions::NL_ProtocolMembers, nullptr, lookupResults);
  // Append matches to results.
  for (ValueDecl *decl : lookupResults)
    results.push_back(decl);
}

SILFunction *tf::findSILFunctionForRequiredProtocolMember(
    NominalTypeDecl *typeDecl, ProtocolDecl *proto, DeclName name,
    ModuleDecl *module, SILModule &silModule) {
  SmallVector<ValueDecl *, 4> results;
  lookupProtocolRequiredMembers(typeDecl, proto, name, module, results);
  for (auto *result : results) {
    std::string name = SILDeclRef(result).mangle();
    if (auto *fn = lookupOrLinkFunction(name, silModule))
      return fn;
  }
  return nullptr;
}

SubstitutionMap tf::getSingleSubstitutionMapForElementTypeAndSignature(
    Type ty, GenericSignature *genericSig) {
  assert(genericSig->getGenericParams().size() == 1);
  return SubstitutionMap::get(genericSig,
                              [&](SubstitutableType *t) { return ty; },
                              LookUpConformanceInSignature(*genericSig));
}

SubstitutionMap tf::getSingleSubstitutionMapForElementType(Type ty,
                                                           ASTContext &ctx) {
  // FIXME: consider storing `genericSig` somewhere to avoid rebuilding it
  // each time.
  auto builder = GenericSignatureBuilder(ctx);
  builder.addGenericParameter(GenericTypeParamType::get(0, 0, ctx));
  auto *genericSig = std::move(builder).computeGenericSignature(SourceLoc());
  return getSingleSubstitutionMapForElementTypeAndSignature(ty, genericSig);
}

/// If the specified value is a single-element struct_inst wrapper, look through
/// them.  We special case arrays, and return Array<T> values as themselves.
static SILValue getValueInsideStructInst(SILValue value) {
  // Dig through one-argument struct insts.
  while (auto structVal = dyn_cast<StructInst>(value)) {
    // If this is an ArrayType, don't dig in.
    if (getArrayElementType(structVal->getType().getASTType()))
      break;

    if (structVal->getNumOperands() != 1)
      break;
    value = structVal->getOperand(0);
  }
  return value;
}

/// Return true if this is a reference to the _allocateUninitialized helper
/// in array in the standard library allocating zero elements.
/// TODO: Move to deabstraction when other clients are removed.
bool isArrayAllocUninit(SILValue op, SILValue &numElements) {
  auto *apply = dyn_cast<ApplyInst>(op->getDefiningInstruction());
  if (!apply) return false;
  auto *callee = dyn_cast<FunctionRefInst>(apply->getOperand(0));
  if (!callee) return false;

  auto calleeName = callee->getReferencedFunction()->getName();
  // FIXME: Gross hack because this is specialized by perf optimizer.  Remove
  // when deabstraction does arrays.
  if (!calleeName.contains("_allocateUninitializedArray"))
    return false;

  numElements = getValueInsideStructInst(apply->getOperand(1));
  return true;
}

/// Return true if this is a reference to the _allocateUninitialized helper
/// in array in the standard library allocating zero elements.
static bool isZeroElementArrayAlloc(TupleExtractInst *tei) {
  if (tei->getFieldNo() != 0) return false;
  SILValue numElements;

  if (!isArrayAllocUninit(tei->getOperand(), numElements))
    return false;

  auto *ili = dyn_cast<IntegerLiteralInst>(numElements);
  return ili && ili->getValue() == 0;
}

/// Given a SILValue that may be an array, attempt to decode it into the
/// literal constant values that make up its elements.  If this fails or if
/// the value is not an array, this returns false.  Otherwise it decodes the
/// array and returns the element initializer in elements.
///
/// If arrayInsts is non-null and if decoding succeeds, this function adds all
/// of the instructions relevant to the definition of this array into the set.
/// If decoding fails, then the contents of this set are undefined.
static bool decodeArrayElements(SILValue value,
                                SmallVectorImpl<SILValue> &elements,
                                Type &elementType,
                        SmallPtrSet<SILInstruction*, 8> *arrayInsts = nullptr) {
  elementType = getArrayElementType(value->getType().getASTType());
  if (!elementType) return false;

  // Handle zero element pattern.
  if (auto *tei = dyn_cast<TupleExtractInst>(value)) {
    SILValue numElements;
    if (tei->getFieldNo() == 0 &&
        isArrayAllocUninit(tei->getOperand(), numElements)) {
      auto *ili = dyn_cast<IntegerLiteralInst>(numElements);
      if (ili && ili->getValue() == 0) {
        if (arrayInsts) {
          arrayInsts->insert(tei);
          arrayInsts->insert(tei->getOperand()->getDefiningInstruction());
        }
        return true;
      }
    }
  }

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
    auto *inst = dyn_cast<SILInstruction>((SILNode*)value);
    if (!inst) return false;

    // Remember this instruction if requested.
    if (arrayInsts) arrayInsts->insert(inst);

    if (auto *si = dyn_cast<StructInst>(inst)) {
      if (si->getNumOperands() != 1) return false;
      value = si->getOperand(0);
    } else if (auto *urci = dyn_cast<UncheckedRefCastInst>(inst)) {
      value = urci->getOperand();
    } else if (auto *uci = dyn_cast<UpcastInst>(inst)) {
      value = uci->getOperand();
    } else if (auto *globalValue = dyn_cast<GlobalValueInst>(inst)) {
      // If we found a GlobalValueInst, then we're referring to an array that
      // got moved to being a static initializer.
      auto *init = dyn_cast_or_null<ObjectInst>(
              globalValue->getReferencedGlobal()->getStaticInitializerValue());
      if (!init) return false;

      // Do not add "init" to arrayInsts, since it is not in the host function.

      // The initializer elements are the tail elements of the object_inst, see
      // if they are all decodable.
      for (auto elt : init->getTailElements()) {
        elements.push_back(getValueInsideStructInst(elt));
      }

      return true;
    } else if (auto *rptr = dyn_cast<RawPointerToRefInst>(inst)) {
      // The empty array is specially recognized by the optimizer and
      // transformed into a well-known global produced by the standard library.
      // Uses of it look like this:
      //   %5 = global_addr @_swiftEmptyArrayStorage : $*_SwiftEmptyArrayStorage
      //   %6 = address_to_pointer %5 : $*_SwiftEmptyArrayStorage to $RawPointer
      //   %7 = raw_pointer_to_ref %6 : $RawPointer to $_EmptyArrayStorage
      //   %8 = unchecked_ref_cast %7 : $_EmptyArrayStorage to $BridgeObject
      auto a2p = dyn_cast<AddressToPointerInst>(rptr->getOperand());
      if (!a2p) return false;
      if (arrayInsts) arrayInsts->insert(a2p);


      auto *ga = dyn_cast<GlobalAddrInst>(a2p->getOperand());
      if (arrayInsts && ga) arrayInsts->insert(ga);

      elements.clear();
      return ga &&
             ga->getReferencedGlobal()->getName() == "_swiftEmptyArrayStorage";
    } else {
      return false;
    }
  }
  if (arrayInsts) arrayInsts->insert(allocRef);

  // The allocation must be of a constant number of elements.
  if (allocRef->getNumOperands() != 1) return false;

  auto numElementsInst = dyn_cast<IntegerLiteralInst>(allocRef->getOperand(0));
  if (!numElementsInst)
    return false;

  uint64_t numElements = numElementsInst->getValue().getLimitedValue();

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
  if (arrayInsts) arrayInsts->insert(uci);

  auto *rti = uci->getSingleUserOfType<RefTailAddrInst>();
  if (!rti) return false;
  if (arrayInsts) arrayInsts->insert(rti);

  elements.resize(numElements);

  for (auto *use : rti->getUses()) {
    auto *user = use->getUser();

    uint64_t index = 0;
    if (auto *iai = dyn_cast<IndexAddrInst>(user)) {
      if (arrayInsts) arrayInsts->insert(iai);

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
    auto *store = dyn_cast<StoreInst>(user);
    if (!store || index >= elements.size() || elements[index] != SILValue())
      return false;

    if (arrayInsts) arrayInsts->insert(store);

    // If we got a store to a valid index, it must be our element.
    elements[index] = store->getOperand(0);

    // Track how many elements we see so we can know if we got them all.
    --numElements;
  }

  // Make sure that all of the elements were found.
  if (numElements != 0)
    return false;

  return true;
}


/// This method is called when an owning reference to an array value is just
/// removed.  It checks to see if the array value is now unused: if so, it
/// removes the array and returns true.  Otherwise it returns false without
/// modifying the code.
///
/// This is important because otherwise the array will look like uses of its
/// element values.  If these are tensors, it will cause copies to be inserted
/// from the accelerator program back to the host.
///
static bool tryToRemoveArrayValue(SILValue value) {
  SmallVector<SILValue, 4> elements;
  Type elementType;
  SmallPtrSet<SILInstruction*, 8> arrayInsts;
  // Decode the array, collecting all of the instructions we see that are
  // relevant to its definition.
  if (!decodeArrayElements(value, elements, elementType, &arrayInsts))
    return false;

  // Okay, we've collected all of the core instructions that make up the
  // array implementation guts.  We can remove the array if we can account for
  // all uses of these instructions.  If we find any use that we don't know,
  // then we conservatively leave the array.

  // Iterate over a copy of the set using indexes, to avoid invalidating
  // iterators.
  SmallVector<SILInstruction*, 8> instsToCheck(arrayInsts.begin(),
                                               arrayInsts.end());
  for (unsigned i = 0; i != instsToCheck.size(); ++i) {
    auto *inst = instsToCheck[i];

    for (auto result : inst->getResults()) {
      for (auto use : result->getUses()) {
        auto *user = use->getUser();
        // Its ok if the user is something we're planning to remove.
        if (arrayInsts.count(user))
          continue;

        // If this is debug info or refcounting logic, we can add it to the set
        // of instructions to remove.
        if (isa<DebugValueInst>(user) || isa<RefCountingInst>(user)) {
          instsToCheck.push_back(user);
          arrayInsts.insert(user);
          continue;
        }

        // The implementation of array does some stores into the array, which
        // aren't picked up by the array decoding.  We can remove these.
        if (auto *ref = dyn_cast<RefElementAddrInst>(user)) {
          if (auto use = ref->getSingleUse())
            if (auto *store = dyn_cast<StoreInst>(use->getUser()))
              // Check that this is a store INTO the array, not OF the array.
              if (store->getDest() == ref) {
                arrayInsts.insert(store);
                instsToCheck.push_back(store);
                arrayInsts.insert(ref);
                instsToCheck.push_back(ref);
                continue;
              }
        }

        // Otherwise we don't know what this is, conservatively bail out.
        DEBUG(llvm::dbgs() << "Could not remove array because of: " << *user);
        return false;
      }
    }
  }

  // If we can remove all of the instructions then do so!  Start by dropping all
  // inter-dependent references.
  for (auto inst : instsToCheck)
    inst->dropAllReferences();

  // Then delete them.
  for (auto inst : instsToCheck) {
    inst->eraseFromParent();
  }

  // Okay, we successfully nuked the array guts.  Check to see if any of the
  // element value can also be removed.  We'll often get a StructInst or other
  // glue instructions hanging around, and we don't want to consider them uses
  // of the elements on the host.
  SmallPtrSet<SILValue, 8> visitedInsts;
  while (!elements.empty()) {
    auto val = elements.pop_back_val();
    // If we've already visited this instruction we may have removed it, don't
    // reprocess it.
    if (!visitedInsts.insert(val).second)
      continue;

    // Don't delete out of global initializers.
    auto inst = dyn_cast<SILInstruction>((SILNode*)val);
    if (!inst || !inst->getFunction()) continue;

    // FIXME: This should use recursivelyDeleteTriviallyDeadInstructions.
    if (!isa<StructInst>(inst) && !isa<LiteralInst>(inst) &&
        !isa<MetatypeInst>(inst))
      continue;

    // If this has any uses that are non-debug value, then we can't transform
    // it.
    bool hasUnknownUses = false;
    for (auto result : inst->getResults())
      for (auto *use : result->getUses())
        if (!isa<DebugValueInst>(use->getUser()))
          hasUnknownUses = true;
    if (hasUnknownUses) continue;

    // Remove any debug_values.
    for (auto result : inst->getResults())
      while (!result->use_empty())
        result->use_begin()->getUser()->eraseFromParent();

    for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
      visitedInsts.erase(inst->getOperand(i));
      elements.push_back(inst->getOperand(i));
    }
    inst->eraseFromParent();
  }
  return true;
}

/// Given an array value on which we recently dropped a consuming use, try to
/// remove all the computation that produces the array if possible.  If not,
/// emit a destroy_value instruction to avoid leaking it.
///
/// FIXME: Move this logic to deabstraction when it is done.
///
void SILTensorOpInfo::removeOrDestroyArrayValue(SILValue array, SILLocation loc,
                                                SILBuilder &B) {
  if (!tryToRemoveArrayValue(array))
    B.emitDestroyValueOperation(loc, array);
}

/// If the specified value is a valid value for an attribute, return the
/// instruction that provides the value, otherwise null.
SingleValueInstruction *SILTensorOpInfo::getAttrOperand(SILValue v) {
  // Handle a function-typed attr.
  if (auto *fri = dyn_cast<FunctionRefInst>(v))
    return fri;

  // If the value is a string value, then we need to peel off all the SIL
  // instructions between the String struct value and the underlying
  // string_literal instruction.
  auto &ctx = v->getType().getASTType()->getASTContext();
  if (v->getType().getASTType()->isEqual(
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

      if (auto *vtboi = dyn_cast<ValueToBridgeObjectInst>(str)) {
        str = vtboi->getOperand();
        continue;
      }

      // Look through the various operands that bit-mangle things into bridged
      // string representations.  This is gross, Swift should have higher level
      // operations for bridge values like this.
      if (auto *bi = dyn_cast<BuiltinInst>(str)) {
        switch (bi->getBuiltinInfo().ID) {
        case BuiltinValueKind::StringObjectOr:
        case BuiltinValueKind::And:
        case BuiltinValueKind::Or:
        case BuiltinValueKind::ZExtOrBitCast:
        case BuiltinValueKind::PtrToInt:
          str = bi->getOperand(0);
          continue;
        default: break;
        }
      }

      // In this case, the expected SIL code to match looks like:
      //  %0 = string_literal utf8 "foo"
      //  // function_ref specialized String.init(
      //       _builtinStringLiteral:utf8CodeUnitCount:isASCII:)
      //  function_ref @$SSS21_builtinStringLiteral... : $@convention(thin) (
      //       Builtin.RawPointer...) -> @owned String
      //  %4 = apply %3(%0, ...
      // So we want to follow the first func arg of the ApplyInst (%0 above).
      if (auto *ai = dyn_cast<ApplyInst>(str)) {
        // If the ApplyInst does not have such an operand, we bail with failure.
        if (ai->getNumOperands() < 2) return nullptr;
        str = ai->getOperand(1);
        continue;
      }

      // It is possible that we have a variable string, we want to reject it
      // as a non-constant value.
      return nullptr;
    }
  }

  // Dig through struct wrappers.
  v = getValueInsideStructInst(v);

  // Handle cases that create a literal array.
  if (auto *si = dyn_cast<StructInst>(v)) {
    SmallVector<SILValue, 8> elements;
    Type elementType;
    if (decodeArrayElements(v, elements, elementType)) {
      for (auto elt : elements)
        if (!getAttrOperand(elt))
          return nullptr;
      return si;
    }
  }

  // Handle a call to allocate zero elements.
  if (auto *tei = dyn_cast<TupleExtractInst>(v))
    if (isZeroElementArrayAlloc(tei))
      return tei;

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


/// Analyze the specified SIL instruction and return a SILTensorOpInfo result if
/// the instruction is a valid tensor operation.  This is the way that
/// SILTensorOpInfo's are created.
Optional<SILTensorOpInfo>
SILTensorOpInfo::decode(SILInstruction *inst) {
  // Tensor operations are builtin instructions and apply instructions.
  if (auto *builtin = dyn_cast<BuiltinInst>(inst)) {
    SILTensorOpInfo toiInfo(builtin);
    if (toiInfo.decodeBuiltin())
      return toiInfo;
  }
  return None;
}

std::string SILTensorOpInfo::getDeviceString() const {
  // FIXME: consider switching to the following coding style consistently
  // for (auto [op, idx] : enumerate(inst->getAllOperands())) {
  for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
    auto operand = inst->getOperand(i);
    auto opInfo = operandClasses[i];
    if (isInput(i)) continue;

    // Handle attributes.
    auto attrOperand = getAttrOperand(operand);

    // If it's a device attribute, get the device value.
    if (opInfo.first != DEVICE_ATTR) continue;
    auto *sli = cast<StringLiteralInst>(attrOperand);
    assert(sli->getEncoding() == StringLiteralInst::Encoding::UTF8 &&
           "only byte encodings are supported");
    return sli->getValue();
  }
  llvm_unreachable("Tensor op instruction has no device string.");
}

typedef std::pair<StringRef, OperandClass> AttributeEntry;

/// Given a builtin name that refer to a tensorflow op function, this returns
/// the op name and operand clases and returns an empty string.  If the string
/// provided is invalid, this returns an error message to present.
static std::string
decodeTensorOpName(StringRef name, StringRef &opName,
                   SmallVectorImpl<AttributeEntry> &operandClasses){
  // Decode the base name for the op.
  auto pos = name.find(",");
  opName = name.substr(0, pos);
  if (pos == StringRef::npos) return "";
  name = name.substr(pos);

  // Parse out operand information.
  while (!name.empty()) {
    assert(name[0] == ',');
    name = name.drop_front(1);

    pos = name.find(",");
    if (pos == StringRef::npos) pos = name.size();

    // Parse out the attribute name.  If it contains a $, then parse out the
    // OperandClass as well.
    auto attrName = name.substr(0, pos);

    // Figure out what the suffix is (if any) and reject invalid suffixes if
    // present.
    auto dollarLoc = attrName.find('$');

    auto opClass = OperandClass::Normal;
    if (dollarLoc != StringRef::npos) {
      auto suffix = attrName.drop_front(dollarLoc+1);
      if (auto res = SILTensorOpInfo::getOperandClass(suffix))
        opClass = res.getValue();
      else {
        return "invalid attribute modifier '" + attrName.str() + "'";
      }
    }

    // Slice the suffix off the attribute name and add the decoded version.
    operandClasses.push_back({
      attrName.substr(0, dollarLoc), opClass
    });
    name = name.substr(pos);
  }

  return "";
}

/// The vast majority of interesting tensor operations are builtin instructions,
/// which come from the user-exposed #tfop() syntax.
bool SILTensorOpInfo::decodeBuiltin() {
  builtinName = inst->getName().str();

  // If the builtin doesn't start with our magic prefix, then it isn't an op.
  if (!builtinName.startswith("__tfop_"))
    return false;

  // This helper emits a diagnostic if the #tfop descriptor is malformed in a
  // way that prevents it from ever working.  Errors that are a result of a
  // client's misuse of the op is checked by checkAttributeConstants, because
  // the location information is far more important to get right there.
  auto diagInvalid = [&](std::string problem) {
    diagnose(inst->getModule().getASTContext(), inst->getLoc().getSourceLoc(),
             diag::tfop_invalid_tfop, problem);
  };

  // Ok, it is, decode and validate it.
  auto errStr = decodeTensorOpName(builtinName.substr(strlen("__tfop_")),
                                   opName, operandClasses);
  if (!errStr.empty()) {
    diagInvalid(errStr);
    return false;
  }

  // Validate that this instruction is ok.
  if (inst->getNumOperands() != operandClasses.size()) {
    diagInvalid("op has " + llvm::utostr(operandClasses.size()) +
                " operand classes, but " + llvm::utostr(inst->getNumOperands())+
                " inputs and attributes");
    return false;
  }

  return true;
}

/// expandArrayAttribute - Decode the specified array value (which should be an
/// array of constant integer or fp values) and add it as a value$tensor operand
/// to the specified op that is being built up.  This returns false if the
/// operand is not an array of constant values.
///
/// FIXME: Remove this when Deabstraction subsumes op promotion.
///
static bool expandArrayAttribute(SILValue arrayVal, StringRef attrName,
                                 OperandClass attrKind,
                                 std::string &name,
                                 SmallVectorImpl<SILValue> &operands,
                                 SILInstruction *forInst) {
  // Otherwise, this is an array attribute, so expand it out.
  SmallVector<SILValue, 8> elements;
  Type elementType;
  bool isArray = decodeArrayElements(arrayVal, elements, elementType);
  if (!isArray) return false;

  // Verify that we have all constants.
  for (auto &elt : elements) {
    elt = SILTensorOpInfo::getAttrOperand(elt);
    if (!elt) return false;
  }

  SILBuilder B(forInst);

  // Add the first operand, which is the metatype for the element.  If it was
  // a 'Normal' operand, change it to an Array or TensorShape, allowing us to
  // distinguish it in the case of an empty array.
  if (attrKind == OperandClass::Normal) {
    attrKind = OperandClass::Array;
    if (auto nominal = elementType->getNominalOrBoundGenericNominal())
      if (auto sd = nominal->getAsStructOrStructExtensionContext())
        if (sd->getName().str() == "TensorShape")
          attrKind = OperandClass::ShapeArray;
  }
  name += ","+attrName.str();
  name += SILTensorOpInfo::getOperandClassSuffix(attrKind);

  // If this is a ShapeArray, add the element count, otherwise add the element
  // type.
  if (attrKind != OperandClass::ShapeArray) {
    auto metatypeType =
      MetatypeType::get(elementType, MetatypeRepresentation::Thin)
        ->getCanonicalType();
    operands.push_back(B.createMetatype(forInst->getLoc(),
                                SILType::getPrimitiveObjectType(metatypeType)));
  } else {
    auto int64Ty = SILType::getBuiltinIntegerType(64, B.getASTContext());
    operands.push_back(B.createIntegerLiteral(forInst->getLoc(), int64Ty,
                                              elements.size()));
  }

  // Add all of the operands as explicit values.  If the instructions came
  // from an out of line array initializer, make sure to clone them over to
  // our function.
  for (auto eltVal : elements) {
    // If this is a shape array, recursively decompose the elements into shapes.
    if (attrKind == OperandClass::ShapeArray) {
      if (!expandArrayAttribute(eltVal, "", OperandClass::Shape,
                                name, operands, forInst))
        return false;
      continue;
    }

    auto elt = cast<SingleValueInstruction>(eltVal);
    if (elt->getFunction() != forInst->getFunction()) {
      // Make a copy of the instruction.  We can't even use the normal cloning
      // facilities here, because they don't support cloning across functions.
      if (auto *eltInt = dyn_cast<IntegerLiteralInst>(elt))
        elt = B.createIntegerLiteral(eltInt->getLoc(), eltInt->getType(),
                                     eltInt->getValue());
      else if (auto *eltFP = dyn_cast<FloatLiteralInst>(elt))
        elt = B.createFloatLiteral(eltFP->getLoc(), eltFP->getType(),
                                   eltFP->getValue());
      else
        llvm_unreachable("Unknown instruction to initialize array");
      elt->setDebugLocation(B.getSILDebugLocation(forInst->getLoc()));
    }

    operands.push_back(elt);
    name += ",";
    name += SILTensorOpInfo::getOperandClassSuffix(OperandClass::ArrayElement);
  }

  return true;
}

/// Return the string suffix for the specified attribute modifier.
const char *SILTensorOpInfo::
getOperandClassSuffix(OperandClass opClass) {
  switch (opClass) {
  case OperandClass::Input: return "$in";
  case OperandClass::InputElt: return "$inelt";
  case OperandClass::Normal: return "";
  case OperandClass::DType: return "$dtype";
  case OperandClass::Tensor: return "$tensor";
  case OperandClass::Shape: return "$shape";
  case OperandClass::Array: return "$array";
  case OperandClass::ArrayElement: return "$elt";
  case OperandClass::ShapeArray: return "$shapearray";
  }
}

/// Return the operand class of the specified string form like "tensor"
llvm::Optional<OperandClass>
SILTensorOpInfo::getOperandClass(StringRef suffix) {
  return llvm::StringSwitch<llvm::Optional<OperandClass>>(suffix)
                  .Case("in", OperandClass::Input)
                  .Case("inelt", OperandClass::InputElt)
                  .Case("", OperandClass::Normal)
                  .Case("tensor", OperandClass::Tensor)
                  .Case("shape", OperandClass::Shape)
                  .Case("dtype", OperandClass::DType)
                  .Case("array", OperandClass::Array)
                  .Case("elt", OperandClass::ArrayElement)
                  .Case("shapearray", OperandClass::ShapeArray)
                  .Default(None);
}


/// Verify that any attribute operands are passed acceptable constants,
/// returning a non-empty error string to emit if that is not the case.
///
/// TODO: Remove this when deabstraction has taken over.
///
std::string SILTensorOpInfo::checkAndDiagnoseOperands() const {
  // Attribute values require constant values.  If we don't have one then this
  // op is invalid and must be rejected.
  for (unsigned i = 0, e = operandClasses.size(); i != e; ++i) {
    auto operand = inst->getOperand(i);
    auto opClass = operandClasses[i];

    // If this is an attribute, make sure that it simplifies down according to
    // our attribute requirements.
    if (!isInput(i)) {
      operand = getAttrOperand(operand);
      if (!operand) {
        getAttrOperand(inst->getOperand(i));
        return "attribute '" + opClass.first.str() +
               "' requires a constant argument";
      }
    }

    // Check additional requirements imposed by attribute modifiers.
    switch (opClass.second) {
    case OperandClass::Input: {
      auto opTy = operand->getType();

      // If this is tfc.scalarToTensor, then the input must be a valid scalar.
      if (opName == "tfc.scalarToTensor") {
        auto scalarType = opTy.getASTType();
        if (convertSwiftTypeToTF(scalarType) == 0)
          return "scalarToTensor requires scalar value; unrecognized type '"
             + scalarType->getString() + "' is not allowed";
        break;
      }

      // TensorFlow values and metatype inputs are ok.
      if (isTensorFlowValue(opTy) || opTy.is<MetatypeType>())
        break;

      // If this is an Array of TensorHandle or TensorProtocol we're good.
      if (auto elt = getArrayElementType(opTy.getASTType())) {
        if (!isTensorHandle(elt) &&
            !conformsToTensorProtocol(elt, inst->getModule().getSwiftModule()))
          return "array element has unrecognized type '" + elt->getString() +
                 "'";
        // Ok, the array itself looks fine, try to decode the array constant
        // value.
        SmallVector<SILValue, 8> elements;
        Type elementType;
        if (!decodeArrayElements(operand, elements, elementType))
          return "array input is not a constant array of tensors";
        break;
      }

      // Otherwise, this is an error.
      return "operand has unrecognized type '" +
             opTy.getASTType()->getString() + "'";
    }
    case OperandClass::InputElt: // InputList elements must be TensorHandle's.
      if (!isTensorHandle(operand->getType()))
        return "input list elements must be TensorHandle";
      break;

    case OperandClass::Normal:  // No modifier.
      break;
    case OperandClass::DType:   // This integer value is a dtype.
      if (!isa<IntegerLiteralInst>(operand))
        return "attribute '" + opClass.first.str() +
               "' requires a constant integer";
      break;
    case OperandClass::Shape:
    case OperandClass::Array:
      // Decoded shape values are represented by a metatype, and are optionally
      // followed by array element values.
      if (isa<MetatypeInst>(operand))
        break;
      return "attribute '" + opClass.first.str() +
        "' requires a constant integer or floating point constant";

    case OperandClass::ShapeArray:
      // Shape arrays contain an integer count of # elements, and are followed
      // by that many shapes.
      if (isa<IntegerLiteralInst>(operand))
          break;
      return "attribute '" + opClass.first.str() +
        "' requires a constant integer count";

    case OperandClass::ArrayElement:
      // Integer and float elements work.
      if (isa<IntegerLiteralInst>(operand) ||
          isa<FloatLiteralInst>(operand))
        break;
      return "attribute '" + opClass.first.str() +
        "' requires a constant integer or floating point constant";

    case OperandClass::Tensor:
      // If this an integer or float, it should be turned into a TF_Tensor.
      if (isa<IntegerLiteralInst>(operand) ||
          isa<FloatLiteralInst>(operand))
        break;

      // Decoded tensor value as represented by a metatype, and are optionally
      // followed by array element values.
      if (isa<MetatypeInst>(operand))
        break;

      // Otherwise, if it is an array, it should be decodable and should be
      // followed by a shape.
      if (isa<StructInst>(operand)) {
        Type scalarsElementType;
        SmallVector<SILValue, 16> scalars;
        if (!decodeArrayElements(operand, scalars, scalarsElementType)) {
          return "attribute '" + opClass.first.str() +
                 "' requires an array of constant values";
        }

        // Check that all the elements are constants.
        for (auto elt : scalars) {
          if (!getAttrOperand(elt))
            return "attribute '" + opClass.first.str() +
                   "' requires an array of constant values";
        }

        // The next operand must be a shape.
        if (i+1 >= operandClasses.size() ||
            opClass.first != operandClasses[i].first ||
            operandClasses[i+1].second != OperandClass::Shape) {
          // If we have a call to a well-known C function that will be promoted
          // to a tensor op, then we don't need a shape, it will be synthesized
          // later.
          if (isa<ApplyInst>(inst))
            break;

          return "tensor array attribute '" + opClass.first.str() +
                 "' must be followed by a shape";
        }

        auto shapeOperand = getAttrOperand(++i);
        if (!shapeOperand || !isa<StructInst>(shapeOperand))
          return "attribute '" + opClass.first.str() + "' has invalid shape";

        Type shapeElementType;
        SmallVector<SILValue, 4> shape;
        if (!decodeArrayElements(shapeOperand, shape, shapeElementType))
          return "attribute '" + opClass.first.str() +
                 "' has non-constant shape";

        // Verify we have the right number of scalars.
        uint64_t scalarCount = 1;
        for (auto elt : shape) {
          auto *eltCst =
            dyn_cast_or_null<IntegerLiteralInst>(getAttrOperand(elt));
          if (!eltCst)
            return "attribute '" + opClass.first.str() +
                   "' has non-constant shape";

          scalarCount *= eltCst->getValue().getLimitedValue();
        }
        if (scalarCount != scalars.size())
          return "tensor literal should have " + llvm::utostr(scalarCount) +
             " scalars for this shape, but has " + llvm::utostr(scalars.size());

        // If everything is ok, then we're good to go.
        break;
      }

      return "attribute '" + opClass.first.str() +
             "' requires a constant integer or floating point constant";
    }
  }

  // Otherwise everything is ok.
  return "";
}

/// Given something that conforms to the TensorProtocol protocol, extract the
/// 'handle' out of it.
/// TODO: move this to deabstraction and make static there.
SILValue getTensorProtocolHandleMember(SILValue v, SILLocation loc,
                                       SILBuilder &B) {
  // If we already have a TensorHandle, just use it.
  if (isTensorHandle(v->getType()))
    return v;

  auto module = B.getFunction().getModule().getSwiftModule();

  auto vType = v->getType().getASTType();
  if (!vType->getStructOrBoundGenericStruct() ||
      !conformsToTensorProtocol(vType, module))
    return SILValue();

  // If this value is just a struct wrapper around a TensorHandle, use the
  // input of it.  In the case of Tensor2D, we have multiple levels of struct
  // wrapper.
  while (1) {
    auto si = dyn_cast<StructInst>(v);
    if (!si) break;

    if (si->getNumOperands() != 1)
      break;

    auto operand = si->getOperand(0);
    // If we found the TensorHandle itself, then we win - return it.
    if (isTensorHandle(operand->getType()))
      return operand;

    // If we found a wrapper around another TensorProtocol, dig deeper.
    if (!conformsToTensorProtocol(operand->getType().getASTType(),
                                  module))
      break;

    v = operand;
  }

  // TODO(clattner): it would be more correct to generate a call to an accessor
  // to get the handle out, but for now, we know we're always dealing with types
  // that store the field by-value so we can dig it out in with an easier
  // approach.  This handles structs of TensorHandle (like Tensor) and structs
  // of structs of TensorHandle (like Tensor2D).
  while (!isTensorHandle(v->getType())) {
    auto vTy = v->getType().getASTType();
    auto decl = vTy.getNominalOrBoundGenericNominal();
    assert(decl && "Type must be nominal to conform to TensorProtocol");

    auto fieldIt = decl->getStoredProperties().begin();
    assert(fieldIt != decl->getStoredProperties().end() &&
           "Tensor should have one member");
    VarDecl *field = *fieldIt++;
    assert(fieldIt == decl->getStoredProperties().end() &&
           "Expected one stored field in TensorProtocol type");

    // 'vTy' is usually a bound generic type.  Use getTypeOfMember to substitute
    // the type bound into the member type.
    auto fieldTy = vTy->getTypeOfMember(module, field);

    auto silTy = SILType::getPrimitiveObjectType(fieldTy->getCanonicalType());
    v = B.createStructExtract(loc, v, field, silTy);
  }

  return v;
}

/// Replace any indirect memory operands with direct references to the
/// scalars they reference.  This potentially replaces the builtin
/// instruction, so it returns the right one to use.
// TODO(clattner): Remove this when deabstraction has subsumed it.
SILInstruction *
SILTensorOpInfo::canonicalizeOperands(GraphFunctionDeviceInfo &deviceInfo) {
  // TODO: Canonicalize metatypes into constants!

  SmallVector<SILValue, 8> operands;

  std::string name = "__tfop_" + opName.str();
  SILBuilder B(inst);

  SmallVector<SILValue, 4> arrayOperands;
  std::string opDevice;
  for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
    auto operand = inst->getOperand(i);
    auto opInfo = operandClasses[i];
    std::string opName =
      "," + opInfo.first.str() + getOperandClassSuffix(opInfo.second);

    // Handle inputs.
    if (isInput(i)) {
      auto opTy = operand->getType();
      if (isTensorHandle(opTy) || opTy.is<MetatypeType>()) {
        // TensorHandle's and MetaTypes are already fine.
        operands.push_back(operand);
        name += opName;
        continue;
      }

      // Non-array values are scalars.
      auto elt = getArrayElementType(opTy.getASTType());
      if (!elt) {
        operands.push_back(operand);
        name += opName;
        continue;
      }

      // If this is an Array of TensorHandle or TensorProtocol then we need
      // to flatten this into its component.
      SmallVector<SILValue, 8> elements;
      Type elementType;
      bool isArray = decodeArrayElements(operand, elements, elementType);
      assert(isArray && "Invalid case got through checking pass?");


      // If we have an array of TensorProtocol values, we have to extract the
      // tensor handle out of them.
      if (!isTensorHandle(elementType)) {
        // It is common to have arrays with repeated elements.  These will
        // generally be uniqued on entry to this routine.  If so, make sure to
        // reuse them as we project out the .handle members to avoid code bloat.
        llvm::DenseMap<SILValue, SILValue> loweredElts;
        for (auto &elt : elements) {
          auto &eltVal = loweredElts[elt];
          if (!eltVal)
            eltVal = getTensorProtocolHandleMember(elt, inst->getLoc(), B);
          elt = eltVal;
        }
      }

      // Add the metatype marker so we know that there is an array of elements.
      auto metatypeType =
        MetatypeType::get(elementType, MetatypeRepresentation::Thin)
          ->getCanonicalType();
      operands.push_back(B.createMetatype(inst->getLoc(),
                              SILType::getPrimitiveObjectType(metatypeType)));
      name += ",$in";

      // Add one element operand for each element of the array.
      for (auto elt : elements) {
        assert(isTensorHandle(elt->getType()) && "elements should be lowered");
        operands.push_back(elt);
        name += ",$inelt";
      }

      arrayOperands.push_back(operand);
      continue;
    }

    // Handle attributes.
    auto attrOperand = getAttrOperand(operand);

    // If it's a device attribute, get the device value.
    if (opInfo.first == DEVICE_ATTR) {
      auto *sli = cast<StringLiteralInst>(attrOperand);
      assert(sli->getEncoding() == StringLiteralInst::Encoding::UTF8 &&
             "only byte encodings are supported");
      opDevice = sli->getValue();
      continue;
    }

    // If this is a normal operand, just add it.
    auto *si = dyn_cast<StructInst>(attrOperand);
    if (!si) {
      // Otherwise, this is a normal operand.
      operands.push_back(attrOperand);
      name += opName;
      continue;
    }

    // If this is an array, then we need to expand it out into its constituent
    // elements.
    bool isArray = expandArrayAttribute(attrOperand, opInfo.first,
                                        opInfo.second, name, operands, inst);
    assert(isArray && "array should be validated in earlier pass");

    arrayOperands.push_back(attrOperand);
  }

  // Determine whether canonicalization changed anything.
  bool changed = name != builtinName ||
                 operands.size() != inst->getNumOperands();
  for (unsigned i = 0, e = operands.size(); !changed && i != e; ++i)
    changed |= operands[i] != inst->getOperand(i);

  if (!opDevice.empty()) {
    // User code should not specify this pseudo device.
    // FIXME: Issue diagnostics over an invalid device string.
    assert(opDevice != ALL_DEVICES);
  } else {
    changed = true;
  }
  deviceInfo.handleDevicePlacementLegacy(opName, opDevice, B, inst->getLoc(),
                                         operands, name);

  // If everything is already copasetic, just return our existing instruction.
  if (!changed)
    return inst;

  // Otherwise, rebuild a new builtin instruction with the simplified operands.
  auto *newInst =
    B.createBuiltin(inst->getLoc(),
                    B.getASTContext().getIdentifier(name),
                    inst->getType(), /*no substitions*/{}, operands);
  newInst->setDebugLocation(inst->getDebugLocation());

  // Replace the old with the new and delete the old instruction.
  inst->replaceAllUsesPairwiseWith(newInst);
  inst->eraseFromParent();

  B.setInsertionPoint(newInst);
  for (auto array : arrayOperands) {
    // Try to remove the arrays entirely if it is dead, otherwise emit a
    // release of them, since we've dropped a consuming use of it.
    removeOrDestroyArrayValue(array, newInst->getLoc(), B);
  }


  // Now that we have a new instruction, reparse it to make sure that our
  // internal state is all up to date, and that we built it correctly.
  auto newResult = decode(newInst);
  assert(newResult.hasValue() && "Misformed builtin when canonicalizing");
  *this = newResult.getValue();
  return newInst;
}

//===----------------------------------------------------------------------===//
// GraphOperationDecoder implementation
//===----------------------------------------------------------------------===//

/// Return the device attribute associated with `inst`, which is required to
/// exist.
StringRef GraphOperationInfo::getDeviceString() const {
  auto attr = inst->getAttributeNamed(DEVICE_ATTR);
  assertWithDump(attr.hasValue(), "Tensor op instruction has no device string");
  return attr.getValue().getStringValue();
}

void GraphOperationInfo::assertWithDump(bool cond,
                                        const char *assertMsg) const {
#ifndef NDEBUG
  if (cond)
    return;
  inst->dump();
  llvm_unreachable(assertMsg);
#endif // NDEBUG
}

/// Decode the name of a graph_op into its TensorFlow op name and a list of
/// information about the operands.
StringRef GraphOperationInfo::
decodeName(SmallVectorImpl<InputMarker> &inputInfo) {
  auto name = inst->getName().str();
  auto pos = name.find(',');
  auto opName = name.substr(0, pos);

  while (pos != StringRef::npos) {
    name = name.drop_front(pos+1);
    pos = name.find(',');
    auto letter = name.substr(0, pos);
    assertWithDump(letter.size() == 1, "malformed graph_op instruction");
    InputMarker kind;
    switch (letter[0]) {
    case 's': kind = InputMarker::IM_Scalar; break;
    case 'i': kind = InputMarker::IM_Normal; break;
    case 'L': kind = InputMarker::IM_InputList; break;
    case 'e': kind = InputMarker::IM_InputListElt; break;
    default:
      assertWithDump(false, "malformed graph_op instruction");
    }
    inputInfo.push_back(kind);
  }

  return opName;
}

/// Given an attribute name like foo$dtype, decode the name and the class.  If
/// there is no modifier specified, this defaults to OperandClass::Normal.
std::pair<StringRef, SILTensorOpInfo::OperandClass>
GraphOperationInfo::decodeAttributeName(Identifier name) {
  auto nameStr = name.str();
  // Figure out what the suffix is (if any).
  auto dollarLoc = nameStr.find('$');

  auto opClass = OperandClass::Normal;
  if (dollarLoc != StringRef::npos) {
    auto suffix = nameStr.drop_front(dollarLoc+1);
    opClass = SILTensorOpInfo::getOperandClass(suffix).getValue();
  }

  // Slice the suffix off the attribute name and add the decoded version.
  return { nameStr.substr(0, dollarLoc), opClass };
}

int64_t GraphOperationInfo::getIntAttr(unsigned attrIdx,
                                       StringRef attrName) const {
  auto attr = inst->getAttribute(attrIdx);
  auto attrInfo = GraphOperationInfo::decodeAttributeName(attr.name);
  assert(attrInfo.first == attrName);
  auto attrValue = attr.value;
  return attrValue.getIntegerValue().getLimitedValue();
}

std::string GraphOperationInfo::getStringAttr(unsigned attrIdx,
                                              StringRef attrName) const {
  auto attr = inst->getAttribute(attrIdx);
  auto attrInfo = GraphOperationInfo::decodeAttributeName(attr.name);
  assert(attrInfo.first == attrName);
  auto attrValue = attr.value;
  return attrValue.getStringValue().str();
}

//===----------------------------------------------------------------------===//
// Source Location Manipulation Helpers
//===----------------------------------------------------------------------===//

/// The SIL location for operations we process are usually deep in the bowels
/// of the tensor library code, which are all implementation details to the
/// user.  As such, walk the inlining location of the specified node to return
/// the first location *outside* of the tensor implementation goop.
SILDebugLocation tf::skipInternalLocations(SILDebugLocation loc) {
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
    auto outerType = sei->getType().getASTType();
    if (outerType->is<BuiltinType>() || isTensorHandle(outerType)) {
      if (sei->getOperand())  // Could be called after dropAllReferences().
        return getUserSourceLocation(sei->getOperand());
    }
  }

  return getUserSourceLocation(inst->getDebugLocation());
}

/// Create a "Const" tensor operation containing the specified scalars, with
/// the specified shape and elementType (setting dtype).  The resultType is
/// the TensorHandle type to produce, and targetDevice is the device set for
/// the operation.
GraphOperationInst *
tf::createConstTensor(Type elementType, SymbolicValue scalars,
                      SymbolicValue shape, SILType resultType, SILLocation loc,
                      DeviceType targetDevice, SILBuilder &B) {
  auto &context = B.getASTContext();
  auto &allocator = context.getAllocator();

  // Ensure that the array of initializer values is in the module's allocator.
  scalars = scalars.cloneInto(allocator);
  shape = shape.cloneInto(allocator);
  assert(scalars.getKind() == SymbolicValue::Array &&
         shape.getKind() == SymbolicValue::Array &&
         "expected array constants for scalars and shape");

  SmallVector<GraphOperationAttribute, 8> attributes;

  // Add a dtype attribute for the array element.
  attributes.push_back({
    context.getIdentifier("dtype"),
    SymbolicValue::getMetatype(elementType->getCanonicalType())
  });

  // Add an attribute for the value$tensor attribute.
  auto tensorSuffix =
  SILTensorOpInfo::getOperandClassSuffix(SILTensorOpInfo::OperandClass::Tensor);
  attributes.push_back({
    context.getIdentifier(std::string("value") + tensorSuffix), scalars
  });

  // Add the value$shape attribute.
  auto shapeSuffix =
  SILTensorOpInfo::getOperandClassSuffix(SILTensorOpInfo::OperandClass::Shape);
  attributes.push_back({
    context.getIdentifier(std::string("value") + shapeSuffix), shape
  });

  // All graph_op's get a device.
  attributes.push_back({
    context.getIdentifier(DEVICE_ATTR),
    SymbolicValue::getString(getDeviceString(targetDevice), allocator)
  });

  // Finally build a new graphop instruction with the simplified operands.
  return B.createGraphOperation(loc, context.getIdentifier("Const"),
                                /*operands*/{}, attributes, resultType);
}


//===----------------------------------------------------------------------===//
// TensorFunctionClassifier Implementation
//===----------------------------------------------------------------------===//

/// Return true if the specified function is the top-level context that
/// tensor partitioning should be applied to.  This returns false (for
/// example) for inlined functions that take and return tensors, since we
/// know that they are either unreachable or will be inlined into any
/// clients that use them.
bool TensorFunctionClassifier::shouldBePartitioned(SILFunction *fn) {
  // Ignore transparent functions.
  if (fn->isTransparent())
    return false;

  auto hasInlinableAttrs = [&](Decl *decl) -> bool {
    if (decl->getAttrs().hasAttribute<InlinableAttr>())
      return true;
    return false;
  };

  // Don't transform functions that are marked @_inlineable or inline(always)
  // unless they are a thunk.  Thunks are only generated for inherently
  // interesting declarations.
  if (!fn->isThunk()) {
    if (fn->getInlineStrategy() == AlwaysInline)
      return false;

    if (auto dc = fn->getDeclContext()) {
      if (auto fnDecl = dc->getInnermostDeclarationDeclContext()) {
        if (hasInlinableAttrs(fnDecl))
          return false;

        // If this is an accessor for a computed property, check the property
        // for @_inlineable as well.
        if (auto *fd = dyn_cast<AccessorDecl>(fnDecl))
          if (hasInlinableAttrs(fd->getStorage()))
            return false;
      }
    }
  }

  // If the function is marked public, but it isn't marked inlinable, then it is
  // a public entrypoint that cannot be deabstracted through, so we must
  // transform it.
  //
  // TODO: It will probably be a common error to forget to add the inlinable
  // attributes, we should either infer the attribute or produce better QoI that
  // suggests adding it when an error occurs.
  if (fn->getLinkage() == SILLinkage::Public)
    return true;

  // If the function is explicitly marked @noinline, then it should be
  // partitioned, even if it otherwise looks like it shouldn't be.
  if (fn->getInlineStrategy() == NoInline)
    return true;

  // If this is a function that was inlined from some other module but only
  // exists so we can see into it, don't transform it.  It won't be a canonical
  // declaration for anything anyway.
  if (isAvailableExternally(fn->getLinkage()))
    return false;

  // Something is creating public thunks around 'shared' implementations, which
  // prevents the above check from working.  Check for public functions.
  // FIXME: This should go away when we get deabstraction.
  if (fn->getLinkage() == SILLinkage::Shared)
    if (auto *dc = fn->getDeclContext())
      if (auto *fd = dyn_cast<FuncDecl>(dc))
        if (fd->getFormalAccess() >= AccessLevel::Public)
          return true;

  // Otherwise, the function is either public and inlininable or it is internal
  // to the current module.  In both cases, we check to see if the function
  // takes TensorHandle values as arguments or results.  If so, then we know
  // that it will be inlined away by deabstraction, and we don't need to touch
  // it.
  if (containsTensorFlowValue(fn->getLoweredFunctionType()))
    return false;

  // If it contains no tensor inputs or results, then we are willing to
  // transform it!
  return true;
}

/// Return true if the specified function type has TensorHandle's in its
/// argument or result list, even if they are abstracted by structs or
/// tuples.
bool TensorFunctionClassifier::
containsTensorFlowValue(CanSILFunctionType fnType) {
  for (auto &result : fnType->getResults())
    if (containsTensorFlowValue(result.getType()))
      return true;

  for (auto &param : fnType->getParameters())
    if (containsTensorFlowValue(param.getType()))
      return true;

  return false;
}

bool tf::isShapeArrayPseudoAttr(StringRef attrName, SymbolicValue attrValue) {
  if (attrName != SHAPE_ARRAY_ATTR)
    return false;
  CanType eltType;
  (void)attrValue.getArrayValue(eltType);
  return eltType->getString() == "TensorShape";
}
