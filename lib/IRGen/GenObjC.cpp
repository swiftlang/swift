//===--- GenObjC.cpp - Objective-C interaction ----------------------------===//
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
//  This file implements bridging to Objective-C.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/InlineAsm.h"

#include "clang/AST/ASTContext.h"

#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILModule.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"

#include "CallEmission.h"
#include "Explosion.h"
#include "FormalType.h"
#include "GenClass.h"
#include "GenFunc.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenType.h"
#include "HeapTypeInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "ScalarTypeInfo.h"
#include "StructLayout.h"

#include "GenObjC.h"

using namespace swift;
using namespace irgen;

void IRGenFunction::emitObjCRelease(llvm::Value *value) {
  // Get an appropriately-cast function pointer.
  auto fn = IGM.getObjCReleaseFn();
  if (value->getType() != IGM.ObjCPtrTy) {
    auto fnTy = llvm::FunctionType::get(IGM.VoidTy, value->getType(),
                                        false)->getPointerTo();
    fn = llvm::ConstantExpr::getBitCast(fn, fnTy);
  }

  auto call = Builder.CreateCall(fn, value);
  call->setDoesNotThrow();
}

/// Given a function of type %objc* (%objc*)*, cast it as appropriate
/// to be used with values of type T.
static llvm::Constant *getCastOfRetainFn(IRGenModule &IGM,
                                         llvm::Constant *fn,
                                         llvm::Type *valueTy) {
#ifndef NDEBUG
  auto origFnTy = cast<llvm::FunctionType>(fn->getType()->getPointerElementType());
  assert(origFnTy->getReturnType() == IGM.ObjCPtrTy);
  assert(origFnTy->getNumParams() == 1);
  assert(origFnTy->getParamType(0) == IGM.ObjCPtrTy);
  assert(isa<llvm::PointerType>(valueTy) ||
         valueTy == IGM.IntPtrTy); // happens with optional types
#endif
  if (valueTy == IGM.ObjCPtrTy)
    return fn;

  auto fnTy = llvm::FunctionType::get(valueTy, valueTy, false);
  return llvm::ConstantExpr::getBitCast(fn, fnTy->getPointerTo(0));
}

void IRGenFunction::emitObjCRetain(llvm::Value *v, Explosion &explosion) {
  explosion.add(emitObjCRetainCall(v));
}

llvm::Value *IRGenFunction::emitObjCRetainCall(llvm::Value *value) {
  // Get an appropriately cast function pointer.
  auto fn = IGM.getObjCRetainFn();
  fn = getCastOfRetainFn(IGM, fn, value->getType());

  auto call = Builder.CreateCall(fn, value);
  call->setDoesNotThrow();
  return call;
}

llvm::Value *IRGenFunction::emitObjCAutoreleaseCall(llvm::Value *val) {
  if (val->getType()->isPointerTy())
    val = Builder.CreateBitCast(val, IGM.ObjCPtrTy);
  else
    val = Builder.CreateIntToPtr(val, IGM.ObjCPtrTy);
  
  auto call = Builder.CreateCall(IGM.getObjCAutoreleaseFn(), val);
  call->setDoesNotThrow();
  return call;
}

llvm::Value *IRGenModule::getObjCRetainAutoreleasedReturnValueMarker() {
  // Check to see if we've already computed the market.  Note that we
  // might have cached a null marker, and that's fine.
  auto &cache = ObjCRetainAutoreleasedReturnValueMarker;
  if (cache.hasValue())
    return cache.getValue();

  // Ask the target for the string.
  StringRef asmString = TargetInfo.ObjCRetainAutoreleasedReturnValueMarker;

  // If the string is empty, just leave, remembering that we did all this.
  if (asmString.empty()) {
    cache = nullptr;
    return nullptr;
  }

  // If we're emitting optimized code, record the string in the module
  // and let the late ARC pass insert it, but don't generate any calls
  // right now.
  if (Opts.Optimize) {
    llvm::NamedMDNode *metadata =
      Module.getOrInsertNamedMetadata(
                            "clang.arc.retainAutoreleasedReturnValueMarker");
    assert(metadata->getNumOperands() <= 1);
    if (metadata->getNumOperands() == 0) {
      auto *string = llvm::MDString::get(LLVMContext, asmString);
      metadata->addOperand(llvm::MDNode::get(LLVMContext, string));
    }

    cache = nullptr;

  // Otherwise, create the module
  } else {
    llvm::FunctionType *type =
      llvm::FunctionType::get(VoidTy, /*variadic*/false);
    cache = llvm::InlineAsm::get(type, asmString, "", /*sideeffects*/ true);
  }

  return cache.getValue();
}

/// Reclaim an autoreleased return value.
llvm::Value *irgen::emitObjCRetainAutoreleasedReturnValue(IRGenFunction &IGF,
                                                          llvm::Value *value) {
  // Call the inline-assembly marker if we need one.
  if (auto marker = IGF.IGM.getObjCRetainAutoreleasedReturnValueMarker()) {
    IGF.Builder.CreateCall(marker);
  }

  auto fn = IGF.IGM.getObjCRetainAutoreleasedReturnValueFn();

  // We don't want to cast the function here because it interferes with
  // LLVM's ability to recognize and special-case this function.
  // Note that the parameter and result must also have type i8*.
  llvm::Type *valueType = value->getType();
  if (isa<llvm::PointerType>(valueType)) {
    value = IGF.Builder.CreateBitCast(value, IGF.IGM.Int8PtrTy);
  } else {
    value = IGF.Builder.CreateIntToPtr(value, IGF.IGM.Int8PtrTy);
  }

  auto call = IGF.Builder.CreateCall(fn, value);
  call->setDoesNotThrow();

  llvm::Value *result = call;
  if (isa<llvm::PointerType>(valueType)) {
    result = IGF.Builder.CreateBitCast(result, valueType);
  } else {
    result = IGF.Builder.CreatePtrToInt(result, valueType);
  }
  return result;
}

/// Autorelease a return value.
llvm::Value *irgen::emitObjCAutoreleaseReturnValue(IRGenFunction &IGF,
                                                   llvm::Value *value) {
  auto fn = IGF.IGM.getObjCAutoreleaseReturnValueFn();
  fn = getCastOfRetainFn(IGF.IGM, fn, value->getType());

  auto call = IGF.Builder.CreateCall(fn, value);
  call->setDoesNotThrow();
  call->setTailCall(); // force tail calls at -O0
  return call;
}

namespace {
  /// A type-info implementation suitable for an ObjC pointer type.
  class ObjCTypeInfo : public HeapTypeInfo<ObjCTypeInfo> {
  public:
    ObjCTypeInfo(llvm::PointerType *storageType, Size size,
                 SpareBitVector spareBits, Alignment align)
      : HeapTypeInfo(storageType, size, spareBits, align) {
    }

    /// Builtin.UnknownObject requires ObjC reference-counting.
    ReferenceCounting getReferenceCounting() const {
      return ReferenceCounting::ObjC;
    }
  };
}

const LoadableTypeInfo *TypeConverter::convertBuiltinUnknownObject() {
  return new ObjCTypeInfo(IGM.ObjCPtrTy, IGM.getPointerSize(),
                          IGM.getHeapObjectSpareBits(),
                          IGM.getPointerAlignment());
}

namespace {
  /// A type info implementation for BridgeObject
  class BridgeObjectTypeInfo : public HeapTypeInfo<BridgeObjectTypeInfo> {
  public:
    BridgeObjectTypeInfo(llvm::PointerType *storageType, Size size,
                 SpareBitVector spareBits, Alignment align)
      : HeapTypeInfo(storageType, size, spareBits, align) {
    }

    /// Builtin.BridgeObject uses its own specialized refcounting implementation.
    ReferenceCounting getReferenceCounting() const {
      return ReferenceCounting::Bridge;
    }
    
    // BridgeObject exposes only null as an extra inhabitant for enum layout.
    // Other representations are reserved for future use by the stdlib.
    
    bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
      return true;
    }
    unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
      return 1;
    }
    llvm::ConstantInt *getFixedExtraInhabitantValue(IRGenModule &IGM,
                                                unsigned bits,
                                                unsigned index) const override {
      return llvm::ConstantInt::get(
                         llvm::IntegerType::get(IGM.getLLVMContext(), bits), 0);
    }
    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                         SILType T) const override {
      src = IGF.Builder.CreateBitCast(src, IGF.IGM.SizeTy->getPointerTo());
      auto val = IGF.Builder.CreateLoad(src);
      auto isNonzero = IGF.Builder.CreateICmpNE(val,
                                    llvm::ConstantInt::get(IGF.IGM.SizeTy, 0));
      // We either have extra inhabitant 0 or no extra inhabitant (-1).
      // Conveniently, this is just a sext i1 -> i32 away.
      return IGF.Builder.CreateSExt(isNonzero, IGF.IGM.Int32Ty);
    }
    void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                              Address dest, SILType T) const override {
      // There's only one extra inhabitant, 0.
      dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.SizeTy->getPointerTo());
      IGF.Builder.CreateStore(llvm::ConstantInt::get(IGF.IGM.SizeTy, 0), dest);
    }
  };
}


const LoadableTypeInfo *TypeConverter::convertBuiltinBridgeObject() {
  return new BridgeObjectTypeInfo(IGM.BridgeObjectPtrTy, IGM.getPointerSize(),
      SpareBitVector::getConstant(IGM.getPointerSize().getValueInBits(), false),
                                  IGM.getPointerAlignment());
}

const TypeInfo &IRGenModule::getObjCClassPtrTypeInfo() {
  return Types.getObjCClassPtrTypeInfo();
}

const LoadableTypeInfo &TypeConverter::getObjCClassPtrTypeInfo() {
  // ObjC class pointers look like unmanaged (untagged) object references.
  if (ObjCClassPtrTI) return *ObjCClassPtrTI;
  ObjCClassPtrTI =
    createUnmanagedStorageType(IGM.ObjCClassPtrTy);
  ObjCClassPtrTI->NextConverted = FirstType;
  FirstType = ObjCClassPtrTI;
  return *ObjCClassPtrTI;
}

/// Get or create a global Objective-C method name.  Always returns an i8*.
llvm::Constant *IRGenModule::getAddrOfObjCMethodName(StringRef selector) {
  // Check whether this selector already exists.
  auto &entry = ObjCMethodNames[selector];
  if (entry) return entry;

  // If not, create it.  This implicitly adds a trailing null.
  auto init = llvm::ConstantDataArray::getString(LLVMContext, selector);
  auto global = new llvm::GlobalVariable(Module, init->getType(), true,
                                         llvm::GlobalValue::InternalLinkage,
                                         init,
                          llvm::Twine("\01L_selector_data(") + selector + ")");
  global->setSection("__TEXT,__objc_methname,cstring_literals");
  global->setAlignment(1);

  // Drill down to make an i8*.
  auto zero = llvm::ConstantInt::get(SizeTy, 0);
  llvm::Constant *indices[] = { zero, zero };
  auto address = llvm::ConstantExpr::getInBoundsGetElementPtr(global, indices);

  // Cache and return.
  entry = address;
  return address;
}

/// Get or create an Objective-C selector reference.  Always returns
/// an i8**.  The design is that the compiler will emit a load of this
/// pointer, and the linker will ensure that that pointer is unique.
llvm::Constant *IRGenModule::getAddrOfObjCSelectorRef(StringRef selector) {
  // Check whether a reference for this selector already exists.
  auto &entry = ObjCSelectorRefs[selector];
  if (entry) return entry;

  // If not, create it.  The initializer is just a pointer to the
  // method name.  Note that the label here is unimportant, so we
  // choose something descriptive to make the IR readable.
  auto init = getAddrOfObjCMethodName(selector);
  auto global = new llvm::GlobalVariable(Module, init->getType(), false,
                                         llvm::GlobalValue::InternalLinkage,
                                         init,
                                llvm::Twine("\01L_selector(") + selector + ")");
  global->setAlignment(getPointerAlignment().getValue());

  // This section name is magical for the Darwin static and dynamic linkers.
  global->setSection("__DATA,__objc_selrefs,literal_pointers,no_dead_strip");

  // Make sure that this reference does not get optimized away.
  addUsedGlobal(global);

  // Cache and return.
  entry = global;
  return global;
}

/// Get or create an ObjC protocol record. Always returns an i8*. We lazily
/// create ObjC protocol_t records for protocols, storing references to the
/// record into the __objc_protolist and  and __objc_protorefs sections to be
/// fixed up by the runtime.
///
/// It is not correct to use this value as a Protocol* reference directly. The
/// ObjC runtime requires protocol references to be loaded from an
/// indirect variable, the address of which is given by
/// getAddrOfObjCProtocolRef.
llvm::Constant *IRGenModule::getAddrOfObjCProtocolRecord(ProtocolDecl *proto,
                                               ForDefinition_t forDefinition) {
  return const_cast<llvm::Constant*>
    (cast<llvm::Constant>(getObjCProtocolGlobalVars(proto).record));
}

/// Get or create an ObjC protocol reference. Always returns an i8**. We lazily
/// create ObjC protocol_t records for protocols, storing references to the
/// record into the __objc_protolist and  and __objc_protorefs sections to be
/// fixed up by the runtime.
llvm::Constant *IRGenModule::getAddrOfObjCProtocolRef(ProtocolDecl *proto,
                                               ForDefinition_t forDefinition) {
  return const_cast<llvm::Constant*>
    (cast<llvm::Constant>(getObjCProtocolGlobalVars(proto).ref));
}

IRGenModule::ObjCProtocolPair
IRGenModule::getObjCProtocolGlobalVars(ProtocolDecl *proto) {
  // See whether we already emitted this protocol reference.
  auto found = ObjCProtocols.find(proto);
  if (found != ObjCProtocols.end()) {
    return found->second;
  }
  
  // Emit the protocol record.
  llvm::Constant *protocolRecord = emitObjCProtocolData(*this, proto);
  protocolRecord = llvm::ConstantExpr::getBitCast(protocolRecord, Int8PtrTy);

  // Introduce a variable to label the protocol.
  llvm::SmallString<64> nameBuffer;
  StringRef protocolName = proto->getObjCRuntimeName(nameBuffer);
  auto *protocolLabel
    = new llvm::GlobalVariable(Module, protocolRecord->getType(),
                               /*constant*/ false,
                               llvm::GlobalValue::WeakAnyLinkage,
                               protocolRecord,
                               llvm::Twine("\01l_OBJC_LABEL_PROTOCOL_$_")
                                 + protocolName);
  protocolLabel->setAlignment(getPointerAlignment().getValue());
  protocolLabel->setVisibility(llvm::GlobalValue::HiddenVisibility);
  protocolLabel->setSection("__DATA,__objc_protolist,coalesced,no_dead_strip");
  
  // Introduce a variable to reference the protocol.
  auto *protocolRef
    = new llvm::GlobalVariable(Module, protocolRecord->getType(),
                               /*constant*/ false,
                               llvm::GlobalValue::WeakAnyLinkage,
                               protocolRecord,
                               llvm::Twine("\01l_OBJC_PROTOCOL_REFERENCE_$_")
                                 + protocolName);
  protocolRef->setAlignment(getPointerAlignment().getValue());
  protocolRef->setVisibility(llvm::GlobalValue::HiddenVisibility);
  protocolRef->setSection("__DATA,__objc_protorefs,coalesced,no_dead_strip");

  ObjCProtocolPair pair{protocolRecord, protocolRef};
  ObjCProtocols.insert({proto, pair});
  
  return pair;
}

namespace {
  class Selector {
    
    llvm::SmallString<80> Buffer;
    StringRef Text;

  public:

    static constexpr struct ForGetter_t { } ForGetter{};
    static constexpr struct ForSetter_t { } ForSetter{};

#define FOREACH_FAMILY(FAMILY)         \
    FAMILY(Alloc, "alloc")             \
    FAMILY(Copy, "copy")               \
    FAMILY(Init, "init")               \
    FAMILY(MutableCopy, "mutableCopy") \
    FAMILY(New, "new")

    // Note that these are in parallel with 'prefixes', below.
    enum class Family {
      None,
#define GET_LABEL(LABEL, PREFIX) LABEL,
      FOREACH_FAMILY(GET_LABEL)
#undef GET_LABEL
    };
    
    Selector() = default;

    Selector(FuncDecl *method) {
      Text = method->getObjCSelector().getString(Buffer);
    }
    
    Selector(ConstructorDecl *ctor) {
      Text = ctor->getObjCSelector().getString(Buffer);
    }
    
    Selector(ValueDecl *methodOrCtorOrDtor) {
      if (auto *method = dyn_cast<FuncDecl>(methodOrCtorOrDtor)) {
        Text = method->getObjCSelector().getString(Buffer);
      } else if (auto *ctor = dyn_cast<ConstructorDecl>(methodOrCtorOrDtor)) {
        Text = ctor->getObjCSelector().getString(Buffer);
      } else if (isa<DestructorDecl>(methodOrCtorOrDtor)) {
        Text = "dealloc";
      } else {
        llvm_unreachable("property or subscript selector should be generated "
                         "using ForGetter or ForSetter constructors");
      }
    }
    
    Selector(AbstractStorageDecl *asd, ForGetter_t) {
      Text = asd->getObjCGetterSelector().getString(Buffer);
    }

    Selector(AbstractStorageDecl *asd, ForSetter_t) {
      Text = asd->getObjCSetterSelector().getString(Buffer);
    }

    Selector(SILDeclRef ref) {
      switch (ref.kind) {
      case SILDeclRef::Kind::DefaultArgGenerator:
      case SILDeclRef::Kind::EnumElement:
      case SILDeclRef::Kind::GlobalAccessor:
      case SILDeclRef::Kind::GlobalGetter:
        llvm_unreachable("Method does not have a selector");

      case SILDeclRef::Kind::Destroyer:
      case SILDeclRef::Kind::Deallocator:
        Text = "dealloc";
        break;
          
      case SILDeclRef::Kind::Func:
        Text = cast<FuncDecl>(ref.getDecl())->getObjCSelector()
                 .getString(Buffer);
        break;

      case SILDeclRef::Kind::Allocator:
      case SILDeclRef::Kind::Initializer:
        Text = cast<ConstructorDecl>(ref.getDecl())->getObjCSelector()
                 .getString(Buffer);
        break;

      case SILDeclRef::Kind::IVarInitializer:
        Text = ".cxx_construct";
        break;

      case SILDeclRef::Kind::IVarDestroyer:
        Text = ".cxx_destruct";
        break;
      }
    }

    StringRef str() const {
      return Text;
    }

    /// Return the family string of this selector.
    Family getFamily() const {
      StringRef text = str();
      while (!text.empty() && text[0] == '_') text = text.substr(1);

#define CHECK_PREFIX(LABEL, PREFIX) \
      if (hasPrefix(text, PREFIX)) return Family::LABEL;
      FOREACH_FAMILY(CHECK_PREFIX)
#undef CHECK_PREFIX

      return Family::None;
    }

  private:
    /// Does the given selector start with the given string as a
    /// prefix, in the sense of the selector naming conventions?
    static bool hasPrefix(StringRef text, StringRef prefix) {
      if (!text.startswith(prefix)) return false;
      if (text.size() == prefix.size()) return true;
      assert(text.size() > prefix.size());
      return !islower(text[prefix.size()]);
    }

#undef FOREACH_FAMILY
  };
}

static void emitSuperArgument(IRGenFunction &IGF, bool isInstanceMethod,
                              llvm::Value *selfValue,
                              Explosion &selfValues,
                              SILType searchClass) {
  // Allocate an objc_super struct.
  Address super = IGF.createAlloca(IGF.IGM.ObjCSuperStructTy,
                                   IGF.IGM.getPointerAlignment(),
                                   "objc_super");
  llvm::Value *self = IGF.Builder.CreateBitCast(selfValue,
                                                IGF.IGM.ObjCPtrTy);
  
  // Generate the search class object reference.
  llvm::Value *searchValue;
  if (isInstanceMethod) {
    searchValue = emitClassHeapMetadataRef(IGF, searchClass.getSwiftRValueType(),
                                           MetadataValueType::ObjCClass,
                                           /*allow uninitialized*/ true);
  } else {
    ClassDecl *searchClassDecl =
      searchClass.castTo<MetatypeType>().getInstanceType()
        .getClassOrBoundGenericClass();
    searchValue = IGF.IGM.getAddrOfMetaclassObject(searchClassDecl,
                                                   NotForDefinition);
  }
  
  // Store the receiver and class to the struct.
  llvm::Value *selfIndices[2] = {
    IGF.Builder.getInt32(0),
    IGF.Builder.getInt32(0)
  };
  llvm::Value *selfAddr = IGF.Builder.CreateGEP(super.getAddress(),
                                                selfIndices);
  IGF.Builder.CreateStore(self, selfAddr, super.getAlignment());

  llvm::Value *searchIndices[2] = {
    IGF.Builder.getInt32(0),
    IGF.Builder.getInt32(1)
  };
  llvm::Value *searchAddr = IGF.Builder.CreateGEP(super.getAddress(),
                                                  searchIndices);
  IGF.Builder.CreateStore(searchValue, searchAddr, super.getAlignment());
  
  // Pass a pointer to the objc_super struct to the messenger.
  // Project the ownership semantics of 'self' to the super argument.
  selfValues.add(super.getAddress());
}

static llvm::FunctionType *getMsgSendSuperTy(IRGenModule &IGM,
                                             llvm::FunctionType *fnTy,
                                             bool indirectResult) {
  SmallVector<llvm::Type*, 4> args(fnTy->param_begin(), fnTy->param_end());
  if (indirectResult)
    args[1] = IGM.ObjCSuperPtrTy;
  else
    args[0] = IGM.ObjCSuperPtrTy;
  return llvm::FunctionType::get(fnTy->getReturnType(), args, fnTy->isVarArg());
}

/// Prepare a call using ObjC method dispatch without applying the 'self' and
/// '_cmd' arguments.
CallEmission irgen::prepareObjCMethodRootCall(IRGenFunction &IGF,
                                              SILDeclRef method,
                                              CanSILFunctionType origFnType,
                                              CanSILFunctionType substFnType,
                                              ArrayRef<Substitution> subs,
                                              ObjCMessageKind kind) {
  assert((method.kind == SILDeclRef::Kind::Initializer
          || method.kind == SILDeclRef::Kind::Allocator
          || method.kind == SILDeclRef::Kind::Func
          || method.kind == SILDeclRef::Kind::Destroyer
          || method.kind == SILDeclRef::Kind::Deallocator) &&
         "objc method call must be to a func/initializer/getter/setter/dtor");

  llvm::AttributeSet attrs;
  auto fnTy = IGF.IGM.getFunctionType(origFnType,
                                      ExtraData::None,
                                      attrs);
  bool indirectResult = requiresExternalIndirectResult(IGF.IGM, origFnType);
  if (kind != ObjCMessageKind::Normal)
    fnTy = getMsgSendSuperTy(IGF.IGM, fnTy, indirectResult);

  // Create the appropriate messenger function.
  // FIXME: this needs to be target-specific.
  llvm::Constant *messenger;
  if (indirectResult && IGF.IGM.TargetInfo.ObjCUseStret) {
    switch (kind) {
    case ObjCMessageKind::Normal:
      messenger = IGF.IGM.getObjCMsgSendStretFn();
      break;

    case ObjCMessageKind::Peer:
      messenger = IGF.IGM.getObjCMsgSendSuperStretFn();
      break;

    case ObjCMessageKind::Super:
      messenger = IGF.IGM.getObjCMsgSendSuperStret2Fn();
      break;
    }
  } else {
    switch (kind) {
    case ObjCMessageKind::Normal:
      messenger = IGF.IGM.getObjCMsgSendFn();
      break;

    case ObjCMessageKind::Peer:
      messenger = IGF.IGM.getObjCMsgSendSuperFn();
      break;

    case ObjCMessageKind::Super:
      messenger = IGF.IGM.getObjCMsgSendSuper2Fn();
      break;
    }
  }

  // Cast the messenger to the right type.
  messenger = llvm::ConstantExpr::getBitCast(messenger, fnTy->getPointerTo());

  CallEmission emission(IGF,
                        Callee::forKnownFunction(origFnType,
                                                 substFnType,
                                                 subs,
                                                 messenger, nullptr));
  return emission;
}

/// Emit the 'self'/'super' and '_cmd' arguments for an ObjC method dispatch.
void irgen::addObjCMethodCallImplicitArguments(IRGenFunction &IGF,
                                               Explosion &args,
                                               SILDeclRef method,
                                               llvm::Value *self,
                                               SILType searchType) {
  // Compute the selector.
  Selector selector(method);
    
  // super.constructor references an instance method (even though the
  // decl is really a 'static' member). Similarly, destructors refer
  // to the instance method -dealloc.
  bool isInstanceMethod
    = method.kind == SILDeclRef::Kind::Initializer
      || method.kind == SILDeclRef::Kind::Deallocator
      || method.getDecl()->isInstanceMember();

  if (searchType) {
    emitSuperArgument(IGF, isInstanceMethod, self, args, searchType);
  } else {
    args.add(self);
  }
  assert(args.size() == 1);
  
  // Add the selector value.
  args.add(IGF.emitObjCSelectorRefLoad(selector.str()));
}

/// Return the formal type that we would use for +allocWithZone:.
static CanSILFunctionType getAllocObjectFormalType(ASTContext &ctx,
                                                   CanType classType) {
  SILParameterInfo inputs[] = {
    SILParameterInfo(CanType(ctx.TheRawPointerType), /* (NSZone*), kindof */
                     ParameterConvention::Direct_Unowned),
    SILParameterInfo(CanType(MetatypeType::get(classType,
                                               MetatypeRepresentation::Thick)),
                     ParameterConvention::Direct_Unowned)
  };
  auto result = SILResultInfo(classType, ResultConvention::Owned);
  auto extInfo = SILFunctionType::ExtInfo(AbstractCC::ObjCMethod,
                                          FunctionType::Representation::Thin,
                                          /*noreturn*/ false);

  return SILFunctionType::get(nullptr, extInfo,
                              /*callee*/ ParameterConvention::Direct_Unowned,
                              inputs, result, ctx);
}

/// Call [self allocWithZone: nil].
llvm::Value *irgen::emitObjCAllocObjectCall(IRGenFunction &IGF,
                                            llvm::Value *self,
                                            CanType classType) {
  // Compute the formal type that we expect +allocWithZone: to have.
  auto formalType = getAllocObjectFormalType(IGF.IGM.Context, classType);

  // Compute the appropriate LLVM type for the function.
  llvm::AttributeSet attrs;
  auto fnTy = IGF.IGM.getFunctionType(formalType, ExtraData::None, attrs);

  // Get the messenger function.
  llvm::Constant *messenger = IGF.IGM.getObjCMsgSendFn();
  messenger = llvm::ConstantExpr::getBitCast(messenger, fnTy->getPointerTo());

  // Prepare the call.
  CallEmission emission(IGF, Callee::forKnownFunction(formalType,
                                                      formalType, {},
                                                      messenger, nullptr));

  // Emit the arguments.
  {
    Explosion args;
    args.add(self);
    args.add(IGF.emitObjCSelectorRefLoad("allocWithZone:"));
    args.add(llvm::ConstantPointerNull::get(IGF.IGM.Int8PtrTy));
    emission.addArg(args);
  }

  // Emit the call.
  Explosion out;
  emission.emitToExplosion(out);
  return out.claimNext();
}

static llvm::Function *emitObjCPartialApplicationForwarder(IRGenModule &IGM,
                                            SILDeclRef method,
                                            CanSILFunctionType origMethodType,
                                            CanSILFunctionType resultType,
                                            const HeapLayout &layout,
                                            SILType selfType) {
  auto &selfTI = IGM.getTypeInfo(selfType);
  
  llvm::AttributeSet attrs;
  llvm::FunctionType *fwdTy = IGM.getFunctionType(resultType,
                                                  ExtraData::Retainable,
                                                  attrs);
  // FIXME: Give the thunk a real name.
  // FIXME: Maybe cache the thunk by function and closure types?
  llvm::Function *fwd =
    llvm::Function::Create(fwdTy, llvm::Function::InternalLinkage,
                           "_TPAo", &IGM.Module);
  fwd->setAttributes(attrs);
  
  IRGenFunction subIGF(IGM, fwd);
  
  // Do we need to lifetime-extend self?
  bool lifetimeExtendsSelf;
  switch (origMethodType->getResult().getConvention()) {
  case ResultConvention::UnownedInnerPointer:
    lifetimeExtendsSelf = true;
    break;
      
  case ResultConvention::Unowned:
  case ResultConvention::Owned:
  case ResultConvention::Autoreleased:
    lifetimeExtendsSelf = false;
    break;
  }
  
  // Do we need to retain self before calling, and/or release it after?
  bool retainsSelf;
  switch (origMethodType->getParameters().back().getConvention()) {
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Deallocating:
    retainsSelf = false;
    break;
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Owned:
    retainsSelf = true;
    break;
  case ParameterConvention::Indirect_In_Guaranteed:
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_Out:
  case ParameterConvention::Indirect_Inout:
    llvm_unreachable("self passed indirectly?!");
  }
  
  // Recover 'self' from the context.
  Explosion params = subIGF.collectParameters();
  llvm::Value *context = params.takeLast();
  Address dataAddr = layout.emitCastTo(subIGF, context);
  auto &fieldLayout = layout.getElements()[0];
  Address selfAddr = fieldLayout.project(subIGF, dataAddr, None);
  Explosion selfParams;
  if (retainsSelf)
    cast<LoadableTypeInfo>(selfTI).loadAsCopy(subIGF, selfAddr, selfParams);
  else
    cast<LoadableTypeInfo>(selfTI).loadAsTake(subIGF, selfAddr, selfParams);
  llvm::Value *self = selfParams.claimNext();
  
  // Save off the forwarded indirect return address if we have one.
  llvm::Value *indirectReturn = nullptr;
  SILType appliedResultTy = origMethodType->getSemanticResultSILType();
  auto &appliedResultTI = IGM.getTypeInfo(appliedResultTy);
  if (appliedResultTI.getSchema().requiresIndirectResult(IGM)) {
    indirectReturn = params.claimNext();
  }

  // Prepare the call to the underlying method.
  CallEmission emission
    = prepareObjCMethodRootCall(subIGF, method, origMethodType, origMethodType,
                                ArrayRef<Substitution>{},
                                ObjCMessageKind::Normal);
  
  Explosion args;
  addObjCMethodCallImplicitArguments(subIGF, args, method, self, SILType());
  args.add(params.claimAll());
  emission.addArg(args);
  
  // Cleanup that always has to occur after the function call.
  auto cleanup = [&]{
    // Lifetime-extend 'self' by sending it to the autorelease pool if need be.
    if (lifetimeExtendsSelf) {
      subIGF.emitObjCRetainCall(self);
      subIGF.emitObjCAutoreleaseCall(self);
    }
    // Release the context.
    subIGF.emitRelease(context);
  };
  
  // Emit the call and produce the return value.
  if (indirectReturn) {
    emission.emitToMemory(appliedResultTI.getAddressForPointer(indirectReturn),
                          appliedResultTI);
    cleanup();
    subIGF.Builder.CreateRetVoid();
  } else {
    Explosion result;
    emission.emitToExplosion(result);
    cleanup();
    auto &callee = emission.getCallee();
    auto resultType = callee.getOrigFunctionType()->getSILResult();
    subIGF.emitScalarReturn(resultType, result);
  }
  
  return fwd;
}

void irgen::emitObjCPartialApplication(IRGenFunction &IGF,
                                       SILDeclRef method,
                                       CanSILFunctionType origMethodType,
                                       CanSILFunctionType resultType,
                                       llvm::Value *self,
                                       SILType selfType,
                                       Explosion &out) {
  // Create a heap object to contain the self argument.
  // TODO: If function context arguments were given objc retain counts,
  // we wouldn't need to create a separate heap object here.
  auto *selfTypeInfo = &IGF.getTypeInfo(selfType);
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal,
                    selfType, selfTypeInfo);
  llvm::Value *data = IGF.emitUnmanagedAlloc(layout, "closure");
  // FIXME: non-fixed offsets
  NonFixedOffsets offsets = None;
  Address dataAddr = layout.emitCastTo(IGF, data);
  auto &fieldLayout = layout.getElements()[0];
  auto &fieldType = layout.getElementTypes()[0];
  Address fieldAddr = fieldLayout.project(IGF, dataAddr, offsets);
  Explosion selfParams;
  selfParams.add(self);
  fieldLayout.getType().initializeFromParams(IGF, selfParams,
                                             fieldAddr, fieldType);

  // Create the forwarding stub.
  llvm::Function *forwarder = emitObjCPartialApplicationForwarder(IGF.IGM,
                                                                method,
                                                                origMethodType,
                                                                resultType,
                                                                layout,
                                                                selfType);
  llvm::Value *forwarderValue = IGF.Builder.CreateBitCast(forwarder,
                                                          IGF.IGM.Int8PtrTy);
  
  // Emit the result explosion.
  out.add(forwarderValue);
  out.add(data);
}

/// Create the LLVM function declaration for a thunk that acts like
/// an Objective-C method for a Swift method implementation.
static llvm::Constant *findSwiftAsObjCThunk(IRGenModule &IGM, SILDeclRef ref) {
  SILFunction *SILFn = IGM.SILMod->lookUpFunction(ref);
  assert(SILFn && "no IR function for swift-as-objc thunk");
  auto fn = IGM.getAddrOfSILFunction(SILFn, NotForDefinition);
  // FIXME: Should set the linkage of the SILFunction to 'internal'.
  fn->setVisibility(llvm::GlobalValue::DefaultVisibility);
  fn->setLinkage(llvm::GlobalValue::InternalLinkage);
  fn->setUnnamedAddr(true);

  return llvm::ConstantExpr::getBitCast(fn, IGM.Int8PtrTy);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given property's getter method implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCGetterPointer(IRGenModule &IGM,
                                            AbstractStorageDecl *property) {
  // Protocol properties have no impl.
  if (isa<ProtocolDecl>(property->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  // FIXME: Explosion level
  ResilienceExpansion expansion = ResilienceExpansion::Minimal;

  SILDeclRef getter = SILDeclRef(property->getGetter(), SILDeclRef::Kind::Func,
                                 expansion,
                                 SILDeclRef::ConstructAtNaturalUncurryLevel,
                                 /*foreign*/ true);

  return findSwiftAsObjCThunk(IGM, getter);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given property's setter method implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCSetterPointer(IRGenModule &IGM,
                                            AbstractStorageDecl *property) {
  // Protocol properties have no impl.
  if (isa<ProtocolDecl>(property->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  assert(property->isSettable(property->getDeclContext()) &&
         "property is not settable?!");
  
  ResilienceExpansion expansion = ResilienceExpansion::Minimal;
  SILDeclRef setter = SILDeclRef(property->getSetter(), SILDeclRef::Kind::Func,
                                 expansion,
                                 SILDeclRef::ConstructAtNaturalUncurryLevel,
                                 /*foreign*/ true);

  return findSwiftAsObjCThunk(IGM, setter);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given method implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCMethodPointer(IRGenModule &IGM,
                                            FuncDecl *method) {
  // Protocol methods have no impl.
  if (isa<ProtocolDecl>(method->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  ResilienceExpansion expansion = ResilienceExpansion::Minimal;
  SILDeclRef declRef = SILDeclRef(method, SILDeclRef::Kind::Func,
                                  expansion,
                                  SILDeclRef::ConstructAtNaturalUncurryLevel,
                                  /*foreign*/ true);

  return findSwiftAsObjCThunk(IGM, declRef);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given constructor implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCMethodPointer(IRGenModule &IGM,
                                            ConstructorDecl *constructor) {
  // Protocol methods have no impl.
  if (isa<ProtocolDecl>(constructor->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  ResilienceExpansion expansion = ResilienceExpansion::Minimal;
  SILDeclRef declRef = SILDeclRef(constructor, SILDeclRef::Kind::Initializer,
                                  expansion,
                                  SILDeclRef::ConstructAtNaturalUncurryLevel,
                                  /*foreign*/ true);

  return findSwiftAsObjCThunk(IGM, declRef);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given destructor implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCMethodPointer(IRGenModule &IGM,
                                            DestructorDecl *destructor) {
  ResilienceExpansion expansion = ResilienceExpansion::Minimal;
  SILDeclRef declRef = SILDeclRef(destructor, SILDeclRef::Kind::Deallocator,
                                  expansion,
                                  SILDeclRef::ConstructAtNaturalUncurryLevel,
                                  /*foreign*/ true);

  return findSwiftAsObjCThunk(IGM, declRef);
}

void irgen::getObjCEncodingForPropertyType(IRGenModule &IGM,
                                           Type t, std::string &s) {
  // FIXME: Property encoding differs in slight ways that aren't publicly
  // exposed from Clang.
  IGM.getClangASTContext()
    .getObjCEncodingForPropertyType(IGM.getClangType(t->getCanonicalType()), s);
}

/// True if the value is of class type, or of a type that is bridged to class
/// type.
bool irgen::hasObjCClassRepresentation(IRGenModule &IGM, Type t) {
  return IGM.SILMod->Types.getLoweredBridgedType(t, AbstractCC::ObjCMethod,
                                                 nullptr)
    ->getClassOrBoundGenericClass();
}

static void
HelperGetObjCEncodingForType(const clang::ASTContext &Context,
                             clang::CanQualType T,
                             std::string &S, bool Extended) {
  
  Context.getObjCEncodingForMethodParameter(clang::Decl::OBJC_TQ_None,
                                            T, S, Extended);
}

static llvm::Constant *GetObjCEncodingForTypes(IRGenModule &IGM,
                                               CanType Result,
                                               ArrayRef<CanType> Args,
                                               StringRef FixedArgs,
                                               Size::int_type ParmOffset,
                                               bool Extended) {
  auto &clangASTContext = IGM.getClangASTContext();
  
  // TODO. Encode type qualifer, 'in', 'inout', etc. for the parameter.
  std::string EncodeStr;
  auto clangType = IGM.getClangType(Result->getCanonicalType());
  if (clangType.isNull())
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  HelperGetObjCEncodingForType(clangASTContext, clangType, EncodeStr, Extended);
  std::string ArgsStr;
  
  // Argument types.
  for (auto ArgType : Args) {
    auto PType = IGM.getClangType(ArgType->getCanonicalType());
    if (PType.isNull())
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    
    // TODO. Some stuff related to Array and Function type is missing.
    // TODO. Encode type qualifer, 'in', 'inout', etc. for the parameter.
    HelperGetObjCEncodingForType(clangASTContext, PType, ArgsStr, Extended);
    ArgsStr += llvm::itostr(ParmOffset);
    clang::CharUnits sz = clangASTContext.getObjCEncodingTypeSize(PType);
    ParmOffset += sz.getQuantity();
  }
  
  EncodeStr += llvm::itostr(ParmOffset);
  EncodeStr += FixedArgs;
  EncodeStr += ArgsStr;
  return IGM.getAddrOfGlobalString(StringRef(EncodeStr));
}

static llvm::Constant * GetObjCEncodingForMethodType(IRGenModule &IGM,
                                                     CanAnyFunctionType T,
                                                     bool Extended) {
  CanType Result = T.getResult();
  CanType Input = T.getInput();
  SmallVector<CanType, 4> Inputs;
  
  // Unravel one level of tuple.
  if (auto tuple = dyn_cast<TupleType>(Input)) {
    for (unsigned i = 0, e = tuple->getNumElements(); i < e; ++i)
      Inputs.push_back(tuple.getElementType(i));
  } else {
    Inputs.push_back(Input);
  }
  
  // Include the encoding for 'self' and '_cmd'.
  llvm::SmallString<8> specialParams;
  specialParams += "@0:";
  auto ptrSize = IGM.getPointerSize().getValue();
  specialParams += llvm::itostr(ptrSize);
  return GetObjCEncodingForTypes(IGM, Result, Inputs, specialParams,
                                 ptrSize * 2, Extended);
}

/// Emit the components of an Objective-C method descriptor: its selector,
/// type encoding, and IMP pointer.
void irgen::emitObjCMethodDescriptorParts(IRGenModule &IGM,
                                          AbstractFunctionDecl *method,
                                          bool extendedEncoding,
                                          bool concrete,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  Selector selector(method);
  
  /// The first element is the selector.
  selectorRef = IGM.getAddrOfObjCMethodName(selector.str());
  
  /// The second element is the type @encoding.
  CanAnyFunctionType methodType
    = cast<AnyFunctionType>(method->getType()->getCanonicalType());
  
  if (!isa<DestructorDecl>(method)) {
    // Account for the 'self' pointer being curried.
    methodType = cast<AnyFunctionType>(methodType.getResult());
  }
  atEncoding = GetObjCEncodingForMethodType(IGM, methodType, extendedEncoding);
  
  /// The third element is the method implementation pointer.
  if (!concrete) {
    impl = nullptr;
    return;
  }
  
  if (auto func = dyn_cast<FuncDecl>(method))
    impl = getObjCMethodPointer(IGM, func);
  else if (auto ctor = dyn_cast<ConstructorDecl>(method))
    impl = getObjCMethodPointer(IGM, ctor);
  else
    impl = getObjCMethodPointer(IGM, cast<DestructorDecl>(method));
}

/// Emit the components of an Objective-C method descriptor for a
/// property getter method.
void irgen::emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                          VarDecl *property,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  Selector getterSel(property, Selector::ForGetter);
  selectorRef = IGM.getAddrOfObjCMethodName(getterSel.str());
  
  auto &clangASTContext = IGM.getClangASTContext();
  std::string TypeStr;
  auto swiftType = property->getType()->getReferenceStorageReferent();
  auto clangType = IGM.getClangType(swiftType->getCanonicalType());
  if (clangType.isNull()) {
    atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    return;
  }
  clangASTContext.getObjCEncodingForType(clangType, TypeStr);
  
  Size PtrSize = IGM.getPointerSize();
  Size::int_type ParmOffset = 2 * PtrSize.getValue();
  
  TypeStr += llvm::itostr(ParmOffset);
  TypeStr += "@0:";
  TypeStr += llvm::itostr(PtrSize.getValue());
  atEncoding = IGM.getAddrOfGlobalString(TypeStr.c_str());
  impl = getObjCGetterPointer(IGM, property);
}

/// Emit the components of an Objective-C method descriptor for a
/// subscript getter method.
void irgen::emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                          SubscriptDecl *subscript,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  Selector getterSel(subscript, Selector::ForGetter);
  selectorRef = IGM.getAddrOfObjCMethodName(getterSel.str());
  atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  impl = getObjCGetterPointer(IGM, subscript);
}

void irgen::emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                          AbstractStorageDecl *decl,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  if (auto sub = dyn_cast<SubscriptDecl>(decl)) {
    return emitObjCGetterDescriptorParts(IGM, sub,
                                         selectorRef, atEncoding, impl);
  }
  if (auto var = dyn_cast<VarDecl>(decl)) {
    return emitObjCGetterDescriptorParts(IGM, var,
                                         selectorRef, atEncoding, impl);
  }
  llvm_unreachable("unknown storage!");
}

/// Emit the components of an Objective-C method descriptor for a
/// property getter method.
void irgen::emitObjCSetterDescriptorParts(IRGenModule &IGM,
                                          VarDecl *property,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  assert(property->isSettable(property->getDeclContext()) &&
         "not a settable property?!");

  Selector setterSel(property, Selector::ForSetter);
  selectorRef = IGM.getAddrOfObjCMethodName(setterSel.str());
  
  auto &clangASTContext = IGM.getClangASTContext();
  std::string TypeStr;
  auto clangType = clangASTContext.VoidTy;
  clangASTContext.getObjCEncodingForType(clangType, TypeStr);
  
  Size PtrSize = IGM.getPointerSize();
  Size::int_type ParmOffset = 2 * PtrSize.getValue();

  Type ArgType = property->getType()->getReferenceStorageReferent();
  clangType = IGM.getClangType(ArgType->getCanonicalType());
  if (clangType.isNull()) {
    atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    return;
  }
  clang::CharUnits sz = clangASTContext.getObjCEncodingTypeSize(clangType);
  if (!sz.isZero())
    ParmOffset += sz.getQuantity();
  TypeStr += llvm::itostr(ParmOffset);
  TypeStr += "@0:";
  TypeStr += llvm::itostr(PtrSize.getValue());
  ParmOffset = 2 * PtrSize.getValue();
  clangASTContext.getObjCEncodingForType(clangType, TypeStr);
  TypeStr += llvm::itostr(ParmOffset);
  atEncoding = IGM.getAddrOfGlobalString(TypeStr.c_str());

  impl = getObjCSetterPointer(IGM, property);
}

/// Emit the components of an Objective-C method descriptor for a
/// subscript getter method.
void irgen::emitObjCSetterDescriptorParts(IRGenModule &IGM,
                                          SubscriptDecl *subscript,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  assert(subscript->isSettable() && "not a settable subscript?!");

  Selector setterSel(subscript, Selector::ForSetter);
  selectorRef = IGM.getAddrOfObjCMethodName(setterSel.str());
  atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  impl = getObjCSetterPointer(IGM, subscript);
}

void irgen::emitObjCSetterDescriptorParts(IRGenModule &IGM,
                                          AbstractStorageDecl *decl,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  if (auto sub = dyn_cast<SubscriptDecl>(decl)) {
    return emitObjCSetterDescriptorParts(IGM, sub,
                                         selectorRef, atEncoding, impl);
  }
  if (auto var = dyn_cast<VarDecl>(decl)) {
    return emitObjCSetterDescriptorParts(IGM, var,
                                         selectorRef, atEncoding, impl);
  }
  llvm_unreachable("unknown storage!");
}

/// Emit an Objective-C method descriptor for the given method.
/// struct method_t {
///   SEL name;
///   const char *types;
///   IMP imp;
/// };
llvm::Constant *irgen::emitObjCMethodDescriptor(IRGenModule &IGM,
                                                AbstractFunctionDecl *method) {
  llvm::Constant *selectorRef, *atEncoding, *impl;
  emitObjCMethodDescriptorParts(IGM, method,
                                /*extended*/ false,
                                /*concrete*/ true,
                                selectorRef, atEncoding, impl);
  
  llvm::Constant *fields[] = { selectorRef, atEncoding, impl };
  return llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), fields);
}

Optional<llvm::Constant*> 
irgen::emitObjCIVarInitDestroyDescriptor(IRGenModule &IGM, ClassDecl *cd,
                                         bool isDestroyer) {
  // Check whether we have an implementation.
  Optional<llvm::Function*> objcImpl 
    = IGM.getAddrOfObjCIVarInitDestroy(cd, isDestroyer, NotForDefinition);
  if (!objcImpl)
    return None;

  /// The first element is the selector.
  SILDeclRef declRef = SILDeclRef(cd, 
                                  isDestroyer? SILDeclRef::Kind::IVarDestroyer
                                             : SILDeclRef::Kind::IVarInitializer,
                                  ResilienceExpansion::Minimal,
                                  1, 
                                  /*foreign*/ true);
  Selector selector(declRef);
  llvm::Constant *selectorRef = IGM.getAddrOfObjCMethodName(selector.str());
  
  /// The second element is the type @encoding, which is always "@?"
  /// for a function type.
  llvm::Constant *atEncoding = IGM.getAddrOfGlobalString("@?");

  /// The third element is the method implementation pointer.
  llvm::Constant *impl = llvm::ConstantExpr::getBitCast(*objcImpl,
                                                        IGM.Int8PtrTy);

  // Form the method_t instance.
  llvm::Constant *fields[] = { selectorRef, atEncoding, impl };
  return llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), fields);
}

llvm::Constant *
irgen::getMethodTypeExtendedEncoding(IRGenModule &IGM,
                                     AbstractFunctionDecl *method) {
  CanAnyFunctionType methodType
    = cast<AnyFunctionType>(method->getType()->getCanonicalType());
  if (!isa<DestructorDecl>(method)) {
    // Account for the 'self' pointer being curried.
    methodType = cast<AnyFunctionType>(methodType.getResult());
  }
  return GetObjCEncodingForMethodType(IGM, methodType, true/*Extended*/);
}

llvm::Constant *
irgen::getBlockTypeExtendedEncoding(IRGenModule &IGM,
                                    CanSILFunctionType invokeTy) {
  CanType resultType = invokeTy->getResult().getType();
  SmallVector<CanType, 4> paramTypes;
  
  // Skip the storage pointer, which is encoded as '@?' to avoid the infinite
  // recursion of the usual '@?<...>' rule for blocks.
  for (auto param : invokeTy->getParameters().slice(1)) {
    assert(!param.isIndirect()
           && "indirect C arguments not supported");
    paramTypes.push_back(param.getType());
  }
  
  return GetObjCEncodingForTypes(IGM, resultType, paramTypes,
                                 "@?0", IGM.getPointerSize().getValue(),
                                 /*extended*/ true);
}

/// Emit Objective-C method descriptors for the property accessors of the given
/// property. Returns a pair of Constants consisting of the getter and setter
/// function pointers, in that order. The setter llvm::Constant* will be null if
/// the property is not settable.
std::pair<llvm::Constant *, llvm::Constant *>
irgen::emitObjCPropertyMethodDescriptors(IRGenModule &IGM,
                                         VarDecl *property) {
  llvm::Constant *selectorRef, *atEncoding, *impl;
  emitObjCGetterDescriptorParts(IGM, property,
                                selectorRef, atEncoding, impl);
  
  llvm::Constant *getterFields[] = {selectorRef, atEncoding, impl};
  llvm::Constant *getter = llvm::ConstantStruct::getAnon(IGM.getLLVMContext(),
                                                         getterFields);
  llvm::Constant *setter = nullptr;
  
  if (property->isSettable(property->getDeclContext())) {
    emitObjCSetterDescriptorParts(IGM, property,
                                  selectorRef, atEncoding, impl);
    
    llvm::Constant *setterFields[] = {selectorRef, atEncoding, impl};
    setter = llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), setterFields);
  }
  
  return {getter, setter};
}

std::pair<llvm::Constant *, llvm::Constant *>
irgen::emitObjCSubscriptMethodDescriptors(IRGenModule &IGM,
                                          SubscriptDecl *subscript) {
  llvm::Constant *selectorRef, *atEncoding, *impl;
  emitObjCGetterDescriptorParts(IGM, subscript,
                                selectorRef, atEncoding, impl);
  
  llvm::Constant *getterFields[] = {selectorRef, atEncoding, impl};
  llvm::Constant *getter = llvm::ConstantStruct::getAnon(IGM.getLLVMContext(),
                                                         getterFields);
  llvm::Constant *setter = nullptr;
  
  if (subscript->isSettable()) {
    emitObjCSetterDescriptorParts(IGM, subscript,
                                  selectorRef, atEncoding, impl);
    
    llvm::Constant *setterFields[] = {selectorRef, atEncoding, impl};
    setter = llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), setterFields);
  }
  
  return {getter, setter};
}

bool irgen::requiresObjCMethodDescriptor(FuncDecl *method) {
  // Property accessors should be generated alongside the property.
  if (method->isAccessor())
    return false;
    
    // We don't export generic methods or subclasses to IRGen yet.
  if (method->getGenericParamsOfContext())
    return false;
  
  if (method->isObjC() || method->getAttrs().hasAttribute<IBActionAttr>())
    return true;
  if (auto override = method->getOverriddenDecl())
    return requiresObjCMethodDescriptor(override);
  return false;
}

bool irgen::requiresObjCMethodDescriptor(ConstructorDecl *constructor) {
  // We don't export generic methods or subclasses to IRGen yet.
  // FIXME: Total hack. Sema should filter these out.
  if (constructor->getGenericParamsOfContext())
    return false;

  return constructor->isObjC();
}

bool irgen::requiresObjCPropertyDescriptor(IRGenModule &IGM,
                                           VarDecl *property) {
  // We don't export generic methods or subclasses to IRGen yet.
  if (property->getDeclContext()->getDeclaredTypeInContext()
          ->is<BoundGenericType>())
    return false;

  if (auto override = property->getOverriddenDecl())
    return requiresObjCPropertyDescriptor(IGM, override);

  if (!property->isObjC())
    return false;
  
  // Don't expose objc properties for function types we can't bridge.
  if (auto ft = property->getType()->getAs<AnyFunctionType>())
    switch (ft->getRepresentation()) {
    case FunctionType::Representation::Thin:
      // We can't bridge thin types at all.
      return false;
    case FunctionType::Representation::Thick:
    case FunctionType::Representation::Block:
      return true;
    }
  
  return true;
}

bool irgen::requiresObjCSubscriptDescriptor(IRGenModule &IGM,
                                            SubscriptDecl *subscript) {
  // We don't export generic methods or subclasses to IRGen yet.
  if (subscript->getDeclContext()->getDeclaredTypeInContext()
          ->is<BoundGenericType>())
    return false;

  if (auto override = subscript->getOverriddenDecl())
    return requiresObjCSubscriptDescriptor(IGM, override);

  if (!subscript->isObjC())
    return false;
  
  // Don't expose objc properties for function types we can't bridge.
  if (auto ft = subscript->getElementType()->getAs<AnyFunctionType>())
    switch (ft->getRepresentation()) {
    case FunctionType::Representation::Thin:
      // We can't bridge thin types at all.
      return false;
    case FunctionType::Representation::Thick:
    case FunctionType::Representation::Block:
      return true;
    }
  
  return true;
}

llvm::Value *IRGenFunction::emitBlockCopyCall(llvm::Value *value) {
  // Get an appropriately-cast function pointer.
  auto fn = IGM.getBlockCopyFn();
  if (value->getType() != IGM.ObjCBlockPtrTy) {
    auto fnTy = llvm::FunctionType::get(value->getType(), value->getType(),
                                        false)->getPointerTo();
    fn = llvm::ConstantExpr::getBitCast(fn, fnTy);
  }
  
  auto call = Builder.CreateCall(fn, value);
  return call;
}

void IRGenFunction::emitBlockRelease(llvm::Value *value) {
  // Get an appropriately-cast function pointer.
  auto fn = IGM.getBlockReleaseFn();
  if (value->getType() != IGM.ObjCBlockPtrTy) {
    auto fnTy = llvm::FunctionType::get(IGM.VoidTy, value->getType(),
                                        false)->getPointerTo();
    fn = llvm::ConstantExpr::getBitCast(fn, fnTy);
  }
  auto call = Builder.CreateCall(fn, value);
  call->setDoesNotThrow();
}
