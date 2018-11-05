//===--- GenObjC.cpp - Objective-C interaction ----------------------------===//
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
#include "clang/Basic/CharInfo.h"
#include "clang/CodeGen/CGFunctionInfo.h"

#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILModule.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"

#include "CallEmission.h"
#include "ConstantBuilder.h"
#include "Explosion.h"
#include "GenCall.h"
#include "GenClass.h"
#include "GenFunc.h"
#include "GenHeap.h"
#include "GenProto.h"
#include "GenType.h"
#include "HeapTypeInfo.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "MetadataRequest.h"
#include "NativeConventionSchema.h"
#include "ScalarTypeInfo.h"
#include "StructLayout.h"

#include "GenObjC.h"

using namespace swift;
using namespace irgen;

void IRGenFunction::emitObjCStrongRelease(llvm::Value *value) {
  // Get an appropriately-cast function pointer.
  auto fn = IGM.getObjCReleaseFn();
  auto cc = IGM.C_CC;
  if (auto fun = dyn_cast<llvm::Function>(fn))
    cc = fun->getCallingConv();

  if (value->getType() != IGM.ObjCPtrTy) {
    auto fnTy = llvm::FunctionType::get(IGM.VoidTy, value->getType(),
                                        false)->getPointerTo();
    fn = llvm::ConstantExpr::getBitCast(fn, fnTy);
  }

  auto call = Builder.CreateCall(fn, value);
  call->setCallingConv(cc);
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

void IRGenFunction::emitObjCStrongRetain(llvm::Value *v) {
  emitObjCRetainCall(v);
}

llvm::Value *IRGenFunction::emitObjCRetainCall(llvm::Value *value) {
  // Get an appropriately cast function pointer.
  auto fn = IGM.getObjCRetainFn();
  auto cc = IGM.C_CC;
  if (auto fun = dyn_cast<llvm::Function>(fn))
    cc = fun->getCallingConv();
  fn = getCastOfRetainFn(IGM, fn, value->getType());

  auto call = Builder.CreateCall(fn, value);
  call->setCallingConv(cc);
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

llvm::InlineAsm *IRGenModule::getObjCRetainAutoreleasedReturnValueMarker() {
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
  if (IRGen.Opts.shouldOptimize()) {
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
    IGF.Builder.CreateAsmCall(marker, {});
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

  const llvm::Triple &triple = IGF.IGM.Context.LangOpts.Target;
  if (triple.getArch() == llvm::Triple::x86_64) {
    // Don't tail call objc_retainAutoreleasedReturnValue. This blocks the
    // autoreleased return optimization.
    // callq  0x01ec08 ; symbol stub for: objc_msgSend
    // movq   %rax, %rdi
    // popq   %rbp  ;<== Blocks the handshake from objc_autoreleaseReturnValue
    // jmp    0x01ec20 ; symbol stub for: objc_retainAutoreleasedReturnValue
    call->setTailCallKind(llvm::CallInst::TCK_NoTail);
  }

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
  /// A type-info implementation suitable for Builtin.UnknownObject.
  class UnknownTypeInfo : public HeapTypeInfo<UnknownTypeInfo> {
  public:
    UnknownTypeInfo(llvm::PointerType *storageType, Size size,
                 SpareBitVector spareBits, Alignment align)
      : HeapTypeInfo(storageType, size, spareBits, align) {
    }

    /// Builtin.UnknownObject requires ObjC reference-counting.
    ReferenceCounting getReferenceCounting() const {
      return ReferenceCounting::Unknown;
    }
  };
} // end anonymous namespace

const LoadableTypeInfo *TypeConverter::convertBuiltinUnknownObject() {
  // UnknownObject is only interestingly different from NativeObject on
  // platforms with ObjC interop.
  if (IGM.Context.LangOpts.EnableObjCInterop) {
    return new UnknownTypeInfo(IGM.ObjCPtrTy, IGM.getPointerSize(),
                               IGM.getHeapObjectSpareBits(),
                               IGM.getPointerAlignment());
  }
  
  // Without ObjC interop, UnknownObject handles just like a NativeObject.
  return convertBuiltinNativeObject();
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
    APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
                                       unsigned bits,
                                       unsigned index) const override {
      return APInt(bits, 0);
    }
    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                         SILType T,
                                         bool isOutlined) const override {
      src = IGF.Builder.CreateBitCast(src, IGF.IGM.SizeTy->getPointerTo());
      auto val = IGF.Builder.CreateLoad(src);
      auto isNonzero = IGF.Builder.CreateICmpNE(val,
                                    llvm::ConstantInt::get(IGF.IGM.SizeTy, 0));
      // We either have extra inhabitant 0 or no extra inhabitant (-1).
      // Conveniently, this is just a sext i1 -> i32 away.
      return IGF.Builder.CreateSExt(isNonzero, IGF.IGM.Int32Ty);
    }
    void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                              Address dest, SILType T, bool isOutlined)
    const override {
      // There's only one extra inhabitant, 0.
      dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.SizeTy->getPointerTo());
      IGF.Builder.CreateStore(llvm::ConstantInt::get(IGF.IGM.SizeTy, 0), dest);
    }
  };
} // end anonymous namespace


const LoadableTypeInfo *TypeConverter::convertBuiltinBridgeObject() {
  return new BridgeObjectTypeInfo(IGM.BridgeObjectPtrTy, IGM.getPointerSize(),
      SpareBitVector::getConstant(IGM.getPointerSize().getValueInBits(), false),
                                  IGM.getPointerAlignment());
}

const TypeInfo &IRGenModule::getObjCClassPtrTypeInfo() {
  return Types.getObjCClassPtrTypeInfo();
}

const TypeInfo &TypeConverter::getObjCClassPtrTypeInfo() {
  // ObjC class pointers look like unmanaged (untagged) object references.
  if (ObjCClassPtrTI) return *ObjCClassPtrTI;
  ObjCClassPtrTI =
    createUnmanagedStorageType(IGM.ObjCClassPtrTy, ReferenceCounting::ObjC,
                               /*isOptional*/false);
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
  auto global = new llvm::GlobalVariable(Module, init->getType(), false,
                                         llvm::GlobalValue::PrivateLinkage,
                                         init,
                          llvm::Twine("\01L_selector_data(") + selector + ")");
  SetCStringLiteralSection(global, ObjCLabelType::MethodVarName);
  global->setAlignment(1);
  addCompilerUsedGlobal(global);

  // Drill down to make an i8*.
  auto zero = llvm::ConstantInt::get(SizeTy, 0);
  llvm::Constant *indices[] = { zero, zero };
  auto address = llvm::ConstantExpr::getInBoundsGetElementPtr(
      init->getType(), global, indices);

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
                                         llvm::GlobalValue::PrivateLinkage,
                                         init,
                                llvm::Twine("\01L_selector(") + selector + ")");
  global->setExternallyInitialized(true);
  global->setAlignment(getPointerAlignment().getValue());

  // This section name is magical for the Darwin static and dynamic linkers.
  global->setSection(GetObjCSectionName("__objc_selrefs",
                                        "literal_pointers,no_dead_strip"));

  // Make sure that this reference does not get optimized away.
  addCompilerUsedGlobal(global);

  // Cache and return.
  entry = global;
  return global;
}

/// Get or create an ObjC protocol record. Always returns an i8*. We lazily
/// create ObjC protocol_t records for protocols, storing references to the
/// record into the __objc_protolist and __objc_protorefs sections to be
/// fixed up by the runtime.
///
/// It is not correct to use this value as a Protocol* reference directly. The
/// ObjC runtime requires protocol references to be loaded from an
/// indirect variable, the address of which is given by
/// getAddrOfObjCProtocolRef.
llvm::Constant *
IRGenModule::getAddrOfObjCProtocolRecord(ProtocolDecl *proto,
                                         ForDefinition_t forDefinition) {
  return const_cast<llvm::Constant*>
    (cast<llvm::Constant>(getObjCProtocolGlobalVars(proto).record));
}

/// Get or create an ObjC protocol reference. Always returns an i8**. We lazily
/// create ObjC protocol_t records for protocols, storing references to the
/// record into the __objc_protolist and __objc_protorefs sections to be
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
  
  // Create a placeholder protocol record.
  llvm::Constant *protocolRecord =
    new llvm::GlobalVariable(Module, Int8Ty, /*constant*/ false,
                             llvm::GlobalValue::PrivateLinkage, nullptr);
  LazyObjCProtocolDefinitions.push_back(proto);

  // Introduce a variable to label the protocol.
  llvm::SmallString<64> nameBuffer;
  StringRef protocolName = proto->getObjCRuntimeName(nameBuffer);
  auto *protocolLabel
    = new llvm::GlobalVariable(Module, Int8PtrTy,
                               /*constant*/ false,
                               llvm::GlobalValue::WeakAnyLinkage,
                               protocolRecord,
                               llvm::Twine("\01l_OBJC_LABEL_PROTOCOL_$_")
                                 + protocolName);
  protocolLabel->setAlignment(getPointerAlignment().getValue());
  protocolLabel->setVisibility(llvm::GlobalValue::HiddenVisibility);
  protocolLabel->setSection(GetObjCSectionName("__objc_protolist",
                                               "coalesced,no_dead_strip"));

  // Introduce a variable to reference the protocol.
  auto *protocolRef =
      new llvm::GlobalVariable(Module, Int8PtrTy, /*constant*/ false,
                               llvm::GlobalValue::WeakAnyLinkage,
                               protocolRecord,
                               llvm::Twine("\01l_OBJC_PROTOCOL_REFERENCE_$_") + protocolName);
  protocolRef->setAlignment(getPointerAlignment().getValue());
  protocolRef->setVisibility(llvm::GlobalValue::HiddenVisibility);
  protocolRef->setSection(GetObjCSectionName("__objc_protorefs",
                                             "coalesced,no_dead_strip"));

  ObjCProtocolPair pair{protocolRecord, protocolRef};
  ObjCProtocols.insert({proto, pair});
  
  return pair;
}

void IRGenModule::emitLazyObjCProtocolDefinition(ProtocolDecl *proto) {
  // Emit the real definition.
  auto record = cast<llvm::GlobalVariable>(emitObjCProtocolData(*this, proto));

  // Find the placeholder.  It should always still be a placeholder,
  // because it was created as an anonymous symbol and nobody should
  // ever be randomly messing with those.
  auto placeholder =
    cast<llvm::GlobalVariable>(ObjCProtocols.find(proto)->second.record);

  // Move the new record to the placeholder's position.
  Module.getGlobalList().remove(record);
  Module.getGlobalList().insertAfter(placeholder->getIterator(), record);

  // Replace and destroy the placeholder.
  placeholder->replaceAllUsesWith(
                            llvm::ConstantExpr::getBitCast(record, Int8PtrTy));
  placeholder->eraseFromParent();
}

void IRGenModule::emitLazyObjCProtocolDefinitions() {
  // Emit any lazy ObjC protocol definitions we require.  Try to do
  // this in the order in which we needed them, since they can require
  // other protocol definitions recursively.
  for (size_t i = 0; i != LazyObjCProtocolDefinitions.size(); ++i) {
    ProtocolDecl *protocol = LazyObjCProtocolDefinitions[i];
    emitLazyObjCProtocolDefinition(protocol);
  }
}

namespace {
  class Selector {
    
    llvm::SmallString<80> Buffer;
    StringRef Text;

  public:

    static constexpr struct ForGetter_t { } ForGetter{};
    static constexpr struct ForSetter_t { } ForSetter{};
    
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
      case SILDeclRef::Kind::StoredPropertyInitializer:
      case SILDeclRef::Kind::EnumElement:
      case SILDeclRef::Kind::GlobalAccessor:
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
  };
} // end anonymous namespace

llvm::Constant *IRGenModule::getAddrOfObjCSelectorRef(SILDeclRef method) {
  assert(method.isForeign);
  return getAddrOfObjCSelectorRef(Selector(method).str());
}

static llvm::Value *emitSuperArgument(IRGenFunction &IGF,
                                      bool isInstanceMethod,
                                      llvm::Value *selfValue,
                                      CanType searchClass) {
  // Allocate an objc_super struct.
  Address super = IGF.createAlloca(IGF.IGM.ObjCSuperStructTy,
                                   IGF.IGM.getPointerAlignment(),
                                   "objc_super");
  // TODO: Track lifetime markers for function args.
  llvm::Value *self = IGF.Builder.CreateBitCast(selfValue,
                                                IGF.IGM.ObjCPtrTy);
  
  // Generate the search class object reference.
  llvm::Value *searchValue;
  if (isInstanceMethod) {
    searchValue = emitClassHeapMetadataRef(IGF, searchClass,
                                           MetadataValueType::ObjCClass,
                                           MetadataState::Complete,
                                           /*allow uninitialized*/ true);
  } else {
    searchClass = cast<MetatypeType>(searchClass).getInstanceType();
    ClassDecl *searchClassDecl = searchClass.getClassOrBoundGenericClass();
    if (doesClassMetadataRequireUpdate(IGF.IGM, searchClassDecl)) {
      searchValue = emitClassHeapMetadataRef(IGF, searchClass,
                                             MetadataValueType::ObjCClass,
                                             MetadataState::Complete,
                                             /*allow uninitialized*/ true);
      searchValue = emitLoadOfObjCHeapMetadataRef(IGF, searchValue);
      searchValue = IGF.Builder.CreateBitCast(searchValue, IGF.IGM.ObjCClassPtrTy);
    } else {
      searchValue = IGF.IGM.getAddrOfMetaclassObject(searchClassDecl,
                                                     NotForDefinition);
    }
  }
  
  // Store the receiver and class to the struct.
  Address selfAddr = IGF.Builder.CreateStructGEP(super, 0, Size(0));
  IGF.Builder.CreateStore(self, selfAddr);

  Address searchAddr =
    IGF.Builder.CreateStructGEP(super, 1, IGF.IGM.getPointerSize());
  IGF.Builder.CreateStore(searchValue, searchAddr);
  
  // Pass a pointer to the objc_super struct to the messenger.
  // Project the ownership semantics of 'self' to the super argument.
  return super.getAddress();
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

Callee irgen::getObjCMethodCallee(IRGenFunction &IGF,
                                  const ObjCMethod &methodInfo,
                                  llvm::Value *selfValue,
                                  CalleeInfo &&info) {
  SILDeclRef method = methodInfo.getMethod();
  assert((method.kind == SILDeclRef::Kind::Initializer
          || method.kind == SILDeclRef::Kind::Allocator
          || method.kind == SILDeclRef::Kind::Func
          || method.kind == SILDeclRef::Kind::Destroyer
          || method.kind == SILDeclRef::Kind::Deallocator) &&
         "objc method call must be to a func/initializer/getter/setter/dtor");

  auto kind = methodInfo.getMessageKind();

  Signature sig = IGF.IGM.getSignature(info.OrigFnType);
  bool indirectResult =
    sig.getForeignInfo().ClangInfo->getReturnInfo().isIndirect();
  if (kind != ObjCMessageKind::Normal) {
    sig.setType(getMsgSendSuperTy(IGF.IGM, sig.getType(), indirectResult));
  }

  // Create the appropriate messenger function.
  // FIXME: this needs to be target-specific.  Ask Clang for it!
  llvm::Constant *messenger = [&]() -> llvm::Constant* {
    if (indirectResult && IGF.IGM.TargetInfo.ObjCUseStret) {
      switch (kind) {
      case ObjCMessageKind::Normal:
        return IGF.IGM.getObjCMsgSendStretFn();

      case ObjCMessageKind::Peer:
        return IGF.IGM.getObjCMsgSendSuperStretFn();

      case ObjCMessageKind::Super:
        return IGF.IGM.getObjCMsgSendSuperStret2Fn();
      }
      llvm_unreachable("unhandled kind");
    } else {
      switch (kind) {
      case ObjCMessageKind::Normal:
        return IGF.IGM.getObjCMsgSendFn();

      case ObjCMessageKind::Peer:
        return IGF.IGM.getObjCMsgSendSuperFn();

      case ObjCMessageKind::Super:
        return IGF.IGM.getObjCMsgSendSuper2Fn();
      }
      llvm_unreachable("unhandled kind");
    }
  }();

  messenger = llvm::ConstantExpr::getBitCast(messenger,
                                             sig.getType()->getPointerTo());

  // super.constructor references an instance method (even though the
  // decl is really a 'static' member). Similarly, destructors refer
  // to the instance method -dealloc.
  bool isInstanceMethod
    = method.kind == SILDeclRef::Kind::Initializer
      || method.kind == SILDeclRef::Kind::Deallocator
      || method.getDecl()->isInstanceMember();

  llvm::Value *receiverValue;
  if (auto searchType = methodInfo.getSearchType()) {
    receiverValue =
      emitSuperArgument(IGF, isInstanceMethod, selfValue,
                        searchType.getASTType());
  } else {
    receiverValue = selfValue;
  }

  // Compute the selector.
  Selector selector(method);
  llvm::Value *selectorValue = IGF.emitObjCSelectorRefLoad(selector.str());

  auto fn = FunctionPointer::forDirect(messenger, sig);
  return Callee(std::move(info), fn, receiverValue, selectorValue);
}

/// Call [self allocWithZone: nil].
llvm::Value *irgen::emitObjCAllocObjectCall(IRGenFunction &IGF,
                                            llvm::Value *self,
                                            SILType selfType) {
  // Get an appropriately-cast function pointer.
  auto fn = IGF.IGM.getObjCAllocWithZoneFn();

  if (self->getType() != IGF.IGM.ObjCClassPtrTy) {
    auto fnTy = llvm::FunctionType::get(self->getType(), self->getType(),
                                        false)->getPointerTo();
    fn = llvm::ConstantExpr::getBitCast(fn, fnTy);
  }
  
  auto call = IGF.Builder.CreateCall(fn, self);

  // Cast the returned pointer to the right type.
  auto &classTI = IGF.getTypeInfo(selfType);
  llvm::Type *destType = classTI.getStorageType();
  return IGF.Builder.CreateBitCast(call, destType);
}

static llvm::Function *emitObjCPartialApplicationForwarder(IRGenModule &IGM,
                                                           ObjCMethod method,
                                            CanSILFunctionType origMethodType,
                                            CanSILFunctionType resultType,
                                            const HeapLayout &layout,
                                            SILType selfType) {
  auto &selfTI = IGM.getTypeInfo(selfType);
 
  assert(resultType->getRepresentation()
           == SILFunctionType::Representation::Thick);

  llvm::AttributeList attrs;
  llvm::FunctionType *fwdTy = IGM.getFunctionType(resultType, attrs);
  // FIXME: Give the thunk a real name.
  // FIXME: Maybe cache the thunk by function and closure types?
  llvm::Function *fwd =
    llvm::Function::Create(fwdTy, llvm::Function::InternalLinkage,
                           MANGLE_AS_STRING(OBJC_PARTIAL_APPLY_THUNK_SYM),
                           &IGM.Module);
  fwd->setCallingConv(
      expandCallingConv(IGM, SILFunctionTypeRepresentation::Thick));

  fwd->setAttributes(attrs);
  // Merge initial attributes with attrs.
  llvm::AttrBuilder b;
  IGM.constructInitialFnAttributes(b);
  fwd->addAttributes(llvm::AttributeList::FunctionIndex, b);
  
  IRGenFunction subIGF(IGM, fwd);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(subIGF, fwd);
  
  // Do we need to lifetime-extend self?
  bool lifetimeExtendsSelf;
  auto results = origMethodType->getResults();
  if (results.size() == 1) {
    switch (results[0].getConvention()) {
    case ResultConvention::UnownedInnerPointer:
      lifetimeExtendsSelf = true;
      break;

    case ResultConvention::Indirect:
    case ResultConvention::Unowned:
    case ResultConvention::Owned:
    case ResultConvention::Autoreleased:
      lifetimeExtendsSelf = false;
      break;
    }
  } else {
    lifetimeExtendsSelf = false;
  }
  
  // Do we need to retain self before calling, and/or release it after?
  bool retainsSelf;
  switch (origMethodType->getParameters().back().getConvention()) {
  case ParameterConvention::Direct_Unowned:
    retainsSelf = false;
    break;
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Owned:
    retainsSelf = true;
    break;
  case ParameterConvention::Indirect_In_Guaranteed:
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Constant:
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    llvm_unreachable("self passed indirectly?!");
  }
  
  // Recover 'self' from the context.
  Explosion params = subIGF.collectParameters();
  llvm::Value *context = params.takeLast();
  Address dataAddr = layout.emitCastTo(subIGF, context);
  auto &fieldLayout = layout.getElement(0);
  Address selfAddr = fieldLayout.project(subIGF, dataAddr, None);
  Explosion selfParams;
  if (retainsSelf)
    cast<LoadableTypeInfo>(selfTI).loadAsCopy(subIGF, selfAddr, selfParams);
  else
    cast<LoadableTypeInfo>(selfTI).loadAsTake(subIGF, selfAddr, selfParams);
  llvm::Value *self = selfParams.claimNext();
  
  // Save off the forwarded indirect return address if we have one.
  llvm::Value *formalIndirectResult = nullptr;
  llvm::Value *indirectedDirectResult = nullptr;
  const LoadableTypeInfo *indirectedResultTI = nullptr;
  if (origMethodType->hasIndirectFormalResults()) {
    // We should never import an ObjC method as returning a tuple which
    // would get broken up into multiple results like this.
    assert(origMethodType->getNumIndirectFormalResults() == 1);
    formalIndirectResult = params.claimNext();
  } else {
    SILType appliedResultTy = origMethodType->getDirectFormalResultsType();
    indirectedResultTI =
      &cast<LoadableTypeInfo>(IGM.getTypeInfo(appliedResultTy));
    auto &nativeSchema = indirectedResultTI->nativeReturnValueSchema(IGM);
    if (nativeSchema.requiresIndirect()) {
      indirectedDirectResult = params.claimNext();
    }
  }

  // Translate direct parameters passed indirectly.
  Explosion translatedParams;

  // Add the formal indirect return here.
  if (formalIndirectResult)
    translatedParams.add(formalIndirectResult);

  // We already handled self.
  assert(origMethodType->hasSelfParam());
  auto origParamInfos = origMethodType->getParameters();
  origParamInfos = origParamInfos.drop_back();

  for (auto info : origParamInfos) {
    // Addresses consist of a single pointer argument.
    if (isIndirectFormalParameter(info.getConvention())) {
      translatedParams.add(params.claimNext());
      continue;
    }
    // Otherwise, we have a loadable type that can either be passed directly or
    // indirectly.
    assert(info.getSILStorageType().isObject());
    auto curSILType = info.getSILStorageType();
    auto &ti = cast<LoadableTypeInfo>(IGM.getTypeInfo(curSILType));

    // Load the indirectly passed parameter.
    auto &nativeSchema = ti.nativeParameterValueSchema(IGM);
    if (nativeSchema.requiresIndirect()) {
      Address paramAddr = ti.getAddressForPointer(params.claimNext());
      ti.loadAsTake(subIGF, paramAddr, translatedParams);
      continue;
    }
    // Map from the native calling convention into the explosion schema.
    auto &nativeParamSchema = ti.nativeParameterValueSchema(IGM);
    Explosion nativeParam;
    params.transferInto(nativeParam, nativeParamSchema.size());
    Explosion nonNativeParam = nativeParamSchema.mapFromNative(
        subIGF.IGM, subIGF, nativeParam, curSILType);
    assert(nativeParam.empty());

    // Pass along the value.
    ti.reexplode(subIGF, nonNativeParam, translatedParams);
  }

  // Prepare the call to the underlying method.
  CallEmission emission(subIGF,
                        getObjCMethodCallee(subIGF, method, self,
                          CalleeInfo(origMethodType, origMethodType, {})));

  emission.setArgs(translatedParams, false);

  // Cleanup that always has to occur after the function call.
  auto cleanup = [&]{
    // Lifetime-extend 'self' by sending it to the autorelease pool if need be.
    if (lifetimeExtendsSelf) {
      subIGF.emitObjCRetainCall(self);
      subIGF.emitObjCAutoreleaseCall(self);
    }
    // Release the context.
    if (!resultType->isCalleeGuaranteed())
      subIGF.emitNativeStrongRelease(context, subIGF.getDefaultAtomicity());
  };
  
   // Emit the call and produce the return value.
  if (indirectedDirectResult) {
    Address addr =
      indirectedResultTI->getAddressForPointer(indirectedDirectResult);
    emission.emitToMemory(addr, *indirectedResultTI, false);
    cleanup();
    subIGF.Builder.CreateRetVoid();
  } else {
    Explosion result;
    emission.emitToExplosion(result, false);
    cleanup();
    auto &callee = emission.getCallee();
    auto resultType =
        callee.getOrigFunctionType()->getDirectFormalResultsType();
    subIGF.emitScalarReturn(resultType, result, true /*isSwiftCCReturn*/,
                            false);
  }
  
  return fwd;
}

void irgen::emitObjCPartialApplication(IRGenFunction &IGF,
                                       ObjCMethod method,
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

  // FIXME: Either emit a descriptor for this or create a metadata kind
  // that indicates its trivial layout.
  auto Descriptor
    = llvm::ConstantPointerNull::get(IGF.IGM.CaptureDescriptorPtrTy);
  llvm::Value *data = IGF.emitUnmanagedAlloc(layout, "closure",
                                             Descriptor);
  // FIXME: non-fixed offsets
  NonFixedOffsets offsets = None;
  Address dataAddr = layout.emitCastTo(IGF, data);
  auto &fieldLayout = layout.getElement(0);
  auto &fieldType = layout.getElementTypes()[0];
  Address fieldAddr = fieldLayout.project(IGF, dataAddr, offsets);
  Explosion selfParams;
  selfParams.add(self);
  fieldLayout.getType().initializeFromParams(IGF, selfParams, fieldAddr,
                                             fieldType, false);

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
static llvm::Constant *findSwiftAsObjCThunk(IRGenModule &IGM, SILDeclRef ref,
                                            SILFunction *&SILFn) {
  SILFn = IGM.getSILModule().lookUpFunction(ref);
  assert(SILFn && "no IR function for swift-as-objc thunk");
  auto fn = IGM.getAddrOfSILFunction(SILFn, NotForDefinition);
  ApplyIRLinkage({llvm::GlobalValue::InternalLinkage,
                  llvm::GlobalValue::DefaultVisibility,
                  llvm::GlobalValue::DefaultStorageClass})
      .to(fn);
  fn->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  return llvm::ConstantExpr::getBitCast(fn, IGM.Int8PtrTy);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given property's getter method implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCGetterPointer(IRGenModule &IGM,
                                            AbstractStorageDecl *property,
                                            SILFunction *&silFn) {
  // Protocol properties have no impl.
  if (isa<ProtocolDecl>(property->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  SILDeclRef getter = SILDeclRef(property->getGetter(), SILDeclRef::Kind::Func)
    .asForeign();

  return findSwiftAsObjCThunk(IGM, getter, silFn);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given property's setter method implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCSetterPointer(IRGenModule &IGM,
                                            AbstractStorageDecl *property,
                                            SILFunction *&silFn) {
  // Protocol properties have no impl.
  if (isa<ProtocolDecl>(property->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  assert(property->isSettable(property->getDeclContext()) &&
         "property is not settable?!");
  
  SILDeclRef setter = SILDeclRef(property->getSetter(), SILDeclRef::Kind::Func)
    .asForeign();
  return findSwiftAsObjCThunk(IGM, setter, silFn);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given method implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCMethodPointer(IRGenModule &IGM,
                                            FuncDecl *method,
                                            SILFunction *&silFn) {
  // Protocol methods have no impl.
  if (isa<ProtocolDecl>(method->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  SILDeclRef declRef = SILDeclRef(method, SILDeclRef::Kind::Func)
    .asForeign();

  return findSwiftAsObjCThunk(IGM, declRef, silFn);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given constructor implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCMethodPointer(IRGenModule &IGM,
                                            ConstructorDecl *constructor,
                                            SILFunction *&silFn) {
  // Protocol methods have no impl.
  if (isa<ProtocolDecl>(constructor->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  SILDeclRef declRef = SILDeclRef(constructor, SILDeclRef::Kind::Initializer)
    .asForeign();

  return findSwiftAsObjCThunk(IGM, declRef, silFn);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given destructor implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCMethodPointer(IRGenModule &IGM,
                                            DestructorDecl *destructor,
                                            SILFunction *&silFn) {
  SILDeclRef declRef = SILDeclRef(destructor, SILDeclRef::Kind::Deallocator)
    .asForeign();

  return findSwiftAsObjCThunk(IGM, declRef, silFn);
}

static SILDeclRef getObjCMethodRef(AbstractFunctionDecl *method) {
  if (isa<ConstructorDecl>(method))
    return SILDeclRef(method, SILDeclRef::Kind::Initializer).asForeign();
  if (isa<DestructorDecl>(method))
    return SILDeclRef(method, SILDeclRef::Kind::Deallocator).asForeign();
  return SILDeclRef(method, SILDeclRef::Kind::Func).asForeign();
}

static CanSILFunctionType getObjCMethodType(IRGenModule &IGM,
                                            AbstractFunctionDecl *method) {  
  return IGM.getSILTypes().getConstantFunctionType(getObjCMethodRef(method));
}

static clang::CanQualType getObjCPropertyType(IRGenModule &IGM,
                                              VarDecl *property) {
  // Use the lowered return type of the foreign getter.
  auto getter = property->getGetter();
  assert(getter);
  CanSILFunctionType methodTy = getObjCMethodType(IGM, getter);
  return IGM.getClangType(
      methodTy->getFormalCSemanticResult().getASTType());
}

void irgen::getObjCEncodingForPropertyType(IRGenModule &IGM,
                                           VarDecl *property, std::string &s) {
  // FIXME: Property encoding differs in slight ways that aren't publicly
  // exposed from Clang.
  IGM.getClangASTContext()
    .getObjCEncodingForPropertyType(getObjCPropertyType(IGM, property), s);
}

static void
HelperGetObjCEncodingForType(const clang::ASTContext &Context,
                             clang::CanQualType T,
                             std::string &S, bool Extended) {
  
  Context.getObjCEncodingForMethodParameter(clang::Decl::OBJC_TQ_None,
                                            T, S, Extended);
}

static llvm::Constant *getObjCEncodingForTypes(IRGenModule &IGM,
                                               SILType resultType,
                                               ArrayRef<SILParameterInfo> params,
                                               StringRef fixedParamsString,
                                               Size::int_type parmOffset,
                                               bool useExtendedEncoding) {
  auto &clangASTContext = IGM.getClangASTContext();
  
  std::string encodingString;

  // Return type.
  {
    auto clangType = IGM.getClangType(resultType.getASTType());
    if (clangType.isNull())
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    HelperGetObjCEncodingForType(clangASTContext, clangType, encodingString,
                                 useExtendedEncoding);
  }

  // Parameter types.
  // TODO. Encode type qualifier, 'in', 'inout', etc. for the parameter.
  std::string paramsString;
  for (auto param : params) {
    auto clangType = IGM.getClangType(param.getType());
    if (clangType.isNull())
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    
    // TODO. Some stuff related to Array and Function type is missing.
    // TODO. Encode type qualifier, 'in', 'inout', etc. for the parameter.
    HelperGetObjCEncodingForType(clangASTContext, clangType, paramsString,
                                 useExtendedEncoding);
    paramsString += llvm::itostr(parmOffset);
    clang::CharUnits sz = clangASTContext.getObjCEncodingTypeSize(clangType);
    parmOffset += sz.getQuantity();
  }
  
  encodingString += llvm::itostr(parmOffset);
  encodingString += fixedParamsString;
  encodingString += paramsString;
  return IGM.getAddrOfGlobalString(encodingString);
}

static llvm::Constant *getObjCEncodingForMethodType(IRGenModule &IGM,
                                                    CanSILFunctionType fnType,
                                                    bool useExtendedEncoding) {
  SILType resultType = fnType->getFormalCSemanticResult();

  // Get the inputs without 'self'.
  auto inputs = fnType->getParameters().drop_back();

  // Include the encoding for 'self' and '_cmd'.
  llvm::SmallString<8> specialParams;
  specialParams += "@0:";
  auto ptrSize = IGM.getPointerSize().getValue();
  specialParams += llvm::itostr(ptrSize);
  GenericContextScope scope(IGM, fnType->getGenericSignature());
  return getObjCEncodingForTypes(IGM, resultType, inputs, specialParams,
                                 ptrSize * 2, useExtendedEncoding);
}

/// Emit the components of an Objective-C method descriptor: its selector,
/// type encoding, and IMP pointer.
SILFunction *irgen::emitObjCMethodDescriptorParts(IRGenModule &IGM,
                                                  AbstractFunctionDecl *method,
                                                  bool extendedEncoding,
                                                  bool concrete,
                                                  llvm::Constant *&selectorRef,
                                                  llvm::Constant *&atEncoding,
                                                  llvm::Constant *&impl) {
  Selector selector(method);
  
  /// The first element is the selector.
  selectorRef = IGM.getAddrOfObjCMethodName(selector.str());
  
  /// The second element is the method signature. A method signature is made of
  /// the return type @encoding and every parameter type @encoding, glued with
  /// numbers that used to represent stack offsets for each of these elements.
  CanSILFunctionType methodType = getObjCMethodType(IGM, method);
  atEncoding = getObjCEncodingForMethodType(IGM, methodType, extendedEncoding);
  
  /// The third element is the method implementation pointer.
  if (!concrete) {
    impl = nullptr;
    return nullptr;
  }
  SILFunction *silFn = nullptr;
  if (auto func = dyn_cast<FuncDecl>(method))
    impl = getObjCMethodPointer(IGM, func, silFn);
  else if (auto ctor = dyn_cast<ConstructorDecl>(method))
    impl = getObjCMethodPointer(IGM, ctor, silFn);
  else
    impl = getObjCMethodPointer(IGM, cast<DestructorDecl>(method), silFn);
  return silFn;
}

/// Emit the components of an Objective-C method descriptor for a
/// property getter method.
SILFunction *irgen::emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                                  VarDecl *property,
                                                  llvm::Constant *&selectorRef,
                                                  llvm::Constant *&atEncoding,
                                                  llvm::Constant *&impl) {
  Selector getterSel(property, Selector::ForGetter);
  selectorRef = IGM.getAddrOfObjCMethodName(getterSel.str());
  
  auto clangType = getObjCPropertyType(IGM, property);
  if (clangType.isNull()) {
    atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    return nullptr;
  }

  auto &clangASTContext = IGM.getClangASTContext();
  std::string TypeStr;
  clangASTContext.getObjCEncodingForType(clangType, TypeStr);
  
  Size PtrSize = IGM.getPointerSize();
  Size::int_type ParmOffset = 2 * PtrSize.getValue();
  
  TypeStr += llvm::itostr(ParmOffset);
  TypeStr += "@0:";
  TypeStr += llvm::itostr(PtrSize.getValue());
  atEncoding = IGM.getAddrOfGlobalString(TypeStr.c_str());
  SILFunction *silFn = nullptr;
  impl = getObjCGetterPointer(IGM, property, silFn);
  return silFn;
}

/// Emit the components of an Objective-C method descriptor for a
/// subscript getter method.
SILFunction *irgen::emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                                  SubscriptDecl *subscript,
                                                  llvm::Constant *&selectorRef,
                                                  llvm::Constant *&atEncoding,
                                                  llvm::Constant *&impl) {
  Selector getterSel(subscript, Selector::ForGetter);
  selectorRef = IGM.getAddrOfObjCMethodName(getterSel.str());
  atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  SILFunction *silFn = nullptr;
  impl = getObjCGetterPointer(IGM, subscript, silFn);
  return silFn;
}

SILFunction *irgen::emitObjCGetterDescriptorParts(IRGenModule &IGM,
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
  return nullptr;
}

/// Emit the components of an Objective-C method descriptor for a
/// property getter method.
SILFunction *irgen::emitObjCSetterDescriptorParts(IRGenModule &IGM,
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

  clangType = getObjCPropertyType(IGM, property);
  if (clangType.isNull()) {
    atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    return nullptr;
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
  SILFunction *silFn = nullptr;
  impl = getObjCSetterPointer(IGM, property, silFn);
  return silFn;
}

/// Emit the components of an Objective-C method descriptor for a
/// subscript getter method.
SILFunction *irgen::emitObjCSetterDescriptorParts(IRGenModule &IGM,
                                                  SubscriptDecl *subscript,
                                                  llvm::Constant *&selectorRef,
                                                  llvm::Constant *&atEncoding,
                                                  llvm::Constant *&impl) {
  assert(subscript->isSettable() && "not a settable subscript?!");

  Selector setterSel(subscript, Selector::ForSetter);
  selectorRef = IGM.getAddrOfObjCMethodName(setterSel.str());
  atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  SILFunction *silFn = nullptr;
  impl = getObjCSetterPointer(IGM, subscript, silFn);
  return silFn;
}

SILFunction *irgen::emitObjCSetterDescriptorParts(IRGenModule &IGM,
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
  return nullptr;
}

static void buildMethodDescriptor(ConstantArrayBuilder &descriptors,
                                  llvm::Constant *selectorRef,
                                  llvm::Constant *atEncoding,
                                  llvm::Constant *impl) {
  auto descriptor = descriptors.beginStruct();
  descriptor.add(selectorRef);
  descriptor.add(atEncoding);
  descriptor.add(impl);
  descriptor.finishAndAddTo(descriptors);
}

/// Emit an Objective-C method descriptor for the given method.
/// struct method_t {
///   SEL name;
///   const char *types;
///   IMP imp;
/// };
void irgen::emitObjCMethodDescriptor(IRGenModule &IGM,
                                     ConstantArrayBuilder &descriptors,
                                     AbstractFunctionDecl *method) {
  llvm::Constant *selectorRef, *atEncoding, *impl;
  auto silFn = emitObjCMethodDescriptorParts(IGM, method,
                                /*extended*/ false,
                                /*concrete*/ true,
                                selectorRef, atEncoding, impl);
  buildMethodDescriptor(descriptors, selectorRef, atEncoding, impl);

  if (silFn && silFn->hasObjCReplacement()) {
    auto replacedSelector =
        IGM.getAddrOfObjCMethodName(silFn->getObjCReplacement().str());
    buildMethodDescriptor(descriptors, replacedSelector, atEncoding, impl);
  }
}

void irgen::emitObjCIVarInitDestroyDescriptor(IRGenModule &IGM,
                                              ConstantArrayBuilder &descriptors,
                                              ClassDecl *cd,
                                              llvm::Function *objcImpl,
                                              bool isDestroyer) {
  /// The first element is the selector.
  SILDeclRef declRef = SILDeclRef(cd, 
                                  isDestroyer? SILDeclRef::Kind::IVarDestroyer
                                             : SILDeclRef::Kind::IVarInitializer,
                                  1, 
                                  /*foreign*/ true);
  Selector selector(declRef);
  auto selectorRef = IGM.getAddrOfObjCMethodName(selector.str());
  
  /// The second element is the method signature. A method signature is made of
  /// the return type @encoding and every parameter type @encoding, glued with
  /// numbers that used to represent stack offsets for each of these elements.
  auto ptrSize = IGM.getPointerSize().getValue();
  llvm::SmallString<8> signature;
  signature = "v" + llvm::itostr(ptrSize * 2) + "@0:" + llvm::itostr(ptrSize);
  auto atEncoding = IGM.getAddrOfGlobalString(signature);

  /// The third element is the method implementation pointer.
  auto impl = llvm::ConstantExpr::getBitCast(objcImpl, IGM.Int8PtrTy);

  // Form the method_t instance.
  buildMethodDescriptor(descriptors, selectorRef, atEncoding, impl);
}

llvm::Constant *
irgen::getMethodTypeExtendedEncoding(IRGenModule &IGM,
                                     AbstractFunctionDecl *method) {
  CanSILFunctionType methodType = getObjCMethodType(IGM, method);
  return getObjCEncodingForMethodType(IGM, methodType, true/*Extended*/);
}

llvm::Constant *
irgen::getBlockTypeExtendedEncoding(IRGenModule &IGM,
                                    CanSILFunctionType invokeTy) {
  SILType resultType = invokeTy->getFormalCSemanticResult();

  // Skip the storage pointer, which is encoded as '@?' to avoid the infinite
  // recursion of the usual '@?<...>' rule for blocks.
  auto paramTypes = invokeTy->getParameters().slice(1);
  
  return getObjCEncodingForTypes(IGM, resultType, paramTypes,
                                 "@?0", IGM.getPointerSize().getValue(),
                                 /*extended*/ true);
}

void irgen::emitObjCGetterDescriptor(IRGenModule &IGM,
                                     ConstantArrayBuilder &descriptors,
                                     AbstractStorageDecl *storage) {
  llvm::Constant *selectorRef, *atEncoding, *impl;
  auto *silFn = emitObjCGetterDescriptorParts(IGM, storage, selectorRef,
                                              atEncoding, impl);
  buildMethodDescriptor(descriptors, selectorRef, atEncoding, impl);
  if (silFn && silFn->hasObjCReplacement()) {
    auto replacedSelector =
        IGM.getAddrOfObjCMethodName(silFn->getObjCReplacement().str());
    buildMethodDescriptor(descriptors, replacedSelector, atEncoding, impl);
  }
}

void irgen::emitObjCSetterDescriptor(IRGenModule &IGM,
                                     ConstantArrayBuilder &descriptors,
                                     AbstractStorageDecl *storage) {
  llvm::Constant *selectorRef, *atEncoding, *impl;
  auto *silFn = emitObjCSetterDescriptorParts(IGM, storage, selectorRef,
                                              atEncoding, impl);
  buildMethodDescriptor(descriptors, selectorRef, atEncoding, impl);
  if (silFn && silFn->hasObjCReplacement()) {
    auto replacedSelector =
        IGM.getAddrOfObjCMethodName(silFn->getObjCReplacement().str());
    buildMethodDescriptor(descriptors, replacedSelector, atEncoding, impl);
  }
}

bool irgen::requiresObjCMethodDescriptor(FuncDecl *method) {
  // Property accessors should be generated alongside the property.
  if (isa<AccessorDecl>(method))
    return false;

  return method->isObjC() || method->getAttrs().hasAttribute<IBActionAttr>();
}

bool irgen::requiresObjCMethodDescriptor(ConstructorDecl *constructor) {
  return constructor->isObjC();
}

bool irgen::requiresObjCPropertyDescriptor(IRGenModule &IGM,
                                           VarDecl *property) {
  // Don't generate a descriptor for a property without any accessors.
  // This is only possible in SIL files because Sema will normally
  // implicitly synthesize accessors for @objc properties.
  return property->isObjC() && property->getGetter();
}

bool irgen::requiresObjCSubscriptDescriptor(IRGenModule &IGM,
                                            SubscriptDecl *subscript) {
  return subscript->isObjC();
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
