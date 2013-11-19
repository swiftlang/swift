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
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"

#include "swift/IRGen/Options.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILModule.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"

#include "ASTVisitor.h"
#include "CallEmission.h"
#include "Explosion.h"
#include "FormalType.h"
#include "FunctionRef.h"
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
  // Get an appropriately-casted function pointer.
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
  assert(isa<llvm::PointerType>(valueTy));
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

/// Reclaim an autoreleased return value.
llvm::Value *irgen::emitObjCRetainAutoreleasedReturnValue(IRGenFunction &IGF,
                                                          llvm::Value *value) {
  auto fn = IGF.IGM.getObjCRetainAutoreleasedReturnValueFn();
  fn = getCastOfRetainFn(IGF.IGM, fn, value->getType());

  auto call = IGF.Builder.CreateCall(fn, value);
  call->setDoesNotThrow();
  return call;
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
                 llvm::BitVector spareBits, Alignment align)
      : HeapTypeInfo(storageType, size, spareBits, align) {
    }

    /// Builtin.ObjCPointer requires ObjC reference-counting.
    bool hasSwiftRefcount() const { return false; }
  };
}

const TypeInfo *TypeConverter::convertBuiltinObjCPointer() {
  return new ObjCTypeInfo(IGM.ObjCPtrTy, IGM.getPointerSize(),
                          IGM.getHeapObjectSpareBits(),
                          IGM.getPointerAlignment());
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
llvm::Constant *IRGenModule::getAddrOfObjCProtocolRecord(ProtocolDecl *proto) {
  return const_cast<llvm::Constant*>
    (cast<llvm::Constant>(getObjCProtocolGlobalVars(proto).record));
}

/// Get or create an ObjC protocol reference. Always returns an i8**. We lazily
/// create ObjC protocol_t records for protocols, storing references to the
/// record into the __objc_protolist and  and __objc_protorefs sections to be
/// fixed up by the runtime.
llvm::Constant *IRGenModule::getAddrOfObjCProtocolRef(ProtocolDecl *proto) {
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
  auto *protocolLabel
    = new llvm::GlobalVariable(Module, protocolRecord->getType(),
                               /*constant*/ false,
                               llvm::GlobalValue::WeakAnyLinkage,
                               protocolRecord,
                               llvm::Twine("\01l_OBJC_LABEL_PROTOCOL_$_")
                                 + getObjCProtocolName(proto));
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
                                 + getObjCProtocolName(proto));
  protocolRef->setAlignment(getPointerAlignment().getValue());
  protocolRef->setVisibility(llvm::GlobalValue::HiddenVisibility);
  protocolRef->setSection("__DATA,__objc_protorefs,coalesced,no_dead_strip");

  ObjCProtocolPair pair{protocolRecord, protocolRef};
  ObjCProtocols.insert({proto, pair});
  
  return pair;
}

namespace {
  class Selector {
    
    llvm::SmallString<80> Text;

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
      method->getObjCSelector(Text);
    }
    
    Selector(ConstructorDecl *ctor) {
      ctor->getObjCSelector(Text);
    }
    
    Selector(ValueDecl *methodOrCtor) {
      if (auto *method = dyn_cast<FuncDecl>(methodOrCtor)) {
        method->getObjCSelector(Text);
      } else if (auto *ctor = dyn_cast<ConstructorDecl>(methodOrCtor)) {
        ctor->getObjCSelector(Text);
      } else {
        llvm_unreachable("property or subscript selector should be generated "
                         "using ForGetter or ForSetter constructors");
      }
    }
    
    Selector(VarDecl *property, ForGetter_t) {
      property->getObjCGetterSelector(Text);
    }

    Selector(VarDecl *property, ForSetter_t) {
      property->getObjCSetterSelector(Text);
    }

    Selector(SubscriptDecl *subscript, ForGetter_t) {
      Text = subscript->getObjCGetterSelector();
    }

    Selector(SubscriptDecl *subscript, ForSetter_t) {
      Text = subscript->getObjCSetterSelector();
    }

    Selector(SILDeclRef ref) {
      switch (ref.kind) {
      case SILDeclRef::Kind::Allocator:
      case SILDeclRef::Kind::DefaultArgGenerator:
      case SILDeclRef::Kind::Destroyer:
      case SILDeclRef::Kind::EnumElement:
      case SILDeclRef::Kind::GlobalAccessor:
        llvm_unreachable("Method does not have a selector");

      case SILDeclRef::Kind::Func:
        cast<FuncDecl>(ref.getDecl())->getObjCSelector(Text);
        break;

      case SILDeclRef::Kind::Getter:
        if (auto var = dyn_cast<VarDecl>(ref.getDecl()))
          var->getObjCGetterSelector(Text);
        else
          Text = cast<SubscriptDecl>(ref.getDecl())->getObjCGetterSelector();
        break;

      case SILDeclRef::Kind::Initializer:
        cast<ConstructorDecl>(ref.getDecl())->getObjCSelector(Text);
        break;

      case SILDeclRef::Kind::Setter:
        if (auto var = dyn_cast<VarDecl>(ref.getDecl()))
          var->getObjCSetterSelector(Text);
        else
          Text = cast<SubscriptDecl>(ref.getDecl())->getObjCSetterSelector();
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
    searchValue = emitClassHeapMetadataRef(IGF, searchClass);
  } else {
    ClassDecl *searchClassDecl =
      searchClass.castTo<MetaTypeType>()->getInstanceType()
        ->getClassOrBoundGenericClass();
    searchValue = IGF.IGM.getAddrOfMetaclassObject(searchClassDecl);
  }
  searchValue = IGF.Builder.CreateBitCast(searchValue, IGF.IGM.ObjCClassPtrTy);
  
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
                                              ExplosionKind maxExplosion,
                                              bool isSuper) {
  assert((method.kind == SILDeclRef::Kind::Initializer
          || method.kind == SILDeclRef::Kind::Func
          || method.kind == SILDeclRef::Kind::Getter
          || method.kind == SILDeclRef::Kind::Setter)
         && "objc method call must be to a func/initializer/getter/setter");

  ExplosionKind explosionLevel = ExplosionKind::Minimal;

  llvm::AttributeSet attrs;
  auto fnTy = IGF.IGM.getFunctionType(origFnType,
                                      explosionLevel,
                                      ExtraData::None,
                                      attrs);
  bool indirectResult = requiresExternalIndirectResult(IGF.IGM, origFnType,
                                                       explosionLevel);
  if (isSuper)
    fnTy = getMsgSendSuperTy(IGF.IGM, fnTy, indirectResult);

  // Create the appropriate messenger function.
  // FIXME: this needs to be target-specific.
  llvm::Constant *messenger;
  if (indirectResult && IGF.IGM.TargetInfo.ObjCUseStret) {
    messenger = isSuper
      ? IGF.IGM.getObjCMsgSendSuperStretFn()
      : IGF.IGM.getObjCMsgSendStretFn();
  } else {
    messenger = isSuper
      ? IGF.IGM.getObjCMsgSendSuperFn()
      : IGF.IGM.getObjCMsgSendFn();
  }

  // Cast the messenger to the right type.
  messenger = llvm::ConstantExpr::getBitCast(messenger, fnTy->getPointerTo());

  CallEmission emission(IGF,
                        Callee::forKnownFunction(origFnType,
                                                 substFnType,
                                                 subs,
                                                 messenger, nullptr,
                                                 explosionLevel));
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
  // decl is really a 'static' member).
  bool isInstanceMethod
    = method.kind == SILDeclRef::Kind::Initializer
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
    SILParameterInfo(CanType(MetaTypeType::get(classType, ctx)),
                     ParameterConvention::Direct_Unowned)
  };
  auto result = SILResultInfo(classType, ResultConvention::Owned);
  auto extInfo = SILFunctionType::ExtInfo(AbstractCC::ObjCMethod,
                                          /*thin*/ true,
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
  auto explosionLevel = ExplosionKind::Minimal;
  unsigned uncurryLevel = 0;

  // Compute the appropriate LLVM type for the function.
  llvm::AttributeSet attrs;
  auto fnTy = IGF.IGM.getFunctionType(formalType, explosionLevel,
                                      ExtraData::None, attrs);

  // Get the messenger function.
  llvm::Constant *messenger = IGF.IGM.getObjCMsgSendFn();
  messenger = llvm::ConstantExpr::getBitCast(messenger, fnTy->getPointerTo());

  // Prepare the call.
  CallEmission emission(IGF, Callee::forKnownFunction(formalType,
                                                      formalType, {},
                                                      messenger, nullptr,
                                                      explosionLevel,
                                                      uncurryLevel));

  // Emit the arguments.
  {
    Explosion args(emission.getCurExplosionLevel());
    args.add(self);
    args.add(IGF.emitObjCSelectorRefLoad("allocWithZone:"));
    args.add(llvm::ConstantPointerNull::get(IGF.IGM.Int8PtrTy));
    emission.addArg(args);
  }

  // Emit the call.
  Explosion out(explosionLevel);
  emission.emitToExplosion(out);
  return out.claimNext();
}

static llvm::Function *emitObjCPartialApplicationForwarder(IRGenModule &IGM,
                                            SILDeclRef method,
                                            CanSILFunctionType origMethodType,
                                            CanSILFunctionType resultType,
                                            const HeapLayout &layout,
                                            const TypeInfo &selfTI) {
  llvm::AttributeSet attrs;
  llvm::FunctionType *fwdTy = IGM.getFunctionType(resultType,
                                                  ExplosionKind::Minimal,
                                                  ExtraData::Retainable,
                                                  attrs);
  // FIXME: Give the thunk a real name.
  // FIXME: Maybe cache the thunk by function and closure types?
  llvm::Function *fwd =
    llvm::Function::Create(fwdTy, llvm::Function::InternalLinkage,
                           "_TPAo", &IGM.Module);
  fwd->setAttributes(attrs);
  
  IRGenFunction subIGF(IGM, fwd);
  
  // Recover 'self' from the context.
  Explosion params = subIGF.collectParameters(ExplosionKind::Minimal);
  llvm::Value *context = params.takeLast();
  Address dataAddr = layout.emitCastTo(subIGF, context);
  auto &fieldLayout = layout.getElements()[0];
  Address selfAddr = fieldLayout.project(subIGF, dataAddr, Nothing);
  Explosion selfParams(ExplosionKind::Minimal);
  // FIXME: Copying the value leaks if 'self' is unconsumed, but not copying it
  // overreleases if 'self' is consumed and the forwarder is invoked multiple
  // times.
  cast<LoadableTypeInfo>(selfTI).loadAsCopy(subIGF, selfAddr, selfParams);
  llvm::Value *self = selfParams.claimNext();
  
  // Save off the forwarded indirect return address if we have one.
  llvm::Value *indirectReturn = nullptr;
  SILType appliedResultTy = origMethodType->getSemanticResultSILType();
  auto &appliedResultTI = IGM.getTypeInfo(appliedResultTy);
  if (appliedResultTI.getSchema(ExplosionKind::Minimal)
        .requiresIndirectResult(IGM)) {
    indirectReturn = params.claimNext();
  }

  // Prepare the call to the underlying method.
  CallEmission emission
    = prepareObjCMethodRootCall(subIGF, method, origMethodType, origMethodType,
       ArrayRef<Substitution>{}, ExplosionKind::Minimal, /*isSuper*/ false);
  
  Explosion args(params.getKind());
  addObjCMethodCallImplicitArguments(subIGF, args, method, self, SILType());
  args.add(params.claimAll());
  emission.addArg(args);
  
  // Emit the call and produce the return value.
  if (indirectReturn) {
    emission.emitToMemory(appliedResultTI.getAddressForPointer(indirectReturn),
                          appliedResultTI);
    subIGF.Builder.CreateRetVoid();
  } else {
    Explosion result(ExplosionKind::Minimal);
    emission.emitToExplosion(result);
    subIGF.emitScalarReturn(result);
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
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, selfTypeInfo);
  llvm::Value *data = IGF.emitUnmanagedAlloc(layout, "closure");
  // FIXME: non-fixed offsets
  NonFixedOffsets offsets = Nothing;
  Address dataAddr = layout.emitCastTo(IGF, data);
  auto &fieldLayout = layout.getElements()[0];
  Address fieldAddr = fieldLayout.project(IGF, dataAddr, offsets);
  Explosion selfParams(ExplosionKind::Minimal);
  selfParams.add(self);
  fieldLayout.getType().initializeFromParams(IGF, selfParams, fieldAddr);

  // Create the forwarding stub.
  llvm::Function *forwarder = emitObjCPartialApplicationForwarder(IGF.IGM,
                                                                method,
                                                                origMethodType,
                                                                resultType,
                                                                layout,
                                                                *selfTypeInfo);
  llvm::Value *forwarderValue = IGF.Builder.CreateBitCast(forwarder,
                                                          IGF.IGM.Int8PtrTy);
  
  // Emit the result explosion.
  out.add(forwarderValue);
  out.add(data);
}

/// Create the LLVM function declaration for a thunk that acts like
/// an Objective-C method for a Swift method implementation.
static llvm::Function *findSwiftAsObjCThunk(IRGenModule &IGM, StringRef name) {
  // Construct the thunk name.
  llvm::SmallString<128> buffer;
  buffer.reserve(name.size() + 2);
  buffer.append("_TTo");
  assert(name.startswith("_T"));
  buffer.append(name.substr(2));

  auto fn = IGM.Module.getFunction(buffer);
  assert(fn && "no SIL function for swift-as-objc thunk");
  // FIXME: Should set the linkage of the SILFunction to 'internal'.
  fn->setLinkage(llvm::GlobalValue::InternalLinkage);
  fn->setUnnamedAddr(true);
  return fn;
}

/// Produce a pointer to the objc_msgSend-compatible thunk wrapping the
/// given Swift implementation, at the given explosion and uncurry levels.
static llvm::Constant *getObjCMethodPointerForSwiftImpl(IRGenModule &IGM,
                                                  const Selector &selector,
                                                        SILDeclRef declRef,
                                                  llvm::Function *swiftImpl,
                                               ExplosionKind explosionLevel) {

  // Construct a callee and derive its ownership conventions.
  auto origFormalType = IGM.SILMod->Types.getConstantFormalType(declRef);
  auto origFnType = IGM.SILMod->Types.getSILFunctionType(origFormalType,
                                               origFormalType, /*uncurry*/ 0);

  auto callee = Callee::forMethod(origFnType,
                                  origFnType,
                                  ArrayRef<Substitution>{},
                                  swiftImpl,
                                  explosionLevel,
                                  declRef.uncurryLevel);

  llvm::Function *objcImpl
    = findSwiftAsObjCThunk(IGM, swiftImpl->getName());
  return llvm::ConstantExpr::getBitCast(objcImpl, IGM.Int8PtrTy);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given property's getter method implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCGetterPointer(IRGenModule &IGM,
                                            const Selector &selector,
                                            ValueDecl *property) {
  // Protocol properties have no impl.
  if (isa<ProtocolDecl>(property->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  
  // FIXME: Explosion level
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  
  FormalType getterType = IGM.getTypeOfGetter(property);
  llvm::SmallString<32> swiftName;
  
  // Find the ObjC thunk for the property.
  CodeRef getterCode = CodeRef::forGetter(property, explosionLevel,
                                        getterType.getNaturalUncurryLevel());
  LinkEntity getterEntity = LinkEntity::forFunction(getterCode);
  getterEntity.mangle(swiftName);
  
  llvm::Function *objcImpl = findSwiftAsObjCThunk(IGM, swiftName);
  return llvm::ConstantExpr::getBitCast(objcImpl, IGM.Int8PtrTy);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given property's setter method implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCSetterPointer(IRGenModule &IGM,
                                            const Selector &selector,
                                            ValueDecl *property) {
  // Protocol properties have no impl.
  if (isa<ProtocolDecl>(property->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  assert(property->isSettable() && "property is not settable?!");
  
  // FIXME: Explosion level
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  
  FormalType setterType = IGM.getTypeOfSetter(property);
  llvm::SmallString<32> swiftName;
  
  // Generate the name of the ObjC thunk for the setter.
  CodeRef setterCode = CodeRef::forSetter(property, explosionLevel,
                                          setterType.getNaturalUncurryLevel());
  LinkEntity setterEntity = LinkEntity::forFunction(setterCode);
  setterEntity.mangle(swiftName);
  
  llvm::Function *objcImpl = findSwiftAsObjCThunk(IGM, swiftName);
  return llvm::ConstantExpr::getBitCast(objcImpl, IGM.Int8PtrTy);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given method implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCMethodPointer(IRGenModule &IGM,
                                            const Selector &selector,
                                            FuncDecl *method) {
  // Protocol methods have no impl.
  if (isa<ProtocolDecl>(method->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  auto absCallee = AbstractCallee::forDirectGlobalFunction(IGM, method);
  auto fnRef = FunctionRef(method, absCallee.getBestExplosionLevel(),
                             absCallee.getMaxUncurryLevel());
  ExplosionKind explosionLevel = fnRef.getExplosionLevel();
  unsigned uncurryLevel = fnRef.getUncurryLevel();

  SILDeclRef declRef = SILDeclRef(method, SILDeclRef::Kind::Func,
                                  uncurryLevel, /*foreign*/ true);
    
  llvm::Function *swiftImpl = IGM.getAddrOfFunction(fnRef, ExtraData::None);

  return getObjCMethodPointerForSwiftImpl(IGM, selector, declRef,
                                          swiftImpl, explosionLevel);
}

/// Produce a function pointer, suitable for invocation by
/// objc_msgSend, for the given constructor implementation.
///
/// Returns a value of type i8*.
static llvm::Constant *getObjCMethodPointer(IRGenModule &IGM,
                                            const Selector &selector,
                                            ConstructorDecl *constructor) {
  // Protocol methods have no impl.
  if (isa<ProtocolDecl>(constructor->getDeclContext()))
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  
  auto absCallee = AbstractCallee::forDirectGlobalFunction(IGM, constructor);
  unsigned uncurryLevel = absCallee.getMaxUncurryLevel();
  auto explosionLevel = absCallee.getBestExplosionLevel();

  llvm::Function *swiftImpl
    = IGM.getAddrOfConstructor(constructor, ConstructorKind::Initializing,
                               explosionLevel);

  SILDeclRef declRef = SILDeclRef(constructor, SILDeclRef::Kind::Initializer,
                                  uncurryLevel, /*foreign*/ true);

  return getObjCMethodPointerForSwiftImpl(IGM, selector, declRef,
                                          swiftImpl, explosionLevel);
}

/// True if the value is of class type, or of a type that is bridged to class
/// type.
bool irgen::hasObjCClassRepresentation(IRGenModule &IGM, Type t) {
  return IGM.SILMod->Types.getLoweredBridgedType(t, AbstractCC::ObjCMethod)
    ->getClassOrBoundGenericClass();
}

static bool isObjCGetterSignature(IRGenModule &IGM,
                                  AnyFunctionType *methodType) {
  return hasObjCClassRepresentation(IGM, methodType->getResult()) &&
    methodType->getInput()->isEqual(TupleType::getEmpty(IGM.Context));
}

static bool isObjCSetterSignature(IRGenModule &IGM,
                                  AnyFunctionType *methodType) {
  if (!methodType->getResult()->isEqual(TupleType::getEmpty(IGM.Context)))
    return false;
  if (hasObjCClassRepresentation(IGM, methodType->getInput()))
    return true;
  if (TupleType *inputTuple = methodType->getInput()->getAs<TupleType>()) {
    return inputTuple->getNumElements() == 1
      && hasObjCClassRepresentation(IGM, inputTuple->getElementType(0));
  }
  return false;
}

/// ObjC method encoding for a property getter of class type.
/// - (SomeClass*)foo;
static const char * const GetterMethodSignature = "@@:";
/// ObjC method encoding for a property setter of class type.
/// - (void)setFoo:(SomeClass*);
static const char * const SetterMethodSignature = "v@:@";

/// Emit the components of an Objective-C method descriptor: its selector,
/// type encoding, and IMP pointer.
void irgen::emitObjCMethodDescriptorParts(IRGenModule &IGM,
                                          AbstractFunctionDecl *method,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  Selector selector(method);
  
  /// The first element is the selector.
  selectorRef = IGM.getAddrOfObjCMethodName(selector.str());
  
  /// The second element is the type @encoding. Handle some simple cases, and
  /// leave the rest as null for now.
  AnyFunctionType *methodType = method->getType()->castTo<AnyFunctionType>();
  // Account for the 'self' pointer being curried.
  methodType = methodType->getResult()->castTo<AnyFunctionType>();
  
  if (isObjCGetterSignature(IGM, methodType))
    atEncoding = IGM.getAddrOfGlobalString(GetterMethodSignature);
  else if (isObjCSetterSignature(IGM, methodType))
    atEncoding = IGM.getAddrOfGlobalString(SetterMethodSignature);
  else
    atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  
  /// The third element is the method implementation pointer.
  if (auto func = dyn_cast<FuncDecl>(method))
    impl = getObjCMethodPointer(IGM, selector, func);
  else
    impl = getObjCMethodPointer(IGM, selector, cast<ConstructorDecl>(method));
}

/// Emit the components of an Objective-C method descriptor for a
/// property getter method.
void irgen::emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                          VarDecl *property,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  bool isClassProperty = hasObjCClassRepresentation(IGM, property->getType());
  
  Selector getterSel(property, Selector::ForGetter);
  selectorRef = IGM.getAddrOfObjCMethodName(getterSel.str());
  atEncoding = isClassProperty
   ? IGM.getAddrOfGlobalString(GetterMethodSignature)
   : llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  impl = getObjCGetterPointer(IGM, getterSel, property);
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
  impl = getObjCGetterPointer(IGM, getterSel, subscript);
}

/// Emit the components of an Objective-C method descriptor for a
/// property getter method.
void irgen::emitObjCSetterDescriptorParts(IRGenModule &IGM,
                                          VarDecl *property,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  assert(property->isSettable() && "not a settable property?!");

  bool isClassProperty = hasObjCClassRepresentation(IGM, property->getType());
  
  Selector setterSel(property, Selector::ForSetter);
  selectorRef = IGM.getAddrOfObjCMethodName(setterSel.str());
  atEncoding = isClassProperty
     ? IGM.getAddrOfGlobalString(SetterMethodSignature)
     : llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  impl = getObjCSetterPointer(IGM, setterSel, property);
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
  impl = getObjCSetterPointer(IGM, setterSel, subscript);
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
                                selectorRef, atEncoding, impl);
  
  llvm::Constant *fields[] = { selectorRef, atEncoding, impl };
  return llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), fields);
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
  
  if (property->isSettable()) {
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
  if (method->isGetterOrSetter())
    return false;
    
    // We don't export generic methods or subclasses to IRGen yet.
  if (method->getType()->is<PolymorphicFunctionType>()
      || method->getType()->getAs<AnyFunctionType>()
          ->getResult()->is<PolymorphicFunctionType>()
      || method->getDeclContext()->getDeclaredTypeInContext()
          ->is<BoundGenericType>())
    return false;
  
  if (method->isObjC() || method->getAttrs().isIBAction())
    return true;
  if (auto override = method->getOverriddenDecl())
    return requiresObjCMethodDescriptor(override);
  return false;
}

bool irgen::requiresObjCMethodDescriptor(ConstructorDecl *constructor) {
  // We don't export generic methods or subclasses to IRGen yet.
  // FIXME: Total hack. Sema should filter these out.
  if (constructor->getType()->is<PolymorphicFunctionType>()
      || constructor->getType()->getAs<AnyFunctionType>()
           ->getResult()->is<PolymorphicFunctionType>()
      || constructor->getDeclContext()->getDeclaredTypeInContext()
           ->is<BoundGenericType>())
    return false;

  return constructor->isObjC();
}

bool irgen::requiresObjCPropertyDescriptor(VarDecl *property) {
  // We don't export generic methods or subclasses to IRGen yet.
  if (property->getDeclContext()->getDeclaredTypeInContext()
          ->is<BoundGenericType>())
    return false;

  if (auto override = property->getOverriddenDecl())
    return requiresObjCPropertyDescriptor(override);

  if (!property->isObjC())
    return false;
  
  // Don't expose objc properties for function types. We can't autorelease them,
  // and eventually we want to map them back to blocks.
  if (property->getType()->is<AnyFunctionType>())
    return false;
  
  return true;
}

bool irgen::requiresObjCSubscriptDescriptor(SubscriptDecl *subscript) {
  // We don't export generic methods or subclasses to IRGen yet.
  if (subscript->getDeclContext()->getDeclaredTypeInContext()
          ->is<BoundGenericType>())
    return false;

  if (auto override = subscript->getOverriddenDecl())
    return requiresObjCSubscriptDescriptor(override);

  if (!subscript->isObjC())
    return false;
  
  // Don't expose objc properties for function types. We can't autorelease them,
  // and eventually we want to map them back to blocks.
  if (subscript->getElementType()->is<AnyFunctionType>())
    return false;
  
  return true;
}
