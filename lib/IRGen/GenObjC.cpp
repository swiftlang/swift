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
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"

#include "ASTVisitor.h"
#include "CallEmission.h"
#include "Explosion.h"
#include "FormalType.h"
#include "FunctionRef.h"
#include "GenClass.h"
#include "GenFunc.h"
#include "GenMeta.h"
#include "GenType.h"
#include "HeapTypeInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "ScalarTypeInfo.h"
#include "TypeVisitor.h"

#include "GenObjC.h"

using namespace swift;
using namespace irgen;

/// Create an Objective-C runtime function.  The ObjC runtime uses the
/// standard C conventions.
static llvm::Constant *createObjCRuntimeFunction(IRGenModule &IGM, StringRef name,
                                                 llvm::FunctionType *fnType) {
  llvm::Constant *addr = IGM.Module.getOrInsertFunction(name, fnType);
  return addr;
}

namespace {
  template<llvm::Constant * (IRGenModule::*FIELD), char const *NAME>
  llvm::Constant *getObjCSendFn(IRGenModule &IGM) {
    if (IGM.*FIELD)
      return IGM.*FIELD;

    // We use a totally bogus signature to make sure we *always* cast.
    llvm::FunctionType *fnType =
      llvm::FunctionType::get(IGM.VoidTy, ArrayRef<llvm::Type*>(), false);
    IGM.*FIELD = createObjCRuntimeFunction(IGM, NAME, fnType);
    return IGM.*FIELD;
  }
  // T objc_msgSend(id, SEL*, U...);
  static const char objc_msgSend_name[] = "objc_msgSend";
  // void objc_msgSend_stret([[sret]] T *, id, SEL, U...);
  static const char objc_msgSend_stret_name[] = "objc_msgSend_stret";
  // T objc_msgSendSuper2(struct objc_super *, SEL, U...);
  static const char objc_msgSendSuper_name[] = "objc_msgSendSuper2";
  // void objc_msgSendSuper2_stret([[sret]] T *, struct objc_super *, SEL, U...);
  static const char objc_msgSendSuper_stret_name[]
    = "objc_msgSendSuper2_stret";

} // end anonymous namespace



llvm::Constant *IRGenModule::getObjCMsgSendFn() {
  return getObjCSendFn<&IRGenModule::ObjCMsgSendFn, objc_msgSend_name>(*this);
}

llvm::Constant *IRGenModule::getObjCMsgSendStretFn() {
  return getObjCSendFn<&IRGenModule::ObjCMsgSendStretFn,
                       objc_msgSend_stret_name>(*this);
}

llvm::Constant *IRGenModule::getObjCMsgSendSuperFn() {
  return getObjCSendFn<&IRGenModule::ObjCMsgSendSuperFn,
                       objc_msgSendSuper_name>(*this);
}

llvm::Constant *IRGenModule::getObjCMsgSendSuperStretFn() {
  return getObjCSendFn<&IRGenModule::ObjCMsgSendSuperStretFn,
                       objc_msgSendSuper_stret_name>(*this);
}

llvm::Constant *IRGenModule::getObjCSelRegisterNameFn() {
  if (ObjCSelRegisterNameFn) return ObjCSelRegisterNameFn;

  // SEL sel_registerName(const char *str);
  llvm::Type *argTypes[1] = { Int8PtrTy };
  auto fnType = llvm::FunctionType::get(ObjCSELTy, argTypes, false);
  ObjCSelRegisterNameFn = createObjCRuntimeFunction(*this, "sel_registerName",
                                                    fnType);
  return ObjCSelRegisterNameFn;
}

#define DEFINE_OBJC_RUNTIME_FUNCTION(LABEL, NAME, RETTY)            \
llvm::Constant *IRGenModule::getObjC##LABEL##Fn() {                 \
  if (ObjC##LABEL##Fn) return ObjC##LABEL##Fn;                      \
                                                                    \
  llvm::FunctionType *fnType =                                      \
    llvm::FunctionType::get(RETTY, ObjCPtrTy, false);               \
  ObjC##LABEL##Fn = createObjCRuntimeFunction(*this, NAME, fnType); \
  return ObjC##LABEL##Fn;                                           \
}

DEFINE_OBJC_RUNTIME_FUNCTION(Retain,
                             "objc_retain",
                             ObjCPtrTy)
DEFINE_OBJC_RUNTIME_FUNCTION(RetainAutoreleasedReturnValue,
                             "objc_retainAutoreleasedReturnValue",
                             ObjCPtrTy)
DEFINE_OBJC_RUNTIME_FUNCTION(Release,
                             "objc_release",
                             VoidTy)
DEFINE_OBJC_RUNTIME_FUNCTION(AutoreleaseReturnValue,
                             "objc_autoreleaseReturnValue",
                             ObjCPtrTy);

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

llvm::Value *IRGenFunction::emitObjCRetainCall(llvm::Value *value) {
  // Get an appropriately-casted function pointer.
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
    ObjCTypeInfo(llvm::PointerType *storageType, Size size, Alignment align)
      : HeapTypeInfo(storageType, size, align) {
    }

    /// Builtin.ObjCPointer requires ObjC reference-counting.
    bool hasSwiftRefcount() const { return false; }
  };
}

const TypeInfo *TypeConverter::convertBuiltinObjCPointer() {
  return new ObjCTypeInfo(IGM.ObjCPtrTy, IGM.getPointerSize(),
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
        llvm_unreachable("property selector should be generated using ForGetter"
                         " or ForSetter constructors");
      }
    }
    
    Selector(VarDecl *property, ForGetter_t) {
      property->getObjCGetterSelector(Text);
    }

    Selector(VarDecl *property, ForSetter_t) {
      property->getObjCSetterSelector(Text);
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
                                              SILConstant method,
                                              SILType origType,
                                              SILType substResultType,
                                              ArrayRef<Substitution> subs,
                                              ExplosionKind maxExplosion,
                                              bool isSuper) {
  assert((method.kind == SILConstant::Kind::Initializer
          || method.kind == SILConstant::Kind::Func)
         && "objc method call must be to a func or constructor decl");
  llvm::AttributeSet attrs;
  auto fnTy = IGF.IGM.getFunctionType(AbstractCC::ObjCMethod,
                                      origType.getSwiftRValueType(),
                                      ExplosionKind::Minimal,
                                      0,
                                      ExtraData::None,
                                      attrs);
  bool indirectResult = requiresExternalIndirectResult(IGF.IGM,
                                                       substResultType);
  if (isSuper)
    fnTy = getMsgSendSuperTy(IGF.IGM, fnTy, indirectResult);

  // Create the appropriate messenger function.
  // FIXME: this needs to be target-specific.
  llvm::Constant *messenger;
  if (indirectResult) {
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

  // FIXME: ObjC method constants should get SILGen-ed with a [sil_cc=c] type.
  CallEmission emission(IGF,
                        Callee::forKnownFunction(origType,
                                                 substResultType,
                                                 subs,
                                                 messenger, nullptr,
                                                 ExplosionKind::Minimal));
  // Compute the selector.
  Selector selector(method.getDecl());

  return emission;
}

/// Emit the 'self'/'super' and '_cmd' arguments for an ObjC method dispatch.
void irgen::addObjCMethodCallImplicitArguments(IRGenFunction &IGF,
                                               Explosion &args,
                                               SILConstant method,
                                               llvm::Value *self,
                                               SILType searchType) {
  // Compute the selector.
  Selector selector(method.getDecl());
    
  // super.constructor references an instance method (even though the
  // decl is really a 'static' member).
  bool isInstanceMethod
    = method.kind == SILConstant::Kind::Initializer
      || method.getDecl()->isInstanceMember();

  if (searchType) {
    emitSuperArgument(IGF, isInstanceMethod, self, args, searchType);
  } else {
    args.add(self);
  }
  assert(args.size() == 1);
  
  // Add the selector value.
  auto selectorRef = IGF.IGM.getAddrOfObjCSelectorRef(selector.str());
  llvm::Value *selectorV;
  if (IGF.IGM.Opts.UseJIT) {
    // When generating JIT'd code, we need to call sel_registerName() to force
    // the runtime to unique the selector.
    selectorV = IGF.Builder.CreateLoad(Address(selectorRef,
                                               IGF.IGM.getPointerAlignment()));
    selectorV = IGF.Builder.CreateCall(IGF.IGM.getObjCSelRegisterNameFn(),
                                       selectorV);
  } else {
    // When generating statically-compiled code, just build a reference to
    // the selector.
    selectorV = IGF.Builder.CreateLoad(Address(selectorRef,
                                               IGF.IGM.getPointerAlignment()));
  }
  args.add(selectorV);
}

/// Create the LLVM function declaration for a thunk that acts like
/// an Objective-C method for a Swift method implementation.
static llvm::Function *findSwiftAsObjCThunk(IRGenModule &IGM,
                                            llvm::StringRef name) {
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
                                                  FuncDecl *method,
                                                  llvm::Function *swiftImpl,
                                                  ExplosionKind explosionLevel,
                                                  unsigned uncurryLevel) {

  // Construct a callee and derive its ownership conventions.
  auto *origFormalType
    = cast<AnyFunctionType>(method->getType()->getCanonicalType());
  auto *origFnType
    = cast<AnyFunctionType>(CanType(origFormalType->getResult()));
  auto callee = Callee::forMethod(CanType(origFormalType),
                                  CanType(origFnType->getResult()),
                                  ArrayRef<Substitution>{},
                                  swiftImpl,
                                  explosionLevel,
                                  uncurryLevel);

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
                                            VarDecl *property) {
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
                                            VarDecl *property) {
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
  auto absCallee = AbstractCallee::forDirectGlobalFunction(IGM, method);
  auto fnRef = FunctionRef(method, absCallee.getBestExplosionLevel(),
                             absCallee.getMaxUncurryLevel());
    
  llvm::Function *swiftImpl = IGM.getAddrOfFunction(fnRef, ExtraData::None);
  ExplosionKind explosionLevel = fnRef.getExplosionLevel();
  unsigned uncurryLevel = fnRef.getUncurryLevel();
  
  return getObjCMethodPointerForSwiftImpl(IGM, selector, method,
                                          swiftImpl, explosionLevel,
                                          uncurryLevel);
}

static bool isObjCGetterSignature(IRGenModule &IGM,
                                  AnyFunctionType *methodType) {
  return methodType->getResult()->getClassOrBoundGenericClass() &&
    methodType->getInput()->isEqual(TupleType::getEmpty(IGM.Context));
}

static bool isObjCSetterSignature(IRGenModule &IGM,
                                  AnyFunctionType *methodType) {
  if (!methodType->getResult()->isEqual(TupleType::getEmpty(IGM.Context)))
    return false;
  if (methodType->getInput()->getClassOrBoundGenericClass())
    return true;
  if (TupleType *inputTuple = methodType->getInput()->getAs<TupleType>()) {
    return inputTuple->getFields().size() == 1
      && inputTuple->getFields()[0].getType()->getClassOrBoundGenericClass();
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
                                          FuncDecl *method,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  Selector selector(method);
  
  /// The first element is the selector.
  selectorRef = IGM.getAddrOfObjCMethodName(selector.str());
  
  /// The second element is the type @encoding. Handle some simple cases, and
  /// leave the rest as null for now.
  AnyFunctionType *methodType = method->getType()->castTo<AnyFunctionType>();
  // Account for the 'this' pointer being curried.
  methodType = methodType->getResult()->castTo<AnyFunctionType>();
  
  if (isObjCGetterSignature(IGM, methodType))
    atEncoding = IGM.getAddrOfGlobalString(GetterMethodSignature);
  else if (isObjCSetterSignature(IGM, methodType))
    atEncoding = IGM.getAddrOfGlobalString(SetterMethodSignature);
  else
    atEncoding = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  
  /// The third element is the method implementation pointer.
  impl = getObjCMethodPointer(IGM, selector, method);
}

/// Emit the components of an Objective-C method descriptor for a
/// property getter method.
void irgen::emitObjCGetterDescriptorParts(IRGenModule &IGM,
                                          VarDecl *property,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  bool isClassProperty = property->getType()->getClassOrBoundGenericClass();
  
  Selector getterSel(property, Selector::ForGetter);
  selectorRef = IGM.getAddrOfObjCMethodName(getterSel.str());
  atEncoding = isClassProperty
   ? IGM.getAddrOfGlobalString(GetterMethodSignature)
   : llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  impl = getObjCGetterPointer(IGM, getterSel, property);
}

/// Emit the components of an Objective-C method descriptor for a
/// property getter method.
void irgen::emitObjCSetterDescriptorParts(IRGenModule &IGM,
                                          VarDecl *property,
                                          llvm::Constant *&selectorRef,
                                          llvm::Constant *&atEncoding,
                                          llvm::Constant *&impl) {
  assert(property->isSettable() && "not a settable property?!");

  bool isClassProperty = property->getType()->getClassOrBoundGenericClass();
  
  Selector setterSel(property, Selector::ForSetter);
  selectorRef = IGM.getAddrOfObjCMethodName(setterSel.str());
  atEncoding = isClassProperty
     ? IGM.getAddrOfGlobalString(SetterMethodSignature)
     : llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  impl = getObjCSetterPointer(IGM, setterSel, property);
}

/// Emit an Objective-C method descriptor for the given method.
/// struct method_t {
///   SEL name;
///   const char *types;
///   IMP imp;
/// };
llvm::Constant *irgen::emitObjCMethodDescriptor(IRGenModule &IGM,
                                                FuncDecl *method) {
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

bool irgen::requiresObjCPropertyDescriptor(VarDecl *property) {
  // We don't export generic methods or subclasses to IRGen yet.
  if (property->getDeclContext()->getDeclaredTypeInContext()
          ->is<BoundGenericType>())
    return false;

  if (property->isObjC())
    return true;
  if (auto override = property->getOverriddenDecl())
    return requiresObjCPropertyDescriptor(override);
  return false;
}
