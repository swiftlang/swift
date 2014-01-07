//===--- GenFunc.cpp - Swift IR Generation for Function Types -------------===//
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
//  This file implements IR generation for function types in Swift.  This
//  includes creating the IR type as well as capturing variables and
//  performing calls.
//
//  Swift function types are always expanded as a struct containing
//  two opaque pointers.  The first pointer is to a function (should
//  this be a descriptor?) to which the second pointer is passed,
//  along with the formal arguments.  The function pointer is opaque
//  because the alternative would require infinite types to faithfully
//  represent, since aggregates containing function types can be
//  passed and returned by value, not necessary as first-class
//  aggregates.
//
//  There are several considerations for whether to pass the data
//  pointer as the first argument or the last:
//    - On CCs that pass anything in registers, dropping the last
//      argument is significantly more efficient than dropping the
//      first, and it's not that unlikely that the data might
//      be ignored.
//    - A specific instance of that:  we can use the address of a
//      global "data-free" function directly when taking an
//      address-of-function.
//    - Replacing a pointer argument with a different pointer is
//      quite efficient with pretty much any CC.
//    - Later arguments can be less efficient to access if they
//      actually get passed on the stack, but there's some leeway
//      with a decent CC.
//    - Passing the data pointer last inteferes with native variadic
//      arguments, but we probably don't ever want to use native
//      variadic arguments.
//  This works out to a pretty convincing argument for passing the
//  data pointer as the last argument.
//
//  On the other hand, it is not compatible with blocks.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/Optional.h"
#include "swift/SIL/SILModule.h"
#include "clang/AST/CanonicalType.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"
#include "clang/CodeGen/CodeGenABITypes.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/StringSwitch.h"

#include "ASTVisitor.h"
#include "CallingConvention.h"
#include "CallEmission.h"
#include "Explosion.h"
#include "FunctionRef.h"
#include "GenClass.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenObjC.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenType.h"
#include "HeapTypeInfo.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "FixedTypeInfo.h"
#include "ScalarTypeInfo.h"
#include "GenFunc.h"

using namespace swift;
using namespace irgen;

bool ExplosionSchema::requiresIndirectResult(IRGenModule &IGM) const {
  return containsAggregate() ||
         size() > IGM.TargetInfo.MaxScalarsForDirectResult;
}

llvm::Type *ExplosionSchema::getScalarResultType(IRGenModule &IGM) const {
  if (size() == 0) {
    return IGM.VoidTy;
  } else if (size() == 1) {
    return begin()->getScalarType();
  } else {
    SmallVector<llvm::Type*, 16> elts;
    for (auto &elt : *this) elts.push_back(elt.getScalarType());
    return llvm::StructType::get(IGM.getLLVMContext(), elts);
  }
}

void ExplosionSchema::addToArgTypes(IRGenModule &IGM,
                                    SmallVectorImpl<llvm::Type*> &types) const {
  for (auto &elt : *this) {
    if (elt.isAggregate())
      types.push_back(elt.getAggregateType()->getPointerTo());
    else
      types.push_back(elt.getScalarType());
  }
}

/// Return the natural level at which to uncurry this function.  This
/// is the number of additional parameter clauses that are uncurried
/// in the function body.
unsigned irgen::getDeclNaturalUncurryLevel(ValueDecl *val) {
  if (FuncDecl *FD = dyn_cast<FuncDecl>(val)) {
    return FD->getNaturalArgumentCount() - 1;
  }
  if (isa<ConstructorDecl>(val) || isa<EnumElementDecl>(val)) {
    return 1;
  }
  llvm_unreachable("Unexpected ValueDecl");
}

/// Given a function type, return the formal result type at the given
/// uncurrying level.  For 'a -> b -> c', this is 'b' at 0 and 'c' at 1.
CanType irgen::getResultType(CanType type, unsigned uncurryLevel) {
  do {
    type = CanType(cast<AnyFunctionType>(type)->getResult());
  } while (uncurryLevel--);
  return type;
}

static llvm::CallingConv::ID getFreestandingConvention(IRGenModule &IGM) {
  // TODO: use a custom CC that returns three scalars efficiently
  return llvm::CallingConv::C;
}

/// Expand the requirements of the given abstract calling convention
/// into a "physical" calling convention.
llvm::CallingConv::ID irgen::expandAbstractCC(IRGenModule &IGM,
                                              AbstractCC convention) {
  switch (convention) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    return llvm::CallingConv::C;

  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    //   TODO: maybe add 'inreg' to the first non-result argument.
    SWIFT_FALLTHROUGH;
  case AbstractCC::Freestanding:
    return getFreestandingConvention(IGM);
  }
  llvm_unreachable("bad calling convention!");
}

namespace {
  /// The natural form of the result of performing a call.  A call
  /// result may be indirect, in which case it is returned in memory
  /// whose address is passed as an implicit first argument, or it may
  /// be direct.
  class CallResult {
    union Value {
      /// The buffer for the set of direct values produced by the call.
      /// This can be greater than the normal cap on scalar values if
      /// the actual call is inlined or builtin.
      Explosion Direct;

      /// The address into which to emit an indirect call.  If this is
      /// set, the call will be evaluated (as an initialization) into
      /// this address; otherwise, memory will be allocated on the stack.
      Address Indirect;

      Value() {}
      ~Value() {}
    };

    enum class State {
      Invalid, Indirect, Direct
    };

    Value CurValue;
    State CurState;

  public:
    CallResult() : CurState(State::Invalid) {}
    ~CallResult() { reset(); }

    /// Configure this result to carry a number of direct values at
    /// the given explosion level.
    Explosion &initForDirectValues(ExplosionKind level) {
      assert(CurState == State::Invalid);
      CurState = State::Direct;
      return *new (&CurValue.Direct) Explosion(level);
    }

    /// As a potential efficiency, set that this is a direct result
    /// with no values.
    void setAsEmptyDirect() {
      initForDirectValues(ExplosionKind::Maximal);
    }

    /// Set this result so that it carries a single directly-returned
    /// maximally-fragile value without management.
    void setAsSingleDirectUnmanagedFragileValue(llvm::Value *value) {
      initForDirectValues(ExplosionKind::Maximal).add(value);
    }

    void setAsIndirectAddress(Address address) {
      assert(CurState == State::Invalid);
      CurState = State::Indirect;
      CurValue.Indirect = address;
    }

    bool isInvalid() const { return CurState == State::Invalid; } 
    bool isDirect() const { return CurState == State::Direct; }
    bool isIndirect() const { return CurState == State::Indirect; }

    Explosion &getDirectValues() {
      assert(isDirect());
      return CurValue.Direct;
    }

    Address getIndirectAddress() const {
      assert(isIndirect());
      return CurValue.Indirect;
    }

    void reset() {
      if (CurState == State::Direct)
        CurValue.Direct.~Explosion();
      CurState = State::Invalid;
    }
  };

  /// A signature represents something which can actually be called.
  class Signature {
    llvm::PointerIntPair<llvm::FunctionType*, 1, bool> TypeAndHasIndirectReturn;
    llvm::AttributeSet Attributes;

  public:
    bool isValid() const {
      return TypeAndHasIndirectReturn.getPointer() != nullptr;
    }

    void set(llvm::FunctionType *type, bool hasIndirectReturn,
             llvm::AttributeSet attrs) {
      TypeAndHasIndirectReturn.setPointer(type);
      TypeAndHasIndirectReturn.setInt(hasIndirectReturn);
      Attributes = attrs;
      assert(isValid());
    }

    llvm::FunctionType *getType() const {
      assert(isValid());
      return TypeAndHasIndirectReturn.getPointer();
    }

    bool hasIndirectReturn() const {
      assert(isValid());
      return TypeAndHasIndirectReturn.getInt();
    }
    
    llvm::AttributeSet getAttributes() const {
      return Attributes;
    }
  };

  /// Calculate the extra data kind for a function type.
  static ExtraData getExtraDataKind(IRGenModule &IGM,
                                    CanSILFunctionType formalType) {
    // If the type is thin, there is no extra data.
    if (formalType->isThin())
      return ExtraData::None;
    
    // Otherwise, the extra data depends on the calling convention.
    switch (formalType->getAbstractCC()) {
    case AbstractCC::Freestanding:
    case AbstractCC::Method:
      // For non-witness methods, 'thick' always indicates a retainable context
      // pointer.
      return ExtraData::Retainable;

    case AbstractCC::WitnessMethod:
      // A 'thick' witness is partially applied to its Self archetype binding.
      //
      // TODO: This requires extra data only if the necessary metadata is not
      // already available through a metatype or class 'self' parameter.
      //
      // TODO: For default implementations, the witness table needs to be
      // supplied too.
      return ExtraData::Metatype;
    
    case AbstractCC::C:
    case AbstractCC::ObjCMethod:
      llvm_unreachable("thick foreign functions should be lowered to a "
                       "block type");
    }
  }
  
  /// The type-info class.
  class FuncTypeInfo : public ScalarTypeInfo<FuncTypeInfo, ReferenceTypeInfo> {
    /// Each possible currying of a function type has different function
    /// type variants along each of three orthogonal axes:
    ///   - the explosion kind desired
    ///   - whether a data pointer argument is required
    /// TODO: The ExtraData bit will be fully dependent on the formal type when
    /// we enable SIL witness tables.
    struct Currying {
      Signature Signatures[unsigned(ExplosionKind::Last_ExplosionKind) + 1]
                          [unsigned(ExtraData::Last_ExtraData) + 1];

      Signature &select(ExplosionKind kind, ExtraData extraData) {
        return Signatures[unsigned(kind)][unsigned(extraData)];
      }
    };

    /// The SIL function type being represented.
    const CanSILFunctionType FormalType;
    
    /// The ExtraData kind associated with the function reference.
    ExtraData ExtraDataKind;

    mutable Currying TheSignatures;

    FuncTypeInfo(CanSILFunctionType formalType, llvm::Type *storageType,
                 Size size, Alignment align, ExtraData ExtraDataKind)
      // FIXME: Spare bits.
      : ScalarTypeInfo(storageType, size, llvm::BitVector{}, align),
        FormalType(formalType), ExtraDataKind(ExtraDataKind)
    {
    }

  public:
    static const FuncTypeInfo *create(CanSILFunctionType formalType,
                                      llvm::Type *storageType,
                                      Size size, Alignment align,
                                      ExtraData extraDataKind) {
      return new FuncTypeInfo(formalType, storageType, size, align,
                              extraDataKind);
    }
    
    // Function types do not satisfy allowsOwnership.
    const WeakTypeInfo *
    createWeakStorageType(TypeConverter &TC) const override {
      llvm_unreachable("[weak] function type");
    }
    const UnownedTypeInfo *
    createUnownedStorageType(TypeConverter &TC) const override {
      llvm_unreachable("[unowned] function type");
    }

    ExtraData getExtraDataKind() const {
      return ExtraDataKind;
    }
    
    Signature getSignature(IRGenModule &IGM,
                           ExplosionKind explosionKind,
                           ExtraData extraData) const;

    bool hasExtraData() const {
      switch (ExtraDataKind) {
      case ExtraData::None:
        return false;
      case ExtraData::Metatype:
      case ExtraData::Retainable:
        return true;
      }
    }
    
    unsigned getExplosionSize(ExplosionKind kind) const {
      return hasExtraData() ? 2 : 1;
    }

    void getSchema(ExplosionSchema &schema) const {
      llvm::Type *storageTy = getStorageType();
      llvm::StructType *structTy = dyn_cast<llvm::StructType>(storageTy);
      
      if (structTy) {
        assert(structTy->getNumElements() == 2);
        schema.add(ExplosionSchema::Element::forScalar(structTy->getElementType(0)));
        schema.add(ExplosionSchema::Element::forScalar(structTy->getElementType(1)));
      } else {
        schema.add(ExplosionSchema::Element::forScalar(storageTy));
      }
    }

    Address projectFunction(IRGenFunction &IGF, Address address) const {
      if (hasExtraData()) {
        return IGF.Builder.CreateStructGEP(address, 0, Size(0),
                                           address->getName() + ".fn");
      }
      return address;
    }

    Address projectData(IRGenFunction &IGF, Address address) const {
      assert(hasExtraData() && "no data");
      return IGF.Builder.CreateStructGEP(address, 1, IGF.IGM.getPointerSize(),
                                         address->getName() + ".data");
    }

    void loadAsCopy(IRGenFunction &IGF, Address address, Explosion &e) const {
      // Load the function.
      Address fnAddr = projectFunction(IGF, address);
      e.add(IGF.Builder.CreateLoad(fnAddr, fnAddr->getName()+".load"));

      // Load the data, if any.
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable: {
        Address dataAddr = projectData(IGF, address);
        IGF.emitLoadAndRetain(dataAddr, e);
        break;
      }
      case ExtraData::Metatype: {
        Address dataAddr = projectData(IGF, address);
        e.add(IGF.Builder.CreateLoad(dataAddr));
        break;
      }
      }
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // Load the function.
      Address fnAddr = projectFunction(IGF, addr);
      e.add(IGF.Builder.CreateLoad(fnAddr));

      // Load the data, if any.
      if (hasExtraData()) {
        Address dataAddr = projectData(IGF, addr);
        e.add(IGF.Builder.CreateLoad(dataAddr));
      }
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the function pointer.
      Address fnAddr = projectFunction(IGF, address);
      IGF.Builder.CreateStore(e.claimNext(), fnAddr);

      // Store the data pointer, if any.
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable: {
        Address dataAddr = projectData(IGF, address);
        IGF.emitAssignRetained(e.claimNext(), dataAddr);
        break;
      }
      case ExtraData::Metatype: {
        Address dataAddr = projectData(IGF, address);
        IGF.Builder.CreateStore(e.claimNext(), dataAddr);
        break;
      }
      }
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the function pointer.
      Address fnAddr = projectFunction(IGF, address);
      IGF.Builder.CreateStore(e.claimNext(), fnAddr);

      // Store the data pointer, if any, transferring the +1.
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable: {
        Address dataAddr = projectData(IGF, address);
        IGF.emitInitializeRetained(e.claimNext(), dataAddr);
        break;
      }
      case ExtraData::Metatype: {
        Address dataAddr = projectData(IGF, address);
        IGF.Builder.CreateStore(e.claimNext(), dataAddr);
        break;
      }
      }
    }

    void copy(IRGenFunction &IGF, Explosion &src,
              Explosion &dest) const override {
      src.transferInto(dest, 1);
      
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable:
        IGF.emitRetain(src.claimNext(), dest);
        break;
      case ExtraData::Metatype:
        src.transferInto(dest, 1);
        break;
      }
    }
    
    void consume(IRGenFunction &IGF, Explosion &src) const override {
      src.claimNext();
      
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable:
        IGF.emitRelease(src.claimNext());
        break;
      case ExtraData::Metatype:
        src.claimNext();
        break;
      }
    }

    void retain(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable:
        IGF.emitRetainCall(e.claimNext());
        break;
      case ExtraData::Metatype:
        e.claimNext();
        break;
      }
    }
    
    void release(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable:
        IGF.emitRelease(e.claimNext());
        break;
      case ExtraData::Metatype:
        e.claimNext();
        break;
      }
    }

    void retainUnowned(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable:
        IGF.emitRetainUnowned(e.claimNext());
        break;
      case ExtraData::Metatype:
        e.claimNext();
        break;
      }
    }
    
    void unownedRetain(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable:
        IGF.emitUnownedRetain(e.claimNext());
        break;
      case ExtraData::Metatype:
        e.claimNext();
        break;
      }
    }

    void unownedRelease(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable:
        IGF.emitUnownedRelease(e.claimNext());
        break;
      case ExtraData::Metatype:
        e.claimNext();
        break;
      }
    }
    
    void destroy(IRGenFunction &IGF, Address addr) const {
      switch (ExtraDataKind) {
      case ExtraData::None:
        break;
      case ExtraData::Retainable:
        IGF.emitRelease(IGF.Builder.CreateLoad(projectData(IGF, addr)));
        break;
      case ExtraData::Metatype:
        break;
      }
    }
    
    llvm::Value *packEnumPayload(IRGenFunction &IGF,
                                  Explosion &src,
                                  unsigned bitWidth,
                                  unsigned offset) const override {
      PackEnumPayload pack(IGF, bitWidth);
      pack.addAtOffset(src.claimNext(), offset);
      if (hasExtraData())
        pack.add(src.claimNext());
      return pack.get();
    }
    
    void unpackEnumPayload(IRGenFunction &IGF,
                            llvm::Value *payload,
                            Explosion &dest,
                            unsigned offset) const override {
      UnpackEnumPayload unpack(IGF, payload);
      auto storageTy = getStorageType();
      if (hasExtraData()) {
        auto structTy = cast<llvm::StructType>(storageTy);
        dest.add(unpack.claimAtOffset(structTy->getElementType(0),
                                      offset));
        dest.add(unpack.claim(structTy->getElementType(1)));
      } else {
        dest.add(unpack.claimAtOffset(storageTy, offset));
      }
    }
  };

  /// The type-info class for ObjC blocks, which are represented by an ObjC
  /// heap pointer.
  class BlockTypeInfo : public HeapTypeInfo<BlockTypeInfo> {
  public:
    BlockTypeInfo(llvm::PointerType *storageType,
                  Size size, llvm::BitVector spareBits, Alignment align)
      : HeapTypeInfo(storageType, size, spareBits, align) {
    }

    bool hasSwiftRefcount() const { return false; }
  };
}

const TypeInfo *TypeConverter::convertFunctionType(SILFunctionType *T) {
  if (T->isBlock())
    return new BlockTypeInfo(IGM.ObjCPtrTy,
                             IGM.getPointerSize(),
                             IGM.getHeapObjectSpareBits(),
                             IGM.getPointerAlignment());
  
  CanSILFunctionType ct(T);
  auto extraDataKind = getExtraDataKind(IGM, ct);
  llvm::Type *ty;
  switch (extraDataKind) {
  case ExtraData::None:
    ty = IGM.FunctionPtrTy;
    break;
  case ExtraData::Retainable:
    ty = IGM.FunctionPairTy;
    break;
  case ExtraData::Metatype:
    ty = IGM.WitnessFunctionPairTy;
    break;
  }
  
  return FuncTypeInfo::create(ct, ty,
                              IGM.getPointerSize() * 2,
                              IGM.getPointerAlignment(),
                              extraDataKind);
}

void irgen::addIndirectReturnAttributes(IRGenModule &IGM,
                                        llvm::AttributeSet &attrs) {
  static const llvm::Attribute::AttrKind attrKinds[] = {
    llvm::Attribute::StructRet,
    llvm::Attribute::NoAlias
  };
  auto resultAttrs = llvm::AttributeSet::get(IGM.LLVMContext, 1, attrKinds);
  attrs = attrs.addAttributes(IGM.LLVMContext, 1, resultAttrs);
}

static void addNoAliasAttribute(IRGenModule &IGM,
                                llvm::AttributeSet &attrs,
                                unsigned argIndex) {
  static const llvm::Attribute::AttrKind attrKinds[] = {
    llvm::Attribute::NoAlias
  };
  auto resultAttrs = llvm::AttributeSet::get(IGM.LLVMContext, argIndex+1,
                                             attrKinds);
  attrs = attrs.addAttributes(IGM.LLVMContext, argIndex+1, resultAttrs);
}

void irgen::addByvalArgumentAttributes(IRGenModule &IGM,
                                       llvm::AttributeSet &attrs,
                                       unsigned argIndex,
                                       Alignment align) {
  llvm::AttrBuilder b;
  b.addAttribute(llvm::Attribute::ByVal);
  b.addAttribute(llvm::Attribute::getWithAlignment(IGM.LLVMContext,
                                                   align.getValue()));
  auto resultAttrs = llvm::AttributeSet::get(IGM.LLVMContext, argIndex+1, b);
  attrs = attrs.addAttributes(IGM.LLVMContext,
                              argIndex+1,
                              resultAttrs);
}

namespace {
  class SignatureExpansion {
    IRGenModule &IGM;
    CanSILFunctionType FnType;
    ExplosionKind ExplosionLevel;
  public:
    SmallVector<llvm::Type*, 8> ParamIRTypes;
    llvm::AttributeSet Attrs;
    bool HasIndirectResult = false;

    SignatureExpansion(IRGenModule &IGM, CanSILFunctionType fnType,
                       ExplosionKind explosionLevel)
      : IGM(IGM), FnType(fnType), ExplosionLevel(explosionLevel) {}

    llvm::Type *expandResult();
    void expandParameters();

  private:
    void expand(SILParameterInfo param);
    llvm::Type *addIndirectResult();

    unsigned getCurParamIndex() {
      return ParamIRTypes.size();
    }

    /// Add a pointer to the given type as the next parameter.
    void addPointerParameter(llvm::Type *storageType) {
      ParamIRTypes.push_back(storageType->getPointerTo());
    }
  };
}

/// Given a Swift type, attempt to return an appropriate Clang
/// CanQualType for the purpose of generating correct code for the
/// ABI.
static clang::CanQualType getClangABIType(CanType swiftType) {
  // FIXME: ProtocolType generates a struct and might need special
  //        handling here for some platforms.
  if (auto structType = swiftType->getAs<StructType>())
    if (auto *clangDecl = structType->getDecl()->getClangDecl()) {
      auto *typeDecl = cast<clang::TypeDecl>(clangDecl);
      return typeDecl->getTypeForDecl()->getCanonicalTypeUnqualified();
    }

  // FIXME: For parameters, we need to be able to generate a Clang
  // type for all Swift types.
  return clang::CanQualType();
}

llvm::Type *SignatureExpansion::addIndirectResult() {
  auto resultType = FnType->getResult().getSILType();
  const TypeInfo &resultTI = IGM.getTypeInfo(resultType);
  addPointerParameter(resultTI.getStorageType());
  addIndirectReturnAttributes(IGM, Attrs);
  return IGM.VoidTy;
}

llvm::Type *SignatureExpansion::expandResult() {
  // Handle the direct result type, checking for supposedly scalar
  // result types that we actually want to return indirectly.
  auto resultType = FnType->getResult().getSILType();

  // Fast-path the empty tuple type.
  if (auto tuple = resultType.getAs<TupleType>())
    if (tuple->getNumElements() == 0)
      return IGM.VoidTy;

  ExplosionSchema schema = IGM.getSchema(resultType, ExplosionLevel);
  switch (FnType->getAbstractCC()) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod: {
    if (requiresExternalIndirectResult(IGM, FnType, ExplosionLevel))
      return addIndirectResult();

    auto clangType = getClangABIType(resultType.getSwiftRValueType());

    // Fall back on native Swift type lowering for things that we
    // cannot generate a Clang type from.
    if (!clangType)
      return schema.getScalarResultType(IGM);

    auto &ABITypes = *IGM.ABITypes;
    SmallVector<clang::CanQualType,1> args;

    auto extInfo = clang::FunctionType::ExtInfo();
    auto &FI = ABITypes.arrangeLLVMFunctionInfo(clangType, args, extInfo,
                                             clang::CodeGen::RequiredArgs::All);

    auto &returnInfo = FI.getReturnInfo();
    return returnInfo.getCoerceToType();
  }
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod: {
    if (schema.requiresIndirectResult(IGM))
      return addIndirectResult();
    return schema.getScalarResultType(IGM);
  }
  }
}

void SignatureExpansion::expand(SILParameterInfo param) {
  switch (param.getConvention()) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_Out:
    if (param.isIndirectResult()) {
      assert(ParamIRTypes.empty());
      addIndirectReturnAttributes(IGM, Attrs);
      HasIndirectResult = true;
    } else {
      addNoAliasAttribute(IGM, Attrs, getCurParamIndex());
    }
    addPointerParameter(IGM.getStorageType(param.getSILType()));
    return;

  case ParameterConvention::Direct_Owned:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
    // Go ahead and further decompose tuples.
    if (auto tuple = dyn_cast<TupleType>(param.getType())) {
      for (auto elt : tuple.getElementTypes()) {
        // Propagate the same ownedness down to the element.
        expand(SILParameterInfo(elt, param.getConvention()));
      }
      return;
    }

    switch (FnType->getAbstractCC()) {
    case AbstractCC::C:
    case AbstractCC::ObjCMethod:
      if (requiresExternalByvalArgument(IGM, param.getSILType())) {
        auto &paramTI = cast<FixedTypeInfo>(IGM.getTypeInfo(param.getSILType()));
        addByvalArgumentAttributes(IGM, Attrs, getCurParamIndex(),
                                   paramTI.getFixedAlignment());
        addPointerParameter(paramTI.getStorageType());
        return;
      }
      SWIFT_FALLTHROUGH;
    case AbstractCC::Freestanding:
    case AbstractCC::Method:
    case AbstractCC::WitnessMethod: {
      auto schema = IGM.getSchema(param.getSILType(), ExplosionLevel);
      schema.addToArgTypes(IGM, ParamIRTypes);
      return;
    }
    }
    llvm_unreachable("bad abstract CC");
  }
  llvm_unreachable("bad parameter convention");
}

/// Expand the abstract parameters of a SIL function type into the
/// physical parameters of an LLVM function type.
void SignatureExpansion::expandParameters() {
  auto params = FnType->getParameters();

  // Some CCs secretly rearrange the parameters.
  switch (FnType->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
  case AbstractCC::C:
    break;

  case AbstractCC::ObjCMethod: {
    // ObjC methods take their 'self' argument first, followed by an
    // implicit _cmd argument.
    expand(params.back());
    ParamIRTypes.push_back(IGM.Int8PtrTy);
    params = params.slice(0, params.size() - 1);
    break;
  }
  }

  for (auto param : params) {
    expand(param);
  }

  if (hasPolymorphicParameters(FnType))
    expandPolymorphicSignature(IGM, FnType, ParamIRTypes);
}

Signature FuncTypeInfo::getSignature(IRGenModule &IGM,
                                     ExplosionKind explosionLevel,
                                     ExtraData extraData) const {
  // Compute a reference to the appropriate signature cache.
  Signature &signature = TheSignatures.select(explosionLevel, extraData);

  // If it's already been filled in, we're done.
  if (signature.isValid())
    return signature;

  SignatureExpansion expansion(IGM, FormalType, explosionLevel);
  llvm::Type *resultType = expansion.expandResult();
  expansion.expandParameters();

  // Data arguments are last.
  // See the comment in this file's header comment.
  switch (extraData) {
  case ExtraData::None: break;
  case ExtraData::Retainable:
    expansion.ParamIRTypes.push_back(IGM.RefCountedPtrTy);
    break;
  case ExtraData::Metatype:
    expansion.ParamIRTypes.push_back(IGM.TypeMetadataPtrTy);
    break;
  }

  // Create the appropriate LLVM type.
  llvm::FunctionType *llvmType =
    llvm::FunctionType::get(resultType, expansion.ParamIRTypes,
                            /*variadic*/ false);

  // Update the cache and return.
  signature.set(llvmType, expansion.HasIndirectResult, expansion.Attrs);
  return signature;
}

llvm::FunctionType *
IRGenModule::getFunctionType(CanSILFunctionType type,
                             ExplosionKind explosionKind,
                             ExtraData extraData,
                             llvm::AttributeSet &attrs) {
  auto &fnTypeInfo = getTypeInfoForLowered(type).as<FuncTypeInfo>();
  Signature sig = fnTypeInfo.getSignature(*this, explosionKind, extraData);
  attrs = sig.getAttributes();
  return sig.getType();
}

static bool isClassMethod(ValueDecl *vd) {
  if (!vd->getDeclContext())
    return false;
  if (!vd->getDeclContext()->getDeclaredTypeInContext())
    return false;
  return vd->getDeclContext()->getDeclaredTypeInContext()
  ->getClassOrBoundGenericClass();
}

AbstractCC irgen::getAbstractCC(ValueDecl *fn) {
  if (fn->isInstanceMember())
    return AbstractCC::Method;
  if (fn->hasClangNode()) {
    if (isClassMethod(fn))
      return AbstractCC::ObjCMethod;
    return AbstractCC::C;
  }
  return AbstractCC::Freestanding;
}

static AbstractCallee getAbstractDirectCallee(ValueDecl *val,
                                              ExplosionKind level,
                                              ExtraData extraData) {
  unsigned minUncurry = 0;
  if (val->getDeclContext()->isTypeContext())
    minUncurry = 1;
  unsigned maxUncurry = getDeclNaturalUncurryLevel(val);
  
  AbstractCC convention = getAbstractCC(val);

  return AbstractCallee(convention, level, minUncurry, maxUncurry, extraData);
}

/// Construct the best known limits on how we can call the given
/// global function.
AbstractCallee AbstractCallee::forDirectGlobalFunction(IRGenModule &IGM,
                                                       ValueDecl *val) {
  assert(!val->getDeclContext()->isLocalContext());

  // FIXME: be more aggressive about this.
  ExplosionKind level = ExplosionKind::Minimal;

  return getAbstractDirectCallee(val, level, ExtraData::None);
}

/// Return this function pointer, bitcasted to an i8*.
llvm::Value *Callee::getOpaqueFunctionPointer(IRGenFunction &IGF) const {
  if (FnPtr->getType() == IGF.IGM.Int8PtrTy)
    return FnPtr;
  return IGF.Builder.CreateBitCast(FnPtr, IGF.IGM.Int8PtrTy);
}

/// Return this data pointer.
llvm::Value *Callee::getDataPointer(IRGenFunction &IGF) const {
  if (hasDataPointer()) return DataPtr;
  return IGF.IGM.RefCountedNull;
}

static void extractScalarResults(IRGenFunction &IGF, llvm::Type *bodyType,
                                  llvm::Value *call, Explosion &out) {
  assert(!bodyType->isVoidTy() && "Unexpected void result type!");

  auto *returned = call;
  auto *callType = call->getType();

  // If the type of the result of the call differs from the type used
  // elsewhere in the caller due to ABI type coercion, we need to
  // coerce the result back from the ABI type before extracting the
  // elements.
  if (bodyType != callType) {
    assert(!callType->isVoidTy() && "Unexpected void type in call result!");

    // If both are pointers, we can simply insert a bitcast, otherwise
    // we need to store, bitcast, and load.
    if (bodyType->isPointerTy() && callType->isPointerTy()) {
      returned = IGF.Builder.CreateBitCast(call, bodyType);
    } else {
      auto address = IGF.createAlloca(callType, Alignment(0),
                                      "call.coerced");
      IGF.Builder.CreateStore(call, address.getAddress());
      auto *coerced = IGF.Builder.CreateBitCast(address.getAddress(),
                                                bodyType->getPointerTo());
      returned = IGF.Builder.CreateLoad(coerced);
    }
  }

  if (llvm::StructType *structType = dyn_cast<llvm::StructType>(bodyType))
    for (unsigned i = 0, e = structType->getNumElements(); i != e; ++i)
      out.add(IGF.Builder.CreateExtractValue(returned, i));
  else
    out.add(returned);
}

static void emitCastBuiltin(IRGenFunction &IGF, CanSILFunctionType substFnType,
                            Explosion &result,
                            Explosion &args,
                            llvm::Instruction::CastOps opcode) {
  llvm::Value *input = args.claimNext();
  assert(args.empty() && "wrong operands to cast operation");

  assert(substFnType->getResult().getConvention() == ResultConvention::Unowned);
  SILType destType = substFnType->getResult().getSILType();
  llvm::Type *destTy = IGF.IGM.getStorageType(destType);
  llvm::Value *output = IGF.Builder.CreateCast(opcode, input, destTy);
  result.add(output);
}

static void emitCastOrBitCastBuiltin(IRGenFunction &IGF,
                                     CanSILFunctionType substFnType,
                                     Explosion &result,
                                     Explosion &args,
                                     BuiltinValueKind BV) {
  llvm::Value *input = args.claimNext();
  assert(args.empty() && "wrong operands to cast operation");

  assert(substFnType->getResult().getConvention() ==
           ResultConvention::Unowned);
  SILType destType = substFnType->getResult().getSILType();
  llvm::Type *destTy = IGF.IGM.getStorageType(destType);
  llvm::Value *output;
  switch (BV) {
  default: llvm_unreachable("Not a cast-or-bitcast operation");
  case BuiltinValueKind::TruncOrBitCast:
    output = IGF.Builder.CreateTruncOrBitCast(input, destTy); break;
  case BuiltinValueKind::ZExtOrBitCast:
    output = IGF.Builder.CreateZExtOrBitCast(input, destTy); break;
  case BuiltinValueKind::SExtOrBitCast:
    output = IGF.Builder.CreateSExtOrBitCast(input, destTy); break;
  }
  result.add(output);
}

static void emitCompareBuiltin(IRGenFunction &IGF, Explosion &result,
                               Explosion &args, llvm::CmpInst::Predicate pred) {
  llvm::Value *lhs = args.claimNext();
  llvm::Value *rhs = args.claimNext();
  
  llvm::Value *v;
  if (lhs->getType()->isFPOrFPVectorTy())
    v = IGF.Builder.CreateFCmp(pred, lhs, rhs);
  else
    v = IGF.Builder.CreateICmp(pred, lhs, rhs);
  
  result.add(v);
}

/// decodeLLVMAtomicOrdering - turn a string like "release" into the LLVM enum.
static llvm::AtomicOrdering decodeLLVMAtomicOrdering(StringRef O) {
  using namespace llvm;
  return StringSwitch<AtomicOrdering>(O)
    .Case("unordered", Unordered)
    .Case("monotonic", Monotonic)
    .Case("acquire", Acquire)
    .Case("release", Release)
    .Case("acqrel", AcquireRelease)
    .Case("seqcst", SequentiallyConsistent);
}

/// emitBuiltinCall - Emit a call to a builtin function.
void irgen::emitBuiltinCall(IRGenFunction &IGF, Identifier FnId,
                            CanSILFunctionType substFnType,
                            Explosion &args, Explosion *out,
                            Address indirectOut,
                            ArrayRef<Substitution> substitutions) {
  assert(((out != nullptr) ^ indirectOut.isValid()) &&
         "cannot emit builtin to both explosion and memory");

  // Decompose the function's name into a builtin name and type list.
  const BuiltinInfo &Builtin = IGF.IGM.SILMod->getBuiltinInfo(FnId);

  // These builtins don't care about their argument:
  if (Builtin.ID == BuiltinValueKind::Sizeof) {
    args.claimAll();
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.getTypeInfoForUnlowered(valueTy);
    out->add(valueTI.getSize(IGF));
    return;
  }

  if (Builtin.ID == BuiltinValueKind::Strideof) {
    args.claimAll();
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.getTypeInfoForUnlowered(valueTy);
    out->add(valueTI.getStride(IGF));
    return;
  }

  if (Builtin.ID == BuiltinValueKind::Alignof) {
    args.claimAll();
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.getTypeInfoForUnlowered(valueTy);
    // The alignof value is one greater than the alignment mask.
    out->add(IGF.Builder.CreateAdd(valueTI.getAlignmentMask(IGF),
                                   IGF.IGM.getSize(Size(1))));
    return;
  }
  
  // addressof expects an lvalue argument.
  if (Builtin.ID == BuiltinValueKind::AddressOf) {
    llvm::Value *address = args.claimNext();
    llvm::Value *value = IGF.Builder.CreateBitCast(address,
                                                   IGF.IGM.Int8PtrTy);
    out->add(value);
    return;
  }

  // Everything else cares about the (rvalue) argument.

  // If this is an LLVM IR intrinsic, lower it to an intrinsic call.
  const IntrinsicInfo &IInfo = IGF.IGM.SILMod->getIntrinsicInfo(FnId);
  llvm::Intrinsic::ID IID = IInfo.ID;
  if (IID != llvm::Intrinsic::not_intrinsic) {
    SmallVector<llvm::Type*, 4> ArgTys;
    for (auto T : IInfo.Types)
      ArgTys.push_back(IGF.IGM.getStorageTypeForLowered(T->getCanonicalType()));
      
    auto F = llvm::Intrinsic::getDeclaration(&IGF.IGM.Module,
                                             (llvm::Intrinsic::ID)IID, ArgTys);
    llvm::FunctionType *FT = F->getFunctionType();
    SmallVector<llvm::Value*, 8> IRArgs;
    for (unsigned i = 0, e = FT->getNumParams(); i != e; ++i)
      IRArgs.push_back(args.claimNext());
    llvm::Value *TheCall = IGF.Builder.CreateCall(F, IRArgs);

    if (!TheCall->getType()->isVoidTy())
      extractScalarResults(IGF, TheCall->getType(), TheCall, *out);

    return;
  }

  // TODO: A linear series of ifs is suboptimal.
#define BUILTIN_SIL_OPERATION(id, name, overload) \
  if (Builtin.ID == BuiltinValueKind::id) \
    llvm_unreachable(name " builtin should be lowered away by SILGen!");

#define BUILTIN_CAST_OPERATION(id, name, attrs) \
  if (Builtin.ID == BuiltinValueKind::id) \
    return emitCastBuiltin(IGF, substFnType, *out, args, \
                           llvm::Instruction::id);

#define BUILTIN_CAST_OR_BITCAST_OPERATION(id, name, attrs) \
  if (Builtin.ID == BuiltinValueKind::id) \
    return emitCastOrBitCastBuiltin(IGF, substFnType, *out, args, \
                                    BuiltinValueKind::id);
  
#define BUILTIN_BINARY_OPERATION(id, name, attrs, overload) \
  if (Builtin.ID == BuiltinValueKind::id) { \
    llvm::Value *lhs = args.claimNext(); \
    llvm::Value *rhs = args.claimNext(); \
    llvm::Value *v = IGF.Builder.Create##id(lhs, rhs); \
    return out->add(v); \
  }

#define BUILTIN_BINARY_OPERATION_WITH_OVERFLOW(id, name, attrs, overload) \
if (Builtin.ID == BuiltinValueKind::id) { \
  SmallVector<llvm::Type*, 2> ArgTys; \
  auto opType = Builtin.Types[0]->getCanonicalType(); \
  ArgTys.push_back(IGF.IGM.getStorageTypeForLowered(opType)); \
  auto F = llvm::Intrinsic::getDeclaration(&IGF.IGM.Module, \
    getLLVMIntrinsicIDForBuiltinWithOverflow(Builtin.ID), ArgTys); \
  SmallVector<llvm::Value*, 2> IRArgs; \
  IRArgs.push_back(args.claimNext()); \
  IRArgs.push_back(args.claimNext()); \
  args.claimNext();\
  llvm::Value *TheCall = IGF.Builder.CreateCall(F, IRArgs); \
  extractScalarResults(IGF, TheCall->getType(), TheCall, *out);  \
  return; \
}
  // FIXME: We could generate the code to dynamically report the overflow if the
  // thrid argument is true. Now, we just ignore it.

#define BUILTIN_BINARY_PREDICATE(id, name, attrs, overload) \
  if (Builtin.ID == BuiltinValueKind::id) \
    return emitCompareBuiltin(IGF, *out, args, llvm::CmpInst::id);
#define BUILTIN(ID, Name, Attrs)  // Ignore the rest.
#include "swift/AST/Builtins.def"

  if (Builtin.ID == BuiltinValueKind::FNeg) {
    llvm::Value *rhs = args.claimNext();
    llvm::Value *lhs = llvm::ConstantFP::get(rhs->getType(), "-0.0");
    llvm::Value *v = IGF.Builder.CreateFSub(lhs, rhs);
    return out->add(v);
  }
  
  if (Builtin.ID == BuiltinValueKind::AllocRaw) {
    auto size = args.claimNext();
    auto align = args.claimNext();
    // Translate the alignment to a mask.
    auto alignMask = IGF.Builder.CreateSub(align, IGF.IGM.getSize(Size(1)));
    auto alloc = IGF.emitAllocRawCall(size, alignMask, "builtin-allocRaw");
    out->add(alloc);
    return;
  }

  if (Builtin.ID == BuiltinValueKind::DeallocRaw) {
    auto pointer = args.claimNext();
    auto size = args.claimNext();
    IGF.emitDeallocRawCall(pointer, size);
    return;
  }

  if (Builtin.ID == BuiltinValueKind::Fence) {
    SmallVector<Type, 4> Types;
    StringRef BuiltinName =
      getBuiltinBaseName(IGF.IGM.Context, FnId.str(), Types);
    BuiltinName = BuiltinName.drop_front(strlen("fence_"));
    // Decode the ordering argument, which is required.
    auto underscore = BuiltinName.find('_');
    auto ordering = decodeLLVMAtomicOrdering(BuiltinName.substr(0, underscore));
    BuiltinName = BuiltinName.substr(underscore);
    
    // Accept singlethread if present.
    bool isSingleThread = BuiltinName.startswith("_singlethread");
    if (isSingleThread)
      BuiltinName = BuiltinName.drop_front(strlen("_singlethread"));
    assert(BuiltinName.empty() && "Mismatch with sema");
    
    IGF.Builder.CreateFence(ordering,
                      isSingleThread ? llvm::SingleThread : llvm::CrossThread);
    return;
  }

  
  if (Builtin.ID == BuiltinValueKind::CmpXChg) {
    SmallVector<Type, 4> Types;
    StringRef BuiltinName =
      getBuiltinBaseName(IGF.IGM.Context, FnId.str(), Types);
    BuiltinName = BuiltinName.drop_front(strlen("cmpxchg_"));
    // Decode the ordering argument, which is required.
    auto underscore = BuiltinName.find('_');
    auto ordering = decodeLLVMAtomicOrdering(BuiltinName.substr(0, underscore));
    BuiltinName = BuiltinName.substr(underscore);
    
    // Accept volatile and singlethread if present.
    bool isVolatile = BuiltinName.startswith("_volatile");
    if (isVolatile) BuiltinName = BuiltinName.drop_front(strlen("_volatile"));
    
    bool isSingleThread = BuiltinName.startswith("_singlethread");
    if (isSingleThread)
      BuiltinName = BuiltinName.drop_front(strlen("_singlethread"));
    assert(BuiltinName.empty() && "Mismatch with sema");

    auto pointer = args.claimNext();
    auto cmp = args.claimNext();
    auto newval = args.claimNext();

    llvm::Type *origTy = cmp->getType();
    if (origTy->isPointerTy()) {
      cmp = IGF.Builder.CreatePtrToInt(cmp, IGF.IGM.IntPtrTy);
      newval = IGF.Builder.CreatePtrToInt(newval, IGF.IGM.IntPtrTy);
    }

    pointer = IGF.Builder.CreateBitCast(pointer,
                                  llvm::PointerType::getUnqual(cmp->getType()));
    llvm::Value *value = IGF.Builder.CreateAtomicCmpXchg(pointer, cmp, newval,
                                                          ordering,
                      isSingleThread ? llvm::SingleThread : llvm::CrossThread);
    cast<llvm::AtomicCmpXchgInst>(value)->setVolatile(isVolatile);

    if (origTy->isPointerTy())
      value = IGF.Builder.CreateIntToPtr(value, origTy);

    out->add(value);
    return;
  }
  
  if (Builtin.ID == BuiltinValueKind::AtomicRMW) {
    using namespace llvm;

    SmallVector<Type, 4> Types;
    StringRef BuiltinName = getBuiltinBaseName(IGF.IGM.Context,
                                               FnId.str(), Types);
    BuiltinName = BuiltinName.drop_front(strlen("atomicrmw_"));
    auto underscore = BuiltinName.find('_');
    StringRef SubOp = BuiltinName.substr(0, underscore);
    
    auto SubOpcode = StringSwitch<AtomicRMWInst::BinOp>(SubOp)
      .Case("xchg", AtomicRMWInst::Xchg)
      .Case("add",  AtomicRMWInst::Add)
      .Case("sub",  AtomicRMWInst::Sub)
      .Case("and",  AtomicRMWInst::And)
      .Case("nand", AtomicRMWInst::Nand)
      .Case("or",   AtomicRMWInst::Or)
      .Case("xor",  AtomicRMWInst::Xor)
      .Case("max",  AtomicRMWInst::Max)
      .Case("min",  AtomicRMWInst::Min)
      .Case("umax", AtomicRMWInst::UMax)
      .Case("umin", AtomicRMWInst::UMin);
    BuiltinName = BuiltinName.drop_front(underscore+1);
    
    // Decode the ordering argument, which is required.
    underscore = BuiltinName.find('_');
    auto ordering = decodeLLVMAtomicOrdering(BuiltinName.substr(0, underscore));
    BuiltinName = BuiltinName.substr(underscore);
    
    // Accept volatile and singlethread if present.
    bool isVolatile = BuiltinName.startswith("_volatile");
    if (isVolatile) BuiltinName = BuiltinName.drop_front(strlen("_volatile"));
    
    bool isSingleThread = BuiltinName.startswith("_singlethread");
    if (isSingleThread)
      BuiltinName = BuiltinName.drop_front(strlen("_singlethread"));
    assert(BuiltinName.empty() && "Mismatch with sema");
    
    auto pointer = args.claimNext();
    auto val = args.claimNext();

    // Handle atomic ops on pointers by casting to intptr_t.
    llvm::Type *origTy = val->getType();
    if (origTy->isPointerTy())
      val = IGF.Builder.CreatePtrToInt(val, IGF.IGM.IntPtrTy);

    pointer = IGF.Builder.CreateBitCast(pointer,
                                  llvm::PointerType::getUnqual(val->getType()));
    llvm::Value *value = IGF.Builder.CreateAtomicRMW(SubOpcode, pointer, val,
                                                      ordering,
                      isSingleThread ? llvm::SingleThread : llvm::CrossThread);
    cast<AtomicRMWInst>(value)->setVolatile(isVolatile);

    if (origTy->isPointerTy())
      value = IGF.Builder.CreateIntToPtr(value, origTy);

    out->add(value);
    return;
  }

  if (Builtin.ID == BuiltinValueKind::ExtractElement) {
    using namespace llvm;

    auto vector = args.claimNext();
    auto index = args.claimNext();
    out->add(IGF.Builder.CreateExtractElement(vector, index));
    return;
  }

  if (Builtin.ID == BuiltinValueKind::InsertElement) {
    using namespace llvm;

    auto vector = args.claimNext();
    auto newValue = args.claimNext();
    auto index = args.claimNext();
    out->add(IGF.Builder.CreateInsertElement(vector, newValue, index));
    return;
  }

  if (Builtin.ID == BuiltinValueKind::SToSCheckedTrunc ||
      Builtin.ID == BuiltinValueKind::UToUCheckedTrunc ||
      Builtin.ID == BuiltinValueKind::SToUCheckedTrunc) {
    auto FromTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[0]->getCanonicalType());
    auto ToTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[1]->getCanonicalType());

    // Compute the result for SToSCheckedTrunc_IntFrom_IntTo(Arg):
    //   Res = trunc_IntTo(Arg)
    //   Ext = sext_IntFrom(Res)
    //   OverflowFlag = (Arg == Ext) ? 0 : 1
    //   return (resultVal, OverflowFlag)
    //
    // Compute the result for UToUCheckedTrunc_IntFrom_IntTo(Arg)
    // and SToUCheckedTrunc_IntFrom_IntTo(Arg):
    //   Res = trunc_IntTo(Arg)
    //   Ext = zext_IntFrom(Res)
    //   OverflowFlag = (Arg == Ext) ? 0 : 1
    //   return (Res, OverflowFlag)
    llvm::Value *Arg = args.claimNext();
    llvm::Value *Res = IGF.Builder.CreateTrunc(Arg, ToTy);
    bool Signed = (Builtin.ID == BuiltinValueKind::SToSCheckedTrunc);
    llvm::Value *Ext = Signed ? IGF.Builder.CreateSExt(Res, FromTy) :
                                IGF.Builder.CreateZExt(Res, FromTy);
    llvm::Value *OverflowCond = IGF.Builder.CreateICmpEQ(Arg, Ext);
    llvm::Value *OverflowFlag = IGF.Builder.CreateSelect(OverflowCond,
                                  llvm::ConstantInt::get(IGF.IGM.Int1Ty, 0),
                                  llvm::ConstantInt::get(IGF.IGM.Int1Ty, 1));
    // Return the tuple - the result + the overflow flag.
    out->add(Res);
    return out->add(OverflowFlag);
  }

  if (Builtin.ID == BuiltinValueKind::UToSCheckedTrunc) {
    auto FromTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[0]->getCanonicalType());
    auto ToTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[1]->getCanonicalType());
    llvm::Type *ToMinusOneTy =
      llvm::Type::getIntNTy(ToTy->getContext(), ToTy->getIntegerBitWidth() - 1);

    // Compute the result for UToSCheckedTrunc_IntFrom_IntTo(Arg):
    //   Res = trunc_IntTo(Arg)
    //   Trunc = trunc_'IntTo-1bit'(Arg)
    //   Ext = zext_IntFrom(Trunc)
    //   OverflowFlag = (Arg == Ext) ? 0 : 1
    //   return (Res, OverflowFlag)
    llvm::Value *Arg = args.claimNext();
    llvm::Value *Res = IGF.Builder.CreateTrunc(Arg, ToTy);
    llvm::Value *Trunc = IGF.Builder.CreateTrunc(Arg, ToMinusOneTy);
    llvm::Value *Ext = IGF.Builder.CreateZExt(Trunc, FromTy);
    llvm::Value *OverflowCond = IGF.Builder.CreateICmpEQ(Arg, Ext);
    llvm::Value *OverflowFlag = IGF.Builder.CreateSelect(OverflowCond,
                                  llvm::ConstantInt::get(IGF.IGM.Int1Ty, 0),
                                  llvm::ConstantInt::get(IGF.IGM.Int1Ty, 1));
    // Return the tuple: (the result, the overflow flag).
    out->add(Res);
    return out->add(OverflowFlag);
  }

  if (Builtin.ID == BuiltinValueKind::SUCheckedConversion ||
      Builtin.ID == BuiltinValueKind::USCheckedConversion) {
    auto Ty =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[0]->getCanonicalType());

    // Report a sign error if the input parameter is a negative number, when
    // interpreted as signed.
    llvm::Value *Arg = args.claimNext();
    llvm::Value *Zero = llvm::ConstantInt::get(Ty, 0);
    llvm::Value *OverflowFlag = IGF.Builder.CreateICmpSLT(Arg, Zero);

    // Return the tuple: (the result (same as input), the overflow flag).
    out->add(Arg);
    return out->add(OverflowFlag);
  }

  // We are currently emiting code for '_convertFromBuiltinIntegerLiteral',
  // which will call the builtin and pass it a non-compile-time-const parameter.
  if (Builtin.ID == BuiltinValueKind::IntToFPWithOverflow) {
    auto TruncTy = IGF.IGM.Int32Ty;
    auto ToTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[1]->getCanonicalType());
    llvm::Value *Arg = args.claimNext();
    llvm::Value *Truncated = IGF.Builder.CreateTrunc(Arg, TruncTy);
    llvm::Value *V = IGF.Builder.CreateSIToFP(Truncated, ToTy);
    return out->add(V);
  }

  if (Builtin.ID == BuiltinValueKind::Once) {
    // The input type is statically (Builtin.RawPointer, () -> ()).
    llvm::Value *Pred = args.claimNext();
    // Cast the predicate to a OnceTy pointer.
    Pred = IGF.Builder.CreateBitCast(Pred, IGF.IGM.OnceTy->getPointerTo());
    llvm::Value *FnCode = args.claimNext();
    llvm::Value *FnContext = args.claimNext();
    
    auto call
      = IGF.Builder.CreateCall3(IGF.IGM.getOnceFn(), Pred, FnCode, FnContext);
    call->setCallingConv(IGF.IGM.RuntimeCC);
    // No return value.
    return;
  }
  
  llvm_unreachable("IRGen unimplemented for this builtin!");
}


/// Emit the unsubstituted result of this call into the given explosion.
/// The unsubstituted result must be naturally returned directly.
void CallEmission::emitToUnmappedExplosion(Explosion &out) {
  assert(LastArgWritten == 0 && "emitting unnaturally to explosion");
  assert(out.getKind() == getCallee().getExplosionLevel());

  auto call = emitCallSite(false);

  // Bail out immediately on a void result.
  llvm::Value *result = call.getInstruction();
  if (result->getType()->isVoidTy()) return;

  // Get the natural IR type in the body of the function that makes
  // the call. This may be different than the IR type returned by the
  // call itself due to ABI type coercion.
  auto resultType = getCallee().getOrigFunctionType()->getSILResult();
  auto &resultTI = IGF.IGM.getTypeInfo(resultType);
  auto schema = resultTI.getSchema(out.getKind());
  auto *bodyType = schema.getScalarResultType(IGF.IGM);

  // Extract out the scalar results.
  extractScalarResults(IGF, bodyType, result, out);
}

/// Emit the unsubstituted result of this call to the given address.
/// The unsubstituted result must be naturally returned indirectly.
void CallEmission::emitToUnmappedMemory(Address result) {
  assert(LastArgWritten == 1 && "emitting unnaturally to indirect result");

  Args[0] = result.getAddress();
  addIndirectReturnAttributes(IGF.IGM, Attrs);
#ifndef NDEBUG
  LastArgWritten = 0; // appease an assert
#endif
  
  emitCallSite(true);
}

// FIXME: This doesn't belong on IGF.
llvm::CallSite CallEmission::emitInvoke(llvm::CallingConv::ID convention,
                                        llvm::Value *fn,
                                        ArrayRef<llvm::Value*> args,
                                        const llvm::AttributeSet &attrs) {
  // TODO: exceptions!
  llvm::CallInst *call = IGF.Builder.CreateCall(fn, args);
  call->setAttributes(attrs);
  call->setCallingConv(convention);
  return call;
}

/// The private routine to ultimately emit a call or invoke instruction.
llvm::CallSite CallEmission::emitCallSite(bool hasIndirectResult) {
  assert(LastArgWritten == 0);
  assert(!EmittedCall);
  EmittedCall = true;

  // Determine the calling convention.
  // FIXME: collect attributes in the CallEmission.
  auto cc = expandAbstractCC(IGF.IGM, getCallee().getAbstractCC());

  // Make the call and clear the arguments array.
  auto fnPtr = getCallee().getFunctionPointer();  
  llvm::CallSite call = emitInvoke(cc, fnPtr, Args,
                                   llvm::AttributeSet::get(fnPtr->getContext(),
                                                           Attrs));
  Args.clear();

  // Return.
  return call;
}

enum class ResultDifference {
  /// The substituted result type is the same as the original result type.
  Identical,

  /// The substituted result type is a different formal type from, but
  /// has the same layout and interpretation as, the original result type.
  Aliasable,

  /// The substitued result type has the same layout as the original
  /// result type, but may differ in interpretation.
  // Reinterpretable,

  /// The substituted result type differs not just in interpretation,
  /// but in layout, from the original result type.
  Divergent
};

static ResultDifference computeResultDifference(IRGenModule &IGM,
                                                CanType origResultType,
                                                CanType substResultType) {
  if (origResultType == substResultType)
    return ResultDifference::Identical;

  if (differsByAbstractionInMemory(IGM, origResultType, substResultType))
    return ResultDifference::Divergent;

  return ResultDifference::Aliasable;
}

/// Emit the result of this call to memory.
void CallEmission::emitToMemory(Address addr, const TypeInfo &substResultTI) {
  assert(LastArgWritten <= 1);

  // If the call is naturally to an explosion, emit it that way and
  // then initialize the temporary.
  if (LastArgWritten == 0) {
    Explosion result(getCallee().getExplosionLevel());
    emitToExplosion(result);
    cast<LoadableTypeInfo>(substResultTI).initialize(IGF, result, addr);
    return;
  }

  // Okay, we're naturally emitting to memory.
  Address origAddr = addr;

  auto origFnType = CurCallee.getOrigFunctionType();
  auto substFnType = CurCallee.getSubstFunctionType();
  assert(origFnType->hasIndirectResult() == substFnType->hasIndirectResult());

  CanType origResultType, substResultType;
  if (origFnType->hasIndirectResult()) {
    origResultType = origFnType->getIndirectResult().getType();
    substResultType = substFnType->getIndirectResult().getType();
  } else {
    origResultType = origFnType->getResult().getType();
    substResultType = substFnType->getResult().getType();
  }

  // Figure out how the substituted result differs from the original.
  auto resultDiff =
    computeResultDifference(IGF.IGM, origResultType, substResultType);
  switch (resultDiff) {

  // For aliasable types, just bitcast the output address.
  case ResultDifference::Aliasable: {
    auto origTy = IGF.IGM.getStoragePointerTypeForLowered(origResultType);
    origAddr = IGF.Builder.CreateBitCast(origAddr, origTy);
    SWIFT_FALLTHROUGH;
  }

  case ResultDifference::Identical:
    emitToUnmappedMemory(origAddr);
    return;

  case ResultDifference::Divergent:
    // We need to do layout+allocation under substitution rules.
    return IGF.unimplemented(SourceLoc(), "divergent emission to memory");
  }
    
  llvm_unreachable("bad difference kind");
}

/// Emit the result of this call to an explosion.
void CallEmission::emitToExplosion(Explosion &out) {
  assert(LastArgWritten <= 1);

  CanType substResultType =
    getCallee().getSubstFunctionType()->getSemanticResultSILType()
               .getSwiftRValueType();

  auto &substResultTI =
    cast<LoadableTypeInfo>(IGF.getTypeInfoForLowered(substResultType));

  // If the call is naturally to memory, emit it that way and then
  // explode that temporary.
  if (LastArgWritten == 1) {
    // FIXME: we might still need to handle abstraction difference here?

    ContainedAddress ctemp = substResultTI.allocateStack(IGF, "call.aggresult");
    Address temp = ctemp.getAddress();
    emitToMemory(temp, substResultTI);
 
    // We can use a take.
    substResultTI.loadAsTake(IGF, temp, out);

    substResultTI.deallocateStack(IGF, ctemp.getContainer());
    return;
  }

  CanType origResultType =
    getCallee().getOrigFunctionType()->getResult().getType();

  // Okay, we're naturally emitting to an explosion.
  // Figure out how the substituted result differs from the original.
  auto resultDiff = computeResultDifference(IGF.IGM, origResultType,
                                            substResultType);

  switch (resultDiff) {
  // If they don't differ at all, we're good. 
  case ResultDifference::Identical:
  case ResultDifference::Aliasable:
    // We can emit directly if the explosion levels match.
    if (out.getKind() == getCallee().getExplosionLevel()) {
      emitToUnmappedExplosion(out);
    // Otherwise we have to re-explode.
    } else {
      Explosion temp(getCallee().getExplosionLevel());
      emitToUnmappedExplosion(temp);

      substResultTI.reexplode(IGF, temp, out);
    }
    return;

  // If they do differ, we need to remap.
  case ResultDifference::Divergent:
    if (isa<MetatypeType>(substResultType) &&
        isa<MetatypeType>(origResultType)) {
      // If we got here, it's because the substituted metatype is trivial.
      // Remapping is easy--the substituted type is empty, so we drop the
      // nontrivial representation of the original type.
      assert(cast<MetatypeType>(substResultType)->isThin()
             && "remapping to thick metatype?!");
      
      Explosion temp(getCallee().getExplosionLevel());
      emitToUnmappedExplosion(temp);
      temp.claimAll();
      return;
    }
      
    if (auto origArchetype = dyn_cast<ArchetypeType>(origResultType)) {
      if (origArchetype->requiresClass()) {
        // Remap a class archetype to an instance.
        assert(substResultType->getClassOrBoundGenericClass() &&
               "remapping class archetype to non-class?!");
        Explosion temp(getCallee().getExplosionLevel());
        emitToUnmappedExplosion(temp);
        llvm::Value *pointer = temp.claimNext();
        pointer = IGF.Builder.CreateBitCast(pointer,
                                            substResultTI.getStorageType());
        out.add(pointer);
        return;
      }
    }
      
    // There's a related FIXME in the Builtin.load/move code.
    IGF.unimplemented(SourceLoc(), "remapping explosion");
    IGF.emitFakeExplosion(substResultTI, out);
    return;
  }
    
  llvm_unreachable("bad difference kind");
}

CallEmission::CallEmission(CallEmission &&other)
  : IGF(other.IGF),
    Args(std::move(other.Args)),
    CurCallee(std::move(other.CurCallee)),
    LastArgWritten(other.LastArgWritten),
    EmittedCall(other.EmittedCall) {
  // Prevent other's destructor from asserting.
  other.invalidate();
}

CallEmission::~CallEmission() {
  assert(LastArgWritten == 0);
  assert(EmittedCall);
}

void CallEmission::invalidate() {
  LastArgWritten = 0;
  EmittedCall = true;
}


/// Set up this emitter afresh from the current callee specs.
void CallEmission::setFromCallee() {
  EmittedCall = false;

  unsigned numArgs = CurCallee.getLLVMFunctionType()->getNumParams();

  // Set up the args array.
  assert(Args.empty());
  Args.reserve(numArgs);
  Args.set_size(numArgs);
  LastArgWritten = numArgs;

  // Add the data pointer if we have one.
  if (CurCallee.hasDataPointer()) {
    assert(LastArgWritten > 0);
    Args[--LastArgWritten] = CurCallee.getDataPointer(IGF);
  }
}

/// Does an ObjC method or C function returning the given type require an
/// sret indirect result?
llvm::PointerType *
irgen::requiresExternalIndirectResult(IRGenModule &IGM,
                                      CanSILFunctionType fnType,
                                      ExplosionKind level) {
  if (fnType->hasIndirectResult()) {
    return IGM.getStoragePointerType(fnType->getIndirectResult().getSILType());
  }

  auto resultTy = fnType->getResult().getSILType();
  auto clangTy = getClangABIType(resultTy.getSwiftRValueType());

  // We are unable to produce an appropriate Clang type in some cases,
  // so fall back on the test used for native Swift types.
  if (!clangTy)
    return IGM.requiresIndirectResult(fnType->getResult().getSILType(), level);

  auto &ABITypes = *IGM.ABITypes;
  SmallVector<clang::CanQualType,1> args;

  auto extInfo = clang::FunctionType::ExtInfo();
  auto &FI = ABITypes.arrangeLLVMFunctionInfo(clangTy, args, extInfo,
                                             clang::CodeGen::RequiredArgs::All);

  auto &returnInfo = FI.getReturnInfo();
  if (!returnInfo.isIndirect())
    return nullptr;

  auto &ti = IGM.getTypeInfo(resultTy);
  return ti.getStorageType()->getPointerTo();
}

/// Does an argument of this type need to be passed by value on the stack to
/// C or ObjC arguments?
llvm::PointerType *irgen::requiresExternalByvalArgument(IRGenModule &IGM,
                                                        SILType type) {
  // FIXME: we need to consider the target's C calling convention.
  return IGM.requiresIndirectResult(type, ExplosionKind::Minimal);
}

static void externalizeArgument(IRGenFunction &IGF,
                                Explosion &in, Explosion &out,
                    SmallVectorImpl<std::pair<unsigned, Alignment>> &newByvals,
                                SILParameterInfo param) {
  auto ty = param.getSILType();
  auto &ti = cast<LoadableTypeInfo>(IGF.getTypeInfo(ty));
  if (requiresExternalByvalArgument(IGF.IGM, ty)) {
    // FIXME: deallocate temporary!
    Address addr = ti.allocateStack(IGF, "byval-temporary").getAddress();
    ti.initialize(IGF, in, addr);
     
    newByvals.push_back({out.size(), addr.getAlignment()});
    out.add(addr.getAddress());
  } else {
    ti.reexplode(IGF, in, out);
  }
}

static void externalizeArguments(IRGenFunction &IGF,
                                 Explosion &in, Explosion &out,
                    SmallVectorImpl<std::pair<unsigned, Alignment>> &newByvals,
                                 ArrayRef<SILParameterInfo> params) {
  for (auto param : params) {
    externalizeArgument(IGF, in, out, newByvals, param);
  }
}

/// Add a new set of arguments to the function.
void CallEmission::addArg(Explosion &arg) {
  SmallVector<std::pair<unsigned, Alignment>, 2> newByvals;

  auto origParams = getCallee().getOrigFunctionType()->getParameters();

  // Convert arguments to a representation appropriate to the calling
  // convention.
  AbstractCC cc = CurCallee.getAbstractCC();
  switch (cc) {
  case AbstractCC::C: {
    Explosion externalized(arg.getKind());
    externalizeArguments(IGF, arg, externalized, newByvals, origParams);
    arg = std::move(externalized);
    break;
  }
  case AbstractCC::ObjCMethod: {
    // The method will be uncurried to ((ArgsN...), ..., (Args1...),
    // Self). The self arg gets lowered to the first argument, and the
    // implicit _cmd argument goes in between it and the rest of the
    // args.
    Explosion externalized(arg.getKind());
    // self
    externalized.add(arg.claimNext());
    // _cmd
    externalized.add(arg.claimNext());
    // method args
    origParams = origParams.slice(0, origParams.size() - 1);
    externalizeArguments(IGF, arg, externalized, newByvals, origParams);
    arg = std::move(externalized);
    break;
  }
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    // Nothing to do.
    break;
  }

  // Add the given number of arguments.
  assert(getCallee().getExplosionLevel() == arg.getKind());
  assert(LastArgWritten >= arg.size());

  size_t targetIndex = LastArgWritten - arg.size();
  assert(targetIndex <= 1);
  LastArgWritten = targetIndex;
  
  // Add byval attributes.
  // FIXME: These should in theory be moved around with the arguments when
  // isLeftToRight, but luckily ObjC methods and C functions should only ever
  // have byvals in the last argument clause.
  // FIXME: these argument indexes are probably nonsense
  for (auto &byval : newByvals)
    addByvalArgumentAttributes(IGF.IGM, Attrs, byval.first+targetIndex,
                               byval.second);

  auto argIterator = Args.begin() + targetIndex;
  for (auto value : arg.claimAll()) {
    *argIterator++ = value;
  }
}

/// Initialize an Explosion with the parameters of the current
/// function.  All of the objects will be added unmanaged.  This is
/// really only useful when writing prologue code.
Explosion IRGenFunction::collectParameters(ExplosionKind explosionLevel) {
  Explosion params(explosionLevel);
  for (auto i = CurFn->arg_begin(), e = CurFn->arg_end(); i != e; ++i)
    params.add(i);
  return params;
}

/// Emit the basic block that 'return' should branch to and insert it into
/// the current function. This creates a second
/// insertion point that most blocks should be inserted before.
void IRGenFunction::emitBBForReturn() {
  ReturnBB = createBasicBlock("return");
  CurFn->getBasicBlockList().push_back(ReturnBB);
}

/// Emit the prologue for the function.
void IRGenFunction::emitPrologue() {
  // Set up the IRBuilder.
  llvm::BasicBlock *EntryBB = createBasicBlock("entry");
  assert(CurFn->getBasicBlockList().empty() && "prologue already emitted?");
  CurFn->getBasicBlockList().push_back(EntryBB);
  Builder.SetInsertPoint(EntryBB);

  // Set up the alloca insertion point.
  AllocaIP = Builder.CreateAlloca(IGM.Int1Ty, /*array size*/ nullptr,
                                  "alloca point");
}

/// Emit a branch to the return block and set the insert point there.
/// Returns true if the return block is reachable, false otherwise.
bool IRGenFunction::emitBranchToReturnBB() {
  // If there are no edges to the return block, we never want to emit it.
  if (ReturnBB->use_empty()) {
    ReturnBB->eraseFromParent();
    
    // Normally this means that we'll just insert the epilogue in the
    // current block, but if the current IP is unreachable then so is
    // the entire epilogue.
    if (!Builder.hasValidIP())
      return false;
    
    // Otherwise, branch to it if the current IP is reachable.
  } else if (Builder.hasValidIP()) {
    Builder.CreateBr(ReturnBB);
    Builder.SetInsertPoint(ReturnBB);
    
    // Otherwise, if there is exactly one use of the return block, merge
    // it into its predecessor.
  } else if (ReturnBB->hasOneUse()) {
    // return statements are never emitted as conditional branches.
    llvm::BranchInst *Br = cast<llvm::BranchInst>(*ReturnBB->use_begin());
    assert(Br->isUnconditional());
    Builder.SetInsertPoint(Br->getParent());
    Br->eraseFromParent();
    ReturnBB->eraseFromParent();
    
    // Otherwise, just move the IP to the return block.
  } else {
    Builder.SetInsertPoint(ReturnBB);
  }
  return true;
}

/// Emit the epilogue for the function.
void IRGenFunction::emitEpilogue() {
  // Destroy the alloca insertion point.
  AllocaIP->eraseFromParent();
}

void IRGenFunction::emitScalarReturn(SILType resultType, Explosion &result) {
  if (result.size() == 0) {
    Builder.CreateRetVoid();
    return;
  }

  if (result.size() == 1) {
    Builder.CreateRet(result.claimNext());
    return;
  }

  // Multiple return values are returned as a struct.
  auto &resultTI = IGM.getTypeInfo(resultType);
  auto schema = resultTI.getSchema(result.getKind());
  auto *bodyType = cast<llvm::StructType>(schema.getScalarResultType(IGM));

  assert(bodyType->getNumElements() == result.size());
  llvm::Value *resultAgg = llvm::UndefValue::get(bodyType);
  for (unsigned i = 0, e = result.size(); i != e; ++i) {
    llvm::Value *elt = result.claimNext();
    resultAgg = Builder.CreateInsertValue(resultAgg, elt, i);
  }

  // The typical case - the result IR type used within the function
  // matches the ABI result type of the function.
  auto *ABIType = cast<llvm::StructType>(CurFn->getReturnType());
  if (bodyType == ABIType) {
    Builder.CreateRet(resultAgg);
    return;
  }

  // Otherwise we need to store the return value, bitcast a pointer
  // to the location we stored to, and load it back with the ABI
  // return type.
  auto *storage = new llvm::AllocaInst(bodyType, "return.coerced", AllocaIP);
  Builder.CreateStore(resultAgg, storage);
  auto *coerced = Builder.CreateBitCast(storage, ABIType->getPointerTo());
  auto *returned = Builder.CreateLoad(coerced);
  Builder.CreateRet(returned);
}

static void emitApplyArgument(IRGenFunction &IGF,
                              SILParameterInfo origParam,
                              SILParameterInfo substParam,
                              ArrayRef<Substitution> subs,
                              Explosion &in,
                              Explosion &out) {
  bool isSubstituted = (substParam.getSILType() != origParam.getSILType());
  
  // For indirect arguments, we just need to pass a pointer.
  if (origParam.isIndirect()) {
    // This address is of the substituted type.
    auto addr = in.claimNext();
    
    // If a substitution is in play, just bitcast the address.
    if (isSubstituted) {
      auto origType = IGF.IGM.getStoragePointerType(origParam.getSILType());
      addr = IGF.Builder.CreateBitCast(addr, origType);
    }
    
    out.add(addr);
    return;
  }
  
  // Otherwise, it's an explosion, which we may need to translate,
  // both in terms of explosion level and substitution levels.

  // Handle the last unsubstituted case.
  if (!isSubstituted) {
    auto &substArgTI
      = cast<LoadableTypeInfo>(IGF.getTypeInfo(substParam.getSILType()));
    substArgTI.reexplode(IGF, in, out);
    return;
  }
  
  reemitAsUnsubstituted(IGF, origParam.getType(), substParam.getType(),
                        subs, in, out);
}


/// Emit the forwarding stub function for a partial application.
static llvm::Function *emitPartialApplicationForwarder(IRGenModule &IGM,
                                       llvm::Function *staticFnPtr,
                                       llvm::Type *fnTy,
                                       ExplosionKind explosionLevel,
                                       CanSILFunctionType origType,
                                       CanSILFunctionType outType,
                                       ArrayRef<Substitution> subs,
                                       HeapLayout const &layout) {
  llvm::AttributeSet attrs;
  ExtraData extraData
    = layout.isKnownEmpty() ? ExtraData::None : ExtraData::Retainable;
  llvm::FunctionType *fwdTy = IGM.getFunctionType(outType,
                                                  explosionLevel,
                                                  extraData,
                                                  attrs);
  // Build a name for the thunk. If we're thunking a static function reference,
  // include its symbol name in the thunk name.
  llvm::SmallString<20> thunkName;
  thunkName += "_TPA";
  if (staticFnPtr) {
    thunkName += '_';
    thunkName += staticFnPtr->getName();
  }
  
  // FIXME: Maybe cache the thunk by function and closure types?.
  llvm::Function *fwd =
    llvm::Function::Create(fwdTy, llvm::Function::InternalLinkage,
                           llvm::StringRef(thunkName), &IGM.Module);
  fwd->setAttributes(attrs);

  IRGenFunction subIGF(IGM, fwd);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(subIGF, fwd);
  
  Explosion origParams = subIGF.collectParameters(explosionLevel);
  
  // Reemit the parameters as unsubstituted.
  Explosion params(explosionLevel);
  for (unsigned i = 0; i < outType->getParameters().size(); ++i) {
    emitApplyArgument(subIGF, origType->getParameters()[i],
                      outType->getParameters()[i],
                      subs, origParams, params);
  }

  typedef std::pair<const TypeInfo &, Address> AddressToDeallocate;
  SmallVector<AddressToDeallocate, 4> addressesToDeallocate;

  // FIXME: support
  NonFixedOffsets offsets = Nothing;
  
  // If there's a data pointer required, grab it (it's always the
  // last parameter) and load out the extra, previously-curried
  // parameters.
  if (!layout.isKnownEmpty()) {
    llvm::Value *rawData = origParams.takeLast();
    Address data = layout.emitCastTo(subIGF, rawData);

    // Perform the loads.
    for (auto &fieldLayout : layout.getElements()) {
      Address fieldAddr = fieldLayout.project(subIGF, data, offsets);
      auto &fieldType = fieldLayout.getType();

      // If the argument is passed indirectly, copy into a temporary.
      // (If it were instead passed "const +0", we could pass a reference
      // to the memory in the data pointer.  But it isn't.)
      if (fieldType.isIndirectArgument(explosionLevel)) {
        auto caddr = fieldType.allocateStack(subIGF, "arg.temp");
        fieldType.initializeWithCopy(subIGF, caddr.getAddress(), fieldAddr);
        params.add(caddr.getAddressPointer());

        // Remember to deallocate later.
        addressesToDeallocate.push_back(
                       AddressToDeallocate(fieldType, caddr.getContainer()));
        continue;
      }

      // Otherwise, just load out.
      cast<LoadableTypeInfo>(fieldType).loadAsCopy(subIGF, fieldAddr, params);
    }
    
    // Kill the allocated data pointer immediately.  The safety of
    // this assumes that neither this release nor any of the loads
    // can throw.
    subIGF.emitRelease(rawData);
  }
  
  // If we didn't receive a static function, dig the function pointer
  // out of the context.
  llvm::Value *fnPtr;
  if (staticFnPtr) {
    assert(staticFnPtr->getType() == fnTy && "static function type mismatch?!");
    fnPtr = staticFnPtr;
  } else {
    // The dynamic function pointer is packed "last" into the context.
    fnPtr = params.takeLast();
    // It comes out of the context as an i8*. Cast to the function type.
    fnPtr = subIGF.Builder.CreateBitCast(fnPtr, fnTy);
  }
  
  llvm::CallInst *call = subIGF.Builder.CreateCall(fnPtr, params.claimAll());
  
  // FIXME: Default Swift attributes for indirect calls?
  if (staticFnPtr) {
    call->setAttributes(staticFnPtr->getAttributes());
    call->setCallingConv(staticFnPtr->getCallingConv());
  }
  call->setTailCall();

  // Deallocate everything we allocated above.
  // FIXME: exceptions?
  for (auto &entry : addressesToDeallocate) {
    entry.first.deallocateStack(subIGF, entry.second);
  }
  
  if (call->getType()->isVoidTy())
    subIGF.Builder.CreateRetVoid();
  else
    subIGF.Builder.CreateRet(call);
  
  return fwd;
}

/// Emit a partial application thunk for a function pointer applied to a partial
/// set of argument values.
void irgen::emitFunctionPartialApplication(IRGenFunction &IGF,
                                           llvm::Value *fnPtr,
                                           llvm::Value *fnContext,
                                           Explosion &args,
                                           ArrayRef<SILType> argTypes,
                                           ArrayRef<Substitution> subs,
                                           CanSILFunctionType origType,
                                           CanSILFunctionType substType,
                                           CanSILFunctionType outType,
                                           Explosion &out) {
  // If we have a single Swift-refcounted context value, we can adopt it
  // directly as our closure context without creating a box and thunk.
  enum HasSingleSwiftRefcountedContext { Maybe, Yes, No }
    hasSingleSwiftRefcountedContext = Maybe;
  
  // Collect the type infos for the context types.
  SmallVector<const TypeInfo *, 4> argTypeInfos;
  for (SILType argType : argTypes) {
    auto &ti = IGF.getTypeInfoForLowered(argType.getSwiftType());
    argTypeInfos.push_back(&ti);

    // Update the single-swift-refcounted check, unless we already ruled that
    // out.
    if (hasSingleSwiftRefcountedContext == No)
      continue;
    
    // Empty values don't matter.
    auto schema = ti.getSchema(out.getKind());
    if (schema.size() == 0)
      continue;
    
    // Adding nonempty values when we already have a single refcounted pointer
    // ruins it.
    if (hasSingleSwiftRefcountedContext == Yes) {
      hasSingleSwiftRefcountedContext = No;
      continue;
    }
      
    if (ti.isSingleRetainablePointer(ResilienceScope::Local))
      hasSingleSwiftRefcountedContext = Yes;
    else
      hasSingleSwiftRefcountedContext = No;
  }
  
  // Include the context pointer, if any, in the function arguments.
  if (fnContext) {
    args.add(fnContext);
    argTypeInfos.push_back(
         &IGF.getTypeInfoForLowered(IGF.IGM.Context.TheObjectPointerType));
    // If this is the only context argument we end up with, we can just share
    // it.
    if (args.size() == 1)
      hasSingleSwiftRefcountedContext = Yes;
  }
  
  // Collect the polymorphic arguments.
  if (hasPolymorphicParameters(origType)) {
    assert(!subs.empty() && "no substitutions for polymorphic argument?!");
    Explosion polymorphicArgs(args.getKind());
    emitPolymorphicArguments(IGF, origType, substType, subs, polymorphicArgs);

    const TypeInfo &metatypeTI = IGF.IGM.getTypeMetadataPtrTypeInfo(),
                   &witnessTI = IGF.IGM.getWitnessTablePtrTypeInfo();
    for (llvm::Value *arg : polymorphicArgs.getAll()) {
      if (arg->getType() == IGF.IGM.TypeMetadataPtrTy)
        argTypeInfos.push_back(&metatypeTI);
      else if (arg->getType() == IGF.IGM.WitnessTablePtrTy)
        argTypeInfos.push_back(&witnessTI);
      else
        llvm_unreachable("unexpected polymorphic argument");
    }
    
    args.add(polymorphicArgs.claimAll());
  } else {
    assert(subs.empty() && "substitutions for non-polymorphic function?!");
  }
  
  // If we have a single refcounted pointer context (and no polymorphic args
  // to capture), skip building the box and thunk and just take the pointer as
  // context.
  if (args.size() == 1 && hasSingleSwiftRefcountedContext == Yes) {
    fnPtr = IGF.Builder.CreateBitCast(fnPtr, IGF.IGM.Int8PtrTy);
    out.add(fnPtr);
    llvm::Value *ctx = args.claimNext();
    ctx = IGF.Builder.CreateBitCast(ctx, IGF.IGM.RefCountedPtrTy);
    out.add(ctx);
    return;
  }
  
  // If the function pointer is dynamic, include it in the context.
  auto staticFn = dyn_cast<llvm::Function>(fnPtr);
  if (!staticFn) {
    llvm::Value *fnRawPtr = IGF.Builder.CreateBitCast(fnPtr, IGF.IGM.Int8PtrTy);
    args.add(fnRawPtr);
    argTypeInfos.push_back(
             &IGF.getTypeInfoForLowered(IGF.IGM.Context.TheRawPointerType));
  }

  // Store the context arguments on the heap.
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, argTypeInfos);
  llvm::Value *data;
  if (layout.isKnownEmpty()) {
    data = IGF.IGM.RefCountedNull;
  } else {
    // Allocate a new object.
    data = IGF.emitUnmanagedAlloc(layout, "closure");
    Address dataAddr = layout.emitCastTo(IGF, data);

    // FIXME: preserve non-fixed offsets
    NonFixedOffsets offsets = Nothing;
    
    // Perform the store.
    for (auto &fieldLayout : layout.getElements()) {
      Address fieldAddr = fieldLayout.project(IGF, dataAddr, offsets);
      fieldLayout.getType().initializeFromParams(IGF, args, fieldAddr);
    }
  }
  assert(args.empty() && "unused args in partial application?!");
  
  // Create the forwarding stub.
  llvm::AttributeSet attrs;
  auto fnPtrTy = IGF.IGM.getFunctionType(origType, args.getKind(),
                                         fnContext ? ExtraData::Retainable
                                                   : ExtraData::None, attrs)
    ->getPointerTo();

  llvm::Function *forwarder = emitPartialApplicationForwarder(IGF.IGM,
                                                              staticFn,
                                                              fnPtrTy,
                                                              args.getKind(),
                                                              origType,
                                                              outType,
                                                              subs,
                                                              layout);
  llvm::Value *forwarderValue = IGF.Builder.CreateBitCast(forwarder,
                                                          IGF.IGM.Int8PtrTy);
  out.add(forwarderValue);
  out.add(data);
}

/// Fetch the declaration of the given block-to-
llvm::Function *IRGenModule::getAddrOfBridgeToBlockConverter(SILType blockType)
{
  LinkEntity entity
    = LinkEntity::forBridgeToBlockConverter(blockType);
  
  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);
  
  // The block converter is a C function with signature
  // __typeof__(R (^)(A...)) converter(R (*)(A..., swift_refcounted*),
  //                                   swift_refcounted*)
  // We simplify that to the llvm type %objc(i8*, %swift.refcounted*)*.
  llvm::Type *fnParams[] = {Int8PtrTy, RefCountedPtrTy};
  llvm::FunctionType *fnType = llvm::FunctionType::get(ObjCPtrTy,
                                                       fnParams,
                                                       /*isVarArg=*/ false);
  
  
  llvm::AttributeSet attrs;
  auto cc = expandAbstractCC(*this, AbstractCC::C);
  
  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// Emit a call to convert a Swift closure to an Objective-C block via a
/// shim function defined in Objective-C.
void irgen::emitBridgeToBlock(IRGenFunction &IGF,
                              SILType blockTy,
                              Explosion &swiftClosure,
                              Explosion &outBlock) {
  // Get the function pointer as an i8*.
  llvm::Value *fn = swiftClosure.claimNext();
  fn = IGF.Builder.CreateBitCast(fn, IGF.IGM.Int8PtrTy);

  // Get the context pointer as a %swift.refcounted*.
  llvm::Value *mContext = swiftClosure.claimNext();
  llvm::Value *context = IGF.Builder.CreateBitCast(mContext,
                                                   IGF.IGM.RefCountedPtrTy);
  // Get the shim function we'll call.
  llvm::Function *converter = IGF.IGM.getAddrOfBridgeToBlockConverter(blockTy);
  
  // Emit the call.
  outBlock.add(IGF.Builder.CreateCall2(converter, fn, context));
}

namespace {

struct EmitLocalDecls : public ASTWalker {
  IRGenModule &IGM;
  
  EmitLocalDecls(IRGenModule &IGM) : IGM(IGM) {}
  
  bool walkToDeclPre(Decl *D) override {
    switch (D->getKind()) {
    case DeclKind::Import:
    case DeclKind::Subscript:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
    case DeclKind::EnumCase:
    case DeclKind::EnumElement:
    case DeclKind::Constructor:
    case DeclKind::Destructor:
    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
      llvm_unreachable("declaration cannot appear in local scope");
      
    case DeclKind::TypeAlias:
    case DeclKind::AssociatedType:
    case DeclKind::GenericTypeParam:
      // no IR generation support required.
    case DeclKind::PatternBinding:
    case DeclKind::Var:
      // These get lowered by SIL.
      return false;
      
    case DeclKind::Func:
      // The body gets lowered by SIL, but we need to check for local decls.
      IGM.emitLocalDecls(cast<FuncDecl>(D));
      return false;
      
    case DeclKind::Enum:
      IGM.emitEnumDecl(cast<EnumDecl>(D));
      return false;
      
    case DeclKind::Struct:
      IGM.emitStructDecl(cast<StructDecl>(D));
      return false;
      
    case DeclKind::Class:
      IGM.emitClassDecl(cast<ClassDecl>(D));
      return false;
    }
  }
  
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    if (auto *CE = dyn_cast<ClosureExpr>(E)) {
      IGM.emitLocalDecls(CE->getBody());
      return { false, E };
    }
    return { true, E };
  }
};

} // end anonymous namespace

void IRGenModule::emitLocalDecls(BraceStmt *body) {
  EmitLocalDecls walker(*this);
  body->walk(walker);
}

void IRGenModule::emitLocalDecls(FuncDecl *fd) {
  if (fd->getBody())
    emitLocalDecls(fd->getBody());
}

void IRGenModule::emitLocalDecls(ConstructorDecl *cd) {
  if (cd->getBody())
    emitLocalDecls(cd->getBody());
}

void IRGenModule::emitLocalDecls(DestructorDecl *dd) {
  if (dd->getBody())
    emitLocalDecls(dd->getBody());
}
