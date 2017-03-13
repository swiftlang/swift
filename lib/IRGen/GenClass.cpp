//===--- GenClass.cpp - Swift IR Generation For 'class' Types -------------===//
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
//  This file implements IR generation for class types.
//
//===----------------------------------------------------------------------===//

#include "GenClass.h"

#include "swift/ABI/Class.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeMemberVisitor.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/CallSite.h"

#include "ConstantBuilder.h"
#include "Explosion.h"
#include "GenFunc.h"
#include "GenMeta.h"
#include "GenObjC.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "GenHeap.h"
#include "HeapTypeInfo.h"
#include "Linking.h"
#include "MemberAccessStrategy.h"


using namespace swift;
using namespace irgen;

static ClassDecl *getRootClass(ClassDecl *theClass) {
  while (theClass->hasSuperclass()) {
    theClass = theClass->getSuperclass()->getClassOrBoundGenericClass();
    assert(theClass && "base type of class not a class?");
  }
  return theClass;
}

/// What reference counting mechanism does a class have?
ReferenceCounting irgen::getReferenceCountingForClass(IRGenModule &IGM,
                                                      ClassDecl *theClass) {
  // If ObjC interop is disabled, we have a Swift refcount.
  if (!IGM.ObjCInterop)
    return ReferenceCounting::Native;

  // NOTE: if you change this, change Type::usesNativeReferenceCounting.
  // If the root class is implemented in swift, then we have a swift
  // refcount; otherwise, we have an ObjC refcount.
  if (getRootClass(theClass)->hasKnownSwiftImplementation())
    return ReferenceCounting::Native;

  return ReferenceCounting::ObjC;
}

/// What isa encoding mechanism does a type have?
IsaEncoding irgen::getIsaEncodingForType(IRGenModule &IGM,
                                         CanType type) {
  if (auto theClass = type->getClassOrBoundGenericClass()) {
    // We can access the isas of pure Swift classes directly.
    if (getRootClass(theClass)->hasKnownSwiftImplementation())
      return IsaEncoding::Pointer;
    // For ObjC or mixed classes, we need to use object_getClass.
    return IsaEncoding::ObjC;
  }
  // Non-class heap objects should be pure Swift, so we can access their isas
  // directly.
  return IsaEncoding::Pointer;
}

namespace {
  /// Layout information for class types.
  class ClassTypeInfo : public HeapTypeInfo<ClassTypeInfo> {
    ClassDecl *TheClass;
    mutable StructLayout *Layout;
    mutable ClassLayout FieldLayout;

    /// Can we use swift reference-counting, or do we have to use
    /// objc_retain/release?
    const ReferenceCounting Refcount;
    
    void generateLayout(IRGenModule &IGM, SILType classType) const;

  public:
    ClassTypeInfo(llvm::PointerType *irType, Size size,
                  SpareBitVector spareBits, Alignment align,
                  ClassDecl *theClass, ReferenceCounting refcount)
      : HeapTypeInfo(irType, size, std::move(spareBits), align),
        TheClass(theClass), Layout(nullptr), Refcount(refcount) {}

    ReferenceCounting getReferenceCounting() const {
      return Refcount;
    }

    ~ClassTypeInfo() override {
      delete Layout;
    }

    ClassDecl *getClass() const { return TheClass; }

    const StructLayout &getLayout(IRGenModule &IGM, SILType classType) const;
    const ClassLayout &getClassLayout(IRGenModule &IGM, SILType type) const;

    Alignment getHeapAlignment(IRGenModule &IGM, SILType type) const {
      return getLayout(IGM, type).getAlignment();
    }
    ArrayRef<ElementLayout> getElements(IRGenModule &IGM, SILType type) const {
      return getLayout(IGM, type).getElements();
    }
  };
} // end anonymous namespace

/// Return the lowered type for the class's 'self' type within its context.
static SILType getSelfType(ClassDecl *base) {
  auto loweredTy = base->getDeclaredTypeInContext()->getCanonicalType();
  return SILType::getPrimitiveObjectType(loweredTy);
}

namespace {
  class ClassLayoutBuilder : public StructLayoutBuilder {
    SmallVector<ElementLayout, 8> Elements;
    SmallVector<VarDecl*, 8> AllStoredProperties;
    SmallVector<FieldAccess, 8> AllFieldAccesses;

    unsigned NumInherited = 0;

    // Does the class metadata require dynamic initialization above and
    // beyond what the runtime can automatically achieve?
    //
    // This is true if the class or any of its ancestors:
    //   - is generic,
    //   - is resilient,
    //   - has a parent type which isn't emittable as a constant,
    //   - or has a field with resilient layout.
    bool ClassMetadataRequiresDynamicInitialization = false;

    // Does the superclass have a fixed number of stored properties?
    // If not, and the class has generally-dependent layout, we have to
    // access stored properties through an indirect offset into the field
    // offset vector.
    bool ClassHasFixedFieldCount = true;

    // Does the class have a fixed size up until the current point?
    // If not, we have to access stored properties either ivar offset globals,
    // or through the field offset vector, based on whether the layout has
    // dependent layout.
    bool ClassHasFixedSize = true;

    // Does the class have identical layout under all generic substitutions?
    // If not, we can have to access stored properties through the field
    // offset vector in the instantiated type metadata.
    bool ClassHasConcreteLayout = true;

  public:
    ClassLayoutBuilder(IRGenModule &IGM, SILType classType)
      : StructLayoutBuilder(IGM)
    {
      // Start by adding a heap header.
      addHeapHeader();

      // Next, add the fields for the given class.
      auto theClass = classType.getClassOrBoundGenericClass();
      assert(theClass);
      addFieldsForClass(theClass, classType);
      
      // Add these fields to the builder.
      addFields(Elements, LayoutStrategy::Universal);
    }

    /// Return the element layouts.
    ArrayRef<ElementLayout> getElements() const {
      return Elements;
    }

    ClassLayout getClassLayout() const {
      ClassLayout fieldLayout;
      auto allStoredProps = IGM.Context.AllocateCopy(AllStoredProperties);
      auto inheritedStoredProps = allStoredProps.slice(0, NumInherited);
      fieldLayout.AllStoredProperties = allStoredProps;
      fieldLayout.InheritedStoredProperties = inheritedStoredProps;
      fieldLayout.AllFieldAccesses = IGM.Context.AllocateCopy(AllFieldAccesses);
      fieldLayout.MetadataRequiresDynamicInitialization =
        ClassMetadataRequiresDynamicInitialization;
      return fieldLayout;
    }

  private:
    void addFieldsForClass(ClassDecl *theClass, SILType classType) {
      if (theClass->isGenericContext())
        ClassMetadataRequiresDynamicInitialization = true;

      if (!ClassMetadataRequiresDynamicInitialization) {
        if (auto parentType =
              theClass->getDeclContext()->getDeclaredTypeInContext()) {
          if (!tryEmitConstantTypeMetadataRef(IGM,
                                              parentType->getCanonicalType(),
                                              SymbolReferenceKind::Absolute))
            ClassMetadataRequiresDynamicInitialization = true;
        }
      }

      if (theClass->hasSuperclass()) {
        SILType superclassType = classType.getSuperclass(nullptr);
        auto superclass = superclassType.getClassOrBoundGenericClass();
        assert(superclass);

        if (superclass->hasClangNode()) {
          // If the superclass was imported from Objective-C, its size is
          // not known at compile time. However, since the field offset
          // vector only stores offsets of stored properties defined in
          // Swift, we don't have to worry about indirect indexing of
          // the field offset vector.
          ClassHasFixedSize = false;

        } else if (IGM.isResilient(superclass, ResilienceExpansion::Maximal)) {
          ClassMetadataRequiresDynamicInitialization = true;

          // If the superclass is resilient to us, we cannot statically
          // know the layout of either its instances or its class objects.
          //
          // FIXME: We need to implement indirect field/vtable entry access
          // before we can enable this
          if (IGM.Context.LangOpts.EnableClassResilience) {
            ClassHasFixedFieldCount = false;
          } else {
            addFieldsForClass(superclass, superclassType);
            NumInherited = Elements.size();
          }

          ClassHasFixedSize = false;

          // Furthermore, if the superclass is a generic context, we have to
          // assume that its layout depends on its generic parameters.
          // But this only propagates down to subclasses whose superclass type
          // depends on the subclass's generic context.
          if (superclassType.hasArchetype())
            ClassHasConcreteLayout = false;

        } else {
          // Otherwise, we have total knowledge of the class and its
          // fields, so walk them to compute the layout.
          addFieldsForClass(superclass, superclassType);
          // Count the fields we got from the superclass.
          NumInherited = Elements.size();
        }
      }

      // Access strategies should be set by the abstract class layout,
      // not using the concrete type information we have.
      const ClassLayout *abstractLayout = nullptr;

      SILType selfType = getSelfType(theClass);
      if (classType != selfType) {
        auto &selfTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();
        abstractLayout = &selfTI.getClassLayout(IGM, selfType);
      }

      // Collect fields from this class and add them to the layout as a chunk.
      addDirectFieldsFromClass(theClass, classType, abstractLayout);
    }

    void addDirectFieldsFromClass(ClassDecl *theClass,
                                  SILType classType,
                                  const ClassLayout *abstractLayout) {
      for (VarDecl *var : theClass->getStoredProperties()) {
        SILType type = classType.getFieldType(var, IGM.getSILModule());
        auto &eltType = IGM.getTypeInfo(type);

        if (!eltType.isFixedSize()) {
          ClassMetadataRequiresDynamicInitialization = true;
          ClassHasFixedSize = false;

          if (type.hasArchetype())
            ClassHasConcreteLayout = false;
        }

        size_t fieldIndex = AllStoredProperties.size();
        assert(!abstractLayout ||
               abstractLayout->getFieldIndex(var) == fieldIndex);

        Elements.push_back(ElementLayout::getIncomplete(eltType));
        AllStoredProperties.push_back(var);
        AllFieldAccesses.push_back(getFieldAccess(abstractLayout, fieldIndex));
      }
    }

    FieldAccess getFieldAccess(const ClassLayout *abstractLayout,
                               size_t abstractFieldIndex) {
      // The class has fixed size, so the field offset is known statically.
      if (ClassHasFixedSize) {
        return FieldAccess::ConstantDirect;
      }

      // If the field offset can't be known at compile time, we need to
      // load a field offset, either from a global or the metadata's field
      // offset vector.

      // The global will exist only if the abstract type has concrete layout,
      // so if we're not laying out the abstract type, use its access rule.
      if (abstractLayout) {
        return abstractLayout->AllFieldAccesses[abstractFieldIndex];
      }

      // If layout doesn't depend on any generic parameters, but it's
      // nonetheless not statically known (because either a superclass
      // or a member type was resilient), then we can rely on the existence
      // of a global field offset variable which will be initialized by
      // either the Objective-C or Swift runtime, depending on the
      // class's heritage.
      if (ClassHasConcreteLayout) {
        return FieldAccess::NonConstantDirect;
      }

      // If layout depends on generic parameters, we have to load the
      // offset from the class metadata.

      // If the layout of the class metadata is statically known, then
      // there should be a fixed offset to the right offset.
      if (ClassHasFixedFieldCount) {
        return FieldAccess::ConstantIndirect;
      }

      // Otherwise, the offset of the offset is stored in a global variable
      // that will be set up by the runtime.
      return FieldAccess::NonConstantIndirect;
    }
  };
} // end anonymous namespace

void ClassTypeInfo::generateLayout(IRGenModule &IGM, SILType classType) const {
  assert(!Layout && FieldLayout.AllStoredProperties.empty() &&
         "already generated layout");

  // Add the heap header.
  ClassLayoutBuilder builder(IGM, classType);

  // generateLayout can call itself recursively in order to compute a layout
  // for the abstract type.  If classType shares an exemplar types with the
  // abstract type, that will end up re-entrantly building the layout
  // of the same ClassTypeInfo.  We don't have anything else to do in this
  // case.
  if (Layout) {
    assert(this == &IGM.getTypeInfo(
                     getSelfType(classType.getClassOrBoundGenericClass())));
    return;
  }
  
  // Set the body of the class type.
  auto classTy =
    cast<llvm::StructType>(getStorageType()->getPointerElementType());
  builder.setAsBodyOfStruct(classTy);
  
  // Record the layout.
  Layout = new StructLayout(builder, classType.getSwiftRValueType(), classTy,
                            builder.getElements());
  FieldLayout = builder.getClassLayout();
}

const StructLayout &
ClassTypeInfo::getLayout(IRGenModule &IGM, SILType classType) const {
  // Return the cached layout if available.
  if (Layout) return *Layout;

  generateLayout(IGM, classType);
  return *Layout;
}

const ClassLayout &
ClassTypeInfo::getClassLayout(IRGenModule &IGM, SILType classType) const {
  // Return the cached layout if available.
  if (Layout)
    return FieldLayout;
  
  generateLayout(IGM, classType);
  return FieldLayout;
}

/// Cast the base to i8*, apply the given inbounds offset (in bytes,
/// as a size_t), and cast to a pointer to the given type.
llvm::Value *IRGenFunction::emitByteOffsetGEP(llvm::Value *base,
                                              llvm::Value *offset,
                                              llvm::Type *objectType,
                                              const llvm::Twine &name) {
  assert(offset->getType() == IGM.SizeTy);
  auto addr = Builder.CreateBitCast(base, IGM.Int8PtrTy);
  addr = Builder.CreateInBoundsGEP(addr, offset);
  return Builder.CreateBitCast(addr, objectType->getPointerTo(), name);
}

/// Cast the base to i8*, apply the given inbounds offset (in bytes,
/// as a size_t), and create an address in the given type.
Address IRGenFunction::emitByteOffsetGEP(llvm::Value *base,
                                         llvm::Value *offset,
                                         const TypeInfo &type,
                                         const llvm::Twine &name) {
  auto addr = emitByteOffsetGEP(base, offset, type.getStorageType(), name);
  return type.getAddressForPointer(addr);
}

/// Emit a field l-value by applying the given offset to the given base.
static OwnedAddress emitAddressAtOffset(IRGenFunction &IGF,
                                        SILType baseType,
                                        llvm::Value *base,
                                        llvm::Value *offset,
                                        VarDecl *field) {
  auto &fieldTI =
    IGF.getTypeInfo(baseType.getFieldType(field, IGF.getSILModule()));
  auto addr = IGF.emitByteOffsetGEP(base, offset, fieldTI,
                              base->getName() + "." + field->getName().str());
  return OwnedAddress(addr, base);
}

OwnedAddress irgen::projectPhysicalClassMemberAddress(IRGenFunction &IGF,
                                                      llvm::Value *base,
                                                      SILType baseType,
                                                      SILType fieldType,
                                                      VarDecl *field) {
  // If the field is empty, its address doesn't matter.
  auto &fieldTI = IGF.getTypeInfo(fieldType);
  if (fieldTI.isKnownEmpty(ResilienceExpansion::Maximal)) {
    return OwnedAddress(fieldTI.getUndefAddress(), base);
  }
  
  auto &baseClassTI = IGF.getTypeInfo(baseType).as<ClassTypeInfo>();
  ClassDecl *baseClass = baseClassTI.getClass();

  auto &classLayout = baseClassTI.getClassLayout(IGF.IGM, baseType);
  unsigned fieldIndex = classLayout.getFieldIndex(field);

  switch (classLayout.AllFieldAccesses[fieldIndex]) {
  case FieldAccess::ConstantDirect: {
    Address baseAddr(base, baseClassTI.getHeapAlignment(IGF.IGM, baseType));
    auto &element = baseClassTI.getElements(IGF.IGM, baseType)[fieldIndex];
    Address memberAddr = element.project(IGF, baseAddr, None);
    // We may need to bitcast the address if the field is of a generic type.
    if (memberAddr.getType()->getElementType() != fieldTI.getStorageType())
      memberAddr = IGF.Builder.CreateBitCast(memberAddr,
                                     fieldTI.getStorageType()->getPointerTo());
    return OwnedAddress(memberAddr, base);
  }
    
  case FieldAccess::NonConstantDirect: {
    Address offsetA = IGF.IGM.getAddrOfFieldOffset(field, /*indirect*/ false,
                                                   NotForDefinition);
    auto offsetVar = cast<llvm::GlobalVariable>(offsetA.getAddress());
    offsetVar->setConstant(false);
    auto offset = IGF.Builder.CreateLoad(offsetA, "offset");
    return emitAddressAtOffset(IGF, baseType, base, offset, field);
  }
    
  case FieldAccess::ConstantIndirect: {
    auto metadata = emitHeapMetadataRefForHeapObject(IGF, base, baseType);
    auto offset = emitClassFieldOffset(IGF, baseClass, field, metadata);
    return emitAddressAtOffset(IGF, baseType, base, offset, field);
  }
    
  case FieldAccess::NonConstantIndirect: {
    auto metadata = emitHeapMetadataRefForHeapObject(IGF, base, baseType);
    Address indirectOffsetA =
      IGF.IGM.getAddrOfFieldOffset(field, /*indirect*/ true,
                                   NotForDefinition);
    auto offsetVar = cast<llvm::GlobalVariable>(indirectOffsetA.getAddress());
    offsetVar->setConstant(false);
    auto indirectOffset =
      IGF.Builder.CreateLoad(indirectOffsetA, "indirect-offset");
    auto offsetA =
      IGF.emitByteOffsetGEP(metadata, indirectOffset, IGF.IGM.SizeTy);
    auto offset =
      IGF.Builder.CreateLoad(Address(offsetA, IGF.IGM.getPointerAlignment()));
    return emitAddressAtOffset(IGF, baseType, base, offset, field);
  }
  }
  llvm_unreachable("bad field-access strategy");
}

MemberAccessStrategy
irgen::getPhysicalClassMemberAccessStrategy(IRGenModule &IGM,
                                            SILType baseType, VarDecl *field) {
  auto &baseClassTI = IGM.getTypeInfo(baseType).as<ClassTypeInfo>();
  ClassDecl *baseClass = baseType.getClassOrBoundGenericClass();

  auto &classLayout = baseClassTI.getClassLayout(IGM, baseType);
  unsigned fieldIndex = classLayout.getFieldIndex(field);

  switch (classLayout.AllFieldAccesses[fieldIndex]) {
  case FieldAccess::ConstantDirect: {
    auto &element = baseClassTI.getElements(IGM, baseType)[fieldIndex];
    return MemberAccessStrategy::getDirectFixed(element.getByteOffset());
  }

  case FieldAccess::NonConstantDirect: {
    std::string symbol =
      LinkEntity::forFieldOffset(field, /*indirect*/ false).mangleAsString();
    return MemberAccessStrategy::getDirectGlobal(std::move(symbol),
                                 MemberAccessStrategy::OffsetKind::Bytes_Word);
  }

  case FieldAccess::ConstantIndirect: {
    Size indirectOffset = getClassFieldOffset(IGM, baseClass, field);
    return MemberAccessStrategy::getIndirectFixed(indirectOffset,
                                 MemberAccessStrategy::OffsetKind::Bytes_Word);
  }

  case FieldAccess::NonConstantIndirect: {
    std::string symbol =
      LinkEntity::forFieldOffset(field, /*indirect*/ true).mangleAsString();
    return MemberAccessStrategy::getIndirectGlobal(std::move(symbol),
                                 MemberAccessStrategy::OffsetKind::Bytes_Word,
                                 MemberAccessStrategy::OffsetKind::Bytes_Word);
  }
  }
  llvm_unreachable("bad field-access strategy");
}

Address irgen::emitTailProjection(IRGenFunction &IGF, llvm::Value *Base,
        SILType ClassType,
        SILType TailType) {
  const ClassTypeInfo &classTI = IGF.getTypeInfo(ClassType).as<ClassTypeInfo>();

  llvm::Value *Offset = nullptr;
  auto &layout = classTI.getLayout(IGF.IGM, ClassType);
  Alignment HeapObjAlign = IGF.IGM.TargetInfo.HeapObjectAlignment;
  Alignment Align;

  // Get the size of the class instance.
  if (layout.isFixedLayout()) {
    Size ClassSize = layout.getSize();
    Offset = llvm::ConstantInt::get(IGF.IGM.SizeTy, ClassSize.getValue());
    Align = HeapObjAlign.alignmentAtOffset(ClassSize);
  } else {
    llvm::Value *metadata = emitHeapMetadataRefForHeapObject(IGF, Base,
                                                             ClassType);
    Offset = emitClassInstanceSizeAndAlignMask(IGF, ClassType, metadata).first;
  }
  // Align up to the TailType.
  assert(TailType.isObject());
  const TypeInfo &TailTI = IGF.getTypeInfo(TailType);
  llvm::Value *AlignMask = TailTI.getAlignmentMask(IGF, TailType);
  Offset = IGF.Builder.CreateAdd(Offset, AlignMask);
  llvm::Value *InvertedMask = IGF.Builder.CreateNot(AlignMask);
  Offset = IGF.Builder.CreateAnd(Offset, InvertedMask);

  llvm::Value *Addr = IGF.emitByteOffsetGEP(Base, Offset,
                                            TailTI.getStorageType(), "tailaddr");

  if (auto *OffsetConst = dyn_cast<llvm::ConstantInt>(Offset)) {
    // Try to get an accurate alignment (only possible if the Offset is a
    // constant).
    Size TotalOffset(OffsetConst->getZExtValue());
    Align = HeapObjAlign.alignmentAtOffset(TotalOffset);
  }
  return Address(Addr, Align);
}

/// Try to stack promote a class instance with possible tail allocated arrays.
///
/// Returns the alloca if successful, or nullptr otherwise.
static llvm::Value *stackPromote(IRGenFunction &IGF,
                      const StructLayout &ClassLayout,
                      int &StackAllocSize,
                      ArrayRef<std::pair<SILType, llvm::Value *>> TailArrays) {
  if (StackAllocSize < 0)
    return nullptr;
  if (!ClassLayout.isFixedLayout())
    return nullptr;

  // Calculate the total size needed.
  // The first part is the size of the class itself.
  Alignment ClassAlign = ClassLayout.getAlignment();
  Size TotalSize = ClassLayout.getSize();

  // Add size for tail-allocated arrays.
  for (const auto &TailArray : TailArrays) {
    SILType ElemTy = TailArray.first;
    llvm::Value *Count = TailArray.second;

    // We can only calculate a constant size if the tail-count is constant.
    auto *CI = dyn_cast<llvm::ConstantInt>(Count);
    if (!CI)
      return nullptr;

    const TypeInfo &ElemTI = IGF.getTypeInfo(ElemTy);
    if (!ElemTI.isFixedSize())
      return nullptr;

    const FixedTypeInfo &ElemFTI = ElemTI.as<FixedTypeInfo>();
    Alignment ElemAlign = ElemFTI.getFixedAlignment();

    // This should not happen - just to be save.
    if (ElemAlign > ClassAlign)
      return nullptr;

    TotalSize = TotalSize.roundUpToAlignment(ElemAlign);
    TotalSize += ElemFTI.getFixedStride() * CI->getValue().getZExtValue();
  }
  if (TotalSize > Size(StackAllocSize))
    return nullptr;
  StackAllocSize = TotalSize.getValue();

  if (TotalSize == ClassLayout.getSize()) {
    // No tail-allocated arrays: we can use the llvm class type for alloca.
    llvm::Type *ClassTy = ClassLayout.getType();
    Address Alloca = IGF.createAlloca(ClassTy, ClassAlign, "reference.raw");
    return Alloca.getAddress();
  }
  // Use a byte-array as type for alloca.
  llvm::Value *SizeVal = llvm::ConstantInt::get(IGF.IGM.Int32Ty,
                                                TotalSize.getValue());
  Address Alloca = IGF.createAlloca(IGF.IGM.Int8Ty, SizeVal, ClassAlign,
                                    "reference.raw");
  return Alloca.getAddress();
}

llvm::Value *irgen::appendSizeForTailAllocatedArrays(IRGenFunction &IGF,
                                                     llvm::Value *size,
                                                     TailArraysRef TailArrays) {
  for (const auto &TailArray : TailArrays) {
    SILType ElemTy = TailArray.first;
    llvm::Value *Count = TailArray.second;

    const TypeInfo &ElemTI = IGF.getTypeInfo(ElemTy);

    // Align up to the tail-allocated array.
    llvm::Value *ElemStride = ElemTI.getStride(IGF, ElemTy);
    llvm::Value *AlignMask = ElemTI.getAlignmentMask(IGF, ElemTy);
    size = IGF.Builder.CreateAdd(size, AlignMask);
    llvm::Value *InvertedMask = IGF.Builder.CreateNot(AlignMask);
    size = IGF.Builder.CreateAnd(size, InvertedMask);

    // Add the size of the tail allocated array.
    llvm::Value *AllocSize = IGF.Builder.CreateMul(ElemStride, Count);
    size = IGF.Builder.CreateAdd(size, AllocSize);
  }
  return size;
}


/// Emit an allocation of a class.
llvm::Value *irgen::emitClassAllocation(IRGenFunction &IGF, SILType selfType,
                                        bool objc, int &StackAllocSize,
                                        TailArraysRef TailArrays) {
  auto &classTI = IGF.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto classType = selfType.getSwiftRValueType();

  // If we need to use Objective-C allocation, do so.
  // If the root class isn't known to use the Swift allocator, we need
  // to call [self alloc].
  if (objc) {
    llvm::Value *metadata =
      emitClassHeapMetadataRef(IGF, classType, MetadataValueType::ObjCClass,
                               /*allow uninitialized*/ true);
    StackAllocSize = -1;
    auto &ti = IGF.getTypeInfo(selfType);
    assert(ti.getSchema().size() == 1);
    assert(!ti.getSchema().containsAggregate());
    auto destType = ti.getSchema()[0].getScalarType();
    auto *val = emitObjCAllocObjectCall(IGF, metadata,
                                        selfType.getSwiftRValueType());
    return IGF.Builder.CreateBitCast(val, destType);
  }

  llvm::Value *metadata =
    emitClassHeapMetadataRef(IGF, classType, MetadataValueType::TypeMetadata);

  // FIXME: Long-term, we clearly need a specialized runtime entry point.
  llvm::Value *size, *alignMask;
  std::tie(size, alignMask)
    = emitClassInstanceSizeAndAlignMask(IGF, selfType, metadata);

  const StructLayout &layout = classTI.getLayout(IGF.IGM, selfType);
  llvm::Type *destType = layout.getType()->getPointerTo();
  llvm::Value *val = nullptr;
  if (llvm::Value *Promoted = stackPromote(IGF, layout, StackAllocSize,
                                           TailArrays)) {
    val = IGF.Builder.CreateBitCast(Promoted, IGF.IGM.RefCountedPtrTy);
    val = IGF.emitInitStackObjectCall(metadata, val, "reference.new");
  } else {
    // Allocate the object on the heap.
    size = appendSizeForTailAllocatedArrays(IGF, size, TailArrays);
    val = IGF.emitAllocObjectCall(metadata, size, alignMask, "reference.new");
    StackAllocSize = -1;
  }
  return IGF.Builder.CreateBitCast(val, destType);
}

llvm::Value *irgen::emitClassAllocationDynamic(IRGenFunction &IGF, 
                                               llvm::Value *metadata,
                                               SILType selfType,
                                               bool objc,
                                               TailArraysRef TailArrays) {
  // If we need to use Objective-C allocation, do so.
  if (objc) {
    return emitObjCAllocObjectCall(IGF, metadata, 
                                   selfType.getSwiftRValueType());
  }

  // Otherwise, allocate using Swift's routines.
  llvm::Value *size, *alignMask;
  std::tie(size, alignMask)
    = emitClassResilientInstanceSizeAndAlignMask(IGF, metadata);
  size = appendSizeForTailAllocatedArrays(IGF, size, TailArrays);

  llvm::Value *val = IGF.emitAllocObjectCall(metadata, size, alignMask,
                                             "reference.new");
  auto &classTI = IGF.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(IGF.IGM, selfType);
  llvm::Type *destType = layout.getType()->getPointerTo();
  return IGF.Builder.CreateBitCast(val, destType);
}

/// Look for the instance method:
///   func __getInstanceSizeAndAlignMask() -> (Int, Int)
/// and use it to populate 'size' and 'alignMask' if it's present.
static bool getInstanceSizeByMethod(IRGenFunction &IGF,
                                    CanType selfType,
                                    ClassDecl *selfClass,
                                    llvm::Value *selfValue,
                                    llvm::Value *&size,
                                    llvm::Value *&alignMask) {
  // Look for a single instance method with the magic name.
  FuncDecl *fn; {
    auto name = IGF.IGM.Context.getIdentifier("__getInstanceSizeAndAlignMask");
    SmallVector<ValueDecl*, 4> results;
    selfClass->lookupQualified(selfType, name, NL_KnownNonCascadingDependency,
                               nullptr, results);
    if (results.size() != 1)
      return false;
    fn = dyn_cast<FuncDecl>(results[0]);
    if (!fn)
      return false;
  }

  // Check whether the SIL module defines it.  (We need a type for it.)
  SILDeclRef fnRef(fn, SILDeclRef::Kind::Func,
                   ResilienceExpansion::Minimal,
                   /*uncurryLevel*/ 1,
                   /*foreign*/ false);
  SILFunction *silFn = IGF.getSILModule().lookUpFunction(fnRef);
  if (!silFn)
    return false;

  // Check that it returns two size_t's and takes no other arguments.
  auto fnType = silFn->getLoweredFunctionType();
  auto fnConv = silFn->getConventions();
  if (fnType->getParameters().size() != 1)
    return false;
  if (fnConv.getNumDirectSILResults() != 2
      || fnConv.getNumIndirectSILResults() != 0)
    return false;
  if ((fnConv.getDirectSILResults().begin()->getConvention()
       != ResultConvention::Unowned)
      || (std::next(fnConv.getDirectSILResults().begin())->getConvention()
          != ResultConvention::Unowned))
    return false;
  llvm::Function *llvmFn =
    IGF.IGM.getAddrOfSILFunction(silFn, NotForDefinition);
  auto llvmFnTy = llvmFn->getFunctionType();
  if (llvmFnTy->getNumParams() != 1) return false;
  auto returnType = dyn_cast<llvm::StructType>(llvmFnTy->getReturnType());
  if (!returnType ||
      returnType->getNumElements() != 2 ||
      returnType->getElementType(0) != IGF.IGM.SizeTy ||
      returnType->getElementType(1) != IGF.IGM.SizeTy)
    return false;

  // Retain 'self' if necessary.
  if (fnType->getParameters()[0].isConsumed()) {
    IGF.emitNativeStrongRetain(selfValue, IGF.getDefaultAtomicity());
  }

  // Adjust down to the defining subclass type if necessary.
  selfValue = IGF.Builder.CreateBitCast(selfValue, llvmFnTy->getParamType(0));

  // Emit a direct call.
  auto result = IGF.Builder.CreateCall(llvmFn, selfValue);
  result->setCallingConv(llvmFn->getCallingConv());

  // Extract the size and alignment.
  size = IGF.Builder.CreateExtractValue(result, 0, "size");
  alignMask = IGF.Builder.CreateExtractValue(result, 1, "alignMask");
  return true;
}

/// Get the instance size and alignment mask for the given class
/// instance.
static void getInstanceSizeAndAlignMask(IRGenFunction &IGF,
                                        SILType selfType,
                                        ClassDecl *selfClass,
                                        llvm::Value *selfValue,
                                        llvm::Value *&size,
                                        llvm::Value *&alignMask) {
  // Use the magic __getInstanceSizeAndAlignMask method if we can
  // see a declaration of it
  if (getInstanceSizeByMethod(IGF, selfType.getSwiftRValueType(),
                              selfClass, selfValue, size, alignMask))
    return;

  // Try to determine the size of the object we're deallocating.
  auto &info = IGF.IGM.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &layout = info.getLayout(IGF.IGM, selfType);

  // If it's fixed, emit the constant size and alignment mask.
  if (layout.isFixedLayout()) {
    size = layout.emitSize(IGF.IGM);
    alignMask = layout.emitAlignMask(IGF.IGM);
    return;
  }

  // Otherwise, get them from the metadata.
  llvm::Value *metadata =
    emitHeapMetadataRefForHeapObject(IGF, selfValue, selfType);
  std::tie(size, alignMask)
    = emitClassInstanceSizeAndAlignMask(IGF, selfType, metadata);
}

void irgen::emitClassDeallocation(IRGenFunction &IGF, SILType selfType,
                                  llvm::Value *selfValue) {
  auto *theClass = selfType.getClassOrBoundGenericClass();

  llvm::Value *size, *alignMask;
  getInstanceSizeAndAlignMask(IGF, selfType, theClass, selfValue,
                              size, alignMask);

  selfValue = IGF.Builder.CreateBitCast(selfValue, IGF.IGM.RefCountedPtrTy);
  emitDeallocateClassInstance(IGF, selfValue, size, alignMask);
}

void irgen::emitPartialClassDeallocation(IRGenFunction &IGF,
                                         SILType selfType,
                                         llvm::Value *selfValue,
                                         llvm::Value *metadataValue) {
  auto *theClass = selfType.getClassOrBoundGenericClass();

  // Foreign classes should not be freed by sending -release.
  // They should also probably not be freed with object_dispose(),
  // either.
  //
  // However, in practice, the only time we should try to free an
  // instance of a foreign class here is inside an initializer
  // delegating to a factory initializer. In this case, the object
  // was allocated with +allocWithZone:, so calling object_dispose()
  // should be OK.
  if (theClass->getForeignClassKind() == ClassDecl::ForeignKind::RuntimeOnly) {
    selfValue = IGF.Builder.CreateBitCast(selfValue, IGF.IGM.ObjCPtrTy);
    IGF.Builder.CreateCall(IGF.IGM.getObjectDisposeFn(),
                           {selfValue});
    return;
  }

  llvm::Value *size, *alignMask;
  getInstanceSizeAndAlignMask(IGF, selfType, theClass, selfValue,
                              size, alignMask);

  selfValue = IGF.Builder.CreateBitCast(selfValue, IGF.IGM.RefCountedPtrTy);
  emitDeallocatePartialClassInstance(IGF, selfValue, metadataValue,
                                     size, alignMask);
}

llvm::Constant *irgen::tryEmitClassConstantFragileInstanceSize(
                                                        IRGenModule &IGM,
                                                        ClassDecl *Class) {
  auto selfType = getSelfType(Class);
  auto &classTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();

  auto &layout = classTI.getLayout(IGM, selfType);
  if (layout.isFixedLayout())
    return layout.emitSize(IGM);
  
  return nullptr;
}

llvm::Constant *irgen::tryEmitClassConstantFragileInstanceAlignMask(
                                                             IRGenModule &IGM,
                                                             ClassDecl *Class) {
  auto selfType = getSelfType(Class);
  auto &classTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();
  
  auto &layout = classTI.getLayout(IGM, selfType);
  if (layout.isFixedLayout())
    return layout.emitAlignMask(IGM);
  
  return nullptr;
}

/// emitClassDecl - Emit all the declarations associated with this class type.
void IRGenModule::emitClassDecl(ClassDecl *D) {
  PrettyStackTraceDecl prettyStackTrace("emitting class metadata for", D);

  SILType selfType = getSelfType(D);
  auto &classTI = getTypeInfo(selfType).as<ClassTypeInfo>();

  // Emit the class metadata.
  emitClassMetadata(*this, D,
                    classTI.getLayout(*this, selfType),
                    classTI.getClassLayout(*this, selfType));
  emitNestedTypeDecls(D->getMembers());
  emitFieldMetadataRecord(D);
}

namespace {
  typedef std::pair<ClassDecl*, ModuleDecl*> CategoryNameKey;
  /// Used to provide unique names to ObjC categories generated by Swift
  /// extensions. The first category for a class in a module gets the module's
  /// name as its key, e.g., NSObject (MySwiftModule). Another extension of the
  /// same class in the same module gets a category name with a number appended,
  /// e.g., NSObject (MySwiftModule1).
  llvm::DenseMap<CategoryNameKey, unsigned> CategoryCounts;

  /// A class for building ObjC class data (in Objective-C terms, class_ro_t),
  /// category data (category_t), or protocol data (protocol_t).
  class ClassDataBuilder : public ClassMemberVisitor<ClassDataBuilder> {
    IRGenModule &IGM;
    PointerUnion<ClassDecl *, ProtocolDecl *> TheEntity;
    ExtensionDecl *TheExtension;
    const StructLayout *Layout;
    const ClassLayout *FieldLayout;
    
    ClassDecl *getClass() const {
      return TheEntity.get<ClassDecl*>();
    }
    ProtocolDecl *getProtocol() const {
      return TheEntity.get<ProtocolDecl*>();
    }
    
    bool isBuildingClass() const {
      return TheEntity.is<ClassDecl*>() && !TheExtension;
    }
    bool isBuildingCategory() const {
      return TheEntity.is<ClassDecl*>() && TheExtension;
    }
    bool isBuildingProtocol() const {
      return TheEntity.is<ProtocolDecl*>();
    }

    bool HasNonTrivialDestructor = false;
    bool HasNonTrivialConstructor = false;

    class MethodDescriptor {
    public:
      enum class Kind {
        Method,
        IVarInitializer,
        IVarDestroyer,
      };

    private:
      llvm::PointerIntPair<void*, 2, Kind> Data;
      static_assert(llvm::PointerLikeTypeTraits<llvm::Function*>
                      ::NumLowBitsAvailable >= 2,
                    "llvm::Function* isn't adequately aligned");
      static_assert(llvm::PointerLikeTypeTraits<AbstractFunctionDecl*>
                      ::NumLowBitsAvailable >= 2,
                    "AbstractFuncDecl* isn't adequately aligned");

      MethodDescriptor(Kind kind, void *ptr) : Data(ptr, kind) {}

    public:
      MethodDescriptor(AbstractFunctionDecl *method)
          : Data(method, Kind::Method) {
        assert(method && "null method provided");
      }
      static MethodDescriptor getIVarInitializer(llvm::Function *fn) {
        assert(fn && "null impl provided");
        return MethodDescriptor(Kind::IVarInitializer, fn);
      }
      static MethodDescriptor getIVarDestroyer(llvm::Function *fn) {
        assert(fn && "null impl provided");
        return MethodDescriptor(Kind::IVarDestroyer, fn);
      }

      Kind getKind() const { return Data.getInt(); }
      AbstractFunctionDecl *getMethod() {
        assert(getKind() == Kind::Method);
        return static_cast<AbstractFunctionDecl*>(Data.getPointer());
      }
      llvm::Function *getImpl() {
        assert(getKind() != Kind::Method);
        return static_cast<llvm::Function*>(Data.getPointer());
      }
    };

    llvm::SmallString<16> CategoryName;
    SmallVector<VarDecl*, 8> Ivars;
    SmallVector<MethodDescriptor, 16> InstanceMethods;
    SmallVector<MethodDescriptor, 16> ClassMethods;
    SmallVector<MethodDescriptor, 16> OptInstanceMethods;
    SmallVector<MethodDescriptor, 16> OptClassMethods;
    SmallVector<ProtocolDecl*, 4> Protocols;
    SmallVector<VarDecl*, 8> InstanceProperties;
    SmallVector<VarDecl*, 8> ClassProperties;
    
    llvm::Constant *Name = nullptr;
    /// Index of the first non-inherited field in the layout.
    unsigned FirstFieldIndex;
    unsigned NextFieldIndex;

    SmallVectorImpl<MethodDescriptor> &getMethodList(ValueDecl *decl) {
      if (decl->getAttrs().hasAttribute<OptionalAttr>()) {
        if (decl->isStatic()) {
          return OptClassMethods;
        } else {
          return OptInstanceMethods;
        }
      } else {
        if (decl->isStatic()) {
          return ClassMethods;
        } else {
          return InstanceMethods;
        }
      }
    }

  public:
    ClassDataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                     const StructLayout &layout,
                     const ClassLayout &fieldLayout)
        : IGM(IGM), TheEntity(theClass), TheExtension(nullptr),
          Layout(&layout),
          FieldLayout(&fieldLayout)
    {
      FirstFieldIndex = fieldLayout.InheritedStoredProperties.size();
      NextFieldIndex = FirstFieldIndex;

      visitConformances(theClass);
      visitMembers(theClass);

      if (Lowering::usesObjCAllocator(theClass)) {
        addIVarInitializer(); 
        addIVarDestroyer(); 
      }
    }
    
    ClassDataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                     ExtensionDecl *theExtension)
      : IGM(IGM), TheEntity(theClass), TheExtension(theExtension),
        Layout(nullptr), FieldLayout(nullptr)
    {
      FirstFieldIndex = -1;
      NextFieldIndex = -1;

      buildCategoryName(CategoryName);

      visitConformances(theExtension);

      for (Decl *member : TheExtension->getMembers())
        visit(member);
    }
    
    ClassDataBuilder(IRGenModule &IGM, ProtocolDecl *theProtocol)
      : IGM(IGM), TheEntity(theProtocol), TheExtension(nullptr)
    {
      // Gather protocol references for all of the explicitly-specified
      // Objective-C protocol conformances.
      // FIXME: We can't use visitConformances() because there are no
      // conformances for protocols to protocols right now.
      for (ProtocolDecl *p : theProtocol->getInheritedProtocols()) {
        if (!p->isObjC())
          continue;
        // Don't emit the magic AnyObject conformance.
        if (auto known = p->getKnownProtocolKind())
          if (*known == KnownProtocolKind::AnyObject)
            continue;
        Protocols.push_back(p);
      }

      for (Decl *member : theProtocol->getMembers())
        visit(member);
    }

    /// Gather protocol records for all of the explicitly-specified Objective-C
    /// protocol conformances.
    void visitConformances(DeclContext *dc) {
      llvm::SmallSetVector<ProtocolDecl *, 2> protocols;
      for (auto conformance : dc->getLocalConformances(
                                ConformanceLookupKind::OnlyExplicit,
                                nullptr, /*sorted=*/true)) {
        ProtocolDecl *proto = conformance->getProtocol();
        getObjCProtocols(proto, protocols);
      }

      for (ProtocolDecl *proto : protocols) {
        // Don't emit the magic AnyObject conformance.
        if (auto known = proto->getKnownProtocolKind())
          if (*known == KnownProtocolKind::AnyObject)
            continue;

        Protocols.push_back(proto);
      }
    }

    /// Add the protocol to the vector, if it's Objective-C protocol,
    /// or search its superprotocols.
    void getObjCProtocols(ProtocolDecl *proto,
                          llvm::SmallSetVector<ProtocolDecl *, 2> &result) {
      if (proto->isObjC()) {
        result.insert(proto);
      } else {
        for (ProtocolDecl *inherited : proto->getInheritedProtocols()) {
          // Recursively check inherited protocol for objc conformance.
          getObjCProtocols(inherited, result);
        }
      }
    }

    llvm::Constant *getMetaclassRefOrNull(ClassDecl *theClass) {
      if (theClass->isGenericContext() && !theClass->hasClangNode()) {
        return llvm::ConstantPointerNull::get(IGM.ObjCClassPtrTy);
      } else {
        return IGM.getAddrOfMetaclassObject(theClass, NotForDefinition);
      }
    }

    void buildMetaclassStub() {
      assert(Layout && "can't build a metaclass from a category");
      // The isa is the metaclass pointer for the root class.
      auto rootClass = getRootClassForMetaclass(IGM, TheEntity.get<ClassDecl *>());
      auto rootPtr = getMetaclassRefOrNull(rootClass);

      // The superclass of the metaclass is the metaclass of the
      // superclass.  Note that for metaclass stubs, we can always
      // ignore parent contexts and generic arguments.
      //
      // If this class has no formal superclass, then its actual
      // superclass is SwiftObject, i.e. the root class.
      llvm::Constant *superPtr;
      if (getClass()->hasSuperclass()) {
        auto base = getClass()->getSuperclass()->getClassOrBoundGenericClass();
        superPtr = getMetaclassRefOrNull(base);
      } else {
        superPtr = getMetaclassRefOrNull(
          IGM.getObjCRuntimeBaseForSwiftRootClass(getClass()));
      }

      auto dataPtr = emitROData(ForMetaClass);
      dataPtr = llvm::ConstantExpr::getPtrToInt(dataPtr, IGM.IntPtrTy);

      llvm::Constant *fields[] = {
        rootPtr,
        superPtr,
        IGM.getObjCEmptyCachePtr(),
        IGM.getObjCEmptyVTablePtr(),
        dataPtr
      };
      auto init = llvm::ConstantStruct::get(IGM.ObjCClassStructTy,
                                            makeArrayRef(fields));
      auto metaclass =
        cast<llvm::GlobalVariable>(
                     IGM.getAddrOfMetaclassObject(getClass(), ForDefinition));
      metaclass->setInitializer(init);
    }
    
  private:
    void buildCategoryName(SmallVectorImpl<char> &s) {
      llvm::raw_svector_ostream os(s);
      // Find the module the extension is declared in.
      ModuleDecl *TheModule = TheExtension->getParentModule();

      os << TheModule->getName();
      
      unsigned categoryCount = CategoryCounts[{getClass(), TheModule}]++;
      if (categoryCount > 0)
        os << categoryCount;
    }
    
  public:
    llvm::Constant *emitCategory() {
      assert(TheExtension && "can't emit category data for a class");
      ConstantInitBuilder builder(IGM);
      auto fields = builder.beginStruct();

      // struct category_t {
      //   char const *name;
      fields.add(IGM.getAddrOfGlobalString(CategoryName));
      //   const class_t *theClass;
      if (getClass()->hasClangNode())
        fields.add(IGM.getAddrOfObjCClass(getClass(), NotForDefinition));
      else {
        auto type = getSelfType(getClass()).getSwiftRValueType();
        llvm::Constant *metadata =
          tryEmitConstantHeapMetadataRef(IGM, type, /*allowUninit*/ true);
        assert(metadata &&
               "extended objc class doesn't have constant metadata?");
        fields.add(metadata);
      }
      //   const method_list_t *instanceMethods;
      fields.add(buildInstanceMethodList());
      //   const method_list_t *classMethods;
      fields.add(buildClassMethodList());
      //   const protocol_list_t *baseProtocols;
      fields.add(buildProtocolList());
      //   const property_list_t *properties;
      fields.add(buildPropertyList(ForClass));
      //   const property_list_t *classProperties;
      fields.add(buildPropertyList(ForMetaClass));
      //   uint32_t size;
      // FIXME: Clang does this by using non-ad-hoc types for ObjC runtime
      // structures.
      Size size = 7 * IGM.getPointerSize() + Size(4);
      fields.addInt32(size.getValue());
      // };

      assert(fields.getNextOffsetFromGlobal() == size);
      return buildGlobalVariable(fields, "_CATEGORY_");
    }
    
    llvm::Constant *emitProtocol() {
      ConstantInitBuilder builder(IGM);
      auto fields = builder.beginStruct();
      llvm::SmallString<64> nameBuffer;

      assert(isBuildingProtocol() && "not emitting a protocol");
      
      // struct protocol_t {
      //   Class super;
      fields.addNullPointer(IGM.Int8PtrTy);
      //   char const *name;
      fields.add(IGM.getAddrOfGlobalString(getEntityName(nameBuffer)));
      //   const protocol_list_t *baseProtocols;
      fields.add(buildProtocolList());
      //   const method_list_t *requiredInstanceMethods;
      fields.add(buildInstanceMethodList());
      //   const method_list_t *requiredClassMethods;
      fields.add(buildClassMethodList());
      //   const method_list_t *optionalInstanceMethods;
      fields.add(buildOptInstanceMethodList());
      //   const method_list_t *optionalClassMethods;
      fields.add(buildOptClassMethodList());
      //   const property_list_t *properties;
      fields.add(buildPropertyList(ForClass));

      //   uint32_t size;
      // FIXME: Clang does this by using non-ad-hoc types for ObjC runtime
      // structures.
      Size size = 11 * IGM.getPointerSize() + 2 * Size(4);
      fields.addInt32(size.getValue());

      //   uint32_t flags;
      auto flags = ProtocolDescriptorFlags()
        .withSwift(!getProtocol()->hasClangNode())
        .withClassConstraint(ProtocolClassConstraint::Class)
        .withDispatchStrategy(ProtocolDispatchStrategy::ObjC)
        .withSpecialProtocol(getSpecialProtocolID(getProtocol()));
      
      fields.addInt32(flags.getIntValue());
      
      //   const char ** extendedMethodTypes;
      fields.add(buildOptExtendedMethodTypes());
      //   const char *demangledName;
      fields.addNullPointer(IGM.Int8PtrTy);
      //   const property_list_t *classProperties;
      fields.add(buildPropertyList(ForMetaClass));
      // };

      assert(fields.getNextOffsetFromGlobal() == size);
      return buildGlobalVariable(fields, "_PROTOCOL_");
    }

    void emitRODataFields(ConstantStructBuilder &b, ForMetaClass_t forMeta) {
      assert(Layout && "can't emit rodata for a category");

      // struct _class_ro_t {
      //   uint32_t flags;
      b.addInt32(unsigned(buildFlags(forMeta)));

      //   uint32_t instanceStart;
      //   uint32_t instanceSize;
      // The runtime requires that the ivar offsets be initialized to
      // a valid layout of the ivars of this class, bounded by these
      // two values.  If the instanceSize of the superclass equals the
      // stored instanceStart of the subclass, the ivar offsets
      // will not be changed.
      Size instanceStart;
      Size instanceSize;
      if (forMeta) {
        // sizeof(struct class_t)
        instanceSize = Size(5 * IGM.getPointerSize().getValue());
        // historical nonsense
        instanceStart = instanceSize;
      } else {
        instanceSize = Layout->getSize();
        if (Layout->getElements().empty()
            || Layout->getElements().size() == FirstFieldIndex) {
          instanceStart = instanceSize;
        } else if (Layout->getElement(FirstFieldIndex).getKind()
                     == ElementLayout::Kind::Fixed ||
                   Layout->getElement(FirstFieldIndex).getKind()
                     == ElementLayout::Kind::Empty) {
          // FIXME: assumes layout is always sequential!
          instanceStart = Layout->getElement(FirstFieldIndex).getByteOffset();
        } else {
          instanceStart = Size(0);
        }
      }
      b.addInt32(instanceStart.getValue());
      b.addInt32(instanceSize.getValue());

      //   uint32_t reserved;  // only when building for 64bit targets
      if (IGM.getPointerAlignment().getValue() > 4) {
        assert(IGM.getPointerAlignment().getValue() == 8);
        b.addInt32(0);
      }

      //   const uint8_t *ivarLayout;
      // GC/ARC layout.  TODO.
      b.addNullPointer(IGM.Int8PtrTy);

      //   const char *name;
      // It is correct to use the same name for both class and metaclass.
      b.add(buildName());

      //   const method_list_t *baseMethods;
      b.add(forMeta ? buildClassMethodList() : buildInstanceMethodList());

      //   const protocol_list_t *baseProtocols;
      // Apparently, this list is the same in the class and the metaclass.
      b.add(buildProtocolList());

      //   const ivar_list_t *ivars;
      if (forMeta) {
        b.addNullPointer(IGM.Int8PtrTy);
      } else {
        b.add(buildIvarList());
      }

      //   const uint8_t *weakIvarLayout;
      // More GC/ARC layout.  TODO.
      b.addNullPointer(IGM.Int8PtrTy);

      //   const property_list_t *baseProperties;
      b.add(buildPropertyList(forMeta));

      // };
    }
    
    llvm::Constant *emitROData(ForMetaClass_t forMeta) {
      ConstantInitBuilder builder(IGM);
      auto fields = builder.beginStruct();
      emitRODataFields(fields, forMeta);
      
      auto dataSuffix = forMeta ? "_METACLASS_DATA_" : "_DATA_";
      return buildGlobalVariable(fields, dataSuffix);
    }

  private:
    ObjCClassFlags buildFlags(ForMetaClass_t forMeta) {
      ObjCClassFlags flags = ObjCClassFlags::CompiledByARC;

      // Mark metaclasses as appropriate.
      if (forMeta) {
        flags |= ObjCClassFlags::Meta;

      // Non-metaclasses need us to record things whether primitive
      // construction/destructor is trivial.
      } else if (HasNonTrivialDestructor || HasNonTrivialConstructor) {
        flags |= ObjCClassFlags::HasCXXStructors;
        if (!HasNonTrivialConstructor)
          flags |= ObjCClassFlags::HasCXXDestructorOnly;
      }

      // FIXME: set ObjCClassFlags::Hidden when appropriate
      return flags;
    }

    llvm::Constant *buildName() {
      if (Name) return Name;

      // If the class is generic, we'll instantiate its name at runtime.
      if (getClass()->isGenericContext()) {
        Name = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
        return Name;
      }
      
      llvm::SmallString<64> buffer;
      Name = IGM.getAddrOfGlobalString(getClass()->getObjCRuntimeName(buffer));
      return Name;
    }

    llvm::Constant *null() {
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }

    /*** Methods ***********************************************************/

  public:
    /// Methods need to be collected into the appropriate methods list.
    void visitFuncDecl(FuncDecl *method) {
      if (!isBuildingProtocol() &&
          !requiresObjCMethodDescriptor(method)) return;
      
      // getters and setters funcdecls will be handled by their parent
      // var/subscript.
      if (method->isAccessor()) return;

      // Don't emit getters/setters for @NSManaged methods.
      if (method->getAttrs().hasAttribute<NSManagedAttr>()) return;

      getMethodList(method).push_back(method);
    }

    /// Constructors need to be collected into the appropriate methods list.
    void visitConstructorDecl(ConstructorDecl *constructor) {
      if (!isBuildingProtocol() &&
          !requiresObjCMethodDescriptor(constructor)) return;
      getMethodList(constructor).push_back(constructor);
    }

    /// Determine whether the given destructor has an Objective-C
    /// definition.
    bool hasObjCDeallocDefinition(DestructorDecl *destructor) {
      // If we have the destructor body, we know whether SILGen
      // generated a -dealloc body.
      if (auto braceStmt = destructor->getBody())
        return braceStmt->getNumElements() != 0;

      // We don't have a destructor body, so hunt for the SIL function
      // for it.
      SILDeclRef dtorRef(destructor, SILDeclRef::Kind::Deallocator,
                         ResilienceExpansion::Minimal,
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         /*isForeign=*/true);
      if (auto silFn = IGM.getSILModule().lookUpFunction(dtorRef))
        return silFn->isDefinition();

      // The Objective-C thunk was never even declared, so it is not defined.
      return false;
    }

    /// Destructors need to be collected into the instance methods
    /// list 
    void visitDestructorDecl(DestructorDecl *destructor) {
      auto classDecl = cast<ClassDecl>(destructor->getDeclContext());
      if (Lowering::usesObjCAllocator(classDecl) &&
          hasObjCDeallocDefinition(destructor)) {
        InstanceMethods.push_back(destructor);
      }
    }

    void addIVarInitializer() {
      if (auto fn = IGM.getAddrOfIVarInitDestroy(getClass(),
                                                 /*destroy*/ false,
                                                 /*isForeign=*/ true,
                                                 NotForDefinition)) {
        InstanceMethods.push_back(MethodDescriptor::getIVarInitializer(*fn));
        HasNonTrivialConstructor = true;
      }
    }

    void addIVarDestroyer() {
      if (auto fn = IGM.getAddrOfIVarInitDestroy(getClass(),
                                                 /*destroy*/ true,
                                                 /*isForeign=*/ true,
                                                 NotForDefinition)) {
        InstanceMethods.push_back(MethodDescriptor::getIVarDestroyer(*fn));
        HasNonTrivialDestructor = true;
      }
    }

    void buildMethod(ConstantArrayBuilder &descriptors,
                     MethodDescriptor descriptor) {
      switch (descriptor.getKind()) {
      case MethodDescriptor::Kind::Method:
        return buildMethod(descriptors, descriptor.getMethod());
      case MethodDescriptor::Kind::IVarInitializer:
        emitObjCIVarInitDestroyDescriptor(IGM, descriptors, getClass(),
                                          descriptor.getImpl(), false);
        return;
      case MethodDescriptor::Kind::IVarDestroyer:
        emitObjCIVarInitDestroyDescriptor(IGM, descriptors, getClass(),
                                          descriptor.getImpl(), true);
        return;
      }
      llvm_unreachable("bad method descriptor kind");
    }

    void buildMethod(ConstantArrayBuilder &descriptors,
                     AbstractFunctionDecl *method) {
      auto func = dyn_cast<FuncDecl>(method);
      if (!func)
        return emitObjCMethodDescriptor(IGM, descriptors, method);

      switch (func->getAccessorKind()) {
      case AccessorKind::NotAccessor:
        return emitObjCMethodDescriptor(IGM, descriptors, method);

      case AccessorKind::IsGetter:
        return emitObjCGetterDescriptor(IGM, descriptors,
                                        func->getAccessorStorageDecl());

      case AccessorKind::IsSetter:
        return emitObjCSetterDescriptor(IGM, descriptors,
                                        func->getAccessorStorageDecl());

      case AccessorKind::IsWillSet:
      case AccessorKind::IsDidSet:
      case AccessorKind::IsMaterializeForSet:
      case AccessorKind::IsAddressor:
      case AccessorKind::IsMutableAddressor:
        llvm_unreachable("shouldn't be trying to build this accessor");
      }
      llvm_unreachable("bad accessor kind");
    }

  private:
    StringRef chooseNamePrefix(StringRef forClass,
                               StringRef forCategory,
                               StringRef forProtocol) {
      if (isBuildingCategory())
        return forCategory;
      if (isBuildingClass())
        return forClass;
      if (isBuildingProtocol())
        return forProtocol;
      
      llvm_unreachable("not a class, category, or protocol?!");
    }
    
    llvm::Constant *buildClassMethodList() {
      return buildMethodList(ClassMethods,
                             chooseNamePrefix("_CLASS_METHODS_",
                                              "_CATEGORY_CLASS_METHODS_",
                                              "_PROTOCOL_CLASS_METHODS_"));
    }

    llvm::Constant *buildInstanceMethodList() {
      return buildMethodList(InstanceMethods,
                             chooseNamePrefix("_INSTANCE_METHODS_",
                                              "_CATEGORY_INSTANCE_METHODS_",
                                              "_PROTOCOL_INSTANCE_METHODS_"));
    }

    llvm::Constant *buildOptClassMethodList() {
      return buildMethodList(OptClassMethods,
                             "_PROTOCOL_CLASS_METHODS_OPT_");
    }

    llvm::Constant *buildOptInstanceMethodList() {
      return buildMethodList(OptInstanceMethods,
                             "_PROTOCOL_INSTANCE_METHODS_OPT_");
    }

    llvm::Constant *buildOptExtendedMethodTypes() {
      if (!isBuildingProtocol()) return null();

      ConstantInitBuilder builder(IGM);
      auto array = builder.beginArray();

      buildExtMethodTypes(array, InstanceMethods);
      buildExtMethodTypes(array, ClassMethods);
      buildExtMethodTypes(array, OptInstanceMethods);
      buildExtMethodTypes(array, OptClassMethods);

      if (array.empty()) {
        array.abandon();
        return null();
      }

      return buildGlobalVariable(array, "_PROTOCOL_METHOD_TYPES_");
    }

    void buildExtMethodTypes(ConstantArrayBuilder &array,
                             ArrayRef<MethodDescriptor> methods) {
      for (auto descriptor : methods) {
        assert(descriptor.getKind() == MethodDescriptor::Kind::Method &&
               "cannot emit descriptor for non-method");
        auto method = descriptor.getMethod();
        array.add(getMethodTypeExtendedEncoding(IGM, method));
      }
    }

    /// struct method_list_t {
    ///   uint32_t entsize; // runtime uses low bits for its own purposes
    ///   uint32_t count;
    ///   method_t list[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildMethodList(ArrayRef<MethodDescriptor> methods,
                                    StringRef name) {
      return buildOptionalList(methods, 3 * IGM.getPointerSize(), name,
                               [&](ConstantArrayBuilder &descriptors,
                                   MethodDescriptor descriptor) {
        buildMethod(descriptors, descriptor);
      });
    }

    /*** Protocols *********************************************************/

    /// typedef uintptr_t protocol_ref_t;  // protocol_t*, but unremapped
    llvm::Constant *buildProtocolRef(ProtocolDecl *protocol) {
      assert(protocol->isObjC());
      return IGM.getAddrOfObjCProtocolRecord(protocol, NotForDefinition);
    }
    
    /// struct protocol_list_t {
    ///   uintptr_t count;
    ///   protocol_ref_t[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildProtocolList() {
      return buildOptionalList(Protocols, Size(0),
                               chooseNamePrefix("_PROTOCOLS_",
                                                "_CATEGORY_PROTOCOLS_",
                                                "_PROTOCOL_PROTOCOLS_"),
                               [&](ConstantArrayBuilder &descriptors,
                                   ProtocolDecl *protocol) {
        buildProtocol(descriptors, protocol);
      });
    }

    void buildProtocol(ConstantArrayBuilder &array, ProtocolDecl *protocol) {
      array.add(buildProtocolRef(protocol));
    }

    /*** Ivars *************************************************************/

  public:
    /// Variables might be stored or computed.
    void visitVarDecl(VarDecl *var) {
      if (var->hasStorage() && !var->isStatic())
        visitStoredVar(var);
      else
        visitProperty(var);
    }

  private:
    /// Ivars need to be collected in the ivars list, and they also
    /// affect flags.
    void visitStoredVar(VarDecl *var) {
      // FIXME: how to handle ivar extensions in categories?
      if (!Layout)
        return;

      Ivars.push_back(var);

      // Build property accessors for the ivar if necessary.
      visitProperty(var);
    }

    /// struct ivar_t {
    ///   uintptr_t *offset;
    ///   const char *name;
    ///   const char *type;
    ///   uint32_t alignment;    // actually the log2 of the alignment
    ///   uint32_t size;
    /// };
    void buildIvar(ConstantArrayBuilder &ivars, VarDecl *ivar) {
      auto fields = ivars.beginStruct();

      // For now, we never try to emit specialized versions of the
      // metadata statically, so compute the field layout using the
      // originally-declared type.
      SILType fieldType =
          IGM.getLoweredType(IGM.getSILTypes().getAbstractionPattern(ivar),
                             ivar->getDeclContext()
                                 ->mapTypeIntoContext(ivar->getInterfaceType())
                                 ->getCanonicalType());

      assert(Layout && "can't build ivar for category");
      // FIXME: this is not always the right thing to do!
      //auto &elt = Layout->getElement(NextFieldIndex++);
      auto &ivarTI = IGM.getTypeInfo(fieldType);

      llvm::Constant *offsetPtr;
      switch (FieldLayout->AllFieldAccesses[NextFieldIndex++]) {
      case FieldAccess::ConstantDirect:
      case FieldAccess::NonConstantDirect: {
        // If the field offset is fixed relative to the start of the superclass,
        // reference the global from the ivar metadata so that the Objective-C
        // runtime will slide it down.
        auto offsetAddr = IGM.getAddrOfFieldOffset(ivar, /*indirect*/ false,
                                                   NotForDefinition);
        offsetPtr = cast<llvm::Constant>(offsetAddr.getAddress());
        break;
      }
      case FieldAccess::ConstantIndirect:
      case FieldAccess::NonConstantIndirect:
        // Otherwise, swift_initClassMetadata_UniversalStrategy() will point
        // the Objective-C runtime into the field offset vector of the
        // instantiated metadata.
        offsetPtr
          = llvm::ConstantPointerNull::get(IGM.IntPtrTy->getPointerTo());
        break;
      }

      fields.add(offsetPtr);

      // TODO: clang puts this in __TEXT,__objc_methname,cstring_literals
      fields.add(IGM.getAddrOfGlobalString(ivar->getName().str()));

      // TODO: clang puts this in __TEXT,__objc_methtype,cstring_literals
      fields.add(IGM.getAddrOfGlobalString(""));

      Size size;
      Alignment alignment;
      if (auto fixedTI = dyn_cast<FixedTypeInfo>(&ivarTI)) {
        size = fixedTI->getFixedSize();
        alignment = fixedTI->getFixedAlignment();
      } else {
        // FIXME: set something up to fill these in at runtime!
        size = Size(0);
        alignment = Alignment(1);
      }

      // If the size is larger than we can represent in 32-bits,
      // complain about the unimplementable ivar.
      if (uint32_t(size.getValue()) != size.getValue()) {
        IGM.error(ivar->getLoc(),
                  "ivar size (" + Twine(size.getValue()) +
                  " bytes) overflows Objective-C ivar layout");
        size = Size(0);
      }
      fields.addInt32(alignment.log2());
      fields.addInt32(size.getValue());

      fields.finishAndAddTo(ivars);
    }

    /// struct ivar_list_t {
    ///   uint32_t entsize;
    ///   uint32_t count;
    ///   ivar_t list[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildIvarList() {
      Size eltSize = 3 * IGM.getPointerSize() + Size(8);
      return buildOptionalList(Ivars, eltSize, "_IVARS_",
                               [&](ConstantArrayBuilder &descriptors,
                                   VarDecl *ivar) {
        buildIvar(descriptors, ivar);
      });
    }

    /*** Properties ********************************************************/

    /// Properties need to be collected in the properties list.
    void visitProperty(VarDecl *var) {
      if (requiresObjCPropertyDescriptor(IGM, var)) {
        if (var->isStatic()) {
          ClassProperties.push_back(var);
        } else {
          InstanceProperties.push_back(var);
        }

        // Don't emit descriptors for properties without accessors.
        auto getter = var->getGetter();
        if (!getter)
          return;

        // Don't emit getter/setter descriptors for @NSManaged properties.
        if (var->getAttrs().hasAttribute<NSManagedAttr>())
          return;

        auto &methods = getMethodList(var);
        methods.push_back(getter);

        if (auto setter = var->getSetter())
          methods.push_back(setter);
      }
    }
    
    /// Build the property attribute string for a property decl.
    void buildPropertyAttributes(VarDecl *prop, SmallVectorImpl<char> &out) {
      llvm::raw_svector_ostream outs(out);

      auto propTy = prop->getInterfaceType()->getReferenceStorageReferent();

      // Emit the type encoding for the property.
      outs << 'T';
      
      std::string typeEnc;
      getObjCEncodingForPropertyType(IGM, prop, typeEnc);
      outs << typeEnc;
      
      // Emit other attributes.

      // All Swift properties are (nonatomic).
      outs << ",N";
      
      // @NSManaged properties are @dynamic.
      if (prop->getAttrs().hasAttribute<NSManagedAttr>())
        outs << ",D";
      
      auto isObject = prop->getDeclContext()->mapTypeIntoContext(propTy)
          ->hasRetainablePointerRepresentation();
      auto hasObjectEncoding = typeEnc[0] == '@';
      
      // Determine the assignment semantics.
      // Get-only properties are (readonly).
      if (!prop->isSettable(prop->getDeclContext()))
        outs << ",R";
      // Weak and Unowned properties are (weak).
      else if (prop->getAttrs().hasAttribute<OwnershipAttr>())
        outs << ",W";
      // If the property is @NSCopying, or is bridged to a value class, the
      // property is (copy).
      else if (prop->getAttrs().hasAttribute<NSCopyingAttr>()
               || (hasObjectEncoding && !isObject))
        outs << ",C";
      // If it's of a managed object type, it is (retain).
      else if (isObject)
        outs << ",&";
      // Otherwise, the property is of a value type, so it is
      // the default (assign).
      else
        (void)0;
      
      // If the property has storage, emit the ivar name last.
      if (prop->hasStorage())
        outs << ",V" << prop->getName();
    }

    /// struct property_t {
    ///   const char *name;
    ///   const char *attributes;
    /// };
    void buildProperty(ConstantArrayBuilder &properties, VarDecl *prop) {
      llvm::SmallString<16> propertyAttributes;
      buildPropertyAttributes(prop, propertyAttributes);
      
      auto fields = properties.beginStruct();
      fields.add(IGM.getAddrOfGlobalString(prop->getObjCPropertyName().str()));
      fields.add(IGM.getAddrOfGlobalString(propertyAttributes));
      fields.finishAndAddTo(properties);
    }

    /// struct property_list_t {
    ///   uint32_t entsize;
    ///   uint32_t count;
    ///   property_t list[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildPropertyList(ForMetaClass_t classOrMeta) {
      if (classOrMeta == ForClass) {
        return buildPropertyList(InstanceProperties,
                                 chooseNamePrefix("_PROPERTIES_",
                                                  "_CATEGORY_PROPERTIES_",
                                                  "_PROTOCOL_PROPERTIES_"));
      }

      // Older OSs' libobjcs can't handle class property data.
      if ((IGM.Triple.isMacOSX() && IGM.Triple.isMacOSXVersionLT(10, 11)) ||
          (IGM.Triple.isiOS() && IGM.Triple.isOSVersionLT(9))) {
        return null();
      }

      return buildPropertyList(ClassProperties,
                               chooseNamePrefix("_CLASS_PROPERTIES_",
                                                "_CATEGORY_CLASS_PROPERTIES_",
                                                "_PROTOCOL_CLASS_PROPERTIES_"));
    }

    llvm::Constant *buildPropertyList(ArrayRef<VarDecl*> properties,
                                      StringRef namePrefix) {
      Size eltSize = 2 * IGM.getPointerSize();
      return buildOptionalList(properties, eltSize, namePrefix,
                               [&](ConstantArrayBuilder &descriptors,
                                   VarDecl *property) {
        buildProperty(descriptors, property);
      });
    }

    /*** General ***********************************************************/

    /// Build a list structure from the given array of objects.
    /// If the array is empty, use null.  The assumption is that every
    /// initializer has the same size.
    ///
    /// \param optionalEltSize - if non-zero, a size which needs
    ///   to be placed in the list header
    template <class C, class Fn>
    llvm::Constant *buildOptionalList(const C &objects,
                                      Size optionalEltSize,
                                      StringRef nameBase,
                                      Fn &&buildElement) {
      if (objects.empty())
        return null();

      ConstantInitBuilder builder(IGM);
      auto fields = builder.beginStruct();

      llvm::IntegerType *countType;
      // In all of the foo_list_t structs, either:
      //   - there's a 32-bit entry size and a 32-bit count or
      //   - there's no entry size and a uintptr_t count.
      if (!optionalEltSize.isZero()) {
        fields.addInt32(optionalEltSize.getValue());
        countType = IGM.Int32Ty;
      } else {
        countType = IGM.IntPtrTy;
      }
      auto countPosition = fields.addPlaceholder();

      auto array = fields.beginArray();
      for (auto &element : objects) {
        buildElement(array, element);
      }

      // If we didn't actually make anything, declare that we're done.
      if (array.empty()) {
        array.abandon();
        fields.abandon();
        return null();
      }

      // Otherwise, remember the size of the array and fill the
      // placeholder with it.
      auto count = array.size();
      array.finishAndAddTo(fields);

      fields.fillPlaceholderWithInt(countPosition, countType, count);

      return buildGlobalVariable(fields, nameBase);
    }
    
    /// Get the name of the class or protocol to mangle into the ObjC symbol
    /// name.
    StringRef getEntityName(llvm::SmallVectorImpl<char> &buffer) const {
      if (auto theClass = TheEntity.dyn_cast<ClassDecl*>()) {
        return theClass->getObjCRuntimeName(buffer);
      }
      
      if (auto theProtocol = TheEntity.dyn_cast<ProtocolDecl*>()) {
        return theProtocol->getObjCRuntimeName(buffer);
      }
      
      llvm_unreachable("not a class or protocol?!");
    }

    /// Build a private global variable as a structure containing the
    /// given fields.
    template <class B>
    llvm::Constant *buildGlobalVariable(B &fields, StringRef nameBase) {
      llvm::SmallString<64> nameBuffer;
      auto var =
        fields.finishAndCreateGlobal(Twine(nameBase) 
                                      + getEntityName(nameBuffer)
                                      + (TheExtension
                                           ? Twine("_$_") + CategoryName.str()
                                           : Twine()),
                                     IGM.getPointerAlignment(),
                                     /*constant*/ true,
                                     llvm::GlobalVariable::PrivateLinkage);
      switch (IGM.TargetInfo.OutputObjectFormat) {
      case llvm::Triple::MachO:
        var->setSection("__DATA, __objc_const");
        break;
      case llvm::Triple::ELF:
        var->setSection(".data");
        break;
      default:
        llvm_unreachable("Don't know how to emit private global constants for "
                         "the selected object format.");
      }
      return var;
    }

  public:
    /// Member types don't get any representation.
    /// Maybe this should change for reflection purposes?
    void visitTypeDecl(TypeDecl *type) {}

    /// Pattern-bindings don't require anything special as long as
    /// these initializations are performed in the constructor, not
    /// .cxx_construct.
    void visitPatternBindingDecl(PatternBindingDecl *binding) {}

    /// Subscripts should probably be collected in extended metadata.
    void visitSubscriptDecl(SubscriptDecl *subscript) {
      if (!requiresObjCSubscriptDescriptor(IGM, subscript)) return;

      auto getter = subscript->getGetter();
      if (!getter) return;

      auto &methods = getMethodList(subscript);
      methods.push_back(getter);

      if (auto setter = subscript->getSetter())
        methods.push_back(setter);
    }
  };
} // end anonymous namespace

/// Emit the private data (RO-data) associated with a class.
llvm::Constant *irgen::emitClassPrivateData(IRGenModule &IGM,
                                            ClassDecl *cls) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  SILType selfType = getSelfType(cls);
  auto &classTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(IGM, selfType);
  auto &fieldLayout = classTI.getClassLayout(IGM, selfType);
  ClassDataBuilder builder(IGM, cls, layout, fieldLayout);

  // First, build the metaclass object.
  builder.buildMetaclassStub();

  // Then build the class RO-data.
  return builder.emitROData(ForClass);
}

std::pair<Size, Size>
irgen::emitClassPrivateDataFields(IRGenModule &IGM,
                                  ConstantStructBuilder &init,
                                  ClassDecl *cls) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");

  SILType selfType = getSelfType(cls);
  auto &classTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(IGM, selfType);
  auto &fieldLayout = classTI.getClassLayout(IGM, selfType);

  ClassDataBuilder builder(IGM, cls, layout, fieldLayout);

  Size startOfClassRO = init.getNextOffsetFromGlobal();
  assert(startOfClassRO.isMultipleOf(IGM.getPointerSize()));
  {
    auto classRO = init.beginStruct();
    builder.emitRODataFields(classRO, ForClass);
    classRO.finishAndAddTo(init);
  }

  Size startOfMetaclassRO = init.getNextOffsetFromGlobal();
  assert(startOfMetaclassRO.isMultipleOf(IGM.getPointerSize()));
  {
    auto classRO = init.beginStruct();
    builder.emitRODataFields(classRO, ForMetaClass);
    classRO.finishAndAddTo(init);
  }

  return std::make_pair(startOfClassRO, startOfMetaclassRO);
}

/// Emit the metadata for an ObjC category.
llvm::Constant *irgen::emitCategoryData(IRGenModule &IGM,
                                        ExtensionDecl *ext) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  ClassDecl *cls = ext->getAsClassOrClassExtensionContext();
  assert(cls && "generating category metadata for a non-class extension");
  
  ClassDataBuilder builder(IGM, cls, ext);
  
  return builder.emitCategory();
}
  
/// Emit the metadata for an ObjC protocol.
llvm::Constant *irgen::emitObjCProtocolData(IRGenModule &IGM,
                                            ProtocolDecl *proto) {
  assert(proto->isObjC() && "not an objc protocol");
  ClassDataBuilder builder(IGM, proto);
  return builder.emitProtocol();
}

const TypeInfo *
TypeConverter::convertClassType(CanType type, ClassDecl *D) {
  llvm::StructType *ST = IGM.createNominalType(type);
  llvm::PointerType *irType = ST->getPointerTo();
  ReferenceCounting refcount = ::getReferenceCountingForClass(IGM, D);
  
  SpareBitVector spareBits;
  
  // Classes known to be implemented in Swift can be assumed not to have tagged
  // pointer representations, so we can use spare bits for enum layout with
  // them. We can't make that assumption about imported ObjC types.
  if (D->hasClangNode() && IGM.TargetInfo.hasObjCTaggedPointers())
    spareBits.appendClearBits(IGM.getPointerSize().getValueInBits());
  else
    spareBits = IGM.getHeapObjectSpareBits();
  
  return new ClassTypeInfo(irType, IGM.getPointerSize(),
                           std::move(spareBits),
                           IGM.getPointerAlignment(),
                           D, refcount);
}

/// Lazily declare a fake-looking class to represent an ObjC runtime base class.
ClassDecl *IRGenModule::getObjCRuntimeBaseClass(Identifier name,
                                                Identifier objcName) {
  auto found = SwiftRootClasses.find(name);
  if (found != SwiftRootClasses.end())
    return found->second;

  // Make a really fake-looking class.
  auto SwiftRootClass = new (Context) ClassDecl(SourceLoc(), name, SourceLoc(),
                                           MutableArrayRef<TypeLoc>(),
                                           /*generics*/ nullptr,
                                           Context.TheBuiltinModule);
  SwiftRootClass->computeType();
  SwiftRootClass->setIsObjC(true);
  SwiftRootClass->getAttrs().add(ObjCAttr::createNullary(Context, objcName,
    /*isNameImplicit=*/true));
  SwiftRootClass->setImplicit();
  SwiftRootClass->setAccessibility(Accessibility::Open);
  
  SwiftRootClasses.insert({name, SwiftRootClass});
  return SwiftRootClass;
}

/// Lazily declare the ObjC runtime base class for a Swift root class.
ClassDecl *
IRGenModule::getObjCRuntimeBaseForSwiftRootClass(ClassDecl *theClass) {
  assert(!theClass->hasSuperclass() && "must pass a root class");
  
  Identifier name;
  // If the class declares its own ObjC runtime base, use it.
  if (auto baseAttr = theClass->getAttrs()
        .getAttribute<SwiftNativeObjCRuntimeBaseAttr>()) {
    name = baseAttr->BaseClassName;
  } else {
    // Otherwise, use the standard SwiftObject class.
    name = Context.Id_SwiftObject;
  }
  return getObjCRuntimeBaseClass(name, name);
}

ClassDecl *irgen::getRootClassForMetaclass(IRGenModule &IGM, ClassDecl *C) {
  while (auto superclass = C->getSuperclass())
    C = superclass->getClassOrBoundGenericClass();

  // If the formal root class is imported from Objective-C, then
  // we should use that.  For a class that's really implemented in
  // Objective-C, this is obviously right.  For a class that's
  // really implemented in Swift, but that we're importing via an
  // Objective-C interface, this would be wrong --- except such a
  // class can never be a formal root class, because a Swift class
  // without a formal superclass will actually be parented by
  // SwiftObject (or maybe eventually something else like it),
  // which will be visible in the Objective-C type system.
  if (C->hasClangNode()) return C;
  
  // FIXME: If the root class specifies its own runtime ObjC base class,
  // assume that that base class ultimately inherits NSObject.
  if (C->getAttrs().hasAttribute<SwiftNativeObjCRuntimeBaseAttr>())
    return IGM.getObjCRuntimeBaseClass(
             IGM.Context.getSwiftId(KnownFoundationEntity::NSObject),
             IGM.Context.getIdentifier("NSObject"));

  return IGM.getObjCRuntimeBaseClass(IGM.Context.Id_SwiftObject,
                                     IGM.Context.Id_SwiftObject);
}

bool irgen::doesClassMetadataRequireDynamicInitialization(IRGenModule &IGM,
                                                          ClassDecl *theClass) {
  // Classes imported from Objective-C never requires dynamic initialization.
  if (theClass->hasClangNode())
    return false;

  SILType selfType = getSelfType(theClass);
  auto &selfTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();

  auto &layout = selfTI.getClassLayout(IGM, selfType);
  return layout.MetadataRequiresDynamicInitialization;
}

bool irgen::doesConformanceReferenceNominalTypeDescriptor(IRGenModule &IGM,
                                                       CanType conformingType) {
  NominalTypeDecl *nom = conformingType->getAnyNominal();
  ClassDecl *clas = dyn_cast<ClassDecl>(nom);
  if (nom->isGenericContext() && (!clas || !clas->usesObjCGenericsModel()))
    return true;

  if (clas && doesClassMetadataRequireDynamicInitialization(IGM, clas))
    return true;

  return false;
}

