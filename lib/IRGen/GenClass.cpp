//===--- GenClass.cpp - Swift IR Generation For 'class' Types -----------===//
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
//  This file implements IR generation for class types.
//
//===----------------------------------------------------------------------===//

#include "GenClass.h"

#include "swift/ABI/Class.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/AST/Attr.h"
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
  // If the root class is implemented in swift, then we have a swift
  // refcount; otherwise, we have an ObjC refcount.
  if (hasKnownSwiftImplementation(IGM, getRootClass(theClass)))
    return ReferenceCounting::Native;
  return ReferenceCounting::ObjC;
}

/// What isa encoding mechanism does a type have?
IsaEncoding irgen::getIsaEncodingForType(IRGenModule &IGM,
                                         CanType type) {
  if (auto theClass = type->getClassOrBoundGenericClass()) {
    // We can access the isas of pure Swift classes directly.
    if (hasKnownSwiftImplementation(IGM, getRootClass(theClass)))
      return IsaEncoding::Pointer;
    // For ObjC or mixed classes, we need to use object_getClass.
    return IsaEncoding::ObjC;
  }
  // Non-class heap objects should be pure Swift, so we can access their isas
  // directly.
  return IsaEncoding::Pointer;
}

/// Different policies for accessing a physical field.
enum class FieldAccess : uint8_t {
  /// Instance variable offsets are constant.
  ConstantDirect,

  /// Instance variable offsets must be loaded from "direct offset"
  /// global variables.
  NonConstantDirect,

  /// Instance variable offsets are kept in fields in metadata, but
  /// the offsets of those fields within the metadata are constant.
  ConstantIndirect,

  /// Instance variable offsets are kept in fields in metadata, and
  /// the offsets of those fields within the metadata must be loaded
  /// from "indirect offset" global variables.
  NonConstantIndirect
};

namespace {
  class FieldEntry {
    llvm::PointerIntPair<VarDecl*, 2, FieldAccess> VarAndAccess;
  public:
    FieldEntry(VarDecl *var, FieldAccess access)
      : VarAndAccess(var, access) {}

    VarDecl *getVar() const {
      return VarAndAccess.getPointer();
    }
    FieldAccess getAccess() const {
      return VarAndAccess.getInt();
    }
  };

  /// Layout information for class types.
  class ClassTypeInfo : public HeapTypeInfo<ClassTypeInfo> {
    ClassDecl *TheClass;
    mutable StructLayout *Layout;
    /// Lazily-initialized array of all fragile stored properties in the class
    /// (including superclass stored properties).
    mutable ArrayRef<VarDecl*> AllStoredProperties;
    /// Lazily-initialized array of all fragile stored properties inherited from
    /// superclasses.
    mutable ArrayRef<VarDecl*> InheritedStoredProperties;

    /// Can we use swift reference-counting, or do we have to use
    /// objc_retain/release?
    const ReferenceCounting Refcount;
    
    void generateLayout(IRGenModule &IGM) const;

  public:
    ClassTypeInfo(llvm::PointerType *irType, Size size,
                  SpareBitVector spareBits, Alignment align,
                  ClassDecl *D, ReferenceCounting refcount)
      : HeapTypeInfo(irType, size, std::move(spareBits), align), TheClass(D),
        Layout(nullptr), Refcount(refcount) {}

    ReferenceCounting getReferenceCounting() const {
      return Refcount;
    }

    ~ClassTypeInfo() {
      delete Layout;
    }

    ClassDecl *getClass() const { return TheClass; }

    const StructLayout &getLayout(IRGenModule &IGM) const;
    ArrayRef<VarDecl*> getAllStoredProperties(IRGenModule &IGM) const;
    ArrayRef<VarDecl*> getInheritedStoredProperties(IRGenModule &IGM) const;

    Alignment getHeapAlignment(IRGenModule &IGM) const {
      return getLayout(IGM).getAlignment();
    }
    ArrayRef<ElementLayout> getElements(IRGenModule &IGM) const {
      return getLayout(IGM).getElements();
    }
  };

  /// A class for computing properties of the instance-variable layout
  /// of a class.  TODO: cache the results!
  class LayoutClass {
    IRGenModule &IGM;

    ClassDecl *Root;
    SmallVector<FieldEntry, 8> Fields;

    bool IsMetadataResilient = false;
    bool IsObjectResilient = false;
    bool IsObjectGenericallyArranged = false;

    ResilienceScope Resilience;

  public:
    LayoutClass(IRGenModule &IGM, ResilienceScope resilience,
                ClassDecl *theClass, SILType type)
        : IGM(IGM), Resilience(resilience) {
      layout(theClass, type);
    }

    /// The root class for purposes of metaclass objects.
    ClassDecl *getRootClassForMetaclass() const {
      // If the formal root class is imported from Objective-C, then
      // we should use that.  For a class that's really implemented in
      // Objective-C, this is obviously right.  For a class that's
      // really implemented in Swift, but that we're importing via an
      // Objective-C interface, this would be wrong --- except such a
      // class can never be a formal root class, because a Swift class
      // without a formal superclass will actually be parented by
      // SwiftObject (or maybe eventually something else like it),
      // which will be visible in the Objective-C type system.
      if (Root->hasClangNode()) return Root;

      return IGM.getSwiftRootClass();
    }

    const FieldEntry &getFieldEntry(VarDecl *field) const {
      for (auto &entry : Fields)
        if (entry.getVar() == field)
          return entry;
      llvm_unreachable("no entry for field!");
    }

  private:
    void layout(ClassDecl *theClass, SILType type) {
      // First, collect information about the superclass.
      if (theClass->hasSuperclass()) {
        SILType superclassType = type.getSuperclass(nullptr);
        auto superclass = superclassType.getClassOrBoundGenericClass();
        assert(superclass);
        layout(superclass, superclassType);
      } else {
        Root = theClass;
      }

      // If the class is resilient, then it may have fields we can't
      // see, and all subsequent fields are *at least* resilient ---
      // and if the class is generic, then it may have
      // dependently-sized fields, and we'll be in the worst case.
      bool isClassResilient = IGM.isResilient(theClass, Resilience);
      if (isClassResilient) {
        IsMetadataResilient = true;
        IsObjectResilient = true;
      }

      // Okay, make entries for all the physical fields we know about.
      for (auto member : theClass->getMembers()) {
        auto var = dyn_cast<VarDecl>(member);
        if (!var) continue;

        // Skip properties that we have to access logically.
        assert(isClassResilient || !IGM.isResilient(var, Resilience));
        if (!var->hasStorage())
          continue;

        // Adjust based on the type of this field.
        // FIXME: this algorithm is assuming that fields are laid out
        // in declaration order.
        adjustAccessAfterField(var, type);

        Fields.push_back(FieldEntry(var, getCurFieldAccess()));
      }
    }

    FieldAccess getCurFieldAccess() const {
      if (IsObjectGenericallyArranged) {
        if (IsMetadataResilient) {
          return FieldAccess::NonConstantIndirect;
        } else {
          return FieldAccess::ConstantIndirect;
        }
      } else {
        if (IsObjectResilient) {
          return FieldAccess::NonConstantDirect;
        } else {
          return FieldAccess::ConstantDirect;
        }
      }
    }

    void adjustAccessAfterField(VarDecl *var, SILType classType) {
      if (!var->hasStorage()) return;

      SILType fieldType = classType.getFieldType(var, *IGM.SILMod);
      switch (IGM.classifyTypeSize(fieldType, ResilienceScope::Local)) {
      case ObjectSize::Fixed:
        return;
      case ObjectSize::Resilient:
        IsObjectResilient = true;
        return;
      case ObjectSize::Dependent:
        IsObjectResilient = IsObjectGenericallyArranged = true;
        return;
      }
      llvm_unreachable("bad ObjectSize value");
    }
  };
}  // end anonymous namespace.

/// Return the lowered type for the class's 'self' type within its context.
static SILType getSelfType(ClassDecl *base) {
  auto loweredTy = base->getDeclaredTypeInContext()->getCanonicalType();
  return SILType::getPrimitiveObjectType(loweredTy);
}

/// Return the type info for the class's 'self' type within its context.
static const ClassTypeInfo &getSelfTypeInfo(IRGenModule &IGM, ClassDecl *base) {
  return IGM.getTypeInfo(getSelfType(base)).as<ClassTypeInfo>();
}

/// Return the index of the given field within the class.
static unsigned getFieldIndex(IRGenModule &IGM,
                              ClassDecl *base, VarDecl *target) {
  // FIXME: This is algorithmically terrible.
  auto &ti = getSelfTypeInfo(IGM, base);
  
  auto props = ti.getAllStoredProperties(IGM);
  auto found = std::find(props.begin(), props.end(), target);
  assert(found != props.end() && "didn't find field in type?!");
  return found - props.begin();
}

namespace {
  class ClassLayoutBuilder : public StructLayoutBuilder {
    SmallVector<ElementLayout, 8> Elements;
    SmallVector<VarDecl*, 8> AllStoredProperties;
    unsigned NumInherited = 0;
  public:
    ClassLayoutBuilder(IRGenModule &IGM, ClassDecl *theClass)
      : StructLayoutBuilder(IGM)
    {
      // Start by adding a heap header.
      addHeapHeader();

      // Next, add the fields for the given class.
      addFieldsForClass(theClass, getSelfType(theClass));
      
      // Add these fields to the builder.
      addFields(Elements, LayoutStrategy::Universal);
    }

    /// Return the element layouts.
    ArrayRef<ElementLayout> getElements() const {
      return Elements;
    }
    
    /// Return the full list of stored properties.
    ArrayRef<VarDecl *> getAllStoredProperties() const {
      return AllStoredProperties;
    }

    /// Return the inherited stored property count.
    unsigned getNumInherited() const {
      return NumInherited;
    }
  private:
    void addFieldsForClass(ClassDecl *theClass,
                           SILType classType) {
      if (theClass->hasSuperclass()) {
        // TODO: apply substitutions when computing base-class layouts!
        SILType superclassType = classType.getSuperclass(nullptr);
        auto superclass = superclassType.getClassOrBoundGenericClass();
        assert(superclass);

        // Recur.
        addFieldsForClass(superclass, superclassType);
        // Count the fields we got from the superclass.
        NumInherited = Elements.size();
      }

      // Collect fields from this class and add them to the layout as a chunk.
      addDirectFieldsFromClass(theClass, classType);
    }

    void addDirectFieldsFromClass(ClassDecl *theClass,
                                  SILType classType) {
      bool complainedAboutUnimplementedLayout = false;
      for (VarDecl *var : theClass->getStoredProperties()) {
        SILType type = classType.getFieldType(var, *IGM.SILMod);
        auto &eltType = IGM.getTypeInfo(type);

        // FIXME: Type-parameter-dependent field layout isn't fully
        // implemented yet for ObjC-derived classes.
        if (theClass->isObjC()
            && !eltType.isFixedSize()
            && !IGM.Opts.EnableDynamicValueTypeLayout) {
          if (!complainedAboutUnimplementedLayout) {
            IGM.unimplemented(var->getLoc(),
                              "non-fixed class layout in ObjC-derived classes");
            complainedAboutUnimplementedLayout = true;
          }
        }
        
        Elements.push_back(ElementLayout::getIncomplete(eltType));
        AllStoredProperties.push_back(var);
      }
    }
  };
}

void ClassTypeInfo::generateLayout(IRGenModule &IGM) const {
  assert(!Layout && AllStoredProperties.empty() && "already generated layout");

  // Add the heap header.
  ClassLayoutBuilder builder(IGM, getClass());
  
  // Set the body of the class type.
  auto classPtrTy = cast<llvm::PointerType>(getStorageType());
  auto classTy = cast<llvm::StructType>(classPtrTy->getElementType());
  builder.setAsBodyOfStruct(classTy);
  
  // Record the layout.
  Layout = new StructLayout(builder, classTy, builder.getElements());
  AllStoredProperties
    = IGM.Context.AllocateCopy(builder.getAllStoredProperties());
  InheritedStoredProperties
    = AllStoredProperties.slice(0, builder.getNumInherited());
}

const StructLayout &ClassTypeInfo::getLayout(IRGenModule &IGM) const {
  // Return the cached layout if available.
  if (Layout) return *Layout;

  generateLayout(IGM);
  return *Layout;
}

ArrayRef<VarDecl*>
ClassTypeInfo::getAllStoredProperties(IRGenModule &IGM) const {
  // Return the cached layout if available.
  if (Layout)
    return AllStoredProperties;
  
  generateLayout(IGM);
  return AllStoredProperties;
}

ArrayRef<VarDecl*>
ClassTypeInfo::getInheritedStoredProperties(IRGenModule &IGM) const {
  // Return the cached layout if available.
  if (Layout)
    return InheritedStoredProperties;
  
  generateLayout(IGM);
  return InheritedStoredProperties;
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
    IGF.getTypeInfo(baseType.getFieldType(field, *IGF.IGM.SILMod));
  auto addr = IGF.emitByteOffsetGEP(base, offset, fieldTI,
                              base->getName() + "." + field->getName().str());
  return OwnedAddress(addr, base);
}

llvm::Constant *irgen::tryEmitClassConstantFragileFieldOffset(IRGenModule &IGM,
                                                            ClassDecl *theClass,
                                                            VarDecl *field) {
  assert(field->hasStorage());
  // FIXME: This field index computation is an ugly hack.
  auto &ti = getSelfTypeInfo(IGM, theClass);

  unsigned fieldIndex = getFieldIndex(IGM, theClass, field);
  auto &element = ti.getElements(IGM)[fieldIndex];
  if (element.getKind() == ElementLayout::Kind::Fixed)
    return IGM.getSize(element.getByteOffset());
  return nullptr;
}

OwnedAddress irgen::projectPhysicalClassMemberAddress(IRGenFunction &IGF,
                                                      llvm::Value *base,
                                                      SILType baseType,
                                                      SILType fieldType,
                                                      VarDecl *field) {
  // If the field is empty, its address doesn't matter.
  auto &fieldTI = IGF.getTypeInfo(fieldType);
  if (fieldTI.isKnownEmpty()) {
    return OwnedAddress(fieldTI.getUndefAddress(), base);
  }
  
  auto &baseClassTI = IGF.getTypeInfo(baseType).as<ClassTypeInfo>();
  ClassDecl *baseClass = baseType.getClassOrBoundGenericClass();
  
  // TODO: Lay out the class based on the substituted baseType rather than
  // the generic type. Doing this requires that we also handle
  // specialized layout in ClassTypeInfo.
  LayoutClass layout(IGF.IGM, ResilienceScope::Local, baseClass,
                     getSelfType(baseClass) /* TODO: should be baseType */);
  
  auto &entry = layout.getFieldEntry(field);
  switch (entry.getAccess()) {
  case FieldAccess::ConstantDirect: {
    // FIXME: This field index computation is an ugly hack.
    unsigned fieldIndex = getFieldIndex(IGF.IGM, baseClass, field);

    Address baseAddr(base, baseClassTI.getHeapAlignment(IGF.IGM));
    auto &element = baseClassTI.getElements(IGF.IGM)[fieldIndex];
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

/// Emit an allocation of a class.
llvm::Value *irgen::emitClassAllocation(IRGenFunction &IGF, SILType selfType,
                                        bool objc) {
  auto &classTI = IGF.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto classType = selfType.getSwiftRValueType();

  // If we need to use Objective-C allocation, do so.
  // If the root class isn't known to use the Swift allocator, we need
  // to call [self alloc].
  if (objc) {
    llvm::Value *metadata =
      emitClassHeapMetadataRef(IGF, classType, MetadataValueType::ObjCClass,
                               /*allow uninitialized*/ true);
    return emitObjCAllocObjectCall(IGF, metadata, selfType.getSwiftRValueType());
  }

  llvm::Value *metadata =
    emitClassHeapMetadataRef(IGF, classType, MetadataValueType::TypeMetadata);

  // FIXME: Long-term, we clearly need a specialized runtime entry point.
  llvm::Value *size, *alignMask;
  std::tie(size, alignMask)
    = emitClassFragileInstanceSizeAndAlignMask(IGF,
                                   selfType.getClassOrBoundGenericClass(),
                                   metadata);
  
  llvm::Value *val = IGF.emitAllocObjectCall(metadata, size, alignMask,
                                             "reference.new");
  auto &layout = classTI.getLayout(IGF.IGM);
  llvm::Type *destType = layout.getType()->getPointerTo();
  return IGF.Builder.CreateBitCast(val, destType);
}

llvm::Value *irgen::emitClassAllocationDynamic(IRGenFunction &IGF, 
                                               llvm::Value *metadata,
                                               SILType selfType,
                                               bool objc) {
  // If we need to use Objective-C allocation, do so.
  if (objc) {
    return emitObjCAllocObjectCall(IGF, metadata, 
                                   selfType.getSwiftRValueType());
  }

  // Otherwise, allocate using Swift's routines.
  llvm::Value *size, *alignMask;
  std::tie(size, alignMask)
    = emitClassResilientInstanceSizeAndAlignMask(IGF,
                                   selfType.getClassOrBoundGenericClass(),
                                   metadata);
  
  llvm::Value *val = IGF.emitAllocObjectCall(metadata, size, alignMask,
                                             "reference.new");
  auto &classTI = IGF.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(IGF.IGM);
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
  SILFunction *silFn; {
    llvm::SmallString<32> name;
    fnRef.mangle(name);
    silFn = IGF.IGM.SILMod->lookUpFunction(name);
    if (!silFn)
      return false;
  }

  // Check that it returns two size_t's and takes no other arguments.
  auto fnType = silFn->getLoweredFunctionType();
  if (fnType->getParameters().size() != 1)
    return false;
  if (fnType->getResult().getConvention() != ResultConvention::Unowned)
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
    IGF.emitRetainCall(selfValue);
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
  auto &layout = info.getLayout(IGF.IGM);

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
    = emitClassFragileInstanceSizeAndAlignMask(IGF, selfClass, metadata);
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

llvm::Constant *irgen::tryEmitClassConstantFragileInstanceSize(
                                                        IRGenModule &IGM,
                                                        ClassDecl *Class) {
  auto &classTI = getSelfTypeInfo(IGM, Class);

  auto &layout = classTI.getLayout(IGM);
  if (layout.isFixedLayout())
    return layout.emitSize(IGM);
  
  return nullptr;
}

llvm::Constant *irgen::tryEmitClassConstantFragileInstanceAlignMask(
                                                             IRGenModule &IGM,
                                                             ClassDecl *Class) {
  auto &classTI = getSelfTypeInfo(IGM, Class);
  
  auto &layout = classTI.getLayout(IGM);
  if (layout.isFixedLayout())
    return layout.emitAlignMask(IGM);
  
  return nullptr;
}

/// emitClassDecl - Emit all the declarations associated with this class type.
void IRGenModule::emitClassDecl(ClassDecl *D) {
  PrettyStackTraceDecl prettyStackTrace("emitting class metadata for", D);

  auto &classTI = Types.getTypeInfo(D).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(*this);

  // Emit the class metadata.
  emitClassMetadata(*this, D, layout);
  emitNestedTypeDecls(D->getMembers());
}

namespace {
  enum ForMetaClass_t : bool {
    ForClass = false,
    ForMetaClass = true
  };

  typedef std::pair<ClassDecl*, Module*> CategoryNameKey;
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
    const LayoutClass *Layout;
    const StructLayout *FieldLayout;
    
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

    bool Generic = false;
    bool HasNonTrivialDestructor = false;
    bool HasNonTrivialConstructor = false;
    llvm::SmallString<16> CategoryName;
    SmallVector<llvm::Constant*, 8> Ivars;
    SmallVector<llvm::Constant*, 16> InstanceMethods;
    SmallVector<llvm::Constant*, 16> ClassMethods;
    SmallVector<llvm::Constant*, 16> OptInstanceMethods;
    SmallVector<llvm::Constant*, 16> OptClassMethods;
    SmallVector<llvm::Constant*, 4> Protocols;
    SmallVector<llvm::Constant*, 8> Properties;
    SmallVector<llvm::Constant*, 8> InstanceMethodTypesExt;
    SmallVector<llvm::Constant*, 8> ClassMethodTypesExt;
    SmallVector<llvm::Constant*, 8> OptInstanceMethodTypesExt;
    SmallVector<llvm::Constant*, 8> OptClassMethodTypesExt;
    
    llvm::Constant *Name = nullptr;
    /// Index of the first non-inherited field in the layout.
    unsigned FirstFieldIndex;
    unsigned NextFieldIndex;
  public:
    ClassDataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                     const LayoutClass &layout,
                     const StructLayout &fieldLayout,
                     unsigned firstField)
        : IGM(IGM), TheEntity(theClass), TheExtension(nullptr),
          Layout(&layout), FieldLayout(&fieldLayout),
          Generic(theClass->isGenericContext()),
          FirstFieldIndex(firstField),
          NextFieldIndex(firstField)
    {
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
        Layout(nullptr), FieldLayout(nullptr),
        Generic(theClass->isGenericContext())
    {
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
      for (ProtocolDecl *p : theProtocol->getProtocols()) {
        if (!p->isObjC())
          continue;
        // Don't emit the magic AnyObject conformance.
        if (p == IGM.Context.getProtocol(KnownProtocolKind::AnyObject))
          continue;
        Protocols.push_back(buildProtocolRef(p));
      }

      for (Decl *member : theProtocol->getMembers())
        visit(member);
    }

    /// Gather protocol records for all of the explicitly-specified Objective-C
    /// protocol conformances.
    void visitConformances(DeclContext *dc) {
      for (auto conformance : dc->getLocalConformances(
                                nullptr,
                                ConformanceLookupKind::OnlyExplicit)) {
        ProtocolDecl *proto = conformance->getProtocol();
        if (!proto->isObjC())
          continue;

        // Don't emit the magic AnyObject conformance.
        if (auto known = proto->getKnownProtocolKind())
          if (*known == KnownProtocolKind::AnyObject)
            continue;

        Protocols.push_back(buildProtocolRef(proto));
      }
    }

    void buildMetaclassStub() {
      assert(Layout && "can't build a metaclass from a category");
      // The isa is the metaclass pointer for the root class.
      auto rootClass = Layout->getRootClassForMetaclass();
      auto rootPtr = IGM.getAddrOfMetaclassObject(rootClass, NotForDefinition);

      // The superclass of the metaclass is the metaclass of the
      // superclass.  Note that for metaclass stubs, we can always
      // ignore parent contexts and generic arguments.
      //
      // If this class has no formal superclass, then its actual
      // superclass is SwiftObject, i.e. the root class.
      llvm::Constant *superPtr;
      if (getClass()->hasSuperclass()) {
        auto base = getClass()->getSuperclass()->getClassOrBoundGenericClass();
        superPtr = IGM.getAddrOfMetaclassObject(base, NotForDefinition);
      } else {
        superPtr = rootPtr;
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
      Module *TheModule = TheExtension->getParentModule();

      os << TheModule->Name;
      
      unsigned categoryCount = CategoryCounts[{getClass(), TheModule}]++;
      if (categoryCount > 0)
        os << categoryCount;
        
      os.flush();
    }
    
  public:
    llvm::Constant *emitCategory() {
      assert(TheExtension && "can't emit category data for a class");
      SmallVector<llvm::Constant*, 11> fields;
      // struct category_t {
      //   char const *name;
      fields.push_back(IGM.getAddrOfGlobalString(CategoryName));
      //   const class_t *theClass;
      if (getClass()->hasClangNode())
        fields.push_back(IGM.getAddrOfObjCClass(getClass(), NotForDefinition));
      else {
        auto type = getSelfType(getClass()).getSwiftRValueType();
        llvm::Constant *metadata = tryEmitConstantHeapMetadataRef(IGM, type);
        assert(metadata &&
               "extended objc class doesn't have constant metadata?");
        fields.push_back(metadata);
      }
      //   const method_list_t *instanceMethods;
      fields.push_back(buildInstanceMethodList());
      //   const method_list_t *classMethods;
      fields.push_back(buildClassMethodList());
      //   const protocol_list_t *baseProtocols;
      fields.push_back(buildProtocolList());
      //   const property_list_t *properties;
      fields.push_back(buildPropertyList());
      // };
      
      return buildGlobalVariable(fields, "_CATEGORY_");
    }
    
    llvm::Constant *emitProtocol() {
      SmallVector<llvm::Constant*, 11> fields;
      llvm::SmallString<64> nameBuffer;

      assert(isBuildingProtocol() && "not emitting a protocol");
      
      // struct protocol_t {
      //   Class super;
      fields.push_back(null());
      //   char const *name;
      fields.push_back(IGM.getAddrOfGlobalString(getEntityName(nameBuffer)));
      //   const protocol_list_t *baseProtocols;
      fields.push_back(buildProtocolList());
      //   const method_list_t *requiredInstanceMethods;
      fields.push_back(buildInstanceMethodList());
      //   const method_list_t *requiredClassMethods;
      fields.push_back(buildClassMethodList());
      //   const method_list_t *optionalInstanceMethods;
      fields.push_back(buildOptInstanceMethodList());
      //   const method_list_t *optionalClassMethods;
      fields.push_back(buildOptClassMethodList());
      //   const property_list_t *properties;
      fields.push_back(buildPropertyList());
      //   uint32_t size;
      unsigned size = IGM.getPointerSize().getValue() * fields.size() +
                      IGM.getPointerSize().getValue(); // This is for extendedMethodTypes
      size += 8; // 'size' and 'flags' fields that haven't been added yet.
      fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty, size));
      //   uint32_t flags;
      auto flags = ProtocolDescriptorFlags()
        .withSwift(!getProtocol()->hasClangNode())
        .withClassConstraint(ProtocolClassConstraint::Class)
        .withNeedsWitnessTable(false)
        .withSpecialProtocol(getSpecialProtocolID(getProtocol()));
      
      fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty, flags.getIntValue()));
      
      // const char ** extendedMethodTypes;
      fields.push_back(buildOptExtendedMethodTypes());
      
      // };
      
      return buildGlobalVariable(fields, "_PROTOCOL_");
    }

    llvm::Constant *emitRODataFields(ForMetaClass_t forMeta) {
      assert(Layout && FieldLayout && "can't emit rodata for a category");
      SmallVector<llvm::Constant*, 11> fields;
      // struct _class_ro_t {
      //   uint32_t flags;
      fields.push_back(buildFlags(forMeta));

      //   uint32_t instanceStart;
      //   uint32_t instanceSize;
      // The runtime requires that the ivar offsets be initialized to
      // a valid layout of the ivars of this class, bounded by these
      // two values.  If the instanceSize of the superclass equals the
      // stored instanceStart of the subclass, the ivar offsets
      // will not be changed.
      // FIXME: This is totally bogus for generic classes with dynamic layout.
      Size instanceStart;
      Size instanceSize;
      if (forMeta) {
        // sizeof(struct class_t)
        instanceSize = Size(5 * IGM.getPointerSize().getValue());
        // historical nonsense
        instanceStart = instanceSize;
      } else {
        instanceSize = FieldLayout->getSize();
        if (FieldLayout->getElements().empty()
            || FieldLayout->getElements().size() == FirstFieldIndex) {
          instanceStart = instanceSize;
        } else if (FieldLayout->getElements()[FirstFieldIndex].getKind()
                     == ElementLayout::Kind::Fixed) {
          // FIXME: assumes layout is always sequential!
          instanceStart = FieldLayout->getElements()[FirstFieldIndex].getByteOffset();
        } else {
          // FIXME: arrange to initialize this at runtime
          instanceStart = Size(0);
        }
      }
      fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty,
                                              instanceStart.getValue()));
      fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty,
                                              instanceSize.getValue()));

      //   uint32_t reserved;  // only when building for 64bit targets
      if (IGM.getPointerAlignment().getValue() > 4) {
        assert(IGM.getPointerAlignment().getValue() == 8);
        fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty, 0));
      }

      //   const uint8_t *ivarLayout;
      // GC/ARC layout.  TODO.
      fields.push_back(null());

      //   const char *name;
      // It is correct to use the same name for both class and metaclass.
      fields.push_back(buildName());

      //   const method_list_t *baseMethods;
      fields.push_back(forMeta ? buildClassMethodList()
                               : buildInstanceMethodList());

      //   const protocol_list_t *baseProtocols;
      // Apparently, this list is the same in the class and the metaclass.
      fields.push_back(buildProtocolList());

      //   const ivar_list_t *ivars;
      fields.push_back(forMeta ? null() : buildIvarList());

      //   const uint8_t *weakIvarLayout;
      // More GC/ARC layout.  TODO.
      fields.push_back(null());

      //   const property_list_t *baseProperties;
      fields.push_back(forMeta ? null() : buildPropertyList());

      // };

      return llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), fields);
    }
    
    llvm::Constant *emitROData(ForMetaClass_t forMeta) {
      auto fields = emitRODataFields(forMeta);
      
      auto dataSuffix = forMeta ? "_METACLASS_DATA_" : "_DATA_";
      return buildGlobalVariable(fields, dataSuffix);
    }

  private:
    llvm::Constant *buildFlags(ForMetaClass_t forMeta) {
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
      return llvm::ConstantInt::get(IGM.Int32Ty, uint32_t(flags));
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
      
      llvm::Constant *entry = emitObjCMethodDescriptor(IGM, method);
      // This pointer will be set if we need to store the extended method type
      // encoding.
      SmallVectorImpl<llvm::Constant *> *ExtMethodTypesList = nullptr;
      if (!method->isStatic()) {
        if (method->getAttrs().hasAttribute<OptionalAttr>()) {
          OptInstanceMethods.push_back(entry);
          if (isBuildingProtocol())
            ExtMethodTypesList = &OptInstanceMethodTypesExt;
        }
        else {
          InstanceMethods.push_back(entry);
          if (isBuildingProtocol())
            ExtMethodTypesList = &InstanceMethodTypesExt;
        }
      } else {
        if (method->getAttrs().hasAttribute<OptionalAttr>()) {
          OptClassMethods.push_back(entry);
          if (isBuildingProtocol())
            ExtMethodTypesList = &OptClassMethodTypesExt;
        }
        else {
          ClassMethods.push_back(entry);
          if (isBuildingProtocol())
            ExtMethodTypesList = &ClassMethodTypesExt;
        }
      }
      if (ExtMethodTypesList) {
        ExtMethodTypesList->push_back(
                                getMethodTypeExtendedEncoding(IGM, method));
      }
    }

    /// Constructors need to be collected into the appropriate methods list.
    void visitConstructorDecl(ConstructorDecl *constructor) {
      if (!isBuildingProtocol() &&
          !requiresObjCMethodDescriptor(constructor)) return;
      llvm::Constant *entry = emitObjCMethodDescriptor(IGM, constructor);
      if (constructor->getAttrs().hasAttribute<OptionalAttr>()) {
        OptInstanceMethods.push_back(entry);
        if (isBuildingProtocol())
          OptInstanceMethodTypesExt.push_back(
                              getMethodTypeExtendedEncoding(IGM, constructor));
      } else {
        InstanceMethods.push_back(entry);
        if (isBuildingProtocol())
          InstanceMethodTypesExt.push_back(
                              getMethodTypeExtendedEncoding(IGM, constructor));
      }
    }

    /// Determine whether the given destructor has an Objective-C
    /// definition.
    bool hasObjCDeallocDefinition(DestructorDecl *destructor) {
      // If we have the destructor body, we know whether SILGen
      // generated a -dealloc body.
      if (auto braceStmt = destructor->getBody())
        return !braceStmt->getElements().empty();

      // We don't have a destructor body, so hunt for the SIL function
      // for it.
      SILDeclRef dtorRef(destructor, SILDeclRef::Kind::Deallocator,
                         ResilienceExpansion::Minimal,
                         SILDeclRef::ConstructAtNaturalUncurryLevel,
                         /*isForeign=*/true);
      llvm::SmallString<64> dtorNameBuffer;
      auto dtorName = dtorRef.mangle(dtorNameBuffer);
      if (auto silFn = IGM.SILMod->lookUpFunction(dtorName))
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
        llvm::Constant *entry = emitObjCMethodDescriptor(IGM, destructor);
        InstanceMethods.push_back(entry);
      }
    }

    void addIVarInitializer() {
      if (auto entry = emitObjCIVarInitDestroyDescriptor(IGM, getClass(),
                                                         false)) {
        InstanceMethods.push_back(*entry);

        HasNonTrivialConstructor = true;
      }
    }

    void addIVarDestroyer() {
      if (auto entry = emitObjCIVarInitDestroyDescriptor(IGM, getClass(),
                                                         true)) {
        InstanceMethods.push_back(*entry);

        HasNonTrivialDestructor = true;
      }
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
      SmallVector<llvm::Constant*, 16> AllMethodTypesExt;
      assert(InstanceMethodTypesExt.size() == InstanceMethods.size()
             && "number of instance methods does not match extended types");
      assert(ClassMethodTypesExt.size() == ClassMethods.size()
             && "number of class methods does not match extended types");
      assert(OptInstanceMethodTypesExt.size() == OptInstanceMethods.size()
             && "number of optional instance methods does not match extended types");
      assert(OptClassMethodTypesExt.size() == OptClassMethods.size()
             && "number of optional class methods does not match extended types");
      AllMethodTypesExt.insert(AllMethodTypesExt.end(),
                 InstanceMethodTypesExt.begin(), InstanceMethodTypesExt.end());
      AllMethodTypesExt.insert(AllMethodTypesExt.end(),
                 ClassMethodTypesExt.begin(), ClassMethodTypesExt.end());
      AllMethodTypesExt.insert(AllMethodTypesExt.end(),
           OptInstanceMethodTypesExt.begin(), OptInstanceMethodTypesExt.end());
      AllMethodTypesExt.insert(AllMethodTypesExt.end(),
           OptClassMethodTypesExt.begin(), OptClassMethodTypesExt.end());
      return buildMethodList(AllMethodTypesExt,
                             "_PROTOCOL_METHOD_TYPES_");
    }

    /// struct method_list_t {
    ///   uint32_t entsize; // runtime uses low bits for its own purposes
    ///   uint32_t count;
    ///   method_t list[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildMethodList(ArrayRef<llvm::Constant*> methods,
                                    StringRef name) {
      return buildOptionalList(methods, 3 * IGM.getPointerSize(), name);
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
                                                "_PROTOCOL_PROTOCOLS_"));
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
      if (!Layout && !FieldLayout)
        return;

      // For now, we never try to emit specialized versions of the
      // metadata statically, so compute the field layout using the
      // originally-declared type.
      SILType fieldType =
        IGM.getLoweredType(AbstractionPattern(var->getType()), var->getType());
      Ivars.push_back(buildIvar(var, fieldType));

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
    llvm::Constant *buildIvar(VarDecl *ivar, SILType loweredType) {
      assert(Layout && FieldLayout && "can't build ivar for category");
      // FIXME: this is not always the right thing to do!
      auto &elt = FieldLayout->getElements()[NextFieldIndex++];
      auto &ivarTI = IGM.getTypeInfo(loweredType);
      
      llvm::Constant *offsetPtr;
      if (elt.getKind() == ElementLayout::Kind::Fixed) {
        // Emit a field offset variable for the fixed field statically.
        auto offsetAddr = IGM.getAddrOfFieldOffset(ivar, /*indirect*/ false,
                                                   ForDefinition);
        auto offsetVar = cast<llvm::GlobalVariable>(offsetAddr.getAddress());
        offsetVar->setConstant(false);
        auto offsetVal =
          llvm::ConstantInt::get(IGM.IntPtrTy, elt.getByteOffset().getValue());
        offsetVar->setInitializer(offsetVal);
        
        offsetPtr = offsetVar;
      } else {
        // We need to set this up when the metadata is instantiated.
        // FIXME: set something up to fill at runtime
        offsetPtr
          = llvm::ConstantPointerNull::get(IGM.IntPtrTy->getPointerTo());
      }

      // TODO: clang puts this in __TEXT,__objc_methname,cstring_literals
      auto name = IGM.getAddrOfGlobalString(ivar->getName().str());

      // TODO: clang puts this in __TEXT,__objc_methtype,cstring_literals
      auto typeEncode = IGM.getAddrOfGlobalString("");

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

      llvm::Constant *fields[] = {
        offsetPtr,
        name,
        typeEncode,
        llvm::ConstantInt::get(IGM.Int32Ty, alignment.log2()),
        llvm::ConstantInt::get(IGM.Int32Ty, size.getValue()),
      };
      return llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), fields);
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
      return buildOptionalList(Ivars, eltSize, "_IVARS_");
    }

    /*** Properties ********************************************************/

    /// Properties need to be collected in the properties list.
    void visitProperty(VarDecl *var) {
      if (requiresObjCPropertyDescriptor(IGM, var)) {
        // ObjC doesn't support formal class properties.
        if (!var->isStatic())
          if (llvm::Constant *prop = buildProperty(var))
            Properties.push_back(prop);

        // Don't emit getter/setter descriptors for @NSManagedAttr properties.
        if (var->getAttrs().hasAttribute<NSManagedAttr>() ||
            // Don't emit descriptors for properties without accessors.
            var->getGetter() == nullptr)
          return;
        
        SmallVectorImpl<llvm::Constant *> *methods;
        SmallVectorImpl<llvm::Constant *> *extMethodTypes = nullptr;
        if (var->getAttrs().hasAttribute<OptionalAttr>()) {
          if (var->isStatic()) {
            methods = &OptClassMethods;
            if (isBuildingProtocol())
              extMethodTypes = &OptClassMethodTypesExt;
          } else {
            methods = &OptInstanceMethods;
            if (isBuildingProtocol())
              extMethodTypes = &OptInstanceMethodTypesExt;
          }
        } else {
          if (var->isStatic()) {
            methods = &ClassMethods;
            if (isBuildingProtocol())
              extMethodTypes = &ClassMethodTypesExt;
          } else {
            methods = &InstanceMethods;
            if (isBuildingProtocol())
              extMethodTypes = &InstanceMethodTypesExt;
          }
        }

        auto getter_setter = emitObjCPropertyMethodDescriptors(IGM, var);
        methods->push_back(getter_setter.first);

        if (getter_setter.second)
          methods->push_back(getter_setter.second);
        
        // Get the getter and setter extended encodings, if needed.
        if (extMethodTypes) {
          extMethodTypes->push_back(
                          getMethodTypeExtendedEncoding(IGM, var->getGetter()));
          if (auto setter = var->getSetter()) {
            assert(getter_setter.second && "no descriptor for setter?!");
            extMethodTypes->push_back(
                          getMethodTypeExtendedEncoding(IGM, setter));
          }
        }
      }
    }
    
    /// Build the property attribute string for a property decl.
    void buildPropertyAttributes(VarDecl *prop, SmallVectorImpl<char> &out) {
      llvm::raw_svector_ostream outs(out);

      auto propTy = prop->getType()->getReferenceStorageReferent();
      
      // Emit the type encoding for the property.
      outs << 'T';
      
      std::string typeEnc;
      getObjCEncodingForPropertyType(IGM, propTy, typeEnc);
      outs << typeEnc;
      
      // Emit other attributes.

      // All Swift properties are (nonatomic).
      outs << ",N";
      
      // @NSManaged properties are @dynamic.
      if (prop->getAttrs().hasAttribute<NSManagedAttr>())
        outs << ",D";
      
      auto isObject = propTy->hasRetainablePointerRepresentation();
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
      
      outs.flush();
    }

    /// struct property_t {
    ///   const char *name;
    ///   const char *attributes;
    /// };
    llvm::Constant *buildProperty(VarDecl *prop) {
      llvm::SmallString<16> propertyAttributes;
      buildPropertyAttributes(prop, propertyAttributes);
      
      llvm::Constant *fields[] = {
        IGM.getAddrOfGlobalString(prop->getObjCPropertyName().str()),
        IGM.getAddrOfGlobalString(propertyAttributes)
      };
      return llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), fields);
    }

    /// struct property_list_t {
    ///   uint32_t entsize;
    ///   uint32_t count;
    ///   property_t list[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildPropertyList() {
      Size eltSize = 2 * IGM.getPointerSize();
      return buildOptionalList(Properties, eltSize,
                               chooseNamePrefix("_PROPERTIES_",
                                                "_CATEGORY_PROPERTIES_",
                                                "_PROTOCOL_PROPERTIES_"));
    }

    /*** General ***********************************************************/

    /// Build a list structure from the given array of objects.
    /// If the array is empty, use null.  The assumption is that every
    /// initializer has the same size.
    ///
    /// \param optionalEltSize - if non-zero, a size which needs
    ///   to be placed in the list header
    llvm::Constant *buildOptionalList(ArrayRef<llvm::Constant*> objects,
                                      Size optionalEltSize,
                                      StringRef nameBase) {
      if (objects.empty())
        return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

      SmallVector<llvm::Constant*, 3> fields;

      // FIXME. _PROTOCOL_METHOD_TYPES_ does not have the first two entries.
      // May want to pull this into its own routine for performance; if needed.
      if (!nameBase.equals("_PROTOCOL_METHOD_TYPES_")) {
        // In all of the foo_list_t structs, either:
        //   - there's a 32-bit entry size and a 32-bit count or
        //   - there's no entry size and a uintptr_t count.
        if (!optionalEltSize.isZero()) {
          fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty,
                                                  optionalEltSize.getValue()));
          fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty, objects.size()));
        } else {
          fields.push_back(llvm::ConstantInt::get(IGM.IntPtrTy, objects.size()));
        }
      }

      auto arrayTy =
        llvm::ArrayType::get(objects[0]->getType(), objects.size());
      fields.push_back(llvm::ConstantArray::get(arrayTy, objects));

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
    llvm::Constant *buildGlobalVariable(llvm::Constant *init,
                                        StringRef nameBase) {
      llvm::SmallString<64> nameBuffer;
      auto var = new llvm::GlobalVariable(IGM.Module, init->getType(),
                                        /*constant*/ true,
                                        llvm::GlobalVariable::PrivateLinkage,
                                        init,
                                        Twine(nameBase) 
                                          + getEntityName(nameBuffer)
                                          + (TheExtension
                                             ? Twine("_$_") + CategoryName.str()
                                             : Twine()));
      var->setAlignment(IGM.getPointerAlignment().getValue());
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

    llvm::Constant *buildGlobalVariable(ArrayRef<llvm::Constant*> fields,
                                        StringRef nameBase) {
      auto init = llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), fields);
      return buildGlobalVariable(init, nameBase);
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
      auto getter_setter = emitObjCSubscriptMethodDescriptors(IGM, subscript);
      if (subscript->getAttrs().hasAttribute<OptionalAttr>()) {
        OptInstanceMethods.push_back(getter_setter.first);
        if (isBuildingProtocol())
          OptInstanceMethodTypesExt.push_back(
                    getMethodTypeExtendedEncoding(IGM, subscript->getGetter()));
      } else {
        InstanceMethods.push_back(getter_setter.first);
        if (isBuildingProtocol())
          InstanceMethodTypesExt.push_back(
                    getMethodTypeExtendedEncoding(IGM, subscript->getGetter()));
      }

      if (getter_setter.second) {
        assert(subscript->getSetter() && "no descriptor for setter?!");
        if (subscript->getAttrs().hasAttribute<OptionalAttr>()) {
          OptInstanceMethods.push_back(getter_setter.second);
          if (isBuildingProtocol())
            OptInstanceMethodTypesExt.push_back(
                    getMethodTypeExtendedEncoding(IGM, subscript->getSetter()));
        } else {
          InstanceMethods.push_back(getter_setter.second);
          if (isBuildingProtocol())
            InstanceMethodTypesExt.push_back(
                    getMethodTypeExtendedEncoding(IGM, subscript->getSetter()));
        }
      }
    }
  };
}

/// Emit the private data (RO-data) associated with a class.
llvm::Constant *irgen::emitClassPrivateData(IRGenModule &IGM,
                                            ClassDecl *cls) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  SILType selfType = getSelfType(cls);
  auto &classTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &fieldLayout = classTI.getLayout(IGM);
  LayoutClass layout(IGM, ResilienceScope::Universal, cls, selfType);
  ClassDataBuilder builder(IGM, cls, layout, fieldLayout,
                           classTI.getInheritedStoredProperties(IGM).size());

  // First, build the metaclass object.
  builder.buildMetaclassStub();

  // Then build the class RO-data.
  return builder.emitROData(ForClass);
}
  
std::tuple<llvm::Constant * /*classData*/,
           llvm::Constant * /*metaclassData*/,
           Size>
irgen::emitClassPrivateDataFields(IRGenModule &IGM, ClassDecl *cls) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  SILType selfType = getSelfType(cls);
  auto &classTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &fieldLayout = classTI.getLayout(IGM);
  LayoutClass layout(IGM, ResilienceScope::Universal, cls, selfType);
  ClassDataBuilder builder(IGM, cls, layout, fieldLayout,
                           classTI.getInheritedStoredProperties(IGM).size());

  auto classFields = builder.emitRODataFields(ForClass);
  auto metaclassFields = builder.emitRODataFields(ForMetaClass);
  Size size(IGM.DataLayout.getTypeAllocSize(classFields->getType()));
  return std::make_tuple(classFields, metaclassFields, size);
}
  
/// Emit the metadata for an ObjC category.
llvm::Constant *irgen::emitCategoryData(IRGenModule &IGM,
                                        ExtensionDecl *ext) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  ClassDecl *cls = ext->getDeclaredTypeInContext()
    ->getClassOrBoundGenericClass();
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

const TypeInfo *TypeConverter::convertClassType(ClassDecl *D) {
  llvm::StructType *ST = IGM.createNominalType(D);
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

/// Lazily declare the Swift root-class, SwiftObject.
ClassDecl *IRGenModule::getSwiftRootClass() {
  if (SwiftRootClass) return SwiftRootClass;

  auto name = Context.getIdentifier("SwiftObject");

  // Make a really fake-looking class.
  SwiftRootClass = new (Context) ClassDecl(SourceLoc(), name, SourceLoc(),
                                           MutableArrayRef<TypeLoc>(),
                                           /*generics*/ nullptr,
                                           Context.TheBuiltinModule);
  SwiftRootClass->computeType();
  SwiftRootClass->setIsObjC(true);
  SwiftRootClass->getAttrs().add(ObjCAttr::createNullary(Context, name,
                                                         /*implicit=*/true));
  SwiftRootClass->setImplicit();
  SwiftRootClass->setAccessibility(Accessibility::Public);
  return SwiftRootClass;
}

ClassDecl *irgen::getRootClassForMetaclass(IRGenModule &IGM, ClassDecl *C) {
  LayoutClass layout(IGM, ResilienceScope::Local, C, getSelfType(C));

  return layout.getRootClassForMetaclass();
}
