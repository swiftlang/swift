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
#include "swift/AST/ASTContext.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeMemberVisitor.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILVTableVisitor.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/Support/raw_ostream.h"

#include "Callee.h"
#include "ClassLayout.h"
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
#include "MemberAccessStrategy.h"
#include "MetadataLayout.h"
#include "MetadataRequest.h"


using namespace swift;
using namespace irgen;

namespace {
  /// Layout information for class types.
  class ClassTypeInfo : public HeapTypeInfo<ClassTypeInfo> {
    ClassDecl *TheClass;

    // The resilient layout of the class, without making any assumptions
    // that violate resilience boundaries. This is used to allocate
    // and deallocate instances of the class, and to access fields.
    mutable Optional<ClassLayout> ResilientLayout;

    // A completely fragile layout, used for metadata emission.
    mutable Optional<ClassLayout> FragileLayout;

    /// Can we use swift reference-counting, or do we have to use
    /// objc_retain/release?
    const ReferenceCounting Refcount;
    
    ClassLayout generateLayout(IRGenModule &IGM, SILType classType,
                               bool forBackwardDeployment) const;

  public:
    ClassTypeInfo(llvm::PointerType *irType, Size size,
                  SpareBitVector spareBits, Alignment align,
                  ClassDecl *theClass, ReferenceCounting refcount)
      : HeapTypeInfo(irType, size, std::move(spareBits), align),
        TheClass(theClass), Refcount(refcount) {}

    ReferenceCounting getReferenceCounting() const {
      return Refcount;
    }

    ClassDecl *getClass() const { return TheClass; }


    const ClassLayout &getClassLayout(IRGenModule &IGM, SILType type,
                                      bool forBackwardDeployment) const;

    StructLayout *createLayoutWithTailElems(IRGenModule &IGM,
                                            SILType classType,
                                            ArrayRef<SILType> tailTypes) const;
  };
} // end anonymous namespace

/// Return the lowered type for the class's 'self' type within its context.
static SILType getSelfType(ClassDecl *base) {
  auto loweredTy = base->getDeclaredTypeInContext()->getCanonicalType();
  return SILType::getPrimitiveObjectType(loweredTy);
}

/// If the superclass came from another module, we may have dropped
/// stored properties due to the Swift language version availability of
/// their types. In these cases we can't precisely lay out the ivars in
/// the class object at compile time so we need to do runtime layout.
static bool classHasIncompleteLayout(IRGenModule &IGM,
                                     ClassDecl *theClass) {
  if (theClass->getParentModule() == IGM.getSwiftModule())
    return false;

  for (auto field : theClass->getStoredPropertiesAndMissingMemberPlaceholders())
    if (isa<MissingMemberDecl>(field))
      return true;

  return false;
}

namespace {
  class ClassLayoutBuilder : public StructLayoutBuilder {
    SmallVector<ElementLayout, 8> Elements;
    SmallVector<VarDecl*, 8> AllStoredProperties;
    SmallVector<FieldAccess, 8> AllFieldAccesses;

    // If we're building a layout with tail-allocated elements, we do
    // things slightly differently; all fields from the superclass are
    // added before the class fields, and the tail elements themselves
    // come after. We don't make a ClassLayout in this case, only a
    // StructLayout.
    Optional<ArrayRef<SILType>> TailTypes;

    // Normally, Swift only emits static metadata for a class if it has no
    // generic ancestry and no fields with resilient value types, which
    // require dynamic layout.
    //
    // However, for interop with Objective-C, where the runtime does not
    // know how to invoke arbitrary code to initialize class metadata, we
    // ignore resilience and emit a static layout and metadata for classes
    // that would otherwise have static metadata, were it not for any
    // resilient fields.
    //
    // This enables two things:
    //
    // - Objective-C can reference the class symbol by calling a static
    //   method on it, for example +alloc, which requires the InstanceSize
    //   to be known, except for possibly sliding ivars.
    //
    // - Objective-C message sends can call methods defined in categories
    //   emitted by Swift, which again require the class metadata symbol
    //   to have a static address.
    //
    // Note that we don't do this if the class is generic, has generic
    // ancestry, or has a superclass that is itself resilient.
    bool CompletelyFragileLayout;

    // The below flags indicate various things about the metadata of the
    // class that might require dynamic initialization or resilient
    // access patterns:

    // Does the class or any of its superclasses have stored properties that
    // where dropped due to the Swift language version availability of
    // their types?
    bool ClassHasMissingMembers = false;

    // Does the class or any of its fragile superclasses have stored
    // properties of unknown size, which do *not* depend on generic
    // parameters?
    //
    // This is different from the class itself being resilient or
    // having resilient ancestry, because we still have a fixed layout
    // for the class metadata in this case.
    //
    // In fact, for a class with resilient ancestry, this can still be
    // false if all of the fields known to us are fixed size.
    bool ClassHasResilientMembers = false;

    // Is this class or any of its superclasses generic?
    bool ClassHasGenericAncestry = false;

    // Is this class itself generic via the Swift generic system, ie. not a
    // lightweight Objective-C generic class?
    bool ClassIsGeneric = false;

    // Does the class layout depend on the size or alignment of its
    // generic parameters?
    //
    // This can be the case if the class has generic resilient ancestry
    // that depends on the class's generic parameters, of it it has
    // fields of generic type that are not fixed size.
    bool ClassHasGenericLayout = false;

    // Is this class or any of its superclasses resilient from the viewpoint
    // of the current module? This means that their metadata can change size
    // and field offsets, generic arguments and virtual methods must be
    // accessed relative to a metadata base global variable.
    bool ClassHasResilientAncestry = false;

    // Are any of this class's superclasses defined in Objective-C?
    // This means that field offsets must be loaded from field offset globals
    // or the field offset vector in the metadata, and the Objective-C runtime
    // will slide offsets based on the actual superclass size, which is not
    // known at compile time.
    bool ClassHasObjCAncestry = false;

  public:
    ClassLayoutBuilder(IRGenModule &IGM, SILType classType,
                       ReferenceCounting refcounting,
                       bool completelyFragileLayout,
                       Optional<ArrayRef<SILType>> tailTypes = None)
      : StructLayoutBuilder(IGM),
        TailTypes(tailTypes),
        CompletelyFragileLayout(completelyFragileLayout) {
      // Start by adding a heap header.
      switch (refcounting) {
      case ReferenceCounting::Native:
        // For native classes, place a full object header.
        addHeapHeader();
        break;
      case ReferenceCounting::ObjC:
        // For ObjC-inheriting classes, we don't reliably know the size of the
        // base class, but NSObject only has an `isa` pointer at most.
        addNSObjectHeader();
        break;
      case ReferenceCounting::Block:
      case ReferenceCounting::Unknown:
      case ReferenceCounting::Bridge:
      case ReferenceCounting::Error:
        llvm_unreachable("not a class refcounting kind");
      }
      
      // Next, add the fields for the given class.
      auto theClass = classType.getClassOrBoundGenericClass();
      assert(theClass);

      if (theClass->isGenericContext() && !theClass->hasClangNode())
        ClassIsGeneric = true;

      addFieldsForClass(theClass, classType, /*superclass=*/false);

      if (TailTypes) {
        // Add the tail elements.
        for (SILType TailTy : *TailTypes) {
          const TypeInfo &tailTI = IGM.getTypeInfo(TailTy);
          addTailElement(ElementLayout::getIncomplete(tailTI));
        }
      }
    }

    /// Return the element layouts.
    ArrayRef<ElementLayout> getElements() const {
      return Elements;
    }

    /// Does the class metadata have a completely known, static layout that
    /// does not require initialization at runtime beyond registeration of
    /// the class with the Objective-C runtime?
    bool isFixedSize() const {
      return !(ClassHasMissingMembers ||
               ClassHasResilientMembers ||
               ClassHasGenericLayout ||
               ClassHasResilientAncestry ||
               ClassHasObjCAncestry);
    }

    bool doesMetadataRequireInitialization() const {
      return (ClassHasMissingMembers ||
              ClassHasResilientMembers ||
              ClassHasResilientAncestry ||
              ClassHasGenericAncestry);
    }

    bool doesMetadataRequireRelocation() const {
      return (ClassHasResilientAncestry ||
              ClassIsGeneric);
    }

    ClassLayout getClassLayout(llvm::Type *classTy) const {
      assert(!TailTypes);

      auto allStoredProps = IGM.Context.AllocateCopy(AllStoredProperties);
      auto allFieldAccesses = IGM.Context.AllocateCopy(AllFieldAccesses);
      auto allElements = IGM.Context.AllocateCopy(Elements);

      return ClassLayout(*this,
                         isFixedSize(),
                         doesMetadataRequireInitialization(),
                         doesMetadataRequireRelocation(),
                         classTy,
                         allStoredProps,
                         allFieldAccesses,
                         allElements);
    }

  private:
    /// Adds a layout of a tail-allocated element.
    void addTailElement(const ElementLayout &Elt) {
      Elements.push_back(Elt);
      if (!addField(Elements.back(), LayoutStrategy::Universal)) {
        // For empty tail allocated elements we still add 1 padding byte.
        assert(cast<FixedTypeInfo>(Elt.getType()).getFixedStride() == Size(1) &&
               "empty elements should have stride 1");
        StructFields.push_back(llvm::ArrayType::get(IGM.Int8Ty, 1));
        CurSize += Size(1);
      }
    }

    /// If 'superclass' is true, we're adding fields for one of our
    /// superclasses, which means they become part of the struct
    /// layout calculation, but are not actually added to any of
    /// the vectors like AllStoredProperties, etc. Also, we don't need
    /// to compute FieldAccesses for them.
    void addFieldsForClass(ClassDecl *theClass, SILType classType,
                           bool superclass) {
      if (theClass->hasClangNode()) {
        ClassHasObjCAncestry = true;
        return;
      }

      if (theClass->hasSuperclass()) {
        SILType superclassType = classType.getSuperclass();
        auto superclassDecl = superclassType.getClassOrBoundGenericClass();
        assert(superclassType && superclassDecl);

        if (IGM.isResilient(superclassDecl, ResilienceExpansion::Maximal)) {
          // If the class is resilient, don't walk over its fields; we have to
          // calculate the layout at runtime.
          ClassHasResilientAncestry = true;

          // Furthermore, if the superclass is generic, we have to assume
          // that its layout depends on its generic parameters. But this only
          // propagates down to subclasses whose superclass type depends on the
          // subclass's generic context.
          if (superclassType.hasArchetype())
            ClassHasGenericLayout = true;
        } else {
          // Otherwise, we have total knowledge of the class and its
          // fields, so walk them to compute the layout.
          addFieldsForClass(superclassDecl, superclassType, /*superclass=*/true);
        }
      }

      if (theClass->isGenericContext())
        ClassHasGenericAncestry = true;

      if (classHasIncompleteLayout(IGM, theClass))
        ClassHasMissingMembers = true;

      if (IGM.isResilient(theClass, ResilienceExpansion::Maximal)) {
        ClassHasResilientAncestry = true;
        return;
      }

      // Collect fields from this class and add them to the layout as a chunk.
      addDirectFieldsFromClass(theClass, classType, superclass);
    }

    void addDirectFieldsFromClass(ClassDecl *theClass,
                                  SILType classType,
                                  bool superclass) {
      for (VarDecl *var : theClass->getStoredProperties()) {
        SILType type = classType.getFieldType(var, IGM.getSILModule());

        // Lower the field type.
        auto *eltType = &IGM.getTypeInfo(type);
        if (CompletelyFragileLayout && !eltType->isFixedSize()) {
          // For staging purposes, only do the new thing if the path flag
          // is provided.
          auto mode = (IGM.IRGen.Opts.ReadTypeInfoPath.empty()
                       ? TypeConverter::Mode::CompletelyFragile
                       : TypeConverter::Mode::Legacy);
          LoweringModeScope scope(IGM, mode);
          eltType = &IGM.getTypeInfo(type);
        }

        if (!eltType->isFixedSize()) {
          if (type.hasArchetype())
            ClassHasGenericLayout = true;
          else
            ClassHasResilientMembers = true;
        }

        auto element = ElementLayout::getIncomplete(*eltType);
        addField(element, LayoutStrategy::Universal);

        // The 'Elements' list only contains superclass fields when we're
        // building a layout for tail allocation.
        if (!superclass || TailTypes)
          Elements.push_back(element);

        if (!superclass) {
          AllStoredProperties.push_back(var);
          AllFieldAccesses.push_back(getFieldAccess());
        }
      }

      if (!superclass) {
        // If we're calculating the layout of a specialized generic class type,
        // we cannot use field offset globals for dependently-typed fields,
        // because they will not exist -- we only emit such globals for fields
        // which are not dependent in all instantiations.
        //
        // So make sure to fall back to the fully unsubstituted 'abstract layout'
        // for any fields whose offsets are not completely fixed.
        auto *classTI = &IGM.getTypeInfo(classType).as<ClassTypeInfo>();

        SILType selfType = getSelfType(theClass);
        auto *selfTI = &IGM.getTypeInfo(selfType).as<ClassTypeInfo>();

        // Only calculate an abstract layout if its different than the one
        // being computed now.
        if (classTI != selfTI) {
          auto *abstractLayout = &selfTI->getClassLayout(IGM, selfType,
                                                       CompletelyFragileLayout);

          for (unsigned index : indices(AllFieldAccesses)) {
            auto &access = AllFieldAccesses[index];
            auto *var = AllStoredProperties[index];
            if (access == FieldAccess::NonConstantDirect)
              access = abstractLayout->getFieldAccessAndElement(var).first;
          }
        }

        // If the class has Objective-C ancestry and we're doing runtime layout
        // that depends on generic parameters, the Swift runtime will first
        // layout the fields relative to the static instance start offset, and
        // then ask the Objective-C runtime to slide them.
        //
        // However, this means that if some fields have a generic type, their
        // alignment will change the instance start offset between generic
        // instantiations, and we cannot use field offset global variables at
        // all, even for fields that come before any generically-typed fields.
        //
        // For example, the alignment of 'x' and 'y' below might depend on 'T':
        //
        // class Foo<T> : NSFoobar {
        //   var x : AKlass = AKlass()
        //   var y : AKlass = AKlass()
        //   var t : T?
        // }
        if (ClassHasGenericLayout && ClassHasObjCAncestry) {
          for (auto &access : AllFieldAccesses) {
            if (access == FieldAccess::NonConstantDirect)
              access = FieldAccess::ConstantIndirect;
          }
        }
      }
    }

    FieldAccess getFieldAccess() {
      // If the layout so far has a fixed size, the field offset is known
      // statically.
      if (isFixedSize())
        return FieldAccess::ConstantDirect;

      // If layout so far depends on generic parameters, we have to load the
      // offset from the field offset vector in class metadata.
      if (ClassHasGenericLayout)
        return FieldAccess::ConstantIndirect;

      // If layout so far doesn't depend on any generic parameters, but it's
      // nonetheless not statically known (because either a superclass
      // or a member type was resilient), then we can rely on the existence
      // of a global field offset variable which will be initialized by
      // either the Objective-C or Swift runtime, depending on the
      // class's heritage.
      return FieldAccess::NonConstantDirect;
    }
  };
} // end anonymous namespace

ClassLayout ClassTypeInfo::generateLayout(IRGenModule &IGM, SILType classType,
                                          bool completelyFragileLayout) const {
  ClassLayoutBuilder builder(IGM, classType, Refcount, completelyFragileLayout);

  auto *classTy =
      cast<llvm::StructType>(getStorageType()->getPointerElementType());

  if (completelyFragileLayout) {
    // Create a name for the new llvm type.
    SmallString<32> typeName = classTy->getName();
    typeName += "_fragile";

    // Create the llvm type.
    classTy = llvm::StructType::create(IGM.getLLVMContext(), typeName.str());
  }

  builder.setAsBodyOfStruct(classTy);

  return builder.getClassLayout(classTy);
}

StructLayout *
ClassTypeInfo::createLayoutWithTailElems(IRGenModule &IGM,
                                         SILType classType,
                                         ArrayRef<SILType> tailTypes) const {
  // Add the elements for the class properties.
  ClassLayoutBuilder builder(IGM, classType, Refcount,
                             /*CompletelyFragileLayout=*/false,
                             tailTypes);

  // Create a name for the new llvm type.
  llvm::StructType *classTy =
    cast<llvm::StructType>(getStorageType()->getPointerElementType());
  SmallString<32> typeName;
  llvm::raw_svector_ostream os(typeName);
  os << classTy->getName() << "_tailelems" << IGM.TailElemTypeID++;

  // Create the llvm type.
  llvm::StructType *ResultTy = llvm::StructType::create(IGM.getLLVMContext(),
                                                        os.str());
  builder.setAsBodyOfStruct(ResultTy);

  // Create the StructLayout, which is transfered to the caller (the caller is
  // responsible for deleting it).
  return new StructLayout(builder, classType.getClassOrBoundGenericClass(),
                          ResultTy, builder.getElements());
}

const ClassLayout &
ClassTypeInfo::getClassLayout(IRGenModule &IGM, SILType classType,
                              bool forBackwardDeployment) const {
  // Perform fragile layout only if Objective-C interop is enabled.
  //
  // FIXME: EnableClassResilience staging flag will go away once we can do
  // in-place re-initialization of class metadata.
  bool completelyFragileLayout = (forBackwardDeployment &&
                                  IGM.Context.LangOpts.EnableObjCInterop &&
                                  !IGM.IRGen.Opts.EnableClassResilience);

  // Return the cached layout if available.
  auto &Layout = completelyFragileLayout ? FragileLayout : ResilientLayout;
  if (!Layout) {
    auto NewLayout = generateLayout(IGM, classType, completelyFragileLayout);
    assert(!Layout && "generateLayout() should not call itself recursively");
    Layout = NewLayout;
  }

  return *Layout;
}

/// Cast the base to i8*, apply the given inbounds offset (in bytes,
/// as a size_t), and cast to a pointer to the given type.
llvm::Value *IRGenFunction::emitByteOffsetGEP(llvm::Value *base,
                                              llvm::Value *offset,
                                              llvm::Type *objectType,
                                              const llvm::Twine &name) {
  assert(offset->getType() == IGM.SizeTy || offset->getType() == IGM.Int32Ty);
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

llvm::Constant *
irgen::tryEmitConstantClassFragilePhysicalMemberOffset(IRGenModule &IGM,
                                                       SILType baseType,
                                                       VarDecl *field) {
  auto fieldType = baseType.getFieldType(field, IGM.getSILModule());
  // If the field is empty, its address doesn't matter.
  auto &fieldTI = IGM.getTypeInfo(fieldType);
  if (fieldTI.isKnownEmpty(ResilienceExpansion::Maximal)) {
    return llvm::ConstantInt::get(IGM.SizeTy, 0);
  }

  auto &baseClassTI = IGM.getTypeInfo(baseType).as<ClassTypeInfo>();

  auto &classLayout = baseClassTI.getClassLayout(IGM, baseType,
                                               /*forBackwardDeployment=*/false);

  auto fieldInfo = classLayout.getFieldAccessAndElement(field);
  switch (fieldInfo.first) {
  case FieldAccess::ConstantDirect: {
    auto element = fieldInfo.second;
    return llvm::ConstantInt::get(IGM.SizeTy,
                                  element.getByteOffset().getValue());
  }
  case FieldAccess::NonConstantDirect:
  case FieldAccess::ConstantIndirect:
    return nullptr;
  }
  llvm_unreachable("unhandled access");
}

FieldAccess
irgen::getClassFieldAccess(IRGenModule &IGM, SILType baseType, VarDecl *field) {
  auto &baseClassTI = IGM.getTypeInfo(baseType).as<ClassTypeInfo>();
  auto &classLayout = baseClassTI.getClassLayout(IGM, baseType,
                                               /*forBackwardDeployment=*/false);
  return classLayout.getFieldAccessAndElement(field).first;
}

Size
irgen::getClassFieldOffset(IRGenModule &IGM, SILType baseType, VarDecl *field) {
  auto &baseClassTI = IGM.getTypeInfo(baseType).as<ClassTypeInfo>();

  // FIXME: For now we just assume fragile layout here, because this is used as
  // part of emitting class metadata.
  auto &classLayout = baseClassTI.getClassLayout(IGM, baseType,
                                                /*forBackwardDeployment=*/true);

  auto fieldInfo = classLayout.getFieldAccessAndElement(field);
  auto element = fieldInfo.second;
  assert(element.getKind() == ElementLayout::Kind::Fixed ||
         element.getKind() == ElementLayout::Kind::Empty);
  return element.getByteOffset();
}

StructLayout *
irgen::getClassLayoutWithTailElems(IRGenModule &IGM, SILType classType,
                                   ArrayRef<SILType> tailTypes) {
  auto &ClassTI = IGM.getTypeInfo(classType).as<ClassTypeInfo>();
  return ClassTI.createLayoutWithTailElems(IGM, classType, tailTypes);
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

  auto &classLayout = baseClassTI.getClassLayout(IGF.IGM, baseType,
                                               /*forBackwardDeployment=*/false);

  auto fieldInfo = classLayout.getFieldAccessAndElement(field);

  switch (fieldInfo.first) {
  case FieldAccess::ConstantDirect: {
    Address baseAddr(base, classLayout.getAlignment());
    auto element = fieldInfo.second;
    Address memberAddr = element.project(IGF, baseAddr, None);
    // We may need to bitcast the address if the field is of a generic type.
    if (memberAddr.getType()->getElementType() != fieldTI.getStorageType())
      memberAddr = IGF.Builder.CreateBitCast(memberAddr,
                                     fieldTI.getStorageType()->getPointerTo());
    return OwnedAddress(memberAddr, base);
  }
    
  case FieldAccess::NonConstantDirect: {
    Address offsetA = IGF.IGM.getAddrOfFieldOffset(field, NotForDefinition);
    auto offset = IGF.Builder.CreateLoad(offsetA, "offset");
    return emitAddressAtOffset(IGF, baseType, base, offset, field);
  }
    
  case FieldAccess::ConstantIndirect: {
    auto metadata = emitHeapMetadataRefForHeapObject(IGF, base, baseType);
    auto offset = emitClassFieldOffset(IGF, baseClass, field, metadata);
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

  auto &classLayout = baseClassTI.getClassLayout(IGM, baseType,
                                               /*forBackwardDeployment=*/false);
  auto fieldInfo = classLayout.getFieldAccessAndElement(field);

  switch (fieldInfo.first) {
  case FieldAccess::ConstantDirect: {
    auto element = fieldInfo.second;
    return MemberAccessStrategy::getDirectFixed(element.getByteOffset());
  }

  case FieldAccess::NonConstantDirect: {
    std::string symbol =
      LinkEntity::forFieldOffset(field).mangleAsString();
    return MemberAccessStrategy::getDirectGlobal(std::move(symbol),
                                 MemberAccessStrategy::OffsetKind::Bytes_Word);
  }

  case FieldAccess::ConstantIndirect: {
    Size indirectOffset = getClassFieldOffsetOffset(IGM, baseClass, field);
    return MemberAccessStrategy::getIndirectFixed(indirectOffset,
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
  auto &layout = classTI.getClassLayout(IGF.IGM, ClassType,
                                        /*forBackwardDeployment=*/false);
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
    Offset = emitClassResilientInstanceSizeAndAlignMask(IGF,
                                        ClassType.getClassOrBoundGenericClass(),
                                        metadata).first;
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
                      const ClassLayout &FieldLayout,
                      int &StackAllocSize,
                      ArrayRef<std::pair<SILType, llvm::Value *>> TailArrays) {
  if (StackAllocSize < 0)
    return nullptr;
  if (!FieldLayout.isFixedLayout())
    return nullptr;

  // Calculate the total size needed.
  // The first part is the size of the class itself.
  Alignment ClassAlign = FieldLayout.getAlignment();
  Size TotalSize = FieldLayout.getSize();

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

  if (TotalSize == FieldLayout.getSize()) {
    // No tail-allocated arrays: we can use the llvm class type for alloca.
    llvm::Type *ClassTy = FieldLayout.getType();
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

std::pair<llvm::Value *, llvm::Value *>
irgen::appendSizeForTailAllocatedArrays(IRGenFunction &IGF,
                                    llvm::Value *size, llvm::Value *alignMask,
                                    TailArraysRef TailArrays) {
  for (const auto &TailArray : TailArrays) {
    SILType ElemTy = TailArray.first;
    llvm::Value *Count = TailArray.second;

    const TypeInfo &ElemTI = IGF.getTypeInfo(ElemTy);

    // Align up to the tail-allocated array.
    llvm::Value *ElemStride = ElemTI.getStride(IGF, ElemTy);
    llvm::Value *ElemAlignMask = ElemTI.getAlignmentMask(IGF, ElemTy);
    size = IGF.Builder.CreateAdd(size, ElemAlignMask);
    llvm::Value *InvertedMask = IGF.Builder.CreateNot(ElemAlignMask);
    size = IGF.Builder.CreateAnd(size, InvertedMask);

    // Add the size of the tail allocated array.
    llvm::Value *AllocSize = IGF.Builder.CreateMul(ElemStride, Count);
    size = IGF.Builder.CreateAdd(size, AllocSize);
    alignMask = IGF.Builder.CreateOr(alignMask, ElemAlignMask);
  }
  return {size, alignMask};
}


/// Emit an allocation of a class.
llvm::Value *irgen::emitClassAllocation(IRGenFunction &IGF, SILType selfType,
                                        bool objc, int &StackAllocSize,
                                        TailArraysRef TailArrays) {
  auto &classTI = IGF.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto classType = selfType.getASTType();

  // If we need to use Objective-C allocation, do so.
  // If the root class isn't known to use the Swift allocator, we need
  // to call [self alloc].
  if (objc) {
    llvm::Value *metadata =
      emitClassHeapMetadataRef(IGF, classType, MetadataValueType::ObjCClass,
                               MetadataState::Complete,
                               /*allow uninitialized*/ true);
    StackAllocSize = -1;
    return emitObjCAllocObjectCall(IGF, metadata, selfType);
  }

  llvm::Value *metadata =
    emitClassHeapMetadataRef(IGF, classType, MetadataValueType::TypeMetadata,
                             MetadataState::Complete);

  auto &classLayout = classTI.getClassLayout(IGF.IGM, selfType,
                                             /*forBackwardDeployment=*/false);

  llvm::Value *size, *alignMask;
  if (classLayout.isFixedSize()) {
    size = IGF.IGM.getSize(classLayout.getSize());
    alignMask = IGF.IGM.getSize(classLayout.getAlignMask());
  } else {
    std::tie(size, alignMask)
      = emitClassResilientInstanceSizeAndAlignMask(IGF,
                                     selfType.getClassOrBoundGenericClass(),
                                     metadata);
  }

  llvm::Type *destType = classLayout.getType()->getPointerTo();
  llvm::Value *val = nullptr;
  if (llvm::Value *Promoted = stackPromote(IGF, classLayout, StackAllocSize,
                                           TailArrays)) {
    val = IGF.Builder.CreateBitCast(Promoted, IGF.IGM.RefCountedPtrTy);
    val = IGF.emitInitStackObjectCall(metadata, val, "reference.new");
  } else {
    // Allocate the object on the heap.
    std::tie(size, alignMask)
      = appendSizeForTailAllocatedArrays(IGF, size, alignMask, TailArrays);
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
    return emitObjCAllocObjectCall(IGF, metadata, selfType);
  }

  // Otherwise, allocate using Swift's routines.
  llvm::Value *size, *alignMask;
  std::tie(size, alignMask)
    = emitClassResilientInstanceSizeAndAlignMask(IGF,
                                   selfType.getClassOrBoundGenericClass(),
                                   metadata);
  std::tie(size, alignMask)
    = appendSizeForTailAllocatedArrays(IGF, size, alignMask, TailArrays);

  llvm::Value *val = IGF.emitAllocObjectCall(metadata, size, alignMask,
                                             "reference.new");
  auto &classTI = IGF.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &layout = classTI.getClassLayout(IGF.IGM, selfType,
                                        /*forBackwardDeployment=*/false);
  llvm::Type *destType = layout.getType()->getPointerTo();
  return IGF.Builder.CreateBitCast(val, destType);
}

/// Get the instance size and alignment mask for the given class
/// instance.
static void getInstanceSizeAndAlignMask(IRGenFunction &IGF,
                                        SILType selfType,
                                        ClassDecl *selfClass,
                                        llvm::Value *selfValue,
                                        llvm::Value *&size,
                                        llvm::Value *&alignMask) {
  // Try to determine the size of the object we're deallocating.
  auto &info = IGF.IGM.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &layout = info.getClassLayout(IGF.IGM, selfType,
                                     /*forBackwardDeployment=*/false);

  // If it's fixed, emit the constant size and alignment mask.
  if (layout.isFixedLayout()) {
    size = IGF.IGM.getSize(layout.getSize());
    alignMask = IGF.IGM.getSize(layout.getAlignMask());
    return;
  }

  // Otherwise, get them from the metadata.
  llvm::Value *metadata =
    emitHeapMetadataRefForHeapObject(IGF, selfValue, selfType);
  std::tie(size, alignMask)
    = emitClassResilientInstanceSizeAndAlignMask(IGF, selfClass, metadata);
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
  assert(theClass->getForeignClassKind() == ClassDecl::ForeignKind::Normal);

  llvm::Value *size, *alignMask;
  getInstanceSizeAndAlignMask(IGF, selfType, theClass, selfValue,
                              size, alignMask);

  selfValue = IGF.Builder.CreateBitCast(selfValue, IGF.IGM.RefCountedPtrTy);
  emitDeallocatePartialClassInstance(IGF, selfValue, metadataValue,
                                     size, alignMask);
}

/// emitClassDecl - Emit all the declarations associated with this class type.
void IRGenModule::emitClassDecl(ClassDecl *D) {
  PrettyStackTraceDecl prettyStackTrace("emitting class metadata for", D);

  SILType selfType = getSelfType(D);
  auto &classTI = getTypeInfo(selfType).as<ClassTypeInfo>();

  // FIXME: For now, always use the fragile layout when emitting metadata.
  auto &fragileLayout =
    classTI.getClassLayout(*this, selfType, /*forBackwardDeployment=*/true);

  // ... but still compute the resilient layout for better test coverage.
  auto &resilientLayout =
    classTI.getClassLayout(*this, selfType, /*forBackwardDeployment=*/false);
  (void) resilientLayout;

  // Emit the class metadata.
  emitClassMetadata(*this, D, fragileLayout);

  IRGen.addClassForEagerInitialization(D);

  emitNestedTypeDecls(D->getMembers());
  emitFieldMetadataRecord(D);
}

namespace {
  using CategoryNameKey = std::pair<ClassDecl*, ModuleDecl*>;
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
                     const ClassLayout &fieldLayout)
        : IGM(IGM), TheEntity(theClass), TheExtension(nullptr),
          FieldLayout(&fieldLayout)
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
        FieldLayout(nullptr)
    {
      buildCategoryName(CategoryName);

      visitConformances(theExtension);

      for (Decl *member : TheExtension->getMembers())
        visit(member);
    }
    
    ClassDataBuilder(IRGenModule &IGM, ProtocolDecl *theProtocol)
      : IGM(IGM), TheEntity(theProtocol), TheExtension(nullptr)
    {
      llvm::SmallSetVector<ProtocolDecl *, 2> protocols;

      // Gather protocol references for all of the directly inherited
      // Objective-C protocol conformances.
      for (ProtocolDecl *p : theProtocol->getInheritedProtocols()) {
        getObjCProtocols(p, protocols);
      }

      // Add any restated Objective-C protocol conformances.
      for (auto *attr :
             theProtocol
               ->getAttrs().getAttributes<RestatedObjCConformanceAttr>()) {
        getObjCProtocols(attr->Proto, protocols);
      }

      for (ProtocolDecl *proto : protocols) {
        Protocols.push_back(proto);
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
      assert(FieldLayout && "can't build a metaclass from a category");
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
        auto base = getClass()->getSuperclassDecl();
        superPtr = getMetaclassRefOrNull(base);
      } else {
        superPtr = getMetaclassRefOrNull(
          IGM.getObjCRuntimeBaseForSwiftRootClass(getClass()));
      }

      auto dataPtr = emitROData(ForMetaClass, DoesNotHaveUpdateCallback);
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
        auto type = getSelfType(getClass()).getASTType();
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

    void emitRODataFields(ConstantStructBuilder &b,
                          ForMetaClass_t forMeta,
                          HasUpdateCallback_t hasUpdater) {
      assert(FieldLayout && "can't emit rodata for a category");

      // struct _class_ro_t {
      //   uint32_t flags;
      b.addInt32(unsigned(buildFlags(forMeta, hasUpdater)));

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
        assert(!hasUpdater);

        // sizeof(struct class_t)
        instanceSize = Size(5 * IGM.getPointerSize().getValue());
        // historical nonsense
        instanceStart = instanceSize;
      } else {
        instanceSize = FieldLayout->getSize();
        instanceStart = FieldLayout->getInstanceStart();
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

      // If hasUpdater is true, the metadata update callback goes here.
      if (hasUpdater) {
        //   Class _Nullable (*metadataUpdateCallback)(Class _Nonnull cls,
        //                                             void * _Nullable arg);
        b.add(IGM.getAddrOfObjCMetadataUpdateFunction(
                TheEntity.get<ClassDecl *>(),
                NotForDefinition));
      }

      // };
    }
    
    llvm::Constant *emitROData(ForMetaClass_t forMeta,
                               HasUpdateCallback_t hasUpdater) {
      ConstantInitBuilder builder(IGM);
      auto fields = builder.beginStruct();
      emitRODataFields(fields, forMeta, hasUpdater);
      
      auto dataSuffix = forMeta ? "_METACLASS_DATA_" : "_DATA_";
      return buildGlobalVariable(fields, dataSuffix);
    }

  private:
    ObjCClassFlags buildFlags(ForMetaClass_t forMeta,
                              HasUpdateCallback_t hasUpdater) {
      ObjCClassFlags flags = ObjCClassFlags::CompiledByARC;

      // Mark metaclasses as appropriate.
      if (forMeta) {
        assert(!hasUpdater);
        flags |= ObjCClassFlags::Meta;

      // Non-metaclasses need us to record things whether primitive
      // construction/destructor is trivial.
      } else if (HasNonTrivialDestructor || HasNonTrivialConstructor) {
        flags |= ObjCClassFlags::HasCXXStructors;
        if (!HasNonTrivialConstructor)
          flags |= ObjCClassFlags::HasCXXDestructorOnly;
      }

      if (hasUpdater)
        flags |= ObjCClassFlags::HasMetadataUpdateCallback;

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
      if (isa<AccessorDecl>(method)) return;

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
      auto dtorRef = SILDeclRef(destructor, SILDeclRef::Kind::Deallocator)
        .asForeign();
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

    void visitMissingMemberDecl(MissingMemberDecl *placeholder) {
      llvm_unreachable("should not IRGen classes with missing members");
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
      auto accessor = dyn_cast<AccessorDecl>(method);
      if (!accessor)
        return emitObjCMethodDescriptor(IGM, descriptors, method);

      switch (accessor->getAccessorKind()) {
      case AccessorKind::Get:
        return emitObjCGetterDescriptor(IGM, descriptors,
                                        accessor->getStorage());

      case AccessorKind::Set:
        return emitObjCSetterDescriptor(IGM, descriptors,
                                        accessor->getStorage());

#define OBJC_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
      case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
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
      if (!FieldLayout)
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
      assert(FieldLayout && "can't build ivar for category");

      auto fields = ivars.beginStruct();

      // For now, we never try to emit specialized versions of the
      // metadata statically, so compute the field layout using the
      // originally-declared type.
      auto pair = FieldLayout->getFieldAccessAndElement(ivar);

      llvm::Constant *offsetPtr;
      switch (pair.first) {
      case FieldAccess::ConstantDirect:
      case FieldAccess::NonConstantDirect: {
        // If the field offset is fixed relative to the start of the superclass,
        // reference the global from the ivar metadata so that the Objective-C
        // runtime will slide it down.
        auto offsetAddr = IGM.getAddrOfFieldOffset(ivar, NotForDefinition);
        offsetPtr = cast<llvm::Constant>(offsetAddr.getAddress());
        break;
      }
      case FieldAccess::ConstantIndirect:
        // Otherwise, swift_initClassMetadata() will point the Objective-C
        // runtime into the field offset vector of the instantiated metadata.
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
      if (auto fixedTI = dyn_cast<FixedTypeInfo>(&pair.second.getType())) {
        size = fixedTI->getFixedSize();
        alignment = fixedTI->getFixedAlignment();
      } else {
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

      auto propTy = prop->getValueInterfaceType();

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
      else if (prop->getAttrs().hasAttribute<ReferenceOwnershipAttr>())
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
      case llvm::Triple::COFF:
        var->setSection(".data");
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

static llvm::Function *emitObjCMetadataUpdateFunction(IRGenModule &IGM,
                                                      ClassDecl *cls) {
  llvm::Function *f =
    IGM.getAddrOfObjCMetadataUpdateFunction(cls, ForDefinition);
  f->setAttributes(IGM.constructInitialAttributes());

  IRGenFunction IGF(IGM, f);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, f);

  // Our parameters are the metadata pointer, and an argument for
  // future use. We just ignore them.
  Explosion params = IGF.collectParameters();
  (void) params.claimAll();

  // Just directly call our metadata accessor. This should actually
  // return the same metadata; the Objective-C runtime enforces this.
  auto type = cls->getDeclaredType()->getCanonicalType();
  auto *metadata = IGF.emitTypeMetadataRef(type,
                                           MetadataState::Complete)
    .getMetadata();
  IGF.Builder.CreateRet(
    IGF.Builder.CreateBitCast(metadata,
                              IGM.ObjCClassPtrTy));

  return f;
}

/// Emit the private data (RO-data) associated with a class.
llvm::Constant *irgen::emitClassPrivateData(IRGenModule &IGM,
                                            ClassDecl *cls) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  PrettyStackTraceDecl stackTraceRAII("emitting ObjC metadata for", cls);
  SILType selfType = getSelfType(cls);
  auto &classTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();

  // FIXME: For now, always use the fragile layout when emitting metadata.
  auto &fieldLayout = classTI.getClassLayout(IGM, selfType,
                                             /*forBackwardDeployment=*/true);
  ClassDataBuilder builder(IGM, cls, fieldLayout);

  // First, build the metaclass object.
  builder.buildMetaclassStub();

  HasUpdateCallback_t hasUpdater = DoesNotHaveUpdateCallback;
  if (doesClassMetadataRequireUpdate(IGM, cls) &&
      !doesClassMetadataRequireInitialization(IGM, cls)) {
    hasUpdater = HasUpdateCallback;
    emitObjCMetadataUpdateFunction(IGM, cls);
  }

  // Then build the class RO-data.
  return builder.emitROData(ForClass, hasUpdater);
}

std::pair<Size, Size>
irgen::emitClassPrivateDataFields(IRGenModule &IGM,
                                  ConstantStructBuilder &init,
                                  ClassDecl *cls) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  PrettyStackTraceDecl stackTraceRAII("emitting ObjC metadata for", cls);

  // This should only be used with generic classes.
  assert(cls->isGenericContext());

  SILType selfType = getSelfType(cls);
  auto &classTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();

  // FIXME: For now, always use the fragile layout when emitting metadata.
  auto &fieldLayout = classTI.getClassLayout(IGM, selfType,
                                             /*forBackwardDeployment=*/true);

  ClassDataBuilder builder(IGM, cls, fieldLayout);

  Size startOfClassRO = init.getNextOffsetFromGlobal();
  assert(startOfClassRO.isMultipleOf(IGM.getPointerSize()));
  {
    auto classRO = init.beginStruct();

    // Note: an update callback is only ever used with the in-place
    // initialization pattern, which precludes generic classes.
    builder.emitRODataFields(classRO,
                             ForClass,
                             DoesNotHaveUpdateCallback);
    classRO.finishAndAddTo(init);
  }

  Size startOfMetaclassRO = init.getNextOffsetFromGlobal();
  assert(startOfMetaclassRO.isMultipleOf(IGM.getPointerSize()));
  {
    auto classRO = init.beginStruct();
    builder.emitRODataFields(classRO, ForMetaClass, DoesNotHaveUpdateCallback);
    classRO.finishAndAddTo(init);
  }

  return std::make_pair(startOfClassRO, startOfMetaclassRO);
}

/// Emit the metadata for an ObjC category.
llvm::Constant *irgen::emitCategoryData(IRGenModule &IGM,
                                        ExtensionDecl *ext) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  ClassDecl *cls = ext->getSelfClassDecl();
  assert(cls && "generating category metadata for a non-class extension");
  
  PrettyStackTraceDecl stackTraceRAII("emitting ObjC metadata for", ext);
  ClassDataBuilder builder(IGM, cls, ext);
  return builder.emitCategory();
}
  
/// Emit the metadata for an ObjC protocol.
llvm::Constant *irgen::emitObjCProtocolData(IRGenModule &IGM,
                                            ProtocolDecl *proto) {
  assert(proto->isObjC() && "not an objc protocol");
  PrettyStackTraceDecl stackTraceRAII("emitting ObjC metadata for", proto);
  ClassDataBuilder builder(IGM, proto);
  return builder.emitProtocol();
}

const TypeInfo *
TypeConverter::convertClassType(CanType type, ClassDecl *D) {
  llvm::StructType *ST = IGM.createNominalType(type);
  llvm::PointerType *irType = ST->getPointerTo();
  ReferenceCounting refcount = type->getReferenceCounting();
  
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
  SwiftRootClass->setIsObjC(Context.LangOpts.EnableObjCInterop);
  SwiftRootClass->getAttrs().add(ObjCAttr::createNullary(Context, objcName,
    /*isNameImplicit=*/true));
  SwiftRootClass->setImplicit();
  SwiftRootClass->setAccess(AccessLevel::Open);
  
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
  while (auto superclass = C->getSuperclassDecl())
    C = superclass;

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

bool irgen::doesClassMetadataRequireRelocation(IRGenModule &IGM,
                                               ClassDecl *theClass) {
  SILType selfType = getSelfType(theClass);
  auto &selfTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();

  // A completely fragile layout does not change whether the metadata
  // requires *relocation*, since that only depends on resilient class
  // ancestry, or the class itself being generic.
  auto &layout = selfTI.getClassLayout(IGM, selfType,
                                       /*forBackwardDeployment=*/false);
  return layout.doesMetadataRequireRelocation();
}

bool irgen::doesClassMetadataRequireInitialization(IRGenModule &IGM,
                                                   ClassDecl *theClass) {
  SILType selfType = getSelfType(theClass);
  auto &selfTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();

  // If we have a fragile layout used for backward deployment, we must use
  // idempotent initialization; swift_initClassMetadata() does not work with
  // statically registered classes.
  auto &layout = selfTI.getClassLayout(IGM, selfType,
                                       /*forBackwardDeployment=*/true);
  return layout.doesMetadataRequireInitialization();
}

bool irgen::doesClassMetadataRequireUpdate(IRGenModule &IGM,
                                           ClassDecl *theClass) {
  SILType selfType = getSelfType(theClass);
  auto &selfTI = IGM.getTypeInfo(selfType).as<ClassTypeInfo>();

  auto &layout = selfTI.getClassLayout(IGM, selfType,
                                       /*forBackwardDeployment=*/false);
  return layout.doesMetadataRequireInitialization();
}

bool irgen::hasKnownSwiftMetadata(IRGenModule &IGM, CanType type) {
  // This needs to be kept up-to-date with getIsaEncodingForType.

  if (ClassDecl *theClass = type.getClassOrBoundGenericClass()) {
    return hasKnownSwiftMetadata(IGM, theClass);
  }

  if (auto archetype = dyn_cast<ArchetypeType>(type)) {
    if (auto superclass = archetype->getSuperclass()) {
      return hasKnownSwiftMetadata(IGM, superclass->getCanonicalType());
    }
  }

  // Class existentials, etc.
  return false;
}

/// Is the given class known to have Swift-compatible metadata?
bool irgen::hasKnownSwiftMetadata(IRGenModule &IGM, ClassDecl *theClass) {
  // For now, the fact that a declaration was not implemented in Swift
  // is enough to conclusively force us into a slower path.
  // Eventually we might have an attribute here or something based on
  // the deployment target.
  return theClass->hasKnownSwiftImplementation();
}

std::pair<llvm::Value *, llvm::Value *>
irgen::emitClassResilientInstanceSizeAndAlignMask(IRGenFunction &IGF,
                                                  ClassDecl *theClass,
                                                  llvm::Value *metadata) {
  auto &layout = IGF.IGM.getClassMetadataLayout(theClass);

  Address metadataAsBytes(IGF.Builder.CreateBitCast(metadata, IGF.IGM.Int8PtrTy),
                          IGF.IGM.getPointerAlignment());

  Address slot = IGF.Builder.CreateConstByteArrayGEP(
      metadataAsBytes,
      layout.getInstanceSizeOffset());
  slot = IGF.Builder.CreateBitCast(slot, IGF.IGM.Int32Ty->getPointerTo());
  llvm::Value *size = IGF.Builder.CreateLoad(slot);
  if (IGF.IGM.SizeTy != IGF.IGM.Int32Ty)
    size = IGF.Builder.CreateZExt(size, IGF.IGM.SizeTy);

  slot = IGF.Builder.CreateConstByteArrayGEP(
      metadataAsBytes,
      layout.getInstanceAlignMaskOffset());
  slot = IGF.Builder.CreateBitCast(slot, IGF.IGM.Int16Ty->getPointerTo());
  llvm::Value *alignMask = IGF.Builder.CreateLoad(slot);
  alignMask = IGF.Builder.CreateZExt(alignMask, IGF.IGM.SizeTy);

  return {size, alignMask};
}

FunctionPointer irgen::emitVirtualMethodValue(IRGenFunction &IGF,
                                              llvm::Value *metadata,
                                              SILDeclRef method,
                                              CanSILFunctionType methodType) {
  Signature signature = IGF.IGM.getSignature(methodType);

  auto classDecl = cast<ClassDecl>(method.getDecl()->getDeclContext());

  // Find the vtable entry we're interested in.
  auto methodInfo =
    IGF.IGM.getClassMetadataLayout(classDecl).getMethodInfo(IGF, method);
  auto offset = methodInfo.getOffset();

  auto slot = IGF.emitAddressAtOffset(metadata, offset,
                                      signature.getType()->getPointerTo(),
                                      IGF.IGM.getPointerAlignment());
  auto fnPtr = IGF.emitInvariantLoad(slot);

  return FunctionPointer(fnPtr, signature);
}

FunctionPointer irgen::emitVirtualMethodValue(IRGenFunction &IGF,
                                              llvm::Value *base,
                                              SILType baseType,
                                              SILDeclRef method,
                                              CanSILFunctionType methodType,
                                              bool useSuperVTable) {
  // Find the metadata.
  llvm::Value *metadata;
  if (useSuperVTable) {
    // For a non-resilient 'super' call, emit a reference to the superclass
    // of the static type of the 'self' value.
    auto instanceTy = baseType.getASTType()->getMetatypeInstanceType();
    auto superTy = instanceTy->getSuperclass();
    metadata = emitClassHeapMetadataRef(IGF,
                                        superTy->getCanonicalType(),
                                        MetadataValueType::TypeMetadata,
                                        MetadataState::Complete);
  } else {
    if (baseType.is<MetatypeType>()) {
      // For a static method call, the 'self' value is already a class metadata.
      metadata = base;
    } else {
      // Otherwise, load the class metadata from the 'self' value's isa pointer.
      metadata = emitHeapMetadataRefForHeapObject(IGF, base, baseType,
                                                  /*suppress cast*/ true);
    }
  }

  return emitVirtualMethodValue(IGF, metadata, method, methodType);
}
