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
#include "swift/AST/Attr.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
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
#include "llvm/Support/CallSite.h"

#include "Explosion.h"
#include "GenFunc.h"
#include "GenMeta.h"
#include "GenObjC.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "GenHeap.h"
#include "HeapTypeInfo.h"


using namespace swift;
using namespace irgen;

/// Does the given class have a Swift refcount?
bool irgen::hasSwiftRefcount(IRGenModule &IGM, ClassDecl *theClass) {
  // Scan to the root class.
  while (theClass->hasBaseClass()) {
    theClass = theClass->getBaseClass()->getClassOrBoundGenericClass();
    assert(theClass && "base type of class not a class?");
  }

  // If the root class is implemented in swift, then we have a swift
  // refcount.
  return hasKnownSwiftImplementation(IGM, theClass);
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

    /// Can we use swift reference-counting, or do we have to use
    /// objc_retain/release?
    const bool HasSwiftRefcount;

  public:
    ClassTypeInfo(llvm::PointerType *irType, Size size, Alignment align,
                  ClassDecl *D, bool hasSwiftRefcount)
      : HeapTypeInfo(irType, size, align), TheClass(D), Layout(nullptr),
        HasSwiftRefcount(hasSwiftRefcount) {}

    bool hasSwiftRefcount() const {
      return HasSwiftRefcount;
    }

    ~ClassTypeInfo() {
      delete Layout;
    }

    ClassDecl *getClass() const { return TheClass; }

    const StructLayout &getLayout(IRGenModule &IGM) const;

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
                ClassDecl *theClass, CanType type)
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
    void layout(ClassDecl *theClass, CanType type) {
      // TODO: use the full type information to potentially make
      // generic layouts concrete.

      // First, collect information about the base class.
      if (theClass->hasBaseClass()) {
        CanType baseType = theClass->getBaseClass()->getCanonicalType();
        auto baseClass = type->getClassOrBoundGenericClass();
        assert(baseClass);
        layout(baseClass, baseType);
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
        if (theClass->getGenericParamsOfContext() != nullptr) {
          IsObjectGenericallyArranged = true;
        }
      }

      // Okay, make entries for all the physical fields we know about.
      for (auto member : theClass->getMembers()) {
        auto var = dyn_cast<VarDecl>(member);
        if (!var) continue;

        // Skip properties that we have to access logically.
        assert(isClassResilient || !IGM.isResilient(var, Resilience));
        if (var->isProperty())
          continue;

        Fields.push_back(FieldEntry(var, getCurFieldAccess()));

        // Adjust based on the type of this field.
        // FIXME: this algorithm is assuming that fields are laid out
        // in declaration order.
        adjustAccessAfterField(var);
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

    void adjustAccessAfterField(VarDecl *var) {
      if (var->isProperty()) return;

      CanType type = var->getType()->getCanonicalType();
      switch (IGM.classifyTypeSize(type, ResilienceScope::Local)) {
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

/// Return the index of the given field within the class.
static unsigned getFieldIndex(ClassDecl *base, VarDecl *target) {
  // FIXME: This is algorithmically terrible.

  unsigned index = 0;
  for (Decl *member : base->getMembers()) {
    if (member == target) return index;
    if (auto var = dyn_cast<VarDecl>(member))
      if (!var->isProperty())
        ++index;
  }
  llvm_unreachable("didn't find field in type!");
}

namespace {
  class ClassLayoutBuilder : public StructLayoutBuilder {
    SmallVector<ElementLayout, 8> LastElements;
  public:
    ClassLayoutBuilder(IRGenModule &IGM, ClassDecl *theClass)
      : StructLayoutBuilder(IGM) {

      // Start by adding a heap header.
      addHeapHeader();

      // Next, add the fields for the given class.
      addFieldsForClass(theClass);
    }

    /// Return the element layouts for the most-derived class.
    ArrayRef<ElementLayout> getLastElements() const {
      return LastElements;
    }

  private:
    void addFieldsForClass(ClassDecl *theClass) {
      if (theClass->hasBaseClass()) {
        // TODO: apply substitutions when computing base-class layouts!
        auto baseClass = theClass->getBaseClass()->getClassOrBoundGenericClass();
        assert(baseClass);

        // Recurse.
        addFieldsForClass(baseClass);

        // Forget about the fields from the base class.
        LastElements.clear();
      }

      // Collect fields from this class and add them to the layout as a chunk.
      addDirectFieldsFromClass(theClass);
    }

    void addDirectFieldsFromClass(ClassDecl *theClass) {
      assert(LastElements.empty());
      for (Decl *member : theClass->getMembers()) {
        VarDecl *var = dyn_cast<VarDecl>(member);
        if (!var || var->isProperty()) continue;

        auto &eltType = IGM.getFragileTypeInfo(var->getType());
        LastElements.push_back(ElementLayout::getIncomplete(eltType));
      }

      // Add those fields to the builder.
      addFields(LastElements, LayoutStrategy::Universal);
    }
  };
}

const StructLayout &ClassTypeInfo::getLayout(IRGenModule &IGM) const {
  // Return the cached layout if available.
  if (Layout) return *Layout;

  // Add the heap header.
  ClassLayoutBuilder builder(IGM, getClass());

  // Set the body of the class type.
  auto classPtrTy = cast<llvm::PointerType>(getStorageType());
  auto classTy = cast<llvm::StructType>(classPtrTy->getElementType());
  builder.setAsBodyOfStruct(classTy);

  // Record the layout.
  Layout = new StructLayout(builder, classTy, builder.getLastElements());
  return *Layout;
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
                                        llvm::Value *base,
                                        llvm::Value *offset,
                                        VarDecl *field) {
  auto &fieldTI = IGF.getFragileTypeInfo(field->getType());
  auto addr = IGF.emitByteOffsetGEP(base, offset, fieldTI,
                              base->getName() + "." + field->getName().str());
  return OwnedAddress(addr, base);
}

OwnedAddress irgen::projectPhysicalClassMemberAddress(IRGenFunction &IGF,
                                                      llvm::Value *base,
                                                      SILType baseType,
                                                      VarDecl *field) {
  auto &baseClassTI = IGF.getFragileTypeInfo(baseType).as<ClassTypeInfo>();
  ClassDecl *baseClass = baseType.getClassOrBoundGenericClass();
  
  LayoutClass layout(IGF.IGM, ResilienceScope::Local, baseClass,
                     baseType.getSwiftRValueType());
  
  auto &entry = layout.getFieldEntry(field);
  switch (entry.getAccess()) {
    case FieldAccess::ConstantDirect: {
      // FIXME: This field index computation is an ugly hack.
      unsigned fieldIndex = getFieldIndex(baseClass, field);

      Address baseAddr(base, baseClassTI.getHeapAlignment(IGF.IGM));
      auto &element = baseClassTI.getElements(IGF.IGM)[fieldIndex];
      Address memberAddr = element.project(IGF, baseAddr,
                                           /*non fixed offsets*/ nullptr);
      return OwnedAddress(memberAddr, base);
    }
      
    case FieldAccess::NonConstantDirect: {
      Address offsetA = IGF.IGM.getAddrOfFieldOffset(field, /*indirect*/ false);
      auto offset = IGF.Builder.CreateLoad(offsetA, "offset");
      return emitAddressAtOffset(IGF, base, offset, field);
    }
      
    case FieldAccess::ConstantIndirect: {
      auto metadata = emitHeapMetadataRefForHeapObject(IGF, base, baseType);
      auto offset = emitClassFieldOffset(IGF, baseClass, field, metadata);
      return emitAddressAtOffset(IGF, base, offset, field);
    }
      
    case FieldAccess::NonConstantIndirect: {
      auto metadata = emitHeapMetadataRefForHeapObject(IGF, base, baseType);
      Address indirectOffsetA =
        IGF.IGM.getAddrOfFieldOffset(field, /*indirect*/ true);
      auto indirectOffset =
        IGF.Builder.CreateLoad(indirectOffsetA, "indirect-offset");
      auto offsetA =
        IGF.emitByteOffsetGEP(metadata, indirectOffset, IGF.IGM.SizeTy);
      auto offset =
        IGF.Builder.CreateLoad(Address(offsetA, IGF.IGM.getPointerAlignment()));
      return emitAddressAtOffset(IGF, base, offset, field);
    }
  }
  llvm_unreachable("bad field-access strategy");
}


/// Emit the deallocating destructor for a class in terms of its destroying
/// destructor.
void irgen::emitDeallocatingDestructor(IRGenModule &IGM,
                                       ClassDecl *theClass,
                                       llvm::Function *deallocator,
                                       llvm::Function *destroyer) {
  IRGenFunction IGF(IGM, ExplosionKind::Minimal, deallocator);

  Type thisType = theClass->getDeclaredTypeInContext();
  const ClassTypeInfo &info =
    IGM.getFragileTypeInfo(thisType).as<ClassTypeInfo>();
  
  llvm::Value *obj = deallocator->getArgumentList().begin();
  obj = IGF.Builder.CreateBitCast(obj, info.getStorageType());
  // The destroying destructor returns the pointer back as a %swift.refcounted,
  // so we don't need to keep it live across the call.
  obj = IGF.Builder.CreateCall(destroyer, obj);

  // Emit the deallocation.
  llvm::Value *size = info.getLayout(IGM).emitSize(IGF);
  emitDeallocateHeapObject(IGF, obj, size);
  IGF.Builder.CreateRetVoid();
}

/// Emit an allocation of a class.
llvm::Value *irgen::emitClassAllocation(IRGenFunction &IGF, SILType thisType) {
  // FIXME: Long-term, we clearly need a specialized runtime entry point.
  auto &classTI = IGF.IGM.getFragileTypeInfo(thisType).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(IGF.IGM);

  llvm::Value *metadata = emitClassHeapMetadataRef(IGF, thisType);
  
  llvm::Value *size = layout.emitSize(IGF);
  llvm::Value *alignMask = layout.emitAlignMask(IGF);
  llvm::Value *val = IGF.emitAllocObjectCall(metadata, size, alignMask,
                                             "reference.new");
  llvm::Type *destType = layout.getType()->getPointerTo();
  return IGF.Builder.CreateBitCast(val, destType);
}

/// emitClassDecl - Emit all the declarations associated with this class type.
void IRGenModule::emitClassDecl(ClassDecl *D) {
  PrettyStackTraceDecl prettyStackTrace("emitting class metadata for", D);

  auto &classTI = Types.getFragileTypeInfo(D).as<ClassTypeInfo>();
  auto &layout = classTI.getLayout(*this);

  // Emit the class metadata.
  emitClassMetadata(*this, D, layout);

  // Emit the deallocating destructor.
  llvm::Function *deallocator
    = getAddrOfDestructor(D, DestructorKind::Deallocating);
  llvm::Function *destroyer
    = getAddrOfDestructor(D, DestructorKind::Destroying);
  emitDeallocatingDestructor(*this, D, deallocator, destroyer);
  
  // FIXME: This is mostly copy-paste from emitExtension;
  // figure out how to refactor! 
  for (Decl *member : D->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::OneOfElement:
    case DeclKind::Extension:
    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
      llvm_unreachable("decl not allowed in class!");

    // We can have meaningful initializers for variables, but
    // we can't handle them yet.  For the moment, just ignore them.
    case DeclKind::PatternBinding:
      continue;

    case DeclKind::Subscript:
      // Getter/setter will be handled separately.
      continue;

    case DeclKind::TypeAlias:
      continue;
    case DeclKind::OneOf:
      emitOneOfDecl(cast<OneOfDecl>(member));
      continue;
    case DeclKind::Struct:
      emitStructDecl(cast<StructDecl>(member));
      continue;
    case DeclKind::Class:
      emitClassDecl(cast<ClassDecl>(member));
      continue;
    case DeclKind::Var:
      if (cast<VarDecl>(member)->isProperty())
        // Getter/setter will be handled separately.
        continue;
      // FIXME: Will need an implementation here for resilience
      continue;
    case DeclKind::Func:
      emitLocalDecls(cast<FuncDecl>(member));
      continue;
    case DeclKind::Constructor:
      emitLocalDecls(cast<ConstructorDecl>(member));
      continue;
    case DeclKind::Destructor:
      emitLocalDecls(cast<DestructorDecl>(member));
      continue;
    }
    llvm_unreachable("bad extension member kind");
  }
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

  /// A class for building class data (in Objective-C terms, class_ro_t) or
  /// category data (category_t).
  class ClassDataBuilder : public ClassMemberVisitor<ClassDataBuilder> {
    IRGenModule &IGM;
    ClassDecl *TheClass;
    ExtensionDecl *TheExtension;
    const LayoutClass *Layout;
    const StructLayout *FieldLayout;

    bool HasNonTrivialDestructor = false;
    bool HasNonTrivialConstructor = false;
    llvm::SmallString<16> CategoryName;
    SmallVector<llvm::Constant*, 8> Ivars;
    SmallVector<llvm::Constant*, 16> InstanceMethods;
    SmallVector<llvm::Constant*, 16> ClassMethods;
    SmallVector<llvm::Constant*, 4> Protocols;
    SmallVector<llvm::Constant*, 8> Properties;
    llvm::Constant *Name = nullptr;
    unsigned NextFieldIndex = 0;
  public:
    ClassDataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                     const LayoutClass &layout,
                     const StructLayout &fieldLayout)
        : IGM(IGM), TheClass(theClass), TheExtension(nullptr),
          Layout(&layout), FieldLayout(&fieldLayout) {
      visitMembers(TheClass);
    }
    
    ClassDataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                     ExtensionDecl *theExtension)
      : IGM(IGM), TheClass(theClass), TheExtension(theExtension),
        Layout(nullptr), FieldLayout(nullptr) {
          
      buildCategoryName(CategoryName);

      for (Decl *member : TheExtension->getMembers())
        visit(member);
    }

    /// Build the metaclass stub object.
    void buildMetaclassStub() {
      assert(Layout && "can't build a metaclass from a category");
      // The isa is the metaclass pointer for the root class.
      auto rootClass = Layout->getRootClassForMetaclass();
      auto rootPtr = IGM.getAddrOfMetaclassObject(rootClass);

      // The superclass of the metaclass is the metaclass of the
      // superclass.  Note that for metaclass stubs, we can always
      // ignore parent contexts and generic arguments.
      //
      // If this class has no formal superclass, then its actual
      // superclass is SwiftObject, i.e. the root class.
      llvm::Constant *superPtr;
      if (TheClass->hasBaseClass()) {
        auto base = TheClass->getBaseClass()->getClassOrBoundGenericClass();
        superPtr = IGM.getAddrOfMetaclassObject(base);
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
        cast<llvm::GlobalVariable>(IGM.getAddrOfMetaclassObject(TheClass));
      metaclass->setInitializer(init);
    }
    
  private:
    void buildCategoryName(llvm::SmallVectorImpl<char> &s) {
      llvm::raw_svector_ostream os(s);
      // Find the module the extension is declared in.
      DeclContext *ModuleDC = TheExtension;
      do {
        ModuleDC = ModuleDC->getParent();
      } while (ModuleDC && !isa<Module>(ModuleDC));

      Module *TheModule = cast<Module>(ModuleDC);
      
      os << TheModule->Name;
      
      unsigned categoryCount = CategoryCounts[{TheClass, TheModule}]++;
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
      if (TheClass->hasClangNode())
        fields.push_back(IGM.getAddrOfObjCClass(TheClass));
      else {
        llvm::Constant *metadata = tryEmitConstantHeapMetadataRef(IGM,
                      TheClass->getDeclaredTypeOfContext()->getCanonicalType());
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

    llvm::Constant *emitROData(ForMetaClass_t forMeta) {
      assert(Layout && FieldLayout && "can't emit rodata for a category");
      SmallVector<llvm::Constant*, 11> fields;
      // struct _class_ro_t {
      //   uint32_t flags;
      fields.push_back(buildFlags(forMeta));

      //   uint32_t instanceStart;
      //   uint32_t instanceSize;
      // The runtime requires that the ivar offsets be initialized to
      // a valid layout of the ivars of this class, bounded by these
      // two values.  If the instanceSize of the base class equals the
      // stored instanceStart of the derived class, the ivar offsets
      // will not be changed.
      Size instanceStart = Size(0);
      Size instanceSize = Size(0);
      if (!forMeta) {
        instanceSize = FieldLayout->getSize();
        if (FieldLayout->getElements().empty()) {
          instanceStart = instanceSize;
        } else {
          // FIXME: assumes layout is always sequential!
          instanceStart = FieldLayout->getElements()[0].getByteOffset();
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

      auto dataSuffix = forMeta ? "_METACLASS_DATA_" : "_DATA_";
      return buildGlobalVariable(fields, dataSuffix);
    }

  private:
    llvm::Constant *buildFlags(ForMetaClass_t forMeta) {
      ClassFlags flags = ClassFlags::CompiledByARC;

      // Mark metaclasses as appropriate.
      if (forMeta) {
        flags |= ClassFlags::Meta;

      // Non-metaclasses need us to record things whether primitive
      // construction/destructor is trivial.
      } else if (HasNonTrivialDestructor || HasNonTrivialConstructor) {
        flags |= ClassFlags::HasCXXStructors;
        if (!HasNonTrivialConstructor)
          flags |= ClassFlags::HasCXXDestructorOnly;
      }

      // FIXME: set ClassFlags::Hidden when appropriate
      return llvm::ConstantInt::get(IGM.Int32Ty, uint32_t(flags));
    }

    llvm::Constant *buildName() {
      if (Name) return Name;

      // If the class is being exported as an Objective-C class, we
      // should export it under its formal name.
      if (TheClass->isObjC()) {
        Name = IGM.getAddrOfGlobalString(TheClass->getName().str());
        return Name;
      }

      // Otherwise, we need to mangle the type.
      auto type = TheClass->getDeclaredType()->getCanonicalType();

      SmallVector<char, 128> buffer;
      Name = IGM.getAddrOfGlobalString(IGM.mangleType(type, buffer));
      return Name;
    }

    llvm::Constant *null() {
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }

    /*** Methods ***********************************************************/

  public:
    /// Methods need to be collected into the appropriate methods list.
    void visitFuncDecl(FuncDecl *method) {
      if (!requiresObjCMethodDescriptor(method)) return;
      llvm::Constant *entry = emitObjCMethodDescriptor(IGM, method);
      if (!method->isStatic()) {
        InstanceMethods.push_back(entry);
      } else {
        ClassMethods.push_back(entry);
      }
    }

  private:
    llvm::Constant *buildClassMethodList()  {
      return buildMethodList(ClassMethods, TheExtension
                               ? "_CATEGORY_CLASS_METHODS_"
                               : "_CLASS_METHODS_");
    }

    llvm::Constant *buildInstanceMethodList()  {
      return buildMethodList(InstanceMethods, TheExtension
                               ? "_CATEGORY_INSTANCE_METHODS_"
                               : "_INSTANCE_METHODS_");
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
      // FIXME
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }
    
    /// struct protocol_list_t {
    ///   uintptr_t count;
    ///   protocol_ref_t[count];
    /// };
    ///
    /// This method does not return a value of a predictable type.
    llvm::Constant *buildProtocolList() {
      return buildOptionalList(Protocols, Size(0), TheExtension
                                ? "_CATEGORY_PROTOCOLS_"
                                : "_PROTOCOLS_");
    }

    /*** Ivars *************************************************************/

  public:
    /// Variables might be properties or ivars.
    void visitVarDecl(VarDecl *var) {
      if (var->isProperty()) {
        visitProperty(var);
      } else {
        visitIvar(var);
      }
    }

  private:
    /// Ivars need to be collected in the ivars list, and they also
    /// affect flags.
    void visitIvar(VarDecl *var) {
      // FIXME: how to handle ivar extensions in categories?
      if (!Layout && !FieldLayout)
        return;

      Ivars.push_back(buildIvar(var));
      if (!IGM.isPOD(var->getType()->getCanonicalType(),
                     ResilienceScope::Local)) {
        HasNonTrivialDestructor = true;
      }

      // Build property accessors for the ivar if necessary.
      visitProperty(var);
    }

    /// struct ivar_t {
    ///   uintptr_t *offset;
    ///   const char *name;
    ///   const char *type;
    ///   uint32_t alignment;
    ///   uint32_t size;
    /// };
    llvm::Constant *buildIvar(VarDecl *ivar) {
      assert(Layout && FieldLayout && "can't build ivar for category");
      // FIXME: this is not always the right thing to do!
      auto &elt = FieldLayout->getElements()[NextFieldIndex++];
      auto offsetAddr = IGM.getAddrOfFieldOffset(ivar, /*indirect*/ false);
      auto offsetVar = cast<llvm::GlobalVariable>(offsetAddr.getAddress());
      offsetVar->setConstant(false);
      auto offsetVal =
        llvm::ConstantInt::get(IGM.IntPtrTy, elt.getByteOffset().getValue());
      offsetVar->setInitializer(offsetVal);

      // TODO: clang puts this in __TEXT,__objc_methname,cstring_literals
      auto name = IGM.getAddrOfGlobalString(ivar->getName().str());

      // TODO: clang puts this in __TEXT,__objc_methtype,cstring_literals
      auto typeEncode = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

      auto &ivarTI = IGM.getFragileTypeInfo(ivar->getType());
      Size size;
      Alignment alignment;
      if (auto fixedTI = dyn_cast<FixedTypeInfo>(&ivarTI)) {
        size = fixedTI->getFixedSize();
        alignment = fixedTI->getFixedAlignment();
      } else {
        // FIXME: set something up to fill these in at runtime!
        size = Size(0);
        alignment = Alignment(0);
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
        offsetVar,
        name,
        typeEncode,
        llvm::ConstantInt::get(IGM.Int32Ty, size.getValue()),
        llvm::ConstantInt::get(IGM.Int32Ty, alignment.getValue())
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
      if (requiresObjCPropertyDescriptor(var)) {
        if (llvm::Constant *prop = buildProperty(var))
          Properties.push_back(prop);
        auto getter_setter = emitObjCPropertyMethodDescriptors(IGM, var);
        InstanceMethods.push_back(getter_setter.first);
        if (getter_setter.second)
          InstanceMethods.push_back(getter_setter.second);
      }
    }
    
    /// Build the property attribute string for a property decl.
    void buildPropertyAttributes(VarDecl *prop,
                                 llvm::SmallVectorImpl<char> &out,
                                 ClassDecl *theClass) {
      llvm::raw_svector_ostream outs(out);
      
      // Emit the type encoding.
      // FIXME: Only correct for class types.
      outs << "T@";
      // FIXME: Assume 'NSObject' really means 'id'.
      if (theClass->getName() != prop->getASTContext().getIdentifier("NSObject"))
        outs << '"' << theClass->getName().str() << '"';
      
      // FIXME: Emit attributes for (nonatomic, strong) if the property has a
      // setter, or (nonatomic, readonly) if the property has only a getter.
      // Are these attributes always appropriate?
      outs << (prop->isSettable()
        ? ",&,N" // strong, nonatomic
        : ",R,N"); // readonly, nonatomic
      
      // Emit the selector name for the getter. Clang only appears to emit the
      // setter name if the property has an explicit setter= attribute.
      outs << ",V" << prop->getName();
      
      outs.flush();
    }

    /// struct property_t {
    ///   const char *name;
    ///   const char *attributes;
    /// };
    llvm::Constant *buildProperty(VarDecl *prop) {
      // FIXME: For now we only emit properties of ObjC class type.
      ClassDecl *theClass
        = IGM.SILMod->Types.getLoweredBridgedType(prop->getType(),
                                                  AbstractCC::ObjCMethod)
          ->getClassOrBoundGenericClass();
      if (!theClass)
        return nullptr;
      if (!theClass->isObjC())
        return nullptr;

      llvm::SmallString<16> propertyAttributes;
      buildPropertyAttributes(prop, propertyAttributes, theClass);
      
      llvm::Constant *fields[] = {
        IGM.getAddrOfGlobalString(prop->getName().str()),
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
      return buildOptionalList(Properties, eltSize, TheExtension
                                ? "_CATEGORY_PROPERTIES_"
                                : "_PROPERTIES_");
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

      auto arrayTy =
        llvm::ArrayType::get(objects[0]->getType(), objects.size());
      fields.push_back(llvm::ConstantArray::get(arrayTy, objects));

      return buildGlobalVariable(fields, nameBase);
    }

    /// Build a private global variable as a structure containing the
    /// given fields.
    llvm::Constant *buildGlobalVariable(ArrayRef<llvm::Constant*> fields,
                                        StringRef nameBase) {
      auto init = llvm::ConstantStruct::getAnon(IGM.getLLVMContext(), fields);
      auto var = new llvm::GlobalVariable(IGM.Module, init->getType(),
                                        /*constant*/ true,
                                        llvm::GlobalVariable::PrivateLinkage,
                                        init,
                                        Twine(nameBase) 
                                          + TheClass->getName().str()
                                          + (TheExtension
                                             ? Twine("_$_") + CategoryName.str()
                                             : Twine()));
      var->setAlignment(IGM.getPointerAlignment().getValue());
      var->setSection("__DATA, __objc_const");
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
      // TODO
    }

    /// Constructors should probably be collected in extended metadata.
    void visitConstructorDecl(ConstructorDecl *ctor) {
      // TODO
    }

    /// The destructor doesn't really require any special
    /// representation here.
    void visitDestructorDecl(DestructorDecl *dtor) {}
  };
}

/// Emit the private data (RO-data) associated with a class.
llvm::Constant *irgen::emitClassPrivateData(IRGenModule &IGM,
                                            ClassDecl *cls) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  CanType type = cls->getDeclaredTypeInContext()->getCanonicalType();
  auto &classTI = IGM.getFragileTypeInfo(type).as<ClassTypeInfo>();
  auto &fieldLayout = classTI.getLayout(IGM);
  LayoutClass layout(IGM, ResilienceScope::Universal, cls, type);
  ClassDataBuilder builder(IGM, cls, layout, fieldLayout);

  // First, build the metaclass object.
  builder.buildMetaclassStub();

  // Then build the class RO-data.
  return builder.emitROData(ForClass);
}
  
/// Emit the metadata for an ObjC category.
llvm::Constant *irgen::emitCategoryData(IRGenModule &IGM,
                                        ExtensionDecl *ext) {
  assert(IGM.ObjCInterop && "emitting RO-data outside of interop mode");
  ClassDecl *cls = ext->getDeclaredTypeInContext()
    ->getClassOrBoundGenericClass();
  assert(cls && "generating category metadata for a non-class extension");
  assert(cls->isObjC() && "generating category metadata for a non-objc class");
  
  ClassDataBuilder builder(IGM, cls, ext);
  
  return builder.emitCategory();
}

const TypeInfo *TypeConverter::convertClassType(ClassDecl *D) {
  llvm::StructType *ST = IGM.createNominalType(D);
  llvm::PointerType *irType = ST->getPointerTo();
  bool hasSwiftRefcount = ::hasSwiftRefcount(IGM, D);
  return new ClassTypeInfo(irType, IGM.getPointerSize(),
                           IGM.getPointerAlignment(),
                           D, hasSwiftRefcount);
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
  SwiftRootClass->getMutableAttrs().ObjC = true;
  SwiftRootClass->setIsObjC(true);
  return SwiftRootClass;
}
