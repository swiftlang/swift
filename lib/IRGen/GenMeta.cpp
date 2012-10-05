//===--- GenMeta.cpp - IR generation for metadata constructs --------------===//
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
//  This file implements IR generation for metadata constructs like
//  metatypes and modules.  These is presently always trivial, but in
//  the future we will likely have some sort of physical
//  representation for at least some metatypes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Substitution.h"
#include "swift/AST/Types.h"
#include "swift/ABI/MetadataValues.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"
#include "llvm/ADT/SmallString.h"

#include "Address.h"
#include "ClassMetadataLayout.h"
#include "FixedTypeInfo.h"
#include "GenClass.h"
#include "GenHeap.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenModule.h"
#include "ScalarTypeInfo.h"
#include "StructMetadataLayout.h"
#include "TypeVisitor.h"

#include "GenMeta.h"

using namespace swift;
using namespace irgen;

namespace {
  struct EmptyTypeInfo : ScalarTypeInfo<EmptyTypeInfo, FixedTypeInfo> {
    EmptyTypeInfo(llvm::Type *ty)
      : ScalarTypeInfo(ty, Size(0), Alignment(1), IsPOD) {}
    unsigned getExplosionSize(ExplosionKind kind) const { return 0; }
    void getSchema(ExplosionSchema &schema) const {}
    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {}
    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {}
    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {}
    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {}
    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {}
    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {}
    void destroy(IRGenFunction &IGF, Address addr) const {}
  };
}

const TypeInfo *TypeConverter::convertMetaTypeType(MetaTypeType *T) {
  return new EmptyTypeInfo(IGM.Int8Ty);
}

const TypeInfo *TypeConverter::convertModuleType(ModuleType *T) {
  return new EmptyTypeInfo(IGM.Int8Ty);
}

namespace {
  /// A structure for collecting generic arguments for emitting a
  /// nominal metadata reference.  The structure produced here is
  /// consumed by swift_getGenericMetadata() and must correspond to
  /// the fill operations that the compiler emits for the bound decl.
  struct GenericArguments {
    /// The values to use to initialize the arguments structure.
    SmallVector<llvm::Value *, 8> Values;
    SmallVector<llvm::Type *, 8> Types;

    void collect(IRGenFunction &IGF, BoundGenericType *type) {
      // Add all the argument archetypes.
      // TODO: only the *primary* archetypes
      // TODO: not archetypes from outer contexts
      // TODO: but we are partially determined by the outer context!
      for (auto &sub : type->getSubstitutions()) {
        CanType subbed = sub.Replacement->getCanonicalType();
        Values.push_back(emitTypeMetadataRef(IGF, subbed));
      }

      // All of those values are metadata pointers.
      Types.append(Values.size(), IGF.IGM.TypeMetadataPtrTy);

      // Add protocol witness tables for all those archetypes.
      for (auto &sub : type->getSubstitutions())
        emitWitnessTableRefs(IGF, sub, Values);

      // All of those values are witness table pointers.
      Types.append(Values.size() - Types.size(), IGF.IGM.WitnessTablePtrTy);
    }
  };
}

/// Returns a metadata reference for a class type.
llvm::Value *irgen::emitNominalMetadataRef(IRGenFunction &IGF,
                                           NominalTypeDecl *theDecl,
                                           CanType theType) {
  auto generics = theDecl->getGenericParamsOfContext();

  bool isPattern = (generics != nullptr);
  assert(!isPattern || isa<BoundGenericType>(theType));
  assert(isPattern || isa<NominalType>(theType));

  bool isIndirect = false; // FIXME

  // Grab a reference to the metadata or metadata template.
  CanType declaredType = theDecl->getDeclaredType()->getCanonicalType();
  llvm::Value *metadata = IGF.IGM.getAddrOfTypeMetadata(declaredType,
                                                        isIndirect, isPattern);

  // If it's indirected, go ahead and load the true value to use.
  // TODO: startup performance might force this to be some sort of
  // lazy check.
  if (isIndirect) {
    auto addr = Address(metadata, IGF.IGM.getPointerAlignment());
    metadata = IGF.Builder.CreateLoad(addr, "metadata.direct");
  }

  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);

  // If we don't have generic parameters, that's all we need.
  if (!generics) {
    return metadata;
  }

  // Okay, we need to call swift_getGenericMetadata.

  // Grab the substitutions.
  auto boundGeneric = cast<BoundGenericType>(theType);
  assert(boundGeneric->getDecl() == theDecl);

  GenericArguments genericArgs;
  genericArgs.collect(IGF, boundGeneric);

  // Slam that information directly into the generic arguments buffer.
  auto argsBufferTy =
    llvm::StructType::get(IGF.IGM.LLVMContext, genericArgs.Types);
  Address argsBuffer = IGF.createAlloca(argsBufferTy,
                                        IGF.IGM.getPointerAlignment(),
                                        "generic.arguments");
  for (unsigned i = 0, e = genericArgs.Values.size(); i != e; ++i) {
    Address elt = IGF.Builder.CreateStructGEP(argsBuffer, i,
                                              IGF.IGM.getPointerSize() * i);
    IGF.Builder.CreateStore(genericArgs.Values[i], elt);
  }

  // Cast to void*.
  llvm::Value *arguments =
    IGF.Builder.CreateBitCast(argsBuffer.getAddress(), IGF.IGM.Int8PtrTy);

  // Make the call.
  auto result = IGF.Builder.CreateCall2(IGF.IGM.getGetGenericMetadataFn(),
                                        metadata, arguments);
  result->setDoesNotThrow();

  return result;
}

/// Emit a string encoding the labels in the given tuple type.
static llvm::Constant *getTupleLabelsString(IRGenModule &IGM,
                                            TupleType *type) {
  bool hasLabels = false;
  llvm::SmallString<128> buffer;
  for (auto &elt : type->getFields()) {
    if (elt.hasName()) {
      hasLabels = true;
      buffer.append(elt.getName().str());
    }

    // Each label is space-terminated.
    buffer += ' ';
  }

  // If there are no labels, use a null pointer.
  if (!hasLabels) {
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  // Otherwise, create a new string literal.
  // This method implicitly adds a null terminator.
  return IGM.getAddrOfGlobalString(buffer);
}

namespace {
  /// A visitor class for emitting a reference to a metatype object.
  class EmitTypeMetadataRef
    : public irgen::TypeVisitor<EmitTypeMetadataRef, llvm::Value *> {
  private:
    IRGenFunction &IGF;
  public:
    EmitTypeMetadataRef(IRGenFunction &IGF) : IGF(IGF) {}

#define TREAT_AS_OPAQUE(KIND)                          \
    llvm::Value *visit##KIND##Type(KIND##Type *type) { \
      return visitOpaqueType(CanType(type));           \
    }
    TREAT_AS_OPAQUE(BuiltinInteger)
    TREAT_AS_OPAQUE(BuiltinFloat)
    TREAT_AS_OPAQUE(BuiltinRawPointer)
#undef TREAT_AS_OPAQUE

    llvm::Value *emitDirectMetadataRef(CanType type) {
      return IGF.IGM.getAddrOfTypeMetadata(type,
                                           /*indirect*/ false,
                                           /*pattern*/ false);
    }

    /// The given type should use opaque type info.  We assume that
    /// the runtime always provides an entry for such a type;  right
    /// now, that mapping is as one of the integer types.
    llvm::Value *visitOpaqueType(CanType type) {
      IRGenModule &IGM = IGF.IGM;
      const TypeInfo &opaqueTI = IGM.getFragileTypeInfo(type);
      assert(opaqueTI.StorageSize ==
             Size(opaqueTI.StorageAlignment.getValue()));
      assert(opaqueTI.StorageSize.isPowerOf2());
      auto numBits = 8 * opaqueTI.StorageSize.getValue();
      auto intTy = BuiltinIntegerType::get(numBits, IGM.Context);
      return emitDirectMetadataRef(CanType(intTy));
    }

    llvm::Value *visitBuiltinObjectPointerType(BuiltinObjectPointerType *type) {
      return emitDirectMetadataRef(CanType(type));
    }

    llvm::Value *visitBuiltinObjCPointerType(BuiltinObjCPointerType *type) {
      return emitDirectMetadataRef(CanType(type));
    }

    llvm::Value *visitNominalType(NominalType *type) {
      return emitNominalMetadataRef(IGF, type->getDecl(), CanType(type));
    }

    llvm::Value *visitBoundGenericType(BoundGenericType *type) {
      return emitNominalMetadataRef(IGF, type->getDecl(), CanType(type));
    }

    llvm::Value *visitTupleType(TupleType *type) {
      auto elements = type->getFields();

      // I think the sanest thing to do here is drop labels, but maybe
      // that's not correct.  If so, that's really unfortunate in a
      // lot of ways.

      // Er, varargs bit?  Should that go in?

      // For metadata purposes, we consider a singleton tuple to be
      // isomorphic to its element type.
      if (elements.size() == 1)
        return visit(CanType(elements[0].getType()));

      // TODO: use a caching entrypoint (with all information
      // out-of-line) for non-dependent tuples.

      // TODO: make a standard metadata for the empty tuple?

      llvm::Value *pointerToFirst;

      // If there are no elements, we can pass a bogus array pointer.
      if (elements.empty()) {
        auto metadataPtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
        pointerToFirst = llvm::UndefValue::get(metadataPtrPtrTy);

      // Otherwise, we need to fill out a temporary array to pass.
      } else {
        pointerToFirst = nullptr; // appease -Wuninitialized

        auto arrayTy = llvm::ArrayType::get(IGF.IGM.TypeMetadataPtrTy,
                                            elements.size());
        Address buffer = IGF.createAlloca(arrayTy, IGF.IGM.getPointerAlignment(),
                                          "tuple-elements");
        for (unsigned i = 0, e = elements.size(); i != e; ++i) {
          // Find the metadata pointer for this element.
          llvm::Value *eltMetadata = visit(CanType(elements[i].getType()));

          // GEP to the appropriate element and store.
          Address eltPtr = IGF.Builder.CreateStructGEP(buffer, i,
                                                       IGF.IGM.getPointerSize());
          IGF.Builder.CreateStore(eltMetadata, eltPtr);

          // Remember the GEP to the first element.
          if (i == 0) pointerToFirst = eltPtr.getAddress();
        }
      }

      llvm::Value *args[] = {
        llvm::ConstantInt::get(IGF.IGM.SizeTy, elements.size()),
        pointerToFirst,
        getTupleLabelsString(IGF.IGM, type),
        llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
      };

      auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadataFn(), args);
      call->setDoesNotThrow();
      call->setCallingConv(IGF.IGM.RuntimeCC);

      return call;
    }

    llvm::Value *visitPolymorphicFunctionType(PolymorphicFunctionType *type) {
      IGF.unimplemented(SourceLoc(),
                        "metadata ref for polymorphic function type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitFunctionType(FunctionType *type) {
      // TODO: use a caching entrypoint (with all information
      // out-of-line) for non-dependent functions.

      auto argMetadata = visit(CanType(type->getInput()));
      auto resultMetadata = visit(CanType(type->getResult()));

      auto call = IGF.Builder.CreateCall2(IGF.IGM.getGetFunctionMetadataFn(),
                                          argMetadata, resultMetadata);
      call->setDoesNotThrow();
      call->setCallingConv(IGF.IGM.RuntimeCC);

      return call;
    }

    llvm::Value *visitArrayType(ArrayType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for array type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitMetaTypeType(MetaTypeType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for metatype type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitModuleType(ModuleType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for module type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitProtocolCompositionType(ProtocolCompositionType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for protocol comp type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitArchetypeType(ArchetypeType *type) {
      return emitArchetypeMetadataRef(IGF, type);
    }

    llvm::Value *visitLValueType(LValueType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for l-value type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }
  };
}

/// Produce the type metadata pointer for the given type.
llvm::Value *irgen::emitTypeMetadataRef(IRGenFunction &IGF, CanType type) {
  return EmitTypeMetadataRef(IGF).visit(type);
}

/// Emit a DeclRefExpr which refers to a metatype.
void irgen::emitMetaTypeRef(IRGenFunction &IGF, Type type,
                            Explosion &explosion) {
  // For now, all metatype types are empty.
}

/*****************************************************************************/
/** Metadata Emission ********************************************************/
/*****************************************************************************/

namespace {
  /// An adapter class which turns a metadata layout class into a
  /// generic metadata layout class.
  template <class Impl, class Base>
  class GenericMetadataBuilderBase : public Base {
    typedef Base super;

    /// The generics clause for the type we're emitting.
    const GenericParamList &ClassGenerics;
    
    /// The number of generic witnesses in the type we're emitting.
    /// This is not really something we need to track.
    unsigned NumGenericWitnesses = 0;

    struct FillOp {
      unsigned FromIndex;
      unsigned ToIndex;

      FillOp() = default;
      FillOp(unsigned from, unsigned to) : FromIndex(from), ToIndex(to) {}
    };

    SmallVector<FillOp, 8> FillOps;

    enum { TemplateHeaderFieldCount = 5 };

  protected:
    using super::IGM;
    using super::Fields;

    template <class... T>
    GenericMetadataBuilderBase(IRGenModule &IGM, 
                               const GenericParamList &generics,
                               T &&...args)
      : super(IGM, std::forward<T>(args)...), ClassGenerics(generics) {}

  public:
    void layout() {
      // Leave room for the header.
      Fields.append(TemplateHeaderFieldCount, nullptr);

      // Lay out the template data.
      super::layout();

      // Fill in the header:

      //   uint32_t NumArguments;

      // TODO: ultimately, this should be the number of actual template
      // arguments, not the number of value witness tables required.
      Fields[0] = llvm::ConstantInt::get(IGM.Int32Ty, NumGenericWitnesses);

      //   uint32_t NumFillOps;
      Fields[1] = llvm::ConstantInt::get(IGM.Int32Ty, FillOps.size());

      //   size_t MetadataSize;
      // We compute this assuming that every entry in the metadata table
      // is a pointer.
      Size size = this->getNextIndex() * IGM.getPointerSize();
      Fields[2] = llvm::ConstantInt::get(IGM.SizeTy, size.getValue());

      //   void *PrivateData[8];
      Fields[3] = getPrivateDataInit();

      //   struct SwiftGenericHeapMetadataFillOp FillOps[NumArguments];
      Fields[4] = getFillOpsInit();

      assert(TemplateHeaderFieldCount == 5);
    }

    /// Ignore the preallocated header.
    unsigned getNextIndex() const {
      return super::getNextIndex() - TemplateHeaderFieldCount;
    }

    template <class... T>
    void addGenericArgument(ArchetypeType *type, T &&...args) {
      FillOps.push_back(FillOp(NumGenericWitnesses++, getNextIndex()));
      super::addGenericArgument(type, std::forward<T>(args)...);
    }

    template <class... T>
    void addGenericWitnessTable(ArchetypeType *type, ProtocolDecl *protocol,
                                T &&...args) {
      FillOps.push_back(FillOp(NumGenericWitnesses++, getNextIndex()));
      super::addGenericWitnessTable(type, protocol, std::forward<T>(args)...);
    }

  private:
    static llvm::Constant *makeArray(llvm::Type *eltTy,
                                     ArrayRef<llvm::Constant*> elts) {
      auto arrayTy = llvm::ArrayType::get(eltTy, elts.size());
      return llvm::ConstantArray::get(arrayTy, elts);
    }

    /// Produce the initializer for the private-data field of the
    /// template header.
    llvm::Constant *getPrivateDataInit() {
      // Spec'ed to be 8 pointers wide.  An arbitrary choice; should
      // work out an ideal size with the runtime folks.
      auto null = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
      
      llvm::Constant *privateData[8] = {
        null, null, null, null, null, null, null, null
      };
      return makeArray(IGM.Int8PtrTy, privateData);
    }

    llvm::Constant *getFillOpsInit() {
      // Construct the type of individual operations.
      llvm::Type *opMemberTys[] = { IGM.Int32Ty, IGM.Int32Ty };
      auto fillOpTy =
        llvm::StructType::get(IGM.getLLVMContext(), opMemberTys, false);

      // Build the array of fill-ops.
      SmallVector<llvm::Constant*, 4> fillOps(FillOps.size());
      for (size_t i = 0, e = FillOps.size(); i != e; ++i) {
        fillOps[i] = getFillOpInit(FillOps[i], fillOpTy);
      }
      return makeArray(fillOpTy, fillOps);
    }

    llvm::Constant *getFillOpInit(const FillOp &op, llvm::StructType *opTy) {
      llvm::Constant *members[] = {
        llvm::ConstantInt::get(IGM.Int32Ty, op.FromIndex),
        llvm::ConstantInt::get(IGM.Int32Ty, op.ToIndex)
      };
      return llvm::ConstantStruct::get(opTy, members);
    }
  };
}

// Classes

namespace {
  /// An adapter for laying out class metadata.
  template <class Impl>
  class ClassMetadataBuilderBase : public ClassMetadataLayout<Impl> {
    typedef ClassMetadataLayout<Impl> super;

  protected:
    using super::IGM;
    using super::TargetClass;
    SmallVector<llvm::Constant *, 8> Fields;
    const HeapLayout &Layout;    

    /// A mapping from functions to their final overriders.
    llvm::DenseMap<FuncDecl*,FuncDecl*> FinalOverriders;

    ClassMetadataBuilderBase(IRGenModule &IGM, ClassDecl *theClass,
                             const HeapLayout &layout)
      : super(IGM, theClass), Layout(layout) {

      computeFinalOverriders();
    }

    unsigned getNextIndex() const { return Fields.size(); }

    /// Compute a map of all the final overriders for the class.
    void computeFinalOverriders() {
      // Walk up the whole class hierarchy.
      ClassDecl *cls = TargetClass;
      do {
        // Make sure that each function has its final overrider set.
        for (auto member : cls->getMembers()) {
          auto fn = dyn_cast<FuncDecl>(member);
          if (!fn) continue;

          // Check whether we already have an entry for this function.
          auto &finalOverrider = FinalOverriders[fn];

          // If not, the function is its own final overrider.
          if (!finalOverrider) finalOverrider = fn;

          // If the function directly overrides something, update the
          // overridden function's entry.
          if (auto overridden = fn->getOverriddenDecl())
            FinalOverriders.insert(std::make_pair(overridden, finalOverrider));

        }

        
      } while (cls->hasBaseClass() &&
               (cls = cls->getBaseClass()->getClassOrBoundGenericClass()));
    }

  public:
    /// The runtime provides a value witness table for Builtin.ObjectPointer.
    void addValueWitnessTable() {
      auto type = CanType(this->IGM.Context.TheObjectPointerType);
      auto wtable = this->IGM.getAddrOfValueWitnessTable(type);
      Fields.push_back(wtable);
    }

    void addDestructorFunction() {
      Fields.push_back(IGM.getAddrOfDestructor(TargetClass));
    }

    void addSizeFunction() {
      Fields.push_back(Layout.createSizeFn(IGM));
    }

    void addNominalTypeDescriptor() {
      // FIXME!
      Fields.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
    }

    void addParentMetadataRef(ClassDecl *forClass) {
      // FIXME!
      Fields.push_back(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }

    void addSuperClass() {
      // FIXME!
      Fields.push_back(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }

    void addMethod(FunctionRef fn) {
      // If this function is associated with the target class, go
      // ahead and emit the witness offset variable.
      if (fn.getDecl()->getDeclContext() == TargetClass) {
        Address offsetVar = IGM.getAddrOfWitnessTableOffset(fn);
        auto global = cast<llvm::GlobalVariable>(offsetVar.getAddress());

        auto offset = Fields.size() * IGM.getPointerSize();
        auto offsetV = llvm::ConstantInt::get(IGM.SizeTy, offset.getValue());
        global->setInitializer(offsetV);
      }

      // Find the final overrider, which we should already have computed.
      auto it = FinalOverriders.find(fn.getDecl());
      assert(it != FinalOverriders.end());
      FuncDecl *finalOverrider = it->second;

      fn = FunctionRef(finalOverrider, fn.getExplosionLevel(),
                       fn.getUncurryLevel());

      // Add the appropriate method to the module.
      Fields.push_back(IGM.getAddrOfFunction(fn, /*with data*/ false));
    }

    void addGenericArgument(ArchetypeType *archetype, ClassDecl *forClass) {
      Fields.push_back(llvm::Constant::getNullValue(IGM.TypeMetadataPtrTy));
    }

    void addGenericWitnessTable(ArchetypeType *archetype,
                                ProtocolDecl *protocol, ClassDecl *forClass) {
      Fields.push_back(llvm::Constant::getNullValue(IGM.WitnessTablePtrTy));
    }

    llvm::Constant *getInit() {
      return llvm::ConstantStruct::getAnon(Fields);
    }
  };

  class ClassMetadataBuilder :
    public ClassMetadataBuilderBase<ClassMetadataBuilder> {
  public:
    ClassMetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                         const HeapLayout &layout)
      : ClassMetadataBuilderBase(IGM, theClass, layout) {}

    void addMetadataFlags() {
      Fields.push_back(llvm::ConstantInt::get(this->IGM.Int8Ty,
                                              unsigned(MetadataKind::Class)));
    }

    llvm::Constant *getInit() {
      if (Fields.size() == NumHeapMetadataFields) {
        return llvm::ConstantStruct::get(IGM.HeapMetadataStructTy, Fields);
      } else {
        return llvm::ConstantStruct::getAnon(Fields);
      }
    }
  };

  /// A builder for metadata templates.
  class GenericClassMetadataBuilder :
    public GenericMetadataBuilderBase<GenericClassMetadataBuilder,
                      ClassMetadataBuilderBase<GenericClassMetadataBuilder>> {

    typedef GenericMetadataBuilderBase super;

  public:
    GenericClassMetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                                const HeapLayout &layout,
                                const GenericParamList &classGenerics)
      : super(IGM, classGenerics, theClass, layout) {}

    void addMetadataFlags() {
      Fields.push_back(llvm::ConstantInt::get(this->IGM.Int8Ty,
                                       unsigned(MetadataKind::GenericClass)));
    }
  };
}

/// Emit the type metadata or metadata template for a class.
void irgen::emitClassMetadata(IRGenModule &IGM, ClassDecl *classDecl,
                              const HeapLayout &layout) {
  // TODO: classes nested within generic types
  llvm::Constant *init;
  bool isPattern;
  if (auto *generics = classDecl->getGenericParamsOfContext()) {
    GenericClassMetadataBuilder builder(IGM, classDecl, layout, *generics);
    builder.layout();
    init = builder.getInit();
    isPattern = true;
  } else {
    ClassMetadataBuilder builder(IGM, classDecl, layout);
    builder.layout();
    init = builder.getInit();
    isPattern = false;
  }

  // For now, all type metadata is directly stored.
  bool isIndirect = false;

  CanType declaredType = classDecl->getDeclaredType()->getCanonicalType();
  auto var = cast<llvm::GlobalVariable>(
                     IGM.getAddrOfTypeMetadata(declaredType,
                                               isIndirect, isPattern,
                                               init->getType()));

  var->setConstant(!isPattern);
  var->setInitializer(init);
}

namespace {
  /// A visitor for checking whether two types are compatible.
  ///
  /// It's guaranteed that 'override' is subtype-related to a
  /// substitution of 'overridden'; this is because dependent
  /// overrides are not allowed by the language.
  class IsIncompatibleOverride :
      public irgen::TypeVisitor<IsIncompatibleOverride, bool, CanType> {

    IRGenModule &IGM;
    ExplosionKind ExplosionLevel;
    bool AsExplosion;

  public:
    IsIncompatibleOverride(IRGenModule &IGM, ExplosionKind explosionLevel,
                           bool asExplosion)
      : IGM(IGM), ExplosionLevel(explosionLevel), AsExplosion(asExplosion) {}

    bool visit(CanType overridden, CanType override) {
      if (override == overridden) return false;

      return TypeVisitor::visit(overridden, override);
    }

    /// Differences in class types must be subtyping related.
    bool visitClassType(ClassType *overridden, CanType override) {
      assert(override->getClassOrBoundGenericClass());
      return false;
    }

    /// Differences in bound generic class types must be subtyping related.
    bool visitBoundGenericType(BoundGenericType *overridden, CanType override) {
      if (isa<ClassDecl>(overridden->getDecl())) {
        assert(override->getClassOrBoundGenericClass());
        return false;
      }
      return visitType(overridden, override);
    }

    bool visitTupleType(TupleType *overridden, CanType overrideTy) {
      TupleType *override = cast<TupleType>(overrideTy);
      assert(overridden->getFields().size() == override->getFields().size());
      for (unsigned i = 0, e = overridden->getFields().size(); i != e; ++i) {
        if (visit(CanType(overridden->getElementType(i)),
                  CanType(override->getElementType(i))))
          return true;
      }
      return false;
    }

    /// Any other difference (unless we add implicit
    /// covariance/contravariance to generic types?) must be a
    /// substitution difference.
    bool visitType(TypeBase *overridden, CanType override) {
      if (AsExplosion)
        return differsByAbstractionInExplosion(IGM, CanType(overridden),
                                               override, ExplosionLevel);
      return differsByAbstractionInMemory(IGM, CanType(overridden), override);
    }
  };
}

static bool isIncompatibleOverrideArgument(IRGenModule &IGM,
                                           CanType overrideTy,
                                           CanType overriddenTy,
                                           ExplosionKind explosionLevel) {
  return IsIncompatibleOverride(IGM, explosionLevel, /*as explosion*/ true)
    .visit(overriddenTy, overrideTy);  
}

static bool isIncompatibleOverrideResult(IRGenModule &IGM,
                                         CanType overrideTy,
                                         CanType overriddenTy,
                                         ExplosionKind explosionLevel) {
  // Fast path.
  if (overrideTy == overriddenTy) return false;

  bool asExplosion;

  // If the overridden type isn't returned indirectly, the overriding
  // type won't be, either, and we need to check as an explosion.
  if (!IGM.requiresIndirectResult(overriddenTy, explosionLevel)) {
    assert(!IGM.requiresIndirectResult(overrideTy, explosionLevel));
    asExplosion = true;

  // Otherwise, if the overriding type isn't returned indirectly,
  // there's an abstration mismatch and the types are incompatible.
  } else if (!IGM.requiresIndirectResult(overrideTy, explosionLevel)) {
    return true;

  // Otherwise, both are returning indirectly and we need to check as
  // memory.
  } else {
    asExplosion = false;
  }

  return IsIncompatibleOverride(IGM, explosionLevel, asExplosion)
    .visit(overriddenTy, overrideTy);
}

/// Is the given method called in the same way that the overridden
/// method is?
static bool isCompatibleOverride(IRGenModule &IGM, FuncDecl *override,
                                 FuncDecl *overridden,
                                 ExplosionKind explosionLevel,
                                 unsigned uncurryLevel) {
  CanType overrideTy = override->getType()->getCanonicalType();
  CanType overriddenTy = overridden->getType()->getCanonicalType();

  // Check arguments for compatibility.
  for (++uncurryLevel; uncurryLevel; --uncurryLevel) {
    // Fast path.
    if (overrideTy == overriddenTy) return true;

    // Note that we're intentionally ignoring any differences in
    // polymorphism --- at the first level that's because that should
    // all be encapsulated in the self argument, and at the later
    // levels because that shouldn't be a legal override.
    auto overrideFnTy = cast<AnyFunctionType>(overrideTy);
    auto overriddenFnTy = cast<AnyFunctionType>(overriddenTy);

    if (isIncompatibleOverrideArgument(IGM,
                                       CanType(overrideFnTy->getInput()),
                                       CanType(overriddenFnTy->getInput()),
                                       explosionLevel))
      return false;

    overrideTy = CanType(overrideFnTy->getResult());
    overriddenTy = CanType(overriddenFnTy->getResult());
  }

  return isIncompatibleOverrideResult(IGM, overrideTy, overriddenTy,
                                      explosionLevel);
}

/// Does the given method require an override entry in the class v-table?
bool irgen::doesMethodRequireOverrideEntry(IRGenModule &IGM, FuncDecl *fn,
                                           ExplosionKind explosionLevel,
                                           unsigned uncurryLevel) {
  // Check each of the overridden declarations in turn.
  FuncDecl *overridden = fn->getOverriddenDecl();
  do {
    assert(overridden);

    // If we ever find something we compatibly override, we're done.
    if (isCompatibleOverride(IGM, fn, overridden,
                             explosionLevel, uncurryLevel))
      return false;

  } while ((overridden = overridden->getOverriddenDecl()));

  // Otherwise, we need a new entry.
  return true;
}

/// Emit a load from the given metadata at a constant index.
static llvm::Value *emitLoadFromMetadataAtIndex(IRGenFunction &IGF,
                                                llvm::Value *metadata,
                                                unsigned index,
                                                llvm::PointerType *objectTy) {
  // Require the metadata to be some type that we recognize as a
  // metadata pointer.
  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy ||
         metadata->getType() == IGF.IGM.HeapMetadataPtrTy);

  // Some offsets are basically just never going to be right.
  assert(index != 0 && "loading flags field?");

  // We require objectType to be a pointer type so that the GEP will
  // scale by the right amount.  We could load an arbitrary type using
  // some extra bitcasting.

  // Cast to T*.
  auto objectPtrTy = objectTy->getPointerTo();
  metadata = IGF.Builder.CreateBitCast(metadata, objectPtrTy);

  // GEP to the slot.
  Address slot(IGF.Builder.CreateConstInBoundsGEP1_32(metadata, index),
               IGF.IGM.getPointerAlignment());

  // Load.
  auto result = IGF.Builder.CreateLoad(slot);
  return result;
}

/// Load the metadata reference at the given index.
static llvm::Value *emitLoadOfMetadataRefAtIndex(IRGenFunction &IGF,
                                                 llvm::Value *metadata,
                                                 unsigned index) {
  return emitLoadFromMetadataAtIndex(IGF, metadata, index,
                                     IGF.IGM.TypeMetadataPtrTy);
}

/// Load the protocol witness table reference at the given index.
static llvm::Value *emitLoadOfWitnessTableRefAtIndex(IRGenFunction &IGF,
                                                     llvm::Value *metadata,
                                                     unsigned index) {
  return emitLoadFromMetadataAtIndex(IGF, metadata, index,
                                     IGF.IGM.WitnessTablePtrTy);
}

namespace {
  /// A CRTP helper for classes which are simply searching for a
  /// specific index within the metadata.
  ///
  /// The pattern is that subclasses should override an 'add' method
  /// from the appropriate layout class and ensure that they call
  /// setTargetIndex() when the appropriate location is reached.  The
  /// subclass user then just calls getTargetIndex(), which performs
  /// the layout and returns the found index.
  ///
  /// \param Base the base class, which should generally be a CRTP
  ///   class template applied to the most-derived class
  template <class Base> class MetadataSearcher : public Base {
    static const unsigned InvalidIndex = ~0U;
    unsigned TargetIndex = InvalidIndex;

  protected:
    void setTargetIndex() {
      assert(TargetIndex == InvalidIndex && "setting twice");
      TargetIndex = this->NextIndex;
    }

  public:
    template <class... T> MetadataSearcher(T &&...args)
      : Base(std::forward<T>(args)...) {}

    unsigned getTargetIndex() {
      assert(TargetIndex == InvalidIndex && "computing twice");
      this->layout();
      assert(TargetIndex != InvalidIndex && "target not found!");
      return TargetIndex;
    }
  };

  /// A class for finding the 'parent' index in a class metadata object.
  class FindClassParentIndex :
      public MetadataSearcher<ClassMetadataScanner<FindClassParentIndex>> {
    typedef MetadataSearcher super;
  public:
    FindClassParentIndex(IRGenModule &IGM, ClassDecl *theClass)
      : super(IGM, theClass) {}

    void addParentMetadataRef(ClassDecl *forClass) {
      if (forClass == TargetClass) setTargetIndex();
      NextIndex++;
    }
  };
}

/// Given a reference to some metadata, derive a reference to the
/// type's parent type.
llvm::Value *irgen::emitParentMetadataRef(IRGenFunction &IGF,
                                          NominalTypeDecl *decl,
                                          llvm::Value *metadata) {
  assert(decl->getDeclContext()->isTypeContext());

  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(id, parent)
#define DECL(id, parent) \
  case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type");

  case DeclKind::Protocol:
    llvm_unreachable("protocols never have parent types!");

  case DeclKind::Class: {
    unsigned index =
      FindClassParentIndex(IGF.IGM, cast<ClassDecl>(decl)).getTargetIndex();
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, index);
  }

  case DeclKind::OneOf:
  case DeclKind::Struct:
    // In both of these cases, 'Parent' is always the fourth field.
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, 3);
  }
  llvm_unreachable("bad decl kind!");
}

namespace {
  /// A class for finding a type argument in a class metadata object.
  class FindClassArgumentIndex :
      public MetadataSearcher<ClassMetadataScanner<FindClassArgumentIndex>> {
    typedef MetadataSearcher super;

    ArchetypeType *TargetArchetype;

  public:
    FindClassArgumentIndex(IRGenModule &IGM, ClassDecl *theClass,
                           ArchetypeType *targetArchetype)
      : super(IGM, theClass), TargetArchetype(targetArchetype) {}

    void addGenericArgument(ArchetypeType *argument, ClassDecl *forClass) {
      if (forClass == TargetClass && argument == TargetArchetype)
        setTargetIndex();
      NextIndex++;
    }
  };

  /// A class for finding a type argument in a struct metadata object.
  class FindStructArgumentIndex :
      public MetadataSearcher<StructMetadataScanner<FindStructArgumentIndex>> {
    typedef MetadataSearcher super;

    ArchetypeType *TargetArchetype;

  public:
    FindStructArgumentIndex(IRGenModule &IGM, StructDecl *decl,
                            ArchetypeType *targetArchetype)
      : super(IGM, decl), TargetArchetype(targetArchetype) {}

    void addGenericArgument(ArchetypeType *argument) {
      if (argument == TargetArchetype)
        setTargetIndex();
      NextIndex++;
    }
  };
}

/// Given a reference to nominal type metadata of the given type,
/// derive a reference to the nth argument metadata.  The type must
/// have generic arguments.
llvm::Value *irgen::emitArgumentMetadataRef(IRGenFunction &IGF,
                                            NominalTypeDecl *decl,
                                            unsigned argumentIndex,
                                            llvm::Value *metadata) {
  assert(decl->getGenericParams() != nullptr);
  auto targetArchetype =
    decl->getGenericParams()->getAllArchetypes()[argumentIndex];

  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(id, parent)
#define DECL(id, parent) \
  case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type");

  case DeclKind::Protocol:
    llvm_unreachable("protocols are never generic!");

  case DeclKind::Class: {
    unsigned index =
      FindClassArgumentIndex(IGF.IGM, cast<ClassDecl>(decl), targetArchetype)
        .getTargetIndex();
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, index);
  }

  case DeclKind::OneOf:
  case DeclKind::Struct:
    // FIXME: should oneofs really be using the struct logic? (no)
    unsigned index =
      FindStructArgumentIndex(IGF.IGM, cast<StructDecl>(decl), targetArchetype)
        .getTargetIndex();
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, index);
  }
  llvm_unreachable("bad decl kind!");
}

namespace {
  /// A class for finding a protocol witness table for a type argument
  /// in a class metadata object.
  class FindClassWitnessTableIndex :
      public MetadataSearcher<ClassMetadataScanner<FindClassWitnessTableIndex>> {
    typedef MetadataSearcher super;

    ArchetypeType *TargetArchetype;
    ProtocolDecl *TargetProtocol;

  public:
    FindClassWitnessTableIndex(IRGenModule &IGM, ClassDecl *theClass,
                               ArchetypeType *targetArchetype,
                               ProtocolDecl *targetProtocol)
      : super(IGM, theClass), TargetArchetype(targetArchetype),
        TargetProtocol(targetProtocol) {}

    void addGenericWitnessTable(ArchetypeType *argument,
                                ProtocolDecl *protocol,
                                ClassDecl *forClass) {
      if (forClass == TargetClass &&
          argument == TargetArchetype &&
          protocol == TargetProtocol)
        setTargetIndex();
      NextIndex++;
    }
  };

  /// A class for finding a protocol witness table for a type argument
  /// in a struct metadata object.
  class FindStructWitnessTableIndex :
      public MetadataSearcher<StructMetadataScanner<FindStructWitnessTableIndex>> {
    typedef MetadataSearcher super;

    ArchetypeType *TargetArchetype;
    ProtocolDecl *TargetProtocol;

  public:
    FindStructWitnessTableIndex(IRGenModule &IGM, StructDecl *decl,
                                ArchetypeType *targetArchetype,
                                ProtocolDecl *targetProtocol)
      : super(IGM, decl), TargetArchetype(targetArchetype) {}

    void addGenericWitnessTable(ArchetypeType *argument,
                                ProtocolDecl *protocol) {
      if (argument == TargetArchetype && protocol == TargetProtocol)
        setTargetIndex();
      NextIndex++;
    }
  };
}

/// Given a reference to nominal type metadata of the given type,
/// derive a reference to a protocol witness table for the nth
/// argument metadata.  The type must have generic arguments.
llvm::Value *irgen::emitArgumentWitnessTableRef(IRGenFunction &IGF,
                                                NominalTypeDecl *decl,
                                                unsigned argumentIndex,
                                                ProtocolDecl *targetProtocol,
                                                llvm::Value *metadata) {
  assert(decl->getGenericParams() != nullptr);
  auto targetArchetype =
    decl->getGenericParams()->getAllArchetypes()[argumentIndex];

  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(id, parent)
#define DECL(id, parent) \
  case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type");

  case DeclKind::Protocol:
    llvm_unreachable("protocols are never generic!");

  case DeclKind::Class: {
    unsigned index =
      FindClassWitnessTableIndex(IGF.IGM, cast<ClassDecl>(decl),
                                 targetArchetype, targetProtocol)
        .getTargetIndex();
    return emitLoadOfWitnessTableRefAtIndex(IGF, metadata, index);
  }

  case DeclKind::OneOf:
  case DeclKind::Struct:
    // FIXME: should oneofs really be using the struct logic? (no)
    unsigned index =
      FindStructWitnessTableIndex(IGF.IGM, cast<StructDecl>(decl),
                                  targetArchetype, targetProtocol)
        .getTargetIndex();
    return emitLoadOfWitnessTableRefAtIndex(IGF, metadata, index);
  }
  llvm_unreachable("bad decl kind!");
}

// Structs

namespace {
  /// An adapter for laying out struct metadata.
  template <class Impl>
  class StructMetadataBuilderBase : public StructMetadataLayout<Impl> {
    typedef StructMetadataLayout<Impl> super;

  protected:
    using super::IGM;
    SmallVector<llvm::Constant *, 8> Fields;

    StructMetadataBuilderBase(IRGenModule &IGM, StructDecl *theStruct)
      : super(IGM, theStruct) {}

    unsigned getNextIndex() const { return Fields.size(); }

  public:
    void addNominalTypeDescriptor() {
      // FIXME!
      Fields.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
    }

    void addParentMetadataRef() {
      // FIXME!
      Fields.push_back(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }

    void addGenericArgument(ArchetypeType *type) {
      Fields.push_back(llvm::Constant::getNullValue(IGM.TypeMetadataPtrTy));
    }

    void addGenericWitnessTable(ArchetypeType *type, ProtocolDecl *protocol) {
      Fields.push_back(llvm::Constant::getNullValue(IGM.WitnessTablePtrTy));
    }

    llvm::Constant *getInit() {
      if (Fields.size() == NumHeapMetadataFields) {
        return llvm::ConstantStruct::get(this->IGM.HeapMetadataStructTy,
                                         Fields);
      } else {
        return llvm::ConstantStruct::getAnon(Fields);
      }
    }
  };

  class StructMetadataBuilder :
    public StructMetadataBuilderBase<StructMetadataBuilder> {
  public:
    StructMetadataBuilder(IRGenModule &IGM, StructDecl *theStruct)
      : StructMetadataBuilderBase(IGM, theStruct) {}

    void addMetadataFlags() {
      Fields.push_back(llvm::ConstantInt::get(this->IGM.Int8Ty,
                                              unsigned(MetadataKind::Struct)));
    }

    void addValueWitnessTable() {
      auto type = this->Target->getDeclaredType()->getCanonicalType();
      Fields.push_back(emitValueWitnessTable(IGM, type));
    }

    llvm::Constant *getInit() {
      return llvm::ConstantStruct::getAnon(Fields);
    }
  };

  /// A builder for metadata templates.
  class GenericStructMetadataBuilder :
    public GenericMetadataBuilderBase<GenericStructMetadataBuilder,
                      StructMetadataBuilderBase<GenericStructMetadataBuilder>> {

    typedef GenericMetadataBuilderBase super;

  public:
    GenericStructMetadataBuilder(IRGenModule &IGM, StructDecl *theStruct,
                                const GenericParamList &classGenerics)
      : super(IGM, classGenerics, theStruct) {}

    void addMetadataFlags() {
      Fields.push_back(llvm::ConstantInt::get(this->IGM.Int8Ty,
                                       unsigned(MetadataKind::GenericStruct)));
    }

    // FIXME.  This is a *horrendous* hack, but:  just apply the generic
    // type at the empty-tuple type a bunch.  I don't have a theory right
    // now for how this should actually work when the witnesses really
    // need stuff from the type.  Laying out the VWT within the pattern,
    // probably, and then making the value witnesses expect that.
    Type buildFakeBoundType(NominalTypeDecl *target) {
      auto generics = target->getGenericParams();
      if (!generics) return target->getDeclaredType();

      Type parent;
      auto DC = target->getDeclContext();
      switch (DC->getContextKind()) {
      case DeclContextKind::TranslationUnit:
      case DeclContextKind::BuiltinModule:
      case DeclContextKind::CapturingExpr:
      case DeclContextKind::TopLevelCodeDecl:
      case DeclContextKind::ConstructorDecl:
      case DeclContextKind::DestructorDecl:
        parent = Type();
        break;

      case DeclContextKind::ExtensionDecl:
        parent = Type(); // FIXME?
        break;

      case DeclContextKind::NominalTypeDecl:
        parent = buildFakeBoundType(cast<NominalTypeDecl>(DC));
        break;
      }

      SmallVector<Type, 8> args;
      args.append(generics->getAllArchetypes().size(),
                  TupleType::getEmpty(IGM.Context));

      return BoundGenericType::get(target, parent, args);
    }

    void addValueWitnessTable() {
      CanType fakeType = buildFakeBoundType(Target)->getCanonicalType();
      Fields.push_back(emitValueWitnessTable(IGM, fakeType));
    }
  };
}

/// Emit the type metadata or metadata template for a struct.
void irgen::emitStructMetadata(IRGenModule &IGM, StructDecl *structDecl) {
  // TODO: structs nested within generic types
  llvm::Constant *init;
  bool isPattern;
  if (auto *generics = structDecl->getGenericParamsOfContext()) {
    GenericStructMetadataBuilder builder(IGM, structDecl, *generics);
    builder.layout();
    init = builder.getInit();
    isPattern = true;
  } else {
    StructMetadataBuilder builder(IGM, structDecl);
    builder.layout();
    init = builder.getInit();
    isPattern = false;
  }

  // For now, all type metadata is directly stored.
  bool isIndirect = false;

  CanType declaredType = structDecl->getDeclaredType()->getCanonicalType();
  auto var = cast<llvm::GlobalVariable>(
                     IGM.getAddrOfTypeMetadata(declaredType,
                                               isIndirect, isPattern,
                                               init->getType()));
  var->setConstant(!isPattern);
  var->setInitializer(init);
}

// Protocols

namespace {
  class ProtocolMetadataBuilder
      : public MetadataLayout<ProtocolMetadataBuilder> {
    typedef MetadataLayout super;
    ProtocolDecl *Protocol;

    llvm::SmallVector<llvm::Constant*, 8> Fields;

  public:
    ProtocolMetadataBuilder(IRGenModule &IGM, ProtocolDecl *protocol)
      : super(IGM), Protocol(protocol) {}

    void layout() {
      super::layout();

      // nominal type descriptor!
      // and so on!
    }

    void addMetadataFlags() {
      Fields.push_back(llvm::ConstantInt::get(IGM.Int8Ty,
                                       unsigned(MetadataKind::Existential)));
    }

    void addValueWitnessTable() {
      // Build a fresh value witness table.  FIXME: this is actually
      // unnecessary --- every existential type will have the exact
      // same value witness table.
      CanType type = CanType(Protocol->getDeclaredType());
      Fields.push_back(emitValueWitnessTable(IGM, type));
    }

    llvm::Constant *getInit() {
      return llvm::ConstantStruct::get(IGM.TypeMetadataStructTy, Fields);
    }
  };
}

/// Emit global structures associated with the given protocol.  That
/// just means the metadata, so go ahead and emit that.
void IRGenModule::emitProtocolDecl(ProtocolDecl *protocol) {
  ProtocolMetadataBuilder builder(*this, protocol);
  builder.layout();
  auto init = builder.getInit();

  // Protocol metadata are always direct and never a pattern.
  bool isIndirect = false;
  bool isPattern = false;

  CanType declaredType = CanType(protocol->getDeclaredType());
  auto var = cast<llvm::GlobalVariable>(
                         getAddrOfTypeMetadata(declaredType,
                                               isIndirect, isPattern,
                                               init->getType()));
  var->setConstant(true);
  var->setInitializer(init);
}
