//===--- GenKeyPath.cpp - IRGen support for key path objects --------------===//
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
//  This file contains code for emitting key path patterns, which can be used
//  by the standard library to instantiate key path objects.
//
//===----------------------------------------------------------------------===//

#include "Callee.h"
#include "ConstantBuilder.h"
#include "Explosion.h"
#include "GenClass.h"
#include "GenDecl.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenStruct.h"
#include "GenericRequirement.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "ProtocolInfo.h"
#include "StructLayout.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/IR/Module.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/ABI/KeyPath.h"
#include "swift/ABI/HeapObject.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsIRGen.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Types.h"
#include "swift/IRGen/Linking.h"

using namespace swift;
using namespace irgen;

enum GetterOrSetter {
  Getter,
  Setter,
};

static llvm::Function *
getAccessorForComputedComponent(IRGenModule &IGM,
                                const KeyPathPatternComponent &component,
                                GetterOrSetter whichAccessor,
                                GenericEnvironment *genericEnv,
                                ArrayRef<GenericRequirement> requirements) {
  SILFunction *accessor;
  switch (whichAccessor) {
  case Getter:
    accessor = component.getComputedPropertyGetter();
    break;
  case Setter:
    accessor = component.getComputedPropertySetter();
    break;
  }
  
  auto accessorFn = IGM.getAddrOfSILFunction(accessor, NotForDefinition);
  
  // If the accessor is not generic, we can use it as is.
  if (requirements.empty()) {
    return accessorFn;
  }

  auto accessorFnTy = accessorFn->getType()->getPointerElementType();
  
  // Otherwise, we need a thunk to unmarshal the generic environment from the
  // argument area. It'd be nice to have a good way to represent this
  // directly in SIL, of course...
  auto thunkType = llvm::FunctionType::get(
      IGM.VoidTy,
      { /*sret or newValue*/ accessorFnTy->getFunctionParamType(0),
        /*base*/ accessorFnTy->getFunctionParamType(1),
        /*arg*/ IGM.Int8PtrTy },
      /*vararg*/ false);
  const char *thunkName;
  unsigned numArgsToForward = 2;
  switch (whichAccessor) {
  case Getter:
    thunkName = "keypath_get";
    break;
  case Setter:
    thunkName = "keypath_set";
    break;
  }
  
  auto accessorThunk = llvm::Function::Create(thunkType,
    llvm::GlobalValue::PrivateLinkage, thunkName, IGM.getModule());
  accessorThunk->setAttributes(IGM.constructInitialAttributes());
  // Original accessor's args should be @in or @out, meaning they won't be
  // captured or aliased.
  accessorThunk->addAttribute(1, llvm::Attribute::NoCapture);
  accessorThunk->addAttribute(1, llvm::Attribute::NoAlias);
  accessorThunk->addAttribute(2, llvm::Attribute::NoCapture);
  accessorThunk->addAttribute(2, llvm::Attribute::NoAlias);
  // Getter's output is sret.
  if (whichAccessor == Getter)
    accessorThunk->addAttribute(1, llvm::Attribute::StructRet);
  accessorThunk->setCallingConv(IGM.SwiftCC);
  
  {
    IRGenFunction IGF(IGM, accessorThunk);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(IGF, accessorThunk);
      
    auto params = IGF.collectParameters();
    Explosion forwardedArgs;
    forwardedArgs.add(params.claim(numArgsToForward));
    
    // The generic environment is marshaled into the beginning of the component
    // argument area inside the instance. Bind the generic information out of
    // the buffer, and advance past it.
    auto componentArgsBuf = params.claimNext();
    bindFromGenericRequirementsBuffer(IGF, requirements,
      Address(componentArgsBuf, IGM.getPointerAlignment()),
      [&](CanType t) {
        if (!genericEnv)
          return t;
        return genericEnv->mapTypeIntoContext(t)->getCanonicalType();
      });
      
    /* TODO: If the underlying accessor wants index arguments, advance the
     * pointer past the generic requirements here to pass down. */
     
    // Use the bound generic metadata to form a call to the original generic
    // accessor.
    WitnessMetadata witnessMetadata;
    auto forwardingSubs = genericEnv->getGenericSignature()->getSubstitutionMap(
      genericEnv->getForwardingSubstitutions());
    emitPolymorphicArguments(IGF, accessor->getLoweredFunctionType(),
                             forwardingSubs,
                             &witnessMetadata,
                             forwardedArgs);
    auto fnPtr = FunctionPointer::forDirect(IGM, accessorFn,
                                          accessor->getLoweredFunctionType());
    IGF.Builder.CreateCall(fnPtr, forwardedArgs.claimAll());
    
    IGF.Builder.CreateRetVoid();
  }
  
  return accessorThunk;
}

static llvm::Constant *
getLayoutFunctionForComputedComponent(IRGenModule &IGM,
                                    const KeyPathPatternComponent &component,
                                    GenericEnvironment *genericEnv,
                                    ArrayRef<GenericRequirement> requirements) {
  // Generate a function that returns the expected size and alignment necessary
  // to store captured generic context and subscript index arguments.
  auto retTy = llvm::StructType::get(IGM.getLLVMContext(),
                                     {IGM.SizeTy, IGM.SizeTy});
  auto fnTy = llvm::FunctionType::get(
    retTy, { IGM.Int8PtrTy }, /*vararg*/ false);
    
  auto layoutFn = llvm::Function::Create(fnTy,
    llvm::GlobalValue::PrivateLinkage, "keypath_get_arg_layout", IGM.getModule());
    
  {
    IRGenFunction IGF(IGM, layoutFn);
    // TODO: We would need to unmarshal generic arguments to be able to
    // compute the layout of dependent subscript indexes.
    (void)IGF.collectParameters().claimNext();
    
    // Base size is one pointer for each generic requirement; base alignment
    // is pointer alignment.
    llvm::Value *size = llvm::ConstantInt::get(IGM.SizeTy,
      IGM.getPointerSize().getValue() * requirements.size());
    llvm::Value *alignMask = llvm::ConstantInt::get(IGM.SizeTy,
      IGM.getPointerAlignment().getValue() - 1);
      
    // TODO: Combine layout of captured index values
    
    llvm::Value *retValue = IGF.Builder.CreateInsertValue(
      llvm::UndefValue::get(retTy), size, 0);
    retValue = IGF.Builder.CreateInsertValue(
      retValue, alignMask, 1);
      
    IGF.Builder.CreateRet(retValue);
  }
  
  return layoutFn;
}

static llvm::Constant *
getWitnessTableForComputedComponent(IRGenModule &IGM,
                                    const KeyPathPatternComponent &component,
                                    GenericEnvironment *genericEnv,
                                    ArrayRef<GenericRequirement> requirements) {
  // If the only thing we're capturing is generic environment, then we can
  // use a prefab witness table from the runtime.
  // TODO: If there were subscript indexes, we'd need to generate something.
  if (auto existing =
        IGM.Module.getNamedGlobal("swift_keyPathGenericWitnessTable"))
    return existing;
  
  auto linkInfo = LinkInfo::get(IGM, "swift_keyPathGenericWitnessTable",
                                SILLinkage::PublicExternal,
                                /*fragile*/ false,
                                /*sil only*/ false,
                                NotForDefinition,
                                /*weak imported*/ false);
  
  return createVariable(IGM, linkInfo,
                        IGM.Int8PtrTy, IGM.getPointerAlignment());
}

static llvm::Constant *
getInitializerForComputedComponent(IRGenModule &IGM,
                                   const KeyPathPatternComponent &component,
                                   GenericEnvironment *genericEnv,
                                   ArrayRef<GenericRequirement> requirements) {
  auto fnTy = llvm::FunctionType::get(IGM.VoidTy,
    { /*src*/ IGM.Int8PtrTy,
      /*dest*/ IGM.Int8PtrTy }, /*vararg*/ false);
      
  auto initFn = llvm::Function::Create(fnTy,
    llvm::GlobalValue::PrivateLinkage, "keypath_arg_init", IGM.getModule());
    
  {
    IRGenFunction IGF(IGM, initFn);
    auto params = IGF.collectParameters();
    auto src = params.claimNext();
    auto dest = params.claimNext();
    
    // Transfer all of the requirements into the destination instance.
    IGF.Builder.CreateMemCpy(dest, src,
                         IGM.getPointerSize().getValue() * requirements.size(),
                         IGM.getPointerAlignment().getValue());
    
    // TODO: Copy over subscript index values.
    
    IGF.Builder.CreateRetVoid();
  }
  return initFn;
}

llvm::Constant *
IRGenModule::getAddrOfKeyPathPattern(KeyPathPattern *pattern,
                                     SILLocation diagLoc) {
  // See if we already emitted this.
  auto found = KeyPathPatterns.find(pattern);
  if (found != KeyPathPatterns.end())
    return found->second;
  
  // Gather type arguments from the root and leaf types of the key path.
  auto rootTy = pattern->getRootType();
  auto valueTy = pattern->getValueType();

  // Check for parameterization, whether by subscript indexes or by the generic
  // environment. If there isn't any, we can instantiate the pattern in-place.
  bool isInstantiableInPlace = pattern->getNumOperands() == 0
    && !pattern->getGenericSignature();

  // Collect the required parameters for the keypath's generic environment.
  SmallVector<GenericRequirement, 4> requirements;
  
  GenericEnvironment *genericEnv = nullptr;
  if (auto sig = pattern->getGenericSignature()) {
    genericEnv = sig->createGenericEnvironment(*getSwiftModule());
    enumerateGenericSignatureRequirements(pattern->getGenericSignature(),
      [&](GenericRequirement reqt) { requirements.push_back(reqt); });
  }

  /// Generate a metadata accessor that produces metadata for the given type
  /// using arguments from the generic context of the key path.
  auto emitMetadataGenerator = [&](CanType type) -> llvm::Function * {
    // TODO: Use the standard metadata accessor when there are no arguments
    // and the metadata accessor is defined.
    
    // Build a stub that loads the necessary bindings from the key path's
    // argument buffer then fetches the metadata.
    auto fnTy = llvm::FunctionType::get(TypeMetadataPtrTy,
                                        {Int8PtrTy}, /*vararg*/ false);
    auto accessorThunk = llvm::Function::Create(fnTy,
                                              llvm::GlobalValue::PrivateLinkage,
                                              "keypath_get_type", getModule());
    accessorThunk->setAttributes(constructInitialAttributes());
    {
      IRGenFunction IGF(*this, accessorThunk);
      if (DebugInfo)
        DebugInfo->emitArtificialFunction(IGF, accessorThunk);

      if (type->hasTypeParameter()) {
        auto bindingsBufPtr = IGF.collectParameters().claimNext();

        bindFromGenericRequirementsBuffer(IGF, requirements,
          Address(bindingsBufPtr, getPointerAlignment()),
          [&](CanType t) {
            if (!genericEnv)
              return t;
            return genericEnv->mapTypeIntoContext(t)->getCanonicalType();
          });
      
        type = genericEnv->mapTypeIntoContext(type)->getCanonicalType();
      }
      auto ret = IGF.emitTypeMetadataRef(type);
      IGF.Builder.CreateRet(ret);
    }
    return accessorThunk;
  };
  
  // Start building the key path pattern.
  ConstantInitBuilder builder(*this);
  ConstantStructBuilder fields = builder.beginStruct();
  fields.setPacked(true);
  // Add a zero-initialized header we can use for lazy initialization.
  fields.add(llvm::ConstantInt::get(SizeTy, 0));

#ifndef NDEBUG
  auto startOfObject = fields.getNextOffsetFromGlobal();
#endif

  // Store references to metadata generator functions to generate the metadata
  // for the root and leaf. These sit in the "isa" and object header parts of
  // the final object.
  fields.add(emitMetadataGenerator(rootTy));
  fields.add(emitMetadataGenerator(valueTy));
  
  // TODO: 32-bit heap object header still has an extra word
  if (SizeTy == Int32Ty) {
    fields.addInt32(0);
  }
  
#ifndef NDEBUG
  auto endOfObjectHeader = fields.getNextOffsetFromGlobal();
  unsigned expectedObjectHeaderSize;
  if (SizeTy == Int64Ty)
    expectedObjectHeaderSize = SWIFT_ABI_HEAP_OBJECT_HEADER_SIZE_64;
  else if (SizeTy == Int32Ty)
    expectedObjectHeaderSize = SWIFT_ABI_HEAP_OBJECT_HEADER_SIZE_32;
  else
    llvm_unreachable("unexpected pointer size");
  assert((endOfObjectHeader - startOfObject).getValue()
            == expectedObjectHeaderSize
       && "key path pattern header size doesn't match heap object header size");
#endif
  
  // Add a pointer to the ObjC KVC compatibility string, if there is one, or
  // null otherwise.
  llvm::Constant *objcString;
  if (!pattern->getObjCString().empty()) {
    objcString = getAddrOfGlobalString(pattern->getObjCString());
  } else {
    objcString = llvm::ConstantPointerNull::get(Int8PtrTy);
  }
  fields.add(objcString);
  
  // Leave a placeholder for the buffer header, since we need to know the full
  // buffer size to fill it in.
  auto headerPlaceholder = fields.addPlaceholderWithSize(Int32Ty);
  fields.addAlignmentPadding(getPointerAlignment());
  
  auto startOfKeyPathBuffer = fields.getNextOffsetFromGlobal();
  
  // Build out the components.
  auto baseTy = rootTy;
  
  auto assertPointerAlignment = [&]{
    assert(fields.getNextOffsetFromGlobal() % getPointerAlignment() == Size(0)
           && "must be pointer-aligned here");
  };
  
  for (unsigned i : indices(pattern->getComponents())) {
    assertPointerAlignment();
    SILType loweredBaseTy;
    Lowering::GenericContextScope scope(getSILTypes(),
                                        pattern->getGenericSignature());
    loweredBaseTy = getLoweredType(AbstractionPattern::getOpaque(),
                                   baseTy->getWithoutSpecifierType());
    auto &component = pattern->getComponents()[i];
    switch (auto kind = component.getKind()) {
    case KeyPathPatternComponent::Kind::StoredProperty: {
      auto property = cast<VarDecl>(component.getStoredPropertyDecl());
      
      auto addFixedOffset = [&](bool isStruct, llvm::Constant *offset) {
        if (auto offsetInt = dyn_cast_or_null<llvm::ConstantInt>(offset)) {
          auto offsetValue = offsetInt->getValue().getZExtValue();
          if (KeyPathComponentHeader::offsetCanBeInline(offsetValue)) {
            auto header = isStruct
              ? KeyPathComponentHeader::forStructComponentWithInlineOffset(offsetValue)
              : KeyPathComponentHeader::forClassComponentWithInlineOffset(offsetValue);
            fields.addInt32(header.getData());
            return;
          }
        }
        auto header = isStruct
          ? KeyPathComponentHeader::forStructComponentWithOutOfLineOffset()
          : KeyPathComponentHeader::forClassComponentWithOutOfLineOffset();
        fields.addInt32(header.getData());
        fields.add(llvm::ConstantExpr::getTruncOrBitCast(offset, Int32Ty));
      };
      
      // For a struct stored property, we may know the fixed offset of the field,
      // or we may need to fetch it out of the type's metadata at instantiation
      // time.
      if (loweredBaseTy.getStructOrBoundGenericStruct()) {
        if (auto offset = emitPhysicalStructMemberFixedOffset(*this,
                                                              loweredBaseTy,
                                                              property)) {
          // We have a known constant fixed offset.
          addFixedOffset(/*struct*/ true, offset);
          break;
        }
        
        // If the offset isn't fixed, try instead to get the field offset out
        // of the type metadata at instantiation time.
        auto fieldOffset = emitPhysicalStructMemberOffsetOfFieldOffset(
                                                *this, loweredBaseTy, property);
        auto header = KeyPathComponentHeader::forStructComponentWithUnresolvedFieldOffset();
        fields.addInt32(header.getData());
        fields.add(llvm::ConstantExpr::getTruncOrBitCast(fieldOffset,
                                                         Int32Ty));
        break;
      }
      
      // For a class, we may know the fixed offset of a field at compile time,
      // or we may need to fetch it at instantiation time. Depending on the
      // ObjC-ness and resilience of the class hierarchy, there might be a few
      // different ways we need to go about this.
      if (loweredBaseTy.getClassOrBoundGenericClass()) {
        switch (getClassFieldAccess(*this, loweredBaseTy, property)) {
        case FieldAccess::ConstantDirect: {
          // Known constant fixed offset.
          auto offset = tryEmitConstantClassFragilePhysicalMemberOffset(*this,
                                                                  loweredBaseTy,
                                                                  property);
          assert(offset && "no constant offset for ConstantDirect field?!");
          addFixedOffset(/*struct*/ false, offset);
          break;
        }
        case FieldAccess::NonConstantDirect: {
          // A constant offset that's determined at class realization time.
          // We have to load the offset from a global ivar.
          auto header =
            KeyPathComponentHeader::forClassComponentWithUnresolvedIndirectOffset();
          fields.addInt32(header.getData());
          fields.addAlignmentPadding(getPointerAlignment());
          auto offsetVar = getAddrOfFieldOffset(property, /*indirect*/ false,
                                                NotForDefinition);
          fields.add(cast<llvm::Constant>(offsetVar.getAddress()));
          break;
        }
        case FieldAccess::ConstantIndirect: {
          // An offset that depends on the instance's generic parameterization,
          // but whose field offset is at a known vtable offset.
          auto header =
            KeyPathComponentHeader::forClassComponentWithUnresolvedFieldOffset();
          fields.addInt32(header.getData());
          auto fieldOffset =
            getClassFieldOffset(*this, loweredBaseTy.getClassOrBoundGenericClass(),
                                property);
          fields.addInt32(fieldOffset.getValue());
          break;
        }
        case FieldAccess::NonConstantIndirect:
          // An offset that depends on the instance's generic parameterization,
          // whose vtable offset is also unknown.
          // TODO: This doesn't happen until class resilience is enabled.
          llvm_unreachable("not implemented");
        }
        break;
      }
      llvm_unreachable("not struct or class");
    }
    case KeyPathPatternComponent::Kind::GettableProperty:
    case KeyPathPatternComponent::Kind::SettableProperty: {
      // Encode the settability.
      bool settable = kind == KeyPathPatternComponent::Kind::SettableProperty;
      KeyPathComponentHeader::ComputedPropertyKind componentKind;
      if (settable) {
        componentKind = component.isComputedSettablePropertyMutating()
          ? KeyPathComponentHeader::SettableMutating
          : KeyPathComponentHeader::SettableNonmutating;
      } else {
        componentKind = KeyPathComponentHeader::GetOnly;
      }
      
      // Lower the id reference.
      auto id = component.getComputedPropertyId();
      KeyPathComponentHeader::ComputedPropertyIDKind idKind;
      llvm::Constant *idValue;
      bool idResolved;
      switch (id.getKind()) {
      case KeyPathPatternComponent::ComputedPropertyId::Function:
        idKind = KeyPathComponentHeader::Pointer;
        idValue = getAddrOfSILFunction(id.getFunction(), NotForDefinition);
        idResolved = true;
        break;
      case KeyPathPatternComponent::ComputedPropertyId::DeclRef: {
        auto declRef = id.getDeclRef();
      
        // Foreign method refs identify using a selector
        // reference, which is doubly-indirected and filled in with a unique
        // pointer by dyld.
        if (declRef.isForeign) {
          assert(ObjCInterop && "foreign keypath component w/o objc interop?!");
          idKind = KeyPathComponentHeader::Pointer;
          idValue = getAddrOfObjCSelectorRef(declRef);
          idResolved = false;
        } else {
          idKind = KeyPathComponentHeader::VTableOffset;
          auto dc = declRef.getDecl()->getDeclContext();
          if (isa<ClassDecl>(dc)) {
            auto index = getVirtualMethodIndex(*this, declRef);
            idValue = llvm::ConstantInt::get(SizeTy, index);
            idResolved = true;
          } else if (auto methodProto = dyn_cast<ProtocolDecl>(dc)) {
            auto &protoInfo = getProtocolInfo(methodProto);
            auto index = protoInfo.getFunctionIndex(
                                 cast<AbstractFunctionDecl>(declRef.getDecl()));
            idValue = llvm::ConstantInt::get(SizeTy, -index.getValue());
            idResolved = true;
          } else {
            llvm_unreachable("neither a class nor protocol dynamic method?");
          }
        }
        break;
      }
      case KeyPathPatternComponent::ComputedPropertyId::Property:
        // Use the index of the stored property within the aggregate to key
        // the property.
        auto property = id.getProperty();
        idKind = KeyPathComponentHeader::StoredPropertyIndex;
        if (baseTy->getStructOrBoundGenericStruct()) {
          idResolved = true;
          idValue = llvm::ConstantInt::get(SizeTy,
            getPhysicalStructFieldIndex(*this,
                           SILType::getPrimitiveAddressType(baseTy), property));
        } else if (baseTy->getClassOrBoundGenericClass()) {
          // TODO: This field index would require runtime resolution with Swift
          // native class resilience. We never directly access ObjC-imported
          // ivars so we can disregard ObjC ivar resilience for this computation
          // and start counting at the Swift native root.
          switch (getClassFieldAccess(*this, loweredBaseTy, property)) {
          case FieldAccess::ConstantDirect:
          case FieldAccess::ConstantIndirect:
          case FieldAccess::NonConstantDirect:
            idResolved = true;
            idValue = llvm::ConstantInt::get(SizeTy,
              getClassFieldIndex(*this,
                           SILType::getPrimitiveAddressType(baseTy), property));
            break;
          case FieldAccess::NonConstantIndirect:
            llvm_unreachable("not implemented");
          }
          
        } else {
          llvm_unreachable("neither struct nor class");
        }
        break;
      }
      
      auto header = KeyPathComponentHeader::forComputedProperty(componentKind,
                                    idKind, !isInstantiableInPlace, idResolved);
      
      fields.addInt32(header.getData());
      fields.addAlignmentPadding(getPointerAlignment());
      fields.add(idValue);
      
      if (isInstantiableInPlace) {
        // No generic arguments or indexes, so we can invoke the
        // getter/setter as is.
        fields.add(getAddrOfSILFunction(component.getComputedPropertyGetter(),
                                        NotForDefinition));
        if (settable)
          fields.add(getAddrOfSILFunction(component.getComputedPropertySetter(),
                                          NotForDefinition));
      } else {
        // If there's generic context (TODO: or subscript indexes), embed as
        // arguments in the component. Thunk the SIL-level accessors to give the
        // runtime implementation a polymorphically-callable interface.
        
        // Push the accessors, possibly thunked to marshal generic environment.
        fields.add(getAccessorForComputedComponent(*this, component, Getter,
                                                     genericEnv, requirements));
        if (settable)
          fields.add(getAccessorForComputedComponent(*this, component, Setter,
                                                     genericEnv, requirements));
                                                     
        fields.add(getLayoutFunctionForComputedComponent(*this, component,
                                                     genericEnv, requirements));
                                                     
        // Set up a "witness table" for the component that handles copying,
        // destroying, equating, and hashing the captured contents of the
        // component.
        // If there are only generic parameters, we can use a prefab witness
        // table from the runtime.
        // TODO: For subscripts we'd generate functions that dispatch out to
        // the copy/destroy/equals/hash functionality of the subscript indexes.
        fields.add(getWitnessTableForComputedComponent(*this, component,
                                                     genericEnv, requirements));
        
        // Add an initializer function that copies generic arguments out of the
        // pattern argument buffer into the instantiated object.
        fields.add(getInitializerForComputedComponent(*this, component,
                                                     genericEnv, requirements));
      }
      break;
    }
    case KeyPathPatternComponent::Kind::OptionalChain:
      fields.addInt32(KeyPathComponentHeader::forOptionalChain().getData());
      break;
    case KeyPathPatternComponent::Kind::OptionalForce:
      fields.addInt32(KeyPathComponentHeader::forOptionalForce().getData());
      break;
    case KeyPathPatternComponent::Kind::OptionalWrap:
      fields.addInt32(KeyPathComponentHeader::forOptionalWrap().getData());
      break;
    }
    
    // For all but the last component, we pack in the type of the component.
    if (i + 1 != pattern->getComponents().size()) {
      fields.addAlignmentPadding(getPointerAlignment());
      fields.add(emitMetadataGenerator(component.getComponentType()));
    }
    baseTy = component.getComponentType();
  }
  
  // Save the total size of the buffer.
  Size componentSize = fields.getNextOffsetFromGlobal()
    - startOfKeyPathBuffer;
  
  // We now have enough info to build the header.
  KeyPathBufferHeader header(componentSize.getValue(), isInstantiableInPlace,
                             /*reference prefix*/ false);
  // Add the header, followed by the components.
  fields.fillPlaceholder(headerPlaceholder,
                         llvm::ConstantInt::get(Int32Ty, header.getData()));
  
  // Create the global variable.
  // TODO: The pattern could be immutable if
  // it isn't instantiable in place, and if we made the type metadata accessor
  // references private, it could go in true-const memory.
  auto patternVar = fields.finishAndCreateGlobal("keypath",
                                          getPointerAlignment(),
                                          /*constant*/ false,
                                          llvm::GlobalVariable::PrivateLinkage);
  KeyPathPatterns.insert({pattern, patternVar});
  return patternVar;
}

