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

#include "ConstantBuilder.h"
#include "Explosion.h"
#include "GenClass.h"
#include "GenMeta.h"
#include "GenStruct.h"
#include "GenericRequirement.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "ProtocolInfo.h"
#include "StructLayout.h"
#include "llvm/ADT/SetVector.h"
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

using namespace swift;
using namespace irgen;

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
                                   baseTy->getLValueOrInOutObjectType());
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
        // No generic arguments, so we can invoke the getter/setter as is.
        fields.add(getAddrOfSILFunction(component.getComputedPropertyGetter(),
                                        NotForDefinition));
        if (settable)
          fields.add(getAddrOfSILFunction(component.getComputedPropertySetter(),
                                          NotForDefinition));
      } else {
        // If there's generic context (TODO: or subscript indexes), embed as
        // arguments in the component. Thunk the SIL-level accessors to give the
        // runtime implementation a polymorphically-callable interface.
        Context.Diags.diagnose(diagLoc.getSourceLoc(),
                               diag::not_implemented,
                               "generic computed key paths");
        return llvm::UndefValue::get(Int8PtrTy);
      }
    }
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

