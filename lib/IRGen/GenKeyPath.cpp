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
#include "GenClass.h"
#include "GenStruct.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "llvm/ADT/SetVector.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/ABI/KeyPath.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsIRGen.h"
#include "swift/AST/Types.h"

using namespace swift;
using namespace irgen;

llvm::Value *IRGenFunction::emitKeyPath(KeyPathInst *inst) {
  // TODO: Parameterized key paths, such as for key paths in generic contexts
  // or with subscript components, require instantiation. Key paths with
  // resilient index components may need instantiation to adjust component
  // sizes as well.
  
  llvm::SetVector<ArchetypeType *> TypeArgs;
  
  /// Find archetypes inside a type that need to be passed to the key path
  /// pattern for instantiation.
  auto findTypeArgs = [&](CanType type) {
    type.findIf([&](Type t) -> bool {
      if (auto archetype = t->getAs<ArchetypeType>()) {
        IGM.Context.Diags.diagnose(inst->getLoc().getSourceLoc(),
                                   diag::not_implemented,
                                   "dependent generic key path");
        TypeArgs.insert(archetype);
      }
      return false;
    });
  };
  // Gather type arguments from the root and leaf types of the key path.
  auto keyPathTy = inst->getType().castTo<BoundGenericType>();
  auto keyPathLoweredTy = IGM.getStorageType(inst->getType());
  auto rootTy = keyPathTy->getGenericArgs()[0]->getCanonicalType();
  auto valueTy = keyPathTy->getGenericArgs()[1]->getCanonicalType();
  
  findTypeArgs(rootTy);
  findTypeArgs(valueTy);
  
  // Check for type parameterization of any of the components.
  for (auto component : inst->getComponents()) {
    findTypeArgs(component.getComponentType());
  }
  
  if (!TypeArgs.empty())
    // TODO: implement
    return llvm::UndefValue::get(keyPathLoweredTy);

  /// Generate a metadata accessor that produces metadata for the given type
  /// using arguments from the generic context of the key path.
  auto emitMetadataGenerator = [&](CanType type) -> llvm::Function * {
    if (TypeArgs.empty())
      // We can just use the regular metadata accessor.
      // TODO: Make a local copy of public symbols we can relative-reference?
      return IGM.getAddrOfTypeMetadataAccessFunction(type, NotForDefinition);
    llvm_unreachable("not implemented");
  };
  
  // Start building the key path pattern.
  // TODO: Coalesce equivalent key path patterns
  ConstantInitBuilder builder(IGM);
  ConstantStructBuilder fields = builder.beginStruct();
  fields.setPacked(true);
  // Add a version header in case we change the key path format later.
  fields.add(llvm::ConstantInt::get(IGM.SizeTy, 1));
  
  // Store references to metadata generator functions to generate the metadata
  // for the root and leaf. These sit in the "isa" and object header parts of
  // the final object.
  fields.add(emitMetadataGenerator(rootTy));
  fields.add(emitMetadataGenerator(valueTy));
  
  // Leave a placeholder for the buffer header, since we need to know the full
  // buffer size to fill it in.
  auto headerPlaceholder = fields.addPlaceholderWithSize(IGM.Int32Ty);
  
  // Build out the components.
  bool isInstantiableInPlace = true;
  fields.setPacked(true);
  
  auto baseTy = rootTy;
  
  for (unsigned i : indices(inst->getComponents())) {
    auto loweredBaseTy = IGM.getLoweredType(AbstractionPattern::getOpaque(),
                                          baseTy->getLValueOrInOutObjectType());
    auto &component = inst->getComponents()[i];
    switch (component.getKind()) {
    case KeyPathInstComponent::Kind::StoredProperty: {
      // Try to get a constant offset if we can.
      auto property = cast<VarDecl>(component.getValueDecl());
      llvm::Constant *offset;
      bool isStruct;
      if (auto structTy = loweredBaseTy.getStructOrBoundGenericStruct()) {
        offset = emitPhysicalStructMemberFixedOffset(IGM,
                                                     loweredBaseTy,
                                                     property);
        isStruct = true;
      } else if (auto classTy = loweredBaseTy.getClassOrBoundGenericClass()) {
        offset = tryEmitConstantClassFragilePhysicalMemberOffset(IGM,
                                                                 loweredBaseTy,
                                                                 property);
        isStruct = false;
      } else {
        llvm_unreachable("property of non-struct, non-class?!");
      }
      
      // If the projection is a statically known integer, try to pack it into
      // the key path payload.
      if (auto offsetInt = dyn_cast<llvm::ConstantInt>(offset)) {
        auto offsetValue = offsetInt->getValue().getZExtValue();
        if (KeyPathComponentHeader::offsetCanBeInline(offsetValue)) {
          auto header = isStruct
            ? KeyPathComponentHeader::forStructComponentWithInlineOffset(offsetValue)
            : KeyPathComponentHeader::forClassComponentWithInlineOffset(offsetValue);
          fields.addInt32(header.getData());
          break;
        }
      }
      
      
      // Add the resolved offset if we have one.
      if (offset) {
        auto header = isStruct
          ? KeyPathComponentHeader::forStructComponentWithOutOfLineOffset()
          : KeyPathComponentHeader::forClassComponentWithOutOfLineOffset();
        fields.addInt32(header.getData());
        
        offset = llvm::ConstantExpr::getTruncOrBitCast(offset, IGM.Int32Ty);
        fields.add(offset);
      } else {
        // Otherwise, stash a relative reference to the property name as a
        // string, which we'll resolve the offset for at runtime.
        auto header = isStruct
          ? KeyPathComponentHeader::forStructComponentWithUnresolvedOffset()
          : KeyPathComponentHeader::forClassComponentWithUnresolvedOffset();
        fields.addInt32(header.getData());

        auto name = IGM.getAddrOfGlobalString(property->getName().str(),
                                              /*relativelyAddressed*/ true);
        fields.addRelativeAddress(name);
      }
      break;
    }
    }
    
    // For all but the last component, we pack in the type of the component.
    if (i + 1 != inst->getComponents().size()) {
      fields.add(emitMetadataGenerator(component.getComponentType()));
    }
    baseTy = component.getComponentType();
  }
  
  // Save the total size of the buffer, minus a word for the version header.
  Size componentSize = fields.getNextOffsetFromGlobal() - IGM.getPointerSize();
  
  // We now have enough info to build the header.
  KeyPathBufferHeader header(componentSize.getValue(), isInstantiableInPlace,
                             /*reference prefix*/ false);
  // Add the header, followed by the components.
  fields.fillPlaceholder(headerPlaceholder,
                         llvm::ConstantInt::get(IGM.Int32Ty, header.getData()));
  
  // Create the global variable.
  // TODO: Coalesce equivalent patterns. The pattern could be immutable if
  // it isn't instantiable in place, and if we made the type metadata accessor
  // references private, it could go in true-const memory.
  auto pattern = fields.finishAndCreateGlobal("keypath",
                                          IGM.getPointerAlignment(),
                                          /*constant*/ false,
                                          llvm::GlobalVariable::PrivateLinkage);
  
  // Build up the argument vector to instantiate the pattern here.
  llvm::Value *args;
  if (!TypeArgs.empty()) {
    llvm_unreachable("todo!");
  } else {
    args = llvm::UndefValue::get(IGM.Int8PtrTy);
  }
  auto patternPtr = llvm::ConstantExpr::getBitCast(pattern, IGM.Int8PtrTy);
  auto call = Builder.CreateCall(IGM.getGetKeyPathFn(), {patternPtr, args});
  call->setDoesNotThrow();
  return call;
}

