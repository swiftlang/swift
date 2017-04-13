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

  // Check for type parameterization of any of the components.
  if (pattern->getNumOperands() > 0
      || pattern->getGenericSignature()) {
    // TODO: Parameterized key paths, such as for key paths in generic contexts
    // or with subscript components, require instantiation. Key paths with
    // resilient index components may need instantiation to adjust component
    // sizes as well.
    Context.Diags.diagnose(diagLoc.getSourceLoc(),
                           diag::not_implemented,
                           "dependent key path");
    return llvm::UndefValue::get(Int8PtrTy);
  }

  /// Generate a metadata accessor that produces metadata for the given type
  /// using arguments from the generic context of the key path.
  auto emitMetadataGenerator = [&](CanType type) -> llvm::Function * {
    if (!pattern->getGenericSignature())
      // We can just use the regular metadata accessor.
      // TODO: Make a local copy of public symbols we can relative-reference?
      return getAddrOfTypeMetadataAccessFunction(type, NotForDefinition);
    llvm_unreachable("not implemented");
  };
  
  // Start building the key path pattern.
  ConstantInitBuilder builder(*this);
  ConstantStructBuilder fields = builder.beginStruct();
  fields.setPacked(true);
  // Add a zero-initialized header we can use for lazy initialization.
  fields.add(llvm::ConstantInt::get(SizeTy, 0));
  
  // Store references to metadata generator functions to generate the metadata
  // for the root and leaf. These sit in the "isa" and object header parts of
  // the final object.
  fields.add(emitMetadataGenerator(rootTy));
  fields.add(emitMetadataGenerator(valueTy));
  
  // Leave a placeholder for the buffer header, since we need to know the full
  // buffer size to fill it in.
  auto headerPlaceholder = fields.addPlaceholderWithSize(Int32Ty);
  
  auto startOfKeyPathBuffer = fields.getNextOffsetFromGlobal();
  
  // Build out the components.
  bool isInstantiableInPlace = true;
  
  auto baseTy = rootTy;
  
  for (unsigned i : indices(pattern->getComponents())) {
    auto loweredBaseTy = getLoweredType(AbstractionPattern::getOpaque(),
                                        baseTy->getLValueOrInOutObjectType());
    auto &component = pattern->getComponents()[i];
    switch (component.getKind()) {
    case KeyPathPatternComponent::Kind::StoredProperty: {
      // Try to get a constant offset if we can.
      auto property = cast<VarDecl>(component.getStoredPropertyDecl());
      llvm::Constant *offset;
      bool isStruct;
      if (auto structTy = loweredBaseTy.getStructOrBoundGenericStruct()) {
        offset = emitPhysicalStructMemberFixedOffset(*this,
                                                     loweredBaseTy,
                                                     property);
        isStruct = true;
      } else if (auto classTy = loweredBaseTy.getClassOrBoundGenericClass()) {
        offset = tryEmitConstantClassFragilePhysicalMemberOffset(*this,
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
        
        offset = llvm::ConstantExpr::getTruncOrBitCast(offset, Int32Ty);
        fields.add(offset);
      } else {
        // Otherwise, stash a relative reference to the property name as a
        // string, which we'll resolve the offset for at runtime.
        auto header = isStruct
          ? KeyPathComponentHeader::forStructComponentWithUnresolvedOffset()
          : KeyPathComponentHeader::forClassComponentWithUnresolvedOffset();
        fields.addInt32(header.getData());

        auto name = getAddrOfGlobalString(property->getName().str(),
                                          /*relativelyAddressed*/ true);
        fields.addRelativeAddress(name);
      }
      break;
    }
    case KeyPathPatternComponent::Kind::GettableProperty:
    case KeyPathPatternComponent::Kind::SettableProperty: {
      Context.Diags.diagnose(diagLoc.getSourceLoc(),
                             diag::not_implemented,
                             "computed key path");
      return llvm::UndefValue::get(Int8PtrTy);
    }
    }
    
    // For all but the last component, we pack in the type of the component.
    if (i + 1 != pattern->getComponents().size()) {
      fields.add(emitMetadataGenerator(component.getComponentType()));
    }
    baseTy = component.getComponentType();
  }
  
  // Save the total size of the buffer, minus three words for the once token
  // and object header, and 32 bits for the buffer header.
  Size componentSize = fields.getNextOffsetFromGlobal()
    - startOfKeyPathBuffer;
  
  // We now have enough info to build the header.
  KeyPathBufferHeader header(componentSize.getValue(), isInstantiableInPlace,
                             /*reference prefix*/ false);
  // Add the header, followed by the components.
  fields.fillPlaceholder(headerPlaceholder,
                         llvm::ConstantInt::get(Int32Ty, header.getData()));
  
  // Create the global variable.
  // TODO: Coalesce equivalent patterns. The pattern could be immutable if
  // it isn't instantiable in place, and if we made the type metadata accessor
  // references private, it could go in true-const memory.
  auto patternVar = fields.finishAndCreateGlobal("keypath",
                                          getPointerAlignment(),
                                          /*constant*/ false,
                                          llvm::GlobalVariable::PrivateLinkage);
  KeyPathPatterns.insert({pattern, patternVar});
  return patternVar;
}

