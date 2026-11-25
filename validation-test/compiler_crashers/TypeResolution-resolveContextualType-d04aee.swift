// {"kind":"typecheck","signature":"swift::TypeResolution::resolveContextualType(swift::TypeRepr*, swift::DeclContext*, swift::GenericSignature, swift::TypeResolutionOptions, llvm::function_ref<swift::Type (swift::UnboundGenericType*)>, llvm::function_ref<swift::Type (swift::ASTContext&, swift::PlaceholderTypeRepr*)>, llvm::function_ref<swift::Type (swift::Type, swift::PackElementTypeRepr*)>, llvm::function_ref<void (swift::GenericTypeDecl*, llvm::function_ref<swift::Type (swift::SubstitutableType*)>)>, swift::SILTypeResolutionContext*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"validateTypedPattern"}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck -I %t/Inputs %t/main.swift
// REQUIRES: objc_interop

//--- Inputs/module.modulemap
module ObjCLib {
  header "ObjCLib.h"
}

//--- Inputs/ObjCLib.h
__attribute__((objc_root_class))
@interface Base
@end

@interface Box<T> : Base
typedef void (^Callback)(T);
@end

//--- main.swift
import ObjCLib
var x: Callback
