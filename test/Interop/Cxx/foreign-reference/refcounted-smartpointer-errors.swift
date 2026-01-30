// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -disable-availability-checking -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %S%{fs-sep}Inputs -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}refcounted-smartptrs.h
import RefCountedSmartPtrs

func triggerWarnings(_ a: errors.MissingToRawPtr,               // @expected-error {{'MissingToRawPtr' is not a member type of enum '__ObjC.errors'}}
                     _ b: errors.MissingConversionFunction,     // @expected-error {{'MissingConversionFunction' is not a member type of enum '__ObjC.errors'}}
                     _ c: errors.MultipleConversionFunctions,   // @expected-error {{'MultipleConversionFunctions' is not a member type of enum '__ObjC.errors'}}
                     _ d: errors.WrongConversionSignature,      // @expected-error {{'WrongConversionSignature' is not a member type of enum '__ObjC.errors'}}
                     _ e: errors.PrivateConversionFunction,     // @expected-error {{'PrivateConversionFunction' is not a member type of enum '__ObjC.errors'}}
                     _ f: errors.NoSuitableCtor,                // @expected-error {{'NoSuitableCtor' is not a member type of enum '__ObjC.errors'}}
                     _ g: errors.AmbiguousCtors,                // @expected-error {{'AmbiguousCtors' is not a member type of enum '__ObjC.errors'}}
                     _ h: errors.MismatchingPointerTypes) {     // @expected-error {{'MismatchingPointerTypes' is not a member type of enum '__ObjC.errors'}}
}
