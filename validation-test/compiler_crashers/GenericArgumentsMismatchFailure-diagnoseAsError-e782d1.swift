// {"kind":"typecheck","signature":"swift::constraints::GenericArgumentsMismatchFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (purpose != CTP_Unused), function diagnoseAsError","signatureNext":"GenericArgumentsMismatch::coalesceAndDiagnose"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b{
         wrappedValue: b
       }
        @propertyWrapper struct e<b{
         wrappedValue: b
       }
           {
               @e @a var value = if true
                 let : e<a<Bool>> = _value
