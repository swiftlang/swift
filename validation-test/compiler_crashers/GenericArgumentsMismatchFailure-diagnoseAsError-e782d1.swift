// {"kind":"typecheck","signature":"swift::constraints::GenericArgumentsMismatchFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (purpose != CTP_Unused), function diagnoseAsError","signatureNext":"GenericArgumentsMismatch::coalesceAndDiagnose"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
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
