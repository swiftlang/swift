// {"kind":"typecheck","signature":"swift::constraints::GenericArgumentsMismatchFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (!(purpose == CTP_Unused || purpose == CTP_CannotFail)), function diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b{
         wrappedValue: b
       }
        @propertyWrapper struct c<b{
         wrappedValue: b
       }
           {
               @c @a var value = if true
                 let : c<a<Bool>> = _value
