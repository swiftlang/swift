@_implementationOnly import NotSoSecret
import NotSoSecret2 // expected-no-warning
@_implementationOnly import NotSoSecret3 // expected-no-note

@_implementationOnly import ActuallySecret // no-warning
import ActuallyOkay // no-warning
