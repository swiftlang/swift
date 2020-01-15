import NotSoSecret // expected-no-warning
@_implementationOnly import NotSoSecret2 // expected-no-note
import NotSoSecret3 // expected-no-warning

@_implementationOnly import ActuallySecret // no-warning
import ActuallyOkay // no-warning

@_implementationOnly import Contradictory // expected-no-note
