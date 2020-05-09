import NotSoSecret // expected-warning {{'NotSoSecret' inconsistently imported as implementation-only}}
@_implementationOnly import NotSoSecret2 // expected-note 2 {{imported as implementation-only here}}
import NotSoSecret3 // expected-warning {{'NotSoSecret3' inconsistently imported as implementation-only}}

@_implementationOnly import ActuallySecret // no-warning
import ActuallyOkay // no-warning

@_implementationOnly import Contradictory // expected-note {{imported as implementation-only here}}
