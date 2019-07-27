@_implementationOnly import NotSoSecret
import NotSoSecret2 // expected-warning {{'NotSoSecret2' inconsistently imported as implementation-only}}
@_implementationOnly import NotSoSecret3 // expected-note 2 {{imported as implementation-only here}}

@_implementationOnly import ActuallySecret // no-warning
import ActuallyOkay // no-warning
