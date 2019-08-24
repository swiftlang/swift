import A
import B.B3
import D

import NotSoSecret // expected-warning {{'NotSoSecret' inconsistently imported as implementation-only}}
@_implementationOnly import NotSoSecret2 // expected-note {{imported as implementation-only here}}
