import A
import B.B3
import D

import NotSoSecret // expected-no-warning
@_implementationOnly import NotSoSecret2 // expected-no-note
