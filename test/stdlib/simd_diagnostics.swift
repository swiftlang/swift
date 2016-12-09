// RUN: %target-swift-frontend -typecheck -verify %s

// FIXME: No simd module on linux rdar://problem/20795411
// XFAIL: linux

import simd

let a = int4(0) + int4(0) // expected-error{{'+' has been renamed to '&+': integer vector types do not support checked arithmetic; use the wrapping operations instead}} {{17-18=&+}}
let b = int4(0) - int4(0) // expected-error{{'-' has been renamed to '&-': integer vector types do not support checked arithmetic; use the wrapping operations instead}} {{17-18=&-}}
let c = int4(0) * int4(0) // expected-error{{'*' has been renamed to '&*': integer vector types do not support checked arithmetic; use the wrapping operations instead}} {{17-18=&*}}
let x = int4(0) * (0 as Int32) // expected-error{{'*' has been renamed to '&*': integer vector types do not support checked arithmetic; use the wrapping operations instead}} {{17-18=&*}}
let y = (0 as Int32) * int4(0) // expected-error{{'*' has been renamed to '&*': integer vector types do not support checked arithmetic; use the wrapping operations instead}} {{22-23=&*}}

var d = int4(0)
d += int4(0) // expected-error{{'+=' is unavailable: integer vector types do not support checked arithmetic; use the wrapping operation 'x = x &+ y' instead}}
d -= int4(0) // expected-error{{'-=' is unavailable: integer vector types do not support checked arithmetic; use the wrapping operation 'x = x &- y' instead}}
d *= int4(0) // expected-error{{'*=' is unavailable: integer vector types do not support checked arithmetic; use the wrapping operation 'x = x &* y' instead}}
d *= 0 // expected-error{{'*=' is unavailable: integer vector types do not support checked arithmetic; use the wrapping operation 'x = x &* y' instead}}

