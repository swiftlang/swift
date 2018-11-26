// RUN: %target-typecheck-verify-swift

let nonBreakingSpace1 = 3 // expected-warning {{non-breaking space (U+00A0) used instead of regular space}} {{22-24= }}

let nonBreakingSpace2 = 3 // expected-warning {{non-breaking space (U+00A0) used instead of regular space}} {{24-26= }}

