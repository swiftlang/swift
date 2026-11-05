// RUN: %target-typecheck-verify-swift

let nonBreakingSpace1 = 3 // expected-cparser-warning {{non-breaking space (U+00A0) used instead of regular space}} {{22-24= }} expected-astgen-warning {{non-breaking space (U+00A0) used instead of regular space}} {{5-24=nonBreakingSpace1 }}

let nonBreakingSpace2 = 3 // expected-cparser-warning {{non-breaking space (U+00A0) used instead of regular space}} {{24-26= }} expected-astgen-warning {{non-breaking space (U+00A0) used instead of regular space}} {{23-26== }}

func foo() {
    _ = 42 // expected-cparser-warning {{non-breaking space (U+00A0) used instead of regular space}} {{1-9=    }} expected-astgen-warning {{non-breaking space (U+00A0) used instead of regular space}} {{9-10=_}}
}
