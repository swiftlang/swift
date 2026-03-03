// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=off \
// RUN:   -disable-availability-checking \
// RUN:   -I %S%{fs-sep}Inputs \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}return-foreign-reference.h

import ReturnForeignReference

let _: FRT = getFRTNoAnnotations() // expected-warning {{cannot infer ownership}}
let _: FRT = getFRTRetained()
let _: FRT = getFRTUnretained()


// These calls to incorrectly annotated functions are necessary to force
// diagnostics to appear in the header file.
let _: FRT = getFRTConflictingAnnotations()
let _ = getValueRetained()
let _ = getValueUnretained()
