// RUN: not %target-swiftc_driver %s 2>&1 | %FileCheck -check-prefix=DEFAULT %s
// RUN: not %target-swiftc_driver -warnings-as-errors %s 2>&1 | %FileCheck -check-prefix=WERR %s
// RUN: not %target-swiftc_driver -suppress-warnings %s 2>&1 | %FileCheck -check-prefix=NOWARN %s

// RUN: not %target-swiftc_driver -suppress-warnings -warnings-as-errors %s 2>&1 | %FileCheck -check-prefix=FLAGS_CONFLICT_WAE %s
// FLAGS_CONFLICT_WAE: error: conflicting options '-warnings-as-errors' and '-suppress-warnings'

// RUN: not %target-swiftc_driver -suppress-warnings -Wwarning test %s 2>&1 | %FileCheck -check-prefix=FLAGS_CONFLICT_WW %s
// FLAGS_CONFLICT_WW: error: conflicting options '-Wwarning' and '-suppress-warnings'

// RUN: not %target-swiftc_driver -suppress-warnings -Werror test  %s 2>&1 | %FileCheck -check-prefix=FLAGS_CONFLICT_WE %s
// FLAGS_CONFLICT_WE: error: conflicting options '-Werror' and '-suppress-warnings'

func foo() -> Int {
	let x = 1
	var y = 2
// DEFAULT:    warning: variable 'y' was never mutated; consider changing to 'let' constant
// WERR:       error: variable 'y' was never mutated; consider changing to 'let' constant
// NOWARN-NOT: variable 'y' was never mutated
	return x + y
}

func bar() {
	foo()
// To help anchor the checks, have an error. Put it inside a later function, to help make sure it comes after
	xyz
// DEFAULT: error: cannot find 'xyz' in scope
// WERR:    error: cannot find 'xyz' in scope
// NOWARN:  error: cannot find 'xyz' in scope
}
