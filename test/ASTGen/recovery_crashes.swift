// Malformed input that used to crash the compiler under ParserASTGen. These
// only check that compilation fails with diagnostics rather than crashing.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/diagnose_unknown_group.swift \
// RUN:   -enable-experimental-feature SourceWarningControl \
// RUN:   -enable-experimental-feature ParserASTGen 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=GROUP --implicit-check-not="Stack dump"
// GROUP: the diagnostic group identifier 'Hmm' is unknown

// RUN: not %target-swift-frontend -typecheck %t/default_stray_attribute.swift \
// RUN:   -enable-experimental-feature ParserASTGen 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=DEFAULT --implicit-check-not="Stack dump"
// DEFAULT: error:

// RUN: not %target-swift-frontend -typecheck %t/nested_private_extension.swift \
// RUN:   -enable-experimental-feature ParserASTGen 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=EXT --implicit-check-not="Assertion"
// EXT: declaration is only valid at file scope

//--- diagnose_unknown_group.swift
@diagnose(Hmm, as: warning)
func f() {}

//--- default_stray_attribute.swift
switch 5 {
case 5: print("5");
default @MainActor
}

//--- nested_private_extension.swift
func g() {
  repeat { _ = 1

private extension String {}
