// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -o %t -module-name=library %S/Inputs/complete_import_overloads.swift

// RUN: %target-swift-ide-test -code-completion -swift-version 4 -source-filename %s -code-completion-token=SELF_DOT_1 -I %t | %FileCheck %s -check-prefix=SWIFT4_SELF_DOT_1
// RUN: %target-swift-ide-test -code-completion -swift-version 4 -source-filename %s -code-completion-token=SELF_DOT_2 -I %t | %FileCheck %s -check-prefix=SWIFT4_SELF_DOT_2

// RUN: %target-swift-ide-test -code-completion -swift-version 5 -source-filename %s -code-completion-token=SELF_DOT_1 -I %t | %FileCheck %s -check-prefix=SWIFT5_SELF_DOT_1
// RUN: %target-swift-ide-test -code-completion -swift-version 5 -source-filename %s -code-completion-token=SELF_DOT_2 -I %t | %FileCheck %s -check-prefix=SWIFT5_SELF_DOT_2

import library

// Ensure we maintain compatibility with Swift 4's overload signature rules.
// Variables defined in extensions of generic types had different overload
// signatures to other variables, so allow overloading in such cases (SR-7341).
extension HasFooGeneric {
  var foo: String { return "" } // foo isn't defined in a generic extension in the other module, so allow overloading in Swift 4 mode.
  var bar: String { return "" } // bar is defined in a generic extension in the other module, so `bar: String` always shadows it.
  func baz() {
    self.#^SELF_DOT_1^#
  }
}

// SWIFT4_SELF_DOT_1: Begin completions
// SWIFT4_SELF_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:      foo[#Int#]; name=foo
// SWIFT4_SELF_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:      foo[#String#]; name=foo
// SWIFT4_SELF_DOT_1-NOT: Decl[InstanceVar]/CurrNominal:      bar[#Int#]; name=bar
// SWIFT4_SELF_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:      bar[#String#]; name=bar
// SWIFT4_SELF_DOT_1: End completions

// But in Swift 5 mode, properties from this module currently always shadow
// properties from the other module – therefore meaning that the properties from
// the other module never show up in the overload set.
// FIX-ME: It seems reasonable for both to show up in the overload set.
// SWIFT5_SELF_DOT_1: Begin completions
// SWIFT5_SELF_DOT_1-NOT: Decl[InstanceVar]/CurrNominal:      foo[#Int#]; name=foo
// SWIFT5_SELF_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:      foo[#String#]; name=foo
// SWIFT5_SELF_DOT_1-NOT: Decl[InstanceVar]/CurrNominal:      bar[#Int#]; name=bar
// SWIFT5_SELF_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:      bar[#String#]; name=bar
// SWIFT5_SELF_DOT_1: End completions

// For non-generic types, the variable overload signature was always the
// null type, so `foo/bar: String` shadows `foo/bar: Int`.
extension HasFooNonGeneric {
  var foo: String { return "" }
  var bar: String { return "" }
  func baz() {
    self.#^SELF_DOT_2^#
  }
}

// SWIFT4_SELF_DOT_2: Begin completions
// SWIFT4_SELF_DOT_2-NOT: Decl[InstanceVar]/CurrNominal:      foo[#Int#]; name=foo
// SWIFT4_SELF_DOT_2-DAG: Decl[InstanceVar]/CurrNominal:      foo[#String#]; name=foo
// SWIFT4_SELF_DOT_2-NOT: Decl[InstanceVar]/CurrNominal:      bar[#Int#]; name=bar
// SWIFT4_SELF_DOT_2-DAG: Decl[InstanceVar]/CurrNominal:      bar[#String#]; name=bar
// SWIFT4_SELF_DOT_2: End completions

// Again, in Swift 5 mode, we currently consistently shadow the properties from
// the other module.
// FIX-ME: It seems reasonable to not shadow them.
// SWIFT5_SELF_DOT_2: Begin completions
// SWIFT5_SELF_DOT_2-NOT: Decl[InstanceVar]/CurrNominal:      foo[#Int#]; name=foo
// SWIFT5_SELF_DOT_2-DAG: Decl[InstanceVar]/CurrNominal:      foo[#String#]; name=foo
// SWIFT5_SELF_DOT_2-NOT: Decl[InstanceVar]/CurrNominal:      bar[#Int#]; name=bar
// SWIFT5_SELF_DOT_2-DAG: Decl[InstanceVar]/CurrNominal:      bar[#String#]; name=bar
// SWIFT5_SELF_DOT_2: End completions
