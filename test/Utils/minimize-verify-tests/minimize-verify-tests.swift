// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// --- basic-merge: Two prefixes merge to default expected-error -----------
// RUN: %minimize-verify-tests %t/basic-merge.swift
// RUN: %diff %t/basic-merge.swift %t/basic-merge.swift.expected

// --- no-merge-subset: No single prefix covers the subset ----------------
// RUN: %minimize-verify-tests %t/no-merge-subset.swift
// RUN: %diff %t/no-merge-subset.swift %t/no-merge-subset.swift.expected

// --- merge-to-additional-prefix: Merge to a shared non-default prefix ---
// RUN: %minimize-verify-tests %t/merge-to-additional-prefix.swift
// RUN: %diff %t/merge-to-additional-prefix.swift %t/merge-to-additional-prefix.swift.expected

// --- multiple-groups: One group mergeable, another not ------------------
// RUN: %minimize-verify-tests %t/multiple-groups.swift
// RUN: %diff %t/multiple-groups.swift %t/multiple-groups.swift.expected

// --- already-minimal: Nothing to change ---------------------------------
// RUN: %minimize-verify-tests %t/already-minimal.swift
// RUN: %diff %t/already-minimal.swift %t/already-minimal.swift.expected

// --- separate-lines: Offset auto-adjusts after line removal -------------
// RUN: %minimize-verify-tests %t/separate-lines.swift
// RUN: %diff %t/separate-lines.swift %t/separate-lines.swift.expected

// --- inline-diag: Inline directive preferred over @-N one ---------------
// RUN: %minimize-verify-tests %t/inline-diag.swift
// RUN: %diff %t/inline-diag.swift %t/inline-diag.swift.expected

// --- run-line-continuation: Backslash-continued RUN lines ---------------
// RUN: %minimize-verify-tests %t/run-line-continuation.swift
// RUN: %diff %t/run-line-continuation.swift %t/run-line-continuation.swift.expected

// --- expansion: Prefixed expansion blocks merge -------------------------
// RUN: %minimize-verify-tests %t/expansion.swift
// RUN: %diff %t/expansion.swift %t/expansion.swift.expected

// --- expansion-already-shared: Non-prefixed expansion block still has its contents merged
// RUN: %minimize-verify-tests %t/expansion-already-shared.swift
// RUN: %diff %t/expansion-already-shared.swift %t/expansion-already-shared.swift.expected

//--- basic-merge.swift
// RUN: true -verify -verify-additional-prefix swift5-
// RUN: true -verify -verify-additional-prefix swift6-

func basic_merge() {
  let x = 1
  // expected-swift5-error @-1 {{cannot find 'x' in scope}}
  // expected-swift6-error @-2 {{cannot find 'x' in scope}}
}
//--- basic-merge.swift.expected
// RUN: true -verify -verify-additional-prefix swift5-
// RUN: true -verify -verify-additional-prefix swift6-

func basic_merge() {
  let x = 1
  // expected-error @-1 {{cannot find 'x' in scope}}
}
//--- no-merge-subset.swift
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b-
// RUN: true -verify -verify-additional-prefix c-

func no_merge_subset() {
  let x = 1
  // expected-a-error @-1 {{cannot find 'x' in scope}}
  // expected-b-error @-2 {{cannot find 'x' in scope}}
}
//--- no-merge-subset.swift.expected
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b-
// RUN: true -verify -verify-additional-prefix c-

func no_merge_subset() {
  let x = 1
  // expected-a-error @-1 {{cannot find 'x' in scope}}
  // expected-b-error @-2 {{cannot find 'x' in scope}}
}
//--- merge-to-additional-prefix.swift
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b- -verify-additional-prefix shared-
// RUN: true -verify -verify-additional-prefix c- -verify-additional-prefix shared-

func merge_to_additional_prefix() {
  let x = 1
  // expected-b-error @-1 {{cannot find 'x' in scope}}
  // expected-c-error @-2 {{cannot find 'x' in scope}}
}
//--- merge-to-additional-prefix.swift.expected
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b- -verify-additional-prefix shared-
// RUN: true -verify -verify-additional-prefix c- -verify-additional-prefix shared-

func merge_to_additional_prefix() {
  let x = 1
  // expected-shared-error @-1 {{cannot find 'x' in scope}}
}
//--- multiple-groups.swift
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b-
// RUN: true -verify -verify-additional-prefix c-

func first() {
  let x = 1
  // expected-a-error @-1 {{first error}}
  // expected-b-error @-2 {{first error}}
  // expected-c-error @-3 {{first error}}
}

func second() {
  let y = 2
  // expected-a-warning @-1 {{second warning}}
  // expected-b-warning @-2 {{second warning}}
}
//--- multiple-groups.swift.expected
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b-
// RUN: true -verify -verify-additional-prefix c-

func first() {
  let x = 1
  // expected-error @-1 {{first error}}
}

func second() {
  let y = 2
  // expected-a-warning @-1 {{second warning}}
  // expected-b-warning @-2 {{second warning}}
}
//--- already-minimal.swift
// RUN: true -verify -verify-additional-prefix swift5-
// RUN: true -verify -verify-additional-prefix swift6-

func already_minimal() {
  let x = 1 // expected-error {{already minimal}}
}
//--- already-minimal.swift.expected
// RUN: true -verify -verify-additional-prefix swift5-
// RUN: true -verify -verify-additional-prefix swift6-

func already_minimal() {
  let x = 1 // expected-error {{already minimal}}
}
//--- separate-lines.swift
// RUN: true -verify -verify-additional-prefix swift5-
// RUN: true -verify -verify-additional-prefix swift6-

func separate_lines() {
  let x = 1
  let y = 2
  // expected-swift5-error @-2 {{error on x}}
  // expected-swift6-error @-3 {{error on x}}
  // expected-swift5-warning @-3 {{warning on y}}
  // expected-swift6-warning @-4 {{warning on y}}
}
//--- separate-lines.swift.expected
// RUN: true -verify -verify-additional-prefix swift5-
// RUN: true -verify -verify-additional-prefix swift6-

func separate_lines() {
  let x = 1
  let y = 2
  // expected-error @-2 {{error on x}}
  // expected-warning @-2 {{warning on y}}
}
//--- inline-diag.swift
// RUN: true -verify -verify-additional-prefix swift5-
// RUN: true -verify -verify-additional-prefix swift6-

func inline_diag() {
  let x = 1 // expected-swift5-error {{inline error}}
  // expected-swift6-error @-1 {{inline error}}
}
//--- inline-diag.swift.expected
// RUN: true -verify -verify-additional-prefix swift5-
// RUN: true -verify -verify-additional-prefix swift6-

func inline_diag() {
  let x = 1 // expected-error {{inline error}}
}
//--- run-line-continuation.swift
// RUN: true -verify \
// RUN:   -verify-additional-prefix swift5-
// RUN: true -verify \
// RUN:   -verify-additional-prefix swift6-

func run_line_continuation() {
  let x = 1
  // expected-swift5-error @-1 {{error msg}}
  // expected-swift6-error @-2 {{error msg}}
}
//--- run-line-continuation.swift.expected
// RUN: true -verify \
// RUN:   -verify-additional-prefix swift5-
// RUN: true -verify \
// RUN:   -verify-additional-prefix swift6-

func run_line_continuation() {
  let x = 1
  // expected-error @-1 {{error msg}}
}
//--- expansion.swift
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b-

@freestanding(expression)
macro myMacro() = #externalMacro(module: "M", type: "T")
let x = #myMacro
// expected-a-expansion@-1:9{{
//   expected-a-warning@1 {{shared warning}}
//   expected-a-warning@2 {{a-only warning}}
// }}
// expected-b-expansion@-5:9{{
//   expected-b-warning@1 {{shared warning}}
// }}
// expected-a-error @-7 {{some error}}
// expected-b-error @-8 {{some error}}
//--- expansion.swift.expected
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b-

@freestanding(expression)
macro myMacro() = #externalMacro(module: "M", type: "T")
let x = #myMacro
// expected-expansion@-1:9{{
//   expected-warning@1 {{shared warning}}
//   expected-a-warning@2 {{a-only warning}}
// }}
// expected-error @-4 {{some error}}
//--- expansion-already-shared.swift
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b-

@freestanding(expression)
macro myMacro() = #externalMacro(module: "M", type: "T")
let x = #myMacro
// expected-expansion@-1:9{{
//   expected-a-warning@1 {{shared warning}}
//   expected-a-warning@2 {{a-only warning}}
//   expected-b-warning@1 {{shared warning}}
// }}
// expected-a-error @-6 {{some error}}
// expected-b-error @-7 {{some error}}
//--- expansion-already-shared.swift.expected
// RUN: true -verify -verify-additional-prefix a-
// RUN: true -verify -verify-additional-prefix b-

@freestanding(expression)
macro myMacro() = #externalMacro(module: "M", type: "T")
let x = #myMacro
// expected-expansion@-1:9{{
//   expected-warning@1 {{shared warning}}
//   expected-a-warning@2 {{a-only warning}}
// }}
// expected-error @-5 {{some error}}
