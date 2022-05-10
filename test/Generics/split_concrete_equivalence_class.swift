// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: split_concrete_equivalence_class.(file).f01@
// CHECK-NEXT: Generic signature: <C, R where C : Collection, R : Collection, C.[Collection]SubSequence == Substring, R.[Sequence]Element == Character>
func f01<C : Collection, R : Collection>(_: C, _: R) where C.SubSequence == Substring, C.Element == R.Element {}

// CHECK-LABEL: split_concrete_equivalence_class.(file).f02@
// CHECK-NEXT: Generic signature: <C, R where C : Collection, R : Collection, C.[Sequence]Element == Character, R.[Collection]SubSequence == Substring>
func f02<C : Collection, R : Collection>(_: C, _: R) where R.SubSequence == Substring, C.Element == R.Element {}

// CHECK-LABEL: split_concrete_equivalence_class.(file).f03@
// CHECK-NEXT: Generic signature: <C, R where C : Collection, R : Collection, C.[Collection]SubSequence == Substring, R.[Sequence]Element == Character>
func f03<C : Collection, R : Collection>(_: C, _: R) where C.SubSequence == Substring, C.Element == R.Element, C.Element == Character {}

// CHECK-LABEL: split_concrete_equivalence_class.(file).f04@
// CHECK-NEXT: Generic signature: <C, R where C : Collection, R : Collection, C.[Sequence]Element == Character, R.[Collection]SubSequence == Substring>
func f04<C : Collection, R : Collection>(_: C, _: R) where R.SubSequence == Substring, C.Element == R.Element, C.Element == Character {}

// CHECK-LABEL: split_concrete_equivalence_class.(file).f05@
// CHECK-NEXT: Generic signature: <C, R where C : Collection, R : Collection, C.[Collection]SubSequence == Substring, R.[Sequence]Element == Character>
func f05<C : Collection, R : Collection>(_: C, _: R) where C.SubSequence == Substring, C.Element == R.Element, R.Element == Character {}

// CHECK-LABEL: split_concrete_equivalence_class.(file).f06@
// CHECK-NEXT: Generic signature: <C, R where C : Collection, R : Collection, C.[Sequence]Element == Character, R.[Collection]SubSequence == Substring>
func f06<C : Collection, R : Collection>(_: C, _: R) where R.SubSequence == Substring, C.Element == R.Element, R.Element == Character {}

// CHECK-LABEL: split_concrete_equivalence_class.(file).f07@
// CHECK-NEXT: Generic signature: <C, R where C : Collection, R : Collection, C.[Collection]SubSequence == Substring, R.[Collection]SubSequence == Substring>
func f07<C : Collection, R : Collection>(_: C, _: R) where C.SubSequence == Substring, R.SubSequence == Substring, C.Element == R.Element {}

// CHECK-LABEL: split_concrete_equivalence_class.(file).f08@
// CHECK-NEXT: Generic signature: <C, R where C : Collection, R : Collection, C.[Collection]SubSequence == Substring, R.[Collection]SubSequence == Substring>
func f08<C : Collection, R : Collection>(_: C, _: R) where C.SubSequence == Substring, R.SubSequence == Substring, C.Element == R.Element, C.Element == Character {}

// CHECK-LABEL: split_concrete_equivalence_class.(file).f09@
// CHECK-NEXT: Generic signature: <C, R where C : Collection, R : Collection, C.[Collection]SubSequence == Substring, R.[Collection]SubSequence == Substring>
func f09<C : Collection, R : Collection>(_: C, _: R) where C.SubSequence == Substring, R.SubSequence == Substring, C.Element == R.Element, R.Element == Character {}
