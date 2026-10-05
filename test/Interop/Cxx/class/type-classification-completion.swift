// RUN: %target-swift-ide-test -code-completion -enable-experimental-cxx-interop -source-filename %s -code-completion-token=METHOD -I %S/Inputs | %FileCheck %s -check-prefix=CHECK-METHOD

import TypeClassification

func foo(x: HasMethodThatReturnsIterator) {
  x.#^METHOD^#
}
// The method keeps its name as '@unsafe(always)'; its migration stub is hidden.
// CHECK-METHOD: Begin completions
// CHECK-METHOD-NOT: __getIteratorUnsafe
// CHECK-METHOD: Decl[InstanceMethod]/CurrNominal:   getIterator()[#Iterator#]
// CHECK-METHOD-NOT: __getIteratorUnsafe
// CHECK-METHOD: End completions
