// RUN: %target-swift-ide-test -code-completion -enable-experimental-cxx-interop -source-filename %s -code-completion-token=METHOD -I %S/Inputs | %FileCheck %s -check-prefix=CHECK-METHOD

import TypeClassification

func foo(x: HasMethodThatReturnsIterator) {
  x.#^METHOD^#
}
// CHECK-METHOD: Begin completions
// CHECK-METHOD-NOT: getIterator
// CHECK-METHOD-NOT: Decl[InstanceMethod]/CurrNominal:   getIterator
// CHECK-METHOD-NOT: __getIteratorUnsafe
// CHECK-METHOD-NOT: Decl[InstanceMethod]/CurrNominal:   __getIteratorUnsafe
// CHECK-METHOD: End completions
