// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METHOD -I %S/Inputs | %FileCheck %s -check-prefix=CHECK-METHOD

import TypeClassification

func foo(x: HasMethodThatReturnsIterator) {
  x.#^METHOD^#
}
// CHECK-METHOD-NOT: getIterator
// CHECK-METHOD-NOT: __getIteratorUnsafe
