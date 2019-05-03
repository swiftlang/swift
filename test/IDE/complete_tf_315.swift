// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | %FileCheck %s

import TensorFlow
let t = Tensor#^COMPLETE^#

// CHECK-LABEL: Begin completions
// CHECK: End completions
