// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE

import TensorFlow
let t = Tensor#^COMPLETE^#
