// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import CxxStdlib
import StdString

let _ = HasMethodThatReturnsString().getString()
