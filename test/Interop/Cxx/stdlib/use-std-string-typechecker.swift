// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import CxxStdlib
import StdString

let _ = HasMethodThatReturnsString().getString()

let x: std.string = "Hello"
let y: std.string = "\(x), World!" 
