// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: objc_interop

import Foundation

var test = CGFloat(24+25+26+27+28+29+30)
// expected-error@-1 {{reasonable time}}
