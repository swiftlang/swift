// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out
// XFAIL: *

//
// Check that terminating with abort() counts as test failure.
//

import Darwin

abort()

