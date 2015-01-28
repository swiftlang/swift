// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out

//
// Check that terminating with abort() counts as test failure.
//

// This test should fail.
// XFAIL: *

import Darwin

abort()

