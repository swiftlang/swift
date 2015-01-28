// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out

//
// Check that non-zero exit code counts as test failure.
//

// This test should fail.
// XFAIL: *

import Darwin

exit(1)

