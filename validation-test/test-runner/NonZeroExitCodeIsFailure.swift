// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out

// This test isn't temporarily disabled; it actually should terminate with a
// non-zero exit code.
//
// XFAIL: *

//
// Check that non-zero exit code counts as test failure.
//

import Darwin

exit(1)

