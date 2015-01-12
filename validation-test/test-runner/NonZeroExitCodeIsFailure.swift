// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out
// XFAIL: *

//
// Check that non-zero exit code counts as test failure.
//

import Darwin

exit(1)

