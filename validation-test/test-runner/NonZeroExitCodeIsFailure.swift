// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-codesign %t.out
// RUN: not %target-run %t.out
// REQUIRES: executable_test

//
// Check that non-zero exit code counts as test failure.
//

import Darwin

exit(1)

