// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-codesign %t.out
// RUN: not %target-run %t.out
// REQUIRES: executable_test

//
// Check that terminating with abort() counts as test failure.
//

import Darwin

abort()

