// RUN: %target-build-swift -emit-ir %s
// REQUIRES: sdk

import Dispatch

dispatch_sync_f(nil, nil, nil)
