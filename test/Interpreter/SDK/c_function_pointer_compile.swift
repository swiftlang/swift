// RUN: %target-build-swift -emit-ir %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Dispatch

func getAnyValue<T>(opt: T?) -> T { return opt! }

dispatch_sync_f(getAnyValue(nil), getAnyValue(nil), getAnyValue(nil))
