// RUN: %target-build-swift -emit-ir %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Darwin

func getAnyValue<T>(_ opt: T?) -> T { return opt! }

qsort(getAnyValue(nil), 0, 0, getAnyValue(nil))
