// RUN: %target-swiftc_driver %s -g -sanitize=thread -target %sanitizers-target-triple -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0:ignore_interceptors_accesses=1 %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: tsan_runtime

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run

// Test that we do not report a race on block release operation.
import Foundation

public class Sad : NSObject {
    private var _source: DispatchSourceTimer?
    public override init() {
        _source = DispatchSource.makeTimerSource()

        // If this line is commented out no data race.
        _source?.setEventHandler(handler: globalFuncHandler)

        super.init()
        _source?.resume()
    }
    deinit {
        _source?.cancel()
    }
}

func globalFuncHandler() {
}

func dotest() {
    _ = Sad()
}

dotest()
sleep(1)
print("Done.")

// CHECK: Done.
// CHECK-NOT: ThreadSanitizer: data race
