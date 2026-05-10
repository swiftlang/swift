// RUN: %target-swiftc_driver %s -g -sanitize=thread %import-libdispatch -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: env %env-TSAN_OPTIONS=abort_on_error=0 %target-run %t_tsan-binary 2>&1 | %FileCheck %s --implicit-check-not='ThreadSanitizer'
// REQUIRES: executable_test
// REQUIRES: tsan_runtime

// rdar://101876380
// UNSUPPORTED: OS=ios

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run

// Test that we do not report a race on block release operation.
import Dispatch
#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#else
#error("Unsupported platform")
#endif

public class Sad {
    private var _source: DispatchSourceTimer?
    public init() {
        _source = DispatchSource.makeTimerSource()

        // If this line is commented out no data race.
        _source?.setEventHandler(handler: globalFuncHandler)

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
