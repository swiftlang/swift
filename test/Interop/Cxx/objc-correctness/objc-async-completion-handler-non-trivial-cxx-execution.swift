// RUN: %empty-directory(%t2)

// RUN: %target-interop-build-clangxx -c %S/Inputs/objc-async-completion-handler-non-trivial.mm -o %t2/objc-async-impl.o -fobjc-arc

// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -target %target-swift-5.1-abi-triple -parse-as-library -Xlinker %t2/objc-async-impl.o) | %FileCheck %s
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -target %target-swift-5.1-abi-triple -parse-as-library -O -Xlinker %t2/objc-async-impl.o) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// The argument of the completion handler block is passed with the Itanium C++
// ABI convention: the caller destroys it.

import Foundation
import ObjCAsyncNonTrivialCxx

func viaCompletionHandler() async {
  await withCheckedContinuation { (c: CheckedContinuation<Void, Never>) in
    TrackedProducer().produce(completionHandler: { t in
      _ = t
      c.resume()
    })
  }
}

func viaAsync() async {
  let t = await TrackedProducer().produce()
  _ = t
}

@main struct Main {
  static func main() async {
    await viaCompletionHandler()
    // CHECK: live after completion handler: 0
    print("live after completion handler: \(getTrackedLiveCount())")

    await viaAsync()
    // CHECK: live after async: 0
    print("live after async: \(getTrackedLiveCount())")
  }
}
