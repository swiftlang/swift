// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
func test_taskGroup_cancelAll() async {
    await withTaskGroup(of: Int.self, returning: Void.self) { group in // expected-note {{parameter 'group' is declared 'inout'}}
        group.spawn {
            await Task.sleep(3_000_000_000)
            let c = Task.isCancelled
            print("group task isCancelled: \(c)")
            return 0
       }

       group.spawn { // expected-error {{escaping closure captures 'inout' parameter 'group'}}
         group.cancelAll() //expected-warning{{capture of 'group' with non-Sendable type 'TaskGroup<Int>' in a '@Sendable' closure}}
         //expected-warning@-1{{mutable capture of 'inout' parameter 'group' is not allowed in concurrently-executing code; this is an error in the Swift 6 language mode}}
         // expected-note@-2 {{captured here}}

         return 0
       }
       group.spawn { [group] in
         group.cancelAll() //expected-warning{{capture of 'group' with non-Sendable type 'TaskGroup<Int>' in a '@Sendable' closure}}
         return 0
       }
       _ = await group.next()
    }

    print("done")
}
