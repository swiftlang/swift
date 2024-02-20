// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-experimental-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

@available(SwiftStdlib 5.1, *)
func test_taskGroup_cancelAll() async {
    await withTaskGroup(of: Int.self, returning: Void.self) { group in
        group.spawn {
            await Task.sleep(3_000_000_000)
            let c = Task.isCancelled
            print("group task isCancelled: \(c)")
            return 0
       }

       group.spawn {
         group.cancelAll() //expected-warning{{capture of 'group' with non-sendable type 'TaskGroup<Int>' in a `@Sendable` closure}}
                           //expected-error@-1{{mutable capture of 'inout' parameter 'group' is not allowed in concurrently-executing code}}

         return 0
       }
       group.spawn { [group] in
         group.cancelAll() //expected-warning{{capture of 'group' with non-sendable type 'TaskGroup<Int>' in a `@Sendable` closure}}
         return 0
       }
       _ = await group.next()
    }

    print("done")
}
