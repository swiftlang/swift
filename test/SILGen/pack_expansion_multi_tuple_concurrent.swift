// RUN: %target-build-swift -parse-as-library %s -o %t
// RUN: %target-run %t | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency

// This test verifies that structs containing multi-pack expansion tuples
// can be captured in Task closures without memory corruption.
//
// The issue: When a struct has a field of type (repeat each P) and P is
// instantiated from multiple pack parameters (e.g., Wrapper<repeat each D, repeat each P>),
// capturing that struct in a Task closure causes "freed pointer was not the last allocation"
// errors in the task allocator.
//
// Related patterns that DO work:
// - Plain multi-pack tuples captured in Tasks (without struct wrapper)
// - Structs with single-pack tuples captured in Tasks
// - Structs with multi-pack tuples captured in regular @Sendable closures (not Tasks)

protocol Plugin: Sendable {}
struct DefaultPlugin: Plugin { let id: Int }
struct CustomPlugin: Plugin { let name: String }

struct Wrapper<each P: Plugin>: Sendable {
    let items: (repeat each P)

    func count() -> Int {
        var c = 0
        for _ in repeat each items { c += 1 }
        return c
    }
}

// This function creates a Wrapper with a multi-pack tuple and captures it in a Task
func runEngine<each D: Plugin, each P: Plugin>(
    defaults: (repeat each D),
    custom: (repeat each P)
) async -> Int {
    let wrapper = Wrapper(items: (repeat each defaults, repeat each custom))

    let task = Task { @Sendable in
        wrapper.count()
    }

    return await task.value
}

@main
struct Main {
    static func main() async {
        let result = await runEngine(
            defaults: (DefaultPlugin(id: 1), DefaultPlugin(id: 2)),
            custom: (CustomPlugin(name: "a"),)
        )
        // CHECK: Result: 3
        print("Result: \(result)")
    }
}
