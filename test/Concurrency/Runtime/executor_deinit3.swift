// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -enable-experimental-concurrency %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// for sleep
#if canImport(Darwin)
    import Darwin
#elseif canImport(Glibc)
    import Glibc
#endif

class Runner {
    func run() async {
        while !Task.isCancelled {
            sleep(1)
        }
    }
}

actor Container {
    var generation = 0
    var runners = [Int : Task.Handle<Void, Never>]()

    func build(_ n: Int) {
        for _ in 0..<n {
            let id = generation
            generation += 1
            let t = Task.runDetached { [weak self] in
                let r = Runner()
                await r.run()
                await self?.remove(id)
            }
            runners[id] = t
        }
    }

    func cancelAll() {
        var count = 0
        for (_, v) in runners {
            v.cancel()
            count += 1
        }
        print("Cancelled \(count) runners.")
    }

    deinit {
        print("deinit Container with \(runners.count) runners")
    }

    func remove(_ id: Int) {
        runners.removeValue(forKey: id)
    }
}

// CHECK: starting
// CHECK: Cancelled 5 runners.

// FIXME: this doesn't work until we have https://github.com/apple/swift/pull/36298
// COM: deinit Container with {{[0-9]+}} runners

@main struct RunIt {
    static func startTest() async {
        let c = Container()
        await c.build(5)
        sleep(5)
        await c.cancelAll()
    }

static func main() async {
        print("starting")
        await RunIt.startTest()
        sleep(5)
    }
}