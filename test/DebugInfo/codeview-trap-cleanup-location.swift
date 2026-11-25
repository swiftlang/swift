// RUN: %target-swift-frontend %s -emit-ir -g -debug-info-format=codeview -Onone \
// RUN:   -module-name M -target %target-swift-5.1-abi-triple -parse-as-library \
// RUN:   -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -debug-info-format=codeview -O \
// RUN:   -module-name M -target %target-swift-5.1-abi-triple -parse-as-library \
// RUN:   -o - | %FileCheck %s

// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/88563

@inline(never)
func use(_ c: @escaping () -> Void) async { c() }

@inline(never)
func use(_ c: @escaping () -> Void) { c() }

public func escapeAfterAwait(_ g: () -> Void) async {
  await withoutActuallyEscaping(g) { eg in
    await use(eg)
  }
  print("after")
}

public func nestedEscapes(_ g: () -> Void, _ h: () -> Void) {
  withoutActuallyEscaping(g) { eg in
    withoutActuallyEscaping(h) { eh in
      use(eg)
      use(eh)
    }
  }
  print("after")
}

public func elaboratePerform<T>(
  work: () -> Int, combine: (T) -> Void, sample: T
) async -> Int {
  await withoutActuallyEscaping(work) { escapingWork in
    await withoutActuallyEscaping(combine) { escapingCombine in
      await withTaskGroup(of: Int.self, returning: Int.self) { group in
        group.addTask { escapingWork() }
        _ = await group.next()!
        escapingCombine(sample)
        return 0
      }
    }
  }
}

public func caller() async {
  _ = await elaboratePerform(work: { 0 }, combine: { (x: Int) in _ = x }, sample: 0)
}

// CHECK: call void @llvm.trap()
// CHECK: ![[RUNTIME_FAILURE:[0-9]+]] = distinct !DISubprogram(name: "Swift runtime failure: non-escaping closure has escaped"
// Every location in a runtime failure subprogram must be inlined.
// CHECK-NOT: !DILocation(line: {{[0-9]+}}, {{(column: [0-9]+, )?}}scope: ![[RUNTIME_FAILURE]])
