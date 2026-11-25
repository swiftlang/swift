// RUN: %target-swift-frontend %s -emit-ir -g -debug-info-format=codeview -Onone \
// RUN:   -module-name M -target %target-swift-5.1-abi-triple -parse-as-library \
// RUN:   -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -debug-info-format=codeview -O \
// RUN:   -module-name M -target %target-swift-5.1-abi-triple -parse-as-library \
// RUN:   -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-silgen -g -Xllvm -sil-print-debuginfo \
// RUN:   -module-name M -target %target-swift-5.1-abi-triple -parse-as-library \
// RUN:   -o - | %FileCheck %s --check-prefix=SIL

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
// The escape check traps on the closing brace of the closure.
// SIL-LABEL: sil [ossa] @$s1M16escapeAfterAwaityyyyXEYaF :
// SIL: cond_fail {{%[0-9]+}}, "non-escaping closure has escaped", loc "{{.*}}":[[@LINE-5]]:3

public func nestedEscapes(_ g: () -> Void, _ h: () -> Void) {
  withoutActuallyEscaping(g) { eg in
    withoutActuallyEscaping(h) { eh in
      use(eg)
      use(eh)
    }
  }
  print("after")
}
// SIL-LABEL: sil [ossa] @$s1M13nestedEscapesyyyyXE_yyXEtF :
// SIL: cond_fail {{%[0-9]+}}, "non-escaping closure has escaped", loc "{{.*}}":[[@LINE-4]]:3
// SIL-LABEL: sil private [closure] [ossa] @$s1M13nestedEscapesyyyyXE_yyXEtFyyycXEfU_ :
// SIL: cond_fail {{%[0-9]+}}, "non-escaping closure has escaped", loc "{{.*}}":[[@LINE-7]]:5

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
// SIL-LABEL: sil [ossa] @$s1M16elaboratePerform4work7combine6sampleS2iyXE_yxXExtYalF :
// SIL: cond_fail {{%[0-9]+}}, "non-escaping closure has escaped", loc "{{.*}}":[[@LINE-3]]:3
// SIL-LABEL: sil private [closure] [ossa] @$s1M16elaboratePerform4work7combine6sampleS2iyXE_yxXExtYalFS2iycYaXEfU_ :
// SIL: cond_fail {{%[0-9]+}}, "non-escaping closure has escaped", loc "{{.*}}":[[@LINE-6]]:5

public func caller() async {
  _ = await elaboratePerform(work: { 0 }, combine: { (x: Int) in _ = x }, sample: 0)
}

// CHECK: call void @llvm.trap()
// CHECK: ![[RUNTIME_FAILURE:[0-9]+]] = distinct !DISubprogram(name: "Swift runtime failure: non-escaping closure has escaped"
// Every location in a runtime failure subprogram must be inlined.
// CHECK-NOT: !DILocation(line: {{[0-9]+}}, {{(column: [0-9]+, )?}}scope: ![[RUNTIME_FAILURE]])
