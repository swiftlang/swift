// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name M  -target %target-swift-5.1-abi-triple \
// RUN:    -parse-as-library | %FileCheck %s --check-prefix=CHECK

// REQUIRES: concurrency
// REQUIRES: CPU=x86_64 || CPU=arm64

public actor Alice {
  let bob = Bob()

  // CHECK: define {{.*}}$s1M5AliceC4callyyYaFTY0_{{.*}} !dbg ![[SCOPE0:[0-9]+]]
  // CHECK:   asyncLet_begin{{.*}} !dbg ![[HOP0:[0-9]+]]

  // CHECK: define {{.*}}$s1M5AliceC4callyyYaFTY1_{{.*}} !dbg ![[SCOPE1:[0-9]+]]
  // CHECK:   load ptr, ptr {{.*}} !dbg ![[HOP1:[0-9]+]]

  // CHECK: define {{.*}}$s1M5AliceC4callyyYaFSiyYaYbcfu_TY0_{{.*}} !dbg ![[LET_SCOPE0:[0-9]+]]
  // CHECK:   load ptr, ptr {{.*}} !dbg ![[LET_HOP0:[0-9]+]]

  // CHECK: define {{.*}}$s1M5AliceC4callyyYaFSiyYaYbcfu_TY2_{{.*}} !dbg ![[LET_SCOPE1:[0-9]+]]
  // CHECK:   store i64 %{{.*}} !dbg ![[LET_HOP1:[0-9]+]]
  public func call() async {
    // CHECK: ![[SCOPE0]] = distinct !DISubprogram({{.*}}line: [[@LINE-1]]
    // CHECK: ![[HOP0]] = !DILocation(line: [[@LINE+1]], column: 11
    async let a = bob.call(x: 1)
    // CHECK: ![[SCOPE1]] = distinct !DISubprogram({{.*}}line: [[@LINE-4]]
    // CHECK: ![[HOP1]] = !DILocation(line: [[@LINE+5]], column: 17
    // CHECK: ![[LET_SCOPE0]] = distinct !DISubprogram({{.*}}line: [[@LINE-3]]
    // CHECK: ![[LET_HOP0]] = !DILocation(line: [[@LINE-4]], column: 19
    // CHECK: ![[LET_SCOPE1]] = distinct !DISubprogram({{.*}}line: [[@LINE-5]]
    // CHECK: ![[LET_HOP1]] = !DILocation(line: [[@LINE-6]], column: 23
    print(await a)
  }
}

public actor Bob {
  public func call(x: Int) async -> Int {
    return x + x
  }
}
