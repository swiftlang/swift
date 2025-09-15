// RUN: %target-swift-frontend %s -emit-irgen -g -o - \
// RUN:    -module-name M  -disable-availability-checking \
// RUN:    -parse-as-library | %FileCheck %s --check-prefix=CHECK

// REQUIRES: concurrency


func ASYNC___1___() async -> Int {
  return 42
}

// Check that the first debug location after a split point is for the line
// _after_ the await.

// CHECK: define {{.*}} @"$s1M12ASYNC___2___SiyYaF"
func ASYNC___2___() async -> Int {
  var x = 10
  await ASYNC___1___()
  // CHECK: call {{.*}} @llvm.coro.suspend.async{{.*}} ptr @__swift_async_resume_get_context{{.*}} !dbg
  // CHECK: !dbg ![[FirstDbg:[0-9]+]]
  // CHECK: ![[FirstDbg]] = !DILocation(line: [[@LINE+1]]
  x = 12
  return x
}
