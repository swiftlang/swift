// RUN: %target-swift-frontend -primary-file %s -emit-irgen  -module-name async -disable-availability-checking -O -enable-async-frame-pointer-all | %FileCheck %s --check-prefix=ENABLED
// RUN: %target-swift-frontend -primary-file %s -emit-irgen  -module-name async -disable-availability-checking -O -disable-async-frame-pointer-all | %FileCheck %s --check-prefix=DISABLED

// x86_64 seems to choose a different default for frame pointers.
// REQUIRES: CPU=arm64 || CPU=arm64e

@inline(never)
public func plusOne() {
  print("+1")
}

@inline(never)
public func minusOne() {
  print("-1")
}
// ENABLED: define{{.*}} swifttailcc void @"$s5async6calleeyyYaF"(ptr swiftasync %0) [[ATTRS:#[0-9]+]]
// ENABLED: define swifttailcc void @"$s5async6callerySiSbYaF"(ptr swiftasync %0, i1 %1) [[ATTRS]]

// ENABLED: attributes [[ATTRS]] = { {{.*}}"frame-pointer"="all"

// DISABLED: define{{.*}} swifttailcc void @"$s5async6calleeyyYaF"(ptr swiftasync %0) [[ATTRS:#[0-9]+]]
// DISABLED: define swifttailcc void @"$s5async6callerySiSbYaF"(ptr swiftasync %0, i1 %1) [[ATTRS]]

// DISABLED: attributes [[ATTRS]] = { {{.*}}"frame-pointer"="non-leaf"



public func callee() async { }

public func caller(_ b: Bool) async -> Int{
  plusOne()
  if b {
    await callee()
  }
  minusOne()
  return 0
}
