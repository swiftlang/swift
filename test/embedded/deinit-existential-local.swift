// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -Osize -c -o /dev/null
// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -O -c -o /dev/null

// The destroy stays as a destroy_addr; no call to the unspecialized generic
// deinit is left behind, and the specialized one exists for IRGen to use.
// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -Osize -emit-sil -o - | %FileCheck %s

// REQUIRES: swift_feature_Embedded

// CHECK-DAG: sil_moveonlydeinit $S<Int> {
// CHECK-NOT: apply {{%[0-9]+}}<Int>({{.*}}) : $@convention(method) <{{.*}}> (@owned S<{{.*}}>) -> ()

protocol P: ~Copyable {
  func f()
}

struct S<T>: ~Copyable, P {
  var p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  func f() {}
  deinit { p.deallocate() }
}

public func bindToLocal() -> Bool {
  let e: any P & ~Copyable = S<Int>()
  e.f()
  return true
}
