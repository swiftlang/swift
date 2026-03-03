// RUN: %target-swift-emit-silgen %s -target %target-cpu-unknown-windows-msvc | %FileCheck %s
// REQUIRES: OS=windows-msvc

@_silgen_name("windows10")
@available(Windows 10, *)
public func windows10()

@_silgen_name("unavailable")
@available(Windows, unavailable)
public func unavailable()


// CHECK-LABEL: sil [ossa] @$s20availability_windows15testIfAvailableyyF : $@convention(thin) () -> ()
// CHECK: cond_br
// CHECK: function_ref @windows10
public func testIfAvailable() {
  if #available(Windows 10, *) {
    windows10()
  }
}
// CHECK: sil [available 10] @windows10 : $@convention(thin) () -> ()

// CHECK-LABEL: sil [ossa] @$s20availability_windows15testUnavailableyyF : $@convention(thin) () -> ()
// CHECK: function_ref @unavailable
@available(*, unavailable)
public func testUnavailable() {
  unavailable()
}

// FIXME: Mark [weak_imported] when weak linking is supported on Windows (https://github.com/apple/swift/issues/53303)
// CHECK: sil @unavailable : $@convention(thin) () -> ()
