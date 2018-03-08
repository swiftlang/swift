// RUN: %swiftc_driver -driver-print-jobs -sanitize=fuzzer,address -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=LIBFUZZER %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=fuzzer -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=LIBFUZZER_NO_ASAN %s

// LIBFUZZER: libclang_rt.fuzzer
// LIBFUZZER: libclang_rt.asan
// LIBFUZZER-NOT: libclang_rt.ubsan


// LIBFUZZER_NO_ASAN: libclang_rt.fuzzer
// LIBFUZZER_NO_ASAN: libclang_rt.ubsan
@_cdecl("LLVMFuzzerTestOneInput") public func fuzzOneInput(Data: UnsafePointer<CChar>, Size: CLong) -> CInt {
  return 0;
}
