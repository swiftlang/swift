// RUN: %swiftc_driver -driver-print-jobs -sanitize=fuzzer,address -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=LIBFUZZER %s

// LIBFUZZER: libclang_rt.fuzzer
@_cdecl("LLVMFuzzerTestOneInput") public func fuzzOneInput(Data: UnsafePointer<CChar>, Size: CLong) -> CInt {
  return 0;
}
