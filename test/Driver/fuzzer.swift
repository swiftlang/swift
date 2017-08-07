// RUN: %swiftc_driver -driver-print-jobs -sanitize=fuzzer,address %s | %FileCheck -check-prefix=LIBFUZZER %s

// LIBFUZZER: libLLVMFuzzer.a
@_cdecl("LLVMFuzzerTestOneInput") public func fuzzOneInput(Data: UnsafePointer<CChar>, Size: CLong) -> CInt {
  return 0;
}
