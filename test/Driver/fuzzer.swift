// XFAIL: linux

// RUN: %swiftc_driver -driver-print-jobs -sanitize=fuzzer,address %s | %FileCheck -check-prefix=LIBFUZZER %s


// LIBFUZZER: libLLVMFuzzer.a
int LLVMFuzzerTestOneInput(const char *Data, long Size) {
  return 0;
}
