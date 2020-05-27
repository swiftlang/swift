// UNSUPPORTED: windows
// UNSUPPORTED: CPU=powerpc64le
// RUN: %swiftc_driver -driver-print-jobs -sanitize=fuzzer,address -target x86_64-apple-macosx10.9 -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=LIBFUZZER_OSX %s
// RUN: %swiftc_driver -driver-print-jobs -sanitize=fuzzer,address -target x86_64-unknown-linux-gnu -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=LIBFUZZER_LINUX %s

// LIBFUZZER_OSX: libclang_rt.fuzzer
// LIBFUZZER_LINUX: -fsanitize=address,fuzzer

@_cdecl("LLVMFuzzerTestOneInput")
public func test(_ start: UnsafeRawPointer, _ count: Int) -> CInt {
  return 0
}
