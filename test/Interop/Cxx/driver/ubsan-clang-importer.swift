// REQUIRES: executable_test
// REQUIRES: ubsan_runtime

// RUN: %target-swiftc_driver %s -sanitize=undefined -import-objc-header %S/sanitizer-detect.h -o %t_ubsan -cxx-interoperability-mode=default
// RUN: %target-codesign %t_ubsan
// RUN: %target-run %t_ubsan

// RUN: %target-swiftc_driver %s -import-objc-header %S/sanitizer-detect.h -o %t_no_ubsan -DEXPECT_NO_SANITIZER -cxx-interoperability-mode=default
// RUN: %target-codesign %t_no_ubsan
// RUN: %target-run %t_no_ubsan

#if EXPECT_NO_SANITIZER
assert(!is_ubsan_enabled(), "UBSan should NOT be detected without -sanitize=undefined")
assert(!is_asan_enabled(), "ASan should NOT be detected without -sanitize=undefined")
assert(!is_tsan_enabled(), "TSan should NOT be detected without -sanitize=undefined")
assert(!is_memtagstack_enabled(), "MemTag should NOT be detected without -sanitize=undefined")
#else
assert(is_ubsan_enabled(), "UBSan should be detected with -sanitize=undefined")
assert(!is_asan_enabled(), "ASan should NOT be detected with -sanitize=undefined")
assert(!is_tsan_enabled(), "TSan should NOT be detected with -sanitize=undefined")
assert(!is_memtagstack_enabled(), "MemTag should NOT be detected with -sanitize=undefined")
#endif
