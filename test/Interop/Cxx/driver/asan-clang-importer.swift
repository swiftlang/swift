// REQUIRES: executable_test
// REQUIRES: asan_runtime

// XFAIL: OS=windows-msvc

// RUN: %target-swiftc_driver %s -sanitize=address -import-objc-header %S/sanitizer-detect.h -o %t_asan -cxx-interoperability-mode=default
// RUN: %target-codesign %t_asan
// RUN: %target-run %t_asan

// RUN: %target-swiftc_driver %s -import-objc-header %S/sanitizer-detect.h -o %t_no_asan -DEXPECT_NO_SANITIZER -cxx-interoperability-mode=default
// RUN: %target-codesign %t_no_asan
// RUN: %target-run %t_no_asan

#if EXPECT_NO_SANITIZER
assert(!is_asan_enabled(), "ASan should NOT be detected without -sanitize=address")
assert(!is_tsan_enabled(), "TSan should NOT be detected without -sanitize=thread")
assert(!is_ubsan_enabled(), "UBSan should NOT be detected without -sanitize=undefined")
assert(!is_memtagstack_enabled(), "MemTag should NOT be detected without -sanitize=memtag-stack")
#else
assert(is_asan_enabled(), "ASan should be detected with -sanitize=address")
assert(!is_tsan_enabled(), "TSan should NOT be detected with -sanitize=address")
assert(!is_ubsan_enabled(), "UBSan should NOT be detected with -sanitize=address")
assert(!is_memtagstack_enabled(), "MemTag should NOT be detected with -sanitize=address")
#endif
