// REQUIRES: executable_test
// REQUIRES: memtag_sanitizer

// RUN: %target-swiftc_driver %s -sanitize=memtag-stack -import-objc-header %S/sanitizer-detect.h -o %t_memtag -cxx-interoperability-mode=default -target arm64-apple-macosx10.9
// RUN: %target-codesign %t_memtag
// RUN: %target-run %t_memtag

// RUN: %target-swiftc_driver %s -import-objc-header %S/sanitizer-detect.h -o %t_no_memtag -DEXPECT_NO_SANITIZER -cxx-interoperability-mode=default
// RUN: %target-codesign %t_no_memtag
// RUN: %target-run %t_no_memtag

#if EXPECT_NO_SANITIZER
assert(!is_memtagstack_enabled(), "MemTag should NOT be detected without -sanitize=memtag-stack")
assert(!is_asan_enabled(), "ASan should NOT be detected without -sanitize=memtag-stack")
assert(!is_tsan_enabled(), "TSan should NOT be detected without -sanitize=memtag-stack")
assert(!is_ubsan_enabled(), "UBSan should NOT be detected without -sanitize=memtag-stack")
#else
assert(is_memtagstack_enabled(), "MemTag should be detected with -sanitize=memtag-stack")
assert(!is_asan_enabled(), "ASan should NOT be detected with -sanitize=memtag-stack")
assert(!is_tsan_enabled(), "TSan should NOT be detected with -sanitize=memtag-stack")
assert(!is_ubsan_enabled(), "UBSan should NOT be detected with -sanitize=memtag-stack")
#endif
