// RUN: %target-build-swift -sanitize=address -sanitize-coverage=edge %s -o %t_binary
// RUN: rm -rf %t_coverage_dir
// RUN: mkdir %t_coverage_dir
// RUN: ASAN_OPTIONS=abort_on_error=0,coverage=1,coverage_dir=%t_coverage_dir %t_binary
// check the coverage file exists
// RUN: ls %t_coverage_dir/sanitizer_coverage.swift.tmp_binary.*.sancov > /dev/null

// REQUIRES: executable_test
// REQUIRES: asan_runtime
// For now restrict this test to platforms where we know this test will pass
// REQUIRES: CPU=x86_64
// REQUIRES: OS=macosx

func sayHello() {
  print("Hello")
}

sayHello()
