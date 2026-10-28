// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -wmo -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/init.o
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -wmo -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/post.o -D POST
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/init.o %target-embedded-multi-threaded-posix-shim %target-embedded-posix-shim -o %t/posix-init.out -dead_strip -pthreads
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/post.o %target-embedded-multi-threaded-posix-shim %target-embedded-posix-shim -o %t/posix-post.out -dead_strip -pthreads
// RUN: %if OS=wasip1 %{ not %target-run %t/posix-init.out %} %else %{ %target-run not --crash %t/posix-init.out %}
// RUN: %if OS=wasip1 %{ not %target-run %t/posix-post.out %} %else %{ %target-run not --crash %t/posix-post.out %}

// RUN: %if OS=macosx %{ %target-embedded-link %target-clang-resource-dir-opt %t/init.o %target-embedded-multi-threaded-darwin-shim %target-embedded-posix-shim -o %t/darwin-init.out -dead_strip %}
// RUN: %if OS=macosx %{ %target-embedded-link %target-clang-resource-dir-opt %t/post.o %target-embedded-multi-threaded-darwin-shim %target-embedded-posix-shim -o %t/darwin-post.out -dead_strip %}
// RUN: %if OS=macosx %{ %target-run not --crash %t/darwin-init.out %}
// RUN: %if OS=macosx %{ %target-run not --crash %t/darwin-post.out %}

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || wasi_threads
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    let sem = UnsafeMutableRawPointer.allocate(
      byteCount: Int(EMBEDDED_SWIFT_SEMAPHORE_NUM_WORDS)
        * MemoryLayout<UnsafeRawPointer>.size,
      alignment: MemoryLayout<UnsafeRawPointer>.alignment)
#if POST
    _swift_sem_init(sem, Int.max)
    _swift_sem_post(sem)
#else
    // -1 converts to the largest __swift_size_t.
    _swift_sem_init(sem, -1)
#endif
  }
}
