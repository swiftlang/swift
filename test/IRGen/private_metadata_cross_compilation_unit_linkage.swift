// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Older targets reference Inner's type metadata directly.
// RUN: %target-swift-frontend -primary-file %t/User.swift %t/Container.swift -module-name main -target %target-cpu-apple-macosx14.0 -emit-ir -o - | %FileCheck %s --check-prefix=OLD-TARGET

// Newer targets (>= macOS 26) instead resolve it at runtime from a
// mangled-name constant that embeds a pointer to Inner's nominal type
// descriptor.
// RUN: %target-swift-frontend -primary-file %t/User.swift %t/Container.swift -module-name main -target %target-cpu-apple-macosx26.0 -emit-ir -o - | %FileCheck %s --check-prefix=NEW-TARGET

// REQUIRES: OS=macosx

// A private type (Inner)'s metadata/descriptor may need to be
// referenced from a different, separately compiled file than the one
// that declares it, even though ordinary name lookup can never name a
// private declaration from another file. For example, deinit on a
// ~Copyable generic type. Without external-but-hidden linkage for
// that reference, IRGen emits an internal-linkage declaration with no
// initializer, which the LLVM verifier rejects.

//--- Container.swift
struct Box<Element: ~Copyable>: ~Copyable {
  init() {}
  deinit {}
}

struct Wrapper: ~Copyable {
  private struct Inner {}
  private var storage: Box<Inner>
  init() { storage = Box() }
}

//--- User.swift
func use() {
  _ = Wrapper()
}

// OLD-TARGET: @"$s4main7WrapperV5Inner{{.*}}LLVN" = external hidden global %swift.type
// NEW-TARGET: @"$s4main7WrapperV5Inner{{.*}}LLVMn" = external hidden global %swift.type_descriptor
