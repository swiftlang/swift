// RUN: %empty-directory(%t)

// Reading an unowned reference whose object is gone.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test -DREAD_DEAD_UNOWNED %s -c -o %t/read.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/read.o -o %t/read.out -dead_strip
// RUN: %target-run not --crash %t/read.out

// Forming a new unowned reference to an object that is already gone, by copying
// a struct that holds one.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test -DRETAIN_DEAD_UNOWNED %s -c -o %t/retain.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/retain.o -o %t/retain.out -dead_strip
// RUN: %target-run not --crash %t/retain.out

// The operation each variant must reach has to be the one IRGen emits, not an
// unrelated trap on the way there.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test -DREAD_DEAD_UNOWNED %s -emit-ir | %FileCheck --check-prefix READ-IR %s
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test -DRETAIN_DEAD_UNOWNED %s -emit-ir | %FileCheck --check-prefix RETAIN-IR %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: embedded_stdlib_default_codegen

// UNSUPPORTED: OS=wasip1
// UNSUPPORTED: OS=emscripten
// The RUN lines assert a trap via LLVM's `not --crash`, but emscripten-run.py
// passes its arguments to `node`, which then cannot load `not` as a JS module
// (`Error: Cannot find module 'not'`).

// Weak and unowned operations that must trap. Each is its own -D variant with
// its own RUN line, since a trap ends the process.
//
// The trap message is not checked, as _embeddedReportFatalError may not produce
// output

final class Target {
  let id: Int
  init(id: Int) { self.id = id }
  deinit { print("deinit \(id)") }
}

// Escape holders to globals so the optimizer cannot delete the reference
// operations along with a dead local.
#if READ_DEAD_UNOWNED
final class UnownedHolder {
  unowned var ref: Target
  init(_ r: Target) { self.ref = r }
}
var escape: UnownedHolder? = nil
#endif

#if RETAIN_DEAD_UNOWNED
struct UnownedBox {
  unowned var ref: Target
}
var escape: UnownedBox? = nil
#endif

@main
struct Main {
  static func main() {
#if READ_DEAD_UNOWNED
    // READ-IR-DAG: call {{.*}}@swift_unownedRetainStrong
    var t: Target? = Target(id: 1)
    let h = UnownedHolder(t!)
    escape = h
    t = nil
    print("reading \(h.ref.id)")
#endif

#if RETAIN_DEAD_UNOWNED
    // RETAIN-IR-DAG: call {{.*}}@swift_unownedRetain
    var t: Target? = Target(id: 2)
    let box = UnownedBox(ref: t!)
    t = nil
    // Copying the box forms a second unowned reference to a dead object.
    escape = box
#endif

    print("unreached")
  }
}
