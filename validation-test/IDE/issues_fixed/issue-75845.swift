// RUN: %batch-code-completion

// https://github.com/apple/swift/issues/75845
// Make sure we don't crash.

struct Foo {
  init() {
    do {
    } catch {
      #^A^#self#^B^# = #^C^#error#^D^#
    }
  }
}
// A: Decl[LocalVar]/Local: error[#any Error#]; name=error
// B: Begin completions
// C: Decl[LocalVar]/Local: error[#any Error#]; name=error
// D: Begin completions
