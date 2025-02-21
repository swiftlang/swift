// RUN: %target-typecheck-verify-swift

enum Foo: Int { case A }
extension Foo: RawRepresentable {}
// UNSUPPORTED: OS=windows-msvc
