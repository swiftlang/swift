// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-module -o /dev/null

// Regression test: a local computed property with an opaque return type inside
// an if #available block at top-level must not crash during -emit-module
// serialization. The crash was an assertion in verifyAttrSerializable<Accessor>
// because inferring actor isolation for local accessors incorrectly added a
// NonisolatedAttr, which DeclAttr.def does not permit on AccessorDecl.
// rdar://175548302

if #available(macOS 10.15, *) {
  var prop: some Equatable { 0 }
}
