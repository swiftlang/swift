// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree > %t
// RUN: diff %t %S/Inputs/serialize_class_decl.json

class Foo {
  let bar: Int
}
