// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree > %t
// RUN: diff %t %S/Inputs/serialize_multiple_decls.json

struct A {
}

struct B {
}
