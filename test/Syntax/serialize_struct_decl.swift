// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree > %t
// RUN: diff %t %S/Inputs/serialize_struct_decl.json -u

struct Foo {
  let   bar : Int

  let baz : Array < Int >
      }
