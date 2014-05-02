// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift %clang-importer-sdk -target x86_64-apple-macosx10.9 -module-cache-path %t -I %S/Inputs/custom-modules -import-objc-header %S/Inputs/mixed-target/imports.h -parse %s -verify

func test(foo : FooProto) {
  let _: CInt = foo.bar
  let _: CInt = ExternIntX.x
}
