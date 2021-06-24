func f() -> <T> () {
}
func g() -> <T> Int {
}

// RUN: %target-swift-ide-test -print-ast-typechecked -enable-experimental-opaque-return-types -source-filename %s | %FileCheck %s -check-prefix=CHECK1
// CHECK1: {{^}}func f() {{{$}}
// CHECK1: {{^}}}{{$}}
// CHECK1: {{^}}func g() -> <T> Int {{{$}}
// CHECK1: {{^}}}{{$}}