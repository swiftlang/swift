func f0() -> <T> () {
}
func f1() -> <T> Int {
}
func f2() -> <T: SignedInteger, U: SignedInteger> Int {
}

// RUN: %target-swift-ide-test -print-ast-typechecked -enable-experimental-opaque-return-types -source-filename %s | %FileCheck %s -check-prefix=CHECK1
// CHECK1: {{^}}func f0() {{{$}}
// CHECK1: {{^}}}{{$}}
// CHECK1: {{^}}func f1() -> <T> Int {{{$}}
// CHECK1: {{^}}}{{$}}
// CHECK1: {{^}}func f2() -> <T : SignedInteger, U : SignedInteger> Int {{{$}}
// CHECK1: {{^}}}{{$}}