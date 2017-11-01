// RUN: %swift-syntax-test -input-source-filename %s -parse-gen > %t
// RUN: diff -u %t %s
// RUN: %swift-syntax-test -input-source-filename %s -parse-gen -print-node-kind > %t.withkinds
// RUN: diff -u %t.withkinds %S/Outputs/round_trip_parse_gen.swift.withkinds

class C {
  func bar(_ a: Int) {}
  func foo() {
    var a = /*comment*/"abc"/*comment*/
    var b = /*comment*/+2/*comment*/
    bar(1)
    bar(+10)
    bar(-10)
  }
}
