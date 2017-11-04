// RUN: %swift-syntax-test -input-source-filename %s -parse-gen > %t
// RUN: diff -u %s %t
// RUN: %swift-syntax-test -input-source-filename %s -parse-gen -print-node-kind > %t.withkinds
// RUN: diff -u %S/Outputs/round_trip_parse_gen.swift.withkinds %t.withkinds

class C {
  func bar(_ a: Int) {}
  func bar1(_ a: Float) -> Float { return -0.6 + 0.1 - 0.3 }
  func foo() {
    var a = /*comment*/"abc"/*comment*/
    var b = /*comment*/+2/*comment*/
    bar(1)
    bar(+10)
    bar(-10)
    bar1(-1.1)
    bar1(1.1)
    var f = /*comments*/+0.1/*comments*/
  }
}
