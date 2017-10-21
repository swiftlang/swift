// RUN: %swift-syntax-test -input-source-filename %s -parse-gen > %t
// RUN: diff %t %s
// RUN: %swift-syntax-test -input-source-filename %s -parse-gen -print-node-kind > %t.withkinds
// RUN: diff %t.withkinds %S/Outputs/round_trip_parse_gen.swift.withkinds

"String Literal"

""

/*comments*/+3 // comments
