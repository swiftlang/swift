// RUN: %swift-syntax-test -input-source-filename %s -parse-gen > %t
// RUN: diff %t %s

"String Literal"

""

/*comments*/+3 // comments
