func test(arg: Int) -> Int { 1 }

func foo() {
  #sourceLocation(file: "first/foo.swift", line: 100)
  test(arg: 1)
}

func bar() {
  #sourceLocation(file: "second/foo.swift", line: 100)
}

test(arg: 2)

// RUN: %target-swift-frontend -emit-silgen -module-name MyMod %s -enable-experimental-feature ParserASTGen -diagnostic-style llvm \
// RUN:  2>&1 >/dev/null | %FileCheck --enable-windows-compatibility --strict-whitespace %s

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// CHECK:      {{^}}second/foo.swift:102:1: warning: result of call to 'test(arg:)' is unused
// CHECK-NEXT: {{^}}test(arg: 2)
// CHECK-NEXT: {{^}}^   ~~~~~~~~

// CHECK:      {{^}}first/foo.swift:100:3: warning: result of call to 'test(arg:)' is unused
// CHECK-NEXT: {{^}}  test(arg: 1)
// CHECK-NEXT: {{^}}  ^   ~~~~~~~~

// CHECK:      {{^SOURCE_DIR[\//]test[/\\]ASTGen[\//]sourcelocation\.swift}}:4:25: warning: '#sourceLocation' directive produces '#fileID' string of 'MyMod/foo.swift', which conflicts with '#fileID' strings produced by other paths in the module
// CHECK-NEXT: {{^}}  #sourceLocation(file: "first/foo.swift", line: 100)
// CHECK-NEXT: {{^}}                        ^

// CHECK:      {{^SOURCE_DIR[\//]test[/\\]ASTGen[\//]sourcelocation\.swift}}:9:25: warning: '#sourceLocation' directive produces '#fileID' string of 'MyMod/foo.swift', which conflicts with '#fileID' strings produced by other paths in the module
// CHECK-NEXT: {{^}}  #sourceLocation(file: "second/foo.swift", line: 100)
// CHECK-NEXT: {{^}}                        ^

// CHECK:      {{^SOURCE_DIR[\//]test[/\\]ASTGen[\//]sourcelocation\.swift}}:9:25: note: change file in '#sourceLocation' to 'first/foo.swift'
// CHECK-NEXT: {{^}}  #sourceLocation(file: "second/foo.swift", line: 100)
// CHECK-NEXT: {{^}}                        ^~~~~~~~~~~~~~~~~~
// CHECK-NEXT: {{^}}                        "first/foo.swift"

