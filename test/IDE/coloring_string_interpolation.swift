// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | %FileCheck %s

// CHECK: <kw>_</kw> = <str>"</str>\<anchor>(</anchor><anchor>)</anchor><str>"</str>
// CHECK: <kw>if</kw> <kw>true</kw> { <kw>_</kw> = <str>"</str>\<anchor>(</anchor><anchor>)</anchor><str>"</str> }

if true {
    _ = "\()"
}

if true { _ = "\()" }