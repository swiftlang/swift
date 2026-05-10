// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | %FileCheck %s

// NOTE: Testing 'class' at the end of the file. Don't add anything after that.
// https://github.com/apple/swift/issues/50905

// CHECK: <kw>protocol</kw> P : <type>class</type>
protocol P : class
