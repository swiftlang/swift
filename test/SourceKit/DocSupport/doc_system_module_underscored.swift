
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mcp)
// RUN: %empty-directory(%t/UnderscoredProto.framework/Modules/UnderscoredProto.swiftmodule)
// RUN: cp %S/../Inputs/UnderscoredProto.swiftinterface %t/UnderscoredProto.framework/Modules/UnderscoredProto.swiftmodule/%module-target-triple.swiftinterface

// RUN: %sourcekitd-test -req=doc-info  -synthesized-extension -module UnderscoredProto -- -target %target-triple -Fsystem %t -module-cache-path %t/mcp > %t.response
// RUN: %diff -u %s.response %t.response
