// RUN: %empty-directory(%t)


// Check that -wmo is ignored when -dump-ast is present and that the AST gets
// dumped to stdout, no matter the order of the options

// RUN: %swiftc_driver -whole-module-optimization -dump-ast -module-name=main %S/../Inputs/empty.swift -### 2>%t/stderr_WMO_dump | %FileCheck -check-prefix CHECK-STDOUT %s
// RUN: %swiftc_driver -dump-ast -whole-module-optimization -module-name=main %S/../Inputs/empty.swift -### 2>%t/stderr_dump_WMO | %FileCheck -check-prefix CHECK-STDOUT %s
// RUN: %FileCheck -check-prefix CHECK-WMO %s <%t/stderr_WMO_dump
// RUN: %FileCheck -check-prefix CHECK-WMO %s <%t/stderr_dump_WMO


// Check that ignoring -wmo doesn't affect the output file paths for the AST
// dumps

// RUN: cd %t
// RUN: echo ' ' > a.swift
// RUN: echo ' ' > main.swift
// RUN: echo '{ "a.swift": { "ast-dump": "a.ast" }, "main.swift": { "ast-dump": "main.ast" }}' > outputFileMap.json

// RUN: %swiftc_driver -whole-module-optimization -dump-ast -module-name=main -output-file-map=outputFileMap.json main.swift a.swift -### 2>%t/stderr_WMO_OFM | %FileCheck -check-prefix CHECK-OFM %s
// RUN: %FileCheck -check-prefix CHECK-WMO %s <%t/stderr_WMO_OFM



// CHECK-WMO: warning: ignoring '-wmo' because '-dump-ast' was also specified

// CHECK-STDOUT-NOT: -whole-module-optimization
// CHECK-STDOUT-NOT: -wmo
// CHECK-STDOUT: -dump-ast
// CHECK-STDOUT: -o -

// CHECK-OFM-NOT: -whole-module-optimization
// CHECK-OFM-NOT: -wmo
// CHECK-OFM: -dump-ast
// CHECK-OFM-SAME: -o main.ast
// CHECK-OFM-NEXT: -dump-ast
// CHECK-OFM-SAME: -o a.ast
