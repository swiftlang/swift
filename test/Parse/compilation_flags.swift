// RUN: %target-swift-frontend -DFOO=99.9 -DBAR='"a string"' -dump-ast %s 2>&1 | %FileCheck --strict-whitespace %s

import Swift

print("\(FOO) \(BAR)")

// CHECK: (float_literal_expr {{.*}} value=99.9))))
// CHECK: (string_literal_expr {{.*}} value="a string" {{.*}}))))
