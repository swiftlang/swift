// RUN: if [[ -d %t ]]; then chmod -R u+rwx %t && rm -rf %t; fi
// RUN: mkdir -p %t/good
// RUN: mkdir -p %t/bad
// RUN: chmod a-rx %t/bad
// RUN: %target-swift-frontend -emit-module -o %t/good %S/../Inputs/empty.swift
// RUN: not %target-swift-frontend %s -typecheck -I %t/bad -I %t/good -show-diagnostics-after-fatal 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend %s -typecheck -I %t/good -I %t/bad -show-diagnostics-after-fatal 2>&1 | %FileCheck %s
// RUN: chmod a+rx %t/bad

import empty // CHECK-NOT: empty
import ThisOneReallyDoesNotExist // CHECK: [[@LINE]]:{{[0-9]+}}: error: no such module 'ThisOneReallyDoesNotExist'
