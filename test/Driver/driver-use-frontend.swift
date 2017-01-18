// RUN: %swift_driver_plain -driver-use-frontend-path bin/completely-fake-driver -driver-print-jobs %s 2>&1 | %FileCheck %s
// CHECK: {{^}}bin/completely-fake-driver -frontend
