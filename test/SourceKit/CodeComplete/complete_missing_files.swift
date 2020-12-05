// RUN: %sourcekitd-test -req=complete -pos=1:1 %s -- /tmp/SOMETHING_DOES_NOT_EXIST_1.swift %s /tmp/SOMETHING_DOES_NOT_EXIST_2.swift | %FileCheck %s

// CHECK: results: [
