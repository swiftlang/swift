// RUN: %empty-directory(%t)
// RUN: echo "func foo() {}" > %t/theinput.swift

// RUN: %target-swift-frontend -index-store-path %t/idx -typecheck %t/theinput.swift -o %t/s.o
// RUN: ls %t/idx/*/records/* | grep "theinput.swift" | count 1
// RUN: cp -r %t/idx %t/idx-orig

// RUN: touch %t/theinput.swift
// RUN: %target-swift-frontend -index-store-path %t/idx -typecheck %t/theinput.swift -o %t/s.o
// RUN: diff -r -u %t/idx/*/records %t/idx-orig/*/records
// No change in record.

// RUN: echo '// Comment.' >> %t/theinput.swift
// RUN: %target-swift-frontend -index-store-path %t/idx -typecheck %t/theinput.swift -o %t/s.o
// RUN: diff -r -u %t/idx/*/records %t/idx-orig/*/records
// No change in record.

// RUN: echo 'func goo() {}' >> %t/theinput.swift
// RUN: %target-swift-frontend -index-store-path %t/idx -typecheck %t/theinput.swift -o %t/s.o
// RUN: not diff -r -u %t/idx/*/records %t/idx-orig/*/records
// RUN: ls %t/idx/*/records/* | grep "theinput.swift" | count 2
// Changed!  Wrote a new record.
