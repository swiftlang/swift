// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -primary-file %s -o %t/a.o -primary-file %S/Inputs/lazy_properties_batch_mode_b.swift -o %t/b.o
func foo(_: B) {}

