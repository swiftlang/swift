// REQUIRES: xcode_7_3
// REQUIRES: long_test
// RUN: mkdir -p %t/Outputs
// RUN: %swift-api-dump -s macosx -o %t/Outputs/
// RUN: diff -r %S/Inputs/OSX %t/Outputs/OSX 1>&2
