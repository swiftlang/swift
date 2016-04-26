// REQUIRES: xcode_7_3
// REQUIRES: long_test
// RUN: mkdir -p %t/Outputs
// RUN: %swift-api-dump -s appletvos -o %t/Outputs/
// RUN: diff -r %S/Inputs/tvOS %t/Outputs/tvOS 1>&2
