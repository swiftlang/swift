// REQUIRES: xcode_7_3
// REQUIRES: long_test
// RUN: mkdir -p %t/Outputs
// RUN: %swift-api-dump -s iphoneos -o %t/Outputs/
// RUN: diff -r %S/Inputs/iOS %t/Outputs/iOS 1>&2
