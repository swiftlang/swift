// REQUIRES: VENDOR=apple 
// RUN: %target-swift-frontend -emit-ir -o/dev/null -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -module-name test -validate-tbd-against-ir=all %s -O

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-as-library -module-name test %s -tbd-install_name test -emit-tbd -emit-tbd-path %t/typecheck.tbd
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name test %s -tbd-install_name test -emit-tbd -emit-tbd-path %t/emit-ir.tbd
// RUN: %llvm-readtapi --compare %t/typecheck.tbd %t/emit-ir.tbd

// Top-level code (i.e. implicit `main`) should be handled
