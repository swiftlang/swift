// RUN: %target-swift-frontend -emit-ir -o/dev/null -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -module-name test -validate-tbd-against-ir=all %s -O

// Top-level code (i.e. implicit `main`) should be handled
