// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -module-name test -validate-tbd-against-ir=all %s -enable-library-evolution -emit-tbd -emit-tbd-path %t.result.tbd -tbd-is-installapi -parse-as-library -emit-ir -o/dev/null

public enum TestError : Error {
  case unsupportedVersion(Int)
}
