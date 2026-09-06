// RUN: %target-swift-frontend -emit-ir -o /dev/null -parse-as-library -module-name test -enable-experimental-feature Namespaces -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o /dev/null -parse-as-library -module-name test -enable-experimental-feature Namespaces -validate-tbd-against-ir=all -enable-testing %s
// RUN: %target-swift-frontend -emit-ir -o /dev/null -parse-as-library -module-name test -enable-experimental-feature Namespaces -enable-experimental-feature ParserASTGen -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o /dev/null -parse-as-library -module-name test -enable-experimental-feature Namespaces -enable-experimental-feature ParserASTGen -validate-tbd-against-ir=all -enable-testing %s

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces

namespace BuildProof {
  public static func answer() -> Int { 42 }
  static func internalAnswer() -> Int { 43 }
}
