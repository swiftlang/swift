// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -dump-interface-hash -enable-experimental-feature Namespaces -primary-file %t/base.swift 2> %t/base.hash
// RUN: %target-swift-frontend -dump-interface-hash -enable-experimental-feature Namespaces -primary-file %t/body.swift 2> %t/body.hash
// RUN: %target-swift-frontend -dump-interface-hash -enable-experimental-feature Namespaces -primary-file %t/signature.swift 2> %t/signature.hash
// RUN: %diff -u %t/base.hash %t/body.hash
// RUN: not %diff -u %t/base.hash %t/signature.hash
// RUN: %target-swift-frontend -dump-interface-hash -enable-experimental-feature Namespaces -enable-experimental-feature ParserASTGen -primary-file %t/base.swift 2> %t/base-astgen.hash
// RUN: %target-swift-frontend -dump-interface-hash -enable-experimental-feature Namespaces -enable-experimental-feature ParserASTGen -primary-file %t/body.swift 2> %t/body-astgen.hash
// RUN: %target-swift-frontend -dump-interface-hash -enable-experimental-feature Namespaces -enable-experimental-feature ParserASTGen -primary-file %t/signature.swift 2> %t/signature-astgen.hash
// RUN: %diff -u %t/base-astgen.hash %t/body-astgen.hash
// RUN: not %diff -u %t/base-astgen.hash %t/signature-astgen.hash

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces

//--- base.swift
namespace BuildProof {
  static func answer() -> Int { 42 }
}

//--- body.swift
namespace BuildProof {
  static func answer() -> Int { 43 }
}

//--- signature.swift
namespace BuildProof {
  static func answer() -> String { "namespace" }
}
