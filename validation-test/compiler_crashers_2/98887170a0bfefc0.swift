// {"kind":"typecheck","signature":"$s11swiftASTGen24evaluatePoundIfCondition10astContext13diagEnginePtr16sourceFileBuffer02ifF4Text14shouldEvaluateSiSo17BridgedASTContextV_SvSo0S9StringRefVAKSbtF","signatureAssert":"Assertion failed: (size() >= 1 && \"must have a top-level module\"), function Module"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: swift_swift_parser
#if canImport(#available
