// {"kind":"typecheck","signature":"$s11swiftASTGen19addQueuedDiagnostic20queuedDiagnosticsPtr011perFrontende5StateH04text8severity3loc12diagnosticID013categoryChainH0011numCategoryR7Entries015highlightRangesH00s9HighlightW013fixItsUntypedySv_SvSo16BridgedStringRefVSo0A0O0E4KindVAQ9SourceLocVAOSPySo07BridgedeT5EntryVGSgSiSPyAQ15CharSourceRangeVGSgSiSo15BridgedArrayRefVtF","signatureNext":"DiagnosticBridge::addQueueDiagnostic"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
// REQUIRES: swift_swift_parser
class a { @objc ( : ( b
