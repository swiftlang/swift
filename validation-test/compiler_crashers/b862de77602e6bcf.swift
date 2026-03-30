// {"kind":"typecheck","signature":"$s11swiftASTGen19addQueuedDiagnostic20queuedDiagnosticsPtr011perFrontende5StateH04text8severity3loc013categoryChainH0011numCategoryP7Entries015highlightRangesH00q9HighlightU013fixItsUntypedySv_SvSo16BridgedStringRefVSo0A0O0E4KindVAP9SourceLocVSPySo0zeR5EntryVGSgSiSPyAP15CharSourceRangeVGSgSiSo0Z8ArrayRefVtF","signatureNext":"addQueueDiagnostic"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
// REQUIRES: swift_swift_parser
class a { @objc ( : ( b
