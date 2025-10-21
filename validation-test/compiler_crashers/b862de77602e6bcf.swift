// {"kind":"typecheck","signature":"$s11swiftASTGen19addQueuedDiagnostic20queuedDiagnosticsPtr011perFrontende5StateH04text8severity3loc12categoryName17documentationPath015highlightRangesH0012numHighlightT013fixItsUntypedySv_SvSo16BridgedStringRefVSo0A0O0E4KindVAP9SourceLocVA2NSPyAP15CharSourceRangeVGSgSiSo0Z8ArrayRefVtF"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
// REQUIRES: swift_swift_parser
class a { @objc ( : ( b
