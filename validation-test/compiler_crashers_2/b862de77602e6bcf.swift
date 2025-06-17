// {"signature":"$s11swiftASTGen19addQueuedDiagnostic20queuedDiagnosticsPtr011perFrontende5StateH04text8severity4cLoc12categoryName17documentationPath015highlightRangesH0012numHighlightT013fixItsUntypedySv_SvSo16BridgedStringRefVSo0zE8SeverityVSo0z6SourceN0VA2NSPySo0Z15CharSourceRangeVGSgSiSo0Z8ArrayRefVtF"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: swift_swift_parser
class a { @objc ( : ( b
