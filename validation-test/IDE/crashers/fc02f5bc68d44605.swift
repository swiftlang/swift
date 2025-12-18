// {"kind":"complete","original":"90f5eddd","signature":"$s11swiftASTGen19emitDiagnosticParts33_5015A222471F7FD97A302015E08809A0LL16diagnosticEngine16sourceFileBuffer7message8severity8position6offset10highlights5editsySo07BridgeddM0V_SRys5UInt8VGSS25_CompilerSwiftDiagnostics0D8SeverityO01_yZ6Syntax16AbsolutePositionVSiSayAT6SyntaxVGSayAT10SourceEditVGtFSo0wD0VSo0W9StringRefVXEfU_","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
// REQUIRES: swift_swift_parser
extension a { b {
#^^#macro c =
#^d^#
