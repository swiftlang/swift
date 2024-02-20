// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -enable-bare-slash-regex -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

func testLiteral() {
  /foo/.#^RE_LITERAL_MEMBER^#
// RE_LITERAL_MEMBER-DAG: Keyword[self]/CurrNominal:          self[#Regex<Substring>#];
}

