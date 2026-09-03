// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces %s
// RUN: %target-swift-frontend -typecheck -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen %s
// RUN: %target-swift-frontend -dump-ast -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces %s \
// RUN:   | %{python} %utils/sanitize-address.py > %t/legacy.ast
// RUN: %target-swift-frontend -dump-ast -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen %s \
// RUN:   | %{python} %utils/sanitize-address.py > %t/astgen.ast
// RUN: %diff -u %t/legacy.ast %t/astgen.ast
// RUN: %FileCheck %s --check-prefix=AST \
// RUN:   --implicit-check-not='(enum_decl' \
// RUN:   --implicit-check-not='(struct_decl' < %t/legacy.ast
// RUN: %target-swift-frontend -print-ast -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces %s \
// RUN:   > %t/legacy.printed
// RUN: %target-swift-frontend -print-ast -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen %s > %t/astgen.printed
// RUN: %diff -u %t/legacy.printed %t/astgen.printed
// RUN: %FileCheck %s --check-prefix=PRINT < %t/legacy.printed

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces
// UNSUPPORTED: asan

// AST: (namespace_decl{{.*}}"BuildProof" interface_type="namespace<BuildProof>"
// AST-NEXT: (func_decl{{.*}}"answer()" interface_type="() -> Int"
// PRINT: namespace BuildProof {
// PRINT-NEXT: static func answer() -> Int
namespace BuildProof {
  static func answer() -> Int { 42 }
}

namespace AccessScope {
  private static func secret() -> Int { 7 }
}

// AST: (func_decl{{.*}}"callAnswer()" interface_type="() -> Int"
// AST: (dot_syntax_base_ignored type="() -> Int"
// AST-NEXT: (declref_expr type="namespace<BuildProof>"{{.*}}decl="main.(file).BuildProof
// AST-NEXT: (declref_expr type="() -> Int"{{.*}}decl="main.(file).BuildProof.answer()
func callAnswer() -> Int {
  BuildProof.answer()
}

func callPrivateNamespaceMember() -> Int {
  AccessScope.secret()
}
