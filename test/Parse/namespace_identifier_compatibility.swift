// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -dump-parse %t/identifiers.swift \
// RUN:   | %{python} %utils/sanitize-address.py > %t/legacy-feature-off.ast
// RUN: %target-swift-frontend -dump-parse \
// RUN:   -enable-experimental-feature Namespaces %t/identifiers.swift \
// RUN:   | %{python} %utils/sanitize-address.py > %t/legacy-feature-on.ast
// RUN: %target-swift-frontend -dump-parse \
// RUN:   -enable-experimental-feature ParserASTGen %t/identifiers.swift \
// RUN:   | %{python} %utils/sanitize-address.py > %t/astgen-feature-off.ast
// RUN: %target-swift-frontend -dump-parse \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature Namespaces %t/identifiers.swift \
// RUN:   | %{python} %utils/sanitize-address.py > %t/astgen-feature-on.ast
// RUN: %diff -u %t/legacy-feature-off.ast %t/legacy-feature-on.ast
// RUN: %diff -u %t/legacy-feature-off.ast %t/astgen-feature-off.ast
// RUN: %diff -u %t/legacy-feature-off.ast %t/astgen-feature-on.ast
// RUN: %FileCheck %s --check-prefix=IDENTIFIERS \
// RUN:   --implicit-check-not=namespace_decl < %t/legacy-feature-off.ast

// RUN: not %target-swift-frontend -dump-parse %t/boundaries.swift 2>/dev/null \
// RUN:   | %FileCheck %s --check-prefix=BOUNDARIES \
// RUN:     --implicit-check-not=namespace_decl
// RUN: not %target-swift-frontend -dump-parse \
// RUN:   -enable-experimental-feature Namespaces %t/boundaries.swift 2>/dev/null \
// RUN:   | %FileCheck %s --check-prefix=BOUNDARIES \
// RUN:     --implicit-check-not=namespace_decl
// RUN: not %target-swift-frontend -dump-parse \
// RUN:   -enable-experimental-feature ParserASTGen %t/boundaries.swift 2>/dev/null \
// RUN:   | %FileCheck %s --check-prefix=BOUNDARIES \
// RUN:     --implicit-check-not=namespace_decl
// RUN: not %target-swift-frontend -dump-parse \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature Namespaces %t/boundaries.swift 2>/dev/null \
// RUN:   | %FileCheck %s --check-prefix=BOUNDARIES \
// RUN:     --implicit-check-not=namespace_decl

// RUN: %target-swift-frontend -dump-parse \
// RUN:   -dump-ast-format default-with-decl-contexts \
// RUN:   -enable-experimental-feature Namespaces %t/mixed-valid.swift \
// RUN:   | %{python} %utils/sanitize-address.py > %t/legacy-mixed-valid.ast
// RUN: %target-swift-frontend -dump-parse \
// RUN:   -dump-ast-format default-with-decl-contexts \
// RUN:   -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen %t/mixed-valid.swift \
// RUN:   | %{python} %utils/sanitize-address.py > %t/astgen-mixed-valid.ast
// RUN: %diff -u %t/legacy-mixed-valid.ast %t/astgen-mixed-valid.ast
// RUN: %FileCheck %s --check-prefix=MIXED-VALID < %t/legacy-mixed-valid.ast

// RUN: not %target-swift-frontend -dump-parse %t/feature-off.swift 2>/dev/null \
// RUN:   | %FileCheck %s --check-prefix=FEATURE-OFF \
// RUN:     --implicit-check-not=namespace_decl
// RUN: not %target-swift-frontend -dump-parse \
// RUN:   -enable-experimental-feature ParserASTGen %t/feature-off.swift 2>/dev/null \
// RUN:   | %FileCheck %s --check-prefix=FEATURE-OFF \
// RUN:     --implicit-check-not=namespace_decl

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces

// IDENTIFIERS: (pattern_named "namespace")
// IDENTIFIERS: (func_decl{{.*}}"namespace()"
// IDENTIFIERS: (parameter "namespace"
// IDENTIFIERS: (unresolved_dot_expr{{.*}}field="member"
// IDENTIFIERS: (closure_expr

// BOUNDARIES-LABEL: (func_decl{{.*}}"adjacentIdentifiers()"
// BOUNDARIES: (unresolved_decl_ref_expr{{.*}}name="namespace"
// BOUNDARIES: (unresolved_decl_ref_expr{{.*}}name="value"
// BOUNDARIES-LABEL: (func_decl{{.*}}"laterBrace()"
// BOUNDARIES: (unresolved_decl_ref_expr{{.*}}name="namespace"
// BOUNDARIES: (unresolved_decl_ref_expr{{.*}}name="value"
// BOUNDARIES: (unresolved_decl_ref_expr{{.*}}name="consume"
// BOUNDARIES: (closure_expr
// BOUNDARIES-LABEL: (func_decl{{.*}}"escapedIntroducer()"
// BOUNDARIES: (unresolved_decl_ref_expr{{.*}}name="namespace"
// BOUNDARIES: (unresolved_decl_ref_expr{{.*}}name="Escaped"

// MIXED-VALID: (func_decl{{.*}}"localDeclaration()"
// MIXED-VALID: (namespace_decl{{.*}}"Local" interface_type="namespace<Local>"
// MIXED-VALID: (struct_decl{{.*}}"Member"

// FEATURE-OFF: (struct_decl{{.*}}"AfterFeatureOff"

//--- identifiers.swift

let namespace = 0

func namespace() {}
func takes(namespace: Int) {}

func ordinaryUses() {
  namespace()
  namespace(namespace)
  takes(namespace: namespace)
  _ = namespace.member
  namespace {}
  `namespace` {}
  _ = `namespace`
}

//--- boundaries.swift

func adjacentIdentifiers() {
  namespace value
}

func laterBrace() {
  namespace value + consume {}
}

func escapedIntroducer() {
  `namespace` Escaped {}
}

//--- mixed-valid.swift

func localDeclaration() {
  namespace Local {
    struct Member {}
  }
}

//--- feature-off.swift

namespace FeatureOff {}
struct AfterFeatureOff {}
