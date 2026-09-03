// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -dump-parse \
// RUN:   -dump-ast-format default-with-decl-contexts \
// RUN:   -enable-experimental-feature Namespaces \
// RUN:   -verify -verify-additional-prefix legacy- %t/recovery.swift \
// RUN:   > %t/legacy.raw
// RUN: %{python} %utils/sanitize-address.py \
// RUN:   < %t/legacy.raw > %t/legacy.ast
// RUN: %target-swift-frontend -dump-parse \
// RUN:   -dump-ast-format default-with-decl-contexts \
// RUN:   -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -verify -verify-additional-prefix astgen- %t/recovery.swift \
// RUN:   > %t/astgen.raw
// RUN: %{python} %utils/sanitize-address.py \
// RUN:   < %t/astgen.raw > %t/astgen.ast
// RUN: %FileCheck %s --check-prefix=RECOVERED \
// RUN:   --implicit-check-not=generic_type_param_decl \
// RUN:   --implicit-check-not=GenericTypeParamDecl \
// RUN:   --implicit-check-not="NamespaceDecl name=class" \
// RUN:   --implicit-check-not="NamespaceDecl name=123" \
// RUN:   --implicit-check-not=DroppedMissingName \
// RUN:   --implicit-check-not=KeywordBody \
// RUN:   --implicit-check-not=NumericBody < %t/legacy.ast
// RUN: %FileCheck %s --check-prefix=RECOVERED \
// RUN:   --implicit-check-not=generic_type_param_decl \
// RUN:   --implicit-check-not=GenericTypeParamDecl \
// RUN:   --implicit-check-not="NamespaceDecl name=class" \
// RUN:   --implicit-check-not="NamespaceDecl name=123" \
// RUN:   --implicit-check-not=DroppedMissingName \
// RUN:   --implicit-check-not=KeywordBody \
// RUN:   --implicit-check-not=NumericBody < %t/astgen.ast
// RUN: %target-swift-frontend -dump-parse \
// RUN:   -dump-ast-format default-with-decl-contexts \
// RUN:   -enable-experimental-feature Namespaces \
// RUN:   -verify -verify-additional-prefix legacy- %t/unterminated.swift \
// RUN:   | %FileCheck %s --check-prefix=UNTERMINATED
// RUN: %target-swift-frontend -dump-parse \
// RUN:   -dump-ast-format default-with-decl-contexts \
// RUN:   -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -verify -verify-additional-prefix astgen- %t/unterminated.swift \
// RUN:   | %FileCheck %s --check-prefix=UNTERMINATED

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces

// RECOVERED: (namespace_decl{{.*}}"Network" interface_type="namespace<Network>"
// RECOVERED: (struct_decl{{.*}}"Retained"
// RECOVERED: (namespace_decl{{.*}}"Generic" interface_type="namespace<Generic>"
// RECOVERED: (namespace_decl{{.*}}"Inheriting" interface_type="namespace<Inheriting>"
// RECOVERED: (namespace_decl{{.*}}"Constrained" interface_type="namespace<Constrained>"
// RECOVERED: (namespace_decl{{.*}}"MissingIntroducers" interface_type="namespace<MissingIntroducers>"
// RECOVERED: (var_decl{{.*}}"value"
// RECOVERED: (func_decl{{.*}}"f()"
// RECOVERED: (struct_decl{{.*}}"Container"
// RECOVERED: (struct_decl{{.*}}"AfterRecovery"
// RECOVERED: (namespace_decl{{.*}}"MissingBody" interface_type="namespace<MissingBody>"

// UNTERMINATED: (namespace_decl{{.*}}"Unterminated" interface_type="namespace<Unterminated>"
// UNTERMINATED: (struct_decl{{.*}}"Member"

//--- recovery.swift

protocol Marker {}

namespace Network.HTTP { // expected-legacy-error {{namespace names must be a single identifier}} expected-astgen-error {{unexpected code '.HTTP' in namespace}}
  struct Retained {}
}

namespace Generic<T> {} // expected-legacy-error {{namespaces cannot have generic parameters}} expected-astgen-error {{unexpected code '<T>' in namespace}}
namespace Inheriting: Marker {} // expected-legacy-error {{namespaces cannot have an inheritance clause}} expected-astgen-error {{unexpected code ': Marker' in namespace}}
namespace Constrained where T: Marker {} // expected-legacy-error {{namespaces cannot have a 'where' clause}} expected-astgen-error {{unexpected code 'where T: Marker' in namespace}}

namespace MissingIntroducers {
  value: Int // expected-legacy-error {{expected 'var' keyword in property declaration}} expected-astgen-error {{expected 'var' in variable}} expected-astgen-note {{insert 'var'}}
  f() {} // expected-legacy-error {{expected 'func' keyword in instance method declaration}} expected-astgen-error {{expected 'func' in function}} expected-astgen-note {{insert 'func'}}
}

struct Container {
  namespace { struct DroppedMissingName {} } // expected-legacy-error {{expected identifier in namespace declaration}} expected-astgen-error {{expected identifier in namespace}} expected-astgen-note {{insert identifier}}
}

namespace class { struct KeywordBody {} } // expected-error {{keyword 'class' cannot be used as an identifier here}} expected-note {{if this name is unavoidable, use backticks to escape it}}
namespace 123 { struct NumericBody {} } // expected-legacy-error {{namespace name can only start with a letter or underscore, not a number}} expected-astgen-error {{identifier can only start with a letter or underscore, not a number}}

struct AfterRecovery {}

public namespace MissingBody // expected-legacy-error {{expected '{' in namespace}} expected-astgen-error {{expected member block in namespace}} expected-astgen-note {{insert member block}}

//--- unterminated.swift

namespace Unterminated { // expected-legacy-note {{to match this opening '{'}} expected-astgen-note {{to match this opening '{'}}
  struct Member {} // expected-astgen-error {{expected '}' to end namespace}} expected-astgen-note {{insert '}'}}
// expected-legacy-error@+1 {{expected '}' at end of namespace}}
