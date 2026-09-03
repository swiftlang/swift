// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature Namespaces \
// RUN:   | %sanitize-address > %t/legacy.ast
// RUN: %diff -u %t/astgen.ast %t/legacy.ast
// RUN: %FileCheck %s --check-prefix=NAMESPACE \
// RUN:   --implicit-check-not=enum_decl < %t/astgen.ast

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces
// UNSUPPORTED: asan

// NAMESPACE: {{^  }}(namespace_decl{{.*}}"Empty" interface_type="namespace<Empty>"
namespace Empty {}

// An escaped keyword remains usable as a namespace name.
// NAMESPACE: {{^  }}(namespace_decl{{.*}}"switch" interface_type="namespace<`switch`>"
namespace `switch` {}

// This body has no pound directives, so the legacy parser delays and reparses
// it when the AST dump asks for members.
// NAMESPACE: {{^  }}(namespace_decl{{.*}}"Delayed" interface_type="namespace<Delayed>"
// NAMESPACE: {{^    }}(namespace_decl{{.*}}"Nested" interface_type="namespace<Nested>"
// NAMESPACE: {{^    }}(struct_decl{{.*}}"Payload"
// NAMESPACE: {{^    }}(var_decl{{.*}}"answer" let static
// NAMESPACE: {{^    }}(func_decl{{.*}}"ping()" static
namespace Delayed {
  namespace Nested {}
  struct Payload {}
  static let answer = 42
  static func ping() {}
}

// NAMESPACE: {{^  }}(namespace_decl{{.*}}"Network" interface_type="namespace<Network>"
// NAMESPACE-DAG: (access_control_attr{{.*}}access_level=public)
// NAMESPACE-DAG: (available_attr{{.*}}deprecated)
// NAMESPACE: {{^    }}(namespace_decl{{.*}}"HTTP" interface_type="namespace<HTTP>"
// NAMESPACE: {{^      }}(struct_decl{{.*}}"Request"
// NAMESPACE: {{^      }}(typealias{{.*}}"StatusCode"
// NAMESPACE: {{^      }}(var_decl{{.*}}"defaultPort" let static
// NAMESPACE: {{^      }}(var_decl{{.*}}"computedPort" static
// NAMESPACE: {{^      }}(func_decl{{.*}}"connect()" static
@available(*, deprecated)
public namespace Network {
  namespace HTTP {
    struct Request {}
    typealias StatusCode = Int

    static let defaultPort = 443
    static var computedPort: Int { 444 }
    static func connect() {}
  }
}
