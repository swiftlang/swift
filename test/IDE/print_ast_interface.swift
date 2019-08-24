/// This is my enum.
public enum Foo {
    /// This is really special.
    case A(String)

    /// This isn't special.
    case B(String)
}

// RUN: %target-swift-ide-test -print-ast-typechecked -print-interface -source-filename %s | %FileCheck %s -check-prefix=CHECK1
// CHECK1: {{^}}/// This is my enum.{{$}}
// CHECK1: {{^}}public enum Foo {{{$}}
// CHECK1: {{^}}    /// This is really special.{{$}}
// CHECK1: {{^}}    case A(String){{$}}
// CHECK1: {{^}}    /// This isn't special.{{$}}
// CHECK1: {{^}}    case B(String){{$}}
// CHECK1: {{^}}}{{$}}
