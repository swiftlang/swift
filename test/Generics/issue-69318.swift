// RUN: %target-typecheck-verify-swift

// We used to crash -- https://github.com/apple/swift/issues/69318

public protocol View {
    associatedtype NodeChildren: ViewGraphNodeChildren
}

public protocol ViewGraphNodeChildren {
    associatedtype ParentView: View where ParentView.NodeChildren == Self
    // expected-note@-1 {{protocol requires nested type 'ParentView'}}
}

public protocol ChildlessView: View where NodeChildren == EmptyViewGraphNodeChildren<Self> {}
// expected-error@-1 {{circular reference}}

public struct EmptyViewGraphNodeChildren<ParentView: ChildlessView>: ViewGraphNodeChildren {}
// expected-note@-1:15 {{through reference here}}
// expected-note@-2:70 {{through reference here}}
// expected-error@-3 {{type 'EmptyViewGraphNodeChildren<ParentView>' does not conform to protocol 'ViewGraphNodeChildren'}}
// expected-note@-4 {{add stubs for conformance}}
