// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-inferred-signatures=on 2>&1 | %FileCheck %s

// The Requirement Machine works with canonical types internally, so make sure
// we reconstitute sugar in trivial cases before surfacing the generic
// signature to the user.

struct G<X, Y, Z> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X, Y, Z where X == Y?>
extension G where X == Optional<Y> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X, Y, Z where X == [Y]>
extension G where X == Array<Y> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X, Y, Z where X == [Y : Z], Y : Hashable>
extension G where X == Dictionary<Y, Z> {}

// We don't do () => Swift.Void.

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X, Y, Z where X == ()>
extension G where X == () {}

// Now make sure we do the same for superclass requirements.

class C<T> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X, Y, Z where X : C<Y?>>
extension G where X : C<Optional<Y>> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X, Y, Z where X : C<[Y]>>
extension G where X : C<Array<Y>> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X, Y, Z where X : C<[Y : Z]>, Y : Hashable>
extension G where X : C<Dictionary<Y, Z>> {}

// We don't do () => Swift.Void.

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G
// CHECK-NEXT: Generic signature: <X, Y, Z where X : C<()>>
extension G where X : C<()> {}
