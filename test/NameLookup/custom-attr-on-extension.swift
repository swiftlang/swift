// RUN: %target-swift-frontend -typecheck %s -verify

// Trips an assertion if an ASTScope lookup is attempted into the innermost
// DeclContext of the extension.

struct Ty{}

@foo extension Ty {} // expected-error {{unknown attribute 'foo'}}

