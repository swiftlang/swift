// RUN: %target-swift-frontend -sdk "" -I %S/Inputs/macros-repeatedly-redefined -typecheck %s -verify

// This used to result in a use-after-free because the SmallVector holding the
// macros was reallocated.
import A
import B
import C
import D
import E
import F

_ = REDEFINED // no-warning
_ = x // expected-error {{cannot find 'x' in scope}}
