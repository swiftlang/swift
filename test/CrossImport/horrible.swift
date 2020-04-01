// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/

// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -I %t/lib/swift

//
// This module declares some really bad cross-import files.
//
import HorribleGoose

//
// Let's load them!
//

import UnitaryGoose
// expected-error@-1 {{cannot list cross-import overlays for 'UnitaryGoose':}}

// FIXME: It might be better to diagnose these errors on HorribleGoose's import
// decl, since they actually belong to HorribleGoose. (SR-12223)

import FlockOfGoose
// expected-error@-1 {{cannot load cross-import overlay for 'HorribleGoose' and 'FlockOfGoose':}}

import GibberishGoose
// expected-error@-1 {{cannot load cross-import overlay for 'HorribleGoose' and 'GibberishGoose': not a mapping}}

import VersionlessGoose
// expected-error@-1 {{cannot load cross-import overlay for 'HorribleGoose' and 'VersionlessGoose': missing required key 'version'}}

import FutureGoose
// expected-error@-1 {{cannot load cross-import overlay for 'HorribleGoose' and 'FutureGoose': key 'version' has invalid value: 42}}

import ListlessGoose
// expected-error@-1 {{cannot load cross-import overlay for 'HorribleGoose' and 'ListlessGoose': missing required key 'modules'}}

import AnonymousGoose
// expected-error@-1 {{cannot load cross-import overlay for 'HorribleGoose' and 'AnonymousGoose': missing required key 'name'}}

import RedundantGoose
// expected-warning@-1 {{modules 'RedundantGoose' and 'HorribleGoose' both declare module '_OverlayLibrary' as a cross-import overlay, which may cause paradoxical behavior when looking up names in them; please report this bug to the maintainers of these modules}}

// CircularGoose overlays itself with EndlessGoose, which overlays itself with
// CircularGoose, which should then break the cycle because we don't load the
// same overlay on an underlying module twice. If the test hangs, we've broken
// this behavior. (A circularity diagnostic instead wouldn't be awful.)
import CircularGoose
// no-error
