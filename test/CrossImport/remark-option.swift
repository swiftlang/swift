// This file tests that the -Rcross-import option causes an appropriate remark to be emitted
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/
// RUN: %target-typecheck-verify-swift -enable-cross-import-overlays -Rcross-import -I %t/include -I %t/lib/swift -F %t/Frameworks

import DeclaringLibrary
// FIXME: Similarly to horrible.swift, ideally we would emit this remark on
// DeclaringLibrary decl, since the cross-import overlay actually belongs to
// the DeclaringLibrary. (https://github.com/apple/swift/issues/54649)
import BystandingLibrary // expected-remark {{import of 'DeclaringLibrary' and 'BystandingLibrary' triggered a cross-import of '_OverlayLibrary'}}
