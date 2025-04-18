// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -suppress-remarks %t%{fs-sep}some%{fs-sep}subdir%{fs-sep}file1.swift -verify-additional-file %t%{fs-sep}Cxx%{fs-sep}include%{fs-sep}cxx-header.h -I %t%{fs-sep}Cxx%{fs-sep}include -cxx-interoperability-mode=default -module-name main

//--- Cxx/include/module.modulemap
module CxxModule {
    requires cplusplus
    header "cxx-header.h"
}

//--- Cxx/include/cxx-header.h
#ifndef __CXX_HEADER_H
#define __CXX_HEADER_H
class __attribute__((__swift_attr__("private_fileid:main/file1.swift"))) OK {
private:
  void priv(void) const {}
};

class
__attribute__((__swift_attr__("private_fileid:main/file1.swift"))) // expected-note {{SWIFT_PRIVATE_FILEID annotation found here}}
__attribute__((__swift_attr__("private_fileid:main/file2.swift"))) // expected-note {{SWIFT_PRIVATE_FILEID annotation found here}}
MultipleAnnotations {}; // expected-error {{multiple SWIFT_PRIVATE_FILEID annotations were found on 'MultipleAnnotations'}}

class
__attribute__((__swift_attr__("private_fileid:main/file1.swift"))) // expected-note {{SWIFT_PRIVATE_FILEID annotation found here}}
__attribute__((__swift_attr__("private_fileid:main/file1.swift"))) // expected-note {{SWIFT_PRIVATE_FILEID annotation found here}}
RepeatedAnnotations {}; // expected-error {{multiple SWIFT_PRIVATE_FILEID annotations were found on 'RepeatedAnnotations'}}

class
__attribute__((__swift_attr__("private_fileid:main/some/subdir/file1.swift")))
WithSubdir {}; // expected-warning@-1 {{SWIFT_PRIVATE_FILEID annotation on 'WithSubdir' does not have a valid file ID}}

class
__attribute__((__swift_attr__("private_fileid:main/file1")))
MissingExtension {}; // expected-warning@-1 {{SWIFT_PRIVATE_FILEID annotation on 'MissingExtension' does not have a valid file ID}}

class
__attribute__((__swift_attr__("private_fileid:main/file1.swift")))
IncompleteType;
class IncompleteType {};

#endif /* CXX_HEADER_H */

//--- some/subdir/file1.swift
import CxxModule
extension OK { func ext() { priv() } } // should work as expected
extension MultipleAnnotations {}
extension RepeatedAnnotations {}
extension WithSubdir {}
extension MissingExtension {}
extension IncompleteType {}

//--- some/subdir/file2.swift
import CxxModule

// This file is referenced in the MultipleAnnotations test case.
// We don't need to create it here but we may as well do so.
