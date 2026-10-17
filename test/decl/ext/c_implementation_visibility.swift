// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t \
// RUN:   -module-name main -package-name pkg
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t \
// RUN:   -module-name main -package-name pkg -enable-testing

// UNSUPPORTED: OS=windows-msvc

//--- module.modulemap
module Vis {
  header "Vis.h"
}

//--- Vis.h
__attribute__((visibility("hidden"))) void hiddenPublic(void);
__attribute__((visibility("hidden"))) void hiddenPackage(void);
__attribute__((visibility("hidden"))) void hiddenInternal(void);
__attribute__((visibility("hidden"))) void hiddenFilePrivate(void);
void defaultPublic(void);

//--- main.swift
internal import Vis

@c @implementation public func hiddenPublic() {}
// expected-warning@-1 {{global function 'hiddenPublic()' is declared 'public' but will not be exported, because C function 'hiddenPublic' has hidden visibility}}{{20-27=}}

@c @implementation package func hiddenPackage() {}
// expected-warning@-1 {{global function 'hiddenPackage()' is declared 'package' but will not be exported, because C function 'hiddenPackage' has hidden visibility}}{{20-28=}}

@c @implementation func hiddenInternal() {}

@c @implementation fileprivate func hiddenFilePrivate() {}

@c @implementation public func defaultPublic() {}
