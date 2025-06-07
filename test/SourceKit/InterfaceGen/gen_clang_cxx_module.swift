// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %sourcekitd-test -req=interface-gen -module CxxModule -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -I %t/Inputs -target %target-triple %clang-importer-sdk-nosource | %FileCheck %s
// RUN: %sourcekitd-test -req=interface-gen -module CxxModule -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -cxx-interoperability-mode=default -I %t/Inputs -target %target-triple %clang-importer-sdk-nosource | %FileCheck %s


// RUN: not %sourcekitd-test -req=interface-gen -module CxxModule -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -Xcc -DERROR  -I %t/Inputs -target %target-triple %clang-importer-sdk-nosource 2>&1 | %FileCheck --check-prefix=NOLOAD %s


//--- Inputs/module.modulemap
module CxxModule {
    header "headerA.h"
    requires cplusplus
}

//--- Inputs/headerA.h

namespace ns {

#ifdef ERROR
#error "Unluckee"
#endif

class CxxClass {
public:
    int x;

    CxxClass(): x(0) {}

    inline void method() const {}
};

} // ns

using ClassType = ns::CxxClass;

// CHECK: public enum ns {
// CHECK: public struct CxxClass {
// CHECK: public init()
// CHECK: public func method()
// CHECK: public typealias ClassType = ns.CxxClass

// NOLOAD: Could not load module: CxxModule
