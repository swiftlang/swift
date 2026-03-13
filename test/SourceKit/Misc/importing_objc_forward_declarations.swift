// RUN: %sourcekitd-test -req=diags %s -- -Xfrontend -enable-objc-interop -swift-version 6 -I %S/Inputs/custom-modules/ForwardDeclarations/ %s

import ProtocolFoo
import ForwardDeclaredInterfaceFoo

class Bar : Foo {
    func sayHello() {
        print("Say hello")
    }
}

let bar = Bar()
bar.sayHello()
