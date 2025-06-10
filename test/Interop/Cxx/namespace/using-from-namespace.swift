// RUN: %target-swift-emit-irgen -I %S/Inputs/ -cxx-interoperability-mode=default -g %s

import UsingFromNamespace

func f(_ foo: Foo) {}
func g(_ bar: Bar) {}
