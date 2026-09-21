// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/main.swift -filecheck %raw-FileCheck -I %t/Inputs -cxx-interoperability-mode=default -completion-output-dir %t/out

// Make sure we don't crash when attempting to mangle a USR for the namespace
// alias here.

//--- Inputs/module.modulemap
module CxxModule {
  header "cxx-module.h"
  requires cplusplus
}

//--- Inputs/cxx-module.h
namespace Foo {
struct Bar {};
}

namespace FooAlias = Foo;

//--- main.swift
import CxxModule

func test() {
  let _ = #^GLOBAL^#
  let _ = FooAlias.#^IN_ALIAS^#
}

// GLOBAL: Decl[TypeAlias]/OtherModule[CxxModule]: FooAlias[#Foo#]; name=FooAlias
// IN_ALIAS: Decl[Struct]/CurrNominal: Bar[#Foo.Bar#]; name=Bar
