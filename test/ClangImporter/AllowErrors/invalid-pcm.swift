// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-pcm -module-name m -o %t/m.pcm -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/mods/module.modulemap
// RUN: %target-swift-frontend -typecheck -verify -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -fmodule-file=%t/m.pcm %t/use.swift

//--- mods/module.modulemap
module m {
  header "m.h"
}

//--- mods/m.h
@import DoesNotExist;

struct SomeTy {
  int a;
};

//--- use.swift
import m
func use(s: SomeTy) {}
