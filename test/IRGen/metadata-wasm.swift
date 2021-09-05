// RUN: %empty-directory(%t)
// RUN: %swift -emit-object -target wasm32-unknown-wasi -parse-as-library -parse-stdlib %s -o %t/main.o
// REQUIRES: CODEGENERATOR=WebAssembly

public enum E {
  case a
  case b
}

public class C {}
public struct S {}
