// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -parse-as-library -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Verify that local types declared inside the body of a function 
// expanded from an attached peer macro are surfaced through 
// SourceFile::getLocalTypeDecls(), allowing SILGen, IRGen, and 
// debug-info type reconstruction to see them.
//
// Without this, the synchronous peer's nested 'Local' type would be 
// missing SIL functions (causing a link error), missing type metadata 
// (causing runtime failures for any operation requiring metadata), and 
// could trigger an IRGenDebugInfo crash when compiling with -g.
//
// https://github.com/swiftlang/swift/issues/88174

// Symptom 1: link and run.
// RUN: %target-build-swift -swift-version 5 -Xfrontend -disable-availability-checking -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library %s -o %t/main -module-name MacroUser
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s --check-prefix=CHECK-EXEC

// Symptom 2: IRGen emits full type metadata (Mn / Ma / Mf) for the
// synchronous peer's local type, not just SIL functions for it.
// RUN: %target-swift-frontend -swift-version 5 -emit-ir -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library -module-name MacroUser -disable-availability-checking %s | %FileCheck %s --check-prefix=CHECK-IR

// Symptom 3: -g compiles cleanly. The successful RUN line is the assertion;
// before the fix, IRGenDebugInfo would fail to round-trip the synchronous
// peer's local type and crash.
// RUN: %target-swift-frontend -swift-version 5 -c -g -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library -module-name MacroUser -disable-availability-checking -o %t/dbg.o %s

@attached(peer, names: overloaded)
macro RemoveAsync() = #externalMacro(module: "MacroDefinition", type: "RemoveAsyncMacro")

// Top-level case: Peer macro on a free function.
@RemoveAsync
func example() async -> Int {
  struct Local {
    let x: Int
  }
  return Local(x: 1).x
}

// Peer-on-nested-method case: Peer macro on a method of a nominal type.
// Verifies the AST walker reaches members of nominal types and the 
// request fires on them.
struct Container {
  @RemoveAsync
  func nested() async -> Int {
    struct NestedLocal {
      let y: Int
    }
    return NestedLocal(y: 2).y
  }
}

// CHECK-IR-DAG: $s9MacroUser7exampleSiyF5LocalL_VMn
// CHECK-IR-DAG: $s9MacroUser7exampleSiyF5LocalL_VMa
// CHECK-IR-DAG: $s9MacroUser7exampleSiyF5LocalL_VMf
// CHECK-IR-DAG: $s9MacroUser9ContainerV6nestedSiyF11NestedLocalL_VMn
// CHECK-IR-DAG: $s9MacroUser9ContainerV6nestedSiyF11NestedLocalL_VMa
// CHECK-IR-DAG: $s9MacroUser9ContainerV6nestedSiyF11NestedLocalL_VMf

@main
struct Driver {
  static func main() {
    print(example())
    print(Container().nested())
    // CHECK-EXEC: 1
    // CHECK-EXEC: 2
  }
}