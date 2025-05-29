// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)

// REQUIRES: executable_test

import NestedEnums
import StdlibUnittest

var NestedEnumsTestSuite = TestSuite("Nested Enums")

NestedEnumsTestSuite.test("Make and compare") {
  let val: ns.EnumInNS = ns.kA
  expectNotEqual(val, ns.kB)
  let valNested = ns.nestedNS.kNestedA
  expectNotEqual(valNested, ns.nestedNS.kNestedB)
}

NestedEnumsTestSuite.test("Same enum, different namespaces") {
  let nsEnum1 : ns.EnumInNS = ns.kA
  let nsEnum2 : ns.EnumInNS = ns.kA

  let nsBEnum1 : nsB.EnumInNS = nsB.kA
  let nsBEnum2 : nsB.EnumInNS = nsB.kA

  expectEqual(nsEnum1, nsEnum2)
  expectEqual(nsBEnum1, nsBEnum2)

  let nsNestedEnum1 : ns.nestedNS.EnumInNS = ns.nestedNS.kA
  let nsNestedEnum2 : ns.nestedNS.EnumInNS = ns.nestedNS.kA

  let nsBNestedEnum1 : nsB.nestedNS.EnumInNS = nsB.nestedNS.kA
  let nsBNestedEnum2 : nsB.nestedNS.EnumInNS = nsB.nestedNS.kA

  expectEqual(nsNestedEnum1, nsNestedEnum2)
  expectEqual(nsBNestedEnum1, nsBNestedEnum2)
}

NestedEnumsTestSuite.test("Same enum class, different namespaces") {
  let nsEnumClass1 : ns.ScopedEnumInNS = ns.ScopedEnumInNS.scopeA
  let nsEnumClass2 : ns.ScopedEnumInNS = ns.ScopedEnumInNS.scopeA

  let nsBEnumClass1 : nsB.ScopedEnumInNS = nsB.ScopedEnumInNS.scopeA
  let nsBEnumClass2 : nsB.ScopedEnumInNS = nsB.ScopedEnumInNS.scopeA

  expectEqual(nsEnumClass1, nsEnumClass2)
  expectEqual(nsBEnumClass1, nsBEnumClass2)
}

NestedEnumsTestSuite.test("Same enum class, different classes") {
  let classAEnum1 : ClassA.EnumInClass = .scopeA
  let classAEnum2 : ClassA.EnumInClass = .scopeA

  let classBEnum1 : ClassB.EnumInClass = .scopeA
  let classBEnum2 : ClassB.EnumInClass = .scopeA

  expectEqual(classAEnum1, classAEnum2)
  expectEqual(classBEnum1, classBEnum2)
}

runAllTests()
