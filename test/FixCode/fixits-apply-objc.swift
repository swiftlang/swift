// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -disable-objc-attr-requires-foundation-module -typecheck %s -emit-fixits-path %t.remap
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result
import ObjectiveC

// REQUIRES: objc_interop

@objc class Selectors {
  func takeSel(_: Selector) {}
  @objc func mySel() {}
  func test() {
    takeSel("mySel")
    takeSel(Selector("mySel"))
  }
}

@objc class OtherClass {
  func test(s: Selectors) {
    s.takeSel("mySel")
    s.takeSel(Selector("mySel"))
  }
}

@objc class Base {
  @objc func baseSel() {}
}

@objc class Outer {
  func takeSel(_: Selector) {}
  @objc func outerSel() {}

  @objc class Inner: Base {
    func takeSel(_: Selector) {}

    @objc func innerSel() {}

    func test(s: Selectors, o: Outer) {
      s.takeSel("mySel")
      s.takeSel(Selector("mySel"))

      takeSel("innerSel")
      takeSel(Selector("innerSel"))

      takeSel("baseSel")
      takeSel(Selector("baseSel"))

      o.takeSel("outerSel")
      o.takeSel(Selector("outerSel"))
    }
  }

  func test(s: Selectors, i: Inner) {
    s.takeSel("mySel")
    s.takeSel(Selector("mySel"))

    i.takeSel("innerSel")
    i.takeSel(Selector("innerSel"))

    i.takeSel("baseSel")
    i.takeSel(Selector("baseSel"))

    takeSel("outerSel")
    takeSel(Selector("outerSel"))
  }
}

extension Outer {
  func test2(s: Selectors, i: Inner) {
    s.takeSel("mySel")
    s.takeSel(Selector("mySel"))

    i.takeSel("innerSel")
    i.takeSel(Selector("innerSel"))

    i.takeSel("baseSel")
    i.takeSel(Selector("baseSel"))

    takeSel("outerSel")
    takeSel(Selector("outerSel"))
  }
}

func freeTest(s: Selectors, o: Outer, i: Outer.Inner) {
  s.takeSel("mySel")
  s.takeSel(Selector("mySel"))

  i.takeSel("innerSel")
  i.takeSel(Selector("innerSel"))

  i.takeSel("baseSel")
  i.takeSel(Selector("baseSel"))

  o.takeSel("outerSel")
  o.takeSel(Selector("outerSel"))
}

func foo(an : Any) {
  let a1 : AnyObject
  a1 = an
  let a2 : AnyObject?
  a2 = an
  let a3 : AnyObject!
  a3 = an
}

func foo1(_ an : Any) {
  let obj: AnyObject = an
}

func foo2(_ messageData: Any?) -> AnyObject? {
  return messageData
}
