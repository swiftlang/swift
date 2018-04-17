// Check that we don't crash when we verify after every pass.
// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend %s -I %S/../stdlib/Inputs/Mirror/ \
// RUN: -emit-ir -sil-verify-all -o /dev/null

class A : CustomReflectable {
  var a: Int = 1
  var customMirror: Mirror {
    return Mirror(self, children: ["aye": a])
  }
}
class X : A {}
class Y : X {}
class B : Y {
  var b: UInt = 42
  override var customMirror: Mirror {
    return Mirror(
      self,
      children: ["bee": b],
      ancestorRepresentation: .customized({ super.customMirror }))
  }
}
