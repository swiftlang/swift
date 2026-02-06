// {"kind":"emit-sil","languageMode":6,"signature":"swift::regionanalysisimpl::PartitionOpTranslator::translateSILInstruction(swift::SILInstruction*)"}
// RUN: not --crash %target-swift-frontend -emit-sil -swift-version 6 %s
struct d {
  var a: Int
  var b: Int
  var c: Int = 0 {
    @storageRestrictions(initializes: b)
    init {
    }
    get {
    }
  }
  init(a: Int) {
    self.a = a
    c = c
  }
}
