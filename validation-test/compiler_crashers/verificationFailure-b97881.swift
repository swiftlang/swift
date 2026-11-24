// {"kind":"emit-sil","original":"4e36a006","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitPartialApplyInst"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
class a<b: Codable>: Codable {
  var c: b
  init(_ c: b) {
    self.c = c
  }
}
@propertyWrapper
struct d<e: Codable>: Codable {
  let box: a<e>
  init(wrappedValue: e) {
    box = a(wrappedValue)
  }
  var wrappedValue: e {
    box.c
  }
}
@propertyWrapper
struct f<e: Codable>: Codable {
  var g: e
  init(wrappedValue: @autoclosure () -> e) {
    g = wrappedValue()
  }
  var wrappedValue: e {
    g
  }
}
typealias h<b> = d<b> where b: Codable
typealias i<b> = f<b> where b: Codable
struct k: Codable {
  @i @h var j = 0
}
