// {"kind":"typecheck","original":"115a94c2","signature":"swift::StorageImplInfo::StorageImplInfo(swift::ReadImplKind, swift::WriteImplKind, swift::ReadWriteImplKind)","signatureAssert":"Assertion failed: (readImpl == ReadImplKind::Inherited), function StorageImplInfo","signatureNext":"StorageImplInfoRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@_hasStorage override var a {
  get {
  }
  set
}
