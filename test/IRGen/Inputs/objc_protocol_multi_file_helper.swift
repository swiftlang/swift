@objc protocol BaseProto {
  func doStuff()
}

@objc protocol IntermediateProto : BaseProto {}
