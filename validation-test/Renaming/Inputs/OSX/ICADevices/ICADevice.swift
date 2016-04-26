
struct ICDHeader {
  var err: ICAError
  var refcon: UInt
  init()
  init(err err: ICAError, refcon refcon: UInt)
}
typealias ICDCompletion = @convention(c) (UnsafeMutablePointer<ICDHeader>!) -> Void
struct ICD_NewObjectPB {
  var header: ICDHeader
  var parentObject: ICAObject
  var objectInfo: ICAObjectInfo
  var object: ICAObject
  init()
  init(header header: ICDHeader, parentObject parentObject: ICAObject, objectInfo objectInfo: ICAObjectInfo, object object: ICAObject)
}
struct ICD_DisposeObjectPB {
  var header: ICDHeader
  var object: ICAObject
  init()
  init(header header: ICDHeader, object object: ICAObject)
}
