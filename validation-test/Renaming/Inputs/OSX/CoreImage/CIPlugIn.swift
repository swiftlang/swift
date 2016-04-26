
@available(OSX 10.4, *)
class CIPlugIn : NSObject {
  class func loadAllPlugIns()
  class func loadNonExecutablePlugIns()
  @available(OSX 10.7, *)
  class func load(_ url: NSURL!, allowExecutableCode allowExecutableCode: Bool)
}
