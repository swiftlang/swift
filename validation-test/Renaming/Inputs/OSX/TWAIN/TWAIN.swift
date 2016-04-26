
var TWON_PROTOCOLMINOR: Int32 { get }
var TWON_PROTOCOLMAJOR: Int32 { get }
typealias TW_HANDLE = UnsafeMutablePointer<Int8>
typealias TW_MEMREF = UnsafeMutablePointer<Int8>
typealias TW_STR32 = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
typealias pTW_STR32 = UnsafeMutablePointer<UInt8>
typealias TW_STR64 = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
typealias pTW_STR64 = UnsafeMutablePointer<UInt8>
typealias TW_STR128 = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
typealias pTW_STR128 = UnsafeMutablePointer<UInt8>
typealias TW_STR255 = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
typealias pTW_STR255 = UnsafeMutablePointer<UInt8>
typealias TW_INT8 = Int8
typealias pTW_INT8 = UnsafeMutablePointer<Int8>
typealias TW_INT16 = Int16
typealias pTW_INT16 = UnsafeMutablePointer<Int16>
typealias TW_INT32 = Int32
typealias pTW_INT32 = UnsafeMutablePointer<Int32>
typealias TW_UINT8 = UInt8
typealias pTW_UINT8 = UnsafeMutablePointer<UInt8>
typealias TW_UINT16 = UInt16
typealias pTW_UINT16 = UnsafeMutablePointer<UInt16>
typealias TW_UINT32 = UInt32
typealias pTW_UINT32 = UnsafeMutablePointer<UInt32>
typealias TW_BOOL = UInt16
typealias pTW_BOOL = UnsafeMutablePointer<UInt16>
struct TW_FIX32 {
  var Whole: TW_INT16
  var Frac: TW_UINT16
  init()
  init(Whole Whole: TW_INT16, Frac Frac: TW_UINT16)
}
typealias pTW_FIX32 = UnsafeMutablePointer<TW_FIX32>
struct TW_CIEPOINT {
  var X: TW_FIX32
  var Y: TW_FIX32
  var Z: TW_FIX32
  init()
  init(X X: TW_FIX32, Y Y: TW_FIX32, Z Z: TW_FIX32)
}
typealias pTW_CIEPOINT = UnsafeMutablePointer<TW_CIEPOINT>
struct TW_DECODEFUNCTION {
  var StartIn: TW_FIX32
  var BreakIn: TW_FIX32
  var EndIn: TW_FIX32
  var StartOut: TW_FIX32
  var BreakOut: TW_FIX32
  var EndOut: TW_FIX32
  var Gamma: TW_FIX32
  var SampleCount: TW_FIX32
  init()
  init(StartIn StartIn: TW_FIX32, BreakIn BreakIn: TW_FIX32, EndIn EndIn: TW_FIX32, StartOut StartOut: TW_FIX32, BreakOut BreakOut: TW_FIX32, EndOut EndOut: TW_FIX32, Gamma Gamma: TW_FIX32, SampleCount SampleCount: TW_FIX32)
}
typealias pTW_DECODEFUNCTION = UnsafeMutablePointer<TW_DECODEFUNCTION>
struct TW_ELEMENT8 {
  var Index: TW_UINT8
  var Channel1: TW_UINT8
  var Channel2: TW_UINT8
  var Channel3: TW_UINT8
  init()
  init(Index Index: TW_UINT8, Channel1 Channel1: TW_UINT8, Channel2 Channel2: TW_UINT8, Channel3 Channel3: TW_UINT8)
}
typealias pTW_ELEMENT8 = UnsafeMutablePointer<TW_ELEMENT8>
struct TW_FRAME {
  var Left: TW_FIX32
  var Top: TW_FIX32
  var Right: TW_FIX32
  var Bottom: TW_FIX32
  init()
  init(Left Left: TW_FIX32, Top Top: TW_FIX32, Right Right: TW_FIX32, Bottom Bottom: TW_FIX32)
}
typealias pTW_FRAME = UnsafeMutablePointer<TW_FRAME>
struct TW_MEMORY {
  var Flags: TW_UINT32
  var Length: TW_UINT32
  var TheMem: TW_MEMREF!
  init()
  init(Flags Flags: TW_UINT32, Length Length: TW_UINT32, TheMem TheMem: TW_MEMREF!)
}
typealias pTW_MEMORY = UnsafeMutablePointer<TW_MEMORY>
struct TW_TRANSFORMSTAGE {
  var Decode: (TW_DECODEFUNCTION, TW_DECODEFUNCTION, TW_DECODEFUNCTION)
  var Mix: ((TW_FIX32, TW_FIX32, TW_FIX32), (TW_FIX32, TW_FIX32, TW_FIX32), (TW_FIX32, TW_FIX32, TW_FIX32))
  init()
  init(Decode Decode: (TW_DECODEFUNCTION, TW_DECODEFUNCTION, TW_DECODEFUNCTION), Mix Mix: ((TW_FIX32, TW_FIX32, TW_FIX32), (TW_FIX32, TW_FIX32, TW_FIX32), (TW_FIX32, TW_FIX32, TW_FIX32)))
}
typealias pTW_TRANSFORMSTAGE = UnsafeMutablePointer<TW_TRANSFORMSTAGE>
struct TW_VERSION {
  var MajorNum: TW_UINT16
  var MinorNum: TW_UINT16
  var Language: TW_UINT16
  var Country: TW_UINT16
  var Info: TW_STR32
  init()
  init(MajorNum MajorNum: TW_UINT16, MinorNum MinorNum: TW_UINT16, Language Language: TW_UINT16, Country Country: TW_UINT16, Info Info: TW_STR32)
}
typealias pTW_VERSION = UnsafeMutablePointer<TW_VERSION>
struct TW_ARRAY {
  var ItemType: TW_UINT16
  var NumItems: TW_UINT32
  var ItemList: (TW_UINT8)
  init()
  init(ItemType ItemType: TW_UINT16, NumItems NumItems: TW_UINT32, ItemList ItemList: (TW_UINT8))
}
typealias pTW_ARRAY = UnsafeMutablePointer<TW_ARRAY>
struct TW_ENUMERATION {
  var ItemType: TW_UINT16
  var NumItems: TW_UINT32
  var CurrentIndex: TW_UINT32
  var DefaultIndex: TW_UINT32
  var ItemList: (TW_UINT8)
  init()
  init(ItemType ItemType: TW_UINT16, NumItems NumItems: TW_UINT32, CurrentIndex CurrentIndex: TW_UINT32, DefaultIndex DefaultIndex: TW_UINT32, ItemList ItemList: (TW_UINT8))
}
typealias pTW_ENUMERATION = UnsafeMutablePointer<TW_ENUMERATION>
struct TW_ONEVALUE {
  var ItemType: TW_UINT16
  var Item: TW_UINT32
  init()
  init(ItemType ItemType: TW_UINT16, Item Item: TW_UINT32)
}
typealias pTW_ONEVALUE = UnsafeMutablePointer<TW_ONEVALUE>
struct TW_RANGE {
  var ItemType: TW_UINT16
  var MinValue: TW_UINT32
  var MaxValue: TW_UINT32
  var StepSize: TW_UINT32
  var DefaultValue: TW_UINT32
  var CurrentValue: TW_UINT32
  init()
  init(ItemType ItemType: TW_UINT16, MinValue MinValue: TW_UINT32, MaxValue MaxValue: TW_UINT32, StepSize StepSize: TW_UINT32, DefaultValue DefaultValue: TW_UINT32, CurrentValue CurrentValue: TW_UINT32)
}
typealias pTW_RANGE = UnsafeMutablePointer<TW_RANGE>
struct TW_CAPABILITY {
  var Cap: TW_UINT16
  var ConType: TW_UINT16
  var hContainer: TW_HANDLE!
  init()
  init(Cap Cap: TW_UINT16, ConType ConType: TW_UINT16, hContainer hContainer: TW_HANDLE!)
}
typealias pTW_CAPABILITY = UnsafeMutablePointer<TW_CAPABILITY>
struct TW_CIECOLOR {
  var ColorSpace: TW_UINT16
  var LowEndian: TW_INT16
  var DeviceDependent: TW_INT16
  var VersionNumber: TW_INT32
  var StageABC: TW_TRANSFORMSTAGE
  var StageLMN: TW_TRANSFORMSTAGE
  var WhitePoint: TW_CIEPOINT
  var BlackPoint: TW_CIEPOINT
  var WhitePaper: TW_CIEPOINT
  var BlackInk: TW_CIEPOINT
  var Samples: (TW_FIX32)
  init()
  init(ColorSpace ColorSpace: TW_UINT16, LowEndian LowEndian: TW_INT16, DeviceDependent DeviceDependent: TW_INT16, VersionNumber VersionNumber: TW_INT32, StageABC StageABC: TW_TRANSFORMSTAGE, StageLMN StageLMN: TW_TRANSFORMSTAGE, WhitePoint WhitePoint: TW_CIEPOINT, BlackPoint BlackPoint: TW_CIEPOINT, WhitePaper WhitePaper: TW_CIEPOINT, BlackInk BlackInk: TW_CIEPOINT, Samples Samples: (TW_FIX32))
}
typealias pTW_CIECOLOR = UnsafeMutablePointer<TW_CIECOLOR>
struct TW_EVENT {
  var pEvent: TW_MEMREF!
  var TWMessage: TW_UINT16
  init()
  init(pEvent pEvent: TW_MEMREF!, TWMessage TWMessage: TW_UINT16)
}
typealias pTW_EVENT = UnsafeMutablePointer<TW_EVENT>
struct TW_GRAYRESPONSE {
  var Response: (TW_ELEMENT8)
  init()
  init(Response Response: (TW_ELEMENT8))
}
typealias pTW_GRAYRESPONSE = UnsafeMutablePointer<TW_GRAYRESPONSE>
struct TW_IDENTITY {
  var Id: TW_MEMREF!
  var Version: TW_VERSION
  var ProtocolMajor: TW_UINT16
  var ProtocolMinor: TW_UINT16
  var SupportedGroups: TW_UINT32
  var Manufacturer: TW_STR32
  var ProductFamily: TW_STR32
  var ProductName: TW_STR32
  init()
  init(Id Id: TW_MEMREF!, Version Version: TW_VERSION, ProtocolMajor ProtocolMajor: TW_UINT16, ProtocolMinor ProtocolMinor: TW_UINT16, SupportedGroups SupportedGroups: TW_UINT32, Manufacturer Manufacturer: TW_STR32, ProductFamily ProductFamily: TW_STR32, ProductName ProductName: TW_STR32)
}
typealias pTW_IDENTITY = UnsafeMutablePointer<TW_IDENTITY>
struct TW_IMAGEINFO {
  var XResolution: TW_FIX32
  var YResolution: TW_FIX32
  var ImageWidth: TW_INT32
  var ImageLength: TW_INT32
  var SamplesPerPixel: TW_INT16
  var BitsPerSample: (TW_INT16, TW_INT16, TW_INT16, TW_INT16, TW_INT16, TW_INT16, TW_INT16, TW_INT16)
  var BitsPerPixel: TW_INT16
  var Planar: TW_BOOL
  var PixelType: TW_INT16
  var Compression: TW_UINT16
  init()
  init(XResolution XResolution: TW_FIX32, YResolution YResolution: TW_FIX32, ImageWidth ImageWidth: TW_INT32, ImageLength ImageLength: TW_INT32, SamplesPerPixel SamplesPerPixel: TW_INT16, BitsPerSample BitsPerSample: (TW_INT16, TW_INT16, TW_INT16, TW_INT16, TW_INT16, TW_INT16, TW_INT16, TW_INT16), BitsPerPixel BitsPerPixel: TW_INT16, Planar Planar: TW_BOOL, PixelType PixelType: TW_INT16, Compression Compression: TW_UINT16)
}
typealias pTW_IMAGEINFO = UnsafeMutablePointer<TW_IMAGEINFO>
struct TW_IMAGELAYOUT {
  var Frame: TW_FRAME
  var DocumentNumber: TW_UINT32
  var PageNumber: TW_UINT32
  var FrameNumber: TW_UINT32
  init()
  init(Frame Frame: TW_FRAME, DocumentNumber DocumentNumber: TW_UINT32, PageNumber PageNumber: TW_UINT32, FrameNumber FrameNumber: TW_UINT32)
}
typealias pTW_IMAGELAYOUT = UnsafeMutablePointer<TW_IMAGELAYOUT>
struct TW_IMAGEMEMXFER {
  var Compression: TW_UINT16
  var BytesPerRow: TW_UINT32
  var Columns: TW_UINT32
  var Rows: TW_UINT32
  var XOffset: TW_UINT32
  var YOffset: TW_UINT32
  var BytesWritten: TW_UINT32
  var Memory: TW_MEMORY
  init()
  init(Compression Compression: TW_UINT16, BytesPerRow BytesPerRow: TW_UINT32, Columns Columns: TW_UINT32, Rows Rows: TW_UINT32, XOffset XOffset: TW_UINT32, YOffset YOffset: TW_UINT32, BytesWritten BytesWritten: TW_UINT32, Memory Memory: TW_MEMORY)
}
typealias pTW_IMAGEMEMXFER = UnsafeMutablePointer<TW_IMAGEMEMXFER>
struct TW_JPEGCOMPRESSION {
  var ColorSpace: TW_UINT16
  var SubSampling: TW_UINT32
  var NumComponents: TW_UINT16
  var RestartFrequency: TW_UINT16
  var QuantMap: (TW_UINT16, TW_UINT16, TW_UINT16, TW_UINT16)
  var QuantTable: (TW_MEMORY, TW_MEMORY, TW_MEMORY, TW_MEMORY)
  var HuffmanMap: (TW_UINT16, TW_UINT16, TW_UINT16, TW_UINT16)
  var HuffmanDC: (TW_MEMORY, TW_MEMORY)
  var HuffmanAC: (TW_MEMORY, TW_MEMORY)
  init()
  init(ColorSpace ColorSpace: TW_UINT16, SubSampling SubSampling: TW_UINT32, NumComponents NumComponents: TW_UINT16, RestartFrequency RestartFrequency: TW_UINT16, QuantMap QuantMap: (TW_UINT16, TW_UINT16, TW_UINT16, TW_UINT16), QuantTable QuantTable: (TW_MEMORY, TW_MEMORY, TW_MEMORY, TW_MEMORY), HuffmanMap HuffmanMap: (TW_UINT16, TW_UINT16, TW_UINT16, TW_UINT16), HuffmanDC HuffmanDC: (TW_MEMORY, TW_MEMORY), HuffmanAC HuffmanAC: (TW_MEMORY, TW_MEMORY))
}
typealias pTW_JPEGCOMPRESSION = UnsafeMutablePointer<TW_JPEGCOMPRESSION>
struct TW_PALETTE8 {
  var NumColors: TW_UINT16
  var PaletteType: TW_UINT16
  var Colors: (TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8)
  init()
  init(NumColors NumColors: TW_UINT16, PaletteType PaletteType: TW_UINT16, Colors Colors: (TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8, TW_ELEMENT8))
}
typealias pTW_PALETTE8 = UnsafeMutablePointer<TW_PALETTE8>
struct TW_PENDINGXFERS {
  struct __Unnamed_union_TW_JOBCONTROL {
    var EOJ: TW_UINT32
    var Reserved: TW_UINT32
    init(EOJ EOJ: TW_UINT32)
    init(Reserved Reserved: TW_UINT32)
    init()
  }
  var Count: TW_UINT16
  var TW_JOBCONTROL: TW_PENDINGXFERS.__Unnamed_union_TW_JOBCONTROL
  init()
  init(Count Count: TW_UINT16, TW_JOBCONTROL TW_JOBCONTROL: TW_PENDINGXFERS.__Unnamed_union_TW_JOBCONTROL)
}
typealias pTW_PENDINGXFERS = UnsafeMutablePointer<TW_PENDINGXFERS>
struct TW_RGBRESPONSE {
  var Response: (TW_ELEMENT8)
  init()
  init(Response Response: (TW_ELEMENT8))
}
typealias pTW_RGBRESPONSE = UnsafeMutablePointer<TW_RGBRESPONSE>
struct TW_SETUPFILEXFER {
  var FileName: TW_STR255
  var Format: TW_UINT16
  var VRefNum: TW_INT16
  init()
  init(FileName FileName: TW_STR255, Format Format: TW_UINT16, VRefNum VRefNum: TW_INT16)
}
typealias pTW_SETUPFILEXFER = UnsafeMutablePointer<TW_SETUPFILEXFER>
struct TW_SETUPFILEXFER2 {
  var FileName: TW_MEMREF!
  var FileNameType: TW_UINT16
  var Format: TW_UINT16
  var VRefNum: TW_INT16
  var ParID: TW_UINT32
  init()
  init(FileName FileName: TW_MEMREF!, FileNameType FileNameType: TW_UINT16, Format Format: TW_UINT16, VRefNum VRefNum: TW_INT16, ParID ParID: TW_UINT32)
}
typealias pTW_SETUPFILEXFER2 = UnsafeMutablePointer<TW_SETUPFILEXFER2>
struct TW_SETUPMEMXFER {
  var MinBufSize: TW_UINT32
  var MaxBufSize: TW_UINT32
  var Preferred: TW_UINT32
  init()
  init(MinBufSize MinBufSize: TW_UINT32, MaxBufSize MaxBufSize: TW_UINT32, Preferred Preferred: TW_UINT32)
}
typealias pTW_SETUPMEMXFER = UnsafeMutablePointer<TW_SETUPMEMXFER>
struct TW_STATUS {
  var ConditionCode: TW_UINT16
  var Reserved: TW_UINT16
  init()
  init(ConditionCode ConditionCode: TW_UINT16, Reserved Reserved: TW_UINT16)
}
typealias pTW_STATUS = UnsafeMutablePointer<TW_STATUS>
struct TW_USERINTERFACE {
  var ShowUI: TW_BOOL
  var ModalUI: TW_BOOL
  var hParent: TW_HANDLE!
  init()
  init(ShowUI ShowUI: TW_BOOL, ModalUI ModalUI: TW_BOOL, hParent hParent: TW_HANDLE!)
}
typealias pTW_USERINTERFACE = UnsafeMutablePointer<TW_USERINTERFACE>
struct TW_TWUNKIDENTITY {
  var identity: TW_IDENTITY
  var dsPath: TW_STR255
  init()
  init(identity identity: TW_IDENTITY, dsPath dsPath: TW_STR255)
}
typealias pTW_TWUNKIDENTITY = UnsafeMutablePointer<TW_TWUNKIDENTITY>
struct TW_TWUNKDSENTRYPARAMS {
  var destFlag: TW_INT8
  var dest: TW_IDENTITY
  var dataGroup: TW_INT32
  var dataArgType: TW_INT16
  var message: TW_INT16
  var pDataSize: TW_INT32
  init()
  init(destFlag destFlag: TW_INT8, dest dest: TW_IDENTITY, dataGroup dataGroup: TW_INT32, dataArgType dataArgType: TW_INT16, message message: TW_INT16, pDataSize pDataSize: TW_INT32)
}
typealias pTW_TWUNKDSENTRYPARAMS = UnsafeMutablePointer<TW_TWUNKDSENTRYPARAMS>
struct TW_TWUNKDSENTRYRETURN {
  var returnCode: TW_UINT16
  var conditionCode: TW_UINT16
  var pDataSize: TW_INT32
  init()
  init(returnCode returnCode: TW_UINT16, conditionCode conditionCode: TW_UINT16, pDataSize pDataSize: TW_INT32)
}
typealias pTW_TWUNKDSENTRYRETURN = UnsafeMutablePointer<TW_TWUNKDSENTRYRETURN>
struct TW_CAPEXT {
  var Cap: TW_UINT16
  var Properties: TW_UINT16
  init()
  init(Cap Cap: TW_UINT16, Properties Properties: TW_UINT16)
}
typealias pTW_CAPEXT = UnsafeMutablePointer<TW_CAPEXT>
struct TW_CUSTOMDSDATA {
  var InfoLength: TW_UINT32
  var hData: TW_HANDLE!
  init()
  init(InfoLength InfoLength: TW_UINT32, hData hData: TW_HANDLE!)
}
typealias pTW_CUSTOMDSDATA = UnsafeMutablePointer<TW_CUSTOMDSDATA>
struct TW_INFO {
  var InfoID: TW_UINT16
  var ItemType: TW_UINT16
  var NumItems: TW_UINT16
  var CondCode: TW_UINT16
  var Item: TW_UINT32
  init()
  init(InfoID InfoID: TW_UINT16, ItemType ItemType: TW_UINT16, NumItems NumItems: TW_UINT16, CondCode CondCode: TW_UINT16, Item Item: TW_UINT32)
}
typealias pTW_INFO = UnsafeMutablePointer<TW_INFO>
struct TW_EXTIMAGEINFO {
  var NumInfos: TW_UINT32
  var Info: (TW_INFO)
  init()
  init(NumInfos NumInfos: TW_UINT32, Info Info: (TW_INFO))
}
typealias pTW_EXTIMAGEINFO = UnsafeMutablePointer<TW_EXTIMAGEINFO>
struct TW_AUDIOINFO {
  var Name: TW_STR255
  var Reserved: TW_UINT32
  init()
  init(Name Name: TW_STR255, Reserved Reserved: TW_UINT32)
}
typealias pTW_AUDIOINFO = UnsafeMutablePointer<TW_AUDIOINFO>
struct TW_DEVICEEVENT {
  var Event: TW_UINT32
  var DeviceName: TW_STR255
  var BatteryMinutes: TW_UINT32
  var BatteryPercentage: TW_INT16
  var PowerSupply: TW_INT32
  var XResolution: TW_FIX32
  var YResolution: TW_FIX32
  var FlashUsed2: TW_UINT32
  var AutomaticCapture: TW_UINT32
  var TimeBeforeFirstCapture: TW_UINT32
  var TimeBetweenCaptures: TW_UINT32
  init()
  init(Event Event: TW_UINT32, DeviceName DeviceName: TW_STR255, BatteryMinutes BatteryMinutes: TW_UINT32, BatteryPercentage BatteryPercentage: TW_INT16, PowerSupply PowerSupply: TW_INT32, XResolution XResolution: TW_FIX32, YResolution YResolution: TW_FIX32, FlashUsed2 FlashUsed2: TW_UINT32, AutomaticCapture AutomaticCapture: TW_UINT32, TimeBeforeFirstCapture TimeBeforeFirstCapture: TW_UINT32, TimeBetweenCaptures TimeBetweenCaptures: TW_UINT32)
}
typealias pTW_DEVICEEVENT = UnsafeMutablePointer<TW_DEVICEEVENT>
struct TW_FILESYSTEM {
  var InputName: TW_STR255
  var OutputName: TW_STR255
  var Context: TW_MEMREF!
  var Recursive: Int32
  var FileType: TW_INT32
  var Size: TW_UINT32
  var CreateTimeDate: TW_STR32
  var ModifiedTimeDate: TW_STR32
  var FreeSpace: TW_UINT32
  var NewImageSize: TW_INT32
  var NumberOfFiles: TW_UINT32
  var NumberOfSnippets: TW_UINT32
  var DeviceGroupMask: TW_UINT32
  var Reserved: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
  init()
  init(InputName InputName: TW_STR255, OutputName OutputName: TW_STR255, Context Context: TW_MEMREF!, Recursive Recursive: Int32, FileType FileType: TW_INT32, Size Size: TW_UINT32, CreateTimeDate CreateTimeDate: TW_STR32, ModifiedTimeDate ModifiedTimeDate: TW_STR32, FreeSpace FreeSpace: TW_UINT32, NewImageSize NewImageSize: TW_INT32, NumberOfFiles NumberOfFiles: TW_UINT32, NumberOfSnippets NumberOfSnippets: TW_UINT32, DeviceGroupMask DeviceGroupMask: TW_UINT32, Reserved Reserved: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8))
}
typealias pTW_FILESYSTEM = UnsafeMutablePointer<TW_FILESYSTEM>
struct TW_PASSTHRU {
  var pCommand: TW_MEMREF!
  var CommandBytes: TW_UINT32
  var Direction: TW_INT32
  var pData: TW_MEMREF!
  var DataBytes: TW_UINT32
  var DataBytesXfered: TW_UINT32
  init()
  init(pCommand pCommand: TW_MEMREF!, CommandBytes CommandBytes: TW_UINT32, Direction Direction: TW_INT32, pData pData: TW_MEMREF!, DataBytes DataBytes: TW_UINT32, DataBytesXfered DataBytesXfered: TW_UINT32)
}
typealias pTW_PASSTHRU = UnsafeMutablePointer<TW_PASSTHRU>
struct TW_SETUPAUDIOFILEXFER {
  var FileName: TW_STR255
  var Format: TW_UINT16
  var VRefNum: TW_INT16
  init()
  init(FileName FileName: TW_STR255, Format Format: TW_UINT16, VRefNum VRefNum: TW_INT16)
}
typealias pTW_SETUPAUDIOFILEXFER = UnsafeMutablePointer<TW_SETUPAUDIOFILEXFER>
struct TW_CALLBACK {
  var CallBackProc: TW_MEMREF!
  var RefCon: TW_MEMREF!
  var Message: TW_INT16
  init()
  init(CallBackProc CallBackProc: TW_MEMREF!, RefCon RefCon: TW_MEMREF!, Message Message: TW_INT16)
}
typealias pTW_CALLBACK = UnsafeMutablePointer<TW_CALLBACK>
var TWON_ARRAY: Int32 { get }
var TWON_ENUMERATION: Int32 { get }
var TWON_ONEVALUE: Int32 { get }
var TWON_RANGE: Int32 { get }
var TWON_ICONID: Int32 { get }
var TWON_DSMID: Int32 { get }
var TWON_DSMCODEID: Int32 { get }
var TWON_DONTCARE8: Int32 { get }
var TWON_DONTCARE16: Int32 { get }
var TWON_DONTCARE32: UInt32 { get }
var TWMF_APPOWNS: Int32 { get }
var TWMF_DSMOWNS: Int32 { get }
var TWMF_DSOWNS: Int32 { get }
var TWMF_POINTER: Int32 { get }
var TWMF_HANDLE: Int32 { get }
var TWPA_RGB: Int32 { get }
var TWPA_GRAY: Int32 { get }
var TWPA_CMY: Int32 { get }
var TWTY_INT8: Int32 { get }
var TWTY_INT16: Int32 { get }
var TWTY_INT32: Int32 { get }
var TWTY_UINT8: Int32 { get }
var TWTY_UINT16: Int32 { get }
var TWTY_UINT32: Int32 { get }
var TWTY_BOOL: Int32 { get }
var TWTY_FIX32: Int32 { get }
var TWTY_FRAME: Int32 { get }
var TWTY_STR32: Int32 { get }
var TWTY_STR64: Int32 { get }
var TWTY_STR128: Int32 { get }
var TWTY_STR255: Int32 { get }
var TWTY_STR1024: Int32 { get }
var TWTY_UNI512: Int32 { get }
var TWBO_LSBFIRST: Int32 { get }
var TWBO_MSBFIRST: Int32 { get }
var TWCP_NONE: Int32 { get }
var TWCP_PACKBITS: Int32 { get }
var TWCP_GROUP31D: Int32 { get }
var TWCP_GROUP31DEOL: Int32 { get }
var TWCP_GROUP32D: Int32 { get }
var TWCP_GROUP4: Int32 { get }
var TWCP_JPEG: Int32 { get }
var TWCP_LZW: Int32 { get }
var TWCP_JBIG: Int32 { get }
var TWCP_PNG: Int32 { get }
var TWCP_RLE4: Int32 { get }
var TWCP_RLE8: Int32 { get }
var TWCP_BITFIELDS: Int32 { get }
var TWFF_TIFF: Int32 { get }
var TWFF_PICT: Int32 { get }
var TWFF_BMP: Int32 { get }
var TWFF_XBM: Int32 { get }
var TWFF_JFIF: Int32 { get }
var TWFF_FPX: Int32 { get }
var TWFF_TIFFMULTI: Int32 { get }
var TWFF_PNG: Int32 { get }
var TWFF_SPIFF: Int32 { get }
var TWFF_EXIF: Int32 { get }
var TWFT_RED: Int32 { get }
var TWFT_GREEN: Int32 { get }
var TWFT_BLUE: Int32 { get }
var TWFT_NONE: Int32 { get }
var TWFT_WHITE: Int32 { get }
var TWFT_CYAN: Int32 { get }
var TWFT_MAGENTA: Int32 { get }
var TWFT_YELLOW: Int32 { get }
var TWFT_BLACK: Int32 { get }
var TWLP_REFLECTIVE: Int32 { get }
var TWLP_TRANSMISSIVE: Int32 { get }
var TWLS_RED: Int32 { get }
var TWLS_GREEN: Int32 { get }
var TWLS_BLUE: Int32 { get }
var TWLS_NONE: Int32 { get }
var TWLS_WHITE: Int32 { get }
var TWLS_UV: Int32 { get }
var TWLS_IR: Int32 { get }
var TWOR_ROT0: Int32 { get }
var TWOR_ROT90: Int32 { get }
var TWOR_ROT180: Int32 { get }
var TWOR_ROT270: Int32 { get }
var TWOR_PORTRAIT: Int32 { get }
var TWOR_LANDSCAPE: Int32 { get }
var TWPC_CHUNKY: Int32 { get }
var TWPC_PLANAR: Int32 { get }
var TWPF_CHOCOLATE: Int32 { get }
var TWPF_VANILLA: Int32 { get }
var TWPT_BW: Int32 { get }
var TWPT_GRAY: Int32 { get }
var TWPT_RGB: Int32 { get }
var TWPT_PALETTE: Int32 { get }
var TWPT_CMY: Int32 { get }
var TWPT_CMYK: Int32 { get }
var TWPT_YUV: Int32 { get }
var TWPT_YUVK: Int32 { get }
var TWPT_CIEXYZ: Int32 { get }
var TWSS_NONE: Int32 { get }
var TWSS_A4LETTER: Int32 { get }
var TWSS_B5LETTER: Int32 { get }
var TWSS_USLETTER: Int32 { get }
var TWSS_USLEGAL: Int32 { get }
var TWSS_A5: Int32 { get }
var TWSS_B4: Int32 { get }
var TWSS_B6: Int32 { get }
var TWSS_USLEDGER: Int32 { get }
var TWSS_USEXECUTIVE: Int32 { get }
var TWSS_A3: Int32 { get }
var TWSS_B3: Int32 { get }
var TWSS_A6: Int32 { get }
var TWSS_C4: Int32 { get }
var TWSS_C5: Int32 { get }
var TWSS_C6: Int32 { get }
var TWSS_4A0: Int32 { get }
var TWSS_2A0: Int32 { get }
var TWSS_A0: Int32 { get }
var TWSS_A1: Int32 { get }
var TWSS_A2: Int32 { get }
var TWSS_A4: Int32 { get }
var TWSS_A7: Int32 { get }
var TWSS_A8: Int32 { get }
var TWSS_A9: Int32 { get }
var TWSS_A10: Int32 { get }
var TWSS_ISOB0: Int32 { get }
var TWSS_ISOB1: Int32 { get }
var TWSS_ISOB2: Int32 { get }
var TWSS_ISOB3: Int32 { get }
var TWSS_ISOB4: Int32 { get }
var TWSS_ISOB5: Int32 { get }
var TWSS_ISOB6: Int32 { get }
var TWSS_ISOB7: Int32 { get }
var TWSS_ISOB8: Int32 { get }
var TWSS_ISOB9: Int32 { get }
var TWSS_ISOB10: Int32 { get }
var TWSS_JISB0: Int32 { get }
var TWSS_JISB1: Int32 { get }
var TWSS_JISB2: Int32 { get }
var TWSS_JISB3: Int32 { get }
var TWSS_JISB4: Int32 { get }
var TWSS_JISB5: Int32 { get }
var TWSS_JISB6: Int32 { get }
var TWSS_JISB7: Int32 { get }
var TWSS_JISB8: Int32 { get }
var TWSS_JISB9: Int32 { get }
var TWSS_JISB10: Int32 { get }
var TWSS_C0: Int32 { get }
var TWSS_C1: Int32 { get }
var TWSS_C2: Int32 { get }
var TWSS_C3: Int32 { get }
var TWSS_C7: Int32 { get }
var TWSS_C8: Int32 { get }
var TWSS_C9: Int32 { get }
var TWSS_C10: Int32 { get }
var TWSS_USSTATEMENT: Int32 { get }
var TWSS_BUSINESSCARD: Int32 { get }
var TWSX_NATIVE: Int32 { get }
var TWSX_FILE: Int32 { get }
var TWSX_MEMORY: Int32 { get }
var TWSX_FILE2: Int32 { get }
var TWUN_INCHES: Int32 { get }
var TWUN_CENTIMETERS: Int32 { get }
var TWUN_PICAS: Int32 { get }
var TWUN_POINTS: Int32 { get }
var TWUN_TWIPS: Int32 { get }
var TWUN_PIXELS: Int32 { get }
var TWBR_THRESHOLD: Int32 { get }
var TWBR_HALFTONE: Int32 { get }
var TWBR_CUSTHALFTONE: Int32 { get }
var TWBR_DIFFUSION: Int32 { get }
var TWDX_NONE: Int32 { get }
var TWDX_1PASSDUPLEX: Int32 { get }
var TWDX_2PASSDUPLEX: Int32 { get }
var TWBT_3OF9: Int32 { get }
var TWBT_2OF5INTERLEAVED: Int32 { get }
var TWBT_2OF5NONINTERLEAVED: Int32 { get }
var TWBT_CODE93: Int32 { get }
var TWBT_CODE128: Int32 { get }
var TWBT_UCC128: Int32 { get }
var TWBT_CODABAR: Int32 { get }
var TWBT_UPCA: Int32 { get }
var TWBT_UPCE: Int32 { get }
var TWBT_EAN8: Int32 { get }
var TWBT_EAN13: Int32 { get }
var TWBT_POSTNET: Int32 { get }
var TWBT_PDF417: Int32 { get }
var TWBT_2OF5INDUSTRIAL: Int32 { get }
var TWBT_2OF5MATRIX: Int32 { get }
var TWBT_2OF5DATALOGIC: Int32 { get }
var TWBT_2OF5IATA: Int32 { get }
var TWBT_3OF9FULLASCII: Int32 { get }
var TWBT_CODABARWITHSTARTSTOP: Int32 { get }
var TWBT_MAXICODE: Int32 { get }
var TWDSK_SUCCESS: Int32 { get }
var TWDSK_REPORTONLY: Int32 { get }
var TWDSK_FAIL: Int32 { get }
var TWDSK_DISABLED: Int32 { get }
var TWPCH_PATCH1: Int32 { get }
var TWPCH_PATCH2: Int32 { get }
var TWPCH_PATCH3: Int32 { get }
var TWPCH_PATCH4: Int32 { get }
var TWPCH_PATCH6: Int32 { get }
var TWPCH_PATCHT: Int32 { get }
var TWJC_NONE: Int32 { get }
var TWJC_JSIC: Int32 { get }
var TWJC_JSIS: Int32 { get }
var TWJC_JSXC: Int32 { get }
var TWJC_JSXS: Int32 { get }
var TWBCOR_ROT0: Int32 { get }
var TWBCOR_ROT90: Int32 { get }
var TWBCOR_ROT180: Int32 { get }
var TWBCOR_ROT270: Int32 { get }
var TWBCOR_ROTX: Int32 { get }
var TWAF_WAV: Int32 { get }
var TWAF_AIFF: Int32 { get }
var TWAF_AU: Int32 { get }
var TWAF_SND: Int32 { get }
var TWAL_ALARM: Int32 { get }
var TWAL_FEEDERERROR: Int32 { get }
var TWAL_FEEDERWARNING: Int32 { get }
var TWAL_BARCODE: Int32 { get }
var TWAL_DOUBLEFEED: Int32 { get }
var TWAL_JAM: Int32 { get }
var TWAL_PATCHCODE: Int32 { get }
var TWAL_POWER: Int32 { get }
var TWAL_SKEW: Int32 { get }
var TWCB_AUTO: Int32 { get }
var TWCB_CLEAR: Int32 { get }
var TWCB_NOCLEAR: Int32 { get }
var TWDE_CUSTOMEVENTS: Int32 { get }
var TWDE_CHECKAUTOMATICCAPTURE: Int32 { get }
var TWDE_CHECKBATTERY: Int32 { get }
var TWDE_CHECKDEVICEONLINE: Int32 { get }
var TWDE_CHECKFLASH: Int32 { get }
var TWDE_CHECKPOWERSUPPLY: Int32 { get }
var TWDE_CHECKRESOLUTION: Int32 { get }
var TWDE_DEVICEADDED: Int32 { get }
var TWDE_DEVICEOFFLINE: Int32 { get }
var TWDE_DEVICEREADY: Int32 { get }
var TWDE_DEVICEREMOVED: Int32 { get }
var TWDE_IMAGECAPTURED: Int32 { get }
var TWDE_IMAGEDELETED: Int32 { get }
var TWDE_PAPERDOUBLEFEED: Int32 { get }
var TWDE_PAPERJAM: Int32 { get }
var TWDE_LAMPFAILURE: Int32 { get }
var TWDE_POWERSAVE: Int32 { get }
var TWDE_POWERSAVENOTIFY: Int32 { get }
var TWFA_NONE: Int32 { get }
var TWFA_LEFT: Int32 { get }
var TWFA_CENTER: Int32 { get }
var TWFA_RIGHT: Int32 { get }
var TWFO_FIRSTPAGEFIRST: Int32 { get }
var TWFO_LASTPAGEFIRST: Int32 { get }
var TWFS_FILESYSTEM: Int32 { get }
var TWFS_RECURSIVEDELETE: Int32 { get }
var TWPS_EXTERNAL: Int32 { get }
var TWPS_BATTERY: Int32 { get }
var TWPR_IMPRINTERTOPBEFORE: Int32 { get }
var TWPR_IMPRINTERTOPAFTER: Int32 { get }
var TWPR_IMPRINTERBOTTOMBEFORE: Int32 { get }
var TWPR_IMPRINTERBOTTOMAFTER: Int32 { get }
var TWPR_ENDORSERTOPBEFORE: Int32 { get }
var TWPR_ENDORSERTOPAFTER: Int32 { get }
var TWPR_ENDORSERBOTTOMBEFORE: Int32 { get }
var TWPR_ENDORSERBOTTOMAFTER: Int32 { get }
var TWPM_SINGLESTRING: Int32 { get }
var TWPM_MULTISTRING: Int32 { get }
var TWPM_COMPOUNDSTRING: Int32 { get }
var TWBD_HORZ: Int32 { get }
var TWBD_VERT: Int32 { get }
var TWBD_HORZVERT: Int32 { get }
var TWBD_VERTHORZ: Int32 { get }
var TWFL_NONE: Int32 { get }
var TWFL_OFF: Int32 { get }
var TWFL_ON: Int32 { get }
var TWFL_AUTO: Int32 { get }
var TWFL_REDEYE: Int32 { get }
var TWFR_BOOK: Int32 { get }
var TWFR_FANFOLD: Int32 { get }
var TWIF_NONE: Int32 { get }
var TWIF_AUTO: Int32 { get }
var TWIF_LOWPASS: Int32 { get }
var TWIF_BANDPASS: Int32 { get }
var TWIF_HIGHPASS: Int32 { get }
var TWIF_TEXT: Int32 { get }
var TWIF_FINELINE: Int32 { get }
var TWNF_NONE: Int32 { get }
var TWNF_AUTO: Int32 { get }
var TWNF_LONEPIXEL: Int32 { get }
var TWNF_MAJORITYRULE: Int32 { get }
var TWOV_NONE: Int32 { get }
var TWOV_AUTO: Int32 { get }
var TWOV_TOPBOTTOM: Int32 { get }
var TWOV_LEFTRIGHT: Int32 { get }
var TWOV_ALL: Int32 { get }
var TWFY_CAMERA: Int32 { get }
var TWFY_CAMERATOP: Int32 { get }
var TWFY_CAMERABOTTOM: Int32 { get }
var TWFY_CAMERAPREVIEW: Int32 { get }
var TWFY_DOMAIN: Int32 { get }
var TWFY_HOST: Int32 { get }
var TWFY_DIRECTORY: Int32 { get }
var TWFY_IMAGE: Int32 { get }
var TWFY_UNKNOWN: Int32 { get }
var TWJQ_UNKNOWN: Int32 { get }
var TWJQ_LOW: Int32 { get }
var TWJQ_MEDIUM: Int32 { get }
var TWJQ_HIGH: Int32 { get }
var TWCY_AFGHANISTAN: Int32 { get }
var TWCY_ALGERIA: Int32 { get }
var TWCY_AMERICANSAMOA: Int32 { get }
var TWCY_ANDORRA: Int32 { get }
var TWCY_ANGOLA: Int32 { get }
var TWCY_ANGUILLA: Int32 { get }
var TWCY_ANTIGUA: Int32 { get }
var TWCY_ARGENTINA: Int32 { get }
var TWCY_ARUBA: Int32 { get }
var TWCY_ASCENSIONI: Int32 { get }
var TWCY_AUSTRALIA: Int32 { get }
var TWCY_AUSTRIA: Int32 { get }
var TWCY_BAHAMAS: Int32 { get }
var TWCY_BAHRAIN: Int32 { get }
var TWCY_BANGLADESH: Int32 { get }
var TWCY_BARBADOS: Int32 { get }
var TWCY_BELGIUM: Int32 { get }
var TWCY_BELIZE: Int32 { get }
var TWCY_BENIN: Int32 { get }
var TWCY_BERMUDA: Int32 { get }
var TWCY_BHUTAN: Int32 { get }
var TWCY_BOLIVIA: Int32 { get }
var TWCY_BOTSWANA: Int32 { get }
var TWCY_BRITAIN: Int32 { get }
var TWCY_BRITVIRGINIS: Int32 { get }
var TWCY_BRAZIL: Int32 { get }
var TWCY_BRUNEI: Int32 { get }
var TWCY_BULGARIA: Int32 { get }
var TWCY_BURKINAFASO: Int32 { get }
var TWCY_BURMA: Int32 { get }
var TWCY_BURUNDI: Int32 { get }
var TWCY_CAMAROON: Int32 { get }
var TWCY_CANADA: Int32 { get }
var TWCY_CAPEVERDEIS: Int32 { get }
var TWCY_CAYMANIS: Int32 { get }
var TWCY_CENTRALAFREP: Int32 { get }
var TWCY_CHAD: Int32 { get }
var TWCY_CHILE: Int32 { get }
var TWCY_CHINA: Int32 { get }
var TWCY_CHRISTMASIS: Int32 { get }
var TWCY_COCOSIS: Int32 { get }
var TWCY_COLOMBIA: Int32 { get }
var TWCY_COMOROS: Int32 { get }
var TWCY_CONGO: Int32 { get }
var TWCY_COOKIS: Int32 { get }
var TWCY_COSTARICA: Int32 { get }
var TWCY_CUBA: Int32 { get }
var TWCY_CYPRUS: Int32 { get }
var TWCY_CZECHOSLOVAKIA: Int32 { get }
var TWCY_DENMARK: Int32 { get }
var TWCY_DJIBOUTI: Int32 { get }
var TWCY_DOMINICA: Int32 { get }
var TWCY_DOMINCANREP: Int32 { get }
var TWCY_EASTERIS: Int32 { get }
var TWCY_ECUADOR: Int32 { get }
var TWCY_EGYPT: Int32 { get }
var TWCY_ELSALVADOR: Int32 { get }
var TWCY_EQGUINEA: Int32 { get }
var TWCY_ETHIOPIA: Int32 { get }
var TWCY_FALKLANDIS: Int32 { get }
var TWCY_FAEROEIS: Int32 { get }
var TWCY_FIJIISLANDS: Int32 { get }
var TWCY_FINLAND: Int32 { get }
var TWCY_FRANCE: Int32 { get }
var TWCY_FRANTILLES: Int32 { get }
var TWCY_FRGUIANA: Int32 { get }
var TWCY_FRPOLYNEISA: Int32 { get }
var TWCY_FUTANAIS: Int32 { get }
var TWCY_GABON: Int32 { get }
var TWCY_GAMBIA: Int32 { get }
var TWCY_GERMANY: Int32 { get }
var TWCY_GHANA: Int32 { get }
var TWCY_GIBRALTER: Int32 { get }
var TWCY_GREECE: Int32 { get }
var TWCY_GREENLAND: Int32 { get }
var TWCY_GRENADA: Int32 { get }
var TWCY_GRENEDINES: Int32 { get }
var TWCY_GUADELOUPE: Int32 { get }
var TWCY_GUAM: Int32 { get }
var TWCY_GUANTANAMOBAY: Int32 { get }
var TWCY_GUATEMALA: Int32 { get }
var TWCY_GUINEA: Int32 { get }
var TWCY_GUINEABISSAU: Int32 { get }
var TWCY_GUYANA: Int32 { get }
var TWCY_HAITI: Int32 { get }
var TWCY_HONDURAS: Int32 { get }
var TWCY_HONGKONG: Int32 { get }
var TWCY_HUNGARY: Int32 { get }
var TWCY_ICELAND: Int32 { get }
var TWCY_INDIA: Int32 { get }
var TWCY_INDONESIA: Int32 { get }
var TWCY_IRAN: Int32 { get }
var TWCY_IRAQ: Int32 { get }
var TWCY_IRELAND: Int32 { get }
var TWCY_ISRAEL: Int32 { get }
var TWCY_ITALY: Int32 { get }
var TWCY_IVORYCOAST: Int32 { get }
var TWCY_JAMAICA: Int32 { get }
var TWCY_JAPAN: Int32 { get }
var TWCY_JORDAN: Int32 { get }
var TWCY_KENYA: Int32 { get }
var TWCY_KIRIBATI: Int32 { get }
var TWCY_KOREA: Int32 { get }
var TWCY_KUWAIT: Int32 { get }
var TWCY_LAOS: Int32 { get }
var TWCY_LEBANON: Int32 { get }
var TWCY_LIBERIA: Int32 { get }
var TWCY_LIBYA: Int32 { get }
var TWCY_LIECHTENSTEIN: Int32 { get }
var TWCY_LUXENBOURG: Int32 { get }
var TWCY_MACAO: Int32 { get }
var TWCY_MADAGASCAR: Int32 { get }
var TWCY_MALAWI: Int32 { get }
var TWCY_MALAYSIA: Int32 { get }
var TWCY_MALDIVES: Int32 { get }
var TWCY_MALI: Int32 { get }
var TWCY_MALTA: Int32 { get }
var TWCY_MARSHALLIS: Int32 { get }
var TWCY_MAURITANIA: Int32 { get }
var TWCY_MAURITIUS: Int32 { get }
var TWCY_MEXICO: Int32 { get }
var TWCY_MICRONESIA: Int32 { get }
var TWCY_MIQUELON: Int32 { get }
var TWCY_MONACO: Int32 { get }
var TWCY_MONGOLIA: Int32 { get }
var TWCY_MONTSERRAT: Int32 { get }
var TWCY_MOROCCO: Int32 { get }
var TWCY_MOZAMBIQUE: Int32 { get }
var TWCY_NAMIBIA: Int32 { get }
var TWCY_NAURU: Int32 { get }
var TWCY_NEPAL: Int32 { get }
var TWCY_NETHERLANDS: Int32 { get }
var TWCY_NETHANTILLES: Int32 { get }
var TWCY_NEVIS: Int32 { get }
var TWCY_NEWCALEDONIA: Int32 { get }
var TWCY_NEWZEALAND: Int32 { get }
var TWCY_NICARAGUA: Int32 { get }
var TWCY_NIGER: Int32 { get }
var TWCY_NIGERIA: Int32 { get }
var TWCY_NIUE: Int32 { get }
var TWCY_NORFOLKI: Int32 { get }
var TWCY_NORWAY: Int32 { get }
var TWCY_OMAN: Int32 { get }
var TWCY_PAKISTAN: Int32 { get }
var TWCY_PALAU: Int32 { get }
var TWCY_PANAMA: Int32 { get }
var TWCY_PARAGUAY: Int32 { get }
var TWCY_PERU: Int32 { get }
var TWCY_PHILLIPPINES: Int32 { get }
var TWCY_PITCAIRNIS: Int32 { get }
var TWCY_PNEWGUINEA: Int32 { get }
var TWCY_POLAND: Int32 { get }
var TWCY_PORTUGAL: Int32 { get }
var TWCY_QATAR: Int32 { get }
var TWCY_REUNIONI: Int32 { get }
var TWCY_ROMANIA: Int32 { get }
var TWCY_RWANDA: Int32 { get }
var TWCY_SAIPAN: Int32 { get }
var TWCY_SANMARINO: Int32 { get }
var TWCY_SAOTOME: Int32 { get }
var TWCY_SAUDIARABIA: Int32 { get }
var TWCY_SENEGAL: Int32 { get }
var TWCY_SEYCHELLESIS: Int32 { get }
var TWCY_SIERRALEONE: Int32 { get }
var TWCY_SINGAPORE: Int32 { get }
var TWCY_SOLOMONIS: Int32 { get }
var TWCY_SOMALI: Int32 { get }
var TWCY_SOUTHAFRICA: Int32 { get }
var TWCY_SPAIN: Int32 { get }
var TWCY_SRILANKA: Int32 { get }
var TWCY_STHELENA: Int32 { get }
var TWCY_STKITTS: Int32 { get }
var TWCY_STLUCIA: Int32 { get }
var TWCY_STPIERRE: Int32 { get }
var TWCY_STVINCENT: Int32 { get }
var TWCY_SUDAN: Int32 { get }
var TWCY_SURINAME: Int32 { get }
var TWCY_SWAZILAND: Int32 { get }
var TWCY_SWEDEN: Int32 { get }
var TWCY_SWITZERLAND: Int32 { get }
var TWCY_SYRIA: Int32 { get }
var TWCY_TAIWAN: Int32 { get }
var TWCY_TANZANIA: Int32 { get }
var TWCY_THAILAND: Int32 { get }
var TWCY_TOBAGO: Int32 { get }
var TWCY_TOGO: Int32 { get }
var TWCY_TONGAIS: Int32 { get }
var TWCY_TRINIDAD: Int32 { get }
var TWCY_TUNISIA: Int32 { get }
var TWCY_TURKEY: Int32 { get }
var TWCY_TURKSCAICOS: Int32 { get }
var TWCY_TUVALU: Int32 { get }
var TWCY_UGANDA: Int32 { get }
var TWCY_USSR: Int32 { get }
var TWCY_UAEMIRATES: Int32 { get }
var TWCY_UNITEDKINGDOM: Int32 { get }
var TWCY_USA: Int32 { get }
var TWCY_URUGUAY: Int32 { get }
var TWCY_VANUATU: Int32 { get }
var TWCY_VATICANCITY: Int32 { get }
var TWCY_VENEZUELA: Int32 { get }
var TWCY_WAKE: Int32 { get }
var TWCY_WALLISIS: Int32 { get }
var TWCY_WESTERNSAHARA: Int32 { get }
var TWCY_WESTERNSAMOA: Int32 { get }
var TWCY_YEMEN: Int32 { get }
var TWCY_YUGOSLAVIA: Int32 { get }
var TWCY_ZAIRE: Int32 { get }
var TWCY_ZAMBIA: Int32 { get }
var TWCY_ZIMBABWE: Int32 { get }
var TWCY_ALBANIA: Int32 { get }
var TWCY_ARMENIA: Int32 { get }
var TWCY_AZERBAIJAN: Int32 { get }
var TWCY_BELARUS: Int32 { get }
var TWCY_BOSNIAHERZGO: Int32 { get }
var TWCY_CAMBODIA: Int32 { get }
var TWCY_CROATIA: Int32 { get }
var TWCY_CZECHREPUBLIC: Int32 { get }
var TWCY_DIEGOGARCIA: Int32 { get }
var TWCY_ERITREA: Int32 { get }
var TWCY_ESTONIA: Int32 { get }
var TWCY_GEORGIA: Int32 { get }
var TWCY_LATVIA: Int32 { get }
var TWCY_LESOTHO: Int32 { get }
var TWCY_LITHUANIA: Int32 { get }
var TWCY_MACEDONIA: Int32 { get }
var TWCY_MAYOTTEIS: Int32 { get }
var TWCY_MOLDOVA: Int32 { get }
var TWCY_MYANMAR: Int32 { get }
var TWCY_NORTHKOREA: Int32 { get }
var TWCY_PUERTORICO: Int32 { get }
var TWCY_RUSSIA: Int32 { get }
var TWCY_SERBIA: Int32 { get }
var TWCY_SLOVAKIA: Int32 { get }
var TWCY_SLOVENIA: Int32 { get }
var TWCY_SOUTHKOREA: Int32 { get }
var TWCY_UKRAINE: Int32 { get }
var TWCY_USVIRGINIS: Int32 { get }
var TWCY_VIETNAM: Int32 { get }
var TWLG_DAN: Int32 { get }
var TWLG_DUT: Int32 { get }
var TWLG_ENG: Int32 { get }
var TWLG_FCF: Int32 { get }
var TWLG_FIN: Int32 { get }
var TWLG_FRN: Int32 { get }
var TWLG_GER: Int32 { get }
var TWLG_ICE: Int32 { get }
var TWLG_ITN: Int32 { get }
var TWLG_NOR: Int32 { get }
var TWLG_POR: Int32 { get }
var TWLG_SPA: Int32 { get }
var TWLG_SWE: Int32 { get }
var TWLG_USA: Int32 { get }
var TWLG_USERLOCALE: Int32 { get }
var TWLG_AFRIKAANS: Int32 { get }
var TWLG_ALBANIA: Int32 { get }
var TWLG_ARABIC: Int32 { get }
var TWLG_ARABIC_ALGERIA: Int32 { get }
var TWLG_ARABIC_BAHRAIN: Int32 { get }
var TWLG_ARABIC_EGYPT: Int32 { get }
var TWLG_ARABIC_IRAQ: Int32 { get }
var TWLG_ARABIC_JORDAN: Int32 { get }
var TWLG_ARABIC_KUWAIT: Int32 { get }
var TWLG_ARABIC_LEBANON: Int32 { get }
var TWLG_ARABIC_LIBYA: Int32 { get }
var TWLG_ARABIC_MOROCCO: Int32 { get }
var TWLG_ARABIC_OMAN: Int32 { get }
var TWLG_ARABIC_QATAR: Int32 { get }
var TWLG_ARABIC_SAUDIARABIA: Int32 { get }
var TWLG_ARABIC_SYRIA: Int32 { get }
var TWLG_ARABIC_TUNISIA: Int32 { get }
var TWLG_ARABIC_UAE: Int32 { get }
var TWLG_ARABIC_YEMEN: Int32 { get }
var TWLG_BASQUE: Int32 { get }
var TWLG_BYELORUSSIAN: Int32 { get }
var TWLG_BULGARIAN: Int32 { get }
var TWLG_CATALAN: Int32 { get }
var TWLG_CHINESE: Int32 { get }
var TWLG_CHINESE_HONGKONG: Int32 { get }
var TWLG_CHINESE_PRC: Int32 { get }
var TWLG_CHINESE_SINGAPORE: Int32 { get }
var TWLG_CHINESE_SIMPLIFIED: Int32 { get }
var TWLG_CHINESE_TAIWAN: Int32 { get }
var TWLG_CHINESE_TRADITIONAL: Int32 { get }
var TWLG_CROATIA: Int32 { get }
var TWLG_CZECH: Int32 { get }
var TWLG_DANISH: Int32 { get }
var TWLG_DUTCH: Int32 { get }
var TWLG_DUTCH_BELGIAN: Int32 { get }
var TWLG_ENGLISH: Int32 { get }
var TWLG_ENGLISH_AUSTRALIAN: Int32 { get }
var TWLG_ENGLISH_CANADIAN: Int32 { get }
var TWLG_ENGLISH_IRELAND: Int32 { get }
var TWLG_ENGLISH_NEWZEALAND: Int32 { get }
var TWLG_ENGLISH_SOUTHAFRICA: Int32 { get }
var TWLG_ENGLISH_UK: Int32 { get }
var TWLG_ENGLISH_USA: Int32 { get }
var TWLG_ESTONIAN: Int32 { get }
var TWLG_FAEROESE: Int32 { get }
var TWLG_FARSI: Int32 { get }
var TWLG_FINNISH: Int32 { get }
var TWLG_FRENCH: Int32 { get }
var TWLG_FRENCH_BELGIAN: Int32 { get }
var TWLG_FRENCH_CANADIAN: Int32 { get }
var TWLG_FRENCH_LUXEMBOURG: Int32 { get }
var TWLG_FRENCH_SWISS: Int32 { get }
var TWLG_GERMAN: Int32 { get }
var TWLG_GERMAN_AUSTRIAN: Int32 { get }
var TWLG_GERMAN_LUXEMBOURG: Int32 { get }
var TWLG_GERMAN_LIECHTENSTEIN: Int32 { get }
var TWLG_GERMAN_SWISS: Int32 { get }
var TWLG_GREEK: Int32 { get }
var TWLG_HEBREW: Int32 { get }
var TWLG_HUNGARIAN: Int32 { get }
var TWLG_ICELANDIC: Int32 { get }
var TWLG_INDONESIAN: Int32 { get }
var TWLG_ITALIAN: Int32 { get }
var TWLG_ITALIAN_SWISS: Int32 { get }
var TWLG_JAPANESE: Int32 { get }
var TWLG_KOREAN: Int32 { get }
var TWLG_KOREAN_JOHAB: Int32 { get }
var TWLG_LATVIAN: Int32 { get }
var TWLG_LITHUANIAN: Int32 { get }
var TWLG_NORWEGIAN: Int32 { get }
var TWLG_NORWEGIAN_BOKMAL: Int32 { get }
var TWLG_NORWEGIAN_NYNORSK: Int32 { get }
var TWLG_POLISH: Int32 { get }
var TWLG_PORTUGUESE: Int32 { get }
var TWLG_PORTUGUESE_BRAZIL: Int32 { get }
var TWLG_ROMANIAN: Int32 { get }
var TWLG_RUSSIAN: Int32 { get }
var TWLG_SERBIAN_LATIN: Int32 { get }
var TWLG_SLOVAK: Int32 { get }
var TWLG_SLOVENIAN: Int32 { get }
var TWLG_SPANISH: Int32 { get }
var TWLG_SPANISH_MEXICAN: Int32 { get }
var TWLG_SPANISH_MODERN: Int32 { get }
var TWLG_SWEDISH: Int32 { get }
var TWLG_THAI: Int32 { get }
var TWLG_TURKISH: Int32 { get }
var TWLG_UKRANIAN: Int32 { get }
var TWLG_ASSAMESE: Int32 { get }
var TWLG_BENGALI: Int32 { get }
var TWLG_BIHARI: Int32 { get }
var TWLG_BODO: Int32 { get }
var TWLG_DOGRI: Int32 { get }
var TWLG_GUJARATI: Int32 { get }
var TWLG_HARYANVI: Int32 { get }
var TWLG_HINDI: Int32 { get }
var TWLG_KANNADA: Int32 { get }
var TWLG_KASHMIRI: Int32 { get }
var TWLG_MALAYALAM: Int32 { get }
var TWLG_MARATHI: Int32 { get }
var TWLG_MARWARI: Int32 { get }
var TWLG_MEGHALAYAN: Int32 { get }
var TWLG_MIZO: Int32 { get }
var TWLG_NAGA: Int32 { get }
var TWLG_ORISSI: Int32 { get }
var TWLG_PUNJABI: Int32 { get }
var TWLG_PUSHTU: Int32 { get }
var TWLG_SERBIAN_CYRILLIC: Int32 { get }
var TWLG_SIKKIMI: Int32 { get }
var TWLG_SWEDISH_FINLAND: Int32 { get }
var TWLG_TAMIL: Int32 { get }
var TWLG_TELUGU: Int32 { get }
var TWLG_TRIPURI: Int32 { get }
var TWLG_URDU: Int32 { get }
var TWLG_VIETNAMESE: Int32 { get }
var DG_CONTROL: Int { get }
var DG_IMAGE: Int { get }
var DG_AUDIO: Int { get }
var DAT_NULL: Int32 { get }
var DAT_CUSTOMBASE: Int32 { get }
var DAT_CAPABILITY: Int32 { get }
var DAT_EVENT: Int32 { get }
var DAT_IDENTITY: Int32 { get }
var DAT_PARENT: Int32 { get }
var DAT_PENDINGXFERS: Int32 { get }
var DAT_SETUPMEMXFER: Int32 { get }
var DAT_SETUPFILEXFER: Int32 { get }
var DAT_STATUS: Int32 { get }
var DAT_USERINTERFACE: Int32 { get }
var DAT_XFERGROUP: Int32 { get }
var DAT_TWUNKIDENTITY: Int32 { get }
var DAT_CUSTOMDSDATA: Int32 { get }
var DAT_DEVICEEVENT: Int32 { get }
var DAT_FILESYSTEM: Int32 { get }
var DAT_PASSTHRU: Int32 { get }
var DAT_CALLBACK: Int32 { get }
var DAT_IMAGEINFO: Int32 { get }
var DAT_IMAGELAYOUT: Int32 { get }
var DAT_IMAGEMEMXFER: Int32 { get }
var DAT_IMAGENATIVEXFER: Int32 { get }
var DAT_IMAGEFILEXFER: Int32 { get }
var DAT_CIECOLOR: Int32 { get }
var DAT_GRAYRESPONSE: Int32 { get }
var DAT_RGBRESPONSE: Int32 { get }
var DAT_JPEGCOMPRESSION: Int32 { get }
var DAT_PALETTE8: Int32 { get }
var DAT_EXTIMAGEINFO: Int32 { get }
var DAT_AUDIOFILEXFER: Int32 { get }
var DAT_AUDIOINFO: Int32 { get }
var DAT_AUDIONATIVEXFER: Int32 { get }
var DAT_SETUPFILEXFER2: Int32 { get }
var MSG_NULL: Int32 { get }
var MSG_CUSTOMBASE: Int32 { get }
var MSG_GET: Int32 { get }
var MSG_GETCURRENT: Int32 { get }
var MSG_GETDEFAULT: Int32 { get }
var MSG_GETFIRST: Int32 { get }
var MSG_GETNEXT: Int32 { get }
var MSG_SET: Int32 { get }
var MSG_RESET: Int32 { get }
var MSG_QUERYSUPPORT: Int32 { get }
var MSG_XFERREADY: Int32 { get }
var MSG_CLOSEDSREQ: Int32 { get }
var MSG_CLOSEDSOK: Int32 { get }
var MSG_DEVICEEVENT: Int32 { get }
var MSG_CHECKSTATUS: Int32 { get }
var MSG_OPENDSM: Int32 { get }
var MSG_CLOSEDSM: Int32 { get }
var MSG_OPENDS: Int32 { get }
var MSG_CLOSEDS: Int32 { get }
var MSG_USERSELECT: Int32 { get }
var MSG_DISABLEDS: Int32 { get }
var MSG_ENABLEDS: Int32 { get }
var MSG_ENABLEDSUIONLY: Int32 { get }
var MSG_PROCESSEVENT: Int32 { get }
var MSG_ENDXFER: Int32 { get }
var MSG_STOPFEEDER: Int32 { get }
var MSG_CHANGEDIRECTORY: Int32 { get }
var MSG_CREATEDIRECTORY: Int32 { get }
var MSG_DELETE: Int32 { get }
var MSG_FORMATMEDIA: Int32 { get }
var MSG_GETCLOSE: Int32 { get }
var MSG_GETFIRSTFILE: Int32 { get }
var MSG_GETINFO: Int32 { get }
var MSG_GETNEXTFILE: Int32 { get }
var MSG_RENAME: Int32 { get }
var MSG_COPY: Int32 { get }
var MSG_AUTOMATICCAPTUREDIRECTORY: Int32 { get }
var MSG_PASSTHRU: Int32 { get }
var MSG_REGISTER_CALLBACK: Int32 { get }
var MSG_INVOKE_CALLBACK: Int32 { get }
var CAP_CUSTOMBASE: Int32 { get }
var CAP_XFERCOUNT: Int32 { get }
var ICAP_COMPRESSION: Int32 { get }
var ICAP_PIXELTYPE: Int32 { get }
var ICAP_UNITS: Int32 { get }
var ICAP_XFERMECH: Int32 { get }
var CAP_AUTHOR: Int32 { get }
var CAP_CAPTION: Int32 { get }
var CAP_FEEDERENABLED: Int32 { get }
var CAP_FEEDERLOADED: Int32 { get }
var CAP_TIMEDATE: Int32 { get }
var CAP_SUPPORTEDCAPS: Int32 { get }
var CAP_EXTENDEDCAPS: Int32 { get }
var CAP_AUTOFEED: Int32 { get }
var CAP_CLEARPAGE: Int32 { get }
var CAP_FEEDPAGE: Int32 { get }
var CAP_REWINDPAGE: Int32 { get }
var CAP_INDICATORS: Int32 { get }
var CAP_SUPPORTEDCAPSEXT: Int32 { get }
var CAP_PAPERDETECTABLE: Int32 { get }
var CAP_UICONTROLLABLE: Int32 { get }
var CAP_DEVICEONLINE: Int32 { get }
var CAP_AUTOSCAN: Int32 { get }
var CAP_THUMBNAILSENABLED: Int32 { get }
var CAP_DUPLEX: Int32 { get }
var CAP_DUPLEXENABLED: Int32 { get }
var CAP_ENABLEDSUIONLY: Int32 { get }
var CAP_CUSTOMDSDATA: Int32 { get }
var CAP_ENDORSER: Int32 { get }
var CAP_JOBCONTROL: Int32 { get }
var CAP_ALARMS: Int32 { get }
var CAP_ALARMVOLUME: Int32 { get }
var CAP_AUTOMATICCAPTURE: Int32 { get }
var CAP_TIMEBEFOREFIRSTCAPTURE: Int32 { get }
var CAP_TIMEBETWEENCAPTURES: Int32 { get }
var CAP_CLEARBUFFERS: Int32 { get }
var CAP_MAXBATCHBUFFERS: Int32 { get }
var CAP_DEVICETIMEDATE: Int32 { get }
var CAP_POWERSUPPLY: Int32 { get }
var CAP_CAMERAPREVIEWUI: Int32 { get }
var CAP_DEVICEEVENT: Int32 { get }
var CAP_SERIALNUMBER: Int32 { get }
var CAP_PRINTER: Int32 { get }
var CAP_PRINTERENABLED: Int32 { get }
var CAP_PRINTERINDEX: Int32 { get }
var CAP_PRINTERMODE: Int32 { get }
var CAP_PRINTERSTRING: Int32 { get }
var CAP_PRINTERSUFFIX: Int32 { get }
var CAP_LANGUAGE: Int32 { get }
var CAP_FEEDERALIGNMENT: Int32 { get }
var CAP_FEEDERORDER: Int32 { get }
var CAP_REACQUIREALLOWED: Int32 { get }
var CAP_BATTERYMINUTES: Int32 { get }
var CAP_BATTERYPERCENTAGE: Int32 { get }
var ICAP_AUTOBRIGHT: Int32 { get }
var ICAP_BRIGHTNESS: Int32 { get }
var ICAP_CONTRAST: Int32 { get }
var ICAP_CUSTHALFTONE: Int32 { get }
var ICAP_EXPOSURETIME: Int32 { get }
var ICAP_FILTER: Int32 { get }
var ICAP_FLASHUSED: Int32 { get }
var ICAP_GAMMA: Int32 { get }
var ICAP_HALFTONES: Int32 { get }
var ICAP_HIGHLIGHT: Int32 { get }
var ICAP_IMAGEFILEFORMAT: Int32 { get }
var ICAP_LAMPSTATE: Int32 { get }
var ICAP_LIGHTSOURCE: Int32 { get }
var ICAP_ORIENTATION: Int32 { get }
var ICAP_PHYSICALWIDTH: Int32 { get }
var ICAP_PHYSICALHEIGHT: Int32 { get }
var ICAP_SHADOW: Int32 { get }
var ICAP_FRAMES: Int32 { get }
var ICAP_XNATIVERESOLUTION: Int32 { get }
var ICAP_YNATIVERESOLUTION: Int32 { get }
var ICAP_XRESOLUTION: Int32 { get }
var ICAP_YRESOLUTION: Int32 { get }
var ICAP_MAXFRAMES: Int32 { get }
var ICAP_TILES: Int32 { get }
var ICAP_BITORDER: Int32 { get }
var ICAP_CCITTKFACTOR: Int32 { get }
var ICAP_LIGHTPATH: Int32 { get }
var ICAP_PIXELFLAVOR: Int32 { get }
var ICAP_PLANARCHUNKY: Int32 { get }
var ICAP_ROTATION: Int32 { get }
var ICAP_SUPPORTEDSIZES: Int32 { get }
var ICAP_THRESHOLD: Int32 { get }
var ICAP_XSCALING: Int32 { get }
var ICAP_YSCALING: Int32 { get }
var ICAP_BITORDERCODES: Int32 { get }
var ICAP_PIXELFLAVORCODES: Int32 { get }
var ICAP_JPEGPIXELTYPE: Int32 { get }
var ICAP_TIMEFILL: Int32 { get }
var ICAP_BITDEPTH: Int32 { get }
var ICAP_BITDEPTHREDUCTION: Int32 { get }
var ICAP_UNDEFINEDIMAGESIZE: Int32 { get }
var ICAP_IMAGEDATASET: Int32 { get }
var ICAP_EXTIMAGEINFO: Int32 { get }
var ICAP_MINIMUMHEIGHT: Int32 { get }
var ICAP_MINIMUMWIDTH: Int32 { get }
var ICAP_FLIPROTATION: Int32 { get }
var ICAP_BARCODEDETECTIONENABLED: Int32 { get }
var ICAP_SUPPORTEDBARCODETYPES: Int32 { get }
var ICAP_BARCODEMAXSEARCHPRIORITIES: Int32 { get }
var ICAP_BARCODESEARCHPRIORITIES: Int32 { get }
var ICAP_BARCODESEARCHMODE: Int32 { get }
var ICAP_BARCODEMAXRETRIES: Int32 { get }
var ICAP_BARCODETIMEOUT: Int32 { get }
var ICAP_ZOOMFACTOR: Int32 { get }
var ICAP_PATCHCODEDETECTIONENABLED: Int32 { get }
var ICAP_SUPPORTEDPATCHCODETYPES: Int32 { get }
var ICAP_PATCHCODEMAXSEARCHPRIORITIES: Int32 { get }
var ICAP_PATCHCODESEARCHPRIORITIES: Int32 { get }
var ICAP_PATCHCODESEARCHMODE: Int32 { get }
var ICAP_PATCHCODEMAXRETRIES: Int32 { get }
var ICAP_PATCHCODETIMEOUT: Int32 { get }
var ICAP_FLASHUSED2: Int32 { get }
var ICAP_IMAGEFILTER: Int32 { get }
var ICAP_NOISEFILTER: Int32 { get }
var ICAP_OVERSCAN: Int32 { get }
var ICAP_AUTOMATICBORDERDETECTION: Int32 { get }
var ICAP_AUTOMATICDESKEW: Int32 { get }
var ICAP_AUTOMATICROTATE: Int32 { get }
var ICAP_JPEGQUALITY: Int32 { get }
var ACAP_AUDIOFILEFORMAT: Int32 { get }
var ACAP_XFERMECH: Int32 { get }
var TWEI_BARCODEX: Int32 { get }
var TWEI_BARCODEY: Int32 { get }
var TWEI_BARCODETEXT: Int32 { get }
var TWEI_BARCODETYPE: Int32 { get }
var TWEI_DESHADETOP: Int32 { get }
var TWEI_DESHADELEFT: Int32 { get }
var TWEI_DESHADEHEIGHT: Int32 { get }
var TWEI_DESHADEWIDTH: Int32 { get }
var TWEI_DESHADESIZE: Int32 { get }
var TWEI_SPECKLESREMOVED: Int32 { get }
var TWEI_HORZLINEXCOORD: Int32 { get }
var TWEI_HORZLINEYCOORD: Int32 { get }
var TWEI_HORZLINELENGTH: Int32 { get }
var TWEI_HORZLINETHICKNESS: Int32 { get }
var TWEI_VERTLINEXCOORD: Int32 { get }
var TWEI_VERTLINEYCOORD: Int32 { get }
var TWEI_VERTLINELENGTH: Int32 { get }
var TWEI_VERTLINETHICKNESS: Int32 { get }
var TWEI_PATCHCODE: Int32 { get }
var TWEI_ENDORSEDTEXT: Int32 { get }
var TWEI_FORMCONFIDENCE: Int32 { get }
var TWEI_FORMTEMPLATEMATCH: Int32 { get }
var TWEI_FORMTEMPLATEPAGEMATCH: Int32 { get }
var TWEI_FORMHORZDOCOFFSET: Int32 { get }
var TWEI_FORMVERTDOCOFFSET: Int32 { get }
var TWEI_BARCODECOUNT: Int32 { get }
var TWEI_BARCODECONFIDENCE: Int32 { get }
var TWEI_BARCODEROTATION: Int32 { get }
var TWEI_BARCODETEXTLENGTH: Int32 { get }
var TWEI_DESHADECOUNT: Int32 { get }
var TWEI_DESHADEBLACKCOUNTOLD: Int32 { get }
var TWEI_DESHADEBLACKCOUNTNEW: Int32 { get }
var TWEI_DESHADEBLACKRLMIN: Int32 { get }
var TWEI_DESHADEBLACKRLMAX: Int32 { get }
var TWEI_DESHADEWHITECOUNTOLD: Int32 { get }
var TWEI_DESHADEWHITECOUNTNEW: Int32 { get }
var TWEI_DESHADEWHITERLMIN: Int32 { get }
var TWEI_DESHADEWHITERLAVE: Int32 { get }
var TWEI_DESHADEWHITERLMAX: Int32 { get }
var TWEI_BLACKSPECKLESREMOVED: Int32 { get }
var TWEI_WHITESPECKLESREMOVED: Int32 { get }
var TWEI_HORZLINECOUNT: Int32 { get }
var TWEI_VERTLINECOUNT: Int32 { get }
var TWEI_DESKEWSTATUS: Int32 { get }
var TWEI_SKEWORIGINALANGLE: Int32 { get }
var TWEI_SKEWFINALANGLE: Int32 { get }
var TWEI_SKEWCONFIDENCE: Int32 { get }
var TWEI_SKEWWINDOWX1: Int32 { get }
var TWEI_SKEWWINDOWY1: Int32 { get }
var TWEI_SKEWWINDOWX2: Int32 { get }
var TWEI_SKEWWINDOWY2: Int32 { get }
var TWEI_SKEWWINDOWX3: Int32 { get }
var TWEI_SKEWWINDOWY3: Int32 { get }
var TWEI_SKEWWINDOWX4: Int32 { get }
var TWEI_SKEWWINDOWY4: Int32 { get }
var TWEI_BOOKNAME: Int32 { get }
var TWEI_CHAPTERNUMBER: Int32 { get }
var TWEI_DOCUMENTNUMBER: Int32 { get }
var TWEI_PAGENUMBER: Int32 { get }
var TWEI_CAMERA: Int32 { get }
var TWEI_FRAMENUMBER: Int32 { get }
var TWEI_FRAME: Int32 { get }
var TWEI_PIXELFLAVOR: Int32 { get }
var TWEJ_NONE: Int32 { get }
var TWEJ_MIDSEPARATOR: Int32 { get }
var TWEJ_PATCH1: Int32 { get }
var TWEJ_PATCH2: Int32 { get }
var TWEJ_PATCH3: Int32 { get }
var TWEJ_PATCH4: Int32 { get }
var TWEJ_PATCH6: Int32 { get }
var TWEJ_PATCHT: Int32 { get }
var TWDR_GET: Int32 { get }
var TWDR_SET: Int32 { get }
var TWRC_CUSTOMBASE: Int32 { get }
var TWRC_SUCCESS: Int32 { get }
var TWRC_FAILURE: Int32 { get }
var TWRC_CHECKSTATUS: Int32 { get }
var TWRC_CANCEL: Int32 { get }
var TWRC_DSEVENT: Int32 { get }
var TWRC_NOTDSEVENT: Int32 { get }
var TWRC_XFERDONE: Int32 { get }
var TWRC_ENDOFLIST: Int32 { get }
var TWRC_INFONOTSUPPORTED: Int32 { get }
var TWRC_DATANOTAVAILABLE: Int32 { get }
var TWCC_CUSTOMBASE: Int32 { get }
var TWCC_SUCCESS: Int32 { get }
var TWCC_BUMMER: Int32 { get }
var TWCC_LOWMEMORY: Int32 { get }
var TWCC_NODS: Int32 { get }
var TWCC_MAXCONNECTIONS: Int32 { get }
var TWCC_OPERATIONERROR: Int32 { get }
var TWCC_BADCAP: Int32 { get }
var TWCC_BADPROTOCOL: Int32 { get }
var TWCC_BADVALUE: Int32 { get }
var TWCC_SEQERROR: Int32 { get }
var TWCC_BADDEST: Int32 { get }
var TWCC_CAPUNSUPPORTED: Int32 { get }
var TWCC_CAPBADOPERATION: Int32 { get }
var TWCC_CAPSEQERROR: Int32 { get }
var TWCC_DENIED: Int32 { get }
var TWCC_FILEEXISTS: Int32 { get }
var TWCC_FILENOTFOUND: Int32 { get }
var TWCC_NOTEMPTY: Int32 { get }
var TWCC_PAPERJAM: Int32 { get }
var TWCC_PAPERDOUBLEFEED: Int32 { get }
var TWCC_FILEWRITEERROR: Int32 { get }
var TWCC_CHECKDEVICEONLINE: Int32 { get }
var TWQC_GET: Int32 { get }
var TWQC_SET: Int32 { get }
var TWQC_GETDEFAULT: Int32 { get }
var TWQC_GETCURRENT: Int32 { get }
var TWQC_RESET: Int32 { get }
@discardableResult
func DSM_Entry(_ pOrigin: pTW_IDENTITY!, _ pDest: pTW_IDENTITY!, _ DG: TW_UINT32, _ DAT: TW_UINT16, _ MSG: TW_UINT16, _ pData: TW_MEMREF!) -> TW_UINT16
typealias DSMENTRYPROC = @convention(c) (pTW_IDENTITY!, pTW_IDENTITY!, TW_UINT32, TW_UINT16, TW_UINT16, TW_MEMREF!) -> TW_UINT16
@discardableResult
func DS_Entry(_ pOrigin: pTW_IDENTITY!, _ DG: TW_UINT32, _ DAT: TW_UINT16, _ MSG: TW_UINT16, _ pData: TW_MEMREF!) -> TW_UINT16
typealias DSENTRYPROC = @convention(c) (pTW_IDENTITY!, TW_UINT32, TW_UINT16, TW_UINT16, TW_MEMREF!) -> TW_UINT16
