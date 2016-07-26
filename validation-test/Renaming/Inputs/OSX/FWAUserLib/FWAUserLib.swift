
typealias FWARef = OpaquePointer
typealias FWAIsochStreamRef = OpaquePointer
typealias FWADeviceRef = OpaquePointer
typealias FWAEngineRef = OpaquePointer
typealias FWAAudioStreamRef = OpaquePointer
typealias FWAMIDIStreamRef = OpaquePointer
typealias FWAMIDIPlugRef = OpaquePointer
typealias FWAAudioPlugRef = OpaquePointer
typealias FWAMIDIDeviceNubRef = OpaquePointer
typealias FWADeviceID = UInt32
@discardableResult
func FWACountDevices(_ deviceNodeIDArray: UnsafeMutablePointer<UInt16>!, _ deviceCount: UnsafeMutablePointer<UInt16>!) -> OSStatus
@discardableResult
func FWAOpen(_ nodeID: UInt32, _ outRef: UnsafeMutablePointer<FWARef?>!) -> OSStatus
@discardableResult
func FWAOpenLocal(_ outRef: UnsafeMutablePointer<FWARef?>!) -> OSStatus
@discardableResult
func FWAClose(_ inRef: FWARef!) -> OSStatus
@discardableResult
func FWARead(_ inRef: FWARef!, _ inAddress: UInt8, _ inSubAddress: UInt8, _ inDataSize: Int, _ inDataPtr: UnsafeMutablePointer<Void>!) -> OSStatus
@discardableResult
func FWAWrite(_ inRef: FWARef!, _ inAddress: UInt8, _ inSubAddress: UInt8, _ inDataSize: Int, _ inDataPtr: UnsafePointer<Void>!) -> OSStatus
@discardableResult
func FWAGetNodeID(_ inRef: FWARef!, _ outNodeID: UnsafeMutablePointer<UInt32>!, _ outGeneration: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetGUID(_ inRef: FWARef!, _ guid: UnsafeMutablePointer<UInt64>!) -> OSStatus
@discardableResult
func FWAGetMacGUID(_ inRef: FWARef!, _ guid: UnsafeMutablePointer<UInt64>!) -> OSStatus
@discardableResult
func FWAReadQuadlet(_ inRef: FWARef!, _ address: FWAddressPtr!, _ outData: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAReadBlock(_ inRef: FWARef!, _ address: FWAddressPtr!, _ size: UnsafeMutablePointer<UInt32>!, _ outData: UnsafeMutablePointer<UInt8>!) -> OSStatus
@discardableResult
func FWAExecuteAVC(_ inRef: FWARef!, _ cmd: UnsafeMutablePointer<UInt8>!, _ cmdSize: UInt32, _ response: UnsafeMutablePointer<UInt8>!, _ responseSize: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAWriteQuadlet(_ inRef: FWARef!, _ address: FWAddressPtr!, _ data: UInt32) -> OSStatus
@discardableResult
func FWAWriteBlock(_ inRef: FWARef!, _ address: FWAddressPtr!, _ size: UInt32, _ data: UnsafePointer<UInt8>!) -> OSStatus
@discardableResult
func FWACreateMIDIStream(_ inRef: FWARef!, _ midiIO: UInt32, _ bufSizeInBytes: UInt32, _ buf: UnsafeMutablePointer<Void>!, _ sequenceNum: UInt32, _ midiStreamRef: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWADisposeMIDIStream(_ inRef: FWARef!, _ midiStreamRef: UInt32) -> OSStatus
@discardableResult
func FWAWriteMIDIData(_ inRef: FWARef!, _ midiStreamRef: UInt32, _ writeMsgLength: UInt32, _ buf: UnsafeMutablePointer<UInt8>!) -> OSStatus
@discardableResult
func FWAReadMIDIData(_ inRef: FWARef!, _ midiStreamRef: UInt32, _ buf: UnsafeMutablePointer<FWAMIDIReadBuf>!) -> OSStatus
@discardableResult
func FWAGetCycleTimeOffset(_ inRef: FWARef!, _ cycleTimeOffset: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetCycleTimeOffset(_ inRef: FWARef!, _ cycleTimeOffset: UInt32) -> OSStatus
@discardableResult
func FWAGetVendorID(_ inRef: FWARef!, _ vendorID: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetDeviceName(_ inRef: FWARef!, _ name: UnsafeMutablePointer<Int8>!) -> OSStatus
@discardableResult
func FWAGetVendorName(_ inRef: FWARef!, _ name: UnsafeMutablePointer<Int8>!) -> OSStatus
@discardableResult
func FWAIsMIDICapable(_ inRef: FWARef!, _ supportsMIDI: UnsafeMutablePointer<Bool>!) -> OSStatus
@discardableResult
func FWAGetNumMIDIInputPlugs(_ inRef: FWARef!, _ plugs: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetNumMIDIOutputPlugs(_ inRef: FWARef!, _ plugs: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetNumMIDIInputPlugs(_ inRef: FWARef!, _ plugs: UInt32) -> OSStatus
@discardableResult
func FWASetNumMIDIOutputPlugs(_ inRef: FWARef!, _ plugs: UInt32) -> OSStatus
@discardableResult
func FWAGetNumAudioInputPlugs(_ inRef: FWARef!, _ plugs: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetNumAudioOutputPlugs(_ inRef: FWARef!, _ plugs: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWACreateAudioStream(_ inRef: FWARef!, _ audioIO: UInt32, _ audioStreamRef: UnsafeMutablePointer<UInt32>!, _ sequenceNum: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWADisposeAudioStream(_ inRef: FWARef!, _ audioStreamRef: UInt32) -> OSStatus
@discardableResult
func FWAGetDeviceSampleRate(_ inRef: FWARef!, _ rate: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetDeviceSendMode(_ inRef: FWARef!, _ mode: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetDeviceStatus(_ inRef: FWARef!, _ outData: UnsafeMutablePointer<Void>!, _ inSize: UInt32) -> OSStatus
@discardableResult
func FWAGetDeviceStreamInfo(_ inRef: FWARef!, _ audioStreamRef: UInt32, _ numInput: UnsafeMutablePointer<UInt32>!, _ inputIsochChan: UnsafeMutablePointer<UInt32>!, _ numOutput: UnsafeMutablePointer<UInt32>!, _ outputIsochChan: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAInitAEvntSource(_ inRef: FWARef!, _ source: UnsafeMutablePointer<Unmanaged<CFRunLoopSource>?>!, _ refcon: UnsafeMutablePointer<Void>!) -> OSStatus
@discardableResult
func CreateAsyncWakePort(_ inRef: FWARef!, _ notifyPort: UnsafeMutablePointer<mach_port_t>!) -> OSStatus
@discardableResult
func FWAGetAEvntSource(_ inRef: FWARef!) -> Unmanaged<CFRunLoopSource>!
@discardableResult
func FWAWriteMIDIDataAsync(_ inRef: FWARef!, _ midiStreamRef: UInt32, _ writeMsgLength: UInt32, _ callback: IOAsyncCallback1!, _ refCon: UnsafeMutablePointer<Void>!) -> OSStatus
@discardableResult
func FWAReadMIDIDataAsync(_ inRef: FWARef!, _ midiStreamRef: UInt32, _ readBufSize: UInt32, _ callback: IOAsyncCallback2!, _ refCon: UnsafeMutablePointer<Void>!) -> OSStatus
@discardableResult
func FWASetDeviceStreamInfo(_ inRef: FWARef!, _ audioStreamRef: UInt32, _ numInput: UInt32, _ inputIsochChan: UInt32, _ numOutput: UInt32, _ outputIsochChan: UInt32, _ update: Bool) -> OSStatus
@discardableResult
func FWASyncUpDevice(_ inRef: FWARef!) -> OSStatus
@discardableResult
func FWAGetMaxSpeed(_ inRef: FWARef!, _ speed: UnsafeMutablePointer<IOFWSpeed>!) -> OSStatus
@discardableResult
func FWAGetMaxIsochChannels(_ inRef: FWARef!, _ inChannels: UnsafeMutablePointer<UInt32>!, _ outChannels: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetMaxSequences(_ inRef: FWARef!, _ numSequences: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetSupportedSampleRates(_ inRef: FWARef!, _ sampleRates: UnsafeMutablePointer<UInt32>!, _ count: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetSupportedAudioTypes(_ inRef: FWARef!, _ audioTypes: UnsafeMutablePointer<FWAudioType>!, _ count: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetCurrentIsochStreamRefs(_ inRef: FWARef!, _ isochStreamRef: UnsafeMutablePointer<FWAIsochStreamRef?>!, _ count: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWAGetIsochStreamState(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ state: UnsafeMutablePointer<FWAStreamState>!) -> OSStatus
@discardableResult
func FWAGetIsochStreamDirection(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ direction: UnsafeMutablePointer<FWAStreamDirection>!) -> OSStatus
@discardableResult
func FWAGetIsochStreamChannelID(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ channelID: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetIsochStreamChannelID(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ channelID: UInt32) -> OSStatus
@discardableResult
func FWAGetIsochStreamSampleRate(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ rate: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetIsochStreamSampleRate(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ rate: UInt32) -> OSStatus
@discardableResult
func FWAGetIsochStreamOutputSpeed(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ speed: UnsafeMutablePointer<IOFWSpeed>!) -> OSStatus
@discardableResult
func FWASetIsochStreamOutputSpeed(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ speed: IOFWSpeed) -> OSStatus
@discardableResult
func FWAGetIsochStreamAudioType(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ type: UnsafeMutablePointer<FWAudioType>!) -> OSStatus
@discardableResult
func FWASetIsochStreamAudioType(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ type: FWAudioType) -> OSStatus
@discardableResult
func FWACreateIsochStream(_ inRef: FWARef!, _ channelNumber: UInt32, _ direction: FWAStreamDirection, _ numAudioChannels: UInt32, _ numMIDIChannels: UInt32, _ isochStreamRef: UnsafeMutablePointer<FWAIsochStreamRef?>!) -> OSStatus
@discardableResult
func FWADisposeIsochStream(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!) -> OSStatus
@discardableResult
func FWAStartIsochStream(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!) -> OSStatus
@discardableResult
func FWAStopIsochStream(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!) -> OSStatus
@discardableResult
func FWAGetIsochStreamAudioSequenceCount(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ numAudioSequence: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetIsochStreamAudioSequenceCount(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ numAudioSequence: UInt32) -> OSStatus
@discardableResult
func FWAGetIsochStreamMIDISequenceCount(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ numMIDISequence: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetIsochStreamMIDISequenceCount(_ inRef: FWARef!, _ isochStreamRef: FWAIsochStreamRef!, _ numMIDISequence: UInt32) -> OSStatus
@discardableResult
func FWACreateFWAudioDevice(_ inRef: FWARef!, _ deviceName: UnsafePointer<Int8>!, _ vendorID: UInt32, _ guid: UnsafePointer<Int8>!, _ device: UnsafeMutablePointer<FWADeviceRef?>!) -> OSStatus
@discardableResult
func FWADisposeFWAudioDevice(_ inRef: FWARef!, _ device: FWADeviceRef!) -> OSStatus
@discardableResult
func FWAStartFWAudioDevice(_ inRef: FWARef!, _ device: FWADeviceRef!) -> OSStatus
@discardableResult
func FWAStopFWAudioDevice(_ inRef: FWARef!, _ device: FWADeviceRef!) -> OSStatus
@discardableResult
func FWACreateFWAudioEngine(_ inRef: FWARef!, _ owningDevice: FWADeviceRef!, _ hasInput: Bool, _ hasOutput: Bool, _ engine: UnsafeMutablePointer<FWAEngineRef?>!) -> OSStatus
@discardableResult
func FWADisposeFWAudioEngine(_ inRef: FWARef!, _ engine: FWAEngineRef!) -> OSStatus
@discardableResult
func FWACreateFWAudioStream(_ inRef: FWARef!, _ owningIsochStreamRef: FWAIsochStreamRef!, _ channelNumber: UInt32, _ direction: UInt32, _ numAudioChannels: UInt32, _ streamName: UnsafeMutablePointer<Int8>!, _ streamIdent: UnsafeMutablePointer<UInt8>!, _ streamRef: UnsafeMutablePointer<FWAAudioStreamRef?>!) -> OSStatus
@discardableResult
func FWADisposeFWAudioStream(_ inRef: FWARef!, _ streamRef: FWAAudioStreamRef!) -> OSStatus
@discardableResult
func FWACreateFWAudioMIDIStream(_ inRef: FWARef!, _ owningIsochStreamRef: FWAIsochStreamRef!, _ sequenceNumber: UInt32, _ direction: UInt32, _ streamRef: UnsafeMutablePointer<FWAMIDIStreamRef?>!) -> OSStatus
@discardableResult
func FWADisposeFWAudioMIDIStream(_ inRef: FWARef!, _ streamRef: FWAMIDIStreamRef!) -> OSStatus
@discardableResult
func FWACreateFWAudioMIDIPlug(_ inRef: FWARef!, _ owningMIDIStreamRef: FWAMIDIStreamRef!, _ mpxID: UInt8, _ plugName: UnsafeMutablePointer<Int8>!, _ plugIdent: UnsafeMutablePointer<UInt8>!, _ streamRef: UnsafeMutablePointer<FWAMIDIPlugRef?>!) -> OSStatus
@discardableResult
func FWADisposeFWAudioMIDIPlug(_ inRef: FWARef!, _ plugRef: FWAMIDIPlugRef!) -> OSStatus
@discardableResult
func FWAGetClockSource(_ inRef: FWARef!, _ streamRef: UnsafeMutablePointer<FWAIsochStreamRef?>!, _ sequence: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetClockSource(_ inRef: FWARef!, _ streamRef: FWAIsochStreamRef!, _ sequence: UInt32) -> OSStatus
@discardableResult
func FWASetAutoLoad(_ inRef: FWARef!, _ enable: Bool) -> OSStatus
@discardableResult
func FWAGetProperty(_ inRef: FWARef!, _ propertyID: UInt32, _ data: UnsafeMutablePointer<Void>!, _ size: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetProperty(_ inRef: FWARef!, _ propertyID: UInt32, _ data: UnsafeMutablePointer<Void>!, _ size: UInt32) -> OSStatus
@discardableResult
func FWASetPluginPath(_ inRef: FWARef!, _ engine: FWAEngineRef!, _ vendorID: UInt32, _ modelID: UInt32, _ pluginPath: UnsafePointer<Int8>!) -> OSStatus
@discardableResult
func FWACreateFWAudioPlug(_ inRef: FWARef!, _ owningStream: FWAAudioStreamRef!, _ channelID: UInt32, _ plugName: UnsafeMutablePointer<Int8>!, _ plugIdent: UnsafeMutablePointer<UInt8>!, _ streamRef: UnsafeMutablePointer<FWAAudioPlugRef?>!) -> OSStatus
@discardableResult
func FWADisposeFWAudioPlug(_ inRef: FWARef!, _ plugRef: FWAAudioPlugRef!) -> OSStatus
@discardableResult
func FWAGetFWAudioMIDIPlugChannel(_ inRef: FWARef!, _ streamRef: FWAMIDIPlugRef!, _ channelID: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetFWAudioMIDIPlugChannel(_ inRef: FWARef!, _ streamRef: FWAMIDIPlugRef!, _ channelID: UInt32) -> OSStatus
@discardableResult
func FWAGetFWAudioPlugChannel(_ inRef: FWARef!, _ streamRef: FWAAudioPlugRef!, _ channelID: UnsafeMutablePointer<UInt32>!) -> OSStatus
@discardableResult
func FWASetFWAudioPlugChannel(_ inRef: FWARef!, _ streamRef: FWAAudioPlugRef!, _ channelID: UInt32) -> OSStatus
@discardableResult
func FWAGetIndexedFWAudioPlug(_ inRef: FWARef!, _ device: FWADeviceRef!, _ index: UInt32, _ dir: UInt32, _ plugRef: UnsafeMutablePointer<FWAAudioPlugRef?>!) -> OSStatus
@discardableResult
func FWAGetIndexedFWAudioMIDIPlug(_ inRef: FWARef!, _ device: FWAMIDIDeviceNubRef!, _ index: UInt32, _ dir: UInt32, _ plugRef: UnsafeMutablePointer<FWAMIDIPlugRef?>!) -> OSStatus
@discardableResult
func FWAAttachFWAudioStream(_ inRef: FWARef!, _ streamRef: FWAAudioStreamRef!, _ isochChannel: FWAIsochStreamRef!) -> OSStatus
@discardableResult
func FWAAttachFWAudioMIDIStream(_ inRef: FWARef!, _ streamRef: FWAMIDIStreamRef!, _ isochChannel: FWAIsochStreamRef!) -> OSStatus
@discardableResult
func FWASetFWAudioPlugProperty(_ inRef: FWARef!, _ plugRef: FWAAudioPlugRef!, _ keyname: UnsafePointer<Int8>!, _ keyvalue: UnsafePointer<Int8>!) -> OSStatus
@discardableResult
func FWASetFWAudioMIDIPlugProperty(_ inRef: FWARef!, _ plugRef: FWAMIDIPlugRef!, _ keyname: UnsafePointer<Int8>!, _ keyvalue: UnsafePointer<Int8>!) -> OSStatus
@discardableResult
func FWAOpenLocalWithInterface(_ guid: UInt64, _ options: UInt32, _ outRef: UnsafeMutablePointer<FWARef?>!) -> OSStatus
@discardableResult
func FWAOpenWithService(_ _: io_service_t, _ options: UInt32, _ outRef: UnsafeMutablePointer<FWARef?>!) -> OSStatus
@discardableResult
func FWAGetSessionRef(_ inRef: FWARef!, _ sessionRef: UnsafeMutablePointer<IOFireWireSessionRef?>!) -> OSStatus
@discardableResult
func FWAReserveIsochSequences(_ inRef: FWARef!, _ isochStream: FWAIsochStreamRef!, _ type: FWAudioType, _ count: UInt32) -> OSStatus
@discardableResult
func FWACreateFWAudioMIDIDeviceNub(_ inRef: FWARef!, _ owningDevice: FWADeviceRef!, _ deviceName: UnsafePointer<Int8>!, _ vendorID: UInt32, _ guid: UnsafePointer<Int8>!, _ iconFilePath: UnsafePointer<Int8>!, _ modelID: UInt32, _ editorPath: UnsafePointer<Int8>!, _ device: UnsafeMutablePointer<FWAMIDIDeviceNubRef?>!) -> OSStatus
@discardableResult
func FWADisposeFWAudioMIDIDeviceNub(_ inRef: FWARef!, _ device: FWAMIDIDeviceNubRef!) -> OSStatus
@discardableResult
func FWAMIDIDeviceNubAttachMIDIPlug(_ inRef: FWARef!, _ midiDeviceNub: FWAMIDIDeviceNubRef!, _ midiPlug: FWAMIDIPlugRef!) -> OSStatus
@discardableResult
func FWAMIDIDeviceNubDetachMIDIPlug(_ inRef: FWARef!, _ midiPlug: FWAMIDIPlugRef!) -> OSStatus
