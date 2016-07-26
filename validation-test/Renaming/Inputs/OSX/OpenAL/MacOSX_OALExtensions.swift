
var ALC_MAC_OSX_CONVERT_DATA_UPON_LOADING: Int32 { get }
typealias alcMacOSXRenderingQualityProcPtr = @convention(c) (ALint) -> Void
typealias alMacOSXRenderChannelCountProcPtr = @convention(c) (ALint) -> Void
typealias alcMacOSXMixerMaxiumumBussesProcPtr = @convention(c) (ALint) -> Void
typealias alcMacOSXMixerOutputRateProcPtr = @convention(c) (ALdouble) -> Void
typealias alcMacOSXGetRenderingQualityProcPtr = @convention(c) () -> ALint
typealias alMacOSXGetRenderChannelCountProcPtr = @convention(c) () -> ALint
typealias alcMacOSXGetMixerMaxiumumBussesProcPtr = @convention(c) () -> ALint
typealias alcMacOSXGetMixerOutputRateProcPtr = @convention(c) () -> ALdouble
typealias alSourceRenderingQualityProcPtr = @convention(c) (ALuint, ALint) -> Void
typealias alSourceGetRenderingQualityProcPtr = @convention(c) (ALuint) -> ALint
typealias alBufferDataStaticProcPtr = @convention(c) (ALint, ALenum, UnsafePointer<Void>, ALsizei, ALsizei) -> Void
var AL_QUEUE_HAS_LOOPED: Int32 { get }
typealias alSourceNotificationProc = @convention(c) (ALuint, ALuint, UnsafeMutablePointer<Void>?) -> Void
typealias alSourceAddNotificationProcPtr = @convention(c) (ALuint, ALuint, alSourceNotificationProc, UnsafeMutablePointer<Void>?) -> ALenum
typealias alSourceRemoveNotificationProcPtr = @convention(c) (ALuint, ALuint, alSourceNotificationProc, UnsafeMutablePointer<Void>?) -> Void
typealias alcASAGetSourceProcPtr = @convention(c) (ALuint, ALuint, UnsafeMutablePointer<Void>, UnsafeMutablePointer<ALuint>) -> ALenum
typealias alcASASetSourceProcPtr = @convention(c) (ALuint, ALuint, UnsafeMutablePointer<Void>, ALuint) -> ALenum
typealias alcASAGetListenerProcPtr = @convention(c) (ALuint, UnsafeMutablePointer<Void>, UnsafeMutablePointer<ALuint>) -> ALenum
typealias alcASASetListenerProcPtr = @convention(c) (ALuint, UnsafeMutablePointer<Void>, ALuint) -> ALenum
var ALC_ASA_REVERB_ROOM_TYPE_SmallRoom: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_MediumRoom: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_LargeRoom: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_MediumHall: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_LargeHall: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_Plate: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_MediumChamber: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_LargeChamber: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_Cathedral: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_LargeRoom2: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_MediumHall2: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_MediumHall3: Int32 { get }
var ALC_ASA_REVERB_ROOM_TYPE_LargeHall2: Int32 { get }
var ALC_ASA_REVERB_QUALITY_Max: Int32 { get }
var ALC_ASA_REVERB_QUALITY_High: Int32 { get }
var ALC_ASA_REVERB_QUALITY_Medium: Int32 { get }
var ALC_ASA_REVERB_QUALITY_Low: Int32 { get }
var ALC_ASA_REVERB_QUALITY_Min: Int32 { get }
var ALC_ASA_ROGER_BEEP_TYPE_quindartone: Int32 { get }
var ALC_ASA_ROGER_BEEP_TYPE_whitenoise: Int32 { get }
var ALC_ASA_ROGER_BEEP_TYPE_walkietalkie: Int32 { get }
var ALC_ASA_ROGER_BEEP_SENSITIVITY_Light: Int32 { get }
var ALC_ASA_ROGER_BEEP_SENSITIVITY_Medium: Int32 { get }
var ALC_ASA_ROGER_BEEP_SENSITIVITY_Heavy: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_BitBrush: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_BufferBeats: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_LoFi: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_BrokenSpeaker: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_Cellphone: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_Decimated1: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_Decimated2: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_Decimated3: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_Decimated4: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_DistortedFunk: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_DistortionCubed: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_DistortionSquared: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_Echo1: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_Echo2: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_EchoTight1: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_EchoTight2: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_EverythingBroken: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_AlienChatter: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_CosmicInteference: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_GoldenPi: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_RadioTower: Int32 { get }
var ALC_ASA_DISTORTION_TYPE_Waves: Int32 { get }
typealias alcOutputCapturerPrepareProcPtr = @convention(c) (ALCuint, ALCenum, ALCsizei) -> Void
typealias alcOutputCapturerStartProcPtr = @convention(c) () -> Void
typealias alcOutputCapturerStopProcPtr = @convention(c) () -> Void
typealias alcOutputCapturerAvailableSamplesProcPtr = @convention(c) () -> ALint
typealias alcOutputCapturerSamplesProcPtr = @convention(c) (UnsafeMutablePointer<Void>, ALCsizei) -> Void
