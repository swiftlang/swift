
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
typealias alcOutputCapturerPrepareProcPtr = @convention(c) (ALCuint, ALCenum, ALCsizei) -> Void
typealias alcOutputCapturerStartProcPtr = @convention(c) () -> Void
typealias alcOutputCapturerStopProcPtr = @convention(c) () -> Void
typealias alcOutputCapturerAvailableSamplesProcPtr = @convention(c) () -> ALint
typealias alcOutputCapturerSamplesProcPtr = @convention(c) (UnsafeMutablePointer<Void>, ALCsizei) -> Void
