@import Foundation;

#define MAKE_ENUM(name) \
  typedef NS_ENUM(NSInteger, name) { \
    name##WaterMelon,                \
    name##Orange                     \
  }                                  \

MAKE_ENUM(MyCoolEnum);


typedef NS_ENUM(NSInteger, ALL_CAPS_ENUM) {
  ENUM_CASE_ONE,
  ENUM_CASE_TWO
};
typedef NS_ENUM(NSInteger, ALL_CAPS_ENUM2) {
  ALL_CAPS_CASE_ONE,
  ALL_CAPS_CASE_TWO
};

typedef NS_ENUM(NSInteger, SomeRandomEnum) {
  SomeRandomA,
  SomeRandomB
};

// From <AudioUnit/AudioComponent.h>
// The interesting feature of this enum is that the common prefix before
// taking the enum name itself into account extends past the underscore.
typedef CF_OPTIONS(UInt32, AudioComponentInstantiationOptions) {
  kAudioComponentInstantiation_LoadOutOfProcess = 1,
  kAudioComponentInstantiation_LoadInProcess = 2
};

// ...whereas this one has a pluralized name before the underscore prefix.
typedef CF_OPTIONS(UInt32, AudioComponentFlags) {
  kAudioComponentFlag_Unsearchable    = 1,
  kAudioComponentFlag_SandboxSafe     = 2,
  kAudioComponentFlag_IsV3AudioUnit	= 4
};

// ...and this one has both complications.
typedef CF_OPTIONS(UInt32, FakeAudioComponentFlags) {
  kFakeAudioComponentFlag_LoadOutOfProcess  = 1,
  kFakeAudioComponentFlag_LoadInProcess     = 2,
};
