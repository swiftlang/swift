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

typedef NS_ENUM(NSInteger, EnumWithAwkwardDeprecations) {
  EnumWithAwkwardNormalCase1,
  EnumWithAwkwardNormalCase2,
  EnumWithAwkward2BitProblems __attribute__((deprecated)) = EnumWithAwkwardNormalCase1,
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
  kAudioComponentFlag_IsV3AudioUnit   = 4
};

// ...and this one has both complications.
typedef CF_OPTIONS(UInt32, FakeAudioComponentFlags) {
  kFakeAudioComponentFlag_LoadOutOfProcess  = 1,
  kFakeAudioComponentFlag_LoadInProcess     = 2,
};

// From <AudioUnit/AudioUnitProperties.h>
// This enum has a digit immediately after the leading 'k'.
typedef CF_ENUM(UInt32, AU3DMixerAttenuationCurve) {
  k3DMixerAttenuationCurve_Power = 0,
  k3DMixerAttenuationCurve_Exponential = 1,
  k3DMixerAttenuationCurve_Inverse = 2,
  k3DMixerAttenuationCurve_Linear = 3
};

typedef CF_OPTIONS(UInt32, EmptySet1) {
  kEmptySet1DefaultOptions __attribute__((swift_name("default")))
};
typedef CF_OPTIONS(UInt32, EmptySet2) {
  kEmptySet2None __attribute__((swift_name("none")))
};
typedef CF_OPTIONS(UInt32, EmptySet3) {
  kEmptySet3None __attribute__((swift_name("None")))
};
