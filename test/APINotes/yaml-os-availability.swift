# RUN: %swift_driver_plain -apinotes -yaml-to-binary -target i386-apple-ios7 -o %t-ios.apinotesc %S/Inputs/os-availability.apinotes
# RUN: %swift_driver_plain -apinotes -binary-to-yaml %t-ios.apinotesc -o %t.os-availability-ios.apinotes
# RUN: FileCheck %s -check-prefix=IOS < %t.os-availability-ios.apinotes

# RUN: %swift_driver_plain -apinotes -yaml-to-binary -target x86_64-apple-macosx10.9 -o %t-osx.apinotesc %S/Inputs/os-availability.apinotes
# RUN: %swift_driver_plain -apinotes -binary-to-yaml %t-osx.apinotesc -o %t.os-availability-osx.apinotes
# RUN: FileCheck %s -check-prefix=OSX < %t.os-availability-osx.apinotes

# IOS: Foundation
# IOS: NSArray
# IOS: initWithObjects
# IOS: familyNameios
# IOS: fontName
# IOS: NSCountedSet
# IOS: UIApplicationDelegate
# IOS: UIApplicationDelegateIOS
# IOS: NSAvailableWindowDepths
# IOS: NSAvailableWindowDepthsiOS
# IOS-NOT: NSCalibratedWhiteColorSpace
  
# OSX: Foundation
# qqOSX: NSArray
# OSX-NOT: initWithObjects
# OSX-NOT: familyNameios
# OSX: fontName
# OSX-NOT: NSCountedSet
# OSX: UIApplicationDelegate
# OSX-NOT: UIApplicationDelegateIOS
# OSX: NSAvailableWindowDepths
# OSX-NOT: NSAvailableWindowDepthsiOS
# OSX: NSCalibratedWhiteColorSpace