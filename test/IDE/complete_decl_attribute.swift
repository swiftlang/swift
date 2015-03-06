// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY1 | FileCheck %s -check-prefix=AVAILABILITY1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY2 | FileCheck %s -check-prefix=AVAILABILITY2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD1 | FileCheck %s -check-prefix=KEYWORD1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD2 | FileCheck %s -check-prefix=KEYWORD2

@availability(#^AVAILABILITY1^#

// AVAILABILITY1:             Begin completions, 5 items
// AVAILABILITY1-NEXT:        Keyword/None:                       *[#Platform#]; name=*{{$}}
// AVAILABILITY1-NEXT:        Keyword/None:                       iOS[#Platform#]; name=iOS{{$}}
// AVAILABILITY1-NEXT:        Keyword/None:                       OSX[#Platform#]; name=OSX{{$}}
// AVAILABILITY1-NEXT:        Keyword/None:                       iOSApplicationExtension[#Platform#]; name=iOSApplicationExtension{{$}}
// AVAILABILITY1-NEXT:        Keyword/None:                       OSXApplicationExtension[#Platform#]; name=OSXApplicationExtension{{$}}
// AVAILABILITY1-NEXT:        End completions

@availability(*, #^AVAILABILITY2^#

// AVAILABILITY2:             Begin completions, 5 items
// AVAILABILITY2-NEXT:        Keyword/None:                       unavailable; name=unavailable{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       message=[#Specify message#]; name=message{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       renamed=[#Specify replacing name#]; name=renamed{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       introduced=[#Specify version number#]; name=introduced{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       deprecated=[#Specify version number#]; name=deprecated{{$}}
// AVAILABILITY2-NEXT:        End completions

@#^KEYWORD1^#

// KEYWORD1:                  Begin completions, 16 items
// KEYWORD1-NEXT:             Keyword/None:                       availability[#Declaration Attribute#]; name=availability
// KEYWORD1-NEXT:             Keyword/None:                       objc[#Declaration Attribute#]; name=objc
// KEYWORD1-NEXT:             Keyword/None:                       noreturn[#Declaration Attribute#]; name=noreturn
// KEYWORD1-NEXT:             Keyword/None:                       NSCopying[#Declaration Attribute#]; name=NSCopying
// KEYWORD1-NEXT:             Keyword/None:                       IBAction[#Declaration Attribute#]; name=IBAction
// KEYWORD1-NEXT:             Keyword/None:                       IBDesignable[#Declaration Attribute#]; name=IBDesignable
// KEYWORD1-NEXT:             Keyword/None:                       IBInspectable[#Declaration Attribute#]; name=IBInspectable
// KEYWORD1-NEXT:             Keyword/None:                       IBOutlet[#Declaration Attribute#]; name=IBOutlet
// KEYWORD1-NEXT:             Keyword/None:                       NSManaged[#Declaration Attribute#]; name=NSManaged
// KEYWORD1-NEXT:             Keyword/None:                       UIApplicationMain[#Declaration Attribute#]; name=UIApplicationMain
// KEYWORD1-NEXT:             Keyword/None:                       objc_non_lazy_realization[#Declaration Attribute#]; name=objc_non_lazy_realization
// KEYWORD1-NEXT:             Keyword/None:                       inline[#Declaration Attribute#]; name=inline
// KEYWORD1-NEXT:             Keyword/None:                       requires_stored_property_inits[#Declaration Attribute#]; name=requires_stored_property_inits
// KEYWORD1-NEXT:             Keyword/None:                       autoclosure[#Declaration Attribute#]; name=autoclosure
// KEYWORD1-NEXT:             Keyword/None:                       noescape[#Declaration Attribute#]; name=noescape
// KEYWORD1-NEXT:             Keyword/None:                       NSApplicationMain[#Declaration Attribute#]; name=NSApplicationMain
// KEYWORD1-NEXT:             End completions

func method(@#^KEYWORD2^#

// KEYWORD2:                  Begin completions, 2 items
// KEYWORD2-NEXT:             Keyword/None:                       autoclosure[#Parameter Attribute#]; name=autoclosure{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       noescape[#Parameter Attribute#]; name=noescape{{$}}
// KEYWORD2-NEXT:             End completions