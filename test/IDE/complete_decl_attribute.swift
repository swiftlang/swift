// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY1 | FileCheck %s -check-prefix=AVAILABILITY1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY2 | FileCheck %s -check-prefix=AVAILABILITY2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD1 | FileCheck %s -check-prefix=KEYWORD1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD2 | FileCheck %s -check-prefix=KEYWORD2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD3 | FileCheck %s -check-prefix=KEYWORD3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD4 | FileCheck %s -check-prefix=KEYWORD4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD5 | FileCheck %s -check-prefix=KEYWORD5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=KEYWORD_LAST | FileCheck %s -check-prefix=KEYWORD_LAST

@availability(#^AVAILABILITY1^#)

// AVAILABILITY1: Begin completions, 9 items
// AVAILABILITY1-NEXT: Keyword/None:                       *[#Platform#]; name=*{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       iOS[#Platform#]; name=iOS{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       tvOS[#Platform#]; name=tvOS{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       watchOS[#Platform#]; name=watchOS{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       OSX[#Platform#]; name=OSX{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       iOSApplicationExtension[#Platform#]; name=iOSApplicationExtension{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       tvOSApplicationExtension[#Platform#]; name=tvOSApplicationExtension{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       watchOSApplicationExtension[#Platform#]; name=watchOSApplicationExtension{{$}}
// AVAILABILITY1-NEXT: Keyword/None:                       OSXApplicationExtension[#Platform#]; name=OSXApplicationExtension{{$}}
// AVAILABILITY1-NEXT: End completions

@availability(*, #^AVAILABILITY2^#)

// AVAILABILITY2:             Begin completions, 5 items
// AVAILABILITY2-NEXT:        Keyword/None:                       unavailable; name=unavailable{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       message=[#Specify message#]; name=message{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       renamed=[#Specify replacing name#]; name=renamed{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       introduced=[#Specify version number#]; name=introduced{{$}}
// AVAILABILITY2-NEXT:        Keyword/None:                       deprecated=[#Specify version number#]; name=deprecated{{$}}
// AVAILABILITY2-NEXT:        End completions



func method(@#^KEYWORD1^#) {}

// KEYWORD1:                  Begin completions, 2 items
// KEYWORD1-NEXT:             Keyword/None:                       autoclosure[#Param Attribute#]; name=autoclosure{{$}}
// KEYWORD1-NEXT:             Keyword/None:                       noescape[#Param Attribute#]; name=noescape{{$}}
// KEYWORD1-NEXT:             End completions

@#^KEYWORD2^#
func method(){}

// KEYWORD2:                  Begin completions, 5 items
// KEYWORD2-NEXT:             Keyword/None:                       availability[#Func Attribute#]; name=availability{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       objc[#Func Attribute#]; name=objc{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       noreturn[#Func Attribute#]; name=noreturn{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       IBAction[#Func Attribute#]; name=IBAction{{$}}
// KEYWORD2-NEXT:             Keyword/None:                       inline[#Func Attribute#]; name=inline{{$}}
// KEYWORD2-NEXT:             End completions

@#^KEYWORD3^#
class C {}

// KEYWORD3:                  Begin completions, 7 items
// KEYWORD3-NEXT:             Keyword/None:                       availability[#Class Attribute#]; name=availability{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       objc[#Class Attribute#]; name=objc{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       IBDesignable[#Class Attribute#]; name=IBDesignable{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       UIApplicationMain[#Class Attribute#]; name=UIApplicationMain{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       requires_stored_property_inits[#Class Attribute#]; name=requires_stored_property_inits{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       NSApplicationMain[#Class Attribute#]; name=NSApplicationMain{{$}}
// KEYWORD3-NEXT:             Keyword/None:                       objc_non_lazy_realization[#Class Attribute#]; name=objc_non_lazy_realization{{$}}
// KEYWORD3-NEXT:             End completions

@#^KEYWORD4^#
enum E {}
// KEYWORD4:                  Begin completions, 2 items
// KEYWORD4-NEXT:             Keyword/None:                       availability[#Enum Attribute#]; name=availability{{$}}
// KEYWORD4-NEXT:             Keyword/None:                       objc[#Enum Attribute#]; name=objc{{$}}
// KEYWORD4-NEXT:             End completions


@#^KEYWORD5^#
struct S{}
// KEYWORD5:                  Begin completions, 1 items
// KEYWORD5-NEXT:             Keyword/None:                       availability[#Struct Attribute#]; name=availability{{$}}
// KEYWORD5-NEXT:             End completions


@#^KEYWORD_LAST^#

// KEYWORD_LAST:                  Begin completions, 16 items
// KEYWORD_LAST-NEXT:             Keyword/None:                       availability[#Declaration Attribute#]; name=availability{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       objc[#Declaration Attribute#]; name=objc{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       noreturn[#Declaration Attribute#]; name=noreturn{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       NSCopying[#Declaration Attribute#]; name=NSCopying{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       IBAction[#Declaration Attribute#]; name=IBAction{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       IBDesignable[#Declaration Attribute#]; name=IBDesignable{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       IBInspectable[#Declaration Attribute#]; name=IBInspectable{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       IBOutlet[#Declaration Attribute#]; name=IBOutlet{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       NSManaged[#Declaration Attribute#]; name=NSManaged{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       UIApplicationMain[#Declaration Attribute#]; name=UIApplicationMain{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       inline[#Declaration Attribute#]; name=inline{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       requires_stored_property_inits[#Declaration Attribute#]; name=requires_stored_property_inits{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       autoclosure[#Declaration Attribute#]; name=autoclosure{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       noescape[#Declaration Attribute#]; name=noescape{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       NSApplicationMain[#Declaration Attribute#]; name=NSApplicationMain{{$}}
// KEYWORD_LAST-NEXT:             Keyword/None:                       objc_non_lazy_realization[#Declaration Attribute#]; name=objc_non_lazy_realization{{$}}
// KEYWORD_LAST-NEXT:             End completions
