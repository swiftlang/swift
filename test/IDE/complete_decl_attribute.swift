// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY1 | FileCheck %s -check-prefix=AVAILABILITY1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY2 | FileCheck %s -check-prefix=AVAILABILITY2

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
