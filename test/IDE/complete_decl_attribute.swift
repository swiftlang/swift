// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY1 | FileCheck %s -check-prefix=AVAILABILITY1

@availability(#^AVAILABILITY1^#

// AVAILABILITY1:        Begin completions, 5 items
// AVAILABILITY1-NEXT:   Keyword/None:                       *[#Any platform#]; name=*{{$}}
// AVAILABILITY1-NEXT:   Keyword/None:                       iOS[#iOS#]; name=iOS{{$}}
// AVAILABILITY1-NEXT:   Keyword/None:                       OSX[#OS X#]; name=OSX{{$}}
// AVAILABILITY1-NEXT:   Keyword/None:                       iOSApplicationExtension[#iOS application extension#]; name=iOSApplicationExtension{{$}}
// AVAILABILITY1-NEXT:   Keyword/None:                       OSXApplicationExtension[#OS X application extension#]; name=OSXApplicationExtension{{$}}
// AVAILABILITY1-NEXT:   End completions
