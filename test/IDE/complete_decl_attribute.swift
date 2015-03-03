// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY1 | FileCheck %s -check-prefix=AVAILABILITY1

@availability(#^AVAILABILITY1^#
// AVAILABILITY1:        Begin completions, 4 items
// AVAILABILITY1-NEXT:   Keyword/None:                       *[#Platform#]; name=*{{$}}
// AVAILABILITY1-NEXT:   Keyword/None:                       iOS[#Platform#]; name=iOS{{$}}
// AVAILABILITY1-NEXT:   Keyword/None:                       iOSApplicationExtension[#Platform#]; name=iOSApplicationExtension{{$}}
// AVAILABILITY1-NEXT:   Keyword/None:                       OSX[#Platform#]; name=OSX{{$}}
// AVAILABILITY1-NEXT:   End completions

