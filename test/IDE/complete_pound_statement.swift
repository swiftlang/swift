// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLATFORM1 | %FileCheck %s -check-prefix=PLATFORM1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLATFORM2 | %FileCheck %s -check-prefix=PLATFORM1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLATFORM3 | %FileCheck %s -check-prefix=PLATFORM1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLATFORM4 | %FileCheck %s -check-prefix=PLATFORM1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAIL1 | %FileCheck %s -check-prefix=AVAILABLE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAIL2 | %FileCheck %s -check-prefix=AVAILABLE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAIL3 | %FileCheck %s -check-prefix=AVAILABLE1

{
  if #available(#^PLATFORM1^#
}
{
  guard #available(OSX >= 10.9, #^PLATFORM2^#
}
{
  while #available(#^PLATFORM3^#
}
{
  #available(iOS >= 7.0, #^PLATFORM4^#
}
// PLATFORM1: Begin completions
// PLATFORM1-DAG: Keyword/None:                       *[#Platform#]; name=*{{$}}
// PLATFORM1-DAG: Keyword/None:                       iOS[#Platform#]; name=iOS{{$}}
// PLATFORM1-DAG: Keyword/None:                       tvOS[#Platform#]; name=tvOS{{$}}
// PLATFORM1-DAG: Keyword/None:                       watchOS[#Platform#]; name=watchOS{{$}}
// PLATFORM1-DAG: Keyword/None:                       macOS[#Platform#]; name=macOS{{$}}
// PLATFORM1-DAG: Keyword/None:                       iOSApplicationExtension[#Platform#]; name=iOSApplicationExtension{{$}}
// PLATFORM1-DAG: Keyword/None:                       tvOSApplicationExtension[#Platform#]; name=tvOSApplicationExtension{{$}}
// PLATFORM1-DAG: Keyword/None:                       watchOSApplicationExtension[#Platform#]; name=watchOSApplicationExtension{{$}}
// PLATFORM1-DAG: Keyword/None:                       macOSApplicationExtension[#Platform#]; name=macOSApplicationExtension{{$}}
// PLATFORM1-DAG: Keyword/None:                       macCatalyst[#Platform#]; name=macCatalyst
// PLATFORM1-DAG: Keyword/None:                       macCatalystApplicationExtension[#Platform#]; name=macCatalystApplicationExtension
// PLATFORM1: End completions

class C1 {
  func foo() {
    if ##^AVAIL1^# {
    }
  }
}

class C2 {
  func foo() {
    guard ##^AVAIL2^# {
    }
  }
}

class C3 {
  func foo() {
    while ##^AVAIL3^# {
    }
  }
}

// AVAILABLE: Keyword/ExprSpecific:               available({#Platform...#}, *); name=available(Platform..., *)
// AVAILABLE1-NOT: available({#Platform...#}, *)
