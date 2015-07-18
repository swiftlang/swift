// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLATFORM1 | FileCheck %s -check-prefix=PLATFORM1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLATFORM2 | FileCheck %s -check-prefix=PLATFORM1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLATFORM3 | FileCheck %s -check-prefix=PLATFORM1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLATFORM4 | FileCheck %s -check-prefix=PLATFORM1
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
// PLATFORM1: Keyword/None:                       *[#Platform#]; name=*{{$}}
// PLATFORM1: Keyword/None:                       iOS[#Platform#]; name=iOS{{$}}
// PLATFORM1: Keyword/None:                       tvOS[#Platform#]; name=tvOS{{$}}
// PLATFORM1: Keyword/None:                       watchOS[#Platform#]; name=watchOS{{$}}
// PLATFORM1: Keyword/None:                       OSX[#Platform#]; name=OSX{{$}}
