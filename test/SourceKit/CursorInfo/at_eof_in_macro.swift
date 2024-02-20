// RUN: %sourcekitd-test -req=cursor -pos=3:37 %s -- %s
@freestanding(expression)
macro powerAssert() = #externalMacro