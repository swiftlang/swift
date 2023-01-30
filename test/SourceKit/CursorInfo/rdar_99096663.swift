// This should not crash
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=4:11 %s -- %s 

extension RandomAccessCollection 
