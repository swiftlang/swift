enum LogLevel { case error }

func logAsync(level: LogLevel = undefined, messageProducer producer

// RUN: %sourcekitd-test -req=cursor -pos=3:44 %s -- %s | %FileCheck %s

// CHECK: source.lang.swift.decl.function.free (3:6-3:68)
// CHECK: logAsync(level:messageProducer:)
// CHECK: LogLevel</Type> = &lt;&lt;empty&gt;&gt
