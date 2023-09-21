// RUN: %batch-code-completion

struct FileDescriptor: ~#^COMPLETE^# {}
// FIXME: This should be emitting a `Copyable` declaration instead of a keyword, once there is a declaration for `Copyable`
// COMPLETE: Keyword/None:                       Copyable; name=Copyable
