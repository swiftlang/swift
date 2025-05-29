// RUN: %batch-code-completion

// rdar://139212286 - We should only suggest invertible types here.
struct FileDescriptor: ~#^COMPLETE^# {}
// COMPLETE:     Begin completions, 2 items
// COMPLETE-DAG: Decl[Protocol]/OtherModule[Swift]/IsSystem: Copyable[#Copyable#]; name=Copyable
// COMPLETE-DAG: Decl[Protocol]/OtherModule[Swift]/IsSystem: Escapable[#Escapable#]; name=Escapable
