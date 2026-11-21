// RUN: %batch-code-completion -filecheck-additional-suffix _DISABLED
// RUN: %batch-code-completion -filecheck-additional-suffix _ENABLED -enable-experimental-feature NontransferableTypes

// REQUIRES: swift_feature_NontransferableTypes

// rdar://139212286 - We should only suggest invertible types here.
struct FileDescriptor: ~#^COMPLETE^# {}
// COMPLETE_DISABLED: Begin completions, 2 items
// COMPLETE_ENABLED:  Begin completions, 3 items
// COMPLETE-DAG: Decl[Protocol]/OtherModule[Swift]/IsSystem: Copyable[#Copyable#]; name=Copyable
// COMPLETE-DAG: Decl[Protocol]/OtherModule[Swift]/IsSystem: Escapable[#Escapable#]; name=Escapable
// COMPLETE_ENABLED-DAG: Decl[Protocol]/OtherModule[Swift]/IsSystem: Transferable[#Transferable#]; name=Transferable
