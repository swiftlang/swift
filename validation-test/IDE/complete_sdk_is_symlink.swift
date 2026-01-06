// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: ln -s %sdk %t/sdk
// RUN: ln -s %test-resource-dir %t/test_resource_dir

// RUN: %batch-code-completion -sdk %t/sdk -resource-dir %t/test_resource_dir

#^COMPLETE^#
// COMPLETE: Decl[FreeFunction]/OtherModule[Swift]/IsSystem: print
