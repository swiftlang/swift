// RUN: %empty-directory(%t)
// RUN: sed -e "s:INPUT_DIR:%S/Inputs:g" -e "s:OUT_DIR:%t:g" %S/Inputs/vfs/vfsoverlay.yaml > %t/overlay.yaml
// RUN: sed -e "s:INPUT_DIR:%S/Inputs:g" -e "s:OUT_DIR:%t:g" %S/Inputs/vfs/secondary-vfsoverlay.yaml > %t/secondary-overlay.yaml
// RUN: sed -e "s:INPUT_DIR:%S/Inputs:g" -e "s:OUT_DIR:%t:g" %S/Inputs/vfs/tertiary-vfsoverlay.yaml > %t/tertiary-overlay.yaml

// RUN: not %target-swift-frontend -vfsoverlay %t/overlay.yaml -typecheck %s %t/mapped-file.swift 2>&1 | %FileCheck -check-prefix=BASIC_MAPPING_ERROR %s

// BASIC_MAPPING_ERROR: {{.*}}/mapped-file.swift:2:17: error:

// RUN: not %target-swift-frontend -vfsoverlay %t/overlay.yaml -vfsoverlay %t/secondary-overlay.yaml -vfsoverlay %t/tertiary-overlay.yaml -typecheck %s %t/triple-mapped-swift-file.swift 2>&1 | %FileCheck -check-prefix=COMPLEX_MAPPING_ERROR %s

// COMPLEX_MAPPING_ERROR: {{.*}}/triple-mapped-swift-file.swift:2:17: error:

// The Clang Importer should inherit Swift's VFS
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t -DTEST_VFS_CLANG_IMPORTER -import-objc-header %t/VFSMappedModule.h -vfsoverlay %t/overlay.yaml -typecheck %s

#if TEST_VFS_CLANG_IMPORTER
import VFSMappedModule
#endif
