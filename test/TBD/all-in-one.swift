// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t)

// RUN: %gyb %S/class_objc.swift.gyb > %t/class_objc.swift
// RUN: %gyb %S/extension.swift.gyb > %t/extension.swift
// RUN: %gyb %S/subclass.swift.gyb > %t/subclass.swift

// RUN: %target-build-swift %S/Inputs/extension_types.swift -module-name ExtensionTypes -emit-module -emit-module-path %t/ExtensionTypes.swiftmodule
// RUN: %target-build-swift %S/Inputs/subclass_super.swift -emit-library -emit-module -o %t/subclass_super%{target-shared-library-suffix} -enable-library-evolution

// RUN: %target-build-swift -module-name all_in_one -emit-module-path %t/all_in_one.swiftmodule -emit-tbd-path %t/incremental_Onone.tbd %S/class.swift %t/class_objc.swift %S/enum.swift %t/extension.swift %S/function.swift %S/global.swift %S/main.swift %S/protocol.swift %S/struct.swift %t/subclass.swift -I %t -import-objc-header %S/Inputs/objc_class_header.h  -Xfrontend -disable-objc-attr-requires-foundation-module -Xfrontend -tbd-install_name -Xfrontend all_in_one
// RUN: %target-build-swift -module-name all_in_one -emit-module-path %t/all_in_one.swiftmodule -emit-tbd-path %t/wmo_Onone.tbd %S/class.swift %t/class_objc.swift %S/enum.swift %t/extension.swift %S/function.swift %S/global.swift %S/main.swift %S/protocol.swift %S/struct.swift %t/subclass.swift -I %t -import-objc-header %S/Inputs/objc_class_header.h -Xfrontend -disable-objc-attr-requires-foundation-module -wmo -Xfrontend -tbd-install_name -Xfrontend all_in_one

// RUN: %target-build-swift -module-name all_in_one -emit-module-path %t/all_in_one.swiftmodule -emit-tbd-path %t/incremental_O.tbd %S/class.swift %t/class_objc.swift %S/enum.swift %t/extension.swift %S/function.swift %S/global.swift %S/main.swift %S/protocol.swift %S/struct.swift %t/subclass.swift -I %t -import-objc-header %S/Inputs/objc_class_header.h -Xfrontend -disable-objc-attr-requires-foundation-module -Xfrontend -tbd-install_name -Xfrontend all_in_one -O
// RUN: %target-build-swift -module-name all_in_one -emit-module-path %t/all_in_one.swiftmodule -emit-tbd-path %t/wmo_O.tbd %S/class.swift %t/class_objc.swift %S/enum.swift %t/extension.swift %S/function.swift %S/global.swift %S/main.swift %S/protocol.swift %S/struct.swift %t/subclass.swift -I %t -import-objc-header %S/Inputs/objc_class_header.h -Xfrontend -disable-objc-attr-requires-foundation-module -wmo -O -Xllvm -sil-disable-pass=cmo -Xfrontend -tbd-install_name -Xfrontend all_in_one

// RUN: %llvm-readtapi --compare %t/incremental_Onone.tbd %t/wmo_Onone.tbd
// RUN: %llvm-readtapi --compare %t/incremental_O.tbd %t/wmo_O.tbd
// RUN: %llvm-readtapi --compare %t/incremental_Onone.tbd %t/incremental_O.tbd

// REQUIRES: objc_interop
