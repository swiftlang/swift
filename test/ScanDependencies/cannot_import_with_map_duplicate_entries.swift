// UNSUPPORTED: OS=windows-msvc
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/inputs
// RUN: split-file %s %t
// RUN: sed -e "s|INPUTDIR|%t|g" -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/inputs/valid_map.json.template > %t/inputs/valid_map.json
// RUN: sed -e "s|INPUTDIR|%t|g" -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/inputs/map_with_duped_swift_module.json.template > %t/inputs/map_with_duped_swift_module.json
// RUN: sed -e "s|INPUTDIR|%t|g" -e "s|SWIFTLIBDIR|%swift-lib-dir|g" %t/inputs/map_with_duped_clang_module.json.template > %t/inputs/map_with_duped_clang_module.json
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/inputs/Foo.swiftmodule -emit-module-doc-path %t/inputs/Foo.swiftdoc -emit-module-source-info -emit-module-source-info-path %t/inputs/Foo.swiftsourceinfo -module-cache-path %t.module-cache %t/foo.swift -module-name Foo
// RUN: %target-swift-emit-pcm -module-name SwiftShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/SwiftShims.pcm
// RUN: %target-swift-emit-pcm -module-name _SwiftConcurrencyShims %swift-lib-dir/swift/shims/module.modulemap -o %t/inputs/_SwiftConcurrencyShims.pcm

// RUN: %target-swift-frontend -typecheck %t/foo.swift -explicit-swift-module-map-file %t/inputs/valid_map.json
// RUN: not %target-swift-frontend -typecheck %t/foo.swift -explicit-swift-module-map-file %t/inputs/map_with_duped_swift_module.json
// RUN: not %target-swift-frontend -typecheck %t/foo.swift -explicit-swift-module-map-file %t/inputs/map_with_duped_clang_module.json

//--- foo.swift
public func foo() {}

//--- inputs/valid_map.json.template
[{
	"moduleName": "Foo",
	"modulePath": "INPUTDIR/inputs/Foo.swiftmodule",
	"docPath": "INPUTDIR/inputs/Foo.swiftdoc",
	"sourceInfoPath": "INPUTDIR/inputs/Foo.swiftsourceinfo",
	"isFramework": false
},
{
	"moduleName": "SwiftShims",
	"isFramework": false,
	"clangModuleMapPath": "SWIFTLIBDIR/swift/shims/module.modulemap",
	"clangModulePath": "INPUTDIR/inputs/SwiftShims.pcm"
}]

//--- inputs/map_with_duped_swift_module.json.template
[{
	"moduleName": "Foo",
	"modulePath": "INPUTDIR/inputs/Foo.swiftmodule",
	"docPath": "INPUTDIR/inputs/Foo.swiftdoc",
	"sourceInfoPath": "INPUTDIR/inputs/Foo.swiftsourceinfo",
	"isFramework": false
},
{

	"moduleName": "Foo",
	"modulePath": "INPUTDIR/inputs/Foo.swiftmodule",
	"docPath": "INPUTDIR/inputs/Foo.swiftdoc",
	"sourceInfoPath": "INPUTDIR/inputs/Foo.swiftsourceinfo",
	"isFramework": false
},
{
	"moduleName": "SwiftShims",
	"isFramework": false,
	"clangModuleMapPath": "SWIFTLIBDIR/swift/shims/module.modulemap",
	"clangModulePath": "INPUTDIR/inputs/SwiftShims.pcm"
}]

//--- inputs/map_with_duped_clang_module.json.template
[{
	"moduleName": "Foo",
	"modulePath": "INPUTDIR/inputs/Foo.swiftmodule",
	"docPath": "INPUTDIR/inputs/Foo.swiftdoc",
	"sourceInfoPath": "INPUTDIR/inputs/Foo.swiftsourceinfo",
	"isFramework": false
},
{
	"moduleName": "SwiftShims",
	"isFramework": false,
	"clangModuleMapPath": "SWIFTLIBDIR/swift/shims/module.modulemap",
	"clangModulePath": "INPUTDIR/inputs/SwiftShims.pcm"
},
{
	"moduleName": "SwiftShims",
	"isFramework": false,
	"clangModuleMapPath": "SWIFTLIBDIR/swift/shims/module.modulemap",
	"clangModulePath": "INPUTDIR/inputs/SwiftShims.pcm"
}]
