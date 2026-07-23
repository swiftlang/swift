// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

private import Testing
@testable private import SxSAudit

@Test
internal func packageMappings() throws {
  let inputs = try packages(files: [
                              "build=compiler.exe", "build=Support.dll",
                              "package=package.exe",
                            ],
                            features: [
                              "build=BuildRuntime", "package=PackageRuntime",
                            ],
                            authored: [
                              "build=Core", "package=Networking",
                            ])

  #expect(inputs.count == 2)
  #expect(inputs[0].name == "build")
  #expect(inputs[0].files["compiler.exe"] == "compiler.exe")
  #expect(inputs[0].files["support.dll"] == "Support.dll")
  #expect(inputs[0].feature == "BuildRuntime")
  #expect(inputs[0].authored == ["Core"])
  #expect(inputs[1].name == "package")
  #expect(inputs[1].feature == "PackageRuntime")
  #expect(inputs[1].authored == ["Networking"])
}

@Test
internal func planningMappings() throws {
  let inputs = try packages(files: ["build=compiler.exe"], features: [],
                            authored: [])

  #expect(inputs[0].feature == nil)
  #expect(inputs[0].authored == nil)
}

@Test
internal func emptyFeature() throws {
  let inputs = try packages(files: ["build=compiler.exe"],
                            features: ["build=BuildRuntime"],
                            authored: [])

  #expect(inputs[0].feature == "BuildRuntime")
  #expect(inputs[0].authored?.isEmpty == true)
}
