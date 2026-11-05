// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

private import Testing
@testable private import SxSAudit

@Test
internal func emission() {
  let plan = Plan(files: [
                    FilePlan(file: "package,model.dll", package: "package",
                             roots: ["Core"],
                             closure: ["Core"]),
                  ],
                  packages: [
                    PackagePlan(package: "package",
                                feature: "PackageRuntime",
                                required: ["Core"], authored: ["Core"],
                                missing: [],
                                extra: []),
                  ],
                  missing: ["missing.exe"],
                  absent: ["ide"],
                  incomplete: ["Networking"],
                  unmapped: [],
                  unused: ["Unused\"Runtime"])

  let output = report(plan)
  #expect(output == """
    record,package,subject,category,value\r
    file,package,"package,model.dll",runtime-root,Core\r
    file,package,"package,model.dll",runtime-closure,Core\r
    package,package,PackageRuntime,required,Core\r
    package,package,PackageRuntime,authored,Core\r
    diagnostic,,,missing-referenced-package-files,missing.exe\r
    diagnostic,,,missing-authored-features,ide\r
    diagnostic,,,missing-flat-runtime-assemblies,Networking\r
    diagnostic,,,unused-runtime-assemblies,"Unused""Runtime"\r

    """)
}
