// REQUIRES: shell
// Verify that the top-level build record file from the last incremental
// compilation is preserved with the same name, suffixed by a '~'.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way-fine/* %t
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*
// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -c ./main.swift ./other.swift -module-name main -incremental -disable-direct-intramodule-dependencies -v -driver-show-incremental -disable-direct-intramodule-dependencies -output-file-map %t/output.json

// RUN: %FileCheck -check-prefix CHECK-ORIGINAL %s < main~buildrecord.swiftdeps~
// CHECK-ORIGINAL: inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}

// RUN: %FileCheck -check-prefix CHECK-OVERWRITTEN %s < main~buildrecord.swiftdeps
// CHECK-OVERWRITTEN: version: "{{.*}}"
// CHECK-OVERWRITTEN: options: "{{.*}}"
// CHECK-OVERWRITTEN: build_time: [{{[0-9]*}}, {{[0-9]*}}]
// CHECK-OVERWRITTEN: inputs:
// CHECK-OVERWRITTEN: "./main.swift": [443865900, 0]
// CHECK-OVERWRITTEN: "./other.swift": [443865900, 0]
