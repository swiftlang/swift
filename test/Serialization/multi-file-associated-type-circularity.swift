// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/a.swiftmodule -primary-file %S/Inputs/circular-associated-type/a.swift %S/Inputs/circular-associated-type/b.swift %S/Inputs/circular-associated-type/c.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/b.swiftmodule -primary-file %S/Inputs/circular-associated-type/b.swift %S/Inputs/circular-associated-type/a.swift %S/Inputs/circular-associated-type/c.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/c.swiftmodule -primary-file %S/Inputs/circular-associated-type/c.swift %S/Inputs/circular-associated-type/a.swift %S/Inputs/circular-associated-type/b.swift

// RUN: %target-swift-frontend -parse-as-library -emit-module -module-name Multi %t/a.swiftmodule %t/b.swiftmodule %t/c.swiftmodule -o %t

