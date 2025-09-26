// RUN: %target-swift-ide-test -signature-help -code-completion-token=RAW_ID -source-filename=%s | %FileCheck %s --check-prefix=RAW_ID

struct `Raw Identifier` {
  func `some method :)`(`argument label!` `param label?`: Int) {}
}

`Raw Identifier`().`some method :)`(#^RAW_ID^#)
// RAW_ID:     Begin signatures, 1 items
// RAW_ID-DAG: Signature[Active]: `some method :)`(<param name="param label?" active>`argument label!`: Int</param>)
