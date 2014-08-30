// RUN: rm -rf %t && mkdir %t
// RUN: %swift %s -emit-silgen | FileCheck %s

// RUN: cp %s %t
// RUN: %swift %t/mangling_private.swift -emit-silgen | FileCheck %s

// RUN: cp %s %t/other_name.swift
// RUN: %swift %t/other_name.swift -emit-silgen -module-name mangling_private | FileCheck %s -check-prefix=OTHER-NAME

// CHECK-LABEL: sil @_TF16mangling_privateP33_713AFCDB29B710C2AB6F4DF7C1C8FEC911privateFuncFT_Si
// OTHER-NAME-LABEL: sil @_TF16mangling_privateP33_AD9C6D430861F1E1D66B54DBCA7CC94B11privateFuncFT_Si
private func privateFunc() -> Int {
  return 0
}

public struct PublicStruct {
  // CHECK-LABEL: sil @_TFV16mangling_private12PublicStructP33_713AFCDB29B710C2AB6F4DF7C1C8FEC913privateMethodfMS0_FT_T_
  private static func privateMethod() {}
}

public struct InternalStruct {
  // CHECK-LABEL: sil @_TFV16mangling_private14InternalStructP33_713AFCDB29B710C2AB6F4DF7C1C8FEC913privateMethodfMS0_FT_T_
  private static func privateMethod() {}
}

private struct PrivateStruct {
  // CHECK-LABEL: sil @_TFV16mangling_privateP33_713AFCDB29B710C2AB6F4DF7C1C8FEC913PrivateStruct13privateMethodfMS0_FT_T_
  private static func privateMethod() {}

  struct Inner {
    // CHECK-LABEL: sil @_TFVV16mangling_privateP33_713AFCDB29B710C2AB6F4DF7C1C8FEC913PrivateStruct5Inner13privateMethodfMS1_FT_T_
    private static func privateMethod() {}
  }
}

func localTypes() {
  struct LocalStruct {
    // CHECK-LABEL: sil shared @_TFVF16mangling_private10localTypesFT_T_L_11LocalStruct13privateMethodfMS0_FT_T_
    private static func privateMethod() {}
  }
}

extension PublicStruct {
  // CHECK-LABEL: sil @_TFV16mangling_private12PublicStructP33_713AFCDB29B710C2AB6F4DF7C1C8FEC916extPrivateMethodfS0_FT_T_
  private func extPrivateMethod() {}
}
extension PrivateStruct {
  // CHECK-LABEL: sil @_TFV16mangling_privateP33_713AFCDB29B710C2AB6F4DF7C1C8FEC913PrivateStruct16extPrivateMethodfS0_FT_T_
  private func extPrivateMethod() {}
}

