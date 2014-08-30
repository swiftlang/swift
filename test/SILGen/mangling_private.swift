// RUN: rm -rf %t && mkdir %t
// RUN: %swift -emit-module -o %t %S/Inputs/mangling_private_helper.swift -enable-private-discriminators
// RUN: %swift -emit-silgen %S/Inputs/mangling_private_helper.swift -enable-private-discriminators | FileCheck %s -check-prefix=CHECK-BASE

// RUN: %swift %s -I %t -emit-silgen -enable-private-discriminators | FileCheck %s

// RUN: cp %s %t
// RUN: %swift %t/mangling_private.swift -I %t -emit-silgen -enable-private-discriminators | FileCheck %s

// RUN: cp %s %t/other_name.swift
// RUN: %swift %t/other_name.swift -I %t -emit-silgen -enable-private-discriminators -module-name mangling_private | FileCheck %s -check-prefix=OTHER-NAME

import mangling_private_helper

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


// CHECK-LABEL: sil_vtable Sub {
class Sub : Base {
  // CHECK-BASE: #Base.privateMethod!1: _TFC23mangling_private_helper4BaseP33_4EEA0BDF28CD79DFD969F8CFAF130D3813privateMethodfS0_FT_T_
  // CHECK-DAG: #Base.privateMethod!1: _TFC23mangling_private_helper4BaseP33_4EEA0BDF28CD79DFD969F8CFAF130D3813privateMethodfS0_FT_T_

  // CHECK-DAG: #Sub.subMethod!1: _TFC16mangling_private3SubP33_713AFCDB29B710C2AB6F4DF7C1C8FEC99subMethodfS0_FT_T_
  private func subMethod() {}
} // CHECK: {{^[}]$}}

