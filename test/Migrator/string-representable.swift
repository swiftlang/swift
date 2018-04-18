// REQUIRES: objc_interop
// RUN: %empty-directory(%t.mod)
// RUN: %target-swift-frontend -emit-module -o %t.mod/Cities.swiftmodule %S/Inputs/Cities.swift -module-name Cities -parse-as-library
// RUN: %empty-directory(%t) && %target-swift-frontend -c -update-code -primary-file %s  -I %t.mod -api-diff-data-file %S/Inputs/string-representable.json -emit-migrated-file-path %t/string-representable.swift.result -disable-migrator-fixits -o /dev/null
// RUN: diff -u %S/string-representable.swift.expected %t/string-representable.swift.result

import Cities

func foo(_ c: Container) -> String {
  c.Value = ""
  c.addingAttributes(["a": "b", "a": "b", "a": "b"])
  c.addingAttributes(["a": "b", "a": "b", "a": "b"])
  c.adding(attributes: ["a": 1, "a": 2, "a": 3])
  c.adding(optionalAttributes: ["a": 1, "a": 2, "a": 3])
  _ = Container(optionalAttributes: nil)
  _ = Container(optionalAttrArray: nil)
  c.adding(attrArray: ["key1", "key2"])
  c.add(single: "")
  c.add(singleOptional: nil)
  _ = c.getAttrDictionary()
  _ = c.getOptionalAttrDictionary()
  _ = c.getSingleAttr()
  _ = c.getOptionalSingleAttr()
  _ = c.getAttrArray()
  _ = c.getOptionalAttrArray()

  c.addingAttributes(c.getAttrDictionary())
  c.adding(optionalAttributes: c.getAttrDictionary())

  c.attrDict = ["a": "b", "a": "b", "a": "b"]
  c.attrArr = ["key1", "key2"]
  _ = c.attrArr
  _ = c.attrDict
  c.adding(attributes: c.attrDict)
  _ = Container(optionalAttrArray: c.attrArr)
  c.adding(optionalAttributes: c.optionalAttrDict)
  return c.Value
}
