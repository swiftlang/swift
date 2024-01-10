// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/remapped)

// RUN: split-file %s %t
// RUN: sed -e "s|TMPDIR|%/t|g" %t/vfs.json.template > %t/inputs/vfs.json

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -I %/t/inputs -Xcc -ivfsoverlay -Xcc %/t/inputs/vfs.json -vfsoverlay %/t/inputs/vfs.json
// RUN: %validate-json %t/deps.json | %FileCheck %s

//--- remapped/X.h
void funcX(void);

//--- remapped/RedirectedX.modulemap
module RedirectedX {
  header "X.h"
  export *
}

//--- vfs.json.template
{
  "case-sensitive": false,
  "version": 0,
  "roots": [
    {
      "type": "directory",
      "name": "TMPDIR/inputs",
      "contents": [
        {
          "type": "file",
          "name": "module.modulemap",
          "external-contents": "TMPDIR/remapped/RedirectedX.modulemap",
        },
        {
          "type": "file",
          "name": "X.h",
          "external-contents": "TMPDIR/remapped/X.h",
        }
      ]
    }
  ]
}

//--- test.swift
import RedirectedX

// CHECK: "clang": "RedirectedX"
// CHECK: "clang": "RedirectedX"
// CHECK:   "moduleMapPath": "{{.*}}{{/|\\}}remapped{{/|\\}}RedirectedX.modulemap",
