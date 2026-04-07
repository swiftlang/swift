# Module API import remarks (ModuleAPIImport)

Emits a remark about the import bridging in each element composing the API.

## Overview

Enable using `-Rmodule-api-import`. For each declaration referenced from another module, a remark is emitted with information about the module it is imported from. If it was reexported, the original module is also noted.
