// RUN: %target-typecheck-verify-swift

let _ = [#Color(colorLiteralRed: red, green: green, blue: blue, alpha: alpha)#] // expected-error {{'[#Color(...)#]' has been renamed to '#colorLiteral(...)}} {{9-10=}} {{11-16=colorLiteral}} {{17-32=red}} {{78-80=}}
let _ = [#Image(imageLiteral: localResourceNameAsString)#] // expected-error {{'[#Image(...)#]' has been renamed to '#imageLiteral(...)'}} {{9-10=}} {{11-16=imageLiteral}} {{17-29=resourceName}} {{57-59=}}
let _ = [#FileReference(fileReferenceLiteral: localResourceNameAsString)#] // expected-error {{'[#FileReference(...)#]' has been renamed to '#fileLiteral(...)'}} {{9-10=}} {{11-24=fileLiteral}} {{25-45=resourceName}} {{73-75=}}

let _ = #Color(colorLiteralRed: red, green: green, blue: blue, alpha: alpha) // expected-error {{'#Color(...)' has been renamed to '#colorLiteral(...)}} {{10-15=colorLiteral}} {{16-31=red}}
let _ = #Image(imageLiteral: localResourceNameAsString) // expected-error {{'#Image(...)' has been renamed to '#imageLiteral(...)'}} {{10-15=imageLiteral}} {{16-28=resourceName}}
let _ = #FileReference(fileReferenceLiteral: localResourceNameAsString) // expected-error {{'#FileReference(...)' has been renamed to '#fileLiteral(...)'}} {{10-23=fileLiteral}} {{24-44=resourceName}}

let _ = #notAPound // expected-error {{use of unknown directive '#notAPound'}}
let _ = #notAPound(1, 2) // expected-error {{use of unknown directive '#notAPound'}}
let _ = #Color // expected-error {{expected argument list in object literal}} {{none}}

let _ = [##] // expected-error {{expected expression in container literal}} {{none}}
let _ = [#Color(_: 1, green: 1, 2) // expected-error {{'[#Color(...)#]' has been renamed to '#colorLiteral(...)'}} {{9-10=}} {{11-16=colorLiteral}} {{17-18=red}}
let _ = [#Color(red: 1, green: 1, blue: 1)# // expected-error {{'[#Color(...)#]' has been renamed to '#colorLiteral(...)'}} {{9-10=}} {{11-16=colorLiteral}} {{17-20=red}} {{43-44=}} 
let _ = [#Color(withRed: 1, green: 1, whatever: 2)#] // expected-error {{'[#Color(...)#]' has been renamed to '#colorLiteral(...)'}} {{9-10=}} {{11-16=colorLiteral}} {{17-24=red}} {{51-53=}}
let _ = #Color(_: 1, green: 1) // expected-error {{'#Color(...)' has been renamed to '#colorLiteral(...)'}} {{10-15=colorLiteral}} {{16-17=red}}
