// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module %t/Countries.swift -module-name A -emit-module-path %t/Countries.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown %t/Without-package-import.swift -I %t
// RUN: %target-swift-frontend -typecheck -package-modules Countries -verify -verify-ignore-unknown %t/With-package-import.swift -I %t
// RUN: %target-swift-frontend -typecheck -package-modules Countries -verify -verify-ignore-unknown %t/With-package-import-pass.swift -I %t


// BEGIN Countries.swift

public class Portugal {
   public var region: String = "West"

   @package
   public init() {}
   @package
   public func city() -> String {
       return "Lisbon"
   }
}

@package
public struct Spain {
    public var cities: Array<String>?
    public init() {}
    public func showCities() { }
}

@package
public enum Region {
  case west, east, north, south
}

public struct CountryPicker {
  public var countries = [String]()

  public subscript(index: Int) -> String? {
    get {
      if index < countries.count, index >= 0 {
        return countries[index]
      }
      return nil
    }
    set(newValue) {
      countries[index] = newValue
    }
 }
}


// BEGIN Without-package-import.swift
import Countries

public func showSpain() -> Spain {
  let s = Spain()
  s.showCities() // expected-error{{cannot find 'Spain' in scope}}
  return s
}

public func showPortugal() -> Portugal {
  let p = Portugal()
  let _ = p.region
  let _ = p.city() // expected-error{{'city' is  inaccessible due to '@package' protection level}}
  return p
}

// BEGIN With-package-import.swift
import Countries // -package-modules Countries is passed

public func showSpain() -> Spain { // expected-error{{cannot use struct 'Spain' here; it is a package type imported from 'Countries'}}
    let s = Spain()
    s.showCities()
    return s
}

public func showRegion() -> Region { // expected-error{{cannot use struct 'Region' here; it is a package type imported from 'Countries'}}
  return Region.west
}

func showInternalSpain() -> Spain { // pass since it's an internal decl
  return Spain()
}

@usableFromInline
func usableFromInlineCountires() { // pass
  let _ = Spain()
  let _ = Portugal().region
  let _ = Portugal().city()
}

@inlinable
func inlineCountries() {
  let _ = Spain() // expected-error{{struct 'Spain' cannot be used in an '@inlinable' function because it is a package type imported from 'Countries'}}

  let _ = Portugal().region // pass
  let _ = Portugal().city() // expected-error{{instance method 'city()' cannot be used in an '@inlinable' function because it is a package type imported from 'Countries'}}
}

// BEGIN With-package-import-pass.swift
import Countries // -package-modules Countries is passed

@package
public func showSpain() -> Spain { // pass: @package is added since Spain is a package type
    let s = Spain()
    s.showCities()
    return s
}

public func showPortugal() -> Portugal { // pass
  let p = Portugal()
  let eu = p.region
  let lisbon = p.city()
  print(eu, lisbon)
  return p
}
