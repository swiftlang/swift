# Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
# SPDX-License-Identifier: BSD-3-Clause

param
(
  [Parameter(Mandatory = $true)]
  [string] $ToolchainRoot,

  [string] $RuntimeRoot,

  [string] $InstallerRoot,

  [string] $Swift = 'swift'
)

$ErrorActionPreference = 'Stop'

if (!$InstallerRoot) {
  $repository = Join-Path $PSScriptRoot '..\..\..'
  $InstallerRoot = Join-Path $repository 'swift-installer-scripts\platforms\Windows'
}

if (!$RuntimeRoot) {
  $version = (Split-Path $ToolchainRoot -Leaf) -split '\+', 2
  $installation = Split-Path (Split-Path $ToolchainRoot -Parent) -Parent
  $RuntimeRoot = Join-Path $installation "Runtimes\$($version[0])\usr\bin"
}

$ScanRoot = Join-Path $ToolchainRoot 'usr\bin'
$WiXNamespace = 'http://wixtoolset.org/schemas/v4/wxs'
$ToolPackages = @('bld', 'cli', 'dbg', 'ide')
$Disabled = '<\?if\s+True\s*==\s*False\s*\?>.*?<\?endif\s*\?>'
$PE = '\.(exe|dll)$'
$Assembly = '^\$\(ToolchainRoot\)[\\/]usr[\\/]bin[\\/]([^\\/]+)[\\/]([^\\/]+\.dll)$'

function Read-WiX([string] $Path) {
  $text = Get-Content -Raw -Encoding UTF8 $Path
  $options = [Text.RegularExpressions.RegexOptions]::IgnoreCase -bor [Text.RegularExpressions.RegexOptions]::Singleline
  $text = [Text.RegularExpressions.Regex]::Replace($text, $Disabled, '', $options)
  $document = [Xml.XmlDocument]::new()
  $document.LoadXml($text)
  return ,$document
}

function New-NamespaceManager([Xml.XmlDocument] $Document) {
  $manager = [Xml.XmlNamespaceManager]::new($Document.NameTable)
  $manager.AddNamespace('wix', $WiXNamespace)
  return ,$manager
}

$arguments = @()
foreach ($package in $ToolPackages) {
  $path = Join-Path $InstallerRoot "$package\$package.wxi"
  $document = Read-WiX $path
  $namespaces = New-NamespaceManager $document
  $files = $document.SelectNodes('/wix:Include/wix:Package//wix:File[@Source]', $namespaces)
  foreach ($file in $files) {
    $node = $file
    $directory = $null
    while ($null -ne $node) {
      if ($node.NodeType -eq [Xml.XmlNodeType]::Element -and $node.HasAttribute('Directory')) {
        $directory = $node.GetAttribute('Directory')
        break
      }
      $node = $node.ParentNode
    }
    if ($directory -ne 'toolchain_$(VariantName)_usr_bin') {
      continue
    }

    $name = $file.GetAttribute('Name')
    if (!$name) {
      $name = [IO.Path]::GetFileName($file.GetAttribute('Source'))
    }
    if ($name -notmatch $PE) {
      continue
    }
    if (!(Test-Path (Join-Path $ScanRoot $name) -PathType Leaf)) {
      continue
    }
    $arguments += '--file', "$package=$name"
  }
}

$groups = @{}
$path = Join-Path $InstallerRoot 'prt\prt.wxi'
$document = Read-WiX $path
$namespaces = New-NamespaceManager $document
foreach ($group in $document.SelectNodes('/wix:Include/wix:Package//wix:ComponentGroup', $namespaces)) {
  $assemblies = @()
  foreach ($file in $group.SelectNodes('.//wix:File[@Source]', $namespaces)) {
    $match = [Text.RegularExpressions.Regex]::Match($file.GetAttribute('Source'), $Assembly, [Text.RegularExpressions.RegexOptions]::IgnoreCase)
    if (!$match.Success) { continue }
    $name = [IO.Path]::GetFileNameWithoutExtension($match.Groups[2].Value)
    if ($name -ieq $match.Groups[1].Value) {
      $assemblies += $match.Groups[1].Value
    }
  }
  $groups[$group.GetAttribute('Id')] = $assemblies
}

foreach ($package in $ToolPackages) {
  $identifier = "SxSSwiftRuntime$($package.ToUpperInvariant())"
  $feature = $document.SelectSingleNode("/wix:Include/wix:Package//wix:Feature[@Id='$identifier']", $namespaces)
  if ($null -eq $feature) {
    throw "PRT feature not found: $identifier"
  }

  $arguments += '--feature', "$package=$identifier"
  foreach ($reference in $feature.SelectNodes('./wix:ComponentGroupRef', $namespaces)) {
    $group = $reference.GetAttribute('Id')
    foreach ($assembly in $groups[$group]) {
      $arguments += '--authored', "$package=$assembly"
    }
  }
}

$command = @('run', '--package-path', $PSScriptRoot, 'sxs-audit', '--root', $ScanRoot, '--runtime-root', $RuntimeRoot) + $arguments
& $Swift @command
exit $LASTEXITCODE
