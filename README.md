# eform-sdk-delphi

A Delphi SDK for integrating with the Microting eForm API v1.

## Support Platforms

 - .NET Framework 4.5+
 - Windows 7 SP1
 - MS SQL 2016

## Setup

1) Get eform-dsk-delphiwrapper from github and build eFormSdk.Wrapper project
2) Get eform-sdk-dephi from github
3) Add following paths to Tools > Options > Environment Options > Delphi Options > Library: eformdelphi, eformdelphi/rest, eformdelphi/dto, eformdelphi/model, eformdelphi/model/reply
4) After user builds his app put into Win32/Debug (Win32/Release) folder binaries from previosly built eFormSdk.Wrapper project

## Get access token

You need to create an account for Microting API and get your access credentials.

 - Call Microting at +45 66 11 10 66 to get started.

## Docs

[SDK documentation can be found here (beta)](https://microting.github.io/eform-sdk-documentation/?pascal#)

## License

The MIT License (MIT)

Copyright (c) 2007-2018 microting

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
