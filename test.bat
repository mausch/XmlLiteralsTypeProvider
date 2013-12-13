@echo off
%WINDIR%\Microsoft.Net\Framework\v4.0.30319\MSBuild.exe XmlLiteralsTypeProvider.sln || goto :error
%WINDIR%\Microsoft.Net\Framework\v4.0.30319\MSBuild.exe Tests.sln || goto :error
Tests\bin\debug\tests.exe

goto :EOF

:error
echo Failed with error #%errorlevel%.
exit /b %errorlevel%
