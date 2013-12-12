@echo off
msbuild XmlLiteralsTypeProvider.sln || goto :error
msbuild Tests.sln || goto :error
Tests\bin\debug\tests.exe

goto :EOF

:error
echo Failed with error #%errorlevel%.
exit /b %errorlevel%